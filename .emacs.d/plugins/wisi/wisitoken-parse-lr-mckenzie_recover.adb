--  Abstract :
--
--  See spec
--
--  Copyright (C) 2017 - 2018 Free Software Foundation, Inc.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Task_Identification;
with System.Multiprocessors;
with WisiToken.Parse.LR.McKenzie_Recover.Base;
with WisiToken.Parse.LR.McKenzie_Recover.Explore;
with WisiToken.Parse.LR.Parser_Lists;
package body WisiToken.Parse.LR.McKenzie_Recover is

   task type Worker_Task
     (Super  : not null access Base.Supervisor;
      Shared : not null access Base.Shared)
   is
      entry Start;
      --  Start getting parser/configs to check from Config_Store.

      entry Done;
      --  Available when task is ready to terminate; after this rendezvous,
      --  task discriminants may be freed.

   end Worker_Task;

   task body Worker_Task
   is
      use all type Base.Config_Status;
      Status : Base.Config_Status;
   begin
      accept Start;

      loop
         Explore.Process_One (Super, Shared, Status);

         exit when Status = All_Done;
      end loop;

      accept Done;
   exception
   when E : others =>
      Super.Fatal (E);
   end Worker_Task;

   function To_Recover
     (Parser_Stack : in Parser_Lists.Parser_Stacks.Stack;
      Tree         : in Syntax_Trees.Tree)
     return Recover_Stacks.Stack
   is
      use all type SAL.Base_Peek_Type;
      Result : Recover_Stacks.Stack;
      Depth  : constant SAL.Peek_Type := Parser_Stack.Depth;
   begin
      Result.Set_Depth (Depth);
      for I in 1 .. Depth loop
         declare
            Item  : Parser_Lists.Parser_Stack_Item renames Parser_Stack (I);
            Token : constant Recover_Token := (if I = Depth then (others => <>) else Tree.Recover_Token (Item.Token));
         begin
            Result.Set (I, Depth, (Item.State, Item.Token, Token));
         end;
      end loop;
      return Result;
   end To_Recover;

   procedure Recover_Init
     (Shared_Parser : in out LR.Parser.Parser;
      Parser_State  : in out Parser_Lists.Parser_State)
   is
      use all type WisiToken.Parse.LR.Parser.Language_Fixes_Access;

      Trace  : WisiToken.Trace'Class renames Shared_Parser.Trace.all;
      Config : constant Configuration_Access := Parser_State.Recover.Config_Heap.Add (Configuration'(others => <>));
      Error  : Parse_Error renames Parser_State.Errors (Parser_State.Errors.Last);
   begin
      Parser_State.Recover.Enqueue_Count := Parser_State.Recover.Enqueue_Count + 1;

      Config.Resume_Token_Goal := Parser_State.Shared_Token + Shared_Parser.Table.McKenzie_Param.Check_Limit;

      if Trace_McKenzie > Outline then
         Trace.New_Line;
         Trace.Put_Line
           ("parser" & Integer'Image (Parser_State.Label) &
              ": State" & State_Index'Image (Parser_State.Stack (1).State) &
              " Current_Token" & Parser_State.Tree.Image (Parser_State.Current_Token, Trace.Descriptor.all) &
              " Resume_Token_Goal" & Token_Index'Image (Config.Resume_Token_Goal));
         Trace.Put_Line (Image (Error, Parser_State.Tree, Trace.Descriptor.all));
         if Trace_McKenzie > Extra then
            Put_Line
              (Trace, Parser_State.Label, Parser_Lists.Image
                 (Parser_State.Stack, Trace.Descriptor.all, Parser_State.Tree));
         end if;
      end if;

      --  Additional initialization of Parser_State.Recover is done in
      --  Supervisor.Initialize.

      Config.Stack := To_Recover (Parser_State.Stack, Parser_State.Tree);

      --  Parser_State.Recover_Insert_Delete must be empty (else we would not get
      --  here). Therefore Parser_State current token is in
      --  Shared_Parser.Shared_Token.

      Config.Current_Shared_Token := Parser_State.Shared_Token;

      case Error.Label is
      when Action =>
         Config.Error_Token := Parser_State.Tree.Recover_Token (Error.Error_Token);
         if Trace_McKenzie > Detail then
            Put ("enqueue", Trace, Parser_State.Label, Shared_Parser.Terminals, Config.all,
                 Task_ID => False);
         end if;

      when Check =>
         if Shared_Parser.Language_Fixes = null then
            --  The only fix is to ignore the error.
            if Trace_McKenzie > Detail then
               Put ("enqueue", Trace, Parser_State.Label, Shared_Parser.Terminals, Config.all,
                    Task_ID => False);
            end if;

         else
            --  Undo the reduction that encountered the error, let Process_One
            --  enqueue possible solutions. We leave the cost at 0, since this is
            --  the root config. Later logic will enqueue the 'ignore error'
            --  solution; see McKenzie_Recover.Explore Process_One.

            Config.Check_Status      := Error.Check_Status;
            Config.Error_Token       := Config.Stack (1).Token;
            Config.Check_Token_Count := Undo_Reduce (Config.Stack, Parser_State.Tree);

            Config.Ops.Append ((Undo_Reduce, Config.Error_Token.ID, Config.Check_Token_Count));

            if Trace_McKenzie > Detail then
               Put ("undo_reduce " & Image
                      (Config.Error_Token.ID, Trace.Descriptor.all), Trace, Parser_State.Label,
                    Shared_Parser.Terminals, Config.all, Task_ID => False);
            end if;
         end if;

      when Message =>
         --  Last error entry should be the failure that caused us to enter
         --  recovery.
         raise SAL.Programmer_Error;
      end case;
   end Recover_Init;

   function Recover (Shared_Parser : in out LR.Parser.Parser) return Recover_Status
   is
      use all type Parser.Post_Recover_Access;
      use all type SAL.Base_Peek_Type;
      use all type System.Multiprocessors.CPU_Range;
      Trace : WisiToken.Trace'Class renames Shared_Parser.Trace.all;

      Parsers : Parser_Lists.List renames Shared_Parser.Parsers;

      Current_Parser : Parser_Lists.Cursor;

      Super : aliased Base.Supervisor
        (Trace'Access,
         Cost_Limit        => Shared_Parser.Table.McKenzie_Param.Cost_Limit,
         Check_Delta_Limit => Shared_Parser.Table.McKenzie_Param.Check_Delta_Limit,
         Enqueue_Limit     => Shared_Parser.Table.McKenzie_Param.Enqueue_Limit,
         Parser_Count      => Parsers.Count);

      Shared : aliased Base.Shared
        (Shared_Parser.Trace,
         Shared_Parser.Lexer.all'Access,
         Shared_Parser.Table,
         Shared_Parser.Language_Fixes,
         Shared_Parser.Language_Use_Minimal_Complete_Actions,
         Shared_Parser.Language_String_ID_Set,
         Shared_Parser.Terminals'Access,
         Shared_Parser.Line_Begin_Token'Access);

      Task_Count : constant System.Multiprocessors.CPU_Range :=
        (if Shared_Parser.Table.McKenzie_Param.Task_Count = 0
         then System.Multiprocessors.CPU_Range'Max (1, System.Multiprocessors.Number_Of_CPUs - 1)
         --  Keep one CPU free for this main task, and the user.
         else Shared_Parser.Table.McKenzie_Param.Task_Count);

      Worker_Tasks : array (1 .. Task_Count) of Worker_Task (Super'Access, Shared'Access);

      procedure Cleanup
      is begin
         for I in Worker_Tasks'Range loop
            if Worker_Tasks (I)'Callable then
               abort Worker_Tasks (I);
            end if;
         end loop;
      end Cleanup;

   begin
      if Trace_McKenzie > Outline then
         Trace.New_Line;
         Trace.Put_Line (" McKenzie error recovery");
      end if;

      Super.Initialize (Parsers'Unrestricted_Access, Shared_Parser.Terminals'Unrestricted_Access);

      for Parser_State of Parsers loop
         Recover_Init (Shared_Parser, Parser_State);
      end loop;

      if Trace_McKenzie > Outline then
         Trace.New_Line;
         Trace.Put_Line (System.Multiprocessors.CPU_Range'Image (Worker_Tasks'Last) & " parallel tasks");
      end if;

      for I in Worker_Tasks'Range loop
         Worker_Tasks (I).Start;
      end loop;

      declare
         use Ada.Exceptions;
         ID : Exception_Id;
         Message : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Super.Done (ID, Message); -- Wait for all parsers to fail or succeed
         if ID /= Null_Id then
            Raise_Exception (ID, -Message);
         end if;
      end;

      --  Ensure all tasks terminate before proceeding; otherwise local
      --  variables disappear while task is still trying to access them.
      for I in Worker_Tasks'Range loop
         if Worker_Tasks (I)'Callable then
            Worker_Tasks (I).Done;
         end if;
      end loop;

      --  Adjust parser state for each successful recovery.
      --
      --  One option here would be to keep only the parser with the least
      --  cost fix. However, the normal reason for having multiple parsers
      --  is to resolve a grammar ambiguity; the least cost fix might
      --  resolve the ambiguity the wrong way. As could any other fix, of
      --  course.

      --  Spawn new parsers for multiple solutions
      declare
         use Parser_Lists;
         Cur         : Cursor             := Parsers.First;
         Solutions   : SAL.Base_Peek_Type := 0;
         Spawn_Limit : SAL.Base_Peek_Type := Shared_Parser.Max_Parallel; -- per parser
      begin
         for Parser of Parsers loop
            if Parser.Recover.Success then
               Solutions := Solutions + Parser.Recover.Results.Count;
            end if;
         end loop;

         if Solutions > Shared_Parser.Max_Parallel and Trace_McKenzie > Outline then
            Trace.Put_Line ("too many parallel parsers required in recover; dropping some solutions");
            Spawn_Limit := Shared_Parser.Max_Parallel / Parsers.Count;
         end if;

         loop
            declare
               Data : McKenzie_Data renames State_Ref (Cur).Recover;
            begin
               if Data.Success then
                  if Trace_McKenzie > Outline then
                     Trace.Put_Line
                       (Integer'Image (Label (Cur)) &
                          ": succeed" & SAL.Base_Peek_Type'Image (Data.Results.Count) &
                          ", enqueue" & Integer'Image (Data.Enqueue_Count) &
                          ", check " & Integer'Image (Data.Check_Count) &
                          ", cost: " & Integer'Image (Data.Results.Min_Key));
                  end if;

                  if Data.Results.Count > 1 then
                     for I in 1 .. SAL.Base_Peek_Type'Min (Spawn_Limit, Data.Results.Count - 1) loop
                        Parsers.Prepend_Copy (Cur); --  does not copy recover
                        if Trace_McKenzie > Outline or Trace_Parse > Outline then
                           Trace.Put_Line
                             ("spawn parser" & Integer'Image (Parsers.First.Label) & " from " &
                                Trimmed_Image (Cur.Label) & " (" & Trimmed_Image (Integer (Parsers.Count)) &
                                " active)");
                           Put ("", Trace, Parsers.First.Label, Shared_Parser.Terminals,
                                Data.Results.Peek, Task_ID => False);
                        end if;

                        State_Ref (Parsers.First).Recover.Results.Add (Data.Results.Remove);
                        State_Ref (Parsers.First).Recover.Success := True;
                     end loop;
                  end if;

                  if Trace_McKenzie > Outline or Trace_Parse > Outline then
                     Put ("", Trace, Cur.State_Ref.Label, Shared_Parser.Terminals, Data.Results.Peek,
                          Task_ID => False);
                  end if;
               else
                  if Trace_McKenzie > Outline then
                     Trace.Put_Line
                       (Integer'Image (Cur.Label) &
                          ": fail, enqueue" & Integer'Image (Data.Enqueue_Count) &
                          ", check " & Integer'Image (Data.Check_Count) &
                          ", cost_limit: " & Integer'Image (Shared_Parser.Table.McKenzie_Param.Cost_Limit) &
                          ", max shared_token " & Token_Index'Image (Shared_Parser.Terminals.Last_Index));
                  end if;
               end if;

            end;
            Next (Cur);
            exit when Is_Done (Cur);
         end loop;
      end;

      --  Edit Parser_State to apply solutions.

      --  We don't use 'for Parser_State of Parsers loop' here,
      --  because we might need to terminate a parser.
      Current_Parser := Parsers.First;
      loop
         exit when Current_Parser.Is_Done;

         if Current_Parser.State_Ref.Recover.Success then
            begin
               --  Can't have active 'renames State_Ref' when terminate a parser
               declare
                  use all type Syntax_Trees.Node_Index;
                  use Parser_Lists;

                  Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;

                  Descriptor : WisiToken.Descriptor renames Shared_Parser.Trace.Descriptor.all;
                  Tree       : Syntax_Trees.Tree renames Parser_State.Tree;
                  Data       : McKenzie_Data renames Parser_State.Recover;
                  Result     : Configuration renames Data.Results.Peek;

                  Min_Op_Token_Index        : WisiToken.Token_Index := WisiToken.Token_Index'Last;
                  Min_Push_Back_Token_Index : WisiToken.Token_Index := WisiToken.Token_Index'Last;

                  Stack_Matches_Ops     : Boolean := True;
                  Shared_Token_Changed  : Boolean := False;
                  Current_Token_Virtual : Boolean := False;

                  Sorted_Insert_Delete : Sorted_Insert_Delete_Arrays.Vector;
               begin
                  Parser_State.Errors (Parser_State.Errors.Last).Recover := Result;

                  Parser_State.Resume_Token_Goal := Result.Resume_Token_Goal;

                  if Trace_McKenzie > Extra then
                     Put_Line (Trace, Parser_State.Label, "before Ops applied:", Task_ID => False);
                     Put_Line
                       (Trace, Parser_State.Label, "stack " & Image (Parser_State.Stack, Descriptor, Tree),
                        Task_ID => False);
                     Put_Line
                       (Trace, Parser_State.Label, "Shared_Token  " & Image
                          (Parser_State.Shared_Token, Shared_Parser.Terminals, Descriptor),
                        Task_ID => False);
                     Put_Line
                       (Trace, Parser_State.Label, "Current_Token " & Parser_State.Tree.Image
                          (Parser_State.Current_Token, Descriptor),
                        Task_ID => False);
                  end if;

                  --  We don't apply all Ops to the parser stack here, because that
                  --  requires updating the syntax tree as well, and we want to let the
                  --  main parser do that, partly as a double check on the algorithms
                  --  here.
                  --
                  --  However, the main parser can only apply Insert and Delete ops; we
                  --  must apply Push_Back and Undo_Reduce here. Note that Fast_Forward
                  --  ops are just for bookkeeping.
                  --
                  --  In order to apply Undo_Reduce, we also need to apply any preceding
                  --  ops. See test_mckenzie_recover.adb Missing_Name_2 for an example
                  --  of multiple Undo_Reduce. On the other hand, Push_Back can be
                  --  applied without the preceding ops.
                  --
                  --  A Push_Back can go back past preceding ops, including Undo_Reduce;
                  --  there's no point in applying ops that are later superceded by such
                  --  a Push_Back. See test_mckenzie_recover.adb Out_Of_Order_Ops for an
                  --  example.
                  --
                  --  So first we go thru Ops to find the earliest Push_Back. Then we
                  --  apply ops that are before that point, up to the first Insert or
                  --  Fast_Forward. After that, we enqueue Insert and Delete ops on
                  --  Parser_State.Recover_Insert_Delete, in token_index order, and any
                  --  Undo_Reduce are rejected.
                  --
                  --  Then the main parser parses the edited input stream.
                  --
                  --  There's no need to modify Parser_State.Tree. Any tree nodes
                  --  created by the failed parse that are pushed back are useful for
                  --  error repair, and will just be ignored in future parsing. This
                  --  also avoids enlarging a non-flushed branched tree, which saves
                  --  time and space.

                  for Op of Result.Ops loop
                     case Op.Op is
                     when Fast_Forward =>
                        if Op.FF_Token_Index < Min_Op_Token_Index then
                           Min_Op_Token_Index := Op.FF_Token_Index;
                        end if;

                     when Undo_Reduce =>
                        null;

                     when Push_Back | Insert | Delete =>
                        if Op.Token_Index /= Invalid_Token_Index then
                           if Op.Token_Index < Min_Op_Token_Index then
                              Min_Op_Token_Index := Op.Token_Index;
                           end if;
                           if Op.Token_Index < Min_Push_Back_Token_Index then
                              Min_Push_Back_Token_Index := Op.Token_Index;
                           end if;
                        end if;

                     end case;
                  end loop;

                  for Op of Result.Ops loop
                     case Op.Op is
                     when Fast_Forward =>
                        Stack_Matches_Ops := False;

                     when Undo_Reduce =>
                        if not Stack_Matches_Ops then
                           if Trace_McKenzie > Outline then
                              Put_Line
                                (Trace, Parser_State.Label, "Undo_Reduce after insert or fast_forward",
                                 Task_ID => False);
                           end if;
                           raise Bad_Config;
                        end if;

                        declare
                           Item : constant Parser_Lists.Parser_Stack_Item := Parser_State.Stack.Pop;
                        begin
                           case Tree.Label (Item.Token) is
                           when Syntax_Trees.Shared_Terminal | Syntax_Trees.Virtual_Terminal =>
                              raise Bad_Config;

                           when Syntax_Trees.Nonterm =>
                              for C of Tree.Children (Item.Token) loop
                                 Parser_State.Stack.Push ((Tree.State (C), C));
                              end loop;
                           end case;
                        end;

                     when Push_Back =>
                        if Stack_Matches_Ops then
                           Parser_State.Stack.Pop;
                           if Op.Token_Index /= Invalid_Token_Index then
                              Parser_State.Shared_Token := Op.Token_Index;
                              Shared_Token_Changed      := True;
                           end if;

                        elsif Op.Token_Index = Min_Op_Token_Index then
                           loop
                              --  Multiple push_backs can have the same Op.Token_Index, so we may
                              --  already be at the target.
                              exit when Parser_State.Shared_Token <= Op.Token_Index and
                                Tree.Min_Terminal_Index (Parser_State.Stack (1).Token) /= Invalid_Token_Index;
                              --  also push back empty tokens.

                              declare
                                 Item : constant Parser_Lists.Parser_Stack_Item := Parser_State.Stack.Pop;

                                 Min_Index : constant Base_Token_Index :=
                                   Parser_State.Tree.Min_Terminal_Index (Item.Token);
                              begin
                                 if Min_Index /= Invalid_Token_Index then
                                    Shared_Token_Changed := True;
                                    Parser_State.Shared_Token := Min_Index;
                                 end if;
                              end;
                           end loop;
                           pragma Assert (Parser_State.Shared_Token = Op.Token_Index);
                        end if;

                     when Insert =>
                        if Stack_Matches_Ops and Op.Token_Index = Parser_State.Shared_Token then
                           --  This is the first Insert. Even if a later Push_Back supercedes it,
                           --  we record Stack_Matches_Ops false here.
                           Stack_Matches_Ops := False;

                           if Op.Token_Index <= Min_Push_Back_Token_Index then
                              Parser_State.Current_Token := Parser_State.Tree.Add_Terminal (Op.ID);
                              Current_Token_Virtual      := True;
                           else
                              Sorted_Insert_Delete.Insert (Op);
                           end if;
                        else
                           Sorted_Insert_Delete.Insert (Op);
                        end if;

                     when Delete =>
                        if Stack_Matches_Ops and Op.Token_Index = Parser_State.Shared_Token then
                           --  We can apply multiple deletes.
                           Parser_State.Shared_Token := Op.Token_Index + 1;
                           Shared_Token_Changed      := True;
                        else
                           Sorted_Insert_Delete.Insert (Op);
                        end if;
                     end case;
                  end loop;

                  --  We may not have processed the current Insert or Delete above, if
                  --  they are after a fast_forward.
                  for Op of Sorted_Insert_Delete loop
                     if Op.Token_Index = Parser_State.Shared_Token and not Current_Token_Virtual then
                        case Insert_Delete_Op_Label'(Op.Op) is
                        when Insert =>
                           Parser_State.Current_Token := Parser_State.Tree.Add_Terminal (Op.ID);
                           Current_Token_Virtual      := True;

                        when Delete =>
                           Parser_State.Shared_Token := Op.Token_Index + 1;
                           Shared_Token_Changed      := True;
                        end case;
                     else
                        Parser_State.Recover_Insert_Delete.Put (Op);
                     end if;
                  end loop;

                  --  If not Shared_Token_Changed, Shared_Token is the error token,
                  --  which is the next token to read. If Shared_Token_Changed, we
                  --  have set Shared_Token consistent with that; it is the next token to
                  --  read.

                  if (not Current_Token_Virtual) and Shared_Token_Changed then
                     Parser_State.Current_Token := Parser_State.Tree.Add_Terminal
                       (Parser_State.Shared_Token, Shared_Parser.Terminals);
                  end if;

                  --  Parser_State.Verb is the action that produced the current stack
                  --  top. Parser_State.Inc_Shared_Token determines how to get the next
                  --  token from Shared_Parser.Terminals.
                  --
                  --  If the stack top or Current_Token is virtual, then after all
                  --  virtuals are inserted, the main parser would normally increment
                  --  Parser_State.Shared_Token to get the next token. However, we have
                  --  set Shared_Token to the next token, so we don't want it to
                  --  increment. We could set Shared_Token to 1 less, but this way the
                  --  debug messages all show the expected Shared_Terminal.

                  if Parser_State.Stack (1).Token = Syntax_Trees.Invalid_Node_Index then
                     --  a virtual token from a previous recover
                     Parser_State.Set_Verb (Shift_Recover);
                     Parser_State.Inc_Shared_Token := False;
                  else
                     case Tree.Label (Parser_State.Stack (1).Token) is
                     when Syntax_Trees.Shared_Terminal =>
                        Parser_State.Set_Verb (Shift_Recover);
                        Parser_State.Inc_Shared_Token := not Current_Token_Virtual;

                     when Syntax_Trees.Virtual_Terminal =>
                        Parser_State.Set_Verb (Shift_Recover);
                        Parser_State.Inc_Shared_Token := False;

                     when Syntax_Trees.Nonterm =>
                        Parser_State.Set_Verb (Reduce);
                        Parser_State.Inc_Shared_Token := not Current_Token_Virtual;
                     end case;
                  end if;

                  if Trace_McKenzie > Extra then
                     Put_Line (Trace, Parser_State.Label, "after Ops applied:", Task_ID => False);
                     Put_Line
                       (Trace, Parser_State.Label, "stack " & Parser_Lists.Image
                          (Parser_State.Stack, Descriptor, Tree),
                        Task_ID => False);
                     Put_Line
                       (Trace, Parser_State.Label, "Shared_Token  " & Image
                          (Parser_State.Shared_Token, Shared_Parser.Terminals, Descriptor), Task_ID => False);
                     Put_Line
                       (Trace, Parser_State.Label, "Current_Token " & Parser_State.Tree.Image
                          (Parser_State.Current_Token, Descriptor), Task_ID => False);
                     Put_Line
                       (Trace, Parser_State.Label, "recover_insert_delete " & Image
                          (Parser_State.Recover_Insert_Delete, Descriptor), Task_ID => False);
                     Put_Line
                       (Trace, Parser_State.Label, "inc_shared_token " & Boolean'Image (Parser_State.Inc_Shared_Token) &
                          " parser verb " & All_Parse_Action_Verbs'Image (Parser_State.Verb),
                        Task_ID => False);

                  elsif Trace_McKenzie > Outline then
                     Put_Line
                       (Trace, Parser_State.Label, "inc_shared_token " & Boolean'Image (Parser_State.Inc_Shared_Token) &
                          " parser verb " & All_Parse_Action_Verbs'Image (Parser_State.Verb),
                        Task_ID => False);
                  end if;
               end;
            exception
            when Bad_Config =>
               if Parsers.Count = 1 then
                  --  Oops. just give up
                  return Fail_Programmer_Error;
               end if;
               Parsers.Terminate_Parser (Current_Parser, "bad config in recover", Trace);
            end;
         end if;
         Current_Parser.Next;
      end loop;

      if Shared_Parser.Post_Recover /= null then
         Shared_Parser.Post_Recover.all;
      end if;

      return Super.Recover_Result;

   exception
   when others =>
      Cleanup;
      return Fail_Programmer_Error;
   end Recover;

   ----------
   --  Spec private subprograms; for language-specific
   --  child packages.

   procedure Check (ID : Token_ID; Expected_ID : in Token_ID)
   is begin
      pragma Assert (ID = Expected_ID, Token_ID'Image (ID) & " /=" & Token_ID'Image (Expected_ID));
   end Check;

   function Current_Token
     (Terminals                 : in     Base_Token_Arrays.Vector;
      Terminals_Current         : in out WisiToken.Base_Token_Index;
      Restore_Terminals_Current :    out WisiToken.Base_Token_Index;
      Insert_Delete             : in out Sorted_Insert_Delete_Arrays.Vector;
      Current_Insert_Delete     : in out SAL.Base_Peek_Type)
     return Base_Token
   is
      use all type SAL.Base_Peek_Type;

      procedure Inc_I_D
      is begin
         Current_Insert_Delete := Current_Insert_Delete + 1;
         if Current_Insert_Delete > Insert_Delete.Last_Index then
            Current_Insert_Delete := No_Insert_Delete;
            Insert_Delete.Clear;
         end if;
      end Inc_I_D;

   begin
      if Terminals_Current = Base_Token_Index'First then
         --  Happens with really bad syntax; see test_mckenzie_recover.adb Error_4.
         raise Bad_Config;
      end if;

      loop
         if Current_Insert_Delete = No_Insert_Delete then
            Restore_Terminals_Current := Terminals_Current;
            return Terminals (Terminals_Current);

         elsif Insert_Delete (Current_Insert_Delete).Token_Index = Terminals_Current then
            declare
               Op : Insert_Delete_Op renames Insert_Delete (Current_Insert_Delete);
            begin
               case Insert_Delete_Op_Label (Op.Op) is
               when Insert =>
                  --  Decrement Terminals_Current so Next_Token knows it should always
                  --  increment it. Save the initial value, to restore in case of error.
                  Restore_Terminals_Current := Terminals_Current;
                  Terminals_Current         := Terminals_Current - 1;
                  return (ID => Op.ID, others => <>);

               when Delete =>
                  Terminals_Current         := Terminals_Current + 1;
                  Restore_Terminals_Current := Terminals_Current;
                  Inc_I_D;
               end case;
            end;
         else
            return Terminals (Terminals_Current);
         end if;
      end loop;
   end Current_Token;

   function Current_Token_ID_Peek
     (Terminals             : in Base_Token_Arrays.Vector;
      Terminals_Current     : in Base_Token_Index;
      Insert_Delete         : in Sorted_Insert_Delete_Arrays.Vector;
      Current_Insert_Delete : in SAL.Base_Peek_Type)
     return Token_ID
   is
      use all type SAL.Base_Peek_Type;
   begin
      if Terminals_Current = Base_Token_Index'First then
         --  Happens with really bad syntax; see test_mckenzie_recover.adb Error_4.
         raise Bad_Config;
      end if;

      if Current_Insert_Delete = No_Insert_Delete then
         return Terminals (Terminals_Current).ID;

      elsif Insert_Delete (Current_Insert_Delete).Token_Index = Terminals_Current then
         declare
            Op : Insert_Delete_Op renames Insert_Delete (Current_Insert_Delete);
         begin
            case Insert_Delete_Op_Label (Op.Op) is
            when Insert =>
               return Op.ID;

            when Delete =>
               --  This should have been handled in Check
               raise SAL.Programmer_Error;
            end case;
         end;
      else
         return Terminals (Terminals_Current).ID;
      end if;
   end Current_Token_ID_Peek;

   procedure Delete (Config : in out Configuration; ID : in Token_ID)
   is
      Op : constant Config_Op := (Delete, ID, Config.Current_Shared_Token);
   begin
      Config.Ops.Append (Op);
      Config.Insert_Delete.Insert (Op);
      Config.Current_Insert_Delete := 1;
   exception
   when SAL.Container_Full =>
      raise Bad_Config;
   end Delete;

   procedure Find_ID
     (Config         : in     Configuration;
      ID             : in     Token_ID;
      Matching_Index : in out SAL.Peek_Type)
   is
      use all type SAL.Peek_Type;
   begin
      loop
         exit when Matching_Index = Config.Stack.Depth; -- Depth has Invalid_Token_ID
         declare
            Stack_ID : Token_ID renames Config.Stack (Matching_Index).Token.ID;
         begin
            exit when Stack_ID = ID;
         end;
         Matching_Index := Matching_Index + 1;
      end loop;
   end Find_ID;

   procedure Find_ID
     (Config         : in     Configuration;
      IDs            : in     Token_ID_Set;
      Matching_Index : in out SAL.Peek_Type)
   is
      use all type SAL.Peek_Type;
   begin
      loop
         exit when Matching_Index = Config.Stack.Depth; -- Depth has Invalid_Token_ID
         declare
            ID : Token_ID renames Config.Stack (Matching_Index).Token.ID;
         begin
            exit when ID in IDs'First .. IDs'Last and then IDs (ID);
         end;
         Matching_Index := Matching_Index + 1;
      end loop;
   end Find_ID;

   procedure Find_Descendant_ID
     (Tree           : in     Syntax_Trees.Tree;
      Config         : in     Configuration;
      ID             : in     Token_ID;
      ID_Set         : in     Token_ID_Set;
      Matching_Index : in out SAL.Peek_Type)
   is
      use Syntax_Trees;
      use all type SAL.Peek_Type;
   begin
      loop
         exit when Matching_Index = Config.Stack.Depth; -- Depth has Invalid_Token_ID
         exit when ID_Set (Config.Stack (Matching_Index).Token.ID) and
           (Config.Stack (Matching_Index).Tree_Index /= Invalid_Node_Index and then
              Tree.Find_Descendant (Config.Stack (Matching_Index).Tree_Index, ID) /= Invalid_Node_Index);

         Matching_Index := Matching_Index + 1;
      end loop;
   end Find_Descendant_ID;

   procedure Find_Matching_Name
     (Config              : in     Configuration;
      Lexer               : access constant WisiToken.Lexer.Instance'Class;
      Name                : in     String;
      Matching_Name_Index : in out SAL.Peek_Type;
      Case_Insensitive    : in     Boolean)
   is
      use Ada.Characters.Handling;
      use all type SAL.Peek_Type;
      Match_Name : constant String := (if Case_Insensitive then To_Lower (Name) else Name);
   begin
      loop
         exit when Matching_Name_Index = Config.Stack.Depth; -- Depth has Invalid_Token_ID
         declare
            Token       : Recover_Token renames Config.Stack (Matching_Name_Index).Token;
            Name_Region : constant Buffer_Region :=
              (if Token.Name = Null_Buffer_Region
               then Token.Byte_Region
               else Token.Name);
         begin
            exit when Name_Region /= Null_Buffer_Region and then
              Match_Name =
              (if Case_Insensitive
               then To_Lower (Lexer.Buffer_Text (Name_Region))
               else Lexer.Buffer_Text (Name_Region));

            Matching_Name_Index := Matching_Name_Index + 1;
         end;
      end loop;
   end Find_Matching_Name;

   procedure Find_Matching_Name
     (Config              : in     Configuration;
      Lexer               : access constant WisiToken.Lexer.Instance'Class;
      Name                : in     String;
      Matching_Name_Index : in out SAL.Peek_Type;
      Other_ID            : in     Token_ID;
      Other_Count         :    out Integer;
      Case_Insensitive    : in     Boolean)
   is
      use Ada.Characters.Handling;
      use all type SAL.Peek_Type;
      Match_Name : constant String := (if Case_Insensitive then To_Lower (Name) else Name);
   begin
      Other_Count := 0;

      loop
         exit when Matching_Name_Index = Config.Stack.Depth; -- Depth has Invalid_Token_ID
         declare
            Token       : Recover_Token renames Config.Stack (Matching_Name_Index).Token;
            Name_Region : constant Buffer_Region :=
              (if Token.Name = Null_Buffer_Region
               then Token.Byte_Region
               else Token.Name);
         begin
            exit when Name_Region /= Null_Buffer_Region and then
              Match_Name =
              (if Case_Insensitive
               then To_Lower (Lexer.Buffer_Text (Name_Region))
               else Lexer.Buffer_Text (Name_Region));

            if Other_ID = Token.ID then
               Other_Count := Other_Count + 1;
            end if;

            Matching_Name_Index := Matching_Name_Index + 1;
         end;
      end loop;
   end Find_Matching_Name;

   procedure Insert (Config : in out Configuration; ID : in Token_ID)
   is
      Op : constant Config_Op := (Insert, ID, Config.Current_Shared_Token);
   begin
      Config.Ops.Append (Op);
      Config.Insert_Delete.Insert (Op);
      Config.Current_Insert_Delete := 1;
   exception
   when SAL.Container_Full =>
      raise Bad_Config;
   end Insert;

   procedure Insert (Config : in out Configuration; IDs : in Token_ID_Array)
   is begin
      for ID of IDs loop
         Insert (Config, ID);
      end loop;
   end Insert;

   function Next_Token
     (Terminals                 : in     Base_Token_Arrays.Vector;
      Terminals_Current         : in out Base_Token_Index;
      Restore_Terminals_Current : in out WisiToken.Base_Token_Index;
      Insert_Delete             : in out Sorted_Insert_Delete_Arrays.Vector;
      Current_Insert_Delete     : in out SAL.Base_Peek_Type)
     return Base_Token
   is
      use all type SAL.Base_Peek_Type;
   begin
      loop
         if Insert_Delete.Last_Index > 0 and then Current_Insert_Delete = Insert_Delete.Last_Index then
            Current_Insert_Delete     := No_Insert_Delete;
            Insert_Delete.Clear;
            Terminals_Current         := Terminals_Current + 1;
            Restore_Terminals_Current := Terminals_Current;
            return Terminals (Terminals_Current);

         elsif Current_Insert_Delete = No_Insert_Delete then
            Terminals_Current         := Terminals_Current + 1;
            Restore_Terminals_Current := Terminals_Current;
            return Terminals (Terminals_Current);

         elsif Insert_Delete (Current_Insert_Delete + 1).Token_Index = Terminals_Current + 1 then
            Current_Insert_Delete := Current_Insert_Delete + 1;
            declare
               Op : constant Insert_Delete_Op := Insert_Delete (Current_Insert_Delete);
            begin
               case Insert_Delete_Op_Label'(Op.Op) is
               when Insert =>
                  return (ID => Op.ID, others => <>);

               when Delete =>
                  Terminals_Current         := Terminals_Current + 1;
                  Restore_Terminals_Current := Terminals_Current;
               end case;
            end;

         else
            Terminals_Current         := Terminals_Current + 1;
            Restore_Terminals_Current := Terminals_Current;
            return Terminals (Terminals_Current);
         end if;
      end loop;
   end Next_Token;

   procedure Push_Back (Config : in out Configuration)
   is
      Item        : constant Recover_Stack_Item := Config.Stack.Pop;
      Token_Index : constant Base_Token_Index   := Item.Token.Min_Terminal_Index;

      function Compare (Left : in Base_Token_Index; Right : in Config_Op) return Boolean
        is (case Right.Op is
            when Fast_Forward    => False,
            when Undo_Reduce     => False,
            when Push_Back       => False,
            when Insert | Delete => Left < Right.Token_Index);
      --  If Left = Right.Token_Index, we assume the Right ops go _after_
      --  the Left, so the Left do not need to be repeated.
   begin
      if Token_Index /= Invalid_Token_Index then
         Config.Current_Shared_Token := Token_Index;
         for I in Config.Ops.First_Index .. Config.Ops.Last_Index loop
            if Compare (Token_Index, Config.Ops (I)) then
               Config.Insert_Delete.Insert (Config.Ops (I));
            end if;
         end loop;
      end if;

      Config.Ops.Append ((Push_Back, Item.Token.ID, Config.Current_Shared_Token));
   exception
   when SAL.Container_Full =>
      raise Bad_Config;
   end Push_Back;

   procedure Push_Back_Check (Config : in out Configuration; Expected_ID : in Token_ID)
   is begin
      Check (Config.Stack (1).Token.ID, Expected_ID);
      Push_Back (Config);
   end Push_Back_Check;

   procedure Push_Back_Check (Config : in out Configuration; Expected : in Token_ID_Array)
   is begin
      for ID of Expected loop
         Push_Back_Check (Config, ID);
      end loop;
   end Push_Back_Check;

   procedure Put
     (Message      : in     String;
      Trace        : in out WisiToken.Trace'Class;
      Parser_Label : in     Natural;
      Terminals    : in     Base_Token_Arrays.Vector;
      Config       : in     Configuration;
      Task_ID      : in     Boolean := True)
   is
      --  For debugging output

      --  Build a string, call trace.put_line once, so output from multiple
      --  tasks is not interleaved (mostly).
      use all type Ada.Strings.Unbounded.Unbounded_String;
      use all type SAL.Base_Peek_Type;
      use all type WisiToken.Semantic_Checks.Check_Status_Label;

      Descriptor : WisiToken.Descriptor renames Trace.Descriptor.all;

      Result : Ada.Strings.Unbounded.Unbounded_String :=
        (if Task_ID then +Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task) else +"") &
        Integer'Image (Parser_Label) & ": " &
        (if Message'Length > 0 then Message & ":" else "");
   begin
      Result := Result & Natural'Image (Config.Cost) & ", ";
      if Config.Check_Status.Label /= Ok then
         Result := Result & Semantic_Checks.Check_Status_Label'Image (Config.Check_Status.Label) & " ";
      elsif Config.Error_Token.ID /= Invalid_Token_ID then
         Result := Result & "Error " & Image (Config.Error_Token, Descriptor) & " ";
      end if;
      Result := Result & Image (Config.Stack, Descriptor, Depth => 1);

      if Config.Current_Insert_Delete = No_Insert_Delete then
         Result := Result & "|" & Image (Config.Current_Shared_Token, Terminals, Descriptor) & "|";
      else
         Result := Result & "/" & Trimmed_Image (Config.Current_Insert_Delete) & ":" &
           Image (Config.Insert_Delete (Config.Current_Insert_Delete), Descriptor) & "/";
      end if;

      Result := Result & Image (Config.Ops, Descriptor);
      Trace.Put_Line (-Result);
   end Put;

   procedure Put_Line
     (Trace        : in out WisiToken.Trace'Class;
      Parser_Label : in     Natural;
      Message      : in     String;
      Task_ID      : in     Boolean := True)
   is begin
      Trace.Put_Line
        ((if Task_ID then Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task) else "") &
           Integer'Image (Parser_Label) & ": " & Message);
   end Put_Line;

   function Undo_Reduce
     (Stack : in out Recover_Stacks.Stack;
      Tree  : in     Syntax_Trees.Tree)
     return Ada.Containers.Count_Type
   is
      Nonterm_Item : constant Recover_Stack_Item                  := Stack.Pop;
      Children     : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Children (Nonterm_Item.Tree_Index);
   begin
      for C of Children loop
         Stack.Push ((Tree.State (C), C, Tree.Recover_Token (C)));
      end loop;
      return Children'Length;
   end Undo_Reduce;

   procedure Undo_Reduce_Check
     (Config   : in out Configuration;
      Tree     : in     Syntax_Trees.Tree;
      Expected : in     Token_ID)
   is begin
      Check (Config.Stack (1).Token.ID, Expected);
      Config.Ops.Append ((Undo_Reduce, Expected, Undo_Reduce (Config.Stack, Tree)));
   exception
   when SAL.Container_Full =>
      raise Bad_Config;
   end Undo_Reduce_Check;

   procedure Undo_Reduce_Check
     (Config   : in out Configuration;
      Tree     : in     Syntax_Trees.Tree;
      Expected : in     Token_ID_Array)
   is begin
      for ID of Expected loop
         Undo_Reduce_Check (Config, Tree, ID);
      end loop;
   end Undo_Reduce_Check;

end WisiToken.Parse.LR.McKenzie_Recover;
