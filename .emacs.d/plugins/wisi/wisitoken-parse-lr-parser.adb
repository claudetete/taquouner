--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 - 2005, 2008 - 2015, 2017, 2018 Free Software Foundation, Inc.
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

with Ada.Calendar.Formatting;
with Ada.Exceptions;
with WisiToken.Parse.LR.McKenzie_Recover;
package body WisiToken.Parse.LR.Parser is

   function Reduce_Stack_1
     (Current_Parser : in     Parser_Lists.Cursor;
      Action         : in     Reduce_Action_Rec;
      Nonterm        :    out WisiToken.Syntax_Trees.Valid_Node_Index;
      Lexer          : in     WisiToken.Lexer.Handle;
      Trace          : in out WisiToken.Trace'Class)
     return WisiToken.Semantic_Checks.Check_Status_Label
   is
      --  We treat semantic check errors as parse errors here, to allow
      --  error recovery to take better advantage of them. One recovery
      --  strategy is to fix things so the semantic check passes.

      use all type SAL.Base_Peek_Type;
      use all type Semantic_Checks.Check_Status_Label;
      use all type Semantic_Checks.Semantic_Check;

      Parser_State  : Parser_Lists.Parser_State renames Current_Parser.State_Ref.Element.all;
      Children_Tree : Syntax_Trees.Valid_Node_Index_Array (1 .. SAL.Base_Peek_Type (Action.Token_Count));
   begin
      for I in reverse Children_Tree'Range loop
         Children_Tree (I) := Parser_State.Stack.Pop.Token;
      end loop;

      Nonterm := Parser_State.Tree.Add_Nonterm
        (Action.Production, Children_Tree, Action.Action,
         Default_Virtual => Parser_State.Tree.Is_Virtual (Parser_State.Current_Token));
      --  Computes Nonterm.Byte_Region, Virtual

      if Trace_Parse > Detail then
         Trace.Put_Line (Parser_State.Tree.Image (Nonterm, Trace.Descriptor.all, Include_Children => True));
      end if;

      if Action.Check = null then
         return Ok;

      else
         declare
            Nonterm_Token  : Recover_Token := Parser_State.Tree.Recover_Token (Nonterm);
            Children_Token : constant Recover_Token_Array := Parser_State.Tree.Recover_Token_Array (Children_Tree);
            Status : constant Semantic_Checks.Check_Status := Action.Check
              (Lexer, Nonterm_Token, Children_Token);
         begin
            Parser_State.Tree.Set_Name_Region (Nonterm, Nonterm_Token.Name);

            if Trace_Parse > Detail then
               Trace.Put_Line ("semantic check " & Semantic_Checks.Image (Status, Trace.Descriptor.all));
            end if;

            case Status.Label is
            when Ok =>
               return Ok;

            when Semantic_Checks.Error =>
               if Parser_State.Resume_Active then
                  --  Ignore this error; that's how McKenzie_Recover decided to fix it
                  return Ok;

               else
                  Parser_State.Errors.Append
                    ((Label          => Check,
                      First_Terminal => Trace.Descriptor.First_Terminal,
                      Last_Terminal  => Trace.Descriptor.Last_Terminal,
                      Check_Status   => Status,
                      Recover        => (others => <>)));
                  return Status.Label;
               end if;
            end case;
         end;
      end if;
   end Reduce_Stack_1;

   procedure Do_Action
     (Action         : in Parse_Action_Rec;
      Current_Parser : in Parser_Lists.Cursor;
      Shared_Parser  : in LR.Parser.Parser)
   is
      use all type Semantic_Checks.Check_Status_Label;

      Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
      Trace        : WisiToken.Trace'Class renames Shared_Parser.Trace.all;
      Nonterm      : WisiToken.Syntax_Trees.Valid_Node_Index;
      Status       : Semantic_Checks.Check_Status_Label;
   begin
      if Trace_Parse > Detail then
         Trace.Put
           (Integer'Image (Current_Parser.Label) & ": " &
              Trimmed_Image (Parser_State.Stack.Peek.State) & ": " &
              Parser_State.Tree.Image (Parser_State.Current_Token, Trace.Descriptor.all) & " : ");
         Put (Trace, Action);
         Trace.New_Line;
      end if;

      case Action.Verb is
      when Shift =>
         Current_Parser.Set_Verb (Shift);
         Parser_State.Stack.Push ((Action.State, Parser_State.Current_Token));
         Parser_State.Tree.Set_State (Parser_State.Current_Token, Action.State);

      when Reduce =>
         Status := Reduce_Stack_1 (Current_Parser, Action, Nonterm, Shared_Parser.Lexer, Trace);

         --  Even when Reduce_Stack_1 returns Error, it did reduce the stack, so
         --  push Nonterm.
         Parser_State.Stack.Push
           ((State    => Goto_For
               (Table => Shared_Parser.Table.all,
                State => Parser_State.Stack (1).State,
                ID    => Action.Production.LHS),
             Token    => Nonterm));

         Parser_State.Tree.Set_State (Nonterm, Parser_State.Stack (1).State);

         case Status is
         when Ok =>
            Current_Parser.Set_Verb (Reduce);

            if Trace_Parse > Detail then
               Trace.Put_Line (" ... goto state " & Trimmed_Image (Parser_State.Stack.Peek.State));
            end if;

         when Semantic_Checks.Error =>
            Current_Parser.Set_Verb (Error);
            Parser_State.Zombie_Token_Count := 1;
         end case;

      when Accept_It =>
         case Reduce_Stack_1
           (Current_Parser,
            (Reduce, Action.Production, Action.Action, Action.Check, Action.Token_Count),
            Nonterm, Shared_Parser.Lexer, Trace)
         is
         when Ok =>
            Current_Parser.Set_Verb (Action.Verb);

            Parser_State.Tree.Set_Root (Nonterm);

         when Semantic_Checks.Error =>
            Current_Parser.Set_Verb (Error);
            Parser_State.Zombie_Token_Count := 1;
         end case;

      when Error =>
         Current_Parser.Set_Verb (Action.Verb);

         Parser_State.Zombie_Token_Count := 1;

         declare
            Expecting : constant Token_ID_Set := LR.Expecting
              (Shared_Parser.Table.all, Current_Parser.State_Ref.Stack.Peek.State);
         begin
            Parser_State.Errors.Append
              ((Label          => LR.Action,
                First_Terminal => Trace.Descriptor.First_Terminal,
                Last_Terminal  => Trace.Descriptor.Last_Terminal,
                Error_Token    => Parser_State.Current_Token,
                Expecting      => Expecting,
                Recover        => (others => <>)));

            if Trace_Parse > Outline then
               Put
                 (Trace,
                  Integer'Image (Current_Parser.Label) & ": expecting: " &
                    Image (Expecting, Trace.Descriptor.all));
               Trace.New_Line;
            end if;
         end;
      end case;
   end Do_Action;

   procedure Do_Deletes
     (Shared_Parser : in out LR.Parser.Parser;
      Parser_State  : in out Parser_Lists.Parser_State)
   is
      use all type SAL.Base_Peek_Type;
   begin
      if Trace_Parse > Extra then
         Shared_Parser.Trace.Put_Line
           (Integer'Image (Parser_State.Label) & ": recover_insert_delete: " &
              Image (Parser_State.Recover_Insert_Delete, Shared_Parser.Trace.Descriptor.all));
      end if;

      loop
         if Parser_State.Recover_Insert_Delete.Length > 0 and then
           Parser_State.Recover_Insert_Delete.Peek.Op = Delete and then
           Parser_State.Recover_Insert_Delete.Peek.Token_Index =
           (if Parser_State.Inc_Shared_Token
            then Parser_State.Shared_Token + 1
            else Parser_State.Shared_Token)
         then
            Parser_State.Shared_Token := Parser_State.Shared_Token + 1;
            Parser_State.Recover_Insert_Delete.Drop;
         else
            exit;
         end if;
      end loop;
   end Do_Deletes;

   --  Verb: the type of parser cycle to execute;
   --
   --  Accept : all Parsers.Verb return Accept - done parsing.
   --
   --  Shift : some Parsers.Verb return Shift, all with the same current
   --  token in Shared_Parser.Terminals.
   --
   --  Shift_Recover : some Parsers.Verb return Shift, with current
   --  tokens virtual (inserted by error recovery).
   --
   --  Pause : Resume is active, and this parser has reached Resume_Goal,
   --  so it is waiting for the others to catch up.
   --
   --  Reduce : some Parsers.Verb return Reduce.
   --
   --  Error : all Parsers.Verb return Error.
   --
   --  Zombie_Count: count of parsers in Error state
   procedure Parse_Verb
     (Shared_Parser : in out LR.Parser.Parser;
      Verb          :    out All_Parse_Action_Verbs;
      Zombie_Count  :    out SAL.Base_Peek_Type)
   is
      use all type SAL.Base_Peek_Type;

      Shift_Count         : SAL.Base_Peek_Type := 0;
      Shift_Recover_Count : SAL.Base_Peek_Type := 0;
      Shift_Virtual_Count : SAL.Base_Peek_Type := 0;
      Accept_Count        : SAL.Base_Peek_Type := 0;
      Error_Count         : SAL.Base_Peek_Type := 0;
      Resume_Active       : Boolean            := False;
   begin
      Zombie_Count := 0;

      for Parser_State of Shared_Parser.Parsers loop
         case Parser_State.Verb is
         when Pause | Shift_Recover | Shift =>
            Do_Deletes (Shared_Parser, Parser_State);

            if Parser_State.Recover_Insert_Delete.Length > 0 and then
              Parser_State.Recover_Insert_Delete.Peek.Op = Insert and then
              Parser_State.Recover_Insert_Delete.Peek.Token_Index =
              (if Parser_State.Inc_Shared_Token
               then Parser_State.Shared_Token + 1
               else Parser_State.Shared_Token)
            then
               --  Shifting a virtual token.
               Shift_Virtual_Count := Shift_Virtual_Count + 1;
               Shift_Recover_Count := Shift_Recover_Count + 1;
               Parser_State.Set_Verb (Shift_Recover);

            else
               Shift_Count := Shift_Count + 1;
               Parser_State.Set_Verb (Shift);
            end if;

            if Parser_State.Resume_Active then
               if Parser_State.Resume_Token_Goal <= Parser_State.Shared_Token then
                  Parser_State.Resume_Active := False;
                  if Trace_Parse > Detail then
                     Shared_Parser.Trace.Put_Line (Integer'Image (Parser_State.Label) & ": resume_active: False");
                  end if;
               else
                  Resume_Active := True;
               end if;
            end if;

         when Reduce =>
            Verb := Reduce;
            return;

         when Accept_It =>
            Accept_Count := Accept_Count + 1;

         when Error =>
            if Shared_Parser.Enable_McKenzie_Recover then
               --  This parser is waiting for others to error; they can continue
               --  parsing.
               Zombie_Count := Zombie_Count + 1;
            else
               Error_Count := Error_Count + 1;
            end if;
         end case;
      end loop;

      if Accept_Count > 0 and Shared_Parser.Parsers.Count = Accept_Count + Zombie_Count then
         Verb := Accept_It;

      elsif Shared_Parser.Parsers.Count = Error_Count + Zombie_Count then
         Verb := Error;

      elsif Shift_Recover_Count > 0 then
         Verb := Shift_Recover;

      elsif Shift_Count > 0 then
         Verb := Shift;

      else
         raise SAL.Programmer_Error;
      end if;

      if Resume_Active then
         for Parser_State of Shared_Parser.Parsers loop
            if Parser_State.Verb in Shift | Shift_Recover and not Parser_State.Resume_Active then
               Parser_State.Set_Verb (Pause);
            end if;
         end loop;
      end if;
   end Parse_Verb;

   ----------
   --  Public subprograms, declaration order

   overriding procedure Finalize (Object : in out LR.Parser.Parser)
   is begin
      Free_Table (Object.Table);
   end Finalize;

   procedure New_Parser
     (Parser                                :    out          LR.Parser.Parser;
      Trace                                 : not null access WisiToken.Trace'Class;
      Lexer                                 : in              WisiToken.Lexer.Handle;
      Table                                 : in              Parse_Table_Ptr;
      Language_Fixes                        : in              Language_Fixes_Access;
      Language_Use_Minimal_Complete_Actions : in              Language_Use_Minimal_Complete_Actions_Access;
      Language_String_ID_Set                : in              Language_String_ID_Set_Access;
      User_Data                             : in              WisiToken.Syntax_Trees.User_Data_Access;
      Max_Parallel                          : in              SAL.Base_Peek_Type := Default_Max_Parallel;
      Terminate_Same_State                  : in              Boolean            := True)
   is
      use all type Syntax_Trees.User_Data_Access;
   begin
      Parser.Lexer                               := Lexer;
      Parser.Trace                               := Trace;
      Parser.Table                               := Table;
      Parser.Language_Fixes                      := Language_Fixes;
      Parser.Language_Use_Minimal_Complete_Actions := Language_Use_Minimal_Complete_Actions;
      Parser.Language_String_ID_Set              := Language_String_ID_Set;
      Parser.User_Data                           := User_Data;

      --  We can't use Table.McKenzie_Param /= Default_McKenzie_Param here,
      --  because the discriminants are different. We also can't use just
      --  Table.McKenzie_Param.Cost_Limit /=
      --  Default_McKenzie_Param.Cost_Limit, because some grammars don't set
      --  a Cost_Limit, just some other limit.
      Parser.Enable_McKenzie_Recover :=
        Table.McKenzie_Param.Cost_Limit /= Default_McKenzie_Param.Cost_Limit or
          Table.McKenzie_Param.Check_Limit /= Default_McKenzie_Param.Check_Limit or
          Table.McKenzie_Param.Check_Delta_Limit /= Default_McKenzie_Param.Check_Delta_Limit or
          Table.McKenzie_Param.Enqueue_Limit /= Default_McKenzie_Param.Enqueue_Limit;

      Parser.Max_Parallel         := Max_Parallel;
      Parser.Terminate_Same_State := Terminate_Same_State;

      if User_Data /= null then
         User_Data.Set_Lexer_Terminals (Lexer, Parser.Terminals'Unchecked_Access);
      end if;
   end New_Parser;

   overriding procedure Parse (Shared_Parser : aliased in out LR.Parser.Parser)
   is
      use all type Ada.Strings.Unbounded.Unbounded_String;
      use all type Syntax_Trees.User_Data_Access;
      use all type Ada.Containers.Count_Type;
      use all type SAL.Base_Peek_Type;

      Trace : WisiToken.Trace'Class renames Shared_Parser.Trace.all;

      Current_Verb   : All_Parse_Action_Verbs;
      Current_Parser : Parser_Lists.Cursor;
      Action         : Parse_Action_Node_Ptr;
      Zombie_Count   : SAL.Base_Peek_Type;

      procedure Check_Error (Check_Parser : in out Parser_Lists.Cursor)
      is begin
         if Check_Parser.Verb = Error then
            --  This parser errored on last input. This is how grammar conflicts
            --  are resolved when the input text is valid, in which case we should
            --  just terminate this parser. However, this may be due to invalid
            --  input text, so we keep the parser alive but suspended for a few
            --  tokens, to see if the other parsers also error, in which case they
            --  all participate in error recovery.

            --  We do not create zombie parsers during resume.
            if not Check_Parser.State_Ref.Resume_Active then
               --  Parser is now a zombie
               if Trace_Parse > Detail then
                  Trace.Put_Line (Integer'Image (Check_Parser.Label) & ": zombie");
               end if;
               Check_Parser.Next;

            else
               if Shared_Parser.Parsers.Count = 1 then
                  if Trace_Parse > Outline then
                     Trace.Put_Line (Integer'Image (Check_Parser.Label) & ": error during resume");
                  end if;
                  Shared_Parser.Parsers.First_State_Ref.Errors.Append
                    ((Label          => LR.Message,
                      First_Terminal => Trace.Descriptor.First_Terminal,
                      Last_Terminal  => Trace.Descriptor.Last_Terminal,
                      Recover        => <>,
                      Msg            => +"error during resume"));
                  raise Syntax_Error;

               else
                  --  This is ok if a conflict occured during resume - we assume this is
                  --  a branch that failed during recover as well. Otherwise it's a
                  --  programmer error.
                  if Check_Parser.State_Ref.Conflict_During_Resume then
                     Shared_Parser.Parsers.Terminate_Parser
                       (Check_Parser, "error in conflict during resume", Shared_Parser.Trace.all);
                  else
                     raise SAL.Programmer_Error with "error during resume";
                  end if;
               end if;
            end if;
         else
            Check_Parser.Next;
         end if;
      end Check_Error;

   begin
      if Shared_Parser.User_Data /= null then
         Shared_Parser.User_Data.Reset;
      end if;

      Shared_Parser.Lex_All;

      Shared_Parser.String_Quote_Checked := Invalid_Line_Number;
      Shared_Parser.Shared_Tree.Clear;
      Shared_Parser.Parsers              := Parser_Lists.New_List
        (Shared_Tree => Shared_Parser.Shared_Tree'Unchecked_Access);

      Shared_Parser.Parsers.First.State_Ref.Stack.Push ((Shared_Parser.Table.State_First, others => <>));

      Main_Loop :
      loop
         --  exit on Accept_It action or syntax error.

         Parse_Verb (Shared_Parser, Current_Verb, Zombie_Count);

         if Trace_Parse > Extra then
            Trace.Put_Line ("cycle start; current_verb: " & Parse_Action_Verbs'Image (Current_Verb));
         end if;

         case Current_Verb is
         when Pause =>
            null;

         when Shift =>
            --  We just shifted a token; get the next token from
            --  Shared_Parser.Terminals.

            for Parser_State of Shared_Parser.Parsers loop
               if Parser_State.Verb = Error then
                  if Shared_Parser.Enable_McKenzie_Recover then
                     Parser_State.Zombie_Token_Count := Parser_State.Zombie_Token_Count + 1;
                     if Trace_Parse > Extra then
                        Trace.Put_Line
                          (Integer'Image (Parser_State.Label) & ": zombie (" &
                             Token_Index'Image
                               (Shared_Parser.Table.McKenzie_Param.Check_Limit - Parser_State.Zombie_Token_Count) &
                             " tokens remaining)");
                     end if;
                  end if;

               elsif Parser_State.Verb = Shift then
                  if Parser_State.Inc_Shared_Token then
                     --  Inc_Shared_Token is only set False by McKenzie_Recover; see there
                     --  for when/why.
                     Parser_State.Shared_Token := Parser_State.Shared_Token + 1;
                  else
                     Parser_State.Inc_Shared_Token := True;
                  end if;

                  Parser_State.Current_Token := Parser_State.Tree.Add_Terminal
                    (Parser_State.Shared_Token, Shared_Parser.Terminals);

                  if Trace_Parse > Extra then
                     Trace.Put_Line
                       (Integer'Image (Parser_State.Label) & ": current_token" & Parser_State.Tree.Image
                          (Parser_State.Current_Token, Trace.Descriptor.all));
                  end if;
               end if;
            end loop;

         when Shift_Recover =>
            --  Same as Shift, except input a token inserted by error recovery, or
            --  input from Shared_Parser.Terminals during error recovery.

            for Parser_State of Shared_Parser.Parsers loop
               --  We don't check for Verb = Error; during recovery, errors cause
               --  parsers to terminate immediately.

               if Parser_State.Verb = Shift_Recover then
                  if Parser_State.Recover_Insert_Delete.Length > 0 and then
                    Parser_State.Recover_Insert_Delete.Peek.Op = Insert and then
                    Parser_State.Recover_Insert_Delete.Peek.Token_Index =
                    (if Parser_State.Inc_Shared_Token
                     then Parser_State.Shared_Token + 1
                     else Parser_State.Shared_Token)
                  then
                     Parser_State.Current_Token := Parser_State.Tree.Add_Terminal
                       (Parser_State.Recover_Insert_Delete.Get.ID);

                     if Trace_Parse > Extra then
                        Trace.Put_Line
                          (Integer'Image (Parser_State.Label) & ": current_token" & Parser_State.Tree.Image
                             (Parser_State.Current_Token, Trace.Descriptor.all));
                     end if;

                  elsif (if Parser_State.Inc_Shared_Token
                         then Parser_State.Shared_Token + 1
                         else Parser_State.Shared_Token) <= Shared_Parser.Terminals.Last_Index
                  then
                     if Parser_State.Inc_Shared_Token then
                        --  Inc_Shared_Token is only set False by McKenzie_Recover, see there
                        --  for when/why.
                        Parser_State.Shared_Token := Parser_State.Shared_Token + 1;
                     else
                        Parser_State.Inc_Shared_Token := True;
                     end if;

                     Parser_State.Current_Token := Parser_State.Tree.Add_Terminal
                       (Parser_State.Shared_Token, Shared_Parser.Terminals);

                     if Trace_Parse > Extra then
                        Trace.Put_Line
                          (Integer'Image (Parser_State.Label) & ": current_token" & Parser_State.Tree.Image
                             (Parser_State.Current_Token, Trace.Descriptor.all));
                     end if;

                  else
                     --  Set_Verb set the wrong verb.
                     raise SAL.Programmer_Error;
                  end if;
               end if;
            end loop;

         when Accept_It =>
            --  All parsers accepted or are zombies.
            declare
               Count : constant SAL.Base_Peek_Type := Shared_Parser.Parsers.Count;
               Temp  : Parser_Lists.Cursor;
            begin
               if Count = 1 then
                  --  Nothing more to do
                  if Trace_Parse > Outline then
                     Trace.Put_Line (Integer'Image (Shared_Parser.Parsers.First.Label) & ": succeed");
                  end if;
                  exit Main_Loop;

               elsif Zombie_Count + 1 = Count then
                  --  All but one are zombies
                  Current_Parser := Shared_Parser.Parsers.First;
                  loop
                     if Current_Parser.Verb = Accept_It then
                        if Trace_Parse > Outline then
                           Trace.Put_Line (Integer'Image (Current_Parser.Label) & ": succeed with zombies");
                        end if;
                        Current_Parser.Next;
                     else
                        Temp := Current_Parser;
                        Current_Parser.Next;
                        Shared_Parser.Parsers.Terminate_Parser (Temp, "zombie", Shared_Parser.Trace.all);
                     end if;
                     exit when Current_Parser.Is_Done;
                  end loop;

                  exit Main_Loop;

               else
                  --  More than one parser is active.
                  declare
                     use all type Parser_Lists.Cursor;
                     Error_Parser_Count     : Integer := (if Shared_Parser.Lexer.Errors.Length > 0 then 1 else 0);
                     Recover_Ops_Length     : Ada.Containers.Count_Type;
                     Min_Recover_Ops_Length : Ada.Containers.Count_Type := Ada.Containers.Count_Type'Last;
                     Min_Recover_Ops_Cur    : Parser_Lists.Cursor;
                  begin
                     Current_Parser := Shared_Parser.Parsers.First;
                     loop
                        if Current_Parser.Verb = Accept_It then
                           if Current_Parser.State_Ref.Errors.Length > 0 then
                              Error_Parser_Count := Error_Parser_Count + 1;
                           end if;
                           Current_Parser.Next;
                        else
                           Temp := Current_Parser;
                           Current_Parser.Next;
                           Shared_Parser.Parsers.Terminate_Parser (Temp, "zombie", Shared_Parser.Trace.all);
                        end if;
                        exit when Current_Parser.Is_Done;
                     end loop;

                     if Error_Parser_Count > 0 then
                        --  There was at least one error. We assume that caused the ambiguous
                        --  parse, and we pick the parser with the minimum recover ops length
                        --  to allow the parse to succeed. We terminate the other parsers so
                        --  the first parser executes actions.
                        --
                        --  Note all surviving parsers must have the same error count, or only
                        --  the one with the lowest would get here.
                        Current_Parser := Shared_Parser.Parsers.First;
                        loop
                           Recover_Ops_Length := Current_Parser.Max_Recover_Ops_Length;
                           if Recover_Ops_Length < Min_Recover_Ops_Length then
                              Min_Recover_Ops_Length := Recover_Ops_Length;
                              Min_Recover_Ops_Cur    := Current_Parser;
                           end if;
                           Current_Parser.Next;
                           exit when Current_Parser.Is_Done;
                        end loop;

                        Current_Parser := Shared_Parser.Parsers.First;
                        loop
                           if Current_Parser = Min_Recover_Ops_Cur then
                              Current_Parser.Next;
                           else
                              Temp := Current_Parser;
                              Current_Parser.Next;
                              Shared_Parser.Parsers.Terminate_Parser (Temp, "errors", Shared_Parser.Trace.all);
                           end if;
                           exit when Current_Parser.Is_Done;
                        end loop;

                        if Trace_Parse > Outline then
                           Trace.Put_Line ("ambiguous with error");
                        end if;

                        exit Main_Loop;

                     else
                        --  There were no previous errors. We allow the parse to fail, on the
                        --  assumption that an otherwise correct input should not yield an
                        --  ambiguous parse.
                        declare
                           Token : Base_Token renames Shared_Parser.Terminals (Shared_Parser.Terminals.Last_Index);
                        begin
                           raise WisiToken.Parse_Error with Error_Message
                             (Shared_Parser.Lexer.File_Name, Token.Line, Token.Column,
                              "Ambiguous parse:" & SAL.Base_Peek_Type'Image (Count) & " parsers active.");
                        end;
                     end if;
                  end;
               end if;
            end;

         when Reduce =>
            null;

         when Error =>
            --  All parsers errored; attempt recovery
            declare
               use all type McKenzie_Recover.Recover_Status;
               Recover_Result : McKenzie_Recover.Recover_Status := McKenzie_Recover.Recover_Status'First;
            begin
               --  Recover algorithms expect current token at
               --  Parsers(*).Current_Token, will set
               --  Parsers(*).Recover_Insert_Delete with new input tokens and
               --  deletions, adjust Parsers(*).Stack, and set
               --  Parsers(*).Current_Token and Parsers(*).Verb.

               if Shared_Parser.Enable_McKenzie_Recover then
                  if Trace_Parse > Outline then
                     Trace.Put_Line ("recover");
                  end if;
                  Recover_Result := McKenzie_Recover.Recover (Shared_Parser);

                  if Trace_Parse > Outline then
                     if Recover_Result = Success  then
                        Trace.Put_Line
                          ("recover: succeed, parser count" & SAL.Base_Peek_Type'Image (Shared_Parser.Parsers.Count));
                     else
                        Trace.Put_Line
                          ("recover: fail " & McKenzie_Recover.Recover_Status'Image (Recover_Result) &
                             ", parser count" & SAL.Base_Peek_Type'Image (Shared_Parser.Parsers.Count));
                     end if;
                  end if;

                  if Ada.Text_IO.Is_Open (Shared_Parser.Recover_Log_File) then
                     declare
                        use Ada.Text_IO;
                     begin
                        Put
                          (Shared_Parser.Recover_Log_File,
                           Ada.Calendar.Formatting.Image (Ada.Calendar.Clock) & " " &
                             McKenzie_Recover.Recover_Status'Image (Recover_Result) & " " &
                             SAL.Base_Peek_Type'Image (Shared_Parser.Parsers.Count) & " '" &
                             Shared_Parser.Lexer.File_Name & "'");

                        for Parser of Shared_Parser.Parsers loop
                           Put
                             (Shared_Parser.Recover_Log_File,
                              Integer'Image (Parser.Recover.Enqueue_Count) &
                                Integer'Image (Parser.Recover.Check_Count) & " " &
                                Boolean'Image (Parser.Recover.Success));
                        end loop;
                        New_Line (Shared_Parser.Recover_Log_File);
                        Flush (Shared_Parser.Recover_Log_File);
                     end;
                  end if;
               else
                  if Trace_Parse > Outline then
                     Trace.Put_Line ("recover disabled or not defined");
                  end if;
               end if;

               if Recover_Result = Success then
                  declare
                     Shift_Recover_Count : Integer := 0;
                  begin
                     for Parser_State of Shared_Parser.Parsers loop
                        Parser_State.Resume_Active          := True;
                        Parser_State.Conflict_During_Resume := False;

                        if Trace_Parse > Outline then
                           Trace.Put_Line
                             (Integer'Image (Parser_State.Label) & ": Current_Token " &
                                Parser_State.Tree.Image (Parser_State.Current_Token, Trace.Descriptor.all) &
                                " Shared_Token " & Image
                                  (Parser_State.Shared_Token, Shared_Parser.Terminals, Trace.Descriptor.all));

                           if Trace_Parse > Detail then
                              Shared_Parser.Trace.Put_Line
                                (Integer'Image (Parser_State.Label) & ": resume_active: True, token goal" &
                                   Token_Index'Image (Parser_State.Resume_Token_Goal));
                           end if;
                        end if;

                        case Parser_State.Verb is
                        when Shift_Recover =>
                           Shift_Recover_Count := Shift_Recover_Count + 1;

                           Parser_State.Zombie_Token_Count := 0;

                        when Reduce =>
                           Current_Verb := Reduce;

                           Parser_State.Zombie_Token_Count := 0;

                        when Error =>
                           --  Force this parser to be terminated.
                           if Shared_Parser.Enable_McKenzie_Recover then
                              Parser_State.Zombie_Token_Count := Shared_Parser.Table.McKenzie_Param.Check_Limit + 1;
                           end if;

                        when Shift =>
                           if Current_Verb /= Reduce then
                              Current_Verb := Shift;
                           end if;

                        when Pause | Accept_It =>
                           raise SAL.Programmer_Error;
                        end case;
                     end loop;

                     if Shift_Recover_Count > 0 then
                        Current_Verb := Shift_Recover;
                     end if;
                  end;

               else
                  --  Terminate with error. Parser_State has all the required info on
                  --  the original error (recorded by Error in Do_Action); report reason
                  --  recover failed.
                  for Parser_State of Shared_Parser.Parsers loop
                     Parser_State.Errors.Append
                       ((Label          => LR.Message,
                         First_Terminal => Trace.Descriptor.First_Terminal,
                         Last_Terminal  => Trace.Descriptor.Last_Terminal,
                         Recover        => <>,
                         Msg            => +"recover: fail " & McKenzie_Recover.Recover_Status'Image (Recover_Result)));
                  end loop;
                  raise WisiToken.Syntax_Error;
               end if;
            end;
         end case;

         --  We don't use 'for Parser_State of Parsers loop' here,
         --  because terminate on error and spawn on conflict require
         --  changing the parser list.
         Current_Parser := Shared_Parser.Parsers.First;
         Action_Loop :
         loop
            exit Action_Loop when Current_Parser.Is_Done;

            if Shared_Parser.Terminate_Same_State and
              Current_Verb in Shift | Shift_Recover and
              (for all Parser of Shared_Parser.Parsers => Parser.Recover_Insert_Delete.Count = 0)
            then
               Shared_Parser.Parsers.Duplicate_State (Current_Parser, Shared_Parser.Trace.all);
               --  If Duplicate_State terminated Current_Parser, Current_Parser now
               --  points to the next parser. Otherwise it is unchanged.
            end if;

            exit Action_Loop when Current_Parser.Is_Done;

            if Trace_Parse > Extra then
               Trace.Put_Line
                 ("current_verb: " & Parse_Action_Verbs'Image (Current_Verb) &
                    "," & Integer'Image (Current_Parser.Label) &
                    ".verb: " & Parse_Action_Verbs'Image (Current_Parser.Verb));
            end if;

            --  Each branch of the following 'if' calls either Current_Parser.Free
            --  (which advances to the next parser) or Current_Parser.Next.

            if Current_Parser.Verb = Error then
               --  This parser is a zombie; see Check_Error above.
               --
               --  Check to see if it is time to terminate it
               if Shared_Parser.Enable_McKenzie_Recover and then
                 Current_Parser.State_Ref.Zombie_Token_Count <= Shared_Parser.Table.McKenzie_Param.Check_Limit
               then
                  if Trace_Parse > Detail then
                     Trace.Put_Line (Integer'Image (Current_Parser.Label) & ": zombie");
                  end if;

                  Current_Parser.Next;
               else
                  Shared_Parser.Parsers.Terminate_Parser (Current_Parser, "zombie", Shared_Parser.Trace.all);
               end if;

            elsif Current_Parser.Verb = Current_Verb then
               if Trace_Parse > Extra then
                  Parser_Lists.Put_Top_10 (Trace, Current_Parser);
               end if;

               declare
                  State : Parser_Lists.Parser_State renames Current_Parser.State_Ref.Element.all;
               begin
                  Action := Action_For
                    (Table => Shared_Parser.Table.all,
                     State => State.Stack.Peek.State,
                     ID    => State.Tree.ID (State.Current_Token));
               end;

               if Action.Next /= null then
                  --  Conflict; spawn a new parser (before modifying Current_Parser
                  --  stack).

                  Current_Parser.State_Ref.Conflict_During_Resume := Current_Parser.State_Ref.Resume_Active;

                  if Shared_Parser.Parsers.Count = Shared_Parser.Max_Parallel then
                     --  If errors were recovered, terminate a parser that used the
                     --  highest cost solution.
                     declare
                        use all type WisiToken.Parse.LR.Parser_Lists.Cursor;
                        Max_Recover_Cost : Integer             := 0;
                        Max_Parser       : Parser_Lists.Cursor;
                        Cur              : Parser_Lists.Cursor := Shared_Parser.Parsers.First;
                     begin
                        loop
                           exit when Cur.Is_Done;
                           if Cur.Total_Recover_Cost > Max_Recover_Cost then
                              Max_Parser       := Cur;
                              Max_Recover_Cost := Cur.Total_Recover_Cost;
                           end if;
                           Cur.Next;
                        end loop;

                        if Max_Recover_Cost > 0 then
                           if Max_Parser = Current_Parser then
                              Current_Parser.Next;
                              Shared_Parser.Parsers.Terminate_Parser
                                (Current_Parser, "too many parsers; max error repair cost", Trace);
                              exit Action_Loop;
                           else
                              Shared_Parser.Parsers.Terminate_Parser
                                (Max_Parser, "too many parsers; max error repair cost", Trace);
                           end if;
                        end if;
                     end;
                  end if;

                  if Shared_Parser.Parsers.Count = Shared_Parser.Max_Parallel then
                     declare
                        Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
                        Token : Base_Token renames Shared_Parser.Terminals (Parser_State.Shared_Token);
                     begin
                        raise WisiToken.Parse_Error with Error_Message
                          (Shared_Parser.Lexer.File_Name, Token.Line, Token.Column,
                           "too many parallel parsers required in grammar state" &
                             State_Index'Image (Parser_State.Stack.Peek.State) &
                             "; simplify grammar, or increase max-parallel (" &
                             SAL.Base_Peek_Type'Image (Shared_Parser.Max_Parallel) & ")");
                     end;

                  else
                     if Trace_Parse > Outline then
                        Trace.Put_Line
                          (Integer'Image (Current_Parser.Label) & ": spawn" &
                             Integer'Image (Shared_Parser.Parsers.Last_Label + 1) & ", (" &
                             Trimmed_Image (1 + Integer (Shared_Parser.Parsers.Count)) & " active)");
                     end if;

                     Shared_Parser.Parsers.Prepend_Copy (Current_Parser);
                     Do_Action (Action.Next.Item, Shared_Parser.Parsers.First, Shared_Parser);

                     --  We must terminate error parsers immediately in order to avoid
                     --  zombie parsers during recovery.
                     declare
                        Temp : Parser_Lists.Cursor := Shared_Parser.Parsers.First;
                     begin
                        Check_Error (Temp);
                     end;
                  end if;
               end if;

               Do_Action (Action.Item, Current_Parser, Shared_Parser);
               Check_Error (Current_Parser);

            else
               --  Current parser is waiting for others to catch up
               Current_Parser.Next;
            end if;
         end loop Action_Loop;
      end loop Main_Loop;

      --  We don't raise Syntax_Error for lexer errors, since they are all
      --  recovered, either by inserting a quote, or by ignoring the
      --  character.
   exception
   when Syntax_Error | WisiToken.Parse_Error =>
      raise;

   when E : others =>
      declare
         Msg : constant String := Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E);
      begin
         --  Emacs displays errors in the *syntax-errors* buffer
         Shared_Parser.Parsers.First_State_Ref.Errors.Append
           ((Label          => LR.Message,
             First_Terminal => Trace.Descriptor.First_Terminal,
             Last_Terminal  => Trace.Descriptor.Last_Terminal,
             Recover        => <>,
             Msg            => +Msg));

         --  Emacs displays the exception message in the echo area; easy to miss
         raise WisiToken.Parse_Error with Msg;
      end;
   end Parse;

   overriding
   procedure Execute_Actions (Parser : in out LR.Parser.Parser)
   is
      use all type SAL.Base_Peek_Type;
      use all type Syntax_Trees.User_Data_Access;

      Descriptor : WisiToken.Descriptor renames Parser.Trace.Descriptor.all;

      procedure Process_Node
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Syntax_Trees.Valid_Node_Index)
      is
         use all type Syntax_Trees.Node_Label;
      begin
         if Tree.Label (Node) /= Nonterm then
            return;
         end if;

         declare
            use all type Syntax_Trees.Semantic_Action;
            Tree_Children : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Children (Node);
         begin
            Parser.User_Data.Reduce (Tree, Node, Tree_Children);

            if Tree.Action (Node) /= null then
               Tree.Action (Node) (Parser.User_Data.all, Tree, Node, Tree_Children);
            end if;
         end;
      end Process_Node;

   begin
      if Parser.User_Data /= null then
         if (for some Par of Parser.Parsers =>
               (for some Err of Par.Errors => Any (Err.Recover.Ops, Delete)))
         then
            if Parser.Parsers.Count > 1 then
               raise Syntax_Error with "ambiguous parse with deleted tokens; can't execute actions";
            end if;
            for Err of Parser.Parsers.First_State_Ref.Errors loop
               for Op of Err.Recover.Ops loop
                  case Op.Op is
                  when Delete =>
                     Parser.User_Data.Delete_Token (Op.Token_Index);
                  when others =>
                     null;
                  end case;
               end loop;
            end loop;
         end if;

         for Parser_State of Parser.Parsers loop
            if Trace_Action > Outline then
               Parser.Trace.Put_Line
                 (Integer'Image (Parser_State.Label) & ": root node: " & Parser_State.Tree.Image
                    (Parser_State.Tree.Root, Descriptor));
            end if;

            Parser_State.Tree.Process_Tree (Process_Node'Access);
         end loop;
      end if;
   end Execute_Actions;

   overriding function Any_Errors (Parser : in LR.Parser.Parser) return Boolean
   is
      use all type SAL.Base_Peek_Type;
      use all type Ada.Containers.Count_Type;
      Parser_State : Parser_Lists.Parser_State renames Parser.Parsers.First_Constant_State_Ref;
   begin
      pragma Assert (Parser_State.Tree.Flushed);
      return Parser.Parsers.Count > 1 or Parser_State.Errors.Length > 0 or Parser.Lexer.Errors.Length > 0;
   end Any_Errors;

   overriding procedure Put_Errors (Parser : in LR.Parser.Parser)
   is
      use all type SAL.Base_Peek_Type;
      use Ada.Text_IO;

      Parser_State : Parser_Lists.Parser_State renames Parser.Parsers.First_Constant_State_Ref;
      Descriptor   : WisiToken.Descriptor renames Parser.Trace.Descriptor.all;
   begin
      for Item of Parser.Lexer.Errors loop
         Put_Line
           (Current_Error,
            Parser.Lexer.File_Name & ":0:0: lexer unrecognized character at" & Buffer_Pos'Image (Item.Char_Pos));
      end loop;

      for Item of Parser_State.Errors loop
         case Item.Label is
         when Action =>
            declare
               Index : constant Base_Token_Index := Parser_State.Tree.Min_Terminal_Index (Item.Error_Token);
            begin
               if Index = Invalid_Token_Index then
                  --  Error_Token is virtual
                  Put_Line
                    (Current_Error,
                     Error_Message
                       (Parser.Lexer.File_Name, 1, 0,
                        "syntax error: expecting " & Image (Item.Expecting, Descriptor) &
                          ", found " & Image (Parser_State.Tree.ID (Item.Error_Token), Descriptor)));
               else
                  declare
                     Token : Base_Token renames Parser.Terminals (Index);
                  begin
                     Put_Line
                       (Current_Error,
                        Error_Message
                          (Parser.Lexer.File_Name, Token.Line, Token.Column,
                           "syntax error: expecting " & Image (Item.Expecting, Descriptor) &
                             ", found '" & Parser.Lexer.Buffer_Text (Token.Byte_Region) & "'"));
                  end;
               end if;
            end;
         when Check =>
            Put_Line
              (Current_Error,
               Parser.Lexer.File_Name & ":0:0: semantic check error: " &
                 Semantic_Checks.Image (Item.Check_Status, Descriptor));
         when Message =>
            Put_Line (Current_Error, -Item.Msg);
         end case;

         if Item.Recover.Stack.Depth /= 0 then
            Put_Line (Current_Error, "   recovered: " & Image (Item.Recover.Ops, Descriptor));
         end if;
      end loop;
   end Put_Errors;

end WisiToken.Parse.LR.Parser;
