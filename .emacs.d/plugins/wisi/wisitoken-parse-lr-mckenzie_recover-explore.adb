--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Free Software Foundation, Inc.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with WisiToken.Parse.LR.McKenzie_Recover.Parse;
package body WisiToken.Parse.LR.McKenzie_Recover.Explore is

   procedure Do_Shift
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Peek_Type;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type;
      Config            : in out          Configuration;
      State             : in              State_Index;
      ID                : in              Token_ID;
      Cost_Delta        : in              Integer)
   is
      use all type SAL.Base_Peek_Type;
      McKenzie_Param : McKenzie_Param_Type renames Shared.Table.McKenzie_Param;

      Op : constant Config_Op := (Insert, ID, Config.Current_Shared_Token);
   begin
      begin
         if Config.Current_Ops = No_Insert_Delete then
            Config.Ops.Append (Op);
         else
            Config.Ops.Insert (Op, Before => Config.Current_Ops);
            Config.Current_Ops := Config.Current_Ops + 1;
         end if;
      exception
      when SAL.Container_Full =>
         if Trace_McKenzie > Outline then
            Put_Line (Super.Trace.all, Super.Label (Parser_Index), "config.ops is full");
         end if;
         raise Bad_Config;
      end;

      if Cost_Delta = 0 then
         Config.Cost := Config.Cost + McKenzie_Param.Insert (ID);
      else
         --  Cost_Delta /= 0 comes from Try_Insert_Terminal when
         --  Minimal_Complete_Actions is useful. That doesn't mean it is better
         --  than any other solution, so don't let cost be 0.
         Config.Cost := Integer'Max (1, Config.Cost + McKenzie_Param.Insert (ID) + Cost_Delta);
      end if;

      Config.Error_Token.ID := Invalid_Token_ID;

      Config.Stack.Push ((State, Syntax_Trees.Invalid_Node_Index, (ID, Virtual => True, others => <>)));
      if Trace_McKenzie > Detail then
         Base.Put ("insert " & Image (ID, Super.Trace.Descriptor.all), Super, Shared, Parser_Index, Config);
      end if;

      Local_Config_Heap.Add (Config);
   end Do_Shift;

   function Do_Reduce_1
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Peek_Type;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type;
      Config            : in out          Configuration;
      Action            : in              Reduce_Action_Rec)
     return Non_Success_Status
   is
      use all type SAL.Base_Peek_Type;
      --  Perform Action on Config, setting Config.Check_Status. If that is
      --  not Ok, call Language_Fixes (which may enqueue configs),
      --  return Abandon. Otherwise return Continue.
      use all type Semantic_Checks.Check_Status_Label;
      use all type WisiToken.Parse.LR.Parser.Language_Fixes_Access;

      Table     : Parse_Table renames Shared.Table.all;
      Nonterm   : Recover_Token;
      New_State : Unknown_State_Index;
   begin
      Config.Check_Status := Parse.Reduce_Stack (Shared, Config.Stack, Action, Nonterm, Default_Virtual => True);
      case Config.Check_Status.Label is
      when Ok =>
         null;

      when Semantic_Checks.Error =>
         Config.Error_Token       := Nonterm;
         Config.Check_Token_Count := Action.Token_Count;

         if Shared.Language_Fixes /= null then
            Shared.Language_Fixes
              (Super.Trace.all, Shared.Lexer, Super.Label (Parser_Index), Shared.Table.all, Shared.Terminals.all,
               Super.Parser_State (Parser_Index).Tree, Local_Config_Heap,
               Config);
         end if;

         --  Finish the reduce; ignore the check fail.
         Config.Stack.Pop (SAL.Base_Peek_Type (Config.Check_Token_Count));
         Config.Error_Token.ID := Invalid_Token_ID;
         Config.Check_Status   := (Label => Ok);
      end case;

      if Config.Stack.Depth = 0 or else Config.Stack (1).State = Unknown_State then
         raise Bad_Config;
      end if;

      New_State := Goto_For (Table, Config.Stack (1).State, Action.Production.LHS);

      if New_State = Unknown_State then
         raise Bad_Config;
      end if;

      Config.Stack.Push ((New_State, Syntax_Trees.Invalid_Node_Index, Nonterm));
      return Continue;
   end Do_Reduce_1;

   procedure Do_Reduce_2
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Peek_Type;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type;
      Config            : in out          Configuration;
      Inserted_ID       : in              Token_ID;
      Cost_Delta        : in              Integer)
   is
      --  Perform reduce actions until shift Inserted_Token; if all succeed,
      --  add the final configuration to the heap. If a conflict is
      --  encountered, process the other action the same way. If a semantic
      --  check fails, enqueue possible solutions. For parse table error
      --  actions, or exception Bad_Config, just return.

      Table       : Parse_Table renames Shared.Table.all;
      Next_Action : Parse_Action_Node_Ptr;
   begin
      Next_Action := Action_For (Table, Config.Stack (1).State, Inserted_ID);

      if Next_Action.Next /= null then
         --  There is a conflict; create a new config to shift or reduce.
         declare
            New_Config : Configuration := Config;
            Action     : Parse_Action_Rec renames Next_Action.Next.Item;
         begin
            case Action.Verb is
            when Shift =>
               Do_Shift
                 (Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Action.State, Inserted_ID, Cost_Delta);

            when Reduce =>
               case Do_Reduce_1 (Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Action) is
               when Abandon =>
                  null;
               when Continue =>
                  Do_Reduce_2 (Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Inserted_ID, Cost_Delta);
               end case;

            when Accept_It =>
               raise SAL.Programmer_Error with "found test case for Do_Reduce Accept_It conflict";

            when Error =>
               null;
            end case;
         end;

         --  There can be only one conflict.
      end if;

      case Next_Action.Item.Verb is
      when Shift =>
         Do_Shift
           (Super, Shared, Parser_Index, Local_Config_Heap, Config, Next_Action.Item.State, Inserted_ID, Cost_Delta);

      when Reduce =>
         case Do_Reduce_1 (Super, Shared, Parser_Index, Local_Config_Heap, Config, Next_Action.Item) is
         when Abandon =>
            null;
         when Continue =>
            Do_Reduce_2 (Super, Shared, Parser_Index, Local_Config_Heap, Config, Inserted_ID, Cost_Delta);
         end case;

      when Accept_It =>
         raise SAL.Programmer_Error with "found test case for Do_Reduce Accept_It";

      when Error =>
         null;
      end case;

   exception
   when Bad_Config =>
      null;
   end Do_Reduce_2;

   function Fast_Forward
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type;
      Config            : in out          Configuration)
     return Non_Success_Status
   is
      --  Apply the ops in Config; they were inserted by some fix.
      --  Return Abandon if Config should be abandoned, otherwise Continue.
      --  Leaves Config.Error_Token, Config.Check_Status set.
      --
      --  If there are conflicts, all are parsed; if more than one succeed,
      --  all are enqueued in Local_Config_Heap, and this returns Abandon.

      use all type SAL.Base_Peek_Type;
      use all type Ada.Containers.Count_Type;

      Parse_Items : Parse.Parse_Item_Arrays.Vector;
   begin
      if Parse.Parse
        (Super, Shared, Parser_Index, Parse_Items, Config,
         Shared_Token_Goal => Invalid_Token_Index,
         All_Conflicts     => True,
         Trace_Prefix      => "fast_forward")
      then
         --  At least one config parsed without error, so continue with them.
         if Parse_Items.Length = 1 then
            Config := Parse_Items (1).Config;
            Config.Current_Ops := No_Insert_Delete;
            Config.Ops.Append ((Fast_Forward, Config.Current_Shared_Token));
            return Continue;
         else
            --  Enqueue all passing configs, abandon current.
            for Item of Parse_Items loop
               if Item.Parsed and Item.Config.Error_Token.ID = Invalid_Token_ID then
                  Item.Config.Ops.Append ((Fast_Forward, Item.Config.Current_Shared_Token));
                  Config.Current_Ops := No_Insert_Delete;
                  Local_Config_Heap.Add (Item.Config);

                  if Trace_McKenzie > Detail then
                     Base.Put ("fast forward conflict", Super, Shared, Parser_Index, Item.Config);
                  end if;
               end if;
            end loop;
            return Abandon;
         end if;

      else
         --  No parse item parsed without error. This indicates that Config.Ops
         --  (enqueued by language_fixes) did not fix all the problems; see
         --  test_mckenzie_recover Two_Missing_Ends. If it made progress we try
         --  more fixes.
         for Item of Parse_Items loop
            declare
               Parsed_Config : Configuration renames Item.Config;
               Remaining : SAL.Base_Peek_Type;
            begin
               if Parsed_Config.Current_Insert_Delete = No_Insert_Delete then
                  --  Insert_Delete contains only Deletes, and the next token caused an
                  --  error.
                  Parsed_Config.Ops.Append ((Fast_Forward, Config.Current_Shared_Token));
                  Local_Config_Heap.Add (Parsed_Config);
                  if Trace_McKenzie > Detail then
                     Base.Put ("fast forward failure", Super, Shared, Parser_Index, Item.Config);
                  end if;

               elsif Parsed_Config.Current_Insert_Delete = 1 then
                  --  No progress made; abandon config
                  null;

               else
                  --  Find fixes at the failure point. We don't reset
                  --  Config.Current_Insert_Delete here, to allow skipping Check.
                  --
                  --  If the unparsed ops are at Config.Current_Shared_Token, then new
                  --  ops applied in Process_One below must be inserted in Config.Ops
                  --  before the unparsed ops, so the final order applied to the full
                  --  parser is correct.
                  if Parsed_Config.Insert_Delete (Parsed_Config.Current_Insert_Delete).Token_Index =
                    Parsed_Config.Current_Shared_Token
                  then
                     Parsed_Config.Current_Ops := Parsed_Config.Ops.Last_Index;
                     Remaining := Parsed_Config.Insert_Delete.Last_Index - Parsed_Config.Current_Insert_Delete;
                     loop
                        exit when Remaining = 0;
                        if Parsed_Config.Ops (Parsed_Config.Current_Ops).Op in Insert_Delete_Op_Label then
                           Remaining := Remaining - 1;
                        end if;
                        Parsed_Config.Current_Ops := Parsed_Config.Current_Ops - 1;
                        if Parsed_Config.Current_Ops < Parsed_Config.Ops.First_Index then
                           if Trace_McKenzie > Outline then
                              Put_Line
                                (Super.Trace.all, Super.Label (Parser_Index),
                                 "Insert_Delete is out of sync with Ops");
                           end if;
                           raise Bad_Config;
                        end if;
                     end loop;
                  end if;

                  if Parsed_Config.Current_Insert_Delete > 1 then
                     if Parsed_Config.Current_Ops = No_Insert_Delete then
                        Parsed_Config.Ops.Append ((Fast_Forward, Config.Current_Shared_Token));
                     else
                        Parsed_Config.Ops.Insert
                          ((Fast_Forward, Config.Current_Shared_Token), Before => Parsed_Config.Current_Ops);
                        Parsed_Config.Current_Ops := Parsed_Config.Current_Ops + 1;
                     end if;
                  end if;
                  Local_Config_Heap.Add (Parsed_Config);
                  if Trace_McKenzie > Detail then
                     Base.Put ("fast forward failure", Super, Shared, Parser_Index, Item.Config);
                  end if;
               end if;
            end;
         end loop;
         return Abandon;
      end if;
   exception
   when SAL.Container_Full | Bad_Config =>
      return Abandon;
   end Fast_Forward;

   function Check
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in out          Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
     return Check_Status
   is
      use all type Ada.Containers.Count_Type;
      use all type Semantic_Checks.Check_Status_Label;
      use all type Parser.Language_Fixes_Access;

      Parse_Items : Parse.Parse_Item_Arrays.Vector;
   begin
      if Parse.Parse
        (Super, Shared, Parser_Index, Parse_Items, Config, Config.Resume_Token_Goal,
         All_Conflicts => False,
         Trace_Prefix  => "check")
      then
         Config.Error_Token.ID := Invalid_Token_ID;
         return Success;
      end if;

      --  All Parse_Items failed; enqueue them so Language_Fixes can try to fix them.
      declare
         Parse_Error_Found : Boolean := False;
      begin
         for Item of Parse_Items loop

            if Item.Config.Error_Token.ID /= Invalid_Token_ID and Item.Config.Check_Status.Label = Ok then
               Parse_Error_Found := True;

               if Item.Shift_Count = 0 or
                 ((Item.Config.Ops.Length > 0 and then
                     Item.Config.Ops (Item.Config.Ops.Last_Index).Op in Undo_Reduce | Push_Back) and
                    Item.Config.Current_Shared_Token = Config.Current_Shared_Token)
               then
                  --  (Item.config.ops is empty on the very first Check). This is the
                  --  same error Config originally found; report it in Config, so
                  --  Use_Minimal_Complete_Actions can see it.
                  Config.Error_Token  := Item.Config.Error_Token;
                  Config.Check_Status := (Label => Ok);
               end if;
            end if;

            if Item.Shift_Count > 0 and then
              (Item.Config.Check_Status.Label /= Ok or
                 Item.Config.Error_Token.ID /= Invalid_Token_ID)
            then
               --  Some progress was made; let Language_Fixes try to fix the new
               --  error.
               --
               --  This is abandoning the original location of the error, which may
               --  not be entirely fixed. So we increase the cost. See
               --  test_mckenzie_recover Loop_Bounds.
               Item.Config.Cost := Item.Config.Cost + 1;
               begin
                  if Item.Config.Ops (Item.Config.Ops.Last_Index).Op = Fast_Forward then
                     Item.Config.Ops (Item.Config.Ops.Last_Index).FF_Token_Index :=
                       Item.Config.Current_Shared_Token;
                  else
                     Item.Config.Ops.Append ((Fast_Forward, Item.Config.Current_Shared_Token));
                  end if;
               exception
               when SAL.Container_Full =>
                  raise Bad_Config;
               end;
               Local_Config_Heap.Add (Item.Config);
               if Trace_McKenzie > Detail then
                  Base.Put ("for Language_Fixes ", Super, Shared, Parser_Index, Item.Config);
               end if;
            end if;
         end loop;

         if Parse_Error_Found then
            return Continue;
         else
            --  Failed due to Semantic_Check
            if Shared.Language_Fixes = null then
               --  Only fix is to ignore the error
               return Continue;
            else
               --  Assume Language_Fixes handles this, not Explore.
               return Abandon;
            end if;
         end if;
      end;
   exception
   when Bad_Config =>
      return Abandon;
   end Check;

   procedure Try_Push_Back
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in out          Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
   is
      Trace          : WisiToken.Trace'Class renames Super.Trace.all;
      McKenzie_Param : McKenzie_Param_Type renames Shared.Table.McKenzie_Param;

      Token : constant Recover_Token := Config.Stack (1).Token;
   begin
      --  Try pushing back the stack top, to allow insert and other
      --  operations at that point.
      --
      --  Since we are not actually changing the source text, it is tempting
      --  to give this operation zero cost. But then we keep doing push_back
      --  forever, making no progress. So we give it a cost.

      if not Token.Virtual then
         --  If Virtual, this is from earlier in this recover session; no point
         --  in trying to redo it.

         declare
            New_Config : Configuration := Config;
         begin
            New_Config.Error_Token.ID := Invalid_Token_ID;
            New_Config.Check_Status   := (Label => WisiToken.Semantic_Checks.Ok);

            New_Config.Stack.Pop;

            if Token.Min_Terminal_Index = Invalid_Token_Index then
               --  Token is empty; Config.current_shared_token does not change, no
               --  cost increase.
               New_Config.Ops.Append ((Push_Back, Token.ID, New_Config.Current_Shared_Token));
            else
               New_Config.Cost := New_Config.Cost + McKenzie_Param.Push_Back (Token.ID);
               New_Config.Ops.Append ((Push_Back, Token.ID, Token.Min_Terminal_Index));
               New_Config.Current_Shared_Token := Token.Min_Terminal_Index;
            end if;

            Local_Config_Heap.Add (New_Config);

            if Trace_McKenzie > Detail then
               Base.Put ("push_back " & Image (Token.ID, Trace.Descriptor.all), Super, Shared,
                         Parser_Index, New_Config);
            end if;
         end;
      end if;
   exception
   when SAL.Container_Full =>
      if Trace_McKenzie > Outline then
         Put_Line (Super.Trace.all, Super.Label (Parser_Index), "config.ops is full");
      end if;
   end Try_Push_Back;

   procedure Insert_From_Action_List
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in              Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
   is
      use all type Ada.Containers.Count_Type;

      Table  : Parse_Table renames Shared.Table.all;
      EOF_ID : Token_ID renames Super.Trace.Descriptor.EOF_ID;

      --  Find terminal insertions from the current state's action_list to try.
      --
      --  We perform any needed reductions and one shift, so the config is
      --  in a consistent state, and enqueue the result. If there are any
      --  conflicts or semantic check fails encountered, they create other
      --  configs to enqueue.

      I : Action_List_Iterator := First (Table.States (Config.Stack (1).State));

      Cached_Config : Configuration;
      Cached_Action : Reduce_Action_Rec;
      Cached_Status : Non_Success_Status;
      --  Most of the time, all the reductions in a state are the same. So
      --  we cache the first result. This includes one reduction; if an
      --  associated semantic check failed, this does not include the fixes.
   begin
      loop
         exit when I.Is_Done;

         declare
            ID     : constant Token_ID := I.Symbol;
            Action : Parse_Action_Rec renames I.Action;
         begin
            if ID /= EOF_ID and then --  can't insert eof
              ID /= Invalid_Token_ID and then -- invalid when Verb = Error
              (Config.Ops.Length = 0 or else -- don't insert an id we just pushed back; we know that failed.
                 Config.Ops (Config.Ops.Last_Index) /= (Push_Back, ID, Config.Current_Shared_Token))
            then
               case Action.Verb is
               when Shift =>
                  declare
                     New_Config : Configuration := Config;
                  begin
                     New_Config.Error_Token.ID := Invalid_Token_ID;
                     New_Config.Check_Status   := (Label => WisiToken.Semantic_Checks.Ok);

                     Do_Shift
                       (Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Action.State, ID, Cost_Delta => 0);
                  end;

               when Reduce =>
                  if not Equal (Action, Cached_Action) then
                     declare
                        New_Config : Configuration := Config;
                     begin
                        New_Config.Error_Token.ID := Invalid_Token_ID;
                        New_Config.Check_Status   := (Label => WisiToken.Semantic_Checks.Ok);

                        Cached_Status := Do_Reduce_1
                          (Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Action);
                        Cached_Config := New_Config;
                        Cached_Action := Action;

                        if Cached_Status = Continue then
                           Do_Reduce_2
                             (Super, Shared, Parser_Index, Local_Config_Heap, New_Config, ID, Cost_Delta => 0);
                        end if;
                     end;

                  else
                     if Cached_Status = Continue then
                        declare
                           New_Config : Configuration := Cached_Config;
                        begin
                           Do_Reduce_2
                             (Super, Shared, Parser_Index, Local_Config_Heap, New_Config, ID, Cost_Delta => 0);
                        end;
                     end if;
                  end if;

               when Accept_It =>
                  raise SAL.Programmer_Error with "found test case for Process_One Accept_It";

               when Error =>
                  null;
               end case;
            end if;
         end;
         I.Next;
      end loop;
   end Insert_From_Action_List;

   procedure Insert_Minimal_Complete_Actions
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Orig_Config       : in              Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
   is
      use all type SAL.Base_Peek_Type;

      Table      : Parse_Table renames Shared.Table.all;
      Descriptor : WisiToken.Descriptor renames Super.Trace.Descriptor.all;

      Cost_Delta : constant Integer := -1;

      type Work_Type is record
         Config : Configuration;
         Complete_Actions : Minimal_Action_Lists.List;
      end record;

      package Work_Queues is new SAL.Gen_Unbounded_Definite_Queues (Work_Type);

      Work : Work_Queues.Queue;

      function Reduce_Only (Item : in Minimal_Action_Lists.List) return Minimal_Action_Lists.List
      is begin
         return Result : Minimal_Action_Lists.List do
            for Action of Item loop
               if Action.Verb = Reduce then
                  Result.Insert (Action);
               end if;
            end loop;
         end return;
      end Reduce_Only;

      function To_Reduce_Action (Item : in Minimal_Action) return Reduce_Action_Rec
      is begin
         return (Reduce, (Item.Nonterm, 0), null, null, Item.Token_Count);
      end To_Reduce_Action;

   begin
      Work.Put ((Orig_Config, Table.States (Orig_Config.Stack.Peek.State).Minimal_Complete_Actions));
      loop
         exit when Work.Length = 0;
         declare
            Item : constant Work_Type := Work.Get;
         begin
            for Action of Item.Complete_Actions loop
               case Action.Verb is
               when Reduce =>
                  --  Do a reduce, look at resulting state. Keep reducing until we can't
                  --  anymore (ignoring possible shifts along the way; we are looking
                  --  for the _minimal_ terminals to insert).
                  declare
                     use all type Ada.Containers.Count_Type;
                     New_Config    : Configuration     := Item.Config;
                     Reduce_Action : Reduce_Action_Rec := To_Reduce_Action (Action);

                     Temp_Actions : Minimal_Action_Lists.List;
                  begin
                     loop
                        case Do_Reduce_1 (Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Reduce_Action) is
                        when Abandon =>
                           goto Abandon_Reduce;

                        when Continue =>
                           if Trace_McKenzie > Extra then
                              Put_Line
                                (Super.Trace.all, Super.Label (Parser_Index), "Minimal_Complete_Actions reduce to" &
                                   State_Index'Image (New_Config.Stack.Peek.State) & ", " &
                                   Image (Reduce_Action.Production.LHS, Descriptor));
                           end if;

                           Temp_Actions := Reduce_Only
                             (Table.States (New_Config.Stack.Peek.State).Minimal_Complete_Actions);

                           exit when Temp_Actions.Length = 0;

                           Reduce_Action := To_Reduce_Action (Temp_Actions.Pop);

                           if Temp_Actions.Length > 0 then
                              if Trace_McKenzie > Extra then
                                 Put_Line
                                   (Super.Trace.all, Super.Label (Parser_Index),
                                    "Minimal_Complete_Actions add work item");
                              end if;
                              Work.Put ((New_Config, Temp_Actions));
                           end if;
                        end case;
                     end loop;

                     Insert_Minimal_Complete_Actions (Super, Shared, Parser_Index, New_Config, Local_Config_Heap);

                     <<Abandon_Reduce>>
                  end;

               when Shift =>
                  if Trace_McKenzie > Extra then
                     Put_Line
                       (Super.Trace.all, Super.Label (Parser_Index), "Minimal_Complete_Actions shift " &
                          Image (Action.ID, Descriptor));
                  end if;
                  declare
                     New_Config : Configuration := Item.Config;
                  begin
                     New_Config.Check_Status := (Label => WisiToken.Semantic_Checks.Ok);

                     Do_Shift
                       (Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Action.State, Action.ID,
                        Cost_Delta);
                  end;
               end case;
            end loop;
         end;
      end loop;
   end Insert_Minimal_Complete_Actions;

   procedure Try_Insert_Terminal
     (Super                      : not null access Base.Supervisor;
      Shared                     : not null access Base.Shared;
      Parser_Index               : in              SAL.Base_Peek_Type;
      Config                     : in              Configuration;
      Local_Config_Heap          : in out          Config_Heaps.Heap_Type;
      Use_Minimal_Complete_Actions : in              Boolean)
   is begin
      if Use_Minimal_Complete_Actions then
         Insert_Minimal_Complete_Actions (Super, Shared, Parser_Index, Config, Local_Config_Heap);
      else
         Insert_From_Action_List (Super, Shared, Parser_Index, Config, Local_Config_Heap);
      end if;

      --  It is tempting to use the Goto_List to find nonterms to insert.
      --  But that can easily lead to error states, and it turns out to be
      --  not useful, especially if the grammar has been relaxed so most
      --  expressions and lists can be empty.

   exception
   when Bad_Config =>
      null;
   end Try_Insert_Terminal;

   procedure Try_Insert_Quote
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in out          Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
   is
      use all type Parser.Language_String_ID_Set_Access;
      use all type Lexer.Error_Lists.Cursor;

      Descriptor  : WisiToken.Descriptor renames Shared.Trace.Descriptor.all;
      Check_Limit : Token_Index renames Shared.Table.McKenzie_Param.Check_Limit;

      Current_Line            : constant Line_Number_Type := Shared.Terminals.all (Config.Current_Shared_Token).Line;
      Lexer_Error_Token_Index : Base_Token_Index;
      Lexer_Error_Token       : Base_Token;

      function Recovered_Lexer_Error (Line : in Line_Number_Type) return Base_Token_Index
      is
         use WisiToken.Lexer;
         use WisiToken.Lexer.Error_Lists;
      begin
         --  We are assuming the list of lexer errors is short, so binary
         --  search would not be significantly faster.
         for Err of reverse Shared.Lexer.Errors loop
            if Err.Recover_Token /= Invalid_Token_Index and then
              Shared.Terminals.all (Err.Recover_Token).Line = Line
            then
               return Err.Recover_Token;
            end if;
         end loop;
         return Invalid_Token_Index;
      end Recovered_Lexer_Error;

      function String_ID_Set (String_ID : in Token_ID) return Token_ID_Set
      is begin
         return
           (if Shared.Language_String_ID_Set = null
            then (Descriptor.First_Terminal .. Descriptor.Last_Terminal => True)
            else Shared.Language_String_ID_Set (Descriptor, String_ID));
      end String_ID_Set;

      procedure String_Literal_In_Stack
        (New_Config        : in out Configuration;
         Matching          : in     SAL.Peek_Type;
         String_Literal_ID : in     Token_ID)
      is
         Saved_Shared_Token : constant Token_Index := New_Config.Current_Shared_Token;

         Tok         : Recover_Token;
         J           : Token_Index;
         Parse_Items : Parse.Parse_Item_Arrays.Vector;
      begin
         --  Matching is the index of a token on New_Config.Stack containing a string
         --  literal. Push back thru that token, then delete all tokens after
         --  the string literal to Saved_Shared_Token.
         for I in 1 .. Matching loop
            Tok := New_Config.Stack.Pop.Token;
            New_Config.Ops.Append ((Push_Back, Tok.ID, Tok.Min_Terminal_Index));
         end loop;

         New_Config.Current_Shared_Token := Tok.Min_Terminal_Index;

         --  Find last string literal in pushed back terminals.
         J := Saved_Shared_Token - 1;
         loop
            exit when Shared.Terminals.all (J).ID = String_Literal_ID;
            J := J - 1;
         end loop;

         begin
            if Parse.Parse
              (Super, Shared, Parser_Index, Parse_Items, New_Config,
               Shared_Token_Goal => J,
               All_Conflicts     => False,
               Trace_Prefix      => "insert quote parse pushback")
            then
               --  The non-deleted tokens parsed without error. We don't care if any
               --  conflicts were encountered; we are not using the parse result.
               New_Config := Parse_Items (1).Config;
               New_Config.Ops.Append ((Fast_Forward, New_Config.Current_Shared_Token));
            else
               raise SAL.Programmer_Error;
            end if;
         exception
         when Bad_Config =>
            raise SAL.Programmer_Error;
         end;
         J := New_Config.Current_Shared_Token; -- parse result
         loop
            exit when J = Saved_Shared_Token;
            New_Config.Ops.Append ((Delete, Shared.Terminals.all (J).ID, J));
            J := J + 1;
         end loop;

         New_Config.Current_Shared_Token := Saved_Shared_Token;

      end String_Literal_In_Stack;

      procedure Finish
        (Label       : in     String;
         New_Config  : in out Configuration;
         First, Last : in     Base_Token_Index)
      is begin
         --  Delete tokens First .. Last; either First - 1 or Last + 1 should
         --  be a String_Literal. Leave Current_Shared_Token at Last + 1.

         New_Config.Error_Token.ID := Invalid_Token_ID;
         New_Config.Check_Status   := (Label => WisiToken.Semantic_Checks.Ok);

         --  This is a guess, so we give it a nominal cost
         New_Config.Cost := New_Config.Cost + 1;

         for I in First .. Last loop
            if New_Config.Ops.Is_Full then
               if Trace_McKenzie > Outline then
                  Put_Line (Super.Trace.all, Super.Label (Parser_Index), "config.ops is full");
               end if;
               raise Bad_Config;
            end if;
            New_Config.Ops.Append ((Delete, Shared.Terminals.all (I).ID, I));
         end loop;
         New_Config.Current_Shared_Token := Last + 1;

         --  Allow insert/delete tokens
         New_Config.Ops.Append ((Fast_Forward, New_Config.Current_Shared_Token));

         if New_Config.Resume_Token_Goal - Check_Limit < New_Config.Current_Shared_Token then
            New_Config.Resume_Token_Goal := New_Config.Current_Shared_Token + Check_Limit;
            if Trace_McKenzie > Detail then
               Put_Line
                 (Super.Trace.all, Super.Label (Parser_Index), "resume_token_goal:" & Token_Index'Image
                    (New_Config.Resume_Token_Goal));
            end if;
         end if;

         if Trace_McKenzie > Detail then
            Base.Put ("insert missing quote " & Label & " ", Super, Shared, Parser_Index, New_Config);
         end if;
      end Finish;

   begin
      --  When the lexer finds an unbalanced quote, it inserts a virtual
      --  balancing quote at the same character position as the unbalanced
      --  quote, returning an empty string literal token there. The parser
      --  does not see that as an error; it encounters a syntax error
      --  before, at, or after that string literal.
      --
      --  Here we assume the parse error in Config.Error_Token is due to
      --  putting the balancing quote in the wrong place, and attempt to
      --  find a better place to put the balancing quote. Then all tokens
      --  from the balancing quote to the unbalanced quote are now part of a
      --  string literal, so delete them, leaving just the string literal
      --  created by Lexer error recovery.

      --  First we check to see if there is an unbalanced quote in the
      --  current line; if not, just return. Some lexer errors are for other
      --  unrecognized characters; see ada_mode-recover_bad_char.adb.
      --
      --  An alternate strategy is to treat the lexer error as a parse error
      --  immediately, but that complicates the parse logic.

      Config.String_Quote_Checked := Current_Line;

      Lexer_Error_Token_Index := Recovered_Lexer_Error (Current_Line);

      if Lexer_Error_Token_Index = Invalid_Token_Index then
         return;
      end if;

      Lexer_Error_Token := Shared.Terminals.all (Lexer_Error_Token_Index);

      --  It is not possible to tell where the best place to put the
      --  balancing quote is, so we always try all reasonable places.

      if Lexer_Error_Token.Byte_Region.First = Config.Error_Token.Byte_Region.First then
         --  The parse error token is the string literal at the lexer error.
         --
         --  case a: Insert the balancing quote somewhere before the error
         --  point. There is no way to tell how far back to put the balancing
         --  quote, so we just do one non-empty token. See
         --  test_mckenzie_recover.adb String_Quote_0. So far we have not found
         --  a test case for more than one token.
         declare
            New_Config : Configuration := Config;
            Token      : Recover_Token;
         begin
            loop
               Token := New_Config.Stack.Pop.Token;
               if Token.Byte_Region /= Null_Buffer_Region then
                  New_Config.Ops.Append ((Push_Back, Token.ID, Token.Min_Terminal_Index));
                  exit;
               end if;
            end loop;

            Finish ("a", New_Config, Token.Min_Terminal_Index, Config.Current_Shared_Token - 1);
            Local_Config_Heap.Add (New_Config);
         end;

         --  Note that it is not reasonable to insert a quote after the error
         --  in this case. If that were the right solution, the parser error
         --  token would not be the lexer repaired string literal, since a
         --  string literal would be legal here.

      elsif Lexer_Error_Token.Byte_Region.First < Config.Error_Token.Byte_Region.First then
         --  The unbalanced quote is before the parse error token; see
         --  test_mckenzie_recover.adb String_Quote_2.
         --
         --  The missing quote belongs after the parse error token, before or
         --  at the end of the current line; try inserting it at the end of
         --  the current line.
         --
         --  The lexer repaired string literal may be in a reduced token on the
         --  stack.

         declare
            use all type SAL.Base_Peek_Type;
            Matching : SAL.Peek_Type := 1;
         begin
            Find_Descendant_ID
              (Super.Parser_State (Parser_Index).Tree, Config, Lexer_Error_Token.ID,
               String_ID_Set (Lexer_Error_Token.ID), Matching);

            if Matching = Config.Stack.Depth then
               --  String literal is in a virtual nonterm; give up. So far this only
               --  happens in a high cost non critical config.
               if Trace_McKenzie > Detail then
                  Put_Line
                    (Super.Trace.all, Super.Label (Parser_Index), "abandon missing quote b; string literal in virtual");
               end if;
               return;
            end if;

            declare
               New_Config : Configuration := Config;
            begin
               String_Literal_In_Stack (New_Config, Matching, Lexer_Error_Token.ID);

               Finish
                 ("b", New_Config, Config.Current_Shared_Token, Shared.Line_Begin_Token.all (Current_Line + 1) - 1);
               Local_Config_Heap.Add (New_Config);
            end;
         end;

      else
         --  The unbalanced quote is after the parse error token.

         --  case c: Assume a missing quote belongs immediately before the current token.
         --  See test_mckenzie_recover.adb String_Quote_3.
         declare
            New_Config : Configuration := Config;
         begin
            Finish ("c", New_Config, Config.Current_Shared_Token, Lexer_Error_Token_Index - 1);
            Local_Config_Heap.Add (New_Config);
         exception
         when Bad_Config =>
            null;
         end;

         --  case d: Assume a missing quote belongs somewhere farther before
         --  the current token; try one non-empty (as in case a above). See
         --  test_mckenzie_recover.adb String_Quote_4.
         declare
            New_Config : Configuration := Config;
            Token      : Recover_Token;
         begin
            loop
               Token := New_Config.Stack.Pop.Token;
               if Token.Byte_Region /= Null_Buffer_Region then
                  New_Config.Ops.Append ((Push_Back, Token.ID, Token.Min_Terminal_Index));
                  exit;
               end if;
            end loop;

            Finish ("d", New_Config, Token.Min_Terminal_Index, Lexer_Error_Token_Index - 1);
            Local_Config_Heap.Add (New_Config);
         exception
         when Bad_Config =>
            null;
         end;

         --  case e: Assume the actual error is an extra quote that terminates
         --  an intended string literal early, in which case there is a token
         --  on the stack containing the string literal that should be extended
         --  to the found quote. See test_mckenzie_recover.adb String_Quote_1.
         declare
            use all type SAL.Base_Peek_Type;
            Matching : SAL.Peek_Type := 1;
         begin
            --  Lexer_Error_Token is a string literal; find a matching one.
            Find_Descendant_ID
              (Super.Parser_State (Parser_Index).Tree, Config, Lexer_Error_Token.ID, String_ID_Set
                 (Lexer_Error_Token.ID), Matching);

            if Matching = Config.Stack.Depth then
               --  No matching string literal, so this case does not apply.
               null;
            else
               declare
                  New_Config : Configuration := Config;
               begin
                  String_Literal_In_Stack (New_Config, Matching, Lexer_Error_Token.ID);

                  Finish ("e", New_Config, Config.Current_Shared_Token, Lexer_Error_Token_Index);
                  Local_Config_Heap.Add (New_Config);
               end;
            end if;
         end;
      end if;
   exception
   when SAL.Container_Full =>
      if Trace_McKenzie > Outline then
         Put_Line (Super.Trace.all, Super.Label (Parser_Index), "config.ops is full");
      end if;

   when Bad_Config =>
      null;
   end Try_Insert_Quote;

   procedure Try_Delete_Input
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in out          Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
   is
      --  Try deleting (= skipping) the current shared input token.
      Trace       : WisiToken.Trace'Class renames Super.Trace.all;
      EOF_ID      : Token_ID renames Trace.Descriptor.EOF_ID;
      Check_Limit : Token_Index renames Shared.Table.McKenzie_Param.Check_Limit;

      McKenzie_Param : McKenzie_Param_Type renames Shared.Table.McKenzie_Param;

      ID : constant Token_ID := Shared.Terminals.all (Config.Current_Shared_Token).ID;
   begin
      if ID /= EOF_ID then
         --  can't delete EOF
         declare
            New_Config : Configuration := Config;
         begin
            New_Config.Error_Token.ID := Invalid_Token_ID;
            New_Config.Check_Status   := (Label => WisiToken.Semantic_Checks.Ok);

            New_Config.Cost := New_Config.Cost + McKenzie_Param.Delete (ID);

            if Match_Since_FF (Config.Ops, (Push_Back, ID, Config.Current_Shared_Token))
            then
               --  We are deleting a push_back; cancel the push_back cost, to make
               --  this the same as plain deleting.
               New_Config.Cost := Natural'Max (Natural'First, New_Config.Cost - McKenzie_Param.Push_Back (ID));
            end if;

            New_Config.Ops.Append ((Delete, ID, Config.Current_Shared_Token));
            New_Config.Current_Shared_Token := New_Config.Current_Shared_Token + 1;

            if New_Config.Resume_Token_Goal - Check_Limit < New_Config.Current_Shared_Token then
               New_Config.Resume_Token_Goal := New_Config.Current_Shared_Token + Check_Limit;
            end if;

            Local_Config_Heap.Add (New_Config);

            if Trace_McKenzie > Detail then
               Base.Put
                 ("delete " & Image (ID, Trace.Descriptor.all), Super, Shared, Parser_Index, New_Config);
            end if;
         end;
      end if;
   exception
   when SAL.Container_Full =>
      if Trace_McKenzie > Outline then
         Put_Line (Super.Trace.all, Super.Label (Parser_Index), "config.ops is full");
      end if;
   end Try_Delete_Input;

   procedure Process_One
     (Super         : not null access Base.Supervisor;
      Shared        : not null access Base.Shared;
      Config_Status : out             Base.Config_Status)
   is
      --  Get one config from Super, check to see if it is a viable
      --  solution. If not, enqueue variations to check.

      use all type Base.Config_Status;
      use all type Parser.Language_Fixes_Access;
      use all type SAL.Base_Peek_Type;
      use all type Semantic_Checks.Check_Status_Label;

      Trace      : WisiToken.Trace'Class renames Super.Trace.all;
      Descriptor : WisiToken.Descriptor renames Super.Trace.Descriptor.all;
      Table      : Parse_Table renames Shared.Table.all;

      Parser_Index : SAL.Base_Peek_Type;
      Config       : Configuration;

      Local_Config_Heap : Config_Heaps.Heap_Type;
      --  We collect all the variants to enqueue, then deliver them all at
      --  once to Super, to minimizes task interactions.

      Use_Minimal_Complete_Actions : Boolean := False;

      function Allow_Insert_Terminal (Config : in Configuration) return Boolean
      is
         use all type Ada.Containers.Count_Type;
         use all type WisiToken.Parse.LR.Parser.Language_Use_Minimal_Complete_Actions_Access;
      begin
         if Shared.Language_Use_Minimal_Complete_Actions = null then
            return None_Since_FF (Config.Ops, Delete);
         end if;

         Use_Minimal_Complete_Actions := Shared.Language_Use_Minimal_Complete_Actions
           (Current_Token_ID_Peek
              (Shared.Terminals.all, Config.Current_Shared_Token, Config.Insert_Delete, Config.Current_Insert_Delete),
            Config);

         if Use_Minimal_Complete_Actions then
            if Table.States (Config.Stack.Peek.State).Minimal_Complete_Actions.Length = 0 then
               --  This happens when there is an extra token after an acceptable
               --  grammar statement. There is no production to complete, so try
               --  other things.
               Use_Minimal_Complete_Actions := False;
            else
               if Trace_McKenzie > Detail then
                  Put_Line (Super.Trace.all, Super.Label (Parser_Index), "use Minimal_Complete_Actions");
               end if;
               return True;
            end if;
         end if;
         return None_Since_FF (Config.Ops, Delete);
      end Allow_Insert_Terminal;

   begin
      Super.Get (Parser_Index, Config, Config_Status);

      if Config_Status = All_Done then
         return;
      end if;

      if Trace_McKenzie > Extra then
         Base.Put ("dequeue", Super, Shared, Parser_Index, Config);
         Put_Line (Trace, Super.Label (Parser_Index), "stack: " & Image (Config.Stack, Trace.Descriptor.all));
      end if;

      if Config.Current_Insert_Delete = 1 then
         --  If Config.Current_Insert_Delete > 1 then Fast_Forward failed on this
         --  config; don't fast_forward again.

         case Fast_Forward (Super, Shared, Parser_Index, Local_Config_Heap, Config) is
         when Abandon =>
            --  We know Local_Config_Heap is empty; just tell
            --  Super we are done working.
            Super.Put (Parser_Index, Local_Config_Heap);
            return;
         when Continue =>
            --  We don't increase cost for this Fast_Forward, since it is due to a
            --  Language_Fixes.
            null;
         end case;
      end if;

      if Config.Error_Token.ID /= Invalid_Token_ID then
         if Shared.Language_Fixes = null then
            null;
         else
            Shared.Language_Fixes
              (Trace, Shared.Lexer, Super.Label (Parser_Index), Shared.Table.all,
               Shared.Terminals.all, Super.Parser_State (Parser_Index).Tree, Local_Config_Heap,
               Config);

            --  The solutions provided by Language_Fixes should be lower cost than
            --  others (typically 0), so they will be checked first.

            if Config.Check_Status.Label = Ok then
               --  Parse table Error action.
               --
               --  We don't clear Config.Error_Token here, because Try_Insert calls
               --  Language_Use_Minimal_Complete_Actions, which needs it. We only clear it
               --  when a parse results in no error (or a different error), or a
               --  push_back moves the Current_Token.
               null;

            else
               --  Assume "ignore check error" is a viable solution. But give it a
               --  cost, so a solution provided by Language_Fixes is preferred.

               declare
                  New_State : Unknown_State_Index;
               begin
                  Config.Cost := Config.Cost + Table.McKenzie_Param.Ignore_Check_Fail;

                  --  finish reduce.
                  Config.Stack.Pop (SAL.Base_Peek_Type (Config.Check_Token_Count));

                  New_State := Goto_For (Table, Config.Stack (1).State, Config.Error_Token.ID);

                  if New_State = Unknown_State then
                     if Config.Stack.Depth = 1 then
                        --  Stack is empty, and we did not get Accept; really bad syntax got
                        --  us here; abandon this config. See ada_mode-recover_bad_char.adb.
                        Super.Put (Parser_Index, Local_Config_Heap);
                        return;
                     else
                        raise SAL.Programmer_Error with
                          "process_one found test case for new_state = Unknown; old state " &
                          Trimmed_Image (Config.Stack (1).State) & " nonterm " & Image
                            (Config.Error_Token.ID, Trace.Descriptor.all);
                     end if;
                  end if;

                  Config.Stack.Push ((New_State, Syntax_Trees.Invalid_Node_Index, Config.Error_Token));

                  --  We must clear Check_Status here, so if this config comes back
                  --  here, we don't try to reduce the stack again. We also clear
                  --  Error_Token, so this doesn't look like a parse error.
                  Config.Check_Status := (Label => Ok);

                  Config.Error_Token.ID := Invalid_Token_ID;
               end;
            end if;
         end if;
      end if;

      if Config.Current_Insert_Delete > 1 then
         --  Fast_Forward failed on this config; no need to check it. Remove
         --  already parsed items from Insert_Delete, setting
         --  Current_Insert_Delete to 1, so it will be checked after the Ops
         --  applied below.
         Config.Insert_Delete.Delete_First (Config.Current_Insert_Delete - 1);
         Config.Current_Insert_Delete := 1;
      else
         case Check (Super, Shared, Parser_Index, Config, Local_Config_Heap) is
         when Success =>
            Super.Success (Parser_Index, Config, Local_Config_Heap);
            return;

         when Abandon =>
            Super.Put (Parser_Index, Local_Config_Heap);
            return;

         when Continue =>
            null;

         end case;
      end if;

      if Trace_McKenzie > Detail then
         Base.Put ("continuing", Super, Shared, Parser_Index, Config);
         if Trace_McKenzie > Extra then
            Put_Line (Trace, Super.Label (Parser_Index), "stack: " & Image (Config.Stack, Trace.Descriptor.all));
         end if;
      end if;

      --  Grouping these operations ensures that there are no duplicate
      --  solutions found. We reset the grouping after each fast_forward.
      --
      --  All possible permutations will be explored.

      if None_Since_FF (Config.Ops, Delete) and
        None_Since_FF (Config.Ops, Insert) and
        Config.Stack.Depth > 1 -- can't delete the first state
      then
         Try_Push_Back (Super, Shared, Parser_Index, Config, Local_Config_Heap);
      end if;

      if Allow_Insert_Terminal (Config) then
         Try_Insert_Terminal (Super, Shared, Parser_Index, Config, Local_Config_Heap, Use_Minimal_Complete_Actions);
      end if;

      if Config.Current_Insert_Delete = No_Insert_Delete then
         if Config.Check_Status.Label = Ok and
           (Descriptor.String_1_ID /= Invalid_Token_ID or Descriptor.String_2_ID /= Invalid_Token_ID) and
           (Config.String_Quote_Checked = Invalid_Line_Number or else
              Config.String_Quote_Checked < Shared.Terminals.all (Config.Current_Shared_Token).Line)
         then
            --  See if there is a mismatched quote. The solution is to delete
            --  tokens, replacing them with a string literal. So we try this when
            --  it is ok to try delete.
            Try_Insert_Quote (Super, Shared, Parser_Index, Config, Local_Config_Heap);
         end if;

         Try_Delete_Input (Super, Shared, Parser_Index, Config, Local_Config_Heap);
      end if;

      Super.Put (Parser_Index, Local_Config_Heap);
   exception
   when others =>
      --  Just abandon this config; tell Super we are done.
      Super.Put (Parser_Index, Local_Config_Heap);
      if Debug_Mode then
         raise;
      end if;
   end Process_One;

end WisiToken.Parse.LR.McKenzie_Recover.Explore;
