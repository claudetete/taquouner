--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 - 2005, 2008 - 2015, 2017, 2018 Free Software Foundation, Inc.
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHAN- TABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Containers;
with Ada.Text_IO;
with SAL.Gen_Definite_Doubly_Linked_Lists;
package body WisiToken.Generate.LR.LALR_Generate is

   package Item_List_Cursor_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists (LR1_Items.Item_Lists.Cursor);

   type Item_Map is record
      --  Keep track of all copies of Item, so Lookaheads can be updated
      --  after they are initially copied.
      From : LR1_Items.Item_Lists.Cursor;
      To   : Item_List_Cursor_Lists.List;
   end record;

   package Item_Map_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists (Item_Map);
   --  IMPROVEME: should be a 3D array indexed by Prod, rhs_index,
   --  dot_index. But it's not broken or slow, so we're not fixing it.

   function Propagate_Lookahead (Descriptor : in WisiToken.Descriptor) return access LR1_Items.Lookahead
   is begin
      return new Token_ID_Set'(LR1_Items.To_Lookahead (Descriptor.Last_Lookahead, Descriptor));
   end Propagate_Lookahead;

   function Null_Lookahead (Descriptor : in WisiToken.Descriptor) return access LR1_Items.Lookahead
   is begin
      return new Token_ID_Set'(Descriptor.First_Terminal .. Descriptor.Last_Lookahead => False);
   end Null_Lookahead;

   ----------
   --  Debug output

   procedure Put
     (Grammar      : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor   : in WisiToken.Descriptor;
      Propagations : in Item_Map_Lists.List)
   is
      use LR1_Items.Item_Lists;
   begin
      for Map of Propagations loop
         Ada.Text_IO.Put ("From ");
         LR1_Items.Put (Grammar, Descriptor, Constant_Ref (Map.From), Show_Lookaheads => True);
         Ada.Text_IO.New_Line;

         for Cur of Map.To loop
            Ada.Text_IO.Put ("To   ");
            LR1_Items.Put (Grammar, Descriptor, Constant_Ref (Cur), Show_Lookaheads => True);
            Ada.Text_IO.New_Line;
         end loop;
      end loop;
   end Put;

   ----------
   --  Generate utils

   function LALR_Goto_Transitions
     (Kernel            : in LR1_Items.Item_Set;
      Symbol            : in Token_ID;
      First_Nonterm_Set : in Token_Array_Token_Set;
      Grammar           : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor        : in WisiToken.Descriptor)
     return LR1_Items.Item_Set
   is
      use Token_ID_Arrays;
      use LR1_Items;
      use LR1_Items.Item_Lists;

      Goto_Set : Item_Set;
      Dot_ID   : Token_ID;
   begin
      for Item of Kernel.Set loop

         if Has_Element (Item.Dot) then

            Dot_ID := Element (Item.Dot);
            --  ID of token after Dot

            --  If Symbol = EOF_Token, this is the start symbol accept
            --  production; don't need a kernel with dot after EOF.
            if (Dot_ID = Symbol and Symbol /= Descriptor.EOF_ID) and then
              not Has_Element (Find (Item.Prod, Next (Item.Dot), Goto_Set))
            then
               Goto_Set.Set.Insert
                 ((Prod       => Item.Prod,
                   Dot        => Next (Item.Dot),
                   Lookaheads => new Token_ID_Set'(Item.Lookaheads.all)));

               if Trace_Generate > Detail then
                  Ada.Text_IO.Put_Line ("LALR_Goto_Transitions 1 " & Image (Symbol, Descriptor));
                  Put (Grammar, Descriptor, Goto_Set);
               end if;
            end if;

            if Dot_ID in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal and then
              First_Nonterm_Set (Dot_ID, Symbol)
            then
               --  Find the production(s) that create Dot_ID with first token Symbol
               --  and put them in.
               --
               --  This is equivalent to Filter (LR1_Items.Closure, In_Kernel), but
               --  more efficient, because it does not generate non-kernel items. See
               --  Test/compare_goto_transitions.adb.
               for Prod of Grammar loop
                  for RHS_2_I in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index loop
                     declare
                        P_ID  : constant Production_ID          := (Prod.LHS, RHS_2_I);
                        Dot_2 : constant Token_ID_Arrays.Cursor := Prod.RHSs (RHS_2_I).Tokens.First;
                     begin
                        if (Dot_ID = Prod.LHS or First_Nonterm_Set (Dot_ID, Prod.LHS)) and
                          (Has_Element (Dot_2) and then Element (Dot_2) = Symbol)
                        then
                           if not Has_Element (Find (P_ID, Next (Dot_2), Goto_Set)) then
                              Goto_Set.Set.Insert
                                ((Prod       => P_ID,
                                  Dot        => Next (Dot_2),
                                  Lookaheads => Null_Lookahead (Descriptor)));

                              if Trace_Generate > Detail then
                                 Ada.Text_IO.Put_Line ("LALR_Goto_Transitions 2 " & Image (Symbol, Descriptor));
                                 Put (Grammar, Descriptor, Goto_Set);
                              end if;

                              --  else already in goto set
                           end if;
                        end if;
                     end;
                  end loop;
               end loop;
            end if;
         end if; -- item.dot /= null
      end loop;

      return Goto_Set;
   end LALR_Goto_Transitions;

   function LALR_Kernels
     (Grammar           : in WisiToken.Productions.Prod_Arrays.Vector;
      First_Nonterm_Set : in Token_Array_Token_Set;
      Descriptor        : in WisiToken.Descriptor)
     return LR1_Items.Item_Set_List
   is
      use all type Token_ID_Arrays.Cursor;
      use all type Ada.Containers.Count_Type;
      use LR1_Items;

      First_State_Index : constant State_Index := 0;
      Kernels           : LR1_Items.Item_Set_List;
      Kernel_Tree       : LR1_Items.Item_Set_Trees.Tree; -- for fast find
      States_To_Check   : State_Index_Queues.Queue;
      Checking_State    : State_Index;

      New_Item_Set : Item_Set :=
        (Set            => Item_Lists.To_List
           ((Prod       => (Grammar.First_Index, 0),
             Dot        => Grammar (Grammar.First_Index).RHSs (0).Tokens.First,
             Lookaheads => Null_Lookahead (Descriptor))),
         Goto_List      => <>,
         Dot_IDs        => <>,
         State          => First_State_Index);

      Found_State : Unknown_State_Index;
   begin
      Kernels.Set_First (First_State_Index);

      Add (New_Item_Set, Kernels, Kernel_Tree, Descriptor, Include_Lookaheads => False);

      States_To_Check.Put (First_State_Index);
      loop
         exit when States_To_Check.Is_Empty;
         Checking_State := States_To_Check.Get;

         if Trace_Generate > Detail then
            Ada.Text_IO.Put ("Checking ");
            Put (Grammar, Descriptor, Kernels (Checking_State));
         end if;

         for Symbol in Descriptor.First_Terminal .. Descriptor.Last_Nonterminal loop
            --  LALR_Goto_Transitions does _not_ ignore Symbol if it is not in
            --  Item_Set.Dot_IDs, so we can't iterate on that here as we do in
            --  LR1_Generate.

            New_Item_Set := LALR_Goto_Transitions
              (Kernels (Checking_State), Symbol, First_Nonterm_Set, Grammar, Descriptor);

            if New_Item_Set.Set.Length > 0 then

               Found_State := Find (New_Item_Set, Kernel_Tree, Match_Lookaheads => False);

               if Found_State = Unknown_State then
                  New_Item_Set.State := Kernels.Last_Index + 1;

                  States_To_Check.Put (New_Item_Set.State);

                  Add (New_Item_Set, Kernels, Kernel_Tree, Descriptor, Include_Lookaheads => False);

                  if Trace_Generate > Detail then
                     Ada.Text_IO.Put_Line ("  adding state" & Unknown_State_Index'Image (Kernels.Last_Index));
                  end if;

                  Kernels (Checking_State).Goto_List.Insert ((Symbol, Kernels.Last_Index));
               else

                  --  If there's not already a goto entry between these two sets, create one.
                  if not Is_In ((Symbol, Found_State), Kernels (Checking_State).Goto_List) then
                     if Trace_Generate > Detail then
                        Ada.Text_IO.Put_Line
                          ("  state" & Unknown_State_Index'Image (Checking_State) &
                             " adding goto on " & Image (Symbol, Descriptor) & " to state" &
                             Unknown_State_Index'Image (Found_State));

                     end if;

                     Kernels (Checking_State).Goto_List.Insert ((Symbol, Found_State));
                  end if;
               end if;
            end if;
         end loop;
      end loop;

      if Trace_Generate > Detail then
         Ada.Text_IO.New_Line;
      end if;

      return Kernels;
   end LALR_Kernels;

   --  Add a propagation entry (if it doesn't already exist) from From in
   --  From_Set to To_Item.
   procedure Add_Propagation
     (From         : in     LR1_Items.Item;
      From_Set     : in     LR1_Items.Item_Set;
      To_Item      : in     LR1_Items.Item_Lists.Cursor;
      Propagations : in out Item_Map_Lists.List)
   is
      use Item_Map_Lists;
      use Item_List_Cursor_Lists;
      use LR1_Items;
      use LR1_Items.Item_Lists;

      From_Cur : constant Item_Lists.Cursor := Find (From, From_Set);

      From_Match : Item_Map_Lists.Cursor := Propagations.First;
      To_Match   : Item_List_Cursor_Lists.Cursor;
   begin
      Find_From :
      loop
         exit Find_From when not Has_Element (From_Match);

         declare
            Map : Item_Map renames Constant_Ref (From_Match);
         begin
            if From_Cur = Map.From then

               To_Match := Map.To.First;
               loop
                  exit when not Has_Element (To_Match);

                  declare
                     use all type SAL.Compare_Result;
                     Cur       : Item_Lists.Cursor renames Constant_Ref (To_Match);
                     Test_Item : LR1_Items.Item renames Constant_Ref (Cur);
                  begin
                     if Equal = LR1_Items.Item_Compare (Test_Item, Constant_Ref (To_Item)) then
                        exit Find_From;
                     end if;
                  end;
                  Next (To_Match);
               end loop;
               exit Find_From;
            end if;
         end;

         Next (From_Match);
      end loop Find_From;

      if not Has_Element (From_Match) then
         Propagations.Append ((From_Cur, To_List (To_Item)));

      elsif not Has_Element (To_Match) then
         Ref (From_Match).To.Append (To_Item);

      else
         raise SAL.Programmer_Error with "Add_Propagation: unexpected case";
      end if;
   end Add_Propagation;

   --  Calculate the lookaheads from Closure_Item for Source_Item.
   --  Source_Item must be one of the kernel items in Source_Set.
   --  Closure_Item must be an item in the lookahead closure of Source_Item for #.
   --
   --  Spontaneous lookaheads are put in Source_Item.Lookahead,
   --  propagated lookaheads in Propagations.
   --
   --  Set Used_Tokens = True for all tokens in lookaheads.
   procedure Generate_Lookahead_Info
     (Source_Item  : in     LR1_Items.Item;
      Source_Set   : in     LR1_Items.Item_Set;
      Closure_Item : in     LR1_Items.Item;
      Propagations : in out Item_Map_Lists.List;
      Descriptor   : in     WisiToken.Descriptor;
      Grammar      : in     WisiToken.Productions.Prod_Arrays.Vector;
      Kernels      : in out LR1_Items.Item_Set_List)
   is
      use LR1_Items;
      use LR1_Items.Item_Lists;
      use Token_ID_Arrays;

      Spontaneous_Count : Integer := 0;
   begin
      if Trace_Generate > Outline then
         Ada.Text_IO.Put_Line ("  closure_item: ");
         LR1_Items.Put (Grammar, Descriptor, Closure_Item);
         Ada.Text_IO.New_Line;
      end if;

      if not Has_Element (Closure_Item.Dot) then
         return;
      end if;

      declare
         ID         : constant Token_ID               := Element (Closure_Item.Dot);
         Next_Dot   : constant Token_ID_Arrays.Cursor := Next (Closure_Item.Dot);
         Goto_State : constant Unknown_State_Index    := LR1_Items.Goto_State (Source_Set, ID);
         To_Item    : constant Item_Lists.Cursor      :=
           (if Goto_State = Unknown_State then Item_Lists.No_Element
            else LR1_Items.Find (Closure_Item.Prod, Next_Dot, Kernels (Goto_State)));
      begin
         if Closure_Item.Lookaheads (Descriptor.Last_Lookahead) and Has_Element (To_Item) then
            Add_Propagation
              (From         => Source_Item,
               From_Set     => Source_Set,
               To_Item      => To_Item,
               Propagations => Propagations);
         end if;

         if Has_Element (To_Item) then
            if Trace_Generate > Outline then
               Spontaneous_Count := Spontaneous_Count + 1;
               Ada.Text_IO.Put_Line ("  spontaneous: " & Lookahead_Image (Closure_Item.Lookaheads.all, Descriptor));
            end if;

            LR1_Items.Include (Ref (To_Item), Closure_Item.Lookaheads.all, Descriptor);
         end if;
      end;
   end Generate_Lookahead_Info;

   procedure Propagate_Lookaheads
     (List       : in Item_Map_Lists.List;
      Descriptor : in WisiToken.Descriptor)
   is
      --  In List, update all To lookaheads from From lookaheads,
      --  recursively.

      use LR1_Items.Item_Lists;

      More_To_Check : Boolean := True;
      Added_One     : Boolean;
   begin
      while More_To_Check loop

         More_To_Check := False;
         for Mapping of List loop
            for Copy of Mapping.To loop
               LR1_Items.Include (Ref (Copy), Constant_Ref (Mapping.From).Lookaheads.all, Added_One, Descriptor);

               More_To_Check := More_To_Check or Added_One;
            end loop;
         end loop;
      end loop;
   end Propagate_Lookaheads;

   --  Calculate the LALR(1) lookaheads for Grammar.
   --  Kernels should be the sets of LR(0) kernels on input, and will
   --  become the set of LALR(1) kernels on output.
   procedure Fill_In_Lookaheads
     (Grammar                 : in     WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production    : in     Token_ID_Set;
      First_Terminal_Sequence : in     Token_Sequence_Arrays.Vector;
      Kernels                 : in out LR1_Items.Item_Set_List;
      Descriptor              : in     WisiToken.Descriptor)
   is
      pragma Warnings (Off, """Kernel_Item_Set"" is not modified, could be declared constant");
      --  WORKAROUND: GNAT GPL 2018 complains Kernel_Item_Set could be a constant, but
      --  when we declare that, it complains the target of the assignment of
      --  .Prod, .Dot below must be a variable.

      Kernel_Item_Set : LR1_Items.Item_Set := -- used for temporary arg to Closure
        (Set            => LR1_Items.Item_Lists.To_List
           ((Prod       => <>,
             Dot        => <>,
             Lookaheads => Propagate_Lookahead (Descriptor))),
         Goto_List      => <>,
         Dot_IDs        => <>,
         State          => <>);

      Closure : LR1_Items.Item_Set;

      Propagation_List : Item_Map_Lists.List;

   begin
      for Kernel of Kernels loop
         if Trace_Generate > Outline then
            Ada.Text_IO.Put ("Adding lookaheads for ");
            LR1_Items.Put (Grammar, Descriptor, Kernel);
         end if;

         for Kernel_Item of Kernel.Set loop
            Kernel_Item_Set.Set (Kernel_Item_Set.Set.First).Prod := Kernel_Item.Prod;
            Kernel_Item_Set.Set (Kernel_Item_Set.Set.First).Dot  := Kernel_Item.Dot;

            Closure := LR1_Items.Closure
              (Kernel_Item_Set, Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);

            for Closure_Item of Closure.Set loop
               Generate_Lookahead_Info
                 (Kernel_Item, Kernel, Closure_Item, Propagation_List, Descriptor, Grammar, Kernels);
            end loop;
         end loop;
      end loop;

      if Trace_Generate > Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Propagations:");
         Put (Grammar, Descriptor, Propagation_List);
         Ada.Text_IO.New_Line;
      end if;

      Propagate_Lookaheads (Propagation_List, Descriptor);
   end Fill_In_Lookaheads;

   --  Add actions for all Kernels to Table.
   procedure Add_Actions
     (Kernels                 : in     LR1_Items.Item_Set_List;
      Grammar                 : in     WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production    : in     Token_ID_Set;
      First_Nonterm_Set       : in     Token_Array_Token_Set;
      First_Terminal_Sequence : in     Token_Sequence_Arrays.Vector;
      Conflicts               :    out Conflict_Lists.List;
      Table                   : in out Parse_Table;
      Descriptor              : in     WisiToken.Descriptor)
   is
      Closure : LR1_Items.Item_Set;
   begin
      for Kernel of Kernels loop
         --  IMPROVEME: there are three "closure" computations that could
         --  probably be refactored to save computation; in
         --  LALR_Goto_Transitions, Fill_In_Lookaheads, and here.
         Closure := LR1_Items.Closure (Kernel, Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);

         Add_Actions (Closure, Table, Grammar, Has_Empty_Production, First_Nonterm_Set, Conflicts, Descriptor);
      end loop;

      if Trace_Generate > Detail then
         Ada.Text_IO.New_Line;
      end if;
   end Add_Actions;

   function Generate
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor;
      Known_Conflicts : in Conflict_Lists.List := Conflict_Lists.Empty_List;
      McKenzie_Param  : in McKenzie_Param_Type := Default_McKenzie_Param;
      Put_Parse_Table : in Boolean := False)
     return Parse_Table_Ptr
   is
      use all type Ada.Containers.Count_Type;

      Ignore_Unused_Tokens     : constant Boolean := WisiToken.Trace_Generate > Detail;
      Ignore_Unknown_Conflicts : constant Boolean := WisiToken.Trace_Generate > Detail;
      Unused_Tokens            : constant Boolean := WisiToken.Generate.Check_Unused_Tokens (Descriptor, Grammar);

      Table : Parse_Table_Ptr;

      Has_Empty_Production : constant Token_ID_Set := WisiToken.Generate.Has_Empty_Production (Grammar);

      Minimal_Terminal_First : constant Token_Array_Token_ID :=
        WisiToken.Generate.LR.Minimal_Terminal_First (Grammar, Descriptor);

      Ancestors : constant Token_Array_Token_Set := WisiToken.Generate.Ancestors (Grammar, Descriptor);

      First_Nonterm_Set : constant Token_Array_Token_Set := WisiToken.Generate.First
        (Grammar, Has_Empty_Production, Descriptor.First_Terminal);

      First_Terminal_Sequence : constant Token_Sequence_Arrays.Vector :=
        WisiToken.Generate.To_Terminal_Sequence_Array (First_Nonterm_Set, Descriptor);

      Kernels : LR1_Items.Item_Set_List := LALR_Kernels (Grammar, First_Nonterm_Set, Descriptor);

      Unknown_Conflicts    : Conflict_Lists.List;
      Known_Conflicts_Edit : Conflict_Lists.List := Known_Conflicts;

   begin
      WisiToken.Generate.Error := False; -- necessary in unit tests; some previous test might have encountered an error.

      Fill_In_Lookaheads (Grammar, Has_Empty_Production, First_Terminal_Sequence, Kernels, Descriptor);

      if Unused_Tokens then
         WisiToken.Generate.Error := not Ignore_Unused_Tokens;
         Ada.Text_IO.New_Line;
      end if;

      if Trace_Generate > Detail then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("LR(1) Kernels:");
         LR1_Items.Put (Grammar, Descriptor, Kernels, Show_Lookaheads => True);
      end if;

      Table := new Parse_Table
        (State_First       => Kernels.First_Index,
         State_Last        => Kernels.Last_Index,
         First_Terminal    => Descriptor.First_Terminal,
         Last_Terminal     => Descriptor.Last_Terminal,
         First_Nonterminal => Descriptor.First_Nonterminal,
         Last_Nonterminal  => Descriptor.Last_Nonterminal);

      if McKenzie_Param = Default_McKenzie_Param then
         --  Descriminants in Default are wrong
         Table.McKenzie_Param :=
           (First_Terminal    => Descriptor.First_Terminal,
            Last_Terminal     => Descriptor.Last_Terminal,
            First_Nonterminal => Descriptor.First_Nonterminal,
            Last_Nonterminal  => Descriptor.Last_Nonterminal,
            Insert            => (others => 0),
            Delete            => (others => 0),
            Push_Back         => (others => 0),
            Ignore_Check_Fail => Default_McKenzie_Param.Ignore_Check_Fail,
            Task_Count        => Default_McKenzie_Param.Task_Count,
            Cost_Limit        => Default_McKenzie_Param.Cost_Limit,
            Check_Limit       => Default_McKenzie_Param.Check_Limit,
            Check_Delta_Limit => Default_McKenzie_Param.Check_Delta_Limit,
            Enqueue_Limit     => Default_McKenzie_Param.Enqueue_Limit);
      else
         Table.McKenzie_Param := McKenzie_Param;
      end if;

      Add_Actions
        (Kernels, Grammar, Has_Empty_Production, First_Nonterm_Set, First_Terminal_Sequence, Unknown_Conflicts,
         Table.all, Descriptor);

      --  Set Table.States.Productions, Minimal_Terminal_First for McKenzie_Recover
      for State in Table.States'Range loop
         Table.States (State).Productions := LR1_Items.Productions (Kernels (State));
         WisiToken.Generate.LR.Set_Minimal_Complete_Actions
           (Table.States (State), Kernels (State), Minimal_Terminal_First, Ancestors, Descriptor, Grammar);
      end loop;

      if Put_Parse_Table then
         WisiToken.Generate.LR.Put_Parse_Table
           (Table, "LALR", Grammar, Kernels, Ancestors, Unknown_Conflicts, Descriptor);
      end if;

      Delete_Known (Unknown_Conflicts, Known_Conflicts_Edit);

      if Unknown_Conflicts.Length > 0 then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error, "unknown conflicts:");
         Put (Unknown_Conflicts, Ada.Text_IO.Current_Error, Descriptor);
         Ada.Text_IO.New_Line (Ada.Text_IO.Current_Error);
         WisiToken.Generate.Error := WisiToken.Generate.Error or not Ignore_Unknown_Conflicts;
      end if;

      if Known_Conflicts_Edit.Length > 0 then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error, "excess known conflicts:");
         Put (Known_Conflicts_Edit, Ada.Text_IO.Current_Error, Descriptor);
         Ada.Text_IO.New_Line (Ada.Text_IO.Current_Error);
         WisiToken.Generate.Error := WisiToken.Generate.Error or not Ignore_Unknown_Conflicts;
      end if;

      return Table;
   end Generate;

end WisiToken.Generate.LR.LALR_Generate;
