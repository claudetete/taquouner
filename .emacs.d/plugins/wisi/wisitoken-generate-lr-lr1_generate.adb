--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017, 2018 Free Software Foundation, Inc.
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
with WisiToken.Generate;
package body WisiToken.Generate.LR.LR1_Generate is

   function LR1_Goto_Transitions
     (Set                     : in LR1_Items.Item_Set;
      Symbol                  : in Token_ID;
      Has_Empty_Production    : in Token_ID_Set;
      First_Terminal_Sequence : in Token_Sequence_Arrays.Vector;
      Grammar                 : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor              : in WisiToken.Descriptor)
     return LR1_Items.Item_Set
   is
      use all type Ada.Containers.Count_Type;
      use Token_ID_Arrays;
      use LR1_Items;

      Goto_Set : Item_Set;
   begin
      for Item of Set.Set loop
         if Item.Dot /= No_Element then
            if Element (Item.Dot) = Symbol and
              --  We don't need a state with dot after EOF in the
              --  accept production. EOF should only appear in the
              --  accept production.
              Symbol /= Descriptor.EOF_ID
            then
               Goto_Set.Set.Insert ((Item.Prod, Next (Item.Dot), new Token_ID_Set'(Item.Lookaheads.all)));
            end if;
         end if;
      end loop;

      if Goto_Set.Set.Length > 0 then
         return Closure (Goto_Set, Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);
      else
         return Goto_Set;
      end if;
   end LR1_Goto_Transitions;

   function LR1_Item_Sets
     (Has_Empty_Production    : in Token_ID_Set;
      First_Terminal_Sequence : in Token_Sequence_Arrays.Vector;
      Grammar                 : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor              : in WisiToken.Descriptor)
     return LR1_Items.Item_Set_List
   is
      use all type Ada.Containers.Count_Type;

      --  [dragon] algorithm 4.9 pg 231; figure 4.38 pg 232; procedure
      --  "items", with some optimizations.

      use LR1_Items;

      First_State_Index : constant State_Index := 0;

      C               : LR1_Items.Item_Set_List;       -- result
      C_Tree          : LR1_Items.Item_Set_Trees.Tree; -- for fast find
      States_To_Check : State_Index_Queues.Queue;
      --  [dragon] specifies 'until no more items can be added', but we use
      --  a queue to avoid checking unecessary states. Ada LR1 has over
      --  100,000 states, so this is a significant gain (reduced time from
      --  600 seconds to 40).

      I       : State_Index;
      Dot_IDs : Token_ID_Arrays.Vector;

      New_Item_Set : Item_Set := Closure
        ((Set            => Item_Lists.To_List
            ((Prod       => (Grammar.First_Index, 0),
              Dot        => Grammar (Grammar.First_Index).RHSs (0).Tokens.First,
              Lookaheads => new Token_ID_Set'(To_Lookahead (Descriptor.EOF_ID, Descriptor)))),
          Goto_List      => <>,
          Dot_IDs        => <>,
          State          => First_State_Index),
        Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);

      Found_State  : Unknown_State_Index;

   begin
      C.Set_First (First_State_Index);

      Add (New_Item_Set, C, C_Tree, Descriptor, Include_Lookaheads => True);

      States_To_Check.Put (First_State_Index);
      loop
         exit when States_To_Check.Is_Empty;
         I := States_To_Check.Get;

         if Trace_Generate > Outline then
            Ada.Text_IO.Put ("Checking ");
            Put (Grammar, Descriptor, C (I), Show_Lookaheads => True, Show_Goto_List => True);
         end if;

         Dot_IDs := C (I).Dot_IDs;
         --  We can't iterate on C (I).Dot_IDs when the loop adds items to C;
         --  it might be reallocated to grow.

         for Symbol of Dot_IDs loop
            --  [dragon] has 'for each grammar symbol X', but LR1_Goto_Transitions
            --  rejects Symbol that is not in Dot_IDs, so we iterate over that.

            New_Item_Set := LR1_Goto_Transitions
              (C (I), Symbol, Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);

            if New_Item_Set.Set.Length > 0 then -- 'goto (I, X) not empty'

               Found_State := Find (New_Item_Set, C_Tree, Match_Lookaheads => True); -- 'not in C'

               if Found_State = Unknown_State then
                  New_Item_Set.State := C.Last_Index + 1;

                  States_To_Check.Put (New_Item_Set.State);

                  Add (New_Item_Set, C, C_Tree, Descriptor, Include_Lookaheads => True);

                  if Trace_Generate > Outline then
                     Ada.Text_IO.Put_Line
                       ("  adding state" & Unknown_State_Index'Image (C.Last_Index) & ": from state" &
                          Unknown_State_Index'Image (I) & " on " & Image (Symbol, Descriptor));
                     Put (Grammar, Descriptor, New_Item_Set, Show_Lookaheads => True);
                  end if;

                  C (I).Goto_List.Insert ((Symbol, C.Last_Index));
               else

                  --  If there's not already a goto entry between these two sets, create one.
                  if not Is_In ((Symbol, Found_State), Goto_List => C (I).Goto_List) then
                     if Trace_Generate > Outline then
                        Ada.Text_IO.Put_Line
                          ("  adding goto on " & Image (Symbol, Descriptor) & " to state" &
                             Unknown_State_Index'Image (Found_State));

                     end if;

                     C (I).Goto_List.Insert ((Symbol, Found_State));
                  end if;
               end if;
            end if;
         end loop;
      end loop;

      if Trace_Generate > Outline then
         Ada.Text_IO.New_Line;
      end if;

      return C;
   end LR1_Item_Sets;

   procedure Add_Actions
     (Item_Sets            : in     LR1_Items.Item_Set_List;
      Grammar              : in     WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in     Token_ID_Set;
      First_Nonterm_Set    : in     Token_Array_Token_Set;
      Conflicts            :    out Conflict_Lists.List;
      Table                : in out Parse_Table;
      Descriptor           : in     WisiToken.Descriptor)
   is
      --  Add actions for all Item_Sets to Table.
   begin
      for Item_Set of Item_Sets loop
         Add_Actions (Item_Set, Table, Grammar, Has_Empty_Production, First_Nonterm_Set, Conflicts, Descriptor);
      end loop;

      if Trace_Generate > Outline then
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
      use type Ada.Containers.Count_Type;

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

      Item_Sets : constant LR1_Items.Item_Set_List := LR1_Item_Sets
        (Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);

      Unknown_Conflicts    : Conflict_Lists.List;
      Known_Conflicts_Edit : Conflict_Lists.List := Known_Conflicts;
   begin
      if Trace_Generate > Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("LR(1) Item_Sets:");
         LR1_Items.Put (Grammar, Descriptor, Item_Sets);
      end if;

      Table := new Parse_Table
        (State_First       => Item_Sets.First_Index,
         State_Last        => Item_Sets.Last_Index,
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
        (Item_Sets, Grammar, Has_Empty_Production, First_Nonterm_Set, Unknown_Conflicts, Table.all, Descriptor);

      --  Set Table.States.Productions, Minimal_Terminal_First for McKenzie_Recover
      for State in Table.States'Range loop
         Table.States (State).Productions := LR1_Items.Productions
           (LR1_Items.Filter (Item_Sets (State), Grammar, Descriptor, LR1_Items.In_Kernel'Access));
         WisiToken.Generate.LR.Set_Minimal_Complete_Actions
           (Table.States (State),
            LR1_Items.Filter (Item_Sets (State), Grammar, Descriptor, LR1_Items.In_Kernel'Access),
            Minimal_Terminal_First, Ancestors, Descriptor, Grammar);
      end loop;

      if Put_Parse_Table then
         WisiToken.Generate.LR.Put_Parse_Table
           (Table, "LR1", Grammar, Item_Sets, Ancestors, Unknown_Conflicts, Descriptor);
      end if;

      if Trace_Generate > Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Has_Empty_Production: " & Image (Has_Empty_Production, Descriptor));

         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Minimal_Terminal_First:");
         for ID in Minimal_Terminal_First'Range loop
            Ada.Text_IO.Put_Line
              (Image (ID, Descriptor) & " =>" &
                 (if Minimal_Terminal_First (ID) = Invalid_Token_ID
                  then ""
                  else ' ' & Image (Minimal_Terminal_First (ID), Descriptor)));
         end loop;
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

      WisiToken.Generate.Error := WisiToken.Generate.Error or (Unused_Tokens and not Ignore_Unused_Tokens);

      return Table;
   end Generate;

end WisiToken.Generate.LR.LR1_Generate;
