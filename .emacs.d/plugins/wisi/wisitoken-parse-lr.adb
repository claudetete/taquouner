--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2013-2015, 2017, 2018 Free Software Foundation, Inc.
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

pragma License (GPL);

with Ada.Exceptions;
with Ada.Strings.Maps;
with Ada.Text_IO;
package body WisiToken.Parse.LR is

   ----------
   --  Public subprograms, declaration order

   function Image (Item : in Parse_Action_Rec; Descriptor : in WisiToken.Descriptor) return String
   is
      use Ada.Containers;
   begin
      case Item.Verb is
      when Shift =>
         return "(Shift," & State_Index'Image (Item.State) & ")";

      when Reduce =>
         return "(Reduce," & Count_Type'Image (Item.Token_Count) & ", " &
           Image (Item.Production.LHS, Descriptor) & "," & Trimmed_Image (Item.Production.RHS) & ")";
      when Accept_It =>
         return "(Accept It)";
      when Error =>
         return "(Error)";
      end case;
   end Image;

   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Parse_Action_Rec)
   is
      use Ada.Containers;
   begin
      case Item.Verb is
      when Shift =>
         Trace.Put ("shift and goto state" & State_Index'Image (Item.State));

      when Reduce =>
         Trace.Put
           ("reduce" & Count_Type'Image (Item.Token_Count) & " tokens to " &
              Image (Item.Production.LHS, Trace.Descriptor.all));
      when Accept_It =>
         Trace.Put ("accept it");
      when Error =>
         Trace.Put ("ERROR");
      end case;
   end Put;

   function Equal (Left, Right : in Parse_Action_Rec) return Boolean
   is
      use all type Ada.Containers.Count_Type;
   begin
      if Left.Verb = Right.Verb then
         case Left.Verb is
         when Shift =>
            return Left.State = Right.State;

         when Reduce | Accept_It =>
            return Left.Production.LHS = Right.Production.LHS and Left.Token_Count = Right.Token_Count;

         when Error =>
            return True;
         end case;
      else
         return False;
      end if;
   end Equal;

   procedure Add
     (List   : in out Action_Node_Ptr;
      Symbol : in     Token_ID;
      Action : in     Parse_Action_Rec)
   is
      New_Item : constant Action_Node_Ptr := new Action_Node'(Symbol, new Parse_Action_Node'(Action, null), null);
      I        : Action_Node_Ptr          := List;
   begin
      if I = null then
         List := New_Item;
      else
         if List.Symbol > Symbol then
            New_Item.Next := List;
            List          := New_Item;
         else
            if List.Next = null then
               List.Next := New_Item;
            else
               I := List;
               loop
                  exit when I.Next = null or else I.Next.Symbol > Symbol;
                  I := I.Next;
               end loop;
               New_Item.Next := I.Next;
               I.Next        := New_Item;
            end if;
         end if;
      end if;
   end Add;

   function Symbol (List : in Goto_Node_Ptr) return Token_ID
   is begin
      return List.Symbol;
   end Symbol;

   function State (List : in Goto_Node_Ptr) return State_Index
   is begin
      return List.State;
   end State;

   function Next (List : in Goto_Node_Ptr) return Goto_Node_Ptr
   is begin
      return List.Next;
   end Next;

   function Compare_Minimal_Action (Left, Right : in Minimal_Action) return SAL.Compare_Result
   is begin
      if Left.Verb > Right.Verb then
         return SAL.Greater;
      elsif Left.Verb < Right.Verb then
         return SAL.Less;
      else
         case Left.Verb is
         when Shift =>
            if Left.ID > Right.ID then
               return SAL.Greater;
            elsif Left.ID < Right.ID then
               return SAL.Less;
            else
               return SAL.Equal;
            end if;
         when Reduce =>
            if Left.Nonterm > Right.Nonterm then
               return SAL.Greater;
            elsif Left.Nonterm < Right.Nonterm then
               return SAL.Less;
            else
               return SAL.Equal;
            end if;
         end case;
      end if;
   end Compare_Minimal_Action;

   function Strict_Image (Item : in Minimal_Action) return String
   is begin
      case Item.Verb is
      when Shift =>
         return "(Shift," & Token_ID'Image (Item.ID) & "," & State_Index'Image (Item.State) & ")";
      when Reduce =>
         return "(Reduce," & Token_ID'Image (Item.Nonterm) & "," &
           Ada.Containers.Count_Type'Image (Item.Token_Count) & ")";
      end case;
   end Strict_Image;

   procedure Set_Minimal_Action (List : out Minimal_Action_Lists.List; Actions : in Minimal_Action_Array)
   is begin
      for Action of Actions loop
         List.Insert (Action);
      end loop;
   end Set_Minimal_Action;

   function First (State : in Parse_State) return Action_List_Iterator
   is begin
      return Iter : Action_List_Iterator := (Node => State.Action_List, Item => null) do
         loop
            exit when Iter.Node = null;
            Iter.Item := Iter.Node.Action;
            exit when Iter.Item /= null;
            Iter.Node := Iter.Node.Next;
         end loop;
      end return;
   end First;

   function Is_Done (Iter : in Action_List_Iterator) return Boolean
   is begin
      return Iter.Node = null;
   end Is_Done;

   procedure Next (Iter : in out Action_List_Iterator)
   is begin
      if Iter.Node = null then
         return;
      end if;

      if Iter.Item.Next = null then
         loop
            Iter.Node := Iter.Node.Next;
            exit when Iter.Node = null;
            Iter.Item := Iter.Node.Action;
            exit when Iter.Item /= null;
         end loop;
      else
         Iter.Item := Iter.Item.Next; -- a conflict
      end if;
   end Next;

   function Symbol (Iter : in Action_List_Iterator) return Token_ID
   is begin
      return Iter.Node.Symbol;
   end Symbol;

   function Action (Iter : in Action_List_Iterator) return Parse_Action_Rec
   is begin
      return Iter.Item.Item;
   end Action;

   procedure Add_Action
     (State       : in out LR.Parse_State;
      Symbol      : in     Token_ID;
      State_Index : in     WisiToken.State_Index)
   is
      Action   : constant Parse_Action_Rec := (Shift, State_Index);
      New_Node : constant Action_Node_Ptr  := new Action_Node'(Symbol, new Parse_Action_Node'(Action, null), null);
      Node     : Action_Node_Ptr;
   begin
      if State.Action_List = null then
         State.Action_List := New_Node;
      else
         Node := State.Action_List;
         loop
            exit when Node.Next = null;
            Node := Node.Next;
         end loop;
         Node.Next := New_Node;
      end if;
   end Add_Action;

   procedure Add_Action
     (State           : in out LR.Parse_State;
      Symbol          : in     Token_ID;
      Verb            : in     LR.Parse_Action_Verbs;
      Production      : in     Production_ID;
      RHS_Token_Count : in     Ada.Containers.Count_Type;
      Semantic_Action : in     WisiToken.Syntax_Trees.Semantic_Action;
      Semantic_Check  : in     Semantic_Checks.Semantic_Check)
   is
      Action   : Parse_Action_Rec;
      New_Node : Action_Node_Ptr;
      Node     : Action_Node_Ptr;
   begin
      case Verb is
      when Reduce =>
         Action := (Reduce, Production, Semantic_Action, Semantic_Check, RHS_Token_Count);
      when Accept_It =>
         Action := (Accept_It, Production, Semantic_Action, Semantic_Check, RHS_Token_Count);
      when others =>
         null;
      end case;
      New_Node := new Action_Node'(Symbol, new Parse_Action_Node'(Action, null), null);
      if State.Action_List = null then
         State.Action_List := New_Node;
      else
         Node := State.Action_List;
         loop
            exit when Node.Next = null;
            Node := Node.Next;
         end loop;
         Node.Next := New_Node;
      end if;
   end Add_Action;

   procedure Add_Action
     (State           : in out Parse_State;
      Symbols         : in     Token_ID_Array;
      Production      : in     Production_ID;
      RHS_Token_Count : in     Ada.Containers.Count_Type;
      Semantic_Action : in     WisiToken.Syntax_Trees.Semantic_Action;
      Semantic_Check  : in     WisiToken.Semantic_Checks.Semantic_Check)
   is begin
      --  We assume Duplicate_Reduce is True for this state; no
      --  conflicts, all the same action.
      for Symbol of Symbols loop
         Add_Action
           (State, Symbol, Reduce, Production, RHS_Token_Count,
            Semantic_Action, Semantic_Check);
      end loop;
      Add_Error (State);
   end Add_Action;

   procedure Add_Action
     (State             : in out LR.Parse_State;
      Symbol            : in     Token_ID;
      State_Index       : in     WisiToken.State_Index;
      Reduce_Production : in     Production_ID;
      RHS_Token_Count   : in     Ada.Containers.Count_Type;
      Semantic_Action   : in     WisiToken.Syntax_Trees.Semantic_Action;
      Semantic_Check    : in     Semantic_Checks.Semantic_Check)
   is
      Action_1 : constant Parse_Action_Rec := (Shift, State_Index);
      Action_2 : constant Parse_Action_Rec :=
        (Reduce, Reduce_Production, Semantic_Action, Semantic_Check, RHS_Token_Count);
   begin
      State.Action_List := new Action_Node'
        (Symbol, new Parse_Action_Node'(Action_1, new Parse_Action_Node'(Action_2, null)), State.Action_List);
   end Add_Action;

   procedure Add_Action
     (State             : in out LR.Parse_State;
      Symbol            : in     Token_ID;
      Verb              : in     LR.Parse_Action_Verbs;
      Production_1      : in     Production_ID;
      RHS_Token_Count_1 : in     Ada.Containers.Count_Type;
      Semantic_Action_1 : in     Syntax_Trees.Semantic_Action;
      Semantic_Check_1  : in     Semantic_Checks.Semantic_Check;
      Production_2      : in     Production_ID;
      RHS_Token_Count_2 : in     Ada.Containers.Count_Type;
      Semantic_Action_2 : in     Syntax_Trees.Semantic_Action;
      Semantic_Check_2  : in     Semantic_Checks.Semantic_Check)
   is
      Action_1 : constant Parse_Action_Rec :=
        (case Verb is
         when Reduce    =>
           (Reduce, Production_1, Semantic_Action_1, Semantic_Check_1, RHS_Token_Count_1),
         when Accept_It =>
           (Accept_It, Production_1, Semantic_Action_1, Semantic_Check_1, RHS_Token_Count_1),
         when others => raise SAL.Programmer_Error);

      Action_2 : constant Parse_Action_Rec :=
        (Reduce, Production_2, Semantic_Action_2, Semantic_Check_2, RHS_Token_Count_2);
   begin
      State.Action_List := new Action_Node'
        (Symbol, new Parse_Action_Node'(Action_1, new Parse_Action_Node'(Action_2, null)), State.Action_List);
   end Add_Action;

   procedure Add_Error (State  : in out LR.Parse_State)
   is
      Action : constant Parse_Action_Rec := (Verb => Error);
      Node   : Action_Node_Ptr           := State.Action_List;
   begin
      if Node = null then
         raise SAL.Programmer_Error with "adding an error action to a parse table state before other actions.";
      end if;
      loop
         exit when Node.Next = null;
         Node := Node.Next;
      end loop;
      Node.Next := new Action_Node'(Invalid_Token_ID, new Parse_Action_Node'(Action, null), null);
   end Add_Error;

   procedure Add_Goto
     (State      : in out LR.Parse_State;
      Symbol     : in     Token_ID;
      To_State   : in     State_Index)
   is
      List     : Goto_Node_Ptr renames State.Goto_List;
      New_Item : constant Goto_Node_Ptr := new Goto_Node'(Symbol, To_State, null);
      I        : Goto_Node_Ptr := List;
   begin
      if I = null then
         List := New_Item;
      else
         if List.Symbol > Symbol then
            New_Item.Next := List;
            List          := New_Item;
         else
            if List.Next = null then
               List.Next := New_Item;
            else
               I := List;
               loop
                  exit when I.Next = null or List.Symbol > Symbol;
                  I := I.Next;
               end loop;
               New_Item.Next := I.Next;
               I.Next        := New_Item;
            end if;
         end if;
      end if;
   end Add_Goto;

   procedure Set_Production
     (Prod     : in out Productions.Instance;
      LHS      : in     Token_ID;
      RHS_Last : in     Natural)
   is begin
      Prod.LHS := LHS;
      Prod.RHSs.Set_First (0);
      Prod.RHSs.Set_Last (RHS_Last);
   end Set_Production;

   procedure Set_RHS
     (Prod      : in out Productions.Instance;
      RHS_Index : in     Natural;
      Tokens    : in     Token_ID_Array;
      Action    : in     WisiToken.Syntax_Trees.Semantic_Action   := null;
      Check     : in     WisiToken.Semantic_Checks.Semantic_Check := null)
   is begin
      if Tokens'Length > 0 then
         Prod.RHSs (RHS_Index).Tokens.Set_First (1);
         Prod.RHSs (RHS_Index).Tokens.Set_Last (Tokens'Length);
         for I in Tokens'Range loop
            Prod.RHSs (RHS_Index).Tokens (I) := Tokens (I);
         end loop;
         Prod.RHSs (RHS_Index).Action := Action;
         Prod.RHSs (RHS_Index).Check  := Check;
      end if;
   end Set_RHS;

   function Goto_For
     (Table : in Parse_Table;
      State : in State_Index;
      ID    : in Token_ID)
     return Unknown_State_Index
   is
      Goto_Node : constant Goto_Node_Ptr := Goto_For (Table, State, ID);
   begin
      if Goto_Node = null then
         --  We can only get here during error recovery.
         return Unknown_State;
      else
         return Goto_Node.State;
      end if;
   end Goto_For;

   function Goto_For
     (Table : in Parse_Table;
      State : in State_Index;
      ID    : in Token_ID)
     return Goto_Node_Ptr
   is
      Goto_Node : Goto_Node_Ptr := Table.States (State).Goto_List;
   begin
      while Goto_Node /= null and then Goto_Node.Symbol /= ID loop
         Goto_Node := Goto_Node.Next;
      end loop;

      return Goto_Node;
   end Goto_For;

   function Action_For
     (Table : in Parse_Table;
      State : in State_Index;
      ID    : in Token_ID)
     return Parse_Action_Node_Ptr
   is
      Action_Node : Action_Node_Ptr := Table.States (State).Action_List;
   begin
      if Action_Node = null then
         raise SAL.Programmer_Error with "no actions for state" & Unknown_State_Index'Image (State);
      end if;

      while Action_Node.Next /= null and Action_Node.Symbol /= ID loop
         Action_Node := Action_Node.Next;
      end loop;

      return Action_Node.Action;
   end Action_For;

   function Expecting (Table : in Parse_Table; State : in State_Index) return Token_ID_Set
   is
      Result : Token_ID_Set    := (Table.First_Terminal .. Table.Last_Terminal => False);
      Action : Action_Node_Ptr := Table.States (State).Action_List;
   begin
      loop
         --  Last action is error; don't include it.
         exit when Action.Next = null;

         Result (Action.Symbol) := True;
         Action := Action.Next;
      end loop;
      return Result;
   end Expecting;

   procedure Free_Table (Table : in out Parse_Table_Ptr)
   is

      procedure Free is new Ada.Unchecked_Deallocation (Parse_Table, Parse_Table_Ptr);
      Action            : Action_Node_Ptr;
      Temp_Action       : Action_Node_Ptr;
      Parse_Action      : Parse_Action_Node_Ptr;
      Temp_Parse_Action : Parse_Action_Node_Ptr;
      Got               : Goto_Node_Ptr;
      Temp_Got          : Goto_Node_Ptr;
   begin
      if Table = null then
         return;
      end if;

      for State of Table.States loop
         Action := State.Action_List;
         loop
            exit when Action = null;
            Parse_Action := Action.Action;
            loop
               exit when Parse_Action = null;
               Temp_Parse_Action := Parse_Action;
               Parse_Action := Parse_Action.Next;
               Free (Temp_Parse_Action);
            end loop;

            Temp_Action := Action;
            Action := Action.Next;
            Free (Temp_Action);
         end loop;

         Got := State.Goto_List;
         loop
            exit when Got = null;
            Temp_Got := Got;
            Got := Got.Next;
            Free (Temp_Got);
         end loop;
      end loop;

      Free (Table);
   end Free_Table;

   function Get_Action
     (Prod        : in Production_ID;
      Productions : in WisiToken.Productions.Prod_Arrays.Vector)
     return WisiToken.Syntax_Trees.Semantic_Action
   is begin
      return Productions (Prod.LHS).RHSs (Prod.RHS).Action;
   end Get_Action;

   function Get_Check
     (Prod        : in Production_ID;
      Productions : in WisiToken.Productions.Prod_Arrays.Vector)
     return WisiToken.Semantic_Checks.Semantic_Check
   is begin
      return Productions (Prod.LHS).RHSs (Prod.RHS).Check;
   end Get_Check;

   function Get_Text_Rep
     (File_Name      : in String;
      McKenzie_Param : in McKenzie_Param_Type;
      Productions    : in WisiToken.Productions.Prod_Arrays.Vector)
     return Parse_Table_Ptr
   is
      use Ada.Text_IO;
      use Ada.Strings.Unbounded;

      File  : File_Type;
      Line  : Unbounded_String;
      First : Integer;
      Last  : Integer := 0;

      Delimiters : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (" ;");

      function Last_Char return Character
      is begin
         if Last = 0 then
            return Element (Line, Last + 1);
         else
            return Element (Line, Last);
         end if;
      end Last_Char;

      procedure Skip_Char
      is begin
         if Last > 0 then
            Last := Last + 1;
            if Last > Length (Line) then
               Last := 0;
            end if;
         end if;
         if Last = 0 then
            Line := +Get_Line (File);
            Last := -1 + Index_Non_Blank (Line);
         end if;
      end Skip_Char;

      function Next_Value return String
      is begin
         First := Last + 1;
         Last  := Index (Line, Delimiters, First);
         return Result : constant String := Slice (Line, First, (if Last = 0 then Length (Line) else Last - 1))
         do
            if Last = 0 then
               Line := +Get_Line (File);
               Last := -1 + Index_Non_Blank (Line);
            end if;
         end return;
      end Next_Value;

      generic
         type Value_Type is (<>);
         Name : in String;
      function Gen_Next_Value return Value_Type;

      function Gen_Next_Value return Value_Type
      is
         Val : constant String := Next_Value;
      begin
         return Value_Type'Value (Val);
      exception
      when Constraint_Error =>
         raise SAL.Programmer_Error with Error_Message
           (File_Name, Line_Number_Type (Ada.Text_IO.Line (File) - 1), Ada.Text_IO.Count (First),
            "expecting " & Name & ", found '" & Val & "'");
      end Gen_Next_Value;

      function Next_State_Index is new Gen_Next_Value (State_Index, "State_Index");
      function Next_Token_ID is new Gen_Next_Value (Token_ID, "Token_ID");
      function Next_Integer is new Gen_Next_Value (Integer, "Integer");
      function Next_Parse_Action_Verbs is new Gen_Next_Value (Parse_Action_Verbs, "Parse_Action_Verbs");
      function Next_Boolean is new Gen_Next_Value (Boolean, "Boolean");
      function Next_Count_Type is new Gen_Next_Value (Ada.Containers.Count_Type, "Count_Type");
   begin
      Open (File, In_File, File_Name);
      Line := +Get_Line (File);

      declare
         --  We don't read the discriminants in the aggregate, because
         --  aggregate evaluation order is not guaranteed.
         State_First       : constant State_Index := Next_State_Index;
         State_Last        : constant State_Index := Next_State_Index;
         First_Terminal    : constant Token_ID    := Next_Token_ID;
         Last_Terminal     : constant Token_ID    := Next_Token_ID;
         First_Nonterminal : constant Token_ID    := Next_Token_ID;
         Last_Nonterminal  : constant Token_ID    := Next_Token_ID;

         Table : constant Parse_Table_Ptr := new Parse_Table
           (State_First, State_Last, First_Terminal, Last_Terminal, First_Nonterminal, Last_Nonterminal);
      begin
         Table.McKenzie_Param := McKenzie_Param;

         for State of Table.States loop
            State.Productions.Set_First (Next_Integer);
            State.Productions.Set_Last (Next_Integer);
            for I in State.Productions.First_Index .. State.Productions.Last_Index loop
               State.Productions (I).LHS := Next_Token_ID;
               State.Productions (I).RHS := Next_Integer;
            end loop;

            declare
               Node_I       : Action_Node_Ptr := new Action_Node;
               Actions_Done : Boolean         := False;
            begin
               State.Action_List := Node_I;
               loop
                  declare
                     Node_J      : Parse_Action_Node_Ptr := new Parse_Action_Node;
                     Action_Done : Boolean := False;
                     Verb        : Parse_Action_Verbs;
                  begin
                     Node_I.Action := Node_J;
                     loop
                        Verb := Next_Parse_Action_Verbs;
                        Node_J.Item :=
                          (case Verb is
                           when Shift     => (Verb => Shift, others => <>),
                           when Reduce    => (Verb => Reduce, others => <>),
                           when Accept_It => (Verb => Accept_It, others => <>),
                           when Error     => (Verb => Error));

                        case Verb is
                        when Shift =>
                           Node_J.Item.State := Next_State_Index;

                        when Reduce | Accept_It =>
                           Node_J.Item.Production.LHS := Next_Token_ID;
                           Node_J.Item.Production.RHS := Next_Integer;
                           if Next_Boolean then
                              Node_J.Item.Action := Get_Action (Node_J.Item.Production, Productions);
                           else
                              Node_J.Item.Action := null;
                           end if;
                           if Next_Boolean then
                              Node_J.Item.Check := Get_Check (Node_J.Item.Production, Productions);
                           else
                              Node_J.Item.Check := null;
                           end if;
                           Node_J.Item.Token_Count := Next_Count_Type;

                        when Error =>
                           Actions_Done := True;
                        end case;

                        if Element (Line, Last) = ';' then
                           Skip_Char;
                           Action_Done := True;

                           if not Actions_Done then
                              Node_I.Symbol := Next_Token_ID;
                           end if;
                        end if;

                        exit when Action_Done;

                        Node_J.Next := new Parse_Action_Node;
                        Node_J      := Node_J.Next;
                     end loop;
                  end;

                  exit when Actions_Done;
                  Node_I.Next := new Action_Node;
                  Node_I      := Node_I.Next;
               end loop;
            end;

            if Element (Line, 1) = ';' then
               --  No Gotos
               Skip_Char;
            else
               declare
                  Node_I : Goto_Node_Ptr := new Goto_Node;
               begin
                  State.Goto_List  := Node_I;
                  loop
                     Node_I.Symbol := Next_Token_ID;
                     Node_I.State  := Next_State_Index;
                     exit when Element (Line, Last) = ';';
                     Node_I.Next   := new Goto_Node;
                     Node_I        := Node_I.Next;
                  end loop;
                  Skip_Char;
               end;
            end if;

            declare
               Verb         : Minimal_Verbs;
               ID           : Token_ID;
               Action_State : State_Index;
               Count        : Ada.Containers.Count_Type;
            begin
               loop
                  if Last_Char = ';' then
                     Skip_Char;
                     exit;
                  end if;

                  Verb := Next_Parse_Action_Verbs;
                  case Verb is
                  when Shift =>
                     ID           := Next_Token_ID;
                     Action_State := Next_State_Index;
                     State.Minimal_Complete_Actions.Insert ((Shift, ID, Action_State));
                  when Reduce =>
                     ID    := Next_Token_ID;
                     Count := Next_Count_Type;
                     State.Minimal_Complete_Actions.Insert ((Reduce, ID, Count));
                  end case;
               end loop;
            end;
            --  loop exits on End_Error
         end loop;
         --  real return value in End_Error handler; this satisfies the compiler
         return null;
      exception
      when End_Error =>
         Close (File);
         return Table;
      end;
   exception
   when Name_Error =>
      raise User_Error with "parser table text file '" & File_Name & "' not found.";

   when SAL.Programmer_Error =>
      if Is_Open (File) then
         Close (File);
      end if;
      raise;
   when E : others =>
      if Is_Open (File) then
         Close (File);
      end if;
      raise SAL.Programmer_Error with Error_Message
        (File_Name, Line_Number_Type (Ada.Text_IO.Line (File) - 1), Ada.Text_IO.Count (First),
         Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   end Get_Text_Rep;

   function Compare (Left, Right : in Insert_Delete_Op) return SAL.Compare_Result
   is begin
      if Left.Token_Index < Right.Token_Index then
         return SAL.Less;
      elsif Left.Token_Index = Right.Token_Index then
         return SAL.Equal;
      else
         return SAL.Greater;
      end if;
   end Compare;

   function None_Since_FF (Ops : in Config_Op_Arrays.Vector; Op : in Config_Op_Label) return Boolean
   is begin
      for O of reverse Ops loop
         exit when O.Op = Fast_Forward;
         if O.Op = Op then
            return False;
         end if;
      end loop;
      return True;
   end None_Since_FF;

   function Match_Since_FF (Ops : in Config_Op_Arrays.Vector; Op : in Config_Op) return Boolean
   is begin
      for O of reverse Ops loop
         exit when O.Op = Fast_Forward;
         if O = Op then
            return True;
         end if;
      end loop;
      return False;
   end Match_Since_FF;

   function Valid_Tree_Indices (Stack : in Recover_Stacks.Stack; Depth : in SAL.Base_Peek_Type) return Boolean
   is
      use all type Syntax_Trees.Node_Index;
   begin
      for I in 1 .. Depth loop
         if Stack (I).Tree_Index = Syntax_Trees.Invalid_Node_Index then
            return False;
         end if;
      end loop;
      return True;
   end Valid_Tree_Indices;

   procedure Set_Key (Item : in out Configuration; Key : in Integer)
   is begin
      Item.Cost := Key;
   end Set_Key;

   function Image
     (Item       : in Parse_Error;
      Tree       : in Syntax_Trees.Tree;
      Descriptor : in WisiToken.Descriptor)
     return String
   is begin
      case Item.Label is
      when Action =>
         return "Action, expecting: " & Image (Item.Expecting, Descriptor) &
           ", found" & Tree.Image (Item.Error_Token, Descriptor);

      when Check =>
         return "Check, " & Semantic_Checks.Image (Item.Check_Status, Descriptor);

      when Message =>
         return -Item.Msg;
      end case;
   end Image;

end WisiToken.Parse.LR;
