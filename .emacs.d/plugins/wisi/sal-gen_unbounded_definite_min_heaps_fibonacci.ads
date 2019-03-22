--  Abstract:
--
--  An unbounded minimum Fibonacci heap of definite non-limited elements.
--
--  References:
--
--  [1] Introduction to Algorithms, Third Edition. Thomas H. Cormen,
--  Charles E. Leiserson, Ronald L. Rivest, Clifford Stein. Chapter 19.
--
--  Copyright (C) 2017, 2018 Free Software Foundation, Inc.
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

with Ada.Finalization;
generic
   type Element_Type is private;
   type Element_Access is access all Element_Type;
   type Key_Type is private;
   with function Key (Item : in Element_Type) return Key_Type;
   with procedure Set_Key (Item : in out Element_Type; Key : in Key_Type);
   pragma Unreferenced (Set_Key); -- needed for Decrease_Key
   with function "<" (Left, Right : in Key_Type) return Boolean is <>;
package SAL.Gen_Unbounded_Definite_Min_Heaps_Fibonacci is

   type Heap_Type is new Ada.Finalization.Controlled with private;

   Empty_Heap : constant Heap_Type;

   overriding
   procedure Initialize (Object : in out Heap_Type);

   overriding
   procedure Finalize (Object : in out Heap_Type);

   overriding
   procedure Adjust (Object : in out Heap_Type);

   procedure Clear (Heap : in out Heap_Type);
   --  Empty Heap.

   function Count (Heap : in Heap_Type) return Base_Peek_Type;
   --  Return count of elements in Heap.

   function Remove (Heap : in out Heap_Type) return Element_Type;
   --  Remove minimum element in Heap, return it.

   function Min_Key (Heap : in out Heap_Type) return Key_Type;
   --  Return a copy of the minimum key value.

   function Get (Heap : in out Heap_Type) return Element_Type renames Remove;

   procedure Drop (Heap : in out Heap_Type);
   --  Remove minimum element in Heap, discard it.

   procedure Add (Heap : in out Heap_Type; Item : in Element_Type);
   --  Add Item to Heap.

   procedure Insert (Heap : in out Heap_Type; Item : in Element_Type) renames Add;

   function Add (Heap : in out Heap_Type; Item : in Element_Type) return Element_Access;
   --  Add Item to Heap, return a pointer to it. This avoids extra
   --  copying of Item.
   --
   --  Result is valid at least until next Get.

   --  Despite being called a "mergeable heap" in [1], there is no
   --  algorithm for merging two Fibonacci heaps. And the naive method of
   --  simply splicing the root lists apparently breaks the consolidate
   --  algorithm; it assumes there can only be one tree of each degree >
   --  0.

   --  procedure Increase_Key (Heap : in out Heap_Type; index : in index_type; Item : in Element_Type);
   --  IMPROVEME: implement. need Index (heap, Key), or Add return index.

   type Constant_Reference_Type (Element : not null access constant Element_Type) is null record
   with Implicit_Dereference => Element;

   function Peek (Heap : in Heap_Type) return Constant_Reference_Type;
   --  Return a constant reference to the min element.

private

   type Node;
   type Node_Access is access Node;

   type Node is record
      Element : aliased Element_Type;
      Parent  : Node_Access;
      Child   : Node_Access;
      Left    : Node_Access;
      Right   : Node_Access;
      Degree  : Natural;
      Mark    : Boolean;
   end record;

   type Heap_Type is new Ada.Finalization.Controlled with record
      Min   : Node_Access;
      Count : Base_Peek_Type;
   end record;

   Empty_Heap : constant Heap_Type := (Ada.Finalization.Controlled with Min => null, Count => 0);

end SAL.Gen_Unbounded_Definite_Min_Heaps_Fibonacci;
