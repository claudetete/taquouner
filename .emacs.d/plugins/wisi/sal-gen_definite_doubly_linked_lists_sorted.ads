--  Abstract :
--
--  A generic sorted doubly linked list with definite elements.
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

with Ada.Finalization;
with Ada.Iterator_Interfaces;
with Ada.Unchecked_Deallocation;
generic
   type Element_Type is private;
   with function Element_Compare (Left, Right : in Element_Type) return Compare_Result;
package SAL.Gen_Definite_Doubly_Linked_Lists_Sorted is
   use all type Ada.Containers.Count_Type;

   type List is new Ada.Finalization.Controlled with private
   with
      Constant_Indexing => Constant_Reference,
      Variable_Indexing => Reference,
      Default_Iterator  => Iterate,
      Iterator_Element  => Element_Type;

   --  If user uses Variable_Indexing, they must not change the sort
   --  order of the elements.

   type List_Access is access all List;
   for List_Access'Storage_Size use 0;

   Empty_List : constant List;

   overriding procedure Adjust (Container : in out List);
   --  Deep copy.

   overriding procedure Finalize (Container : in out List);
   --  Free all items in List.

   overriding function "=" (Left, Right : in List) return Boolean;
   --  True if contents are the same.

   function Length (Container : in List) return Ada.Containers.Count_Type;

   function To_List (Element : in Element_Type) return List;

   procedure Insert (Container : in out List; Element : in Element_Type);
   --  Insert Element before first item for which Element_Order (item,
   --  element) returns True.

   function Contains (Container : in List; Element : in Element_Type) return Boolean;

   procedure Merge
     (Target : in out List;
      Source : in     List;
      Added  :    out Boolean);
   --  Add all elements of Source to Target, if they are not already
   --  present.
   --
   --  Added is True if any element was not already present.

   procedure Merge
     (Target  : in out List;
      Source  : in     List;
      Added   :    out Boolean;
      Exclude : in     Element_Type);
   --  Add all elements of Source to Target, if they are not already
   --  present, and are not equal to Exclude.
   --
   --  Added is True if any element was not already present.

   type Cursor is private;

   No_Element : constant Cursor;

   function Has_Element (Position : in Cursor) return Boolean;

   function First (Container : in List) return Cursor;
   function Last (Container : in List) return Cursor;

   function Find (Container : in List; Element : in Element_Type) return Cursor;
   --  No_Element if Element not found.

   procedure Next (Position : in out Cursor)
   with Pre => Position /= No_Element;

   function Next (Position : in Cursor) return Cursor
   with Pre => Position /= No_Element;
   function Previous (Position : in Cursor) return Cursor
   with Pre => Position /= No_Element;

   function Element (Position : in Cursor) return Element_Type
   with Pre => Position /= No_Element;

   procedure Delete (Container : in out List; Position : in out Cursor)
   with Pre => Position /= No_Element;

   function Pop (Container : in out List) return Element_Type
   with Pre => Container.Length > 0;
   --  Return Container.First, delete it from Container.

   type Constant_Reference_Type (Element : not null access constant Element_Type) is null record
   with Implicit_Dereference => Element;

   function Constant_Reference (Container : in List; Position : in Cursor) return Constant_Reference_Type;
   function Constant_Ref (Position : in Cursor) return Constant_Reference_Type;

   type Reference_Type (Element : not null access Element_Type) is null record
   with Implicit_Dereference => Element;

   function Reference (Container : in List; Position : in Cursor) return Reference_Type
   with Pre => Position /= No_Element;
   function Ref (Position : in Cursor) return Reference_Type
   with Pre => Position /= No_Element;
   --  User must not change the element in a way that affects the sort order.

   package Iterator_Interfaces is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate (Container : aliased in List) return Iterator_Interfaces.Reversible_Iterator'Class;

private
   type Node_Type;

   type Node_Access is access Node_Type;

   type Node_Type is record
      Element : aliased Element_Type;
      Prev    : Node_Access;
      Next    : Node_Access;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Node_Type, Node_Access);

   type List is new Ada.Finalization.Controlled with record
      Head  : Node_Access               := null;
      Tail  : Node_Access               := null;
      Count : Ada.Containers.Count_Type := 0;
   end record;

   type Cursor is record
      Container : List_Access;
      Ptr       : Node_Access;
   end record;

   Empty_List : constant List := (Ada.Finalization.Controlled with null, null, 0);

   No_Element : constant Cursor := (null, null);

   type Iterator is new Iterator_Interfaces.Reversible_Iterator with
   record
      Container : List_Access;
   end record;

   overriding function First (Object : Iterator) return Cursor;
   overriding function Last  (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor;

end SAL.Gen_Definite_Doubly_Linked_Lists_Sorted;
