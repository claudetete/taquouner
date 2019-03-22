--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017, 2018 Free Software Foundation, Inc.
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

package body SAL.Gen_Bounded_Definite_Vectors is

   function Length (Container : in Vector) return Ada.Containers.Count_Type
   is begin
      --  We assume the type ranges are sensible, so no exceptions occur
      --  here.
      return Ada.Containers.Count_Type (Container.Last - Index_Type'First + 1);
   end Length;

   function Is_Full (Container : in Vector) return Boolean
   is begin
      return To_Peek_Index (Container.Last) = Peek_Type (Capacity);
   end Is_Full;

   procedure Clear (Container : in out Vector)
   is begin
      Container.Last := No_Index;
   end Clear;

   function Element (Container : Vector; Index : Index_Type) return Element_Type
   is begin
      return Container.Elements (Peek_Type (Index - Index_Type'First + 1));
   end Element;

   function Last_Index (Container : Vector) return Extended_Index
   is begin
      return Container.Last;
   end Last_Index;

   procedure Set_Last (Container : in out Vector; Last : in Index_Type)
   is begin
      Container.Last := Last;
   end Set_Last;

   procedure Append (Container : in out Vector; New_Item : in Element_Type)
   is
      J : constant Peek_Type := To_Peek_Index (Container.Last + 1);
   begin
      if J > Container.Elements'Last then
         raise Container_Full;
      end if;
      Container.Elements (J) := New_Item;
      Container.Last := Container.Last + 1;
   end Append;

   procedure Prepend (Container : in out Vector; New_Item : in Element_Type)
   is
      J : constant Peek_Type := Peek_Type (Container.Last + 1 - Index_Type'First + 1);
   begin
      if J > Container.Elements'Last then
         raise Container_Full;
      end if;

      Container.Elements (2 .. J) := Container.Elements (1 .. J - 1);
      Container.Elements (1) := New_Item;
      Container.Last := Container.Last + 1;
   end Prepend;

   procedure Insert
     (Container : in out Vector;
      New_Item  : in     Element_Type;
      Before    : in     Extended_Index)
   is
      J : constant Peek_Type := To_Peek_Index ((if Before = No_Index then Container.Last + 1 else Before));
      K : constant Base_Peek_Type := To_Peek_Index (Container.Last);
   begin
      if K + 1 > Container.Elements'Last then
         raise Container_Full;
      end if;

      Container.Elements (J + 1 .. K + 1) := Container.Elements (J .. K);
      Container.Elements (J) := New_Item;
      Container.Last := Container.Last + 1;
   end Insert;

   function "+" (Item : in Element_Type) return Vector
   is begin
      return Result : Vector do
         Result.Append (Item);
      end return;
   end "+";

   function "&" (Left : in Vector; Right : in Element_Type) return Vector
   is begin
      return Result : Vector := Left do
         Result.Append (Right);
      end return;
   end "&";

   procedure Delete_First (Container : in out Vector; Count : in Index_Type := 1)
   is
      J : constant Peek_Type := Peek_Type (Container.Last - Index_Type'First + Count);
   begin
      if Count > Container.Last then
         raise Container_Empty;
      end if;
      Container.Elements (1 .. J - 1) := Container.Elements (2 .. J);
      Container.Last := Container.Last - Count;
   end Delete_First;

   function Constant_Reference (Container : aliased Vector; Index : in Index_Type) return Constant_Reference_Type
   is
      J : constant Peek_Type := Peek_Type (Index - Index_Type'First + 1);
   begin
      if Index > Container.Last then
         raise Constraint_Error;
      end if;
      return (Element => Container.Elements (J)'Access);
   end Constant_Reference;

   function Variable_Reference
     (Container : aliased in out Vector;
      Index     :         in     Index_Type)
     return Variable_Reference_Type
   is
      J : constant Peek_Type := Peek_Type (Index - Index_Type'First + 1);
   begin
      if Index > Container.Last then
         raise Constraint_Error;
      end if;
      return (Element => Container.Elements (J)'Access);
   end Variable_Reference;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      if Position.Container = null then
         return False;
      end if;

      return Position.Index <= Position.Container.Last;
   end Has_Element;

   overriding function First (Object : Iterator) return Cursor
   is begin
      if Object.Container.Last = No_Index then
         return (null, Index_Type'First);
      else
         return (Object.Container, Object.Container.First_Index);
      end if;
   end First;

   overriding function Last  (Object : Iterator) return Cursor
   is begin
      if Object.Container.Last = No_Index then
         return (null, Index_Type'First);
      else
         return (Object.Container, Object.Container.Last_Index);
      end if;
   end Last;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor
   is begin
      if Position.Index = Object.Container.Last then
         return (null, Index_Type'First);
      else
         return (Object.Container, Position.Index + 1);
      end if;
   end Next;

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor
   is begin
      if Position.Index = Index_Type'First then
         return (null, Index_Type'First);
      else
         return (Object.Container, Position.Index - 1);
      end if;
   end Previous;

   function Iterate (Container : Vector) return Vector_Iterator_Interfaces.Reversible_Iterator'Class
   is begin
      return Iterator'
        (Container => Container'Unrestricted_Access,
         Index     => No_Index);
   end Iterate;

   function Constant_Reference (Container : aliased Vector; Position : in Cursor) return Constant_Reference_Type
   is
      J : constant Peek_Type := Peek_Type (Position.Index - Index_Type'First + 1);
   begin
      return (Element => Container.Elements (J)'Access);
   end Constant_Reference;

   function Variable_Reference
     (Container : aliased in out Vector;
      Position  :         in     Cursor)
     return Variable_Reference_Type
   is
      J : constant Peek_Type := Peek_Type (Position.Index - Index_Type'First + 1);
   begin
      return (Element => Container.Elements (J)'Access);
   end Variable_Reference;

   ----------
   --  Spec private functions

   function To_Peek_Index (Index : in Extended_Index) return Base_Peek_Type
   is begin
      return Base_Peek_Type (Index - Index_Type'First + 1);
   end To_Peek_Index;

end SAL.Gen_Bounded_Definite_Vectors;
