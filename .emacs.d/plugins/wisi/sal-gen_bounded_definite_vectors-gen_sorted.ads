--  Abstract :
--
--  Add sorted behavior to parent.
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

generic
   with function Element_Compare (Left, Right : in Element_Type) return Compare_Result;
package SAL.Gen_Bounded_Definite_Vectors.Gen_Sorted is

   type Vector is new SAL.Gen_Bounded_Definite_Vectors.Vector with null record;

   overriding procedure Append (Container : in out Vector; New_Item : in Element_Type)
   with Inline => True;
   --  Raises Programmer_Error

   overriding procedure Prepend (Container : in out Vector; New_Item : in Element_Type)
   with Inline => True;
   --  Raises Programmer_Error

   overriding
   procedure Insert
     (Container : in out Vector;
      New_Item  : in     Element_Type;
      Before    : in     Extended_Index)
   with Inline => True;
   --  Raises Programmer_Error

   not overriding
   procedure Insert
     (Container : in out Vector;
      New_Item  : in     Element_Type);
   --  Insert New_Item in sorted position. Items are sorted in increasing
   --  order according to Element_Compare. New_Item is inserted after
   --  Equal items.

end SAL.Gen_Bounded_Definite_Vectors.Gen_Sorted;
