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

package body SAL.Gen_Bounded_Definite_Vectors.Gen_Sorted is

   overriding procedure Append (Container : in out Vector; New_Item : in Element_Type)
   is begin
      raise Programmer_Error;
   end Append;

   overriding procedure Prepend (Container : in out Vector; New_Item : in Element_Type)
   is begin
      raise Programmer_Error;
   end Prepend;

   overriding
   procedure Insert
     (Container : in out Vector;
      New_Item  : in     Element_Type;
      Before    : in     Extended_Index)
   is begin
      raise Programmer_Error;
   end Insert;

   procedure Insert
     (Container : in out Vector;
      New_Item  : in     Element_Type)
   is
      K : constant Base_Peek_Type := To_Peek_Index (Container.Last);
      J : Base_Peek_Type := K;
   begin
      if K + 1 > Container.Elements'Last then
         raise Container_Full;

      elsif K = 0 then
         --  Container empty
         Container.Last := Container.Last + 1;
         Container.Elements (1) := New_Item;
         return;
      end if;

      loop
         exit when J < 1;

         case Element_Compare (New_Item, Container.Elements (J)) is
         when Less =>
            J := J - 1;
         when Equal =>
            --  Insert after J
            exit;
         when Greater =>
            --  Insert after J
            exit;
         end case;
      end loop;

      if J = 0 then
         --  Insert before all
         Container.Elements (2 .. K + 1) := Container.Elements (1 .. K);
         Container.Elements (1) := New_Item;
      else
         --  Insert after J
         Container.Elements (J + 2 .. K + 1) := Container.Elements (J + 1 .. K);
         Container.Elements (J + 1) := New_Item;
      end if;
      Container.Last := Container.Last + 1;
   end Insert;

end SAL.Gen_Bounded_Definite_Vectors.Gen_Sorted;
