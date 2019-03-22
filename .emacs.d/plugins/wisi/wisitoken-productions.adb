--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Free Software Foundation, Inc.
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

with Ada.Text_IO;
package body WisiToken.Productions is

   function Image
     (LHS        : in Token_ID;
      RHS_Index  : in Natural;
      RHS        : in Token_ID_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor)
     return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := +Trimmed_Image ((LHS, RHS_Index)) & ": " & Image (LHS, Descriptor) & " <=";
   begin
      for ID of RHS loop
         Result := Result & ' ' & Image (ID, Descriptor);
      end loop;
      return To_String (Result);
   end Image;

   procedure Put (Grammar : Prod_Arrays.Vector; Descriptor : in WisiToken.Descriptor)
   is begin
      for P of Grammar loop
         for R in P.RHSs.First_Index .. P.RHSs.Last_Index loop
            Ada.Text_IO.Put_Line (Image (P.LHS, R, P.RHSs (R).Tokens, Descriptor));
         end loop;
      end loop;
   end Put;

end WisiToken.Productions;
