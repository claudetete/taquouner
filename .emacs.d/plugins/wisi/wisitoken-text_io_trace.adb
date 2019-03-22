--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 Free Software Foundation, Inc.
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

package body WisiToken.Text_IO_Trace is

   overriding
   procedure Put (Trace : in out Text_IO_Trace.Trace; Item : in String)
   is
      use Ada.Text_IO;
   begin
      if Trace.File /= null and then Is_Open (Trace.File.all) then
         Ada.Text_IO.Put (Trace.File.all, Item);
      else
         Ada.Text_IO.Put (Item);
      end if;
   end Put;

   overriding
   procedure Put_Line (Trace : in out Text_IO_Trace.Trace; Item : in String)
   is
      use Ada.Text_IO;
   begin
      if Trace.File /= null and then Is_Open (Trace.File.all) then
         Ada.Text_IO.Put_Line (Trace.File.all, Item);
         Ada.Text_IO.Flush (Trace.File.all);
      else
         Ada.Text_IO.Put_Line (Item);
         Ada.Text_IO.Flush;
      end if;
   end Put_Line;

   overriding
   procedure New_Line (Trace : in out Text_IO_Trace.Trace)
   is
      use Ada.Text_IO;
   begin
      if Trace.File /= null and then Is_Open (Trace.File.all) then
         Ada.Text_IO.New_Line (Trace.File.all);
      else
         Ada.Text_IO.New_Line;
      end if;
   end New_Line;

   procedure Set_File (Trace : in out Text_IO_Trace.Trace; File : in Ada.Text_IO.File_Access)
   is begin
      Trace.File := File;
   end Set_File;

   procedure Clear_File (Trace : in out Text_IO_Trace.Trace)
   is begin
      Trace.File := null;
   end Clear_File;

end WisiToken.Text_IO_Trace;
