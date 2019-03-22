--  Abstract:
--
--  see spec.
--
--  Copyright (C) 2017, 2018 Free Software Foundation, Inc.
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

pragma License (Modified_GPL);

with Ada.Directories;
with Ada.Strings.Unbounded;
with GNATCOLL.Mmap;
package body WisiToken.Lexer.re2c is

   overriding procedure Finalize (Object : in out Instance)
   is
      use all type System.Address;
   begin
      if Object.Lexer /= System.Null_Address then
         Free_Lexer (Object.Lexer);
         Object.Lexer := System.Null_Address;
      end if;

      Finalize (Object.Source);
   end Finalize;

   type Instance_Access is access Instance; --  silence compiler warning

   function New_Lexer
     (Trace  : not null access WisiToken.Trace'Class)
     return Handle
   is begin
      return Handle (Instance_Access'(new Instance (Trace)));
   end New_Lexer;

   overriding procedure Reset_With_String (Lexer : in out Instance; Input : in String)
   is begin
      Finalize (Lexer);

      --  We assume Input is in UTF-8 encoding
      Lexer.Source :=
        (Label       => String_Label,
         File_Name   => +"",
         Buffer      => new String'(Input),
         User_Buffer => False);

      Lexer.Lexer := New_Lexer
        (Buffer    => Lexer.Source.Buffer.all'Address,
         Length    => Interfaces.C.size_t (Input'Length),
         Verbosity => Interfaces.C.int (if Trace_Parse > 3 then Trace_Parse - 3 else 0));

      Reset (Lexer);
   end Reset_With_String;

   overriding procedure Reset_With_String_Access
     (Lexer     : in out Instance;
      Input     : in     Ada.Strings.Unbounded.String_Access;
      File_Name : in     Ada.Strings.Unbounded.Unbounded_String)
   is begin
      Finalize (Lexer);

      --  We assume Input is in UTF-8 encoding
      Lexer.Source :=
        (Label       => String_Label,
         File_Name   =>
           +(if Ada.Strings.Unbounded.Length (File_Name) = 0 then ""
             else Ada.Directories.Simple_Name (-File_Name)),
         Buffer      => Input,
         User_Buffer => True);

      Lexer.Lexer := New_Lexer
        (Buffer    => Lexer.Source.Buffer.all'Address,
         Length    => Interfaces.C.size_t (Input'Length),
         Verbosity => Interfaces.C.int (if Trace_Parse > 3 then Trace_Parse - 3 else 0));

      Reset (Lexer);
   end Reset_With_String_Access;

   overriding procedure Reset_With_File (Lexer : in out Instance; File_Name : in String)
   is
      use GNATCOLL.Mmap;
   begin
      Finalize (Lexer);

      --  We assume the file is in UTF-8 encoding
      Lexer.Source :=
        (File_Label, +Ada.Directories.Simple_Name (File_Name), Open_Read (File_Name), Invalid_Mapped_Region, 1);

      Lexer.Source.Region      := Read (Lexer.Source.File);
      Lexer.Source.Buffer_Last := Last (Lexer.Source.Region);

      if Integer (Length (Lexer.Source.File)) /= Lexer.Source.Buffer_Last then
         raise SAL.Programmer_Error with "not all of file is mapped; file length" &
           File_Size'Image (Length (Lexer.Source.File)) & " mapped:" & Integer'Image (Lexer.Source.Buffer_Last);
      end if;

      Lexer.Lexer := New_Lexer
        (Buffer    => Data (Lexer.Source.Region).all'Address,
         Length    => Interfaces.C.size_t (Last (Lexer.Source.Region)),
         Verbosity => Interfaces.C.int (if Trace_Parse > 3 then Trace_Parse - 3 else 0));

      Reset (Lexer);
   end Reset_With_File;

   overriding procedure Reset (Lexer : in out Instance)
   is begin
      Reset_Lexer (Lexer.Lexer);
      Lexer.Line            := 1;
      Lexer.Char_Line_Start := 1;
      Lexer.ID :=
        --  First token is assumed to be first on a line.
        (if Lexer.Trace.Descriptor.New_Line_ID = Invalid_Token_ID
         then Invalid_Token_ID
         else Lexer.Trace.Descriptor.New_Line_ID);
   end Reset;

   overriding function Find_Next
     (Lexer : in out Instance;
      Token :    out Base_Token)
     return Boolean
   is
      use Interfaces.C;

      procedure Build_Token
      is begin
         Token :=
           (ID => Lexer.ID,

            Byte_Region =>
              (Buffer_Pos (Lexer.Byte_Position),
               Base_Buffer_Pos (Lexer.Byte_Position + Lexer.Byte_Length - 1)),

            Line => Lexer.Line,

            Column =>
              (if Lexer.ID = Lexer.Trace.Descriptor.New_Line_ID or
                 Lexer.ID = Lexer.Trace.Descriptor.EOF_ID
               then 0
               else Ada.Text_IO.Count (Lexer.Char_Position - Lexer.Char_Line_Start)),

            Char_Region =>
              (Buffer_Pos (Lexer.Char_Position),
               Base_Buffer_Pos (Lexer.Char_Position + Lexer.Char_Length - 1)));
      end Build_Token;

   begin
      Lexer.Prev_ID := Lexer.ID;
      loop
         declare
            Status : constant int := Next_Token
              (Lexer.Lexer, Lexer.ID,
               Byte_Position => Interfaces.C.size_t (Lexer.Byte_Position),
               Byte_Length   => Interfaces.C.size_t (Lexer.Byte_Length),
               Char_Position => Interfaces.C.size_t (Lexer.Char_Position),
               Char_Length   => Interfaces.C.size_t (Lexer.Char_Length),
               Line_Start    => Interfaces.C.int (Lexer.Line));
         begin
            case Status is
            when 0 =>
               if Lexer.ID = Lexer.Trace.Descriptor.New_Line_ID then
                  Lexer.Char_Line_Start := Lexer.Char_Position + 1;
               end if;

               Build_Token;
               return False;

            when 1 =>
               --  Unrecognized character from lexer. Handle missing quotes by
               --  inserting a virtual quote at the existing quote, and telling the
               --  lexer to skip the char.
               declare
                  Buffer : constant GNATCOLL.Mmap.Str_Access := WisiToken.Lexer.Buffer (Lexer.Source);
               begin
                  if Buffer (Lexer.Byte_Position) = ''' then
                     --  Lexer has read to next new-line (or eof), then backtracked to next
                     --  char after '.
                     Lexer.Errors.Append
                       ((Buffer_Pos (Lexer.Char_Position), Invalid_Token_Index, (1 => ''', others => ASCII.NUL)));

                     Lexer.ID := Lexer.Trace.Descriptor.String_1_ID;
                     Build_Token;
                     return True;

                  elsif Buffer (Lexer.Byte_Position) = '"' then
                     --  Lexer has read to next new-line (or eof), then backtracked to next
                     --  char after ".
                     Lexer.Errors.Append
                       ((Buffer_Pos (Lexer.Char_Position), Invalid_Token_Index, (1 => '"', others => ASCII.NUL)));

                     Lexer.ID := Lexer.Trace.Descriptor.String_2_ID;
                     Build_Token;
                     return True;

                  else
                     --  Just skip the character; call Next_Token again.
                     Lexer.Errors.Append
                       ((Buffer_Pos (Lexer.Char_Position), Invalid_Token_Index, (others => ASCII.NUL)));
                  end if;
               end;

            when others =>
               raise Fatal_Error with " lexer returned unrecognized status code" & int'Image (Status);
            end case;
         end;
      end loop;
   end Find_Next;

   overriding function First (Lexer : in Instance) return Boolean
   is begin
      return Lexer.Trace.Descriptor.New_Line_ID /= Invalid_Token_ID and then
           Lexer.Prev_ID = Lexer.Trace.Descriptor.New_Line_ID;
   end First;

   overriding function Buffer_Text (Lexer : in Instance; Byte_Bounds : in Buffer_Region) return String
   is begin
      return String (Buffer (Lexer.Source) (Integer (Byte_Bounds.First) .. Integer (Byte_Bounds.Last)));
   end Buffer_Text;

   overriding function File_Name (Lexer : in Instance) return String
   is begin
      return File_Name (Lexer.Source);
   end File_Name;

end WisiToken.Lexer.re2c;
