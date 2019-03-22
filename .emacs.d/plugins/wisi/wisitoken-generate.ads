--  Abstract :
--
--  Types and operations for generating parsers, common to all parser
--  types.
--
--  The wisi* packages deal with reading *.wy files and generating
--  source code files. The wisitoken-generate* packages deal with
--  computing parser properties from the grammar. (For historical
--  reasons, not all packages follow this naming convention yet).
--
--  References :
--
--  See wisitoken.ads
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

with WisiToken.Productions;
package WisiToken.Generate is

   Error : Boolean := False;
   --  Set True by errors during grammar generation

   function Error_Message
     (File_Name : in String;
      File_Line : in WisiToken.Line_Number_Type;
      Message   : in String)
     return String;

   procedure Put_Error (Message : in String);
   --  Set Error True, output Message to Standard_Error

   procedure Check_Consistent
     (Grammar          : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor       : in WisiToken.Descriptor;
      Source_File_Name : in String);
   --  Check requirements on Descriptor values.

   function Check_Unused_Tokens
     (Descriptor : in WisiToken.Descriptor;
      Grammar    : in WisiToken.Productions.Prod_Arrays.Vector)
     return Boolean;
   --  Return False if there is a terminal or nonterminal that is not
   --  used in the grammar.
   --
   --  Raises Grammar_Error if there is a non-grammar token used in the
   --  grammar.

   function Has_Empty_Production (Grammar : in WisiToken.Productions.Prod_Arrays.Vector) return Token_ID_Set;
   --  Result (ID) is True if any production for ID can be an empty
   --  production, recursively.

   function First
     (Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in Token_ID_Set;
      First_Terminal       : in Token_ID)
     return Token_Array_Token_Set;
   --  For each nonterminal in Grammar, find the set of tokens
   --  (terminal or nonterminal) that any string derived from it can
   --  start with. Together with Has_Empty_Production, implements
   --  algorithm FIRST from [dragon], augmented with nonterminals.
   --
   --  LALR, LR1 generate want First as both Token_Sequence_Arrays.Vector
   --  and Token_Array_Token_Set, Packrat wants Token_Array_Token_Set,
   --  existing tests all use Token_Array_Token_Set. So for LR1 we use
   --  To_Terminal_Sequence_Array.

   function To_Terminal_Sequence_Array
     (First      : in Token_Array_Token_Set;
      Descriptor : in WisiToken.Descriptor)
     return Token_Sequence_Arrays.Vector;
   --  Only includes terminals.

   function Follow
     (Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor           : in WisiToken.Descriptor;
      First                : in Token_Array_Token_Set;
      Has_Empty_Production : in Token_ID_Set)
     return Token_Array_Token_Set;
   --  For each nonterminal in Grammar, find the set of terminal
   --  tokens that can follow it. Implements algorithm FOLLOW from
   --  [dragon] pg 189.

   function Ancestors
     (Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor)
     return Token_Array_Token_Set;
   --  For each terminal and nonterm, record the nonterms it reduces to
   --  via one token reductions, recursively. In other words, if there is
   --  a production J <= I, then Ancestors (I, J) is True.

   function Descendants
     (Ancestors : in Token_Array_Token_Set)
     return Token_Sequence_Arrays.Vector;
   --  Inverse of Ancestors, excluding terminals. If there is a
   --  production J <= I and I is a nonterminal, then I is present in
   --  Descendants (J).

   ----------
   --  Indented text output. Mostly used for code generation in wisi,
   --  also used in outputing the parse_table and other debug stuff.

   Max_Line_Length : constant := 120;

   Indent     : Standard.Ada.Text_IO.Positive_Count := 1;
   Line_Count : Integer;

   procedure Indent_Line (Text : in String);
   --  Put Text, indented to Indent, to Current_Output, with newline.

   procedure Indent_Start (Text : in String);
   --  Put Text indented to Indent to Current_Output, without newline.
   --  Should be followed by Put_Line, not Indent_Line.

   procedure Indent_Wrap (Text : in String);
   --  Put Text, indented to Indent, wrapped at Max_Line_Length, to
   --  Current_Output, ending with newline.

   procedure Indent_Wrap_Comment (Text : in String; Comment_Syntax : in String);
   --  Put Text, prefixed by Comment_Syntax and two spaces, indented to
   --  Indent, wrapped at Max_Line_Length, to Current_Output, ending with
   --  newline.

end WisiToken.Generate;
