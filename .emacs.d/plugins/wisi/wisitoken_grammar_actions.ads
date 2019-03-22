--  generated parser support file.
--  command line: wisitoken-bnf-generate.exe  --generate LALR Ada re2c PROCESS wisitoken_grammar.wy
--

--  Copyright (C) 2017, 2018 Free Software Foundation, Inc.
--
--  Author: Stephen Leake <stephe-leake@stephe-leake.org>
--
--  This file is part of GNU Emacs.
--
--  GNU Emacs is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  GNU Emacs is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

with WisiToken.Syntax_Trees;
package Wisitoken_Grammar_Actions is

   Descriptor : aliased WisiToken.Descriptor :=
     (First_Terminal                => 3,
      Last_Terminal                 => 25,
      First_Nonterminal             => 26,
      Last_Nonterminal              => 37,
      EOF_ID                        => 25,
      Accept_ID                     => 26,
      Case_Insensitive              => False,
      New_Line_ID                   => 1,
      Comment_ID                    => 2,
      Left_Paren_ID                 => 2147483647,
      Right_Paren_ID                => 2147483647,
      String_1_ID                   => 24,
      String_2_ID                   => 23,
      Embedded_Quote_Escape_Doubled => False,
      Image                         =>
        (new String'("WHITESPACE"),
         new String'("NEW_LINE"),
         new String'("COMMENT"),
         new String'("CODE"),
         new String'("END"),
         new String'("IF"),
         new String'("KEYWORD"),
         new String'("NON_GRAMMAR"),
         new String'("TOKEN"),
         new String'("RAW_CODE"),
         new String'("REGEXP"),
         new String'("ACTION"),
         new String'("BAR"),
         new String'("COLON"),
         new String'("COMMA"),
         new String'("EQUAL"),
         new String'("GREATER"),
         new String'("LESS"),
         new String'("PERCENT"),
         new String'("SEMICOLON"),
         new String'("SLASH"),
         new String'("NUMERIC_LITERAL"),
         new String'("IDENTIFIER"),
         new String'("STRING_LITERAL"),
         new String'("STRING_LITERAL_CASE_INS"),
         new String'("Wisi_EOI"),
         new String'("wisitoken_accept"),
         new String'("declaration"),
         new String'("token_keyword_non_grammar"),
         new String'("identifier_list"),
         new String'("declaration_item_list"),
         new String'("declaration_item"),
         new String'("nonterminal"),
         new String'("rhs_list"),
         new String'("rhs"),
         new String'("token_list"),
         new String'("compilation_unit"),
         new String'("compilation_unit_list")),
      Terminal_Image_Width => 23,
      Image_Width          => 25,
      Last_Lookahead       => 26);

   type Token_Enum_ID is
     (WHITESPACE_ID,
      NEW_LINE_ID,
      COMMENT_ID,
      CODE_ID,
      END_ID,
      IF_ID,
      KEYWORD_ID,
      NON_GRAMMAR_ID,
      TOKEN_ID,
      RAW_CODE_ID,
      REGEXP_ID,
      ACTION_ID,
      BAR_ID,
      COLON_ID,
      COMMA_ID,
      EQUAL_ID,
      GREATER_ID,
      LESS_ID,
      PERCENT_ID,
      SEMICOLON_ID,
      SLASH_ID,
      NUMERIC_LITERAL_ID,
      IDENTIFIER_ID,
      STRING_LITERAL_ID,
      STRING_LITERAL_CASE_INS_ID,
      Wisi_EOI_ID,
      wisitoken_accept_ID,
      declaration_ID,
      token_keyword_non_grammar_ID,
      identifier_list_ID,
      declaration_item_list_ID,
      declaration_item_ID,
      nonterminal_ID,
      rhs_list_ID,
      rhs_ID,
      token_list_ID,
      compilation_unit_ID,
      compilation_unit_list_ID);

   type Token_Enum_ID_Array is array (Positive range <>) of Token_Enum_ID;
   use all type WisiToken.Token_ID;
   function "+" (Item : in Token_Enum_ID) return WisiToken.Token_ID
     is (WisiToken.Token_ID'First + Token_Enum_ID'Pos (Item));
   function To_Token_Enum (Item : in WisiToken.Token_ID) return Token_Enum_ID
     is (Token_Enum_ID'Val (Item - WisiToken.Token_ID'First));
   function "-" (Item : in WisiToken.Token_ID) return Token_Enum_ID renames To_Token_Enum;

   procedure declaration_0
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);
   procedure declaration_1
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);
   procedure declaration_2
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);
   procedure declaration_3
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);
   procedure declaration_4
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);
   procedure declaration_5
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);
   procedure nonterminal_0
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);
end Wisitoken_Grammar_Actions;
