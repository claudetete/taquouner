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

with Ada.Strings.Unbounded;
with SAL;
with WisiToken.Generate;   use WisiToken.Generate;
with Wisitoken_Grammar_Actions; use Wisitoken_Grammar_Actions;
package body WisiToken_Grammar_Runtime is

   use WisiToken;

   function Get_Text
     (Data         : in User_Data_Type;
      Tree         : in Syntax_Trees.Tree;
      Tree_Index   : in Syntax_Trees.Valid_Node_Index;
      Strip_Quotes : in Boolean := False)
     return String
   is
      use all type Syntax_Trees.Node_Label;

      function Strip_Delimiters (Tree_Index : in Syntax_Trees.Valid_Node_Index) return String
      is
         Region : Buffer_Region renames Data.Terminals.all (Tree.Terminal (Tree_Index)).Byte_Region;
      begin
         if -Tree.ID (Tree_Index) in RAW_CODE_ID | REGEXP_ID | ACTION_ID then
            --  strip delimiters.
            return Data.Grammar_Lexer.Buffer_Text ((Region.First + 2, Region.Last - 2));

         elsif -Tree.ID (Tree_Index) in STRING_LITERAL_ID | STRING_LITERAL_CASE_INS_ID and Strip_Quotes then
            return Data.Grammar_Lexer.Buffer_Text ((Region.First + 1, Region.Last - 1));
         else
            return Data.Grammar_Lexer.Buffer_Text (Region);
         end if;
      end Strip_Delimiters;

   begin
      case Tree.Label (Tree_Index) is
      when Shared_Terminal =>
         return Strip_Delimiters (Tree_Index);

      when Virtual_Terminal =>
         raise SAL.Programmer_Error;

      when Nonterm =>
         declare
            use all type Ada.Strings.Unbounded.Unbounded_String;
            Result       : Ada.Strings.Unbounded.Unbounded_String;
            Tree_Indices : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Get_Terminals (Tree_Index);
            Need_Space   : Boolean                                      := False;
         begin
            for Tree_Index of Tree_Indices loop
               Result := Result & (if Need_Space then " " else "") & Strip_Delimiters (Tree_Index);
               Need_Space := True;
            end loop;
            return -Result;
         end;
      end case;
   end Get_Text;

   function Get_Child_Text
     (Data         : in User_Data_Type;
      Tree         : in Syntax_Trees.Tree;
      Parent       : in Syntax_Trees.Valid_Node_Index;
      Child        : in SAL.Peek_Type;
      Strip_Quotes : in Boolean := False)
     return String
   is
      Tree_Indices : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Get_Terminals (Parent);
   begin
      return Get_Text (Data, Tree, Tree_Indices (Child), Strip_Quotes);
   end Get_Child_Text;

   procedure Start_If_1
     (Data    : in out User_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      A_Index : in     Syntax_Trees.Valid_Node_Index;
      B_Index : in     Syntax_Trees.Valid_Node_Index)
   is
      use all type WisiToken.BNF.Generate_Algorithm;
      use all type WisiToken.BNF.Lexer_Type;
   begin
      if "lexer" = Get_Text (Data, Tree, A_Index) then
         Data.If_Lexer_Present := True;
         Data.Ignore_Lines     := Data.User_Lexer /= WisiToken.BNF.To_Lexer (Get_Text (Data, Tree, B_Index));

      elsif "parser" = Get_Text (Data, Tree, A_Index) then
         Data.If_Parser_Present := True;
         Data.Ignore_Lines := Data.User_Parser /= WisiToken.BNF.Generate_Algorithm'Value
           (Get_Text (Data, Tree, B_Index));

      else
         raise Grammar_Error with
           Error_Message
             (Data.Grammar_Lexer.File_Name, Data.Terminals.all (Tree.Min_Terminal_Index (A_Index)).Line,
              "invalid '%if'; must be one of {lexer | parser}");
      end if;
   end Start_If_1;

   function Get_RHS
     (Data  : in out User_Data_Type;
      Tree  : in     Syntax_Trees.Tree;
      Token : in     Syntax_Trees.Valid_Node_Index)
     return WisiToken.BNF.RHS_Type
   is
      use all type SAL.Base_Peek_Type;
      Tokens : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Children (Token);
   begin
      pragma Assert (-Tree.ID (Token) = rhs_ID);

      return RHS : WisiToken.BNF.RHS_Type do
         if Tokens'Length = 0 then
            --  Token is an empty rhs; parent is a possibly empty rhs_list; grandparent is
            --  a non-empty rhs_list or nonterminal.
            RHS.Source_Line := Data.Terminals.all (Tree.Min_Terminal_Index (Tree.Parent (Tree.Parent (Token)))).Line;

         else
            RHS.Source_Line := Data.Terminals.all (Tree.Min_Terminal_Index (Token)).Line;

            for I of Tree.Get_Terminals (Tokens (1)) loop
               RHS.Tokens.Append (Get_Text (Data, Tree, I));
            end loop;

            if Tokens'Last >= 2 then
               declare
                  Text : constant String := Get_Text (Data, Tree, Tokens (2));
               begin
                  if Text'Length > 0 then
                     RHS.Action := +Text;
                     Data.Action_Count := Data.Action_Count + 1;
                  end if;
               end;
            end if;

            if Tokens'Last >= 3 then
               RHS.Check := +Get_Text (Data, Tree, Tokens (3));
               Data.Check_Count := Data.Check_Count + 1;
            end if;
         end if;
      end return;
   end Get_RHS;

   procedure Get_Right_Hand_Sides
     (Data             : in out User_Data_Type;
      Tree             : in     WisiToken.Syntax_Trees.Tree;
      Right_Hand_Sides : in out WisiToken.BNF.RHS_Lists.List;
      Token            : in     WisiToken.Syntax_Trees.Valid_Node_Index)
   is
      use all type SAL.Base_Peek_Type;

      Tokens : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Children (Token);
   begin
      pragma Assert (-Tree.ID (Token) = rhs_list_ID);

      if Tokens'Last = 1 then
         --  | rhs
         if not Data.Ignore_Lines then
            Right_Hand_Sides.Append (Get_RHS (Data, Tree, Tokens (1)));
         end if;
      else
         --  | rhs_list BAR rhs
         --  | rhs_list PERCENT IF IDENTIFIER EQUAL IDENTIFIER
         --  | rhs_list PERCENT END IF
         Get_Right_Hand_Sides (Data, Tree, Right_Hand_Sides, Tokens (1));

         case Token_Enum_ID'(-Tree.ID (Tokens (3))) is
         when rhs_ID =>
            if not Data.Ignore_Lines then
               Right_Hand_Sides.Append (Get_RHS (Data, Tree, Tokens (3)));
            end if;

         when IF_ID =>
            Start_If_1 (Data, Tree, Tokens (4), Tokens (6));

         when END_ID =>
            Data.Ignore_Lines := False;

         when others =>
            raise SAL.Programmer_Error;
         end case;
      end if;
   end Get_Right_Hand_Sides;

   ----------
   --  Public subprograms, declaration order

   overriding
   procedure Set_Lexer_Terminals
     (User_Data : in out User_Data_Type;
      Lexer     : in     WisiToken.Lexer.Handle;
      Terminals : in     Base_Token_Array_Access)
   is begin
      User_Data.Grammar_Lexer := Lexer;
      User_Data.Terminals     := Terminals;
   end Set_Lexer_Terminals;

   overriding procedure Reset (Data : in out User_Data_Type)
   is begin
      --  Preserve Grammar_Lexer
      --  Preserve User_Lexer
      --  Preserve User_Parser
      --  Perserve Generate_Set
      --  Preserve Terminals
      Data.Raw_Code          := (others => <>);
      Data.Language_Params   := (others => <>);
      WisiToken.BNF.Free (Data.Generate_Set);
      Data.Tokens            := (others => <>);
      Data.User_Names        := (others => <>);
      Data.Conflicts.Clear;
      Data.McKenzie_Recover  := (others => <>);
      Data.Rule_Count        := 0;
      Data.Action_Count      := 0;
      Data.Check_Count       := 0;
      Data.If_Lexer_Present  := False;
      Data.If_Parser_Present := False;
      Data.Ignore_Lines      := False;
   end Reset;

   procedure Start_If
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)
   is begin
      Start_If_1 (User_Data_Type (User_Data), Tree, Tokens (3), Tokens (5));
   end Start_If;

   procedure End_If (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class)
   is
      Data : User_Data_Type renames User_Data_Type (User_Data);
   begin
      Data.Ignore_Lines := False;
   end End_If;

   procedure Add_Declaration
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)
   is
      use all type Ada.Strings.Unbounded.Unbounded_String;

      Data : User_Data_Type renames User_Data_Type (User_Data);

      function Token (Index : in SAL.Peek_Type) return Base_Token
      is
         use all type WisiToken.Syntax_Trees.Node_Label;
         use all type SAL.Base_Peek_Type;
      begin
         if Tokens'Last < Index then
            raise SAL.Programmer_Error;
         elsif Tree.Label (Tokens (Index)) /= WisiToken.Syntax_Trees.Shared_Terminal then
            raise SAL.Programmer_Error;
         else
            return Data.Terminals.all (Tree.Terminal (Tokens (Index)));
         end if;
      end Token;

      function Enum_ID (Index : in SAL.Peek_Type) return Token_Enum_ID
        is (Token_Enum_ID'(-Token (Index).ID));

   begin
      --  Add declaration to User_Data.Generate_Set, Language_Params,
      --  Tokens, Conflicts, or McKenzie_Recover.

      if Data.Ignore_Lines then
         return;
      end if;

      case Tree.Label (Tokens (2)) is
      when Syntax_Trees.Nonterm =>
         --  must be token_keyword_non_grammar
         declare
            Children : Syntax_Trees.Valid_Node_Index_Array renames Tree.Children (Tokens (2));
            Child_1  : Base_Token renames Data.Terminals.all (Tree.Terminal (Children (1)));
         begin
            case Token_Enum_ID'(-Child_1.ID) is
            when Wisitoken_Grammar_Actions.TOKEN_ID =>

               WisiToken.BNF.Add_Token
                 (Data.Tokens.Tokens,
                  Kind  => Get_Text (Data, Tree, Children (3)),
                  Name  => Get_Text (Data, Tree, Tokens (3)),
                  Value => Get_Text (Data, Tree, Tokens (4)));

            when KEYWORD_ID =>

               Data.Tokens.Keywords.Append
                 ((Name  => +Get_Text (Data, Tree, Tokens (3)),
                   Value => +Get_Text (Data, Tree, Tokens (4))));

            when NON_GRAMMAR_ID =>

               WisiToken.BNF.Add_Token
                 (Data.Tokens.Non_Grammar,
                  Kind  => Get_Text (Data, Tree, Children (3)),
                  Name  => Get_Text (Data, Tree, Tokens (3)),
                  Value => Get_Text (Data, Tree, Tokens (4)));

            when others =>
               raise SAL.Programmer_Error;
            end case;
         end;

      when Syntax_Trees.Shared_Terminal =>
         case Enum_ID (2) is
         when CODE_ID =>
            declare
               Location : WisiToken.BNF.Raw_Code_Location;

               --  % code identifier_list raw_code
               --  1 2    3               4
               --
               --  identifier_list = "action spec context"
               --  identifier_list children = identifier_list IDENTIFIER_ID
               --  children = identifier_list IDENTIFIER_ID
               --  children = IDENTIFIER_ID
               function Get_Loc_List return Base_Token_Array
               is
                  use all type SAL.Base_Peek_Type;
                  use WisiToken.Syntax_Trees;
                  Node   : Valid_Node_Index := Tokens (3);
                  Result : Base_Token_Array (1 .. 3);
                  First  : SAL.Peek_Type    := Result'Last + 1;
               begin
                  loop
                     pragma Assert (-Tree.ID (Node) = identifier_list_ID);
                     exit when not Tree.Has_Children (Node);
                     declare
                        Children : constant Valid_Node_Index_Array := Tree.Children (Node);
                     begin
                        if Children'Length = 1 then
                           --  identifier_list : IDENTIFIER
                           First := First - 1;
                           Result (First) := Data.Terminals.all (Tree.Terminal (Children (1)));
                           exit;

                        elsif Children'Length = 2 then
                           --  identifier_list : identifier_list IDENTIFIER
                           First := First - 1;
                           Result (First) := Data.Terminals.all (Tree.Terminal (Children (2)));

                           Node := Children (1);
                        else
                           raise SAL.Programmer_Error;
                        end if;
                     end;
                  end loop;
                  return Result (First .. Result'Last);
               end Get_Loc_List;

               Loc_List : constant Base_Token_Array := Get_Loc_List;

               function Get_Loc (Index : in SAL.Peek_Type) return String
               is (Data.Grammar_Lexer.Buffer_Text (Loc_List (Index).Byte_Region));

            begin
               if Get_Loc (Loc_List'First) = "actions" then
                  Location :=
                    (if Get_Loc (2) = "spec" then
                       (if Get_Loc (3) = "context" then WisiToken.BNF.Actions_Spec_Context
                        elsif Get_Loc (3) = "pre" then WisiToken.BNF.Actions_Spec_Pre
                        elsif Get_Loc (3) = "post" then WisiToken.BNF.Actions_Spec_Post
                        else raise Grammar_Error with
                          Error_Message
                            (Data.Grammar_Lexer.File_Name, Loc_List (2).Line,
                            "expecting {context | pre | post}"))

                     elsif Get_Loc (2) = "body" then
                       (if Get_Loc (3) = "context" then WisiToken.BNF.Actions_Body_Context
                        elsif Get_Loc (3) = "pre" then WisiToken.BNF.Actions_Body_Pre
                        elsif Get_Loc (3) = "post" then WisiToken.BNF.Actions_Body_Post
                        else raise Grammar_Error with
                          Error_Message
                            (Data.Grammar_Lexer.File_Name, Loc_List (2).Line,
                            "expecting {context | pre | post}"))

                     else raise Grammar_Error);

               elsif Get_Loc (Loc_List'First) = "copyright_license" then
                  Location := WisiToken.BNF.Copyright_License;

               else
                  raise Grammar_Error with
                    Error_Message
                      (Data.Grammar_Lexer.File_Name, Loc_List (Loc_List'First).Line,
                       "expecting {actions | copyright_license}");
               end if;

               Data.Raw_Code (Location) := WisiToken.BNF.Split_Lines (Get_Text (Data, Tree, Tokens (4)));
            exception
            when Grammar_Error =>
               Put_Error
                 (Error_Message
                    (Data.Grammar_Lexer.File_Name, Token (2).Line, Token (2).Column,
                     "invalid raw code location; actions {spec | body} {context | pre | post}"));
            end;

         when IDENTIFIER_ID =>
            declare
               Kind : constant String := Data.Grammar_Lexer.Buffer_Text (Token (2).Byte_Region);
            begin
               if Kind = "case_insensitive" then
                  Data.Language_Params.Case_Insensitive := True;

               elsif Kind = "conflict" then
                  declare
                     Tree_Indices : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Get_Terminals
                       (Tokens (3));
                     --   %conflict <action_a>/<action_b> in state <LHS_A>, <LHS_B> on token <on>
                     --              1        2 3         4  5      6     7  8      9  10     11
                  begin
                     Data.Conflicts.Append
                       ((Source_Line => Data.Terminals.all (Tree.Terminal (Tree_Indices (1))).Line,
                         Action_A    => +Get_Text (Data, Tree, Tree_Indices (1)),
                         LHS_A       => +Get_Text (Data, Tree, Tree_Indices (6)),
                         Action_B    => +Get_Text (Data, Tree, Tree_Indices (3)),
                         LHS_B       => +Get_Text (Data, Tree, Tree_Indices (8)),
                         On          => +Get_Text (Data, Tree, Tree_Indices (11))));
                  end;

               elsif Kind = "end" then
                  --  matching '%if' specified current lexer.
                  null;

               elsif Kind = "elisp_face" then
                  Data.User_Names.Faces.Append (Get_Text (Data, Tree, Tokens (3), Strip_Quotes => True));

               elsif Kind = "elisp_indent" then
                  Data.User_Names.Indents.Append
                    ((Name  => +Get_Child_Text (Data, Tree, Tokens (3), 1, Strip_Quotes => True),
                      Value => +Get_Child_Text (Data, Tree, Tokens (3), 2)));

               elsif Kind = "embedded_quote_escape_doubled" then
                  Data.Language_Params.Embedded_Quote_Escape_Doubled := True;

               elsif Kind = "end_names_optional_option" then
                  Data.Language_Params.End_Names_Optional_Option := +Get_Text (Data, Tree, Tokens (3));

               elsif Kind = "generate" then
                  declare
                     Children : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Get_Terminals (Tokens (3));
                     Tuple     : WisiToken.BNF.Generate_Tuple;
                  begin
                     Tuple.Gen_Alg  := WisiToken.BNF.Generate_Algorithm'Value (Get_Text (Data, Tree, Children (1)));
                     Tuple.Out_Lang := WisiToken.BNF.To_Output_Language (Get_Text (Data, Tree, Children (2)));
                     for I in 3 .. SAL.Base_Peek_Type (Children'Length) loop
                        declare
                           Text : constant String := Get_Text (Data, Tree, Children (I));
                        begin
                           if Text = "text_rep" then
                              Tuple.Text_Rep := True;

                           elsif (for some I of WisiToken.BNF.Lexer_Image => Text = I.all) then
                              Tuple.Lexer := WisiToken.BNF.To_Lexer (Text);

                           elsif (for some I in WisiToken.BNF.Valid_Interface =>
                                    WisiToken.BNF.To_Lower (Text) = WisiToken.BNF.To_Lower
                                      (WisiToken.BNF.Valid_Interface'Image (I)))
                           then
                              Tuple.Interface_Kind := WisiToken.BNF.Valid_Interface'Value (Text);
                           else
                              declare
                                 Token : Base_Token renames Data.Terminals.all (Tree.Terminal (Children (I)));
                              begin
                                 raise Grammar_Error with Error_Message
                                   (Data.Grammar_Lexer.File_Name, Token.Line, Token.Column,
                                    "invalid generate param '" & Text & "'");
                              end;
                           end if;
                        end;
                     end loop;
                     WisiToken.BNF.Add (Data.Generate_Set, Tuple);
                  end;

               elsif Kind = "mckenzie_check_limit" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Check_Limit := Token_Index'Value (Get_Text (Data, Tree, Tokens (3)));

               elsif Kind = "mckenzie_check_delta_limit" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Check_Delta_Limit := Integer'Value (Get_Text (Data, Tree, Tokens (3)));

               elsif Kind = "mckenzie_cost_default" then
                  if Tree.Get_Terminals (Tokens (3))'Length /= 4 then
                     raise Grammar_Error with
                       Error_Message
                         (Data.Grammar_Lexer.File_Name, Data.Terminals.all (Tree.Min_Terminal_Index (Tokens (3))).Line,
                          "too " & (if Tree.Get_Terminals (Tokens (3))'Length > 4 then "many" else "few") &
                            " default costs; should be 'insert, delete, push back, ignore check fail'.");
                  end if;

                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Source_Line := Data.Terminals.all (Tree.Min_Terminal_Index (Tokens (1))).Line;

                  Data.McKenzie_Recover.Default_Insert          := Natural'Value
                    (Get_Child_Text (Data, Tree, Tokens (3), 1));
                  Data.McKenzie_Recover.Default_Delete_Terminal := Natural'Value
                    (Get_Child_Text (Data, Tree, Tokens (3), 2));
                  Data.McKenzie_Recover.Default_Push_Back       := Natural'Value
                    (Get_Child_Text (Data, Tree, Tokens (3), 3));
                  Data.McKenzie_Recover.Ignore_Check_Fail       := Natural'Value
                    (Get_Child_Text (Data, Tree, Tokens (3), 4));

               elsif Kind = "mckenzie_cost_delete" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Delete.Append
                    ((+Get_Child_Text (Data, Tree, Tokens (3), 1),
                      +Get_Child_Text (Data, Tree, Tokens (3), 2)));

               elsif Kind = "mckenzie_cost_insert" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Insert.Append
                    ((+Get_Child_Text (Data, Tree, Tokens (3), 1),
                      +Get_Child_Text (Data, Tree, Tokens (3), 2)));

               elsif Kind = "mckenzie_cost_limit" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Cost_Limit := Natural'Value (Get_Text (Data, Tree, Tokens (3)));

               elsif Kind = "mckenzie_cost_push_back" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Push_Back.Append
                    ((+Get_Child_Text (Data, Tree, Tokens (3), 1),
                      +Get_Child_Text (Data, Tree, Tokens (3), 2)));

               elsif Kind = "mckenzie_enqueue_limit" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Enqueue_Limit := Natural'Value (Get_Text (Data, Tree, Tokens (3)));

               elsif Kind = "no_language_runtime" then
                  Data.Language_Params.Language_Runtime := False;

               elsif Kind = "no_enum" then
                  Data.Language_Params.Declare_Enums := False;

               elsif Kind = "start" then
                  Data.Language_Params.Start_Token := +Get_Text (Data, Tree, Tokens (3));

               elsif Kind = "re2c_regexp" then
                  Data.Tokens.re2c_Regexps.Append
                    ((+Get_Child_Text (Data, Tree, Tokens (3), 1),
                      +Get_Child_Text (Data, Tree, Tokens (3), 2)));

               else
                  raise Grammar_Error with Error_Message
                    (Data.Grammar_Lexer.File_Name, Token (2).Line, Token (2).Column, "unexpected syntax");

               end if;
            end;

         when others =>
            raise Grammar_Error with Error_Message
              (Data.Grammar_Lexer.File_Name, Token (2).Line, Token (2).Column, "unexpected syntax");
         end case;

      when Syntax_Trees.Virtual_Terminal =>
         raise SAL.Programmer_Error;
      end case;
   end Add_Declaration;

   procedure Add_Nonterminal
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)
   is
      Data : User_Data_Type renames User_Data_Type (User_Data);

      LHS : constant String := Get_Text (Data, Tree, Tokens (1));

      Right_Hand_Sides : WisiToken.BNF.RHS_Lists.List;
   begin
      Data.Rule_Count := Data.Rule_Count + 1;

      Get_Right_Hand_Sides (Data, Tree, Right_Hand_Sides, Tokens (3));

      if WisiToken.BNF.Is_Present (Data.Tokens.Rules, LHS) then
         declare
            LHS_Token : Base_Token renames Data.Terminals.all (Tree.Terminal (Tokens (1)));
         begin
            raise Grammar_Error with Error_Message
              (Data.Grammar_Lexer.File_Name, LHS_Token.Line, LHS_Token.Column, "duplicate nonterm");
         end;
      else
         Data.Tokens.Rules.Append
           ((+LHS, Right_Hand_Sides,
             Source_Line => Data.Terminals.all (Tree.Min_Terminal_Index (Tokens (1))).Line));
      end if;
   end Add_Nonterminal;

end WisiToken_Grammar_Runtime;
