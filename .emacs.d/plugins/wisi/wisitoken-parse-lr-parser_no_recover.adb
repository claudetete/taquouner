--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 - 2005, 2008 - 2015, 2017, 2018 Free Software Foundation, Inc.
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

package body WisiToken.Parse.LR.Parser_No_Recover is

   procedure Reduce_Stack_1
     (Current_Parser : in     Parser_Lists.Cursor;
      Action         : in     Reduce_Action_Rec;
      Nonterm        :    out WisiToken.Syntax_Trees.Valid_Node_Index;
      Trace          : in out WisiToken.Trace'Class)
   is
      use all type SAL.Base_Peek_Type;

      Parser_State  : Parser_Lists.Parser_State renames Current_Parser.State_Ref.Element.all;
      Children_Tree : Syntax_Trees.Valid_Node_Index_Array (1 .. SAL.Base_Peek_Type (Action.Token_Count));
      --  for Set_Children.
   begin
      for I in reverse Children_Tree'Range loop
         Children_Tree (I) := Parser_State.Stack.Pop.Token;
      end loop;

      Nonterm := Parser_State.Tree.Add_Nonterm
        (Action.Production, Children_Tree, Action.Action, Default_Virtual => False);
      --  Computes Nonterm.Byte_Region

      if Trace_Parse > Detail then
         Trace.Put_Line (Parser_State.Tree.Image (Nonterm, Trace.Descriptor.all, Include_Children => True));
      end if;
   end Reduce_Stack_1;

   procedure Do_Action
     (Action         : in Parse_Action_Rec;
      Current_Parser : in Parser_Lists.Cursor;
      Shared_Parser  : in Parser)
   is
      Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
      Trace        : WisiToken.Trace'Class renames Shared_Parser.Trace.all;
      Nonterm      : WisiToken.Syntax_Trees.Valid_Node_Index;
   begin
      if Trace_Parse > Detail then
         Trace.Put
           (Integer'Image (Current_Parser.Label) & ": " &
              Trimmed_Image (Parser_State.Stack.Peek.State) & ": " &
              Parser_State.Tree.Image (Parser_State.Current_Token, Trace.Descriptor.all) & " : ");
         Put (Trace, Action);
         Trace.New_Line;
      end if;

      case Action.Verb is
      when Shift =>
         Current_Parser.Set_Verb (Shift);
         Parser_State.Stack.Push ((Action.State, Parser_State.Current_Token));
         Parser_State.Tree.Set_State (Parser_State.Current_Token, Action.State);

      when Reduce =>
         Current_Parser.Set_Verb (Reduce);

         Reduce_Stack_1 (Current_Parser, Action, Nonterm, Trace);

         Parser_State.Stack.Push
           ((State    => Goto_For
               (Table => Shared_Parser.Table.all,
                State => Parser_State.Stack (1).State,
                ID    => Action.Production.LHS),
             Token    => Nonterm));

         Parser_State.Tree.Set_State (Nonterm, Parser_State.Stack (1).State);

         if Trace_Parse > Detail then
            Trace.Put_Line (" ... goto state " & Trimmed_Image (Parser_State.Stack.Peek.State));
         end if;

      when Accept_It =>
         Current_Parser.Set_Verb (Accept_It);
         Reduce_Stack_1
           (Current_Parser,
            (Reduce, Action.Production, Action.Action, Action.Check, Action.Token_Count),
            Nonterm, Trace);

         Parser_State.Tree.Set_Root (Nonterm);

      when Error =>
         Current_Parser.Set_Verb (Action.Verb);

         --  We don't raise Syntax_Error here; another parser may be able to
         --  continue.

         declare
            Expecting : constant Token_ID_Set := LR.Expecting
              (Shared_Parser.Table.all, Current_Parser.State_Ref.Stack.Peek.State);
         begin
            Parser_State.Errors.Append
              ((Label          => LR.Action,
                First_Terminal => Trace.Descriptor.First_Terminal,
                Last_Terminal  => Trace.Descriptor.Last_Terminal,
                Error_Token    => Parser_State.Current_Token,
                Expecting      => Expecting,
                Recover        => (others => <>)));

            if Trace_Parse > Outline then
               Put
                 (Trace,
                  Integer'Image (Current_Parser.Label) & ": expecting: " &
                    Image (Expecting, Trace.Descriptor.all));
               Trace.New_Line;
            end if;
         end;
      end case;
   end Do_Action;

   --  Return the type of parser cycle to execute.
   --
   --  Accept : all Parsers.Verb return Accept - done parsing.
   --
   --  Shift : some Parsers.Verb return Shift.
   --
   --  Reduce : some Parsers.Verb return Reduce.
   --
   --  Error : all Parsers.Verb return Error.
   procedure Parse_Verb
     (Shared_Parser : in out Parser;
      Verb          :    out All_Parse_Action_Verbs)
   is
      use all type SAL.Base_Peek_Type;

      Shift_Count  : SAL.Base_Peek_Type := 0;
      Accept_Count : SAL.Base_Peek_Type := 0;
      Error_Count  : SAL.Base_Peek_Type := 0;
   begin
      for Parser_State of Shared_Parser.Parsers loop
         case Parser_State.Verb is
         when Shift =>
            Shift_Count := Shift_Count + 1;

         when Reduce =>
            Verb := Reduce;
            return;

         when Accept_It =>
            Accept_Count := Accept_Count + 1;

         when Error =>
            Error_Count := Error_Count + 1;

         when Pause | Shift_Recover =>
            --  This is parser_no_recover
            raise SAL.Programmer_Error;
         end case;
      end loop;

      if Shared_Parser.Parsers.Count = Accept_Count then
         Verb := Accept_It;

      elsif Shared_Parser.Parsers.Count = Error_Count then
         Verb := Error;

      elsif Shift_Count > 0 then
         Verb := Shift;

      else
         raise SAL.Programmer_Error;
      end if;
   end Parse_Verb;

   ----------
   --  Public subprograms, declaration order

   overriding procedure Finalize (Object : in out Parser)
   is begin
      Free_Table (Object.Table);
   end Finalize;

   procedure New_Parser
     (Parser               :    out          LR.Parser_No_Recover.Parser;
      Trace                : not null access WisiToken.Trace'Class;
      Lexer                : in              WisiToken.Lexer.Handle;
      Table                : in              Parse_Table_Ptr;
      User_Data            : in              WisiToken.Syntax_Trees.User_Data_Access;
      Max_Parallel         : in              SAL.Base_Peek_Type := Default_Max_Parallel;
      First_Parser_Label   : in              Integer            := 1;
      Terminate_Same_State : in              Boolean            := True)
   is
      use all type Syntax_Trees.User_Data_Access;
   begin
      Parser.Lexer                := Lexer;
      Parser.Trace                := Trace;
      Parser.Table                := Table;
      Parser.User_Data            := User_Data;
      Parser.Max_Parallel         := Max_Parallel;
      Parser.First_Parser_Label   := First_Parser_Label;
      Parser.Terminate_Same_State := Terminate_Same_State;

      if User_Data /= null then
         User_Data.Set_Lexer_Terminals (Lexer, Parser.Terminals'Unchecked_Access);
      end if;
   end New_Parser;

   overriding procedure Parse (Shared_Parser : aliased in out Parser)
   is
      use all type Syntax_Trees.User_Data_Access;
      use all type SAL.Base_Peek_Type;

      Trace : WisiToken.Trace'Class renames Shared_Parser.Trace.all;

      Current_Verb   : All_Parse_Action_Verbs;
      Current_Parser : Parser_Lists.Cursor;
      Action         : Parse_Action_Node_Ptr;

      procedure Check_Error (Check_Parser : in out Parser_Lists.Cursor)
      is begin
         if Check_Parser.Verb = Error then
            --  This parser errored on last input. This is how grammar conflicts
            --  are resolved when the input text is valid, so we terminate this
            --  parser.

            if Shared_Parser.Parsers.Count = 1 then
               raise Syntax_Error;
            else
               Shared_Parser.Parsers.Terminate_Parser (Check_Parser, "", Shared_Parser.Trace.all);
            end if;
         else
            Check_Parser.Next;
         end if;
      end Check_Error;

   begin
      --  The user must call Lexer.Reset_* to set the input text.
      Shared_Parser.Lex_All;

      if Shared_Parser.User_Data /= null then
         Shared_Parser.User_Data.Reset;
      end if;
      Shared_Parser.Shared_Tree.Clear;

      Shared_Parser.Parsers := Parser_Lists.New_List
        (Shared_Tree => Shared_Parser.Shared_Tree'Unchecked_Access);

      Shared_Parser.Parsers.First.State_Ref.Stack.Push ((Shared_Parser.Table.State_First, others => <>));

      Main_Loop :
      loop
         --  exit on Accept_It action or syntax error.

         Parse_Verb (Shared_Parser, Current_Verb);

         case Current_Verb is
         when Shift =>
            --  All parsers just shifted a token; get the next token

            for Parser_State of Shared_Parser.Parsers loop
               Parser_State.Shared_Token  := Parser_State.Shared_Token + 1;
               Parser_State.Current_Token := Parser_State.Tree.Add_Terminal
                 (Parser_State.Shared_Token, Shared_Parser.Terminals);
            end loop;

         when Accept_It =>
            --  All parsers accepted.
            declare
               Count : constant SAL.Base_Peek_Type := Shared_Parser.Parsers.Count;
            begin
               if Count = 1 then
                  --  Nothing more to do
                  if Trace_Parse > Outline then
                     Trace.Put_Line (Integer'Image (Shared_Parser.Parsers.First.Label) & ": succeed");
                  end if;
                  exit Main_Loop;

               else
                  --  More than one parser is active; ambiguous parse.
                  declare
                     Token : Base_Token renames Shared_Parser.Terminals (Shared_Parser.Terminals.Last_Index);
                  begin
                     raise WisiToken.Parse_Error with Error_Message
                       (Shared_Parser.Lexer.File_Name, Token.Line, Token.Column,
                        "Ambiguous parse:" & SAL.Base_Peek_Type'Image (Count) & " parsers active.");
                  end;
               end if;
            end;

         when Reduce =>
            null;

         when Error =>
            --  All parsers errored; terminate with error. Semantic_State has all
            --  the required info (recorded by Error in Do_Action), so we just
            --  raise the exception.
            raise Syntax_Error;

         when Pause | Shift_Recover =>
            --  This is parser_no_recover
            raise SAL.Programmer_Error;
         end case;

         --  We don't use 'for Parser_State of Parsers loop' here,
         --  because terminate on error and spawn on conflict require
         --  changing the parser list.
         Current_Parser := Shared_Parser.Parsers.First;
         loop
            exit when Current_Parser.Is_Done;

            if Shared_Parser.Terminate_Same_State and
              Current_Verb = Shift
            then
               Shared_Parser.Parsers.Duplicate_State (Current_Parser, Shared_Parser.Trace.all);
               --  If Duplicate_State terminated Current_Parser, Current_Parser now
               --  points to the next parser. Otherwise it is unchanged.
            end if;

            exit when Current_Parser.Is_Done;

            if Trace_Parse > Extra then
               Trace.Put_Line
                 ("current_verb: " & Parse_Action_Verbs'Image (Current_Verb) &
                    "," & Integer'Image (Current_Parser.Label) &
                    ".verb: " & Parse_Action_Verbs'Image (Current_Parser.Verb));
            end if;

            --  Each branch of the following 'if' calls either Current_Parser.Free
            --  (which advances to the next parser) or Current_Parser.Next.

            if Current_Parser.Verb = Current_Verb then
               if Trace_Parse > Extra then
                  Parser_Lists.Put_Top_10 (Trace, Current_Parser);
               end if;

               declare
                  State : Parser_Lists.Parser_State renames Current_Parser.State_Ref.Element.all;
               begin
                  Action := Action_For
                    (Table => Shared_Parser.Table.all,
                     State => State.Stack.Peek.State,
                     ID    => State.Tree.ID (State.Current_Token));
               end;

               if Action.Next /= null then
                  --  Conflict; spawn a new parser (before modifying Current_Parser
                  --  stack).

                  if Shared_Parser.Parsers.Count = Shared_Parser.Max_Parallel then
                     declare
                        Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
                        Token : Base_Token renames Shared_Parser.Terminals (Parser_State.Shared_Token);
                     begin
                        raise WisiToken.Parse_Error with Error_Message
                          (Shared_Parser.Lexer.File_Name, Token.Line, Token.Column,
                           ": too many parallel parsers required in grammar state" &
                             State_Index'Image (Parser_State.Stack.Peek.State) &
                             "; simplify grammar, or increase max-parallel (" &
                             SAL.Base_Peek_Type'Image (Shared_Parser.Max_Parallel) & ")");
                     end;
                  else
                     if Trace_Parse > Outline then
                        Trace.Put_Line
                          ("spawn parser from " & Trimmed_Image (Current_Parser.Label) &
                             " (" & Trimmed_Image (1 + Integer (Shared_Parser.Parsers.Count)) & " active)");
                     end if;

                     Shared_Parser.Parsers.Prepend_Copy (Current_Parser);
                     Do_Action (Action.Next.Item, Shared_Parser.Parsers.First, Shared_Parser);

                     declare
                        Temp : Parser_Lists.Cursor := Shared_Parser.Parsers.First;
                     begin
                        Check_Error (Temp);
                     end;
                  end if;
               end if;

               Do_Action (Action.Item, Current_Parser, Shared_Parser);
               Check_Error (Current_Parser);

            else
               --  Current parser is waiting for others to catch up
               Current_Parser.Next;
            end if;
         end loop;
      end loop Main_Loop;

      --  We don't raise Syntax_Error for lexer errors, since they are all
      --  recovered, either by inserting a quote, or by ignoring the
      --  character.
   end Parse;

   overriding procedure Execute_Actions (Parser : in out LR.Parser_No_Recover.Parser)
   is
      use all type Syntax_Trees.User_Data_Access;

      procedure Process_Node
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Syntax_Trees.Valid_Node_Index)
      is
         use all type Syntax_Trees.Node_Label;
      begin
         if Tree.Label (Node) /= Nonterm then
            return;
         end if;

         declare
            use all type Syntax_Trees.Semantic_Action;
            Tree_Children : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Children (Node);
         begin
            Parser.User_Data.Reduce (Tree, Node, Tree_Children);

            if Tree.Action (Node) /= null then
               Tree.Action (Node) (Parser.User_Data.all, Tree, Node, Tree_Children);
            end if;
         end;
      end Process_Node;

   begin
      if Parser.User_Data /= null then
         for Parser_State of Parser.Parsers loop
            Parser_State.Tree.Process_Tree (Process_Node'Access);
         end loop;
      end if;
   end Execute_Actions;

   overriding function Any_Errors (Parser : in LR.Parser_No_Recover.Parser) return Boolean
   is
      use all type SAL.Base_Peek_Type;
      use all type Ada.Containers.Count_Type;
      Parser_State : Parser_Lists.Parser_State renames Parser.Parsers.First_Constant_State_Ref;
   begin
      pragma Assert (Parser_State.Tree.Flushed);
      return Parser.Parsers.Count > 1 or Parser_State.Errors.Length > 0 or Parser.Lexer.Errors.Length > 0;
   end Any_Errors;

   overriding procedure Put_Errors (Parser : in LR.Parser_No_Recover.Parser)
   is
      use Ada.Text_IO;

      Parser_State : Parser_Lists.Parser_State renames Parser.Parsers.First_Constant_State_Ref;
      Descriptor   : WisiToken.Descriptor renames Parser.Trace.Descriptor.all;
   begin
      for Item of Parser.Lexer.Errors loop
         Put_Line
           (Current_Error,
            Parser.Lexer.File_Name & ":0:0: lexer unrecognized character at" & Buffer_Pos'Image (Item.Char_Pos));
      end loop;

      for Item of Parser_State.Errors loop
         case Item.Label is
         when Action =>
            declare
               Token : Base_Token renames Parser.Terminals (Parser_State.Tree.Min_Terminal_Index (Item.Error_Token));
            begin
               Put_Line
                 (Current_Error,
                  Error_Message
                    (Parser.Lexer.File_Name, Token.Line, Token.Column,
                     "syntax error: expecting " & Image (Item.Expecting, Descriptor) &
                       ", found '" & Parser.Lexer.Buffer_Text (Token.Byte_Region) & "'"));
            end;

         when Check =>
            null;

         when Message =>
            Put_Line (Current_Error, -Item.Msg);
         end case;

      end loop;
   end Put_Errors;

end WisiToken.Parse.LR.Parser_No_Recover;
