--  Abstract :
--
--  Parser for Wisi grammar files, producing Ada or Elisp source
--  files for a parser.
--
--  Copyright (C) 2012 - 2015, 2017, 2018 Free Software Foundation, Inc.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with WisiToken.BNF.Generate_Utils;
with WisiToken.BNF.Output_Ada;
with WisiToken.BNF.Output_Ada_Common;
with WisiToken.BNF.Output_Ada_Emacs;
with WisiToken.BNF.Output_Elisp;
with WisiToken.BNF.Output_Elisp_Common;
with WisiToken.Generate.Packrat;
with WisiToken.Generate.LR.LALR_Generate;
with WisiToken.Generate.LR.LR1_Generate;
with WisiToken.Parse.LR.Parser_No_Recover; -- for reading BNF file
with WisiToken.Productions;
with WisiToken.Text_IO_Trace;
with WisiToken_Grammar_Runtime;
with Wisitoken_Grammar_Actions;
with Wisitoken_Grammar_Main;
procedure WisiToken.BNF.Generate
is
   use all type Ada.Containers.Count_Type;

   procedure Put_Usage
   is
      use Ada.Text_IO;
      First : Boolean := True;
   begin
      --  verbosity meaning is actually determined by output choice;
      --  they should be consistent with this description.
      Put_Line (Standard_Error, "version 1.0.1");
      Put_Line (Standard_Error, "wisi-generate [options] {wisi grammar file}");
      Put_Line (Standard_Error, "Generate source code implementing a parser for the grammar.");
      New_Line (Standard_Error);
      Put_Line (Standard_Error, "The following grammar file directives control parser generation:");
      Put_Line (Standard_Error,
                "%generate <algorithm> <output language> [<lexer>] [<interface>] [text_rep]");
      Put_Line (Standard_Error, "   specify one of each generate parameter. May be repeated.");
      Put (Standard_Error, "   algorithm: ");
      for I of Generate_Algorithm_Image loop
         if First then
            First := False;
         else
            Put (Standard_Error, " | ");
         end if;
         Put (Standard_Error, I.all);
      end loop;
      New_Line (Standard_Error);

      Put (Standard_Error, "   output language: ");
      First := True;
      for I of Output_Language_Image loop
         if First then
            First := False;
         else
            Put (Standard_Error, " | ");
         end if;
         Put (Standard_Error, I.all);
      end loop;
      New_Line (Standard_Error);

      Put_Line (Standard_Error, "   interface: interface Process | Module");
      Put_Line (Standard_Error, "      only valid with Ada_Emacs:");
      Put_Line (Standard_Error, "      Process is for an external subprocess communicating with Emacs.");
      Put_Line (Standard_Error, "      Module  is for a dynamically loaded Emacs module.");
      Put (Standard_Error, "   lexer: ");
      First := True;
      for I of Output_Language_Image loop
         if First then
            First := False;
         else
            Put (Standard_Error, " | ");
         end if;
         Put (Standard_Error, I.all);
      end loop;
      New_Line (Standard_Error);
      Put_Line
        (Standard_Error, "   text_rep: output LR parse table in a text file, not as source code; for large tables");

      New_Line (Standard_Error);
      Put_Line (Standard_Error, "options:");
      Put_Line (Standard_Error, "  --help: show this help");
      Put_Line (Standard_Error, "  -v level: sets verbosity (default 0):");
      Put_Line (Standard_Error, "     0 - only error messages to standard error");
      Put_Line (Standard_Error, "     1 - add diagnostics to standard out");
      Put_Line (Standard_Error, "     2 - more diagnostics to standard out, ignore unused tokens, unknown conflicts");
      Put_Line (Standard_Error, "  --generate ...: override grammar file %generate directive");
      Put_Line (Standard_Error, "  --suffix <string>; appended to grammar file name");
      Put_Line (Standard_Error,
                "  --test_main; generate standalone main program for running the generated parser, modify file names");
      Put_Line (Standard_Error, "  --time; output execution time of various stages");

   end Put_Usage;

   Language_Name         : Ada.Strings.Unbounded.Unbounded_String; -- The language the grammar defines
   Output_File_Name_Root : Ada.Strings.Unbounded.Unbounded_String;
   Suffix                : Ada.Strings.Unbounded.Unbounded_String;
   Test_Main             : Boolean := False;

   Command_Generate_Set : Generate_Set_Access; -- override grammar file declarations

   Trace          : aliased WisiToken.Text_IO_Trace.Trace (Wisitoken_Grammar_Actions.Descriptor'Access);
   Input_Data     : aliased WisiToken_Grammar_Runtime.User_Data_Type;
   Grammar_Parser : WisiToken.Parse.LR.Parser_No_Recover.Parser;

   Do_Time : Boolean := False;

   procedure Use_Input_File (File_Name : in String)
   is
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
   begin
      Output_File_Name_Root := +Ada.Directories.Base_Name (File_Name) & Suffix;

      Wisitoken_Grammar_Main.Create_Parser
        (Parser    => Grammar_Parser,
         Trace     => Trace'Unchecked_Access,
         User_Data => Input_Data'Unchecked_Access);

      Grammar_Parser.Lexer.Reset_With_File (File_Name);

      declare
         Language_Name_Dir   : constant Integer := Ada.Strings.Fixed.Index
           (File_Name, Ada.Strings.Maps.To_Set ("/\"), Going => Ada.Strings.Backward);
         Language_Name_Ext   : constant Integer := Ada.Strings.Fixed.Index (File_Name, ".wy");
      begin
         Language_Name := +WisiToken.BNF.Output_Elisp_Common.Elisp_Name_To_Ada
           (File_Name
              ((if Language_Name_Dir = 0
                then File_Name'First
                else Language_Name_Dir + 1) ..
                 Language_Name_Ext - 1),
            Append_ID => False,
            Trim      => 0);
      end;
   exception
   when Name_Error | Use_Error =>
      raise Name_Error with "input file '" & File_Name & "' could not be opened.";
   end Use_Input_File;

begin
   declare
      use Ada.Command_Line;
      Arg_Next : Integer := 1;
   begin
      loop
         exit when Argument (Arg_Next)(1) /= '-';

         --   --help, -v first, then alphabetical

         if Argument (Arg_Next) = "--help" then
            Put_Usage;
            return;

         elsif Argument (Arg_Next) = "-v" then
            Arg_Next  := Arg_Next + 1;
            WisiToken.Trace_Generate := Integer'Value (Argument (Arg_Next));
            Arg_Next  := Arg_Next + 1;

         elsif Argument (Arg_Next) = "--generate" then
            Arg_Next  := Arg_Next + 1;
            declare
               Tuple : Generate_Tuple;
               Done  : Boolean := False;
            begin
               begin
                  Tuple.Gen_Alg := Generate_Algorithm'Value (Argument (Arg_Next));
                  Arg_Next     := Arg_Next + 1;
               exception
               when Constraint_Error =>
                  raise User_Error with "invalid value for generator_algorithm: '" & Argument (Arg_Next) & ";";
               end;
               begin
                  Tuple.Out_Lang := To_Output_Language (Argument (Arg_Next));
                  Arg_Next       := Arg_Next + 1;
               end;

               loop
                  exit when Done;
                  declare
                     Text : constant String := Argument (Arg_Next);
                  begin
                     if Text = "text_rep" then
                        Tuple.Text_Rep := True;
                        Arg_Next := Arg_Next + 1;

                     elsif (for some I of Lexer_Image => To_Lower (Text) =  I.all) then
                        Tuple.Lexer := To_Lexer (Text);
                        Arg_Next := Arg_Next + 1;

                     elsif (for some I in Valid_Interface =>
                              To_Lower (Text) = To_Lower (Valid_Interface'Image (I)))
                     then
                        Tuple.Interface_Kind := WisiToken.BNF.Valid_Interface'Value (Text);
                        Arg_Next := Arg_Next + 1;

                     else
                        Done := True;
                     end if;
                  end;
               end loop;

               Add (Command_Generate_Set, Tuple);
            end;

         elsif Argument (Arg_Next) = "--suffix" then
            Arg_Next := Arg_Next + 1;
            Suffix   := +Argument (Arg_Next);
            Arg_Next := Arg_Next + 1;

         elsif Argument (Arg_Next) = "--test_main" then
            Arg_Next  := Arg_Next + 1;
            Test_Main := True;

         elsif Argument (Arg_Next) = "--time" then
            Arg_Next := Arg_Next + 1;
            Do_Time  := True;

         else
            raise User_Error with "invalid argument '" & Argument (Arg_Next) & "'";
         end if;
      end loop;

      Use_Input_File (Argument (Arg_Next));

      if Arg_Next /= Argument_Count then
         raise User_Error with "arg count" & Integer'Image (Argument_Count) &
           " different from expected count" & Integer'Image (Arg_Next);
      end if;
   end;

   begin
      Grammar_Parser.Parse;
   exception
   when WisiToken.Syntax_Error =>
      Grammar_Parser.Put_Errors;
      raise;
   when E : WisiToken.Parse_Error =>
      WisiToken.Generate.Put_Error (Ada.Exceptions.Exception_Message (E));
      raise;
   end;

   declare
      use all type Ada.Strings.Unbounded.Unbounded_String;
      use Ada.Text_IO;

      --  Create a .parse_table file unless verbosity > 0
      Parse_Table_File : File_Type;

      Generate_Set    : Generate_Set_Access;
      Multiple_Tuples : Boolean;

      Lexer_Done : Lexer_Set := (others => False);

      --  In general, all of the data in Generate_Utils.Generate_Data
      --  depends on the generate tuple parameters. However, if
      --  'If_Lexer_Present' is false, then they don't depend on the lexer,
      --  and if 'If_Parser_Present' is false, then they don't depend on the
      --  Gen_Alg, except for the parser table. But it's not worth trying to
      --  cache results in those cases; they only happen in test grammars,
      --  which are small.

      procedure Parse_Check (Lexer : in Lexer_Type; Parser : in Generate_Algorithm)
      is begin
         Input_Data.User_Parser := Parser;
         Input_Data.User_Lexer  := Lexer;
         --  Specifying the parser and lexer can change the parsed grammar, due
         --  to %if {parser | lexer}.

         Input_Data.Reset;
         Grammar_Parser.Execute_Actions;
         --  Ensures Input_Data.User_{Parser|Lexer} are set if needed.

         if Input_Data.Rule_Count = 0 or Input_Data.Tokens.Rules.Length = 0 then
            raise WisiToken.Grammar_Error with "no rules";
         end if;

      end Parse_Check;

   begin
      if Command_Generate_Set = null then
         --  Get the first quad from the input file
         Parse_Check (None, None);

         if Input_Data.Generate_Set = null then
            raise User_Error with
              WisiToken.Generate.Error_Message
                (Input_Data.Grammar_Lexer.File_Name, 1,
                 "generate algorithm, output_language, lexer, interface not specified");
         end if;

         --  Input_Data.Generate_Set will be free'd and regenerated if
         --  Parse_Check is called, but the content won't change. So make a
         --  copy.
         Generate_Set := new WisiToken.BNF.Generate_Set'(Input_Data.Generate_Set.all);
      else
         Generate_Set := Command_Generate_Set;
      end if;

      Multiple_Tuples := Generate_Set'Length > 1;

      for Tuple of Generate_Set.all loop

         Input_Data.User_Parser := Tuple.Gen_Alg;
         Input_Data.User_Lexer  := Tuple.Lexer;

         Parse_Check (Input_Data.User_Lexer, Input_Data.User_Parser);

         declare
            use Ada.Real_Time;

            Time_Start : Time;
            Time_End   : Time;

            Generate_Data : aliased WisiToken.BNF.Generate_Utils.Generate_Data :=
              WisiToken.BNF.Generate_Utils.Initialize (Input_Data);

            Packrat_Data : WisiToken.Generate.Packrat.Data
              (Generate_Data.Descriptor.First_Terminal, Generate_Data.Descriptor.First_Nonterminal,
               Generate_Data.Descriptor.Last_Nonterminal);
         begin
            if not Lexer_Done (Input_Data.User_Lexer) then
               Lexer_Done (Input_Data.User_Lexer) := True;
               if Input_Data.User_Lexer = re2c_Lexer then
                  WisiToken.BNF.Output_Ada_Common.Create_re2c
                    (Input_Data, Tuple, Generate_Data, -Output_File_Name_Root);
               end if;
            end if;

            if WisiToken.Trace_Generate = 0 and Tuple.Gen_Alg /= External then
               Create
                 (Parse_Table_File, Out_File,
                  -Output_File_Name_Root & "_" & To_Lower (Generate_Algorithm'Image (Tuple.Gen_Alg)) &
                    (if Input_Data.If_Lexer_Present
                     then "_" & Lexer_Image (Input_Data.User_Lexer).all
                     else "") &
                    ".parse_table");
               Set_Output (Parse_Table_File);
            end if;

            case Tuple.Gen_Alg is
            when LALR =>

               Time_Start := Clock;

               Generate_Data.LR_Parse_Table := WisiToken.Generate.LR.LALR_Generate.Generate
                 (Generate_Data.Grammar,
                  Generate_Data.Descriptor.all,
                  Generate_Utils.To_Conflicts
                    (Generate_Data, Input_Data.Conflicts, Input_Data.Grammar_Lexer.File_Name),
                  Generate_Utils.To_McKenzie_Param
                    (Generate_Data, Input_Data.McKenzie_Recover, Input_Data.Grammar_Lexer.File_Name),
                  Put_Parse_Table => True);

               if Do_Time then
                  Time_End := Clock;

                  Put_Line
                    (Standard_Error,
                     "LALR " & Lexer_Image (Tuple.Lexer).all & " generate time:" &
                       Duration'Image (To_Duration (Time_End - Time_Start)));
               end if;

               Generate_Data.Parser_State_Count :=
                 Generate_Data.LR_Parse_Table.State_Last - Generate_Data.LR_Parse_Table.State_First + 1;
               WisiToken.BNF.Generate_Utils.Count_Actions (Generate_Data);
               WisiToken.BNF.Generate_Utils.Put_Stats (Input_Data, Generate_Data);

            when LR1 =>
               Time_Start := Clock;

               Generate_Data.LR_Parse_Table := WisiToken.Generate.LR.LR1_Generate.Generate
                 (Generate_Data.Grammar,
                  Generate_Data.Descriptor.all,
                  Generate_Utils.To_Conflicts
                    (Generate_Data, Input_Data.Conflicts, Input_Data.Grammar_Lexer.File_Name),
                  Generate_Utils.To_McKenzie_Param
                    (Generate_Data, Input_Data.McKenzie_Recover, Input_Data.Grammar_Lexer.File_Name),
                  Put_Parse_Table => True);

               if Do_Time then
                  Time_End := Clock;

                  Put_Line
                    (Standard_Error,
                     "LR1 " & Lexer_Image (Tuple.Lexer).all & " generate time:" &
                       Duration'Image (To_Duration (Time_End - Time_Start)));
               end if;

               Generate_Data.Parser_State_Count :=
                 Generate_Data.LR_Parse_Table.State_Last - Generate_Data.LR_Parse_Table.State_First + 1;
               WisiToken.BNF.Generate_Utils.Count_Actions (Generate_Data);
               WisiToken.BNF.Generate_Utils.Put_Stats (Input_Data, Generate_Data);

            when Packrat_Generate_Algorithm =>
               --  The only significant computation done for Packrat is First, done
               --  in Initialize; not worth timing.

               Packrat_Data := WisiToken.Generate.Packrat.Initialize
                 (Input_Data.Grammar_Lexer.File_Name, Generate_Data.Grammar, Generate_Data.Source_Line_Map,
                  Generate_Data.Descriptor.First_Terminal);

               Put_Line ("Tokens:");
               WisiToken.Put_Tokens (Generate_Data.Descriptor.all);
               New_Line;
               Put_Line ("Productions:");
               WisiToken.Productions.Put (Generate_Data.Grammar, Generate_Data.Descriptor.all);

               Packrat_Data.Check_All (Generate_Data.Descriptor.all);

            when External =>
               null;
            end case;

            if WisiToken.Trace_Generate = 0 and Tuple.Gen_Alg /= External then
               Set_Output (Standard_Output);
               Close (Parse_Table_File);
            end if;

            if WisiToken.Generate.Error then
               raise WisiToken.Grammar_Error with "errors: aborting";
            end if;

            case Tuple.Gen_Alg is
            when LR_Generate_Algorithm =>
               if Tuple.Text_Rep then
                  WisiToken.Generate.LR.Put_Text_Rep
                    (Generate_Data.LR_Parse_Table.all,
                     -Output_File_Name_Root & "_" &
                       To_Lower (Generate_Algorithm_Image (Tuple.Gen_Alg).all) &
                       "_parse_table.txt",
                    Generate_Data.Action_Names.all, Generate_Data.Check_Names.all);
               end if;

            when others =>
               null;
            end case;

            case Tuple.Out_Lang is
            when Ada_Lang =>
               WisiToken.BNF.Output_Ada
                 (Input_Data, -Output_File_Name_Root, Generate_Data, Packrat_Data, Tuple, Test_Main, Multiple_Tuples);

            when Ada_Emacs_Lang =>
               WisiToken.BNF.Output_Ada_Emacs
                 (Input_Data, -Output_File_Name_Root, Generate_Data, Packrat_Data, Tuple, Test_Main, Multiple_Tuples,
                  -Language_Name);

            when Elisp_Lang =>
               WisiToken.BNF.Output_Elisp (Input_Data, -Output_File_Name_Root, Generate_Data, Packrat_Data, Tuple);

            end case;
         end;
      end loop;
   end;
exception
when WisiToken.Syntax_Error | WisiToken.Parse_Error =>
   --  error message already output
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

when E : User_Error =>
   declare
      use Ada.Command_Line;
      use Ada.Exceptions;
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, Exception_Message (E));
      Put_Command_Line (Ada_Comment);
      Set_Exit_Status (Failure);
      Put_Usage;
   end;

when E : WisiToken.Grammar_Error =>
   --  error message not already output
   declare
      use Ada.Command_Line;
      use Ada.Exceptions;
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, Exception_Message (E));
      Set_Exit_Status (Failure);
   end;

when E :  others =>
   --  IMPROVEME: for some exceptions, Error message already output via wisi.utils.Put_Error
   declare
      use Ada.Text_IO;
      use Ada.Exceptions;
      use Ada.Command_Line;
   begin
      Put_Line (Standard_Error, Exception_Name (E) & ": " & Exception_Message (E));
      Put_Line (Standard_Error, GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Set_Exit_Status (Failure);
   end;

end WisiToken.BNF.Generate;
