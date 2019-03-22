--  Abstract :
--
--  Base utilities for McKenzie_Recover
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

with Ada.Task_Identification;
with GNAT.Traceback.Symbolic;
package body WisiToken.Parse.LR.McKenzie_Recover.Base is

   function Get_Barrier
     (Parsers                 : not null access Parser_Lists.List;
      Parser_Status           : in              Parser_Status_Array;
      Cost_Limit              : in              Natural;
      Min_Success_Check_Count : in              Natural;
      Check_Delta_Limit       : in              Natural;
      Enqueue_Limit           : in              Natural)
     return Boolean
   is
      use all type SAL.Base_Peek_Type;
      Done_Count : SAL.Base_Peek_Type := 0;
   begin
      --  Return True if all parsers are done, or if any parser has a config
      --  available to check.
      for P_Status of Parser_Status loop
         case P_Status.Recover_State is
         when Active =>
            if P_Status.Parser_State.Recover.Check_Count - Check_Delta_Limit >= Min_Success_Check_Count then
               --  fail; another parser succeeded, this one taking too long.
               Done_Count := Done_Count + 1;

            elsif P_Status.Parser_State.Recover.Enqueue_Count >= Enqueue_Limit then
               --  fail
               Done_Count := Done_Count + 1;

            elsif P_Status.Parser_State.Recover.Config_Heap.Count > 0 then
               if P_Status.Parser_State.Recover.Config_Heap.Min_Key <= Cost_Limit then
                  return True;
               else
                  if P_Status.Active_Workers = 0 then
                     --  fail; remaining configs exceed cost limit
                     Done_Count := Done_Count + 1;
                  end if;
               end if;

            else
               if P_Status.Active_Workers = 0 then
                  --  fail; no configs left to check (rarely happens with real
                  --  languages).
                  Done_Count := Done_Count + 1;
               end if;
            end if;

         when Ready =>
            --  We don't check Enqueue_Limit here; there will only be a few more
            --  to find all the same-cost solutions.

            if P_Status.Parser_State.Recover.Config_Heap.Count > 0 and then
              P_Status.Parser_State.Recover.Config_Heap.Min_Key <= P_Status.Parser_State.Recover.Results.Min_Key
            then
               --  Still more to check.
               return True;

            elsif P_Status.Active_Workers = 0 then
               Done_Count := Done_Count + 1;
            end if;

         when Success | Fail =>
            Done_Count := Done_Count + 1;
         end case;
      end loop;

      return Done_Count = Parsers.Count;
   end Get_Barrier;

   protected body Supervisor is

      procedure Initialize
        (Parsers   : not null access Parser_Lists.List;
         Terminals : not null access constant Base_Token_Arrays.Vector)
      is
         use all type SAL.Base_Peek_Type;
         Index : SAL.Peek_Type := 1;
      begin
         Supervisor.Parsers      := Parsers;
         Supervisor.Terminals    := Terminals;
         All_Parsers_Done        := False;
         Success_Counter         := 0;
         Min_Success_Check_Count := Natural'Last;
         Fatal_Called            := False;
         Result                  := Recover_Status'First;
         Error_ID                := Ada.Exceptions.Null_Id;

         for I in Parsers.Iterate loop
            if Parsers.Reference (I).Recover_Insert_Delete.Length > 0 then
               --  Previous error recovery resume not finished; this is supposed to
               --  be checked in Parser.
               raise SAL.Programmer_Error;
            end if;

            Parser_Status (Index) :=
              (Recover_State  => Active,
               Parser_State   => Parser_Lists.Persistent_State_Ref (I),
               Fail_Mode      => Success,
               Active_Workers => 0);

            declare
               Data : McKenzie_Data renames Parsers.Reference (I).Recover;
            begin
               Data.Config_Heap.Clear;
               Data.Results.Clear;
               Data.Enqueue_Count := 0;
               Data.Check_Count   := 0;
               Data.Success       := False;
            end;

            Index := Index + 1;
         end loop;
      end Initialize;

      entry Get
        (Parser_Index : out SAL.Base_Peek_Type;
         Config       : out Configuration;
         Status       : out Config_Status)
        when (Fatal_Called or All_Parsers_Done) or else
          Get_Barrier (Parsers, Parser_Status, Cost_Limit, Min_Success_Check_Count, Check_Delta_Limit, Enqueue_Limit)
      is
         use all type SAL.Base_Peek_Type;
         Done_Count     : SAL.Base_Peek_Type := 0;
         Min_Cost       : Integer            := Integer'Last;
         Min_Cost_Index : SAL.Base_Peek_Type;

         procedure Set_Outputs (I : in SAL.Peek_Type)
         is begin
            Parser_Index := I;
            Config       := Parser_Status (I).Parser_State.Recover.Config_Heap.Remove;
            Status       := Valid;

            Parser_Status (I).Parser_State.Recover.Check_Count :=
              Parser_Status (I).Parser_State.Recover.Check_Count + 1;

            Parser_Status (I).Active_Workers := Parser_Status (I).Active_Workers + 1;
         end Set_Outputs;

         procedure Set_All_Done
         is begin
            Parser_Index := SAL.Base_Peek_Type'First;
            Config       := (others => <>);
            Status       := All_Done;
         end Set_All_Done;

      begin
         if Fatal_Called or All_Parsers_Done then
            Set_All_Done;
            return;
         end if;

         --  Same logic as in Get_Barrier, but different actions.
         for I in Parser_Status'Range loop
            declare
               P_Status : Base.Parser_Status renames Parser_Status (I);
            begin
               case P_Status.Recover_State is
               when Active =>
                  if P_Status.Parser_State.Recover.Config_Heap.Count > 0 then
                     if P_Status.Parser_State.Recover.Check_Count - Check_Delta_Limit >= Min_Success_Check_Count then
                        if Trace_McKenzie > Outline then
                           Put_Line (Trace.all, P_Status.Parser_State.Label, "fail; check delta (limit" &
                                       Integer'Image (Min_Success_Check_Count + Check_Delta_Limit) & ")");
                        end if;
                        P_Status.Recover_State := Fail;
                        P_Status.Fail_Mode     := Fail_Check_Delta;

                        Done_Count := Done_Count + 1;

                     elsif P_Status.Parser_State.Recover.Enqueue_Count >= Enqueue_Limit then
                        if Trace_McKenzie > Outline then
                           Put_Line (Trace.all, P_Status.Parser_State.Label, "fail; enqueue limit (" &
                                       Integer'Image (Enqueue_Limit) & ")");
                        end if;
                        P_Status.Recover_State := Fail;
                        P_Status.Fail_Mode     := Fail_Enqueue_Limit;

                        Done_Count := Done_Count + 1;

                     elsif P_Status.Parser_State.Recover.Config_Heap.Min_Key <= Cost_Limit then
                        if P_Status.Parser_State.Recover.Config_Heap.Min_Key < Min_Cost then
                           Min_Cost       := P_Status.Parser_State.Recover.Config_Heap.Min_Key;
                           Min_Cost_Index := I;
                        end if;

                     else
                        if P_Status.Active_Workers = 0 then
                           if Trace_McKenzie > Outline then
                              Put_Line (Trace.all, P_Status.Parser_State.Label, "fail; cost");
                           end if;
                           P_Status.Recover_State := Fail;
                           P_Status.Fail_Mode     := Fail_Cost;

                           Done_Count := Done_Count + 1;
                        end if;
                     end if;
                  else
                     if P_Status.Active_Workers = 0 then
                        --  No configs left to check (rarely happens with real languages).
                        if Trace_McKenzie > Outline then
                           Put_Line (Trace.all, P_Status.Parser_State.Label, "fail; no configs left");
                        end if;
                        P_Status.Recover_State := Fail;
                        P_Status.Fail_Mode     := Fail_No_Configs_Left;

                        Done_Count := Done_Count + 1;
                     end if;
                  end if;

               when Ready =>
                  if P_Status.Parser_State.Recover.Enqueue_Count >= Enqueue_Limit then
                     if Trace_McKenzie > Outline then
                        Put_Line (Trace.all, P_Status.Parser_State.Label, "fail; enqueue limit (" &
                                    Integer'Image (Enqueue_Limit) & ")");
                     end if;
                     P_Status.Recover_State := Fail;
                     P_Status.Fail_Mode     := Fail_Enqueue_Limit;

                     Done_Count := Done_Count + 1;

                  elsif P_Status.Parser_State.Recover.Config_Heap.Count > 0 and then
                    P_Status.Parser_State.Recover.Config_Heap.Min_Key <= P_Status.Parser_State.Recover.Results.Min_Key
                  then
                     --  Still more to check.
                     Set_Outputs (I);
                     return;

                  elsif P_Status.Active_Workers = 0 then
                     P_Status.Recover_State := Success;
                     Done_Count             := Done_Count + 1;
                  end if;

               when Success | Fail =>
                  Done_Count := Done_Count + 1;
               end case;
            end;
         end loop;

         if Min_Cost /= Integer'Last then
            Set_Outputs (Min_Cost_Index);

         elsif Done_Count = Parsers.Count then
            if Trace_McKenzie > Extra then
               Trace.Put_Line ("Supervisor: done, " & (if Success_Counter > 0 then "succeed" else "fail"));
            end if;

            Set_All_Done;
            All_Parsers_Done := True;
         else
            raise SAL.Programmer_Error with "Get_Barrier and Get logic do not match";
         end if;
      end Get;

      procedure Success
        (Parser_Index : in     SAL.Peek_Type;
         Config       : in     Configuration;
         Configs      : in out Config_Heaps.Heap_Type)
      is
         use all type SAL.Base_Peek_Type;
         Data : McKenzie_Data renames Parser_Status (Parser_Index).Parser_State.Recover;
      begin
         Put (Parser_Index, Configs); --  Decrements Active_Worker_Count.

         if Trace_McKenzie > Detail then
            Put
              ("succeed: enqueue" & Integer'Image (Data.Enqueue_Count) & ", check " & Integer'Image (Data.Check_Count),
               Trace.all, Parser_Status (Parser_Index).Parser_State.Label, Terminals.all, Config);
         end if;

         if Force_Full_Explore then
            return;
         end if;

         Success_Counter := Success_Counter + 1;
         Result          := Success;

         Data.Success := True;

         if Data.Check_Count < Min_Success_Check_Count then
            Min_Success_Check_Count := Data.Check_Count;
         end if;

         if Force_High_Cost_Solutions then
            Data.Results.Add (Config);
            if Data.Results.Count > 3 then
               Parser_Status (Parser_Index).Recover_State := Ready;
            end if;
         else
            if Data.Results.Count = 0 then
               Data.Results.Add (Config);

               Parser_Status (Parser_Index).Recover_State := Ready;

            elsif Config.Cost < Data.Results.Min_Key then
               --  delete higher cost configs from Results
               loop
                  Data.Results.Drop;
                  exit when Data.Results.Count = 0 or else
                    Config.Cost >= Data.Results.Min_Key;
               end loop;

               Data.Results.Add (Config);

            elsif Config.Cost = Data.Results.Min_Key then
               Data.Results.Add (Config);

            else
               --  Config.Cost > Results.Min_Key
               null;
            end if;
         end if;
      end Success;

      procedure Put (Parser_Index : in SAL.Peek_Type; Configs : in out Config_Heaps.Heap_Type)
      is
         use all type SAL.Base_Peek_Type;
         Configs_Count : constant SAL.Base_Peek_Type := Configs.Count; -- Before it is emptied, for Trace.

         P_Status : Base.Parser_Status renames Parser_Status (Parser_Index);
         Data : McKenzie_Data renames P_Status.Parser_State.Recover;
      begin
         P_Status.Active_Workers := P_Status.Active_Workers - 1;

         loop
            exit when Configs.Count = 0;

            --  [1] has a check for duplicate configs here; that only happens with
            --  higher costs, which take too long for our application.
            Data.Config_Heap.Add (Configs.Remove);
            Data.Enqueue_Count := Data.Enqueue_Count + 1;
         end loop;

         if Trace_McKenzie > Detail then
            Put_Line
              (Trace.all, P_Status.Parser_State.Label,
               "enqueue:" & SAL.Base_Peek_Type'Image (Configs_Count) &
                 "/" & SAL.Base_Peek_Type'Image (Data.Config_Heap.Count) &
                 "/" & Trimmed_Image (Data.Enqueue_Count) &
                 "/" & Trimmed_Image (Data.Check_Count) &
                 ", min cost:" &
                 (if Data.Config_Heap.Count > 0
                  then Integer'Image (Data.Config_Heap.Min_Key)
                  else " ? ") &
                 ", active workers:" & Integer'Image (P_Status.Active_Workers));
         end if;
      end Put;

      function Recover_Result return Recover_Status
      is
         Temp : Recover_Status := Result;
      begin
         if Result = Success then
            return Success;
         else
            for S of Parser_Status loop
               Temp := Recover_Status'Max (Result, S.Fail_Mode);
            end loop;
            return Temp;
         end if;
      end Recover_Result;

      procedure Fatal (E : in Ada.Exceptions.Exception_Occurrence)
      is
         use Ada.Exceptions;
         Task_ID : constant String := Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task);
      begin
         if Trace_McKenzie > Outline then
            Trace.Put_Line (Task_ID & " Supervisor: Error");
         end if;
         Fatal_Called   := True;
         Error_ID       := Exception_Identity (E);
         Error_Message  := +Exception_Message (E);
         if Debug_Mode then
            Trace.Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
            Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
         end if;
      end Fatal;

      entry Done (Error_ID : out Ada.Exceptions.Exception_Id; Message : out Ada.Strings.Unbounded.Unbounded_String)
        when All_Parsers_Done or Fatal_Called
      is begin
         Error_ID := Supervisor.Error_ID;
         Message  := Error_Message;
         if Trace_McKenzie > Detail then
            Trace.New_Line;
            Trace.Put_Line ("Supervisor: Done");
         end if;
      end Done;

      function Parser_State (Parser_Index : in SAL.Peek_Type) return Parser_Lists.Constant_Reference_Type
      is begin
         return (Element => Parser_Status (Parser_Index).Parser_State);
      end Parser_State;

      function Label (Parser_Index : in SAL.Peek_Type) return Natural
      is begin
         return Parser_Status (Parser_Index).Parser_State.Label;
      end Label;

   end Supervisor;

   procedure Put
     (Message      : in              String;
      Super        : not null access Base.Supervisor;
      Shared       : not null access Base.Shared;
      Parser_Index : in              SAL.Peek_Type;
      Config       : in              Configuration;
      Task_ID      : in              Boolean := True)
   is begin
      Put (Message, Super.Trace.all, Super.Parser_State (Parser_Index).Label,
           Shared.Terminals.all, Config, Task_ID);
   end Put;

end WisiToken.Parse.LR.McKenzie_Recover.Base;
