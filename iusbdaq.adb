--------------------------------------------------------------------------------
--  *  Prog name iusbdaq.adb
--  *  Project name iusbdaq
--  *
--  *  Version 1.0
--  *  Last update 8/22/08
--  *
--  *  Created by Adrian Hoe on 6/25/08.
--  *  Copyright (c) 2008 AdaStar Informatics ( http://adastarinformatics.com )
--  *  All rights reserved.
--  *
--------------------------------------------------------------------------------

package body Iusbdaq is

   -----------------------------------------------------------------------------
   procedure Enumerate_Device (Device_Type : in     C.Unsigned_Long := 0;
                               Count       : in out C.Unsigned_Long;
                               Error       :    out C.Int)
   is
      function IUSBDAQ_Enumerate_Device (DevType : C.Unsigned_Long;
                                         Count   : Unsigned_Long_Ptr)
         return C.Int;
      pragma Import (C, IUSBDAQ_Enumerate_Device, "iUSBDAQ_EnumerateDev");
   begin
      Error := IUSBDAQ_Enumerate_Device (Device_Type,
                                         Count'Unrestricted_Access);
   end Enumerate_Device;

   -----------------------------------------------------------------------------
   procedure Open_Device (Device_Type  : in     C.Unsigned_Long := 0;
                          Device_Index : in     C.Unsigned_Long;
                          Session      : in out Device_Session_Record;
                          Error        :    out C.Int)
   is
      function IUSBDAQ_Open_Device (DevType  : C.Unsigned_Long;
                                    DevIndex : C.Unsigned_Long;
                                    Session  : Device_Session_Ptr)
         return C.Int;
      pragma Import (C, IUSBDAQ_Open_Device, "iUSBDAQ_OpenDevice");
   begin
      Error   := IUSBDAQ_Open_Device (Device_Type,
                                      Device_Index,
                                      Session'Unrestricted_Access);
   end Open_Device;

   -----------------------------------------------------------------------------
   procedure Reset (Session : in     Device_Session_Record;
                    Error   :    out C.Int)
   is
      function IUSBDAQ_Reset (Session : Device_Session_Ptr) return C.Int;
      pragma Import (C, IUSBDAQ_Reset, "iUSBDAQ_Reset");
   begin
      Error := IUSBDAQ_Reset (Session'Unrestricted_Access);
   end Reset;

   -----------------------------------------------------------------------------
   procedure Release_Device (Session : in     Device_Session_Record;
                             Error   :    out C.Int)
   is
      function IUSBDAQ_Release_Device (Session : Device_Session_Ptr)
         return C.Int;
      pragma Import (C, IUSBDAQ_Release_Device, "iUSBDAQ_ReleaseDevice");
   begin
      Error := IUSBDAQ_Release_Device (Session'Unrestricted_Access);
   end Release_Device;

   -----------------------------------------------------------------------------
   procedure Get_Device_Serial_No (Session     : in     Device_Session_Record;
                                   Serial_No_1 :    out C.Unsigned_Long;
                                   Serial_No_2 :    out C.Unsigned_Long;
                                   Error       :    out C.Int)
   is
      function IUSBDAQ_Get_Device_Serial_No (Session       : Device_Session_Ptr;
                                             SerialNumber1 : Unsigned_Long_Ptr;
                                             SerialNumber2 : Unsigned_Long_Ptr)
         return C.Int;
      pragma Import (C,
                     IUSBDAQ_Get_Device_Serial_No,
                     "iUSBDAQ_GetDeviceSerialNo");
   begin
      Error := IUSBDAQ_Get_Device_Serial_No (Session'Unrestricted_Access,
                                             Serial_No_1'Unrestricted_Access,
                                             Serial_No_2'Unrestricted_Access);

   end Get_Device_Serial_No;

   -----------------------------------------------------------------------------
   procedure Read_Single_Analog_In (Session     : in     Device_Session_Record;
                                    Channel     : in     Analog_Channel;
                                    Input_Range : in     C.Int := 0;
                                    Voltage     :    out C.C_Float;
                                    Reserved    :    out C.Int;
                                    Error       :    out C.Int)
   is
      function IUSBDAQ_Read_Single_Analog_In (Session    : Device_Session_Ptr;
                                              Channel    : C.Int;
                                              InputRange : C.Int;
                                              Voltage    : Float_Ptr;
                                              Reserved   : Int_Ptr)
         return C.Int;
      pragma Import (C,
                     IUSBDAQ_Read_Single_Analog_In,
                     "iUSBDAQ_ReadSingleAnalogIn");

      Read_Voltage  : C.C_Float;
      Read_Reserved : C.Int;
   begin
      Error := IUSBDAQ_Read_Single_Analog_In (Session'Unrestricted_Access,
                                              C.Int (Channel),
                                              Input_Range,
                                              Read_Voltage'Unrestricted_Access,
                                              Read_Reserved'Unrestricted_Access);

      Voltage  := Read_Voltage;
      Reserved := Read_Reserved;
   end Read_Single_Analog_In;

   -----------------------------------------------------------------------------
   procedure Read_Multi_Analog_In (Session     : in     Device_Session_Record;
                                   Start_Ch    : in     Analog_Channel;
                                   No_Of_Ch    : in     No_Of_Analog_Channel;
                                   Input_Range : in     C.Int := 0;
                                   Voltages    :    out Analog_Data_Array;
                                   Reserved    :    out C.Int;
                                   Error       :    out C.Int)
   is
      function IUSBDAQ_Read_Multi_Analog_In (Session    : Device_Session_Ptr;
                                             Start_Ch   : C.Int;
                                             No_Of_Ch   : C.Int;
                                             InputRange : C.Int;
                                             Voltages   : Analog_Data_Array;
                                             Reserved   : Int_Ptr)
         return C.Int;
      pragma Import (C,
                     IUSBDAQ_Read_Multi_Analog_In,
                     "iUSBDAQ_ReadMultiAnalogIn");
                     
      NOC           : constant Integer := Integer (No_Of_Ch);
      
      Read_Voltages : Analog_Data_Array (1 .. NOC) := (others => 0.0);
      Read_Reserved : C.Int;
   begin
      Error := IUSBDAQ_Read_Multi_Analog_In (Session'Unrestricted_Access,
                                             C.Int (Start_Ch),
                                             C.Int (No_Of_Ch),
                                             Input_Range,
                                             Read_Voltages,
                                             Read_Reserved'Unrestricted_Access);

      Reserved := Read_Reserved;
      for I in 1 .. NOC loop
         Voltages (I) := Read_Voltages (I);
      end loop;
   end Read_Multi_Analog_In;

   -----------------------------------------------------------------------------
   procedure Read_Counter (Session : in     Device_Session_Record;
                           Reset   : in     Boolean;
                           Count   :    out C.Unsigned;
                           Error   :    out C.Int)
   is
      function IUSBDAQ_Read_Counter (Session : Device_Session_Ptr;
                                     Reset   : C.Int;
                                     Count   : Unsigned_Integer_Ptr)
         return C.Int;
      pragma Import (C, IUSBDAQ_Read_Counter, "iUSBDAQ_ReadCounter");

      Reset_Switch : C.Int := 0;
      Read_Count   : C.Unsigned;
   begin
      if Reset then
         Reset_Switch := 1;
      end if;
      Error := IUSBDAQ_Read_Counter (Session'Unrestricted_Access,
                                     Reset_Switch,
                                     Read_Count'Unrestricted_Access);
      Count := Read_Count;
   end Read_Counter;

   -----------------------------------------------------------------------------
   procedure PWM_Output            (Session     : in     Device_Session_Record;
                                    Channel     : in     PWM_Channel;
                                    Duty_Cycle  : in     C.Int;
                                    Period      : in     C.Int;
                                    Error       :    out C.Int)
   is
      function IUSBDAQ_PWM_Output  (Session     : Device_Session_Ptr;
                                    Channel     : Byte;
                                    Duty_Cycle  : C.Int;
                                    Period      : C.Int)
         return C.Int;
      pragma Import (C, IUSBDAQ_PWM_Output, "iUSBDAQ_PWMOut");

      function IUSBDAQ_10_Bit_PWM_Output (Session    : Device_Session_Ptr;
                                          Channel    : Byte;
                                          Duty_Cycle : C.Int;
                                          Period     : C.Int)
         return C.Int;
      pragma Import (C, IUSBDAQ_10_Bit_PWM_Output, "iUSBDAQ_10bitPWMOut");

      Actual_Cycle : C.Int;
      
   begin
      if Duty_Cycle in 0 .. 100 then
         Error := IUSBDAQ_PWM_Output (Session'Unrestricted_Access,
                                      Byte (Channel),
                                      Duty_Cycle,
                                      Period);
      elsif Duty_Cycle in 101 .. 1124 then
         Actual_Cycle := Duty_Cycle - 101;
         Error := IUSBDAQ_10_Bit_PWM_Output (Session'Unrestricted_Access,
                                             Byte (Channel),
                                             Actual_Cycle,
                                             Period);
      end if;
   end PWM_Output;

   -----------------------------------------------------------------------------
   procedure Stop_PWM              (Session     : in     Device_Session_Record;
                                    Channel     : in     PWM_Channel;
                                    Error       :    out C.Int)
   is
      function IUSBDAQ_Stop_PWM    (Session     : Device_Session_Ptr;
                                    Channel     : C.Int)
         return C.Int;
      pragma Import (C, IUSBDAQ_Stop_PWM, "iUSBDAQ_STOPPWM");
   begin
      Error := IUSBDAQ_Stop_PWM (Session'Unrestricted_Access, C.Int (Channel));
   end Stop_PWM;
												 
end Iusbdaq;