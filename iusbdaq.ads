--------------------------------------------------------------------------------
--  *  Spec name iusbdaq.ads
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
with Interfaces.C;

use  Interfaces;
use  Interfaces.C;

package Iusbdaq is

   Max_Analog_Channel : constant := 8;

   type Analog_Data_Array is array (Positive range <>) of C.C_Float;
   pragma Convention (C, Analog_Data_Array);

   type Void is null record;
   pragma Convention (C, Void);

   type Void_Ptr is access all Void;
   pragma Convention (C, Void_Ptr);

   type Void_Ptr_Ptr is access all Void_Ptr;
   pragma Convention (C, Void_Ptr_Ptr);

   type Handle                is new Void_Ptr_Ptr;
   type Dword                 is new C.Unsigned_Long;
   type Byte                  is new C.Unsigned_Char;
   type Unsigned_Integer      is new C.Unsigned;

   type Dword_Ptr             is access all Dword;
   type Byte_Ptr              is access all Byte;

   type Char_Ptr              is access all C.Char;
   type Float_Ptr             is access all C.C_Float;
   type Int_Ptr               is access all C.Int;
   type Unsigned_Integer_Ptr  is access all C.Unsigned;
   type Unsigned_Long_Ptr     is access all C.Unsigned_Long;

   type No_Of_Analog_Channel  is new C.Int range 1 .. Max_Analog_Channel;
   type Analog_Channel        is new C.Int range 0 .. Max_Analog_Channel - 1;
   type PWM_Channel           is new C.Int range 1 .. 2;

   type Device_Session_Record is
      record
         Device_Index    : C.Int;
         Device_Instance : C.Int;
         Device_Type     : C.Unsigned_Long;
         I_Session_1     : Handle;           -- Out Pipe, write
         I_Session_2     : Handle;           -- In Pipe, read
      end record;

   pragma Convention (C, Device_Session_Record);
   type Device_Session_Ptr is access all Device_Session_Record;

   type Errors is (
      IUSBDAQ_No_Error,
      IUSBDAQ_Unknown,
      IUSBDAQ_Invalid_Device_Index,    -- device index exceed the max device number of that type
      IUSBDAQ_Wrong_Type,              -- No such iUSBDAQ module type or wrong device type
      IUSBDAQ_Handle_Error,            -- Open device session error
      IUSBDAQ_Device_Not_Available,    -- device maybe used by other application or session already been opened.
      IUSBDAQ_Write_Error,             -- Write error, it could be the device disconnected
      IUSBDAQ_Read_Error,              -- Read error, it could be the device disconnected
      IUSBDAQ_Null_Session,            -- NULL session
      IUSBDAQ_Receive_Length,          -- Partially failed, incorrect receive length
      IUSBDAQ_Invalid_Byte_Count,      -- the byte count number is invalid
      IUSBDAQ_Invalid_Channel,         -- channel number is incorrect
      IUSBDAQ_Invalid_No_Of_Channels,  -- number of channels invalid
      IUSBDAQ_Input_Range,             -- Wrong input voltage range
      IUSBDAQ_Invalid_Duty_Cycle,      -- Duty cycle value should be between 0- 1023
      IUSBDAQ_Invalid_DIO_Write,       -- The IO channel is not an output channel
      IUSBDAQ_Invalid_Period,          -- wrong PWM period
      IUSBDAQ_Too_Many_Scans,          -- iUSBDAQ_AIGetScans function asks too many scans to get, max 6400/NrOfChannels
      IUSBDAQ_Read_Samples_Timeout,
      IUSBDAQ_Low_No_Of_Samples,
      IUSBDAQ_High_No_Of_Samples,
      IUSBDAQ_Read_Write_Pass_Allowed_Address,
      IUSBDAQ_PMW_Period_Too_Big,
      IUSBDAQ_PMW_Period_Too_Small,

      -- Mac OS X specific
      IUSBDAQ_Exception_Error,
      IUSBDAQ_IO_Kit_Error
   );
   for Errors use (
      IUSBDAQ_No_Error                          =>     0,
      IUSBDAQ_Unknown                           =>     1,
      IUSBDAQ_Invalid_Device_Index              =>     2,
      IUSBDAQ_Wrong_Type                        =>     3,
      IUSBDAQ_Handle_Error                      =>     4,
      IUSBDAQ_Device_Not_Available              =>     5,
      IUSBDAQ_Write_Error                       =>     6,
      IUSBDAQ_Read_Error                        =>     7,
      IUSBDAQ_Null_Session                      =>     8,
      IUSBDAQ_Receive_Length                    =>     9,
      IUSBDAQ_Invalid_Byte_Count                =>    10,
      IUSBDAQ_Invalid_Channel                   =>    11,
      IUSBDAQ_Invalid_No_Of_Channels            =>    12,
      IUSBDAQ_Input_Range                       =>    13,
      IUSBDAQ_Invalid_Duty_Cycle                =>    14,
      IUSBDAQ_Invalid_DIO_Write                 =>    15,
      IUSBDAQ_Invalid_Period                    =>    16,
      IUSBDAQ_Too_Many_Scans                    =>    17,
      IUSBDAQ_Read_Samples_Timeout              =>    18,
      IUSBDAQ_Low_No_Of_Samples                 =>    19,
      IUSBDAQ_High_No_Of_Samples                =>    20,
      IUSBDAQ_Read_Write_Pass_Allowed_Address   =>    21,
      IUSBDAQ_PMW_Period_Too_Big                =>    22,
      IUSBDAQ_PMW_Period_Too_Small              =>    23,

      -- Mac OS X specific
      IUSBDAQ_Exception_Error                   => 1_000,
      IUSBDAQ_IO_Kit_Error                      => 1_001
   );
   for Errors'size use 32;

   procedure Enumerate_Device (Device_Type      : in     C.Unsigned_Long := 0;
                               Count            : in out C.Unsigned_Long;
                               Error            :    out C.Int);

   procedure Open_Device      (Device_Type      : in     C.Unsigned_Long := 0;
                               Device_Index     : in     C.Unsigned_Long;
                               Session          : in out Device_Session_Record;
                               Error            :    out C.Int);

   procedure Reset                (Session      : in     Device_Session_Record;
                                   Error        :    out C.Int);

   procedure Release_Device       (Session      : in     Device_Session_Record;
                                   Error        :    out C.Int);

   procedure Get_Device_Serial_No (Session      : in     Device_Session_Record;
                                   Serial_No_1  :    out C.Unsigned_Long;
                                   Serial_No_2  :    out C.Unsigned_Long;
                                   Error        :    out C.Int);

   procedure Read_Single_Analog_In (Session     : in     Device_Session_Record;
                                    Channel     : in     Analog_Channel;
                                    Input_Range : in     C.Int := 0;
                                    Voltage     :    out C.C_Float;
                                    Reserved    :    out C.Int;
                                    Error       :    out C.Int);
   
   procedure Read_Multi_Analog_In  (Session     : in     Device_Session_Record;
                                    Start_Ch    : in     Analog_Channel;
                                    No_Of_Ch    : in     No_Of_Analog_Channel;
                                    Input_Range : in     C.Int := 0;
                                    Voltages    :    out Analog_Data_Array;
                                    Reserved    :    out C.Int;
                                    Error       :    out C.Int);

   procedure Read_Counter          (Session     : in     Device_Session_Record;
                                    Reset       : in     Boolean;
                                    Count       :    out C.Unsigned;
                                    Error       :    out C.Int);

   procedure PWM_Output            (Session     : in     Device_Session_Record;
                                    Channel     : in     PWM_Channel;
                                    Duty_Cycle  : in     C.Int;
                                    Period      : in     C.Int;
                                    Error       :    out C.Int);

   procedure Stop_PWM              (Session     : in     Device_Session_Record;
                                    Channel     : in     PWM_Channel;
                                    Error       :    out C.Int);

   function Bits_To_Volt (Bits : C.Int) return C.C_Float;
   pragma Import (C, Bits_To_Volt, "iUSBDAQ_BitsToVolt");

   function Volt_To_Bits (Volt : C.C_Float) return C.Int;
   pragma Import (C, Volt_To_Bits, "iUSBDAQ_VoltToBits");

end Iusbdaq;
