--------------------------------------------------------------------------------
--  *  Prog name datalogging.adb
--  *  Project name datalogging
--  *
--  *  Version 1.0
--  *  Last update 12/2/08
--  *
--  *  Created by Kheng-Yin Beh on 12/2/08.
--  *  Copyright (c) 2008 __MyCompanyName__.
--  *  All rights reserved.
--  *    or (keep only one line or write your own)
--  *  GNAT modified GNU General Public License
--  *
--------------------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Calendar;
with Interfaces.C;
with Iusbdaq;

use Ada.Text_IO, Ada.Calendar;
use Interfaces, Interfaces.C;

procedure Datalogging is

package DAQ renames Iusbdaq;
package F_IO is new Ada.Text_IO.Float_IO ( Float );
package I_IO is new Ada.Text_IO.Integer_IO ( Integer );

Device_Session			: DAQ.Device_Session_Record;
Sensor_Analog_Data	: DAQ.Analog_Data_Array ( 1 .. 8 );
Error, Reserved		: C.Int;
Count						: C.Unsigned_Long := 0;
N							: Integer;
Start_Time, End_Time	: Time;
Year, Month, Day,
Hour, Minute, Second	: Integer;
Total_Time				: Duration;
Split_Second			: Duration;
Target_A1, Target_M1,
Target_A2, Target_M2,
Target_A3, Target_M3,
Target_A4, Target_M4 : File_Type;
File_Name				: String ( 1 .. 2 );
Target_A1_Prefix		: constant String := "-a1";
Target_A2_Prefix		: constant String := "-a2";
Target_A3_Prefix		: constant String := "-a3";
Target_A4_Prefix		: constant String := "-a4";
Target_M1_Prefix		: constant String := "-m1";
Target_M2_Prefix		: constant String := "-m2";
Target_M3_Prefix		: constant String := "-m3";
Target_M4_Prefix		: constant String := "-m4";
Extension				: constant String := ".dat";


	begin
		DAQ.Enumerate_Device ( Count => Count, Error => Error );
		if Count > 0 then
			DAQ.Open_Device ( Device_Index	=> 0,
									Session			=> Device_Session,
									Error				=> Error );
			if Error = 0 then
				N := 1;
				delay 5.0;
				Start_Time := Clock;
				Split ( Date	 => Start_Time,
						  Year	 => Year,
						  Month	 => Month,
						  Day		 => Day,
						  Seconds => Split_Second );
				Second := Integer ( Split_Second ) mod 60;
				Minute := Integer ( Split_Second ) mod ( 60 * 60 );
				Hour   := Integer ( Split_Second ) / ( 60 * 60 );
				File_Name := Integer'Image ( Year ) & Integer'Image ( Month ) & Integer'Image ( Day )
								 & Integer'Image ( Hour ) & Integer'Image ( Minute ) & Integer'Image ( Second );
				Create ( File => Target_A1,
							Name => File_Name & Target_A1_Prefix & Extension );				
				Create ( File => Target_A2,
							Name => File_Name & Target_A2_Prefix & Extension );
				Create ( File => Target_A3,
							Name => File_Name & Target_A3_Prefix & Extension );
				Create ( File => Target_A4,
							Name => File_Name & Target_A4_Prefix & Extension );
				Create ( File => Target_M1,
							Name => File_Name & Target_M1_Prefix & Extension );
				Create ( File => Target_M2,
							Name => File_Name & Target_M2_Prefix & Extension );
				Create ( File => Target_M3,
							Name => File_Name & Target_M3_Prefix & Extension );
				Create ( File => Target_M4,
							Name => File_Name & Target_M4_Prefix & Extension );
				loop
					Sensor_Analog_Data := ( others => 0.0 );
					DAQ.Read_Multi_Analog_In ( Session	=> Device_Session,
														Start_Ch => 0,
														No_Of_Ch	=> 8,
														Voltages => Sensor_Analog_Data,
														Reserved	=> Reserved,
														Error		=> Error );
					exit when N > 5000 or Error > 0;
					Put ( File => Target_A1, Item => Float ( Sensor_Analog_Data ( 1 )), Aft => 8, Exp => 0 );
					New_Line ( File => Target_A1 );
					Put ( File => Target_M1, Item => Float ( Sensor_Analog_Data ( 2 )), Aft => 8, Exp => 0 );
					New_Line ( File => Target_M1 );
					Put ( File => Target_A2, Item => Float ( Sensor_Analog_Data ( 3 )), Aft => 8, Exp => 0 );
					New_Line ( File => Target_A2 );
					Put ( File => Target_M2, Item => Float ( Sensor_Analog_Data ( 4 )), Aft => 8, Exp => 0 );
					New_Line ( File => Target_M2 );
					Put ( File => Target_A3, Item => Float ( Sensor_Analog_Data ( 5 )), Aft => 8, Exp => 0 );
					New_Line ( File => Target_A3 );
					Put ( File => Target_M3, Item => Float ( Sensor_Analog_Data ( 6 )), Aft => 8, Exp => 0 );
					New_Line ( File => Target_M3 );
					Put ( File => Target_A4, Item => Float ( Sensor_Analog_Data ( 7 )), Aft => 8, Exp => 0 );
					New_Line ( File => Target_A4 );
					Put ( File => Target_M4, Item => Float ( Sensor_Analog_Data ( 8 )), Aft => 8, Exp => 0 );
					New_Line ( File => Target_M4 );
					
					F_IO.Put ( Float ( Sensor_Analog_Data ( 1 )), Aft => 8, Exp => 0 );
					Put (", ");
					F_IO.Put ( Float ( Sensor_Analog_Data ( 2 )), Aft => 8, Exp => 0 );
					Put (", ");
					F_IO.Put ( Float ( Sensor_Analog_Data ( 3 )), Aft => 8, Exp => 0 );
					Put (", ");
					F_IO.Put ( Float ( Sensor_Analog_Data ( 4 )), Aft => 8, Exp => 0 );
					Put (", ");
					F_IO.Put ( Float ( Sensor_Analog_Data ( 5 )), Aft => 8, Exp => 0 );
					Put (", ");
					F_IO.Put ( Float ( Sensor_Analog_Data ( 6 )), Aft => 8, Exp => 0 );
					Put (", ");
					F_IO.Put ( Float ( Sensor_Analog_Data ( 7 )), Aft => 8, Exp => 0 );
					Put (", ");
					F_IO.Put ( Float ( Sensor_Analog_Data ( 8 )), Aft => 8, Exp => 0 );
					New_Line;
					N := N + 1;
				end loop;
				Close ( Target_A1 );
				Close ( Target_M1 );
				Close ( Target_A2 );
				Close ( Target_M2 );
				Close ( Target_A3 );
				Close ( Target_M3 );
				Close ( Target_A4 );
				Close ( Target_M4 );
			else
				Put ( Integer'Image ( Integer ( Error )) & ": Error opening device." );
			end if;
		end if;
   end Datalogging;
