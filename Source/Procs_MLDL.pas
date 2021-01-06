unit Procs_MLDL;

//---------------------------------------------------------------------------//
//    Copyright (c) 2008  Meindert Kuipers, Netherlands                      //
//    meindert@kuiprs.nl                  www.kuiprs.nl                      //
//                                                                           //
// This program is free software; you can redistribute it and/or             //
// modify it under the terms of the GNU General Public License               //
// as published by the Free Software Foundation; either version 2            //
// of the License, or (at your option) any later version.                    //
//                                                                           //
// This program is distributed in the hope that it will be useful,           //
// but WITHOUT ANY WARRANTY; without even the implied warranty of            //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             //
// GNU General Public License for more details.                              //
//                                                                           //
// You should have received a copy of the GNU General Public License         //
// along with this program; if not, write to the Free Software               //
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA//
//---------------------------------------------------------------------------//
//
//---------------------------------------------------------------------------//
//  PROCS_MLDL.PAS                                                           //
//  High-level MLDL2000 communication and basic functions                    //
//  Ver  Date     Description                                                //
//  1.00 Nov 2005 First version for release                                  //
//  1.01 Nov 2006 Compiled for Turbo Explorer                                //
//                Added FLASH READ ID procedure                              //
//                CPLD is powered down when leaving M2kM                     //
//  1.02 Apr 2007 Fixed error in MLDL_Readblock                              //
//  1.20 Oct 2007 Change RPinsLo and RPinsHi, had bug in timeout code        //
//  1.30 Mar 2008 Added byte typecasts in JTAG to prevent Range Error on SHL //
//  1.50 May 2008 Final release                                              //
//  1.51 Jul 2008 Added ReadBytes after SRAM check to prevent the JTAG code  //
//                to trigger SRAM overwrite                                  //
//                Added procedure ReadSRAMTyp for the above issue            //
//                Added functions for IO read/write                          //
//  1.70 Jul 2010 Adapted for Lazarus                                        //
//  1.80 Feb 2018 restarted development                                      //
//                removed Delphi compatibility                               //
//---------------------------------------------------------------------------//

interface

uses Globals, GlobalConstants, Jtag_constants, D2XXUnit, Forms, Dialogs,
     Types, SysUtils, DateUtils, Controls;

procedure MLDL_Error_Report(ErrStr: string);
procedure AddToBuffer(I : integer);
function  OpenMLDLPort: boolean;
function  PortIsOpen: boolean;
procedure CloseMLDLPort;
procedure BitBang_On;
procedure BitBang_Off;
procedure MPSSE_On;
procedure MPSSE_Off;
procedure SendBytes(NumberOfBytes : integer);
function  RPinsLo: byte;
procedure WPinsLo(Value: Byte);
function  RPinsHi: byte;
procedure WPinsHi(Value: Byte);
function  Get_Data (NumBytes: integer) : boolean;
function  ReceiveBuffer(RxCount: integer): integer;
procedure FlushInBuffer();
procedure PurgeChip;
procedure InitMLDLComm;
procedure ReadSRAMTyp;
procedure InitJTAGMode(USB_mode: boolean);
procedure InitDefault;
procedure DeInitMLDLComm();
procedure SetPinLo(Pin: Byte);
procedure ClearPinLo(Pin: Byte);
procedure TogglePinLo(Pin: Byte);
procedure SetPinHi(Pin: Byte);
procedure ClearPinHi(Pin: Byte);
procedure TogglePinHi(Pin: Byte);
procedure DisableMLDL;
procedure EnableMLDL;
procedure CPLD_On;
procedure CPLD_Off;
procedure SRAMWriteByte(Address: LongWord; Data: Word);
procedure SRAMWriteBlock(Address: LongWord; Index: integer);
procedure MLDL_FLASHReadID(var DevType: Word);
procedure FLASHWriteByte(Address: LongWord; Data: Word);
procedure FLASHWriteByteFast(Address: LongWord; Data: Word);
procedure FLASHBypass;
procedure FLASHBypassReset;
procedure MLDL_FLASHReset;
procedure MLDL_FLASHEraseSector(Address: LongWord);
procedure MLDL_FLASHEraseAll();
procedure MLDL_ReadByte(MemTp: MemType; Address: LongWord; var Data: Word);
procedure MLDL_WriteByte(MemTp: MemType; Address: LongWord; Data: Word);
procedure MLDL_ReadBlock(MemTp: MemType; Address: LongWord; NumBytes: Word);
procedure MLDL_WriteBlock(MemTp: MemType; Address: LongWord; NumBytes: Word);
procedure JTAG_go_state(new_state: jtag_state);
procedure JTAG_change_state(new_state: jtag_state);
function  JTAG_wiggler_state(new_state: jtag_state; last_data: byte;
                             Do_Read: boolean): integer;
procedure JTAG_sout(jtag_register: shift_register; bit_length: word;
                    var out_data: data_type; state: jtag_state);
procedure JTAG_sio(jtag_register: shift_register;
                   bit_length: word; var out_data, in_data: data_type;
                   state: jtag_state; SndImmediate: boolean );
procedure JTAG_sin(jtag_register: shift_register; bit_length: word;
                   var in_data: data_type; state: jtag_state;
                   TDILevel: byte; SndImmediate: boolean);
procedure MLDL_ReadIO(var Data: word);
procedure MLDL_WritIO(Data: byte);
function MLDL_XDAV: boolean;
function MLDL_XBSY: boolean;


const
  USBBuffSize : integer = $800;

var
  MLDL_Result: Word;          // to hold result of function
  MLDL_Open : Boolean;        // MLDL USB connection is open or not
  MLDL_Description, MLDL_Serial: String;
  Err_MLDL: Word;

  OutIndex : integer;         // Index in FT_Out_Buffer

  InData : array[0..2047] of Byte;
  InRead : Integer;

  BlockData : array[0..2000] of Byte;
  OffSet : integer;

  ImageData : array[0..63] of word;

  DataArray : array[0..3000] of byte;

  WordArray : array[0..RomSize] of word;

  ARR_I, ARR_J, ARR_READ: integer;


implementation


procedure DisplayMsg(MsgStr : string);
begin
  Application.ProcessMessages;
end;


procedure MLDL_Error_Report(ErrStr: string);
var
  Str: string;
begin
  if not MLDL_Enable_Error_Report then Exit;
  if MLDL_Error = Err_MLDL_OK then Exit;
  case MLDL_Error of
    Err_MLDL_Timeout   : Str := ErrStr+' - Time Out...';
    else Str := ErrStr+' - other error  ...';
  end;
  MLDL_Error := Err_MLDL_OK;
  MessageDlg(Str, mtError, [mbOk], 0);
end;


function OpenMLDLPort: boolean;
// Opens the connection the the FTDI USB module and the MLDL
var
  DeviceIndex : DWord;
  Found : boolean;
begin
  Found := false;
  MLDL_Error := Err_MLDL_OK;
  FT_Enable_Error_Report := false;
  GetFTDeviceCount;
  DeviceIndex := 0;
  if FT_Device_Count > 0 then repeat
    GetFTDeviceDescription(DeviceIndex);
    MLDL_Description := FT_Device_String;
    GetFTDeviceSerialNo(DeviceIndex);
    MLDL_Serial := FT_Device_String;
    if Pos('MLDL2000', MLDL_Description) = 1 then Found := true;
    DeviceIndex := DeviceIndex + 1;
  until Found or (DeviceIndex > (FT_Device_Count - 1));
  if Found then Open_USB_Device_By_Device_Description(MLDL_Description);
  MLDL_Open := Found;
  OpenMLDLPort := Found;
end;


procedure CloseMLDLPort;
// Clode the MLDL2000 connection
Var
  res : FT_Result;
begin
  FT_Enable_Error_Report := false;
  res := Reset_USB_Device;                   // Send reset to USB device
  res := Reset_USB_Device;                   // again to be certain
  MPSSE_On;                                  // Set MPSSE Mode

  OutIndex := 0;
  AddToBuffer(MPSSE_80);          // 
  AddToBuffer($00);               //
  AddToBuffer($00);               // Set Pins to all inputs
  AddToBuffer(MPSSE_82);          // Set Hi Byte Command
  AddToBuffer($00);               //
  AddToBuffer($00);               // Set Pins to all inputs
  SendBytes(OutIndex);

  res := Close_USB_Device;
  MLDL_Open := False;
end;


function PortIsOpen: boolean;
// check if USB is still connected and if communication is possible
var
  res: FT_Result;
begin
  PortIsOpen := true;
  MLDL_Error := Err_MLDL_OK;
  FT_Enable_Error_Report := false;
  res := Get_USB_Device_QueueStatus;
  if res <> FT_OK then begin
    PortIsOpen := false;
    MLDL_Open := false;
  end;
  FT_Enable_Error_Report := true;
end;

procedure BitBang_On;
var
  res: FT_Result;
begin
  if MLDL_Open then res := Set_USB_Device_BitMode(BitBangMask, BitBangOn);
end;

procedure BitBang_Off;
var
  res: FT_Result;
begin
  if MLDL_Open then res := Set_USB_Device_BitMode(BitBangMask, BitBangOff);
end;

procedure MPSSE_On;
var
  res: FT_Result;
begin
  if MLDL_Open then res := Set_USB_Device_BitMode(BitBangMask, FTMode_MPSSE);
end;

procedure MPSSE_Off;
var
  res: FT_Result;
begin
  if MLDL_Open then res := Set_USB_Device_BitMode(BitBangMask, FTMode_Reset);
end;


procedure AddToBuffer(I : integer);
// Adds one byte to the Output Buffer and increments Index
begin
  FT_Out_Buffer[OutIndex]:= I and $FF;
  Inc(OutIndex);
end;


procedure SendBytes(NumberOfBytes : integer);
// Send NumberOfBytes to the USB buffer
var
  i : integer;
begin
  i := Write_USB_Device_Buffer(NumberOfBytes);
  OutIndex := NumberOfBytes;
end;


procedure SendDirect(OutByte: Byte);
// immediate output of OutByte);
// Will purge input and output buffer
// assumes that MPSSE mode is on
var
  i: integer;
begin
  OutIndex := 0;
  AddToBuffer(OutByte);
  i := Write_USB_Device_Buffer(1);
end;


function ReceiveBuffer(RxCount: integer): integer;
// Read RxCount bytes from Rx Buffer or less
var
  i: integer;
  res: FT_Result;
begin
  result := 0;
  res := Get_USB_Device_QueueStatus;       // Number of bytes in Receive Queue
  if (FT_Q_Bytes > 0) then begin             // if Rx Queue is not empty
    if (RxCount <> 0) then begin               // if RxCount <> 0
      if (RxCount > FT_Q_Bytes) then begin
        // Not enough in buffer, read whole buffer
        i := Read_USB_Device_Buffer(FT_Q_Bytes);
        result := i;
      end else begin
        // Rx buffer has more than requested, read requested amount
        i := Read_USB_Device_Buffer(RxCount);
        result := i;
      end;
    end;
  end;
end;


procedure FlushInBuffer();
// Read the Rx Buffer until it is empty
var
  res: FT_Result;
  i: integer;
begin
  res := Get_USB_Device_QueueStatus;
  IF (FT_Q_Bytes > 0) then i := Read_USB_Device_Buffer(FT_Q_Bytes);
end;


procedure PurgeChip;
// Purge data buffer
var
  res : FT_Result;
  j : integer;
begin
  res := Reset_USB_Device;
  res := Get_USB_Device_QueueStatus;
  if FT_Q_Bytes > 0 then j := Read_USB_Device_Buffer(FT_Q_Bytes)
end;

procedure SendImmediate;
// Flush USB buffer back to PC and send whatever is in the buffer
begin
  AddToBuffer(MPSSE_87);
  SendBytes(OutIndex);
end;

procedure LoopBackOn;
// Enable LoopBack mode
begin
  OutIndex := 0;
  AddToBuffer(MPSSE_84);
  SendBytes(OutIndex);
end;

procedure LoopBackOff;
// Enable LoopBack mode
begin
  OutIndex := 0;
  AddToBuffer(MPSSE_85);
  SendBytes(OutIndex);
end;


procedure WPinsLo(Value: Byte);
begin
  OutIndex := 0;
  AddToBuffer(MPSSE_80);      // Set Low Byte Command
  AddToBuffer(Value);
  AddToBuffer(MPSSE_MaskLo);  // direction = X_CLK + X_DIN + X_STROBE + X_EN + X_PE
  SendBytes(OutIndex);
end;


procedure WPinsHi(Value: Byte);
// Currently the Hi pins are X_SPAREn and are always input!
begin
  OutIndex := 0;
  AddToBuffer(MPSSE_82);      // Set Hi Byte Command
  AddToBuffer(Value);
  AddToBuffer(MPSSE_MaskHi);  // direction = CPLD_PWR
  SendBytes(OutIndex);
end;


function RPinsLo: byte;
var
  res : FT_Result;
  i : integer;
  timeout_end : TDateTime;
  timeout : boolean;
begin
  FlushInBuffer;
  OutIndex := 0;
  AddToBuffer(MPSSE_81);       // Read Low Byte Command
  SendImmediate;
  // Wait for data
  timeout_end := IncMilliSecond(Now, PrefUSBTO);
  repeat
    res := Get_USB_Device_QueueStatus;
    if (FT_Q_Bytes = 0) then sleep(0);     // give up timeslice
    timeout := Now > timeout_end;
  until (FT_Q_Bytes >= 1) or (res <> FT_OK) or timeout;
  if (FT_Q_Bytes > 0) then begin
    i := Read_USB_Device_Buffer(FT_Q_Bytes);
    Result := FT_In_Buffer[0];
  end else
    Result := $00;
end;


function RPinsHi: byte;
var
  res: FT_Result;
  i : integer;
//  timeout_start,
  timeout_end : TDateTime;
  timeout : boolean;
begin
  FlushInBuffer;
  OutIndex := 0;
  AddToBuffer(MPSSE_83);       // Read Low Byte Command
  SendImmediate;
  // Wait for data
  timeout_end := IncMilliSecond(Now, PrefUSBTO);
  repeat
    res := Get_USB_Device_QueueStatus;
    if (FT_Q_Bytes = 0) then sleep(0);     // give up timeslice
    timeout := Now > timeout_end;
  until (FT_Q_Bytes >= 1) or (res <> FT_OK) or timeout;
  if (FT_Q_Bytes > 0) then begin
    i := Read_USB_Device_Buffer(FT_Q_Bytes);
    Result := FT_In_Buffer[0];
  end else
    Result := $00;
end;


procedure MPSSE_Clock(Speed: word);
begin
  OutIndex := 0;
  AddToBuffer(MPSSE_86);
  AddToBuffer(Speed and $FF);
  AddToBuffer(Speed shr 8);
  SendBytes(OutIndex);
end;


function Sync_To_MPSSE : boolean;
// This should satisfy outstanding commands.
// We will use $AA and $AB as commands which
// are invalid so that the MPSSE block should echo these
// back to us preceded with an $FA
var
  res : FT_Result;
  i, j : integer;
  Done : boolean;
begin
  Sync_To_MPSSE := false;
  res := Get_USB_Device_QueueStatus;
  if res <> FT_OK then exit;
  if (FT_Q_Bytes > 0) then
    i := Read_USB_Device_Buffer(FT_Q_Bytes);
  repeat
    OutIndex := 0;
    AddToBuffer($AA); // bad command
    SendBytes(OutIndex);
    res := Get_USB_Device_QueueStatus;
  until (FT_Q_Bytes > 0) or (res <> FT_OK);
  if res <> FT_OK then exit;
  i := Read_USB_Device_Buffer(FT_Q_Bytes);
  j := 0;
  Done := False;
  repeat
    if (FT_In_Buffer[j] = $FA) then begin
      if (j < (i - 2)) then begin
        if (FT_In_Buffer[j + 1] = $AA) then Done := true;
      end;
    end;
    j := j + 1;
  until (j=i) or Done;
  OutIndex := 0;
  AddToBuffer($AB); // bad command
  SendBytes(OutIndex);
  repeat
    res := Get_USB_Device_QueueStatus;
  until (FT_Q_Bytes > 0) or (res <> FT_OK); // or timeout
  if res <> FT_OK then exit;
  i := Read_USB_Device_Buffer(FT_Q_Bytes);
  j := 0;
  Done := False;
  repeat
    if (FT_In_Buffer[j] = $FA) then begin
      if (j <= (i - 2)) then begin
        if (FT_In_Buffer[j + 1] = $AB) then Done := true;
      end;
    end;
    j := j + 1;
  until (j=i) or Done;
  if Done then Sync_To_MPSSE := true;
end;


procedure ReadSRAMTyp;
var
  BottomWd, TopWd, BottomCmp, TopRslt: word;
begin
  SRAM_Sz := SRAMUNKNOWN;

  // This works as follows:
  //   - read a word from the lower part of SRAM and save
  //   - read a word from the 'same' location in the upper part of SRAM and save
  //   - write the first word to the location of the 2nd word
  //   - write the complement of the 1s word to the location of the 1st word
  //   - read the 2nd word
  //   - if the 2nd word is the original 1st word SRAM is dual sized
  //   - if the 2nd word is the complement of the 1st word SRAM is single sized
  //   - if the 2nd word is anything else there is an error
  //   - finally restore the original contents

  MLDL_ReadByte(SRAM, $00000, BottomWd);
  MLDL_ReadByte(SRAM, $80000, TopWd);

  BottomCmp := NOT BottomWd;

  MLDL_WriteByte(SRAM, $80000, BottomWd);
  MLDL_WriteByte(SRAM, $00000, BottomCmp);

  MLDL_ReadByte(SRAM, $80000, TopRslt);

  if TopRslt = BottomWd then begin
    SRAM_Sz := DOUBLE;
    SRAM_Pages := 255;

  end else if TopRslt = BottomCmp then begin
    SRAM_Sz := REGULAR;
    SRAM_Pages := 127;
  end;

  MLDL_WriteByte(SRAM, $00000, BottomWd);
  MLDL_WriteByte(SRAM, $80000, TopWd);

  // next two lines for debugging ...
  MLDL_ReadByte(SRAM, $00000, BottomWd);
  MLDL_ReadByte(SRAM, $80000, TopWd);

end;


procedure InitMLDLComm;
// Initialize, prepare for MLDL2000 communication
var
  res: FT_Result;
  passed: boolean;
  FlashTp: word;
begin
  passed := true;
  if passed then begin
    res := Reset_USB_Device;                   // Send reset to USB device
    res := Reset_USB_Device;                   // again to be certain
    res := Set_USB_Parameters(USBBuffSize,0);  // Set USB Transfer sizes
                                               // TransferSize IN: USBBuffSize = $800
                                               // TransferSize OUT = 0 ?? check

    FT_Current_Baud := MLDLBaudRate;           // Set to 57600, rate is 16*57600
    res := Set_USB_Device_BaudRate;            // Program Baud Rate
    res := Set_USB_Device_LatencyTimer(10);    // and Latency Timer

    PurgeChip;                                 // Reset USB Interface
    Sleep(20);                                 // Wait a little bit ....
    MPSSE_On;                                  // Set MPSSE Mode
    WPinsLo(MPSSE_Default);                    // Outputs to default value
    WPinsHi(PWR_CPLD);                         // CPLD Power always on
    MPSSE_Clock(PrefCommSpeed);
    LoopBackOff;                               // make certain LoopBack mode is off

    DisableMLDL;
    // Determine FLASH Memory Type
    FLASH_TP := FLASHUNKNOWN;
    MLDL_FLASHReadID(FlashTp);
    if FlashTp = FlashTypeTop then
      FLASH_TP := TOPBOOT
      else if FlashTp = FlashTypeBottom then
        FLASH_TP := BOTTOMBOOT;

    SRAM_Sz := SRAMUNKNOWN;

    ReadSRAMTyp;

    EnableMLDL;

  end;
end;


procedure InitJTAGMode(USB_mode: boolean);
// for use with the USB JTAG functions
begin
  if USB_Mode then begin
    // For JTAG programming
    // used with the USB JTAG functions
    MPSSE_Clock(PrefJTAGSpeed);
    OutIndex := 0;
    AddToBuffer(MPSSE_80);          // Set Pins Lo Byte
    AddToBuffer(JTAG_Default);      // same as default mode, drive X_PE
    AddToBuffer(MPSSE_MaskLo);
    AddToBuffer(MPSSE_82);          // Set Hi Byte Command
    AddToBuffer(PWR_CPLD);          // CPLD Power On
    AddToBuffer(MPSSE_MaskHi);      // direction = CPLD_PWR
    SendBytes(OutIndex);
  end else begin
    // For JTAG programming, X_CLK, X_DOUT, X_DIN, X_STROBE should be inputs
    // used with the Xilinx/Impact programming cable
    OutIndex := 0;
    AddToBuffer(MPSSE_80);          // Set Pins Lo Byte
    AddToBuffer(JTAG_ON);           // X_STROBE hi, X_CLK lo
    AddToBuffer(JTAGMask);
    AddToBuffer(MPSSE_82);          // Set Hi Byte Command
    AddToBuffer(PWR_CPLD);          // CPLD Power On
    AddToBuffer(MPSSE_MaskHi);      // direction = CPLD_PWR
    SendBytes(OutIndex);
  end;
end;


procedure InitDefault;
begin
  MPSSE_Clock(PrefCommSpeed);
  OutIndex := 0;
  AddToBuffer(MPSSE_80);          // Set Pins Lo Byte
  AddToBuffer(MPSSE_Default);     // X_STROBE hi, X_CLK lo
  AddToBuffer(MPSSE_MaskLo);
  AddToBuffer(MPSSE_82);          // Set Hi Byte Command
  AddToBuffer(PWR_CPLD);          // CPLD Power On
  AddToBuffer(MPSSE_MaskHi);      // direction = CPLD_PWR
  SendBytes(OutIndex);
end;


procedure DeInitMLDLComm();
// Disable
begin
  CloseMLDLPort;
end;


procedure SetPinLo(Pin: Byte);
// Immediate asynchronous set of indicated pin
// Bits set to '1' will be made high
var
  CurrState: Byte;
begin
  CurrState := RPinsLo;
  CurrState := CurrState or Pin;
  WPinsLo(CurrState);
end;


procedure ClearPinLo(Pin: Byte);
// Immediate asynchronous clear of indicated pin
// Bits set to '1' will be made high
var
  CurrState: Byte;
begin
  CurrState := RPinsLo;
  CurrState := CurrState and not Pin;
  WPinsLo(CurrState);
end;


procedure TogglePinLo(Pin: Byte);
// Immediate asynchronous clear of indicated pin
// Bits set to '1' will be toggled
var
  CurrState, NewState: Byte;
begin                                  //   CurrState NewState Pin   Eind
  CurrState := RPinsLo;                //     0101             1100  1001
  NewState  := CurrState and not Pin;  //     0101      0001   1100  1001
  CurrState := not CurrState;          //     1010      0001   1100  1001
  CurrState := CurrState and Pin;      //     1000      0001   1100  1001
  NewState  := NewState or CurrState;  //     1000      1001   1100  1001
  WPinsLo(NewState);
end;

procedure SetPinHi(Pin: Byte);
// Immediate asynchronous set of indicated pin
// Bits set to '1' will be made high
var
  CurrState: Byte;
begin
  CurrState := RPinsHi;
  CurrState := CurrState or Pin;
  WPinsHi(CurrState);
end;


procedure ClearPinHi(Pin: Byte);
// Immediate asynchronous clear of indicated pin
// Bits set to '1' will be made high
var
  CurrState: Byte;
begin
  CurrState := RPinsHi;
  CurrState := CurrState and not Pin;
  WPinsHi(CurrState);
end;


procedure TogglePinHi(Pin: Byte);
// Immediate asynchronous clear of indicated pin
// Bits set to '1' will be toggled
var
  CurrState, NewState: Byte;
begin                                  //   CurrState NewState Pin   Eind
  CurrState := RPinsHi;                //     0101             1100  1001
  NewState  := CurrState and not Pin;  //     0101      0001   1100  1001
  CurrState := not CurrState;          //     1010      0001   1100  1001
  CurrState := CurrState and Pin;      //     1000      0001   1100  1001
  NewState  := NewState or CurrState;  //     1000      1001   1100  1001
  WPinsHi(NewState);
end;

function Bit_Hi(Data: byte; BitNum: byte): boolean;
// returns True if BitNum in Data is set, otherwise returns false
// only one bit in BitNum may be set!
begin
  if Data and BitNum = $00 then Result := false else Result := true;
end;

function Bit_Lo(Data: byte; BitNum: byte): boolean;
// returns True if BitNum in Data is set, otherwise returns false
// only one bit in BitNum may be set!
begin
  if Data and BitNum = $00 then Result := true else Result := false;
end;

function Bits_Hi(Data: byte; Pattern: byte): boolean;
// returns True if the set bits in the pattern are also set in Data
begin
  if Data and Pattern = Pattern then Result := true else Result := false;
end;

function Bits_Pattern(Data: byte; Pattern: byte; Mask: byte): boolean;
// returns True if the bits in Mask match the exact status of the bits in Pattern
begin
  if (Data and Mask) = (Pattern and Mask) then Result := true else Result := false;
end;


procedure DisableMLDL;
// Disables MLDL2000 for I/O operations
begin
  ClearPinLo(X_EN);
  CPLD_On;
end;


procedure EnableMLDL;
// Enables MLDL2000 for normal operations
begin
  SetPinLo(X_EN);
  CPLD_On;
end;

procedure CPLD_On;
// Power the CPLD
begin
  SetPinHi(PWR_CPLD);
end;

procedure CPLD_Off;
// Turns the CPLD Off
begin
  ClearPinHi(PWR_CPLD);
end;

function Get_Data (NumBytes: integer) : boolean;
// Read NumBytes of Data from the USB interface
var
  res : FT_Result;
  NoBytes, i, j : integer;
  TotalBytes : integer;
  timeout_end  : TDateTime;
  timeout     : boolean;
begin
  Get_Data := false;
  NoBytes := NumBytes;  // get whole bytes
  TotalBytes := 0;
  InRead := 0;

  // Read actual data
  repeat
    timeout_end := IncMilliSecond(Now, PrefUSBTO);
    repeat
      res := Get_USB_Device_QueueStatus;
      if ( FT_Q_Bytes = 0) then sleep(0); // give up timeslice
      timeout := Now > timeout_end;
    until (FT_Q_Bytes > 0) or (res <> FT_OK) or timeout;
    if (FT_Q_Bytes > 0) then begin
      j := Read_USB_Device_Buffer(FT_Q_Bytes);
      for i := 0 to (j-1) do begin
        Indata[TotalBytes] := FT_In_Buffer[i];
        TotalBytes := TotalBytes + 1;
      end;
    end;
  until (TotalBytes >= NoBytes) or (res <> FT_OK) or timeout;

  if not(timeout) and (res = FT_OK) then begin
    Get_Data := true;
    InRead := TotalBytes;
  end;
end;


procedure StrobeSequence;
begin
{ Timing should be

  X_CLK     ________|-|______
  X_STROBE  ------|_____|----
  sequence      1  2 3 4  5        }

  // (1)
  AddToBuffer(MPSSE_80);
  AddToBuffer(X_STROBE);           // X_STROBE hi, X_CLK lo
  AddToBuffer(MPSSE_MaskLo);

  // (2)
  AddToBuffer(MPSSE_80);
  AddToBuffer(0);                  // X_STROBE lo, X_CLK lo
  AddToBuffer(MPSSE_MaskLo);

  // (3)
  AddToBuffer(MPSSE_80);
  AddToBuffer(X_CLK);              // X_STROBE lo, X_CLK hi
  AddToBuffer(MPSSE_MaskLo);

  // (4)
  AddToBuffer(MPSSE_80);
  AddToBuffer(0);                  // X_STROBE lo. X_CLK lo
  AddToBuffer(MPSSE_MaskLo);

  // (5)
  AddToBuffer(MPSSE_80);
  AddToBuffer(X_STROBE);           // X_STROBE hi, X_CLK lo
  AddToBuffer(MPSSE_MaskLo);

end;


//---------------------------------------------------------------------------//
//                                                                           //
//  HP41 I/O Functions                                                       //
//                                                                           //
//---------------------------------------------------------------------------//


procedure StrobeSequenceIO;        // with X_EN high
begin
{ Timing should be

  X_CLK     ________|-|______
  X_STROBE  ------|_____|----
  sequence      1  2 3 4  5        }

  // (1)
  AddToBuffer(MPSSE_80);
  AddToBuffer(X_STROBE+X_EN);         // X_STROBE hi, X_CLK lo
  AddToBuffer(MPSSE_MaskLo);

  // (2)
  AddToBuffer(MPSSE_80);
  AddToBuffer(X_EN);                  // X_STROBE lo, X_CLK lo
  AddToBuffer(MPSSE_MaskLo);

  // (3)
  AddToBuffer(MPSSE_80);
  AddToBuffer(X_EN + X_CLK);          // X_STROBE lo, X_CLK hi
  AddToBuffer(MPSSE_MaskLo);

  // (4)
  AddToBuffer(MPSSE_80);
  AddToBuffer(X_EN);                  // X_STROBE lo. X_CLK lo
  AddToBuffer(MPSSE_MaskLo);

  // (5)
  AddToBuffer(MPSSE_80);
  AddToBuffer(X_EN + X_STROBE);       // X_STROBE hi, X_CLK lo
  AddToBuffer(MPSSE_MaskLo);

end;

procedure MLDL_ReadIO(var Data: word);
// read two bytes from the I/O Port, X_DAV/X_BSY state is not checked
// X_EN must be high (MLDL enabled!
begin
  OutIndex := 0;

  AddToBuffer(MPSSE_80);    // Set outputs to the right setting
  AddToBuffer(X_EN + X_STROBE);
  AddToBuffer(MPSSE_MaskLo);

  // Now read the data, we will read two bytes

  AddToBuffer(MPSSE_28);           // Read LSB first on +ve edge
  AddToBuffer($01);                // 2 bytes to read
  AddToBuffer($00);

  // Do the X_STROBE sequence
  StrobeSequenceIO;

  SendBytes(OutIndex);

  // Now start the actual reading ......
  if Get_Data(2) then
    Data := InData[0] + (InData[1] shl 8)
  else
    Data := 0;

end;


procedure MLDL_WritIO(Data: byte);
// write one byte to the I/O Port, X_DAV/X_BSY state is not checked
// X_EN must be high (MLDL enabled!
begin
  OutIndex := 0;

  AddToBuffer(MPSSE_80);    // Set outputs to the right setting
  AddToBuffer(X_EN + X_STROBE);    // Default setting, all others low
  AddToBuffer(MPSSE_MaskLo);

  // Now clock our byte pattern
  AddToBuffer(MPSSE_19);         // Write LSB First on -ve edge start clock at 0
  AddToBuffer($01);              // 5 bytes to output
  AddToBuffer($00);
  AddToBuffer(Data);
  AddToBuffer(Data);

  // Do the X_STROBE sequence
  StrobeSequenceIO;

  SendBytes(OutIndex);
end;


function MLDL_XDAV: boolean;
// returns the X_DAV state, when true, signal is set (data available)
begin
  MLDL_XDAV := (RPinsLo and X_DAV) = X_DAV;
end;


function MLDL_XBSY: boolean;
// returns the X_BSY state, when true, signal is set (HP41 busy)
begin
  MLDL_XBSY := (RPinsLo and X_BSY) = X_BSY;
end;


//---------------------------------------------------------------------------//
//                                                                           //
//  Memory Access Functions                                                  //
//                                                                           //
//---------------------------------------------------------------------------//

// Read Byte
procedure MLDL_ReadByte(MemTp: MemType; Address: LongWord; var Data: Word);
var
  Byte1, Byte2, Byte3 : Byte;
begin
  // Assume that X_EN is already low and the MLDL2000 is disabled!

  OutIndex := 0;

  AddToBuffer(MPSSE_80);    // Set outputs to the right setting
  AddToBuffer(X_STROBE);
  AddToBuffer(MPSSE_MaskLo);

  if MemTp = SRAM then Byte1 := $0C else Byte1 := $08;
  Byte1 := Byte1 or ((Address shl 4) and $F0);
  Byte2 := (Address shr 4) and $FF;
  Byte3 := (Address shr 12) and $FF;

  // Now clock our byte pattern
  AddToBuffer(MPSSE_19);         //Write LSB First on -ve edge start clock at 0
  AddToBuffer($02);              // 4 bytes to output
  AddToBuffer($00);
  AddToBuffer(Byte1);
  AddToBuffer(Byte2);
  AddToBuffer(Byte3);

  // Do the X_STROBE sequence
  StrobeSequence;

  // Now read the data, we will read two bytes

  AddToBuffer(MPSSE_28);           // Read LSB first on +ve edge
  AddToBuffer($01);                // 2 bytes to read
  AddToBuffer($00);

  SendBytes(OutIndex);

  // Now start the actual reading ......

  if Get_Data(2) then
    Data := InData[0] + (InData[1] shl 8)
  else
    Data := 0;

end;


procedure ReadFast(MemTp: MemType; Address: LongWord);
var
  Byte1, Byte2, Byte3 : Byte;
begin
  // Assume that X_EN is already low and the MLDL2000 is disabled!
  Address := Address and Range_1M;         // First align Address


  AddToBuffer(MPSSE_80);    // Set outputs to the right setting
  AddToBuffer(X_STROBE);
  AddToBuffer(MPSSE_MaskLo);

  if MemTp = SRAM then Byte1 := $0C else Byte1 := $08;
  Byte1 := Byte1 or ((Address shl 4) and $F0);
  Byte2 := (Address shr 4) and $FF;
  Byte3 := (Address shr 12) and $FF;

  // Now clock our byte pattern
  AddToBuffer(MPSSE_19);         //Write LSB First on -ve edge start clock at 0
  AddToBuffer($02);              // 4 bytes to output
  AddToBuffer($00);
  AddToBuffer(Byte1);
  AddToBuffer(Byte2);
  AddToBuffer(Byte3);

  // Do the X_STROBE sequence
  StrobeSequence;

  // Now read the data, we will read two bytes
  AddToBuffer(MPSSE_28);           // Read LSB first on +ve edge
  AddToBuffer($01);                // 2 bytes to read
  AddToBuffer($00);

end;



procedure WriteFast(MemTp: MemType; Address: LongWord; Data: Word);
var
  Byte1, Byte2, Byte3, Byte4, Byte5 : Byte;
begin
  // Assume that X_EN is already low and the MLDL2000 is disabled!

  AddToBuffer(MPSSE_80);    // Set outputs to the right setting
  AddToBuffer(X_STROBE);    // Default setting, all others low
  AddToBuffer(MPSSE_MaskLo);

  Byte1 := (Data shl 2) and $FC;
  Byte2 := (Data shr 6) and $FF;
  if MemTp = SRAM then Byte3 := $04 else Byte3 := $00;
  Byte3 := Byte3 or ((Data shr 14) and $03);
  Byte3 := Byte3 or ((Address shl 4) and $F0);
  Byte4 := (Address shr 4) and $FF;
  Byte5 := (Address shr 12) and $FF;

  // Now clock our byte pattern
  AddToBuffer(MPSSE_19);         // Write LSB First on -ve edge start clock at 0
  AddToBuffer($04);              // 4 bytes to output
  AddToBuffer($00);
  AddToBuffer(Byte1);
  AddToBuffer(Byte2);
  AddToBuffer(Byte3);
  AddToBuffer(Byte4);
  AddToBuffer(Byte5);

  // Do the X_STROBE sequence
  StrobeSequence;
end;


procedure MLDL_WriteByte(MemTp: MemType; Address: LongWord; Data: Word);
// ACtually writes a Word!
var
  Byte1, Byte2, Byte3, Byte4, Byte5 : Byte;
begin
  // Assume that X_EN is already low and the MLDL2000 is disabled!

  OutIndex := 0;

  AddToBuffer(MPSSE_80);    // Set outputs to the right setting
  AddToBuffer(X_STROBE);    // Default setting, all others low
  AddToBuffer(MPSSE_MaskLo);
  Byte1 := (Data shl 2) and $FC;
  Byte2 := (Data shr 6) and $FF;
  if MemTp = SRAM then Byte3 := $04 else Byte3 := $00;
  Byte3 := Byte3 or ((Data shr 14) and $03);
  Byte3 := Byte3 or ((Address shl 4) and $F0);
  Byte4 := (Address shr 4) and $FF;
  Byte5 := (Address shr 12) and $FF;

  // Now clock our byte pattern
  AddToBuffer(MPSSE_19);         // Write LSB First on -ve edge start clock at 0
  AddToBuffer($04);              // 5 bytes to output
  AddToBuffer($00);
  AddToBuffer(Byte1);
  AddToBuffer(Byte2);
  AddToBuffer(Byte3);
  AddToBuffer(Byte4);
  AddToBuffer(Byte5);

  // Do the X_STROBE sequence
  StrobeSequence;

  SendBytes(OutIndex);
end;


procedure MLDL_ReadBlock(MemTp: MemType; Address: LongWord; NumBytes: Word);
// Reads Block of data to WordArray
var
  Index, i: integer;
  NumBlocks, RestWords, WordsLeft, CurWord: integer;
begin
  NumBlocks := NumBytes div PrefBlockSize;
  RestWords := NumBytes mod PrefBlockSize;
  WordsLeft := RestWords;
  Index := 0;
  CurWord := 0;
  Err_MLDL := Err_MLDL_OK;
  while ((NumBlocks > 0) and (Err_MLDL = Err_MLDL_OK)) do begin
    OutIndex := 0;
    for i := 0 to PrefBlockSize - 1 do begin
      ReadFast(MemTp, Address + Index);
      Inc(Index);
    end;
    SendBytes(OutIndex);
    if Get_Data(2 * PrefBlockSize) then begin
      for i := 0 to PrefBlockSize - 1 do begin
        WordArray[CurWord] := InData[i * 2] + (InData[i * 2 + 1] shl 8);
        Inc(CurWord);
      end;
    end else Err_MLDL := Err_MLDL_USBRead;
    Dec(NumBlocks);
  end;

  if (RestWords > 0) and (Err_MLDL = Err_MLDL_OK) then begin
    OutIndex := 0;
    while ((RestWords > 0) and (Err_MLDL = Err_MLDL_OK)) do begin
      ReadFast(MemTp, Address + Index);
      Inc(Index);
      Dec(RestWords);
    end;
    SendBytes(OutIndex);

    if Get_Data(WordsLeft * 2) then begin
      for i := 0 to WordsLeft do begin
        WordArray[CurWord] := InData[i * 2] + (InData[i * 2 + 1] shl 8);
        Inc(CurWord);
      end;
    end else Err_MLDL := Err_MLDL_USBRead;
  end
end;


procedure MLDL_WriteBlock(MemTp: MemType; Address: LongWord; NumBytes: Word);
// Writes Block of data from WordArray
var
  Index, i: integer;
  NumBlocks, RestWords: integer;
begin
  NumBlocks := NumBytes div PrefBlockSize;
  RestWords := NumBytes mod PrefBlockSize;
  Index := 0;
  MLDL_Error := Err_MLDL_OK;

  if MemTp = SRAM then begin
    while ((NumBlocks > 0) and (Err_MLDL = 0)) do begin
      OutIndex := 0;
      for i := 0 to PrefBlockSize - 1 do begin
        WriteFast(SRAM, Address + Index, WordArray[Index]);
        Inc(Index);
      end;
      SendBytes(OutIndex);
      NumBlocks := NumBlocks - 1;
    end;

    if (RestWords > 0) and (Err_MLDL = Err_MLDL_OK) then begin
      OutIndex := 0;
      while ((RestWords > 0) and (Err_MLDL = 0)) do begin
        WriteFast(SRAM, Address + Index, WordArray[Index]);
        Inc(Index);
        Dec(RestWords);
      end;
      SendBytes(OutIndex);
    end;

  end else begin // this is FLASH
    FLASHBypass;
    Index := 0;
    repeat
      FLASHWriteByteFast(Address + Index, WordArray[Index]);
      Inc(Index);
    until ((Index = NumBytes) or (MLDL_Error <> Err_MLDL_OK));
    FLASHBypassReset;
  end;
end;


// Write SRAM Block
procedure SRAMWriteBlock(Address: LongWord; Index: integer);
// Writes block of 16 words to SRAM Address, starting at PageArray[Index]
var
  i : integer;
begin
  OutIndex := 0;
  for i := 0 to 15 do begin
    WriteFast(SRAM, Address + i, swap(PageArray[Index + i]));
  end;
  SendBytes(OutIndex);
end;


// Write SRAM Byte
procedure SRAMWriteByte(Address: LongWord; Data: Word);
// Write single Word to FLASH
// This can be optimized by not doing SendBytes between every Write
begin
  MLDL_WriteByte(SRAM, Address, Data);
end;


//---------------------------------------------------------------------------//
//                                                                           //
//  FLASH Functions                                                          //
//                                                                           //
//---------------------------------------------------------------------------//

procedure MLDL_FLASHReadID(var DevType: Word);
// Returns DevType:  $22C4  Top Boot Block Device
//                   $2249  Bottom Boot Block Device
begin
  // READ FLASH MEMORY DEVICE TYPE
  MLDL_WriteByte(FLASH, $00000555, $AA);
  MLDL_WriteByte(FLASH, $000002AA, $55);
  MLDL_WriteByte(FLASH, $00000555, $90);
  MLDL_ReadByte(FLASH, $00000001, DevType);
  MLDL_FLASHReset;
end;


// Write FLASH Byte
procedure FLASHWriteByte(Address: LongWord; Data: Word);
// Write single Word to FLASH
// Maybe do a direct read immediately after this?
var
  timeout_end : TDateTime;
  timeout : boolean;
begin
  MLDL_Error := Err_MLDL_OK;
  OutIndex := 0;

  WriteFast(FLASH, $00000555, $00AA);
  WriteFast(FLASH, $000002AA, $0055);
  WriteFast(FLASH, $00000555, $00A0);
  WriteFast(FLASH, Address, Data);

// Do an immediate read of the status bits, this may be OK
//  AddToBuffer(MPSSE_81);       // Read Low Byte Command
  SendImmediate;

  // wait until programming ready
  timeout_end := IncMilliSecond(Now, PrefFLASHTO);
  repeat
    sleep(0);
    timeout := Now > timeout_end;
  until (((RPinsLo and X_BSY) = X_BSY) or timeout);
  if timeout then begin
    MLDL_Error := Err_MLDL_TimeOut;
    MLDL_FLASHReset;
  end;
end;

procedure FLASHWriteByteFast(Address: LongWord; Data: Word);
// Write single Word to FLASH, assume Unlock Bypass!
// This can be optimized by not doing SendBytes between every Write
var
  timeout_end : TDateTime;
  timeout : boolean;
begin
  // FLASH WRITE SEQUENCE FOR AM29LV160D, Unlock Bypass
  OutIndex := 0;
  WriteFast(FLASH, $00000000, $A0);
  WriteFast(FLASH, Address, Data);
  SendImmediate;
  // wait until programming ready
  timeout_end := IncMilliSecond(Now, PrefFLASHTO);
  repeat
    sleep(0);
    timeout := Now > timeout_end;
  until (((RPinsLo and X_BSY) = X_BSY) or timeout);
  if timeout then begin
    MLDL_Error := Err_MLDL_TimeOut;
    MLDL_FLASHReset;
  end;
end;

procedure FLASHBypass;
begin
  // FLASH WRITE INITIALIZATION SEQUENCE FOR AM29LV160D
  // Setup Unlock bypass for faster programming
  MLDL_WriteByte(FLASH, $00000555, $AA);
  MLDL_WriteByte(FLASH, $000002AA, $55);
  MLDL_WriteByte(FLASH, $00000555, $20);
end;

procedure FLASHBypassReset;
begin
  // FLASH WRITE UNLOCK BYPASS RESET SEQUENCE FOR AM29LV160D
  MLDL_WriteByte(FLASH, $00000000, $90);
  MLDL_WriteByte(FLASH, $00000000, $00);
end;

procedure MLDL_FLASHReset;
begin
  // RESET SEQUENCE FOR AM29LV160D
  MLDL_WriteByte(FLASH, $00000000, $F0);
end;

// Erase FLASH Block
procedure MLDL_FLASHEraseSector(Address: LongWord);
var
  timeout_end : TDateTime;
  timeout : boolean;
begin
  MLDL_Error := Err_MLDL_OK;
  MLDL_FlashReset;   // Just to be certain
  // BLOCK ERASE SEQUENCE FOR AM29LV160D
  MLDL_WriteByte(FLASH, $00000555, $AA);
  MLDL_WriteByte(FLASH, $000002AA, $55);
  MLDL_WriteByte(FLASH, $00000555, $80);
  MLDL_WriteByte(FLASH, $00000555, $AA);
  MLDL_WriteByte(FLASH, $000002AA, $55);
  MLDL_WriteByte(FLASH, Address, $30);
  // wait until toggle bit stops
  // Have to create time-out here and maybe visual feedback
  timeout_end := IncMilliSecond(Now, PrefSecTO);
  repeat
    sleep(1);
    Application.ProcessMessages;
    timeout := Now > timeout_end;
  until (((RPinsLo and X_BSY) = X_BSY) or timeout);
  if timeout then begin
    MLDL_Error := Err_MLDL_TimeOut;
    MLDL_FLASHReset;
  end;
end;

// Erase ALL FLASH
procedure MLDL_FLASHEraseAll();
var
  timeout_end : TDateTime;
  timeout : boolean;
begin
  MLDL_Error := Err_MLDL_OK;
  MLDL_FLASHReset;    // Just to be cartain
  // CHIP ERASE SEQUENCE FOR AM29LV160D
  MLDL_WriteByte(FLASH, $00000555, $AA);
  MLDL_WriteByte(FLASH, $000002AA, $55);
  MLDL_WriteByte(FLASH, $00000555, $80);
  MLDL_WriteByte(FLASH, $00000555, $AA);
  MLDL_WriteByte(FLASH, $000002AA, $55);
  MLDL_WriteByte(FLASH, $00000555, $10);
  // wait until toggle bit stops
  // have to create visual feedback
  timeout_end := IncMilliSecond(Now, PrefALLTO);
  repeat
    sleep(1);
    Application.ProcessMessages;
    timeout := Now > timeout_end;
  until (((RPinsLo and X_BSY) = X_BSY) or timeout);
  if timeout then begin
    MLDL_Error := Err_MLDL_TimeOut;
    MLDL_FLASHReset;
  end;
end;

//---------------------------------------------------------------------------//
//                                                                           //
//  JTAG Functions                                                           //
//                                                                           //
//---------------------------------------------------------------------------//


//======================================================================
//== JTAG state transition code
//======================================================================


procedure JTAG_wiggler_tms(tmsval, count: byte; Do_Read: boolean);
begin
  if count = 0 then exit;
  if count > 7 then exit;
  if Do_Read then
    AddToBuffer($6B)
  else
    AddToBuffer($4B);
  AddToBuffer(count - 1);
  AddToBuffer(tmsval);
end;


procedure JTAG_go_state(new_state: jtag_state);
// like JTAG_wiggler_state, also fires off command
var
  i: integer;
begin
  i := JTAG_wiggler_state(new_state, 0, false);
  SendBytes(OutIndex); // send off the command
end;

procedure JTAG_change_state(new_state: byte);
// Just changes the JTAG state without too much complications
var
  tmsval, count: byte;
begin

  OutIndex := 0;

  if current_state > $0F then
    current_state := $FF;

  if (current_state = undefined_jtag_state) then begin
    // we do not know current state, so go to TLR always
    JTAG_wiggler_tms($1F, 5, false);
    current_state := test_logic_reset;
  end;

  if (new_state = test_logic_reset) then begin
    // end state is TLR, so do a full reset always)
    JTAG_wiggler_tms($1F, 5, false);
    current_state := test_logic_reset;
  end else begin
    // have to change state to something else
    tmsval := StArray[current_state, new_state];
    count  := NumBitsArray[current_state, new_state];
    JTAG_wiggler_tms(tmsval, count, false);
  end;

  SendBytes(OutIndex); // send off the command
  current_state := new_state;
end;


function JTAG_wiggler_state(new_state: jtag_state; last_data: byte; Do_Read: boolean): integer;
// this returns the number of TMS clocks to work out the last bit of TDO
begin

  if current_state > $0F then begin
    JTAG_wiggler_tms($1F, 5, false);
    current_state := test_logic_reset;
  end;

  JTAG_wiggler_tms(StArray[current_state, new_state] or byte(last_data SHL 7),
          NumBitsArray[current_state, new_state], Do_Read);
  JTAG_wiggler_state := NumBitsArray[current_state, new_state];

  current_state := new_state;
end;


//=======================================================
// Routines to Control MPSSE hardware
//=======================================================


function JTAG_GetData(var in_data: data_type; Bit_Length: word; TMS_Clks: integer): boolean;
// This will work out the number of whole bytes to read and adjust for the TMS read
var
  res: FT_Result;
  NoBytes, i, j: integer;
  BitShift, Mod_Bit_Length: integer;
  ShiftBytes: integer;
  Last_Bit: byte;
  // Temp_Buffer: array[0..64000] of byte;
  Temp_Buffer: data_type;  // check if this works
  TotalBytes: integer;
  timeout_end: TDateTime;
  timeout: boolean;
begin

  JTAG_GetData := false;
  Mod_Bit_Length := Bit_Length - 1; // adjust for bit count of 1 less than no of bits
  NoBytes := Mod_Bit_Length DIV 8;  // get whole bytes
  BitShift := Mod_Bit_Length MOD 8; // get remaining bits
  if BitShift > 0 then
    NoBytes := NoBytes + 1;         // bump whole bytes if bits left over
  BitShift := 8 - BitShift;         // adjust for SHR of incoming byte
  NoBytes := NoBytes + 1;           // add 1 for TMS read byte
  TotalBytes := 0;
  ShiftBytes := 0;                  // Correction factor for reading too much bytes

  repeat

    repeat
      res := Get_USB_Device_QueueStatus;
      if ( FT_Q_Bytes = 0) then
        sleep(0);                   // give up timeslice
      timeout_end := IncMilliSecond(Now, PrefUSBTO);
      timeout := Now > timeout_end;
    until (FT_Q_Bytes > 0) or (res <> FT_OK) or timeout;
    if (FT_Q_Bytes > 0) then begin
      j := Read_USB_Device_Buffer(FT_Q_Bytes);
      for i := 0 to (j - 1) do begin
        Temp_Buffer[TotalBytes] := FT_In_Buffer[i];
        TotalBytes := TotalBytes + 1;
      end;
    end;
  until (TotalBytes >= NoBytes) or (res <> FT_OK) or timeout;

  if not(timeout) and (res = FT_OK) then begin

    JTAG_GetData := true;

    //adjust last 2 bytes
    if (BitShift < 8 ) then begin      // this means that we have rest bits

      // Correct for 2 extra bytes that are being read ??
      if TotalBytes > NoBytes then
        ShiftBytes := TotalBytes - NoBytes;
      NoBytes := TotalBytes;

      // Temp_Buffer[NoBytes - 2] contains the rest bits, align these
      Temp_Buffer[NoBytes - 2] := Temp_Buffer[NoBytes - 2] SHR BitShift;

      // The last bit is in the TMS bit, found in Temp_Buffer[NoBytes - 1]
      Last_Bit := byte(Temp_Buffer[NoBytes - 1] SHL (TMS_Clks - 1));
      Last_Bit := Last_Bit AND $80; // strip the rest
      Temp_Buffer[NoBytes - 2] := Temp_Buffer[NoBytes - 2] OR (Last_Bit SHR (BitShift - 1));

      // Transfer Temp_Buffer to in_data
      for j := 0 to (NoBytes - 2) do
        // correct for 2 extra bytes ???
        in_data[j] := Temp_Buffer[j + ShiftBytes];

    end else begin      // case for 0 bit shift in data + TMS read bit
      // Temp_Buffer[NoBytes - 1] contains TMS bits
      Last_Bit := byte(Temp_Buffer[NoBytes - 1] SHL (TMS_Clks - 1));
      Last_Bit := Last_Bit SHR 7; // strip the rest
      Temp_Buffer[NoBytes - 1] := Last_Bit;
      for j := 0 to (NoBytes - 1) do
        in_data[j] := Temp_Buffer[j];

    end;
  end;

end;


procedure JTAG_sout(jtag_register: shift_register; bit_length: word;
                    var out_data: data_type; state: jtag_state);
// JTAG Scan out
var
  i, j, TMS_Clks: integer;
  LastBit: byte;
  Mod_bit_length: word;
  Do_Read: boolean;
begin
  OutIndex := 0;
  j := 0;
  Mod_bit_length := bit_length - 1;
  Do_Read := false;
  if (jtag_register = instruction_register) then
    TMS_Clks := JTAG_wiggler_state(shift_ir, 0, Do_Read)
  else
    TMS_Clks := JTAG_wiggler_state(shift_dr, 0, Do_Read);

  if Mod_bit_length div 8 > 0 then begin // do whole bytes
    i := (Mod_bit_length div 8) - 1;
    AddToBuffer($19); // clk data bytes out on -ve clk LSB
    AddToBuffer(i AND $FF);
    AddToBuffer((i DIV 256) AND $FF);
    // now add the data bytes to go out
    repeat
      AddToBuffer(out_data[j]);
      j := j + 1;
    until j > i;
  end;

  if Mod_bit_length mod 8 > 0 then begin // do remaining bits
    i := (Mod_bit_length mod 8) - 1;
    AddToBuffer($1B); // clk data bits out on -ve clk LSB
    AddToBuffer(i AND $FF);
    // now add the data bits to go out
    AddToBuffer(out_data[j]);
  end;
  // get LastBit
  LastBit := out_data[j];
  j := bit_length mod 8;
  LastBit := LastBit SHR (j - 1);

  TMS_Clks := JTAG_wiggler_state(state, LastBit, Do_Read); // end it in state passed in
  SendBytes(OutIndex); // send off the command
end;


procedure JTAG_sio(jtag_register: shift_register;
                   bit_length: word; var out_data, in_data: data_type;
                   state: jtag_state; SndImmediate: boolean );
// JTAG Scan in and out
var
  i, j, TMS_Clks: integer;
  LastBit: byte;
  Mod_bit_length: word;
  Do_Read, passed: boolean;
begin
  OutIndex := 0;
  j := 0;
  Mod_bit_length := bit_length - 1;
  Do_Read := false;
  if (jtag_register = instruction_register) then
    TMS_Clks := JTAG_wiggler_state(shift_ir, 0, Do_Read)
  else
    TMS_Clks := JTAG_wiggler_state(shift_dr, 0, Do_Read);

  if Mod_bit_length div 8 > 0 then begin // do whole bytes
    i := (Mod_bit_length div 8) - 1;
    AddToBuffer($39); // clk data bytes out on -ve in +ve clk LSB
    AddToBuffer(i AND $FF);
    AddToBuffer((i DIV 256) AND $FF);
    // now add the data bytes to go out
    repeat
      AddToBuffer(out_data[j]);
      j := j + 1;
    until j > i;
  end;

  if Mod_bit_length mod 8 > 0 then begin // do remaining bits
    i := (Mod_bit_length mod 8) - 1;
    AddToBuffer($3B); // clk data bits out on -ve in +ve clk LSB
    AddToBuffer(i AND $FF);
    // now add the data bits to go out
    AddToBuffer(out_data[j]);
  end;

  // get LastBit
  LastBit := out_data[j];
  j := bit_length mod 8;
  LastBit := LastBit SHR (j - 1);
  Do_Read := true;
  TMS_Clks := JTAG_wiggler_state(state, LastBit, Do_Read); // end it in state passed
  if SndImmediate then
    AddToBuffer($87);
  SendBytes(OutIndex); // send off the command
  // now wait for data
  passed := JTAG_GetData(in_data, Bit_Length, TMS_Clks);
end;


procedure JTAG_sin(jtag_register: shift_register; bit_length: word;
                   var in_data: data_type; state: jtag_state;
                   TDILevel: byte; SndImmediate: boolean);
// JTAG Scan in
var
  i,TMS_Clks: integer;
  Mod_bit_length: word;
  Do_Read: boolean;
  Passed: boolean;
begin
  OutIndex := 0;
  Mod_bit_length := bit_length - 1;
  Do_Read := false;
  if (jtag_register = instruction_register) then
    TMS_Clks := JTAG_wiggler_state(shift_ir, TDILevel, Do_Read)
  else
    TMS_Clks := JTAG_wiggler_state(shift_dr, TDILevel, Do_Read);

  if Mod_bit_length div 8 > 0 then begin // do whole bytes
    i := (Mod_bit_length div 8) - 1;
    AddToBuffer($28); // clk data bytes in LSB +ve clk
    AddToBuffer(i AND $FF);
    AddToBuffer((i DIV 256) AND $FF);
  end;
  if Mod_bit_length mod 8 > 0 then begin // do remaining bits
    i := (Mod_bit_length mod 8) - 1;
    AddToBuffer($2A); // clk data bits in LSB +ve clk
    AddToBuffer(i AND $FF);
  end;
  Do_Read := true;
  TMS_Clks := JTAG_wiggler_state(state, TDILevel, Do_Read); // end it in state passed in
  if SndImmediate then
    AddToBuffer($87);
  SendBytes(OutIndex); // send off the command
  // now wait for data
  passed := JTAG_GetData(in_data, Bit_Length, TMS_Clks);
end;


end.
