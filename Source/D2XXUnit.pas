unit D2XXUnit;

//---------------------------------------------------------------------------//
//    Copyright (c) 2008  Meindert Kuipers, Netherlands                      //
//    meindert@kuiprs.nl                 www.kuiprs.nl                       //
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
//  D2XXUNIT.PAS                                                             //
//  Interface Definition for D2XX DLL, based on FTDI example                 //
//  Ver  Date     Description                                                //
//  1.00 Nov 2005 First version for release                                  //
//  1.01 Nov 2006 Compiled for Turbo Explorer                                //
//  1.50 May 2008 Final release                                              //
//  1.70 Jul 2010 Adapted for Lazarus                                        //
//  1.80 Feb 2018 restarted development                                      //
//                removed Delphi compatibility                               //
//---------------------------------------------------------------------------//


interface

Uses
  LCLIntf,Forms,Dialogs;

Type
  FT_Result = Integer;
  // structure to hold program data for FT_Program function
  TFT_program_data = record
    VendorId         : word;	 // 0x0403
    ProductId        : Word;	 // 0x6001
    Manufacturer     : Pointer;	 // "FTDI"
    ManufacturerId   : Pointer;  // "FT"
    Description      : Pointer;  // Devive Description
    SerialNumber     : Pointer;  // Device Serial Number
    MaxPower         : word;	 // 0 < MaxPower <= 500
    PnP              : word;	 // 0 = disabled, 1 = enabled
    SelfPowered      : word;	 // 0 = bus powered, 1 = self powered
    RemoteWakeup     : word;	 // 0 = not capable, 1 = capable

    // Rev4 extensions
    Rev4             : byte;	 // non-zero if Rev4 chip, zero otherwise
    IsoIn            : byte;	 // non-zero if in endpoint is isochronous
    IsoOut           : byte;	 // non-zero if out endpoint is isochronous
    PullDownEnable   : byte;	 // non-zero if pull down enabled
    SerNumEnable     : byte;	 // non-zero if serial number to be used
    USBVersionEnable : byte;   // non-zero if chip uses USBVersion
    USBVersion       : word;	 // BCD (0x0200 => USB2)
  end;


// Exported Functions -- Standard functions from example
Function GetFTDeviceCount: FT_Result;
Function GetFTDeviceDescription(DeviceIndex: DWord): FT_Result;
Function GetFTDeviceSerialNo(DeviceIndex: DWord): FT_Result;
Function Open_USB_Device: FT_Result;
Function Close_USB_Device: FT_Result;
Function Write_USB_Device_Buffer(Write_Count: Integer): Integer;
Function Read_USB_Device_Buffer(Read_Count: Integer): Integer;
Function Reset_USB_Device: FT_Result;
Function Purge_USB_Device_Out: FT_Result;
Function Purge_USB_Device_In: FT_Result;
Function Purge_USB_Device: FT_Result;
Function Set_USB_Device_RTS: FT_Result;
Function Clr_USB_Device_RTS: FT_Result;
Function Set_USB_Device_DTR: FT_Result;
Function Clr_USB_Device_DTR: FT_Result;
Function Set_USB_Device_BaudRate: FT_Result;
Function Set_USB_Device_DataCharacteristics: FT_Result;
Function Set_USB_Device_FlowControl: FT_Result;
Function Get_USB_Device_ModemStatus: FT_Result;
Function Set_USB_Device_Chars: FT_Result;
Function Set_USB_Device_TimeOuts(ReadTimeOut, WriteTimeOut: DWord): FT_Result;
Function Get_USB_Device_QueueStatus: FT_Result;
Function Open_USB_Device_By_Serial_Number(Serial_Number: string): FT_Result;
Function Open_USB_Device_By_Device_Description(Device_Description: string): FT_Result;
Function Set_USB_Device_LatencyTimer(ucLatency: Byte): FT_Result;
//Function Get_USB_Device_LatencyTimer(var Latency: byte): FT_Result;
Function Get_USB_Device_LatencyTimer: FT_Result;
Function Set_USB_Device_BitMode(ucMask, ucEnable: Byte): FT_Result;
Function Get_USB_Device_BitMode(var ucMode: Byte): FT_Result;
Function Set_USB_Parameters(InSize, OutSize: Dword): FT_Result ;
function USB_FT_EE_Read : FT_Result;


Var
  FT_HANDLE              : DWord = 0;       // Port Handle Returned by the Open Function
  PV_Device              : DWord = 0;       // Used to handle multiple device instances in future versions

  // Holding Variables for the current settings
  // Can be configured visually using the CFGUnit Unit or manually before calling SetUp_USB_Device
  FT_Current_Baud        : Dword;
  FT_Current_DataBits    : Byte;
  FT_Current_StopBits    : Byte;
  FT_Current_Parity      : Byte;
  FT_Current_FlowControl : Word;
  FT_RTS_On              : Boolean;
  FT_DTR_On              : Boolean;
  FT_Event_On            : Boolean;
  FT_Error_On            : Boolean;
  FT_XON_Value           : Byte = $11;
  FT_XOFF_Value          : Byte = $13;
  FT_EVENT_Value         : Byte = $0;
  FT_ERROR_Value         : Byte = $0;
  FT_SetupError          : Boolean;         // Used by CFGUnit to flag a bad value
  FT_Modem_Status        : DWord;           // Used to Return the current Modem Status
  FT_Q_Bytes             : DWord;           // Used to return the number of bytes pending in the Rx Buffer Queue
  FT_Enable_Error_Report : Boolean = True;  // Used to Enable / Disable the Error Report Dialog
  FT_LatencyRd           : Byte;            // Deposit for Get latency time

  EEDataBuffer           : TFT_Program_Data;
  Manufacturer           : array [0..63] of byte;	 // "FTDI"
  ManufacturerId         : array [0..63] of byte;  // "FT"
  Description            : array [0..63] of byte;  //
  SerialNumber           : array [0..63] of byte;  //
  UserData               : array [0..63] of byte;

Const
  // FT_Result Values
  FT_OK                          = 0;
  FT_INVALID_HANDLE              = 1;
  FT_DEVICE_NOT_FOUND            = 2;
  FT_DEVICE_NOT_OPENED           = 3;
  FT_IO_ERROR                    = 4;
  FT_INSUFFICIENT_RESOURCES      = 5;
  FT_INVALID_PARAMETER           = 6;
  FT_SUCCESS                     = FT_OK;
  FT_INVALID_BAUD_RATE           = 7;
  FT_DEVICE_NOT_OPENED_FOR_ERASE = 8;
  FT_DEVICE_NOT_OPENED_FOR_WRITE = 9;
  FT_FAILED_TO_WRITE_DEVICE      = 10;
  FT_EEPROM_READ_FAILED          = 11;
  FT_EEPROM_WRITE_FAILED         = 12;
  FT_EEPROM_ERASE_FAILED         = 13;
  FT_EEPROM_NOT_PRESENT          = 14;
  FT_EEPROM_NOT_PROGRAMMED       = 15;
  FT_INVALID_ARGS                = 16;
  FT_OTHER_ERROR                 = 17;

  // FT_Open_Ex Flags
  FT_OPEN_BY_SERIAL_NUMBER       = 1;
  FT_OPEN_BY_DESCRIPTION         = 2;

  // FT_List_Devices Flags
  FT_LIST_NUMBER_ONLY            = $80000000;
  FT_LIST_BY_INDEX               = $40000000;
  FT_LIST_ALL                    = $20000000;

  // Baud Rate Selection
  FT_BAUD_300                    = 300;
  FT_BAUD_600                    = 600;
  FT_BAUD_1200                   = 1200;
  FT_BAUD_2400                   = 2400;
  FT_BAUD_4800                   = 4800;
  FT_BAUD_9600                   = 9600;
  FT_BAUD_14400                  = 14400;
  FT_BAUD_19200                  = 19200;
  FT_BAUD_38400                  = 38400;
  FT_BAUD_57600                  = 57600;
  FT_BAUD_115200                 = 115200;
  FT_BAUD_230400                 = 230400;
  FT_BAUD_460800                 = 460800;
  FT_BAUD_921600                 = 921600;

  // Data Bits Selection
  FT_DATA_BITS_7                 = 7;
  FT_DATA_BITS_8                 = 8;

  // Stop Bits Selection
  FT_STOP_BITS_1                 = 0;
  FT_STOP_BITS_2                 = 2;

  // Parity Selection
  FT_PARITY_NONE                 = 0;
  FT_PARITY_ODD                  = 1;
  FT_PARITY_EVEN                 = 2;
  FT_PARITY_MARK                 = 3;
  FT_PARITY_SPACE                = 4;

  // Flow Control Selection
  FT_FLOW_NONE                   = $0000;
  FT_FLOW_RTS_CTS                = $0100;
  FT_FLOW_DTR_DSR                = $0200;
  FT_FLOW_XON_XOFF               = $0400;

  // Purge Commands
  FT_PURGE_RX                    = 1;
  FT_PURGE_TX                    = 2;

  // IO Buffer Sizes
  FT_In_Buffer_Size              = $100000;    // 1024k
  //FT_In_Buffer_Size            = $10000;     // 64k
  //FT_In_Buffer_Size            = $8000;      // 32k
  FT_In_Buffer_Index             = FT_In_Buffer_Size - 1;

  FT_Out_Buffer_Size             = $10000;     // 64k
  //FT_Out_Buffer_Size             = $8000;      // 32k
  FT_Out_Buffer_Index            = FT_Out_Buffer_Size - 1;

  // DLL Name
{$IFDEF win64}
  FT_DLL_Name                    = 'ftd2xx.dll';
{$ENDIF}

{$IFDEF linux}
  FT_DLL_Name                    = 'ftd2xx';
{$ENDIF}




var
  // Declare Input and Output Buffers
  FT_In_Buffer            : Array[0..FT_In_Buffer_Index] of byte;
  FT_Out_Buffer           : Array[0..FT_Out_Buffer_Index] of byte;

  // A variable used to detect time-outs. Attach a timer to the main project form
  // which decrements this every 10mS if FT_TimeOut_Count <> 0
  FT_TimeOut_Count        : Integer = 0;
  // Used to determine how many bytes were actually received by FT_Read_Device_All
  // in the case of a time-out
  FT_All_Bytes_Received   : Integer = 0;
  FT_IO_Status            : Ft_Result = FT_OK;
  // Used By FT_ListDevices
  FT_Device_Count         : DWord;
  FT_Device_String_Buffer : Array [1..50] of Char;
  FT_Device_String        : String;


implementation

function FT_Open(PVDevice:Integer; ftHandle:Pointer ) : FT_Result ; stdcall ; External FT_DLL_Name name 'FT_Open';
function FT_Close(ftHandle:Dword): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_Close';
function FT_Read(ftHandle:Dword; FTInBuf: Pointer; BufferSize: LongInt; ResultPtr: Pointer ): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_Read';
function FT_Write(ftHandle:Dword; FTOutBuf: Pointer; BufferSize: LongInt; ResultPtr: Pointer ): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_Write';
function FT_SetBaudRate(ftHandle:Dword;BaudRate:DWord): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_SetBaudRate';
function FT_SetDataCharacteristics(ftHandle:Dword;WordLength,StopBits,Parity:Byte): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_SetDataCharacteristics';
function FT_SetFlowControl(ftHandle:Dword;FlowControl:Word;XonChar,XoffChar:Byte): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_SetFlowControl';
function FT_ResetDevice(ftHandle:Dword): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_ResetDevice';
function FT_SetDtr(ftHandle:Dword): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_SetDtr';
function FT_ClrDtr(ftHandle:Dword): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_ClrDtr';
function FT_SetRts(ftHandle:Dword): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_SetRts';
function FT_ClrRts(ftHandle:Dword): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_ClrRts';
function FT_GetModemStatus(ftHandle:Dword;ModemStatus:Pointer): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_GetModemStatus';
function FT_SetChars(ftHandle:Dword;EventChar,EventCharEnabled,ErrorChar,ErrorCharEnabled: Byte): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_SetChars';
function FT_Purge(ftHandle:Dword;Mask:Dword): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_Purge';
function FT_SetTimeouts(ftHandle:Dword;ReadTimeout,WriteTimeout:Dword): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_SetTimeouts';
function FT_GetQueueStatus(ftHandle:Dword;RxBytes:Pointer): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_GetQueueStatus';
function FT_GetNumDevices(pvArg1:Pointer;pvArg2:Pointer;dwFlags:Dword): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_ListDevices';
function FT_ListDevices(pvArg1:Dword;pvArg2:Pointer;dwFlags:Dword): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_ListDevices';
function FT_OpenEx(pvArg1:Pointer;dwFlags:Dword;ftHandle:Pointer): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_OpenEx';
function FT_SetLatencyTimer(ftHandle:Dword;ucLatency: Byte): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_SetLatencyTimer';
function FT_GetLatencyTimer(ftHandle:Dword;pucLatency: Pointer): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_GetLatencyTimer';
function FT_SetBitMode(ftHandle:Dword;ucMask,ucEnable: Byte): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_SetBitMode';
function FT_GetBitMode(ftHandle:Dword;pucMode: Pointer): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_GetBitMode';
function FT_SetUSBParameters(ftHandle:Dword;InSize,OutSize: Dword): FT_Result ; stdcall ; External FT_DLL_Name name 'FT_SetUSBParameters';
function FT_EE_Read(ftHandle:DWord;pEEData: Pointer): FT_Result; stdcall ; External FT_DLL_Name name 'FT_EE_Read';
function FT_EE_Program(ftHandle: DWord;pEEData: Pointer): FT_Result; stdcall ; External FT_DLL_Name name 'FT_EE_Program';
function FT_EE_UASize(ftHandle: DWord;pUASize: Pointer): FT_Result; stdcall ; External FT_DLL_Name name 'FT_EE_UASize';
function FT_EE_UAWrite(ftHandle: DWord;pUCHAR:Pointer;DLen:DWord): FT_Result; stdcall ; External FT_DLL_Name name 'FT_EE_UAWrite';
function FT_EE_UARead(ftHandle: DWord;pData:Pointer;DLen:DWord;BytesRead:Pointer): FT_Result; stdcall ; External FT_DLL_Name name 'FT_EE_UARead';



Procedure FT_Error_Report(ErrStr: String; PortStatus: Integer);
Var
  Str: String;
Begin
  If Not FT_Enable_Error_Report then Exit;
  If PortStatus = FT_OK then Exit;
  Case PortStatus of
    FT_INVALID_HANDLE              : Str := ErrStr+' - Invalid Handle...';
    FT_DEVICE_NOT_FOUND            : Str := ErrStr+' - Device Not Found....';
    FT_DEVICE_NOT_OPENED           : Str := ErrStr+' - Device Not Opened...';
    FT_IO_ERROR                    : Str := ErrStr+' - General IO Error...';
    FT_INSUFFICIENT_RESOURCES      : Str := ErrStr+' - Insufficient Resources...';
    FT_INVALID_PARAMETER           : Str := ErrStr+' - Invalid Parameter ...';
    FT_INVALID_BAUD_RATE           : Str := ErrStr+' - Invalid baud rate ...';
    FT_DEVICE_NOT_OPENED_FOR_ERASE : Str := ErrStr+' Device not opened for erase  ...';
    FT_DEVICE_NOT_OPENED_FOR_WRITE : Str := ErrStr+' Device not opened for write ...';
    FT_FAILED_TO_WRITE_DEVICE      : Str := ErrStr+' - Failed to write ...';
    FT_EEPROM_READ_FAILED          : Str := ErrStr+' - EEPROM read Failed ...';
    FT_EEPROM_WRITE_FAILED         : Str := ErrStr+' - EEPROM write failed ...';
    FT_EEPROM_ERASE_FAILED         : Str := ErrStr+' - EEPROM erase failed ...';
    FT_EEPROM_NOT_PRESENT          : Str := ErrStr+' - EEPROM not present ...';
    FT_EEPROM_NOT_PROGRAMMED       : Str := ErrStr+' - EEPROM not programmed ...';
    FT_INVALID_ARGS                : Str := ErrStr+' - invalid Arguments ...';
    FT_OTHER_ERROR                 : Str := ErrStr+' - other error  ...';
  End;
  MessageDlg(Str, mtError, [mbOk], 0);
End;


Function GetDeviceString: String;
// Return proper Delphi string
Var
  I: Integer;
Begin
  Result := '';
  I := 1;
  FT_Device_String_Buffer[50] := Chr(0); // Just in case !
  while FT_Device_String_Buffer[I] <> Chr(0) do Begin
    Result := Result + FT_Device_String_Buffer[I];
    Inc(I);
  End;
End;


Procedure SetDeviceString ( S : String );
// Create null-terminated string
Var
  I, L: Integer;
Begin
  FT_Device_String_Buffer[1] := Chr(0);
  L := Length(S);
  if L > 49 then L := 49;
  if L = 0 then Exit;
  for I := 1 to L do FT_Device_String_Buffer[I] := S[I];
  FT_Device_String_Buffer[L+1] := Chr(0);
End;


Function GetFTDeviceCount: FT_Result;
// Wrapper for FT_ListDevices, FT_LIST_NUMBER_ONLY
Begin
  Result := FT_GetNumDevices(@FT_Device_Count, Nil, FT_LIST_NUMBER_ONLY);
  If Result <> FT_OK then FT_Error_Report('GetFTDeviceCount', Result);
End;


Function GetFTDeviceSerialNo(DeviceIndex: DWord): FT_Result;
// Wrapper for FT_ListDevices, by serial number
Begin
  Result := FT_ListDevices(DeviceIndex, @FT_Device_String_Buffer, (FT_OPEN_BY_SERIAL_NUMBER or FT_LIST_BY_INDEX));
  If Result = FT_OK then FT_Device_String := GetDeviceString;
  If Result <> FT_OK then FT_Error_Report('GetFTDeviceSerialNo', Result);
End;


Function GetFTDeviceDescription(DeviceIndex: DWord): FT_Result;
// Wrapper for FT_ListDevices, by description
Begin
  Result := FT_ListDevices(DeviceIndex, @FT_Device_String_Buffer, (FT_OPEN_BY_DESCRIPTION or FT_LIST_BY_INDEX));
  If Result = FT_OK then FT_Device_String := GetDeviceString;
  If Result <> FT_OK then FT_Error_Report('GetFTDeviceDescription', Result);
End;


Function Open_USB_Device: FT_Result;
// Wrapper for FT_Open
Begin
  Result := FT_Open(PV_Device,@FT_Handle);
  If Result <> FT_OK then FT_Error_Report('FT_Open', Result);
End;


Function Open_USB_Device_By_Serial_Number(Serial_Number: string ): FT_Result;
// Wrapper for FT_OpenEx, by Serial Number
Begin
  SetDeviceString(Serial_Number);
  Result := FT_OpenEx(@FT_Device_String_Buffer, FT_OPEN_BY_SERIAL_NUMBER, @FT_Handle);
  If Result <> FT_OK then FT_Error_Report('Open_USB_Device_By_Serial_Number', Result);
End;


Function Open_USB_Device_By_Device_Description( Device_Description: string ): FT_Result;
// Wrapper for OpenEx, by Description
Begin
  SetDeviceString(Device_Description);
  Result := FT_OpenEx(@FT_Device_String_Buffer, FT_OPEN_BY_DESCRIPTION, @FT_Handle);
  If Result <> FT_OK then FT_Error_Report('Open_USB_Device_By_Device_Description', Result);
End;


Function Close_USB_Device: FT_Result;
// Wrapper for FT_Close
Begin
  Result :=  FT_Close(FT_Handle);
  If Result <> FT_OK then FT_Error_Report('FT_Close', Result);
End;


Function Reset_USB_Device: FT_Result;
// Wrapper for FT_ResetDevice;
Begin
  Result :=  FT_ResetDevice(FT_Handle);
  If Result <> FT_OK then FT_Error_Report('FT_ResetDevice', Result);
End;


Function Purge_USB_Device_Out: FT_Result;
// Wrapper for FT_Purge, RX FIFO
Begin
  Result :=  FT_Purge(FT_Handle,FT_PURGE_RX);
  If Result <> FT_OK then FT_Error_Report('FT_Purge RX', Result);
End;


Function Purge_USB_Device_In: FT_Result;
// Wrapper for FT_Purge, TX FIFO
Begin
  Result :=  FT_Purge(FT_Handle,FT_PURGE_TX);
  If Result <> FT_OK then FT_Error_Report('FT_Purge TX', Result);
End;


Function Purge_USB_Device: FT_Result;
// Wrapper for FT_Purge, TX FIFO and RX FIFO
Begin
  Result :=  FT_Purge(FT_Handle,FT_PURGE_TX + FT_PURGE_RX);
  If Result <> FT_OK then FT_Error_Report('FT_Purge TX+RX', Result);
End;



Function Set_USB_Device_RTS: FT_Result;
// Wrapper for FT_SetRTS
Begin
  Result :=  FT_SetRTS(FT_Handle);
  If Result <> FT_OK then FT_Error_Report('FT_SetRTS', Result);
End;


Function Clr_USB_Device_RTS: FT_Result;
// Wrapper for FT_ClrRTS
Begin
  Result :=  FT_ClrRTS(FT_Handle);
  If Result <> FT_OK then FT_Error_Report('FT_ClrRTS', Result);
End;


Function Set_USB_Device_DTR: FT_Result;
// Wrapper for FT_SetDTR
Begin
  Result :=  FT_SetDTR(FT_Handle);
  If Result <> FT_OK then FT_Error_Report('FT_SetDTR', Result);
End;


Function Clr_USB_Device_DTR: FT_Result;
// Wrapper for FT_ClrRTS
Begin
  Result :=  FT_ClrDTR(FT_Handle);
  If Result <> FT_OK then FT_Error_Report('FT_ClrDTR', Result);
End;


Function Set_USB_Device_BaudRate: FT_Result;
// Wrapper for FT_SetBaudRate
Begin
  Result :=  FT_SetBaudRate(FT_Handle, FT_Current_Baud);
  If Result <> FT_OK then FT_Error_Report('FT_SetBaudRate', Result);
End;


Function Set_USB_Device_DataCharacteristics: FT_Result;
// Wrapper for FT_SetDataCharacteristics
Begin
  Result :=  FT_SetDataCharacteristics(FT_Handle, FT_Current_DataBits, FT_Current_StopBits, FT_Current_Parity);
  If Result <> FT_OK then FT_Error_Report('FT_SetDataCharacteristics', Result);
End;


Function Set_USB_Device_FlowControl: FT_Result;
// Wrapper for FT_SetFlowControl
Begin
  Result :=  FT_SetFlowControl(FT_Handle, FT_Current_FlowControl, FT_XON_Value, FT_XOFF_Value);
  If Result <> FT_OK then FT_Error_Report('FT_SetFlowControl', Result);
End;


Function Get_USB_Device_ModemStatus: FT_Result;
// Wrapper for FT_GetModemStatus
Begin
  Result :=  FT_GetModemStatus(FT_Handle, @FT_Modem_Status);
  If Result <> FT_OK then FT_Error_Report('FT_GetModemStatus', Result);
End;


Function Set_USB_Device_Chars: FT_Result;
// Wrapper for FT_SetChars
Var
  Events_On, Errors_On: Byte;
Begin
  If FT_Event_On then Events_On := 1 else Events_On := 0;
  If FT_Error_On then Errors_On := 1 else Errors_On := 0;
  Result :=  FT_SetChars(FT_Handle, FT_EVENT_Value, Events_On, FT_ERROR_Value, Errors_On);
  If Result <> FT_OK then FT_Error_Report('FT_SetChars', Result);
End;


Function Set_USB_Device_TimeOuts(ReadTimeOut, WriteTimeOut: DWord): FT_Result;
// Wrapper for FT_SetTimeouts
Begin
  Result :=  FT_SetTimeouts(FT_Handle, ReadTimeout, WriteTimeout);
  If Result <> FT_OK then FT_Error_Report('FT_SetTimeouts', Result);
End;


Function Get_USB_Device_QueueStatus: FT_Result;
// Wrapper for FT_GetQueueStatus
Begin
  Result :=  FT_GetQueueStatus(FT_Handle, @FT_Q_Bytes);
  If Result <> FT_OK then FT_Error_Report('FT_GetQueueStatus', Result);
End;


function Write_USB_Device_Buffer(Write_Count: Integer ): Integer;
// Writes Write_Count Bytes from FT_Out_Buffer to the USB device
// Function returns the number of bytes actually sent
// In this example, Write_Count should be 32k bytes max
Var
  Write_Result: Integer;
Begin
  FT_IO_Status := FT_Write(FT_Handle, @FT_Out_Buffer, Write_Count, @Write_Result);
  If FT_IO_Status <> FT_OK then FT_Error_Report('FT_Write', FT_IO_Status);
  Result := Write_Result;
End;


function Read_USB_Device_Buffer(Read_Count: Integer ): Integer;
// Reads Read_Count Bytes ( or less ) from the USB device to the FT_In_Buffer
// Function returns the number of bytes actually received  which may range from zero
// to the actual number of bytes requested, depending on how many have been received
// at the time of the request + the read timeout value.
Var
  Read_Result : Integer;
Begin
  if (read_count = 1) then begin
    read_result := read_count;
  end;
  FT_IO_Status := FT_Read(FT_Handle, @FT_In_Buffer, Read_Count, @Read_Result);
  If FT_IO_Status <> FT_OK then FT_Error_Report('FT_Read', FT_IO_Status);
  Result := Read_Result;
End;


Function Set_USB_Device_LatencyTimer(ucLatency: Byte): FT_Result;
// Wrapper for FT_SetLatencyTimer
Begin
  Set_USB_Device_LatencyTimer :=  FT_SetLatencyTimer(FT_Handle, ucLatency);
End;


Function Get_USB_Device_LatencyTimer: FT_Result;
// Wrapper for FT_GetLatencyTimer
Begin
  Get_USB_Device_LatencyTimer := FT_GetLatencyTimer(FT_Handle, @FT_LatencyRd);
End;


Function Set_USB_Device_BitMode(ucMask, ucEnable: Byte): FT_Result;
// Wrapper for FT_SetBitMode
Begin
  Set_USB_Device_BitMode := FT_SetBitMode(FT_Handle, ucMask, ucEnable);
End;


Function Get_USB_Device_BitMode(var ucMode: Byte): FT_Result;
// Wrapper for FT_GetBitMode
Begin
  Get_USB_Device_BitMode := FT_GetBitMode(FT_Handle, @ucMode);
End;


Function Set_USB_Parameters(InSize, OutSize: Dword): FT_Result;
// Wrapper for FT_SetUSBParameters
Begin
  Result :=  FT_SetUSBParameters(FT_Handle, InSize, OutSize);
  If Result <> FT_OK then FT_Error_Report('FT_SetUSBParameters ', Result);
End;


function USB_FT_EE_Read: FT_Result;
// Read EEPROM
begin
  EEDataBuffer.VendorId :=0;
  EEDataBuffer.ProductId := 0;
  EEDataBuffer.Manufacturer := @Manufacturer;
  EEDataBuffer.ManufacturerId := @ManufacturerId;
  EEDataBuffer.Description := @Description;
  EEDataBuffer.SerialNumber := @SerialNumber;
  EEDataBuffer.MaxPower := 0;
  EEDataBuffer.PnP := 0;
  EEDataBuffer.SelfPowered := 0;
  EEDataBuffer.RemoteWakeup := 0;
  EEDataBuffer.Rev4 := 0;
  EEDataBuffer.IsoIn := 0;
  EEDataBuffer.IsoOut := 0;
  EEDataBuffer.PullDownEnable := 0;
  EEDataBuffer.SerNumEnable := 0;
  EEDataBuffer.USBVersionEnable := 0;
  EEDataBuffer.USBVersion := 0;
  Result :=  FT_EE_Read(FT_Handle, @EEDataBuffer);
  If Result <> FT_OK then FT_Error_Report('FT_EE_Read ', Result);
end;


// Function below to be implemented (or not)

//function FT_EE_Program(ftHandle : DWord;pEEData : Pointer) : FT_Result;
//function FT_EE_UASize(ftHandle : DWord;pUASize : Pointer) : FT_Result;
//function FT_EE_UAWrite(ftHandle : DWord;pUCHAR:Pointer;DLen:DWord) : FT_Result;
//function FT_EE_UARead(ftHandle : DWord;pData:Pointer;DLen:DWord;BytesRead:Pointer) : FT_Result;

end.
