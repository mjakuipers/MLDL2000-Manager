unit GlobalConstants;

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
//  GLOBALCONSTANTS.PAS                                                      //
//  Definition of constants and classes for M2kM, USB specific               //
//  Ver  Date     Description                                                //
//  1.00 Nov 2005 First version for release                                  //
//  1.01 Nov 2006 Compiled for Turbo Explorer                                //
//  1.20 Apr 2008 Added various constants to save environment                //
//  1.50 May 2008 Final release                                              //
//  1.60 Jan 2010 Modified for new disassembler                              //
//  1.70 Jul 2010 Adapted for Lazarus                                        //
//  1.80 Feb 2018 restarted development                                      //
//                removed Delphi compatibility                               //
//---------------------------------------------------------------------------//

interface

uses
  Globals;

const

  // General constants for Pin numbering of Ports
  Pin0      =     1;
  Pin1      =     2;
  Pin2      =     4;
  Pin3      =     8;
  Pin4      =    16;
  Pin5      =    32;
  Pin6      =    64;
  Pin7      =   128;


  // The FT2232C is exclusively used in MPSSE Mode as follows:
  //  Bit      Signal    MLDL2000    I/O (for MLDL2000)
  //  ------   -------   --------    ------------------
  //  Bit  0   TCK       X_CLK       Input
  //  Bit  1   TDI/DO    X_DIN       Input
  //  Bit  2   TDO/DI    X_DOUT      Output
  //  Bit  3   TMS       X_STROBE    Input
  //  Bit  4   GPIO_L0   X_EN        Input
  //  Bit  5   GPIO_L1   PE          Input
  //  Bit  6   GPIO_L2   X_DAV       Output
  //  Bit  7   GPIO_L3   X_BSY       Output
  //  Bit  8   GPIO_H0   X_SPARE0    I/O
  //  Bit  9   GPIO_H1   X_SPARE1    I/O
  //  Bit 10   GPIO_H2   X_SPARE2    I/O
  //  Bit 11   GPIO_H3   X_SPARE3    I/O


  X_CLK     =   $01;    // USB Output Data0,    "      "   1, Clock high
  X_DIN     =   $02;    // USB Output Data1,    "      "   1, Data high
  X_DOUT    =   $04;    // USB Input  Data2
  X_STROBE  =   $08;    // USB Output Data3,    "      "   1, Strobe high
  X_EN      =   $10;    // USB Output Data4, default value 1, MLDL Enabled
  X_PE      =   $20;    // USB Output Data5,    "      "   0, Low for normal
  X_DAV     =   $40;    // USB Input  Data6, input for USB device
  X_BSY     =   $80;    // USB Input  Data7, input for USB device
  X_SPARE0  =   $01;    // program as input by default
  X_SPARE1  =   $02;    // program as input by default
  X_SPARE2  =   $04;    // program as input by default
  PWR_CPLD  =   $08;    // program as input by default


  // Mask for programming correct in- and outputs
  CLK_out   = $01;   CLK_hi    = $01;   CLK_lo    = $FE;
  DIN_out   = $02;   DIN_hi    = $02;   DIN_lo    = $FD;
  DOUT_in   = $04;   DOUT_hi   = $04;   DOUT_lo   = $FB;
  STROBE_out= $08;   STROBE_hi = $08;   STROBE_lo = $F7;
  EN_out    = $10;   EN_hi     = $10;   EN_lo     = $EF;
  PE_out    = $20;   PE_hi     = $20;   PE_lo     = $DF;
  DAV_in    = $40;   DAV_hi    = $40;   DAV_lo    = $BF;
  BSY_in    = $80;   BSY_hi    = $80;   BSY_lo    = $7F;
  SPARE0_in = $01;   SPARE0_hi = $01;   SPARE0_lo = $FE;
  SPARE1_in = $02;   SPARE1_hi = $02;   SPARE1_lo = $FD;
  SPARE2_in = $04;   SPARE2_hi = $04;   SPARE2_lo = $FB;
  P_CPLD_in = $08;   P_CPLD_hi = $08;   P_CPLD_lo = $F7;


  BitBangMask      : Byte = X_CLK + X_DIN + X_STROBE + X_EN + X_PE;  // mask to use for MPSSE mode
  BitBangAllInputs : Byte = $FF;  // all inputs, make signals high impedance with weak pullup
  BitBangDefault   : Byte = X_EN + X_CLK + X_DIN + X_STROBE;   // Default value
                     // X_EN     high: MLDL will be running
                     // X_CLK    high: default state
                     // X_DIN    high: default state
                     // X_STROBE high: default state
                     // X_PE     low : I/O port is enabled, JTAG disabled

  MPSSE_MaskLo     = X_CLK + X_DIN + X_STROBE + X_EN + X_PE;    // I/O mask for normal MPSSE Mode
                                                                // listed signals are output
  MPSSE_MaskHi     = PWR_CPLD;                                  // CPLD Power

  MPSSE_Default    = X_EN + X_DIN + X_STROBE;   // Default value
                     // X_EN     high: MLDL will be running
                     // X_CLK    high: default state
                     // X_DIN    high: default state
                     // X_STROBE high: default state
                     // X_PE     low : I/O port is enabled, JTAG disabled

  MPSSE_Start      = X_STROBE;

  JTAGMask         = X_PE;  // All signals inputs except PE
  JTAG_ON          = X_PE;
  JTAG_OFF         = $00;

  USB_JTAGMask     = X_CLK + X_DIN + X_STROBE + X_PE;  // Listed signals are output
  JTAG_Default     = X_PE;
                     // X_EN     low
                     // X_CLK    low
                     // X_DIN    low
                     // X_STROBE low
                     // X_PE     high


  MLDLDisabled     = X_DIN + X_CLK + X_STROBE;  // use for memory I/O


// FT2232C Bit Modes
  FTMode_Reset     = $00;    // Reset FT2232C Mode
  FTMode_AsyncBB   = $01;    // Asynchronous BitBang Mode
  FTMode_MPSSE     = $02;    // MPSSE Mode
  FTMode_SyncBB    = $04;    // Synchronous BitBang Mode
  FTMode_MCUHost   = $08;    // MCU Host Bus Mode
  FTMode_FOpto     = $10;    // Fast Opto-Isolated Serial Mode

  BitBangOn        = FTMode_AsyncBB;
  BitBangOff       = FTMode_Reset;

// MPSSE Modes used for MLDL2000
  MPSSE_19         = $19;    // Write LSB First on -ve edge start clock at 0
                             // $19, LengthL, LengthH, Byte1...ByteN
  MPSSE_28         = $28;    // Read LSB first on +ve edge, no Write
                             // $28, LengthL, LengthH
  MPSSE_80         = $80;    // Set Data Bits Low Byte
                             // $80, Value, Direction (1=output)
  MPSSE_82         = $82;    // Set Data Bits High Byte
                             // $82, Value, Direction (1=output)
  MPSSE_81         = $81;    // Read Data Bits Low Byte
  MPSSE_83         = $83;    // Read Data Bits High Byte
  MPSSE_84         = $84;    // LoopBack Mode On (connect TDI and TDO)
  MPSSE_85         = $85;    // LoopBack Mode Off (disconnect TDI and TDO)
  MPSSE_86         = $86;    // Set TCK Clock divisor
                             // $86, ValueL, ValueH
  MPSSE_87         = $87;    // Send Immediate
  MPSSE_88         = $88;    // Wait on I/O High (GPHIO H1)
  MPSSE_89         = $89;    // Wait on I/O Low (GPHIO H1)
  MPSSE_Invalid    = $BB;    // Invalid Command

  USB_Device_Name  = 'MLDL2000-USB';

  // Actual bitrate is 16*57600=921k, this can be higher??
  //  MLDLBaudRate     = 19200;
  MLDLBaudRate     = 57600;

  // Define the MPSSE TCK/SK bitrate with the following formula:
  //      TCK/SK period = 12MHz / (( 1 +[ (0xValueH * 256) OR 0xValueL] ) * 2)
  //             Value TCK/SK max
  //             0x0000 6 MHz
  //             0x0001 3 MHz
  //             0x0002 2 MHz
  //             0x0003 1.5 MHz
  //             0x0004 1.2 MHz
  //             ...... .......
  //             0xFFFF 91.553 Hz
  //
  //      Let's start here with $00FF, which is 23.437 kHz
//  MPSSE_Speed      = $00FF;
  MPSSE_Speed      = $0010;        // Default speed

  TimeOut_USB          = 1000;     // 1000 ms for timeout on USB read from FIFO
  TimeOut_Flash_Byte   = 1000;     // 1000 ms timout for FLASH Byte programming
  TimeOut_FLASH_Sector = 5000;     // 5000 ms timeout for FLASH Sector Erase
  TimeOut_FLASH_Erase  = 60000;    // One minute timout on FLASH Erase All

  MLDL_BlockSize   = 1024;         // Standard Blocksize for MLDL Communication
  Load_BlockSize   = 512;          // Up/Download BlockSize
  Size_4K          = $1000;        // Size of a 4K Block

  OptBlockRead     = 2000;     // Optimum blocksize to be read from USB


  // Errorcodes of the interface
  Err_No              : Word = $0000;    // No Error
  Err_InvalideHandle  : Word = $0001;    // Invalid Device-Handle
  Err_DeviceNumber    : Word = $0002;    // Invalid Device-Nummer
  Err_IsEnabled       : Word = $0004;    // Control set to Enabled (Change Devicenumber only when Enabled = False)
  Err_IsDisabled      : Word = $0008;    // Control set to Disabled
  Err_DeviceIOCtl     : Word = $0010;    // Function DeviceI0Control returned 'False'
  Err_AI2CNotAck      : Word = $0020;    // I2C-Bus NotAcknowledge after Slaveadress
  Err_DI2CNotAck      : Word = $0040;    // I2C-Bus NotAcknowledge after Data
  Err_I2CBus          : Word = $0080;    // General I2C-BusError
  Err_Array           : Word = $0100;    // ByteArray has invalid size
  Err_InvalideMcMode  : Word = $0200;    // Method not available in current Controller mode
  Err_Pointer         : Word = $0400;    // Invalid RAM-/EEP-Address
  Err_InvalideFile    : Word = $0800;    // Nonexistent file
  Err_InvalidePinState: Word = $1000;    // Pin has different Status
  Err_InvalidePinNo   : Word = $2000;    // Invalid Pinnumber
  Err_EEPType	        : Word = $3000;	   // Invalid EEPROM Type
  Err_Parameter       : Word = $4000;	   // Parameter outside valid range

  // Errorcodes for the MLDL2000
  Err_MLDL_OK         = $0000;    // No Error
  Err_MLDL_Timeout    = $0001;    // Time Out
  Err_MLDL_USBRead    = $0002;    // USB Read Error

  // Hardware definitions
  Range_4k            = $00000FFF; // 4K ROM
  Range_8k            = $00001FFF; // 4K ROM, L8 and U2
  Range_64k           = $0000FFFF; // 64K
  Range_512k          = $0007FFFF; // Full Range of SRAM device
  Range_1M            = $000FFFFF; // Full range of FLASH device
  Range_SRAM          = $0007FFFF; // Full Range of SRAM device
  Range_FLASH         = $000FFFFF; // Full range of FLASH device

  Range_SR            = $0000003F; // Range of Settings Register Set
  SR0_Base            = $000FFF00; // Base of SR Set 0
  SR1_Base            = $000FFF40; // Base of SR Set 1
  SR2_Base            = $000FFF80; // Base of SR Set 2
  SR3_Base            = $000FFFC0; // Base of SR Set 3
  ROMImages_FLASH     = 255;       // Maximum number of ROMs in FLASH


  ROMImages_SRAM_V1   =  64;        // Maximum number of ROMs in SRAM in V1
  ROMImages_SRAM_V15  = 128;        // Maximum number of ROMs in SRAM in V1.9
  ROMImages_SRAM      = 255;        // Maximum number of ROMs in SRAM in V1.9

  AddrBitSR           = $00080000; // if '1' SRAM is addressed
  AddrBitFL           = $FFF7FFFF; // if '0' FLASH is addressed

  Cmd_Write           : Byte = $00;           // Write Command
  Cmd_Read            : Byte = $01;           // Read Command


  // Settings Register definitions
  SR_MASK     : Word = $0FFF;
  SR_EN       : Word = $0800;  // SR Enable bit:                 0 = Enabled
                               //                                1 = Disabled
  SR_IO       : Word = $0400;  // IO or no I/O:                  0 = I/O
                               //                                1 = no I/O
  SR_WP       : Word = $0200;  // Write Protect:                 0 = Write Enable
                               //                                1 = Write Protect
  SR_FL       : Word = $0100;  // FLASH or SRAM:                 0 = FLASH
                               //                                1 = SRAM

  SR_FLASH    : Word = $00FF;  // range for FLASH     addresses A19-A12
  SR_SRAM     : Word = $00FF;  // range for full SRAM addresses A19-A12
  SR_SRAM_H   : Word = $003F;  // for units with standard SRAM  A17-A12
  SR_SRAM_D   : Word = $007F;  // for units with double SRAM    A18-A12


var
  // Global variables for INI File

  // Disassembler section
  PrefComments,                // if true, will generate auto comments in
  PrefAutoLabel,               // if true, will generate auto labels for jump addresses
  PrefMainFrame,               // if true, will use Mainframe label file
  PrefXROM: boolean;           // if true, will use XROM file
  PrefSkipNOPS: boolean;       // if true, will skip multiple NOPS
  PrefLabLst: boolean;         // if true, Labels will be MnemArg2, else this is the Address
  PrefMESLArg: boolean;        // if true, lists MESL and ERROR argument on seperate line
  PrefMnem: integer;           // indicates Mnemonics type:  0 - HP
                               //                            1 - ZenCode
                               //                            2 - Jacobs/De Arras
                               //                            3 - SDK41 JDA
  PrefOneLine: boolean;        // if true, gerenates one line for every word
  PrefSDK41Mode: boolean;      // if true, generates listing for SDK41 re-assembly
  PrefGenXRef: boolean;        // if true, generates a label cross refence table
  PrefAutoFixed: boolean;      // if true, generates labels for XROM and other fixed addresses
  PrefCleanList: boolean;      // if true, does not print address or hexcodes
  
  PrefDisPos: array[1..11] of integer;
  DisOrder: array[1..11] of integer;                               

  // Communication Section
  PrefCommSpeed,               // MPSSE Communication Speed $0000 - $FFFF
  PrefJTAGSpeed: word;         // MPSSE Speed for use with JTAG: $0000 - $FFFF
  PrefUSBTO,                   // regular USB timeout in msec
  PrefFLASHTO,                 // single byte FLASH write timeout in msec
  PrefSecTO,                   // FLASH Sector Erase timeout in msec
  PrefALLTO,                   // FLASH ALl Erase timeout in seconds !
  PrefBlockSize: integer;      // MPSSE Communication block size

  // User Interface Section
  PrefAutoVerify,              // Enable Automatic Verify after read/write
  PrefConfDown,                // Enable Confirm on Download
  PrefConfUp,                  // Enable Confirm on Upload
  PrefRemWindows: boolean;     // Remember Window positions
  PrefAutoFind: boolean;       // Autofind free ROM
  PrefMemType: boolean;        // Default memory type (true = SRAM)


type
  TSetReg = class          // Class for structuring Settings Registers
    Value : word;
    Page  : 0..16;
    Bank  : 0..3;
    Comment: String;
    function Enabled_SR: boolean;   // bit 11 = 1: disabled        false
                                    //        = 0: enabled         true
    function noIO_SR: boolean;      // bit 10 = 1: no I/O          false
                                    //        = 0: I/O             true
    function WEnabled_SR: boolean;  // bit  9 = 1: Write Protected false
                                    //        = 0: Write Enabled   true
    function Flash_SR: boolean;     // bit  8 = 1: in FLASH        true
                                    //        = 0: in SRAM         false
    function IO_SR: boolean;        // bit 10 = 1: no I/O          false
                                    //        = 0: I/O             true
    function FL_Addr: longword;
    function SR_Addr: longword;
    function Get_Addr: longword;
    function FLRomNum: integer;
    function SRRomNum: integer;
    function Get_RomNum: integer;
    procedure SetEnabled(val: boolean);
    procedure SetFlash(val: boolean);
    procedure SetnoIO(val: boolean);
    procedure SetWEnabled(val: boolean);
    procedure SetFL_Addr(Addr: longword);
    procedure SetSR_Addr(Addr: longword);
    procedure SetValue(Val: word);
  end;


implementation

  function TSetReg.Enabled_SR: boolean;      // bit 11 = 1: disabled        false
  // Returns TRUE if this SR is enabled                = 0: enabled         true
  begin
    Result := (Value and SR_EN) = $0000;
  end;

  function TSetReg.Flash_SR: boolean;        // bit  8 = 1: SRAM            false
  // Returns TRUE is the SR points to FLASH            = 0: FLASH           true
  begin
    Result := (Value and SR_FL) = $0000;
  end;

  function TSetReg.noIO_SR: boolean;         // bit 10 = 1: no I/O          false
  // Returns FALSE is SR points to I/O                 = 0: I/O             true
  begin
    Result := (Value and SR_IO) = SR_IO;
  end;

  function TSetReg.IO_SR: boolean;           // bit 10 = 1: no I/O          true
  // Returns TRUE is SR points to I/O                  = 0: I/O             false
  begin
    Result := (Value and SR_IO) = $0000;
  end;

  function TSetReg.WEnabled_SR: boolean;     // bit  9 = 1: Write Protected false
  // Returns TRUE is image is Write Enabled            = 0: Write Enabled   true
  begin
    Result := (Value and SR_WP) = $0000;
  end;

  function TSetReg.FL_Addr: longword;
  // Returns the Address of the ROM Image in FLASH
  begin
    Result := (SR_FLASH and Value) shl 12;
  end;

  function TSetReg.SR_Addr: longword;
  // Returns the Address of the ROM Image in SRAM
  begin
    Result := (SR_SRAM and Value) shl 12;
  end;

  function TSetReg.Get_Addr: longword;
  //  Returns the Address of the ROM Image
  begin
    if (Value and SR_FL) = $0000 then
      // ROM Image is in FLASH
      Result := (SR_FLASH and Value) shl 12
    else
      // ROM Image is in SRAM
      Result := (SR_SRAM and Value) shl 12;
  end;

  function TSetReg.FLRomNum: integer;
  // Returns the ROM Number when in FLASH
  begin
    Result := SR_FLASH and Value;
  end;

  function TSetReg.SRRomNum: integer;
  // Returns the ROM Number when in SRAM
  begin
    Result := SR_SRAM and Value;
  end;

  function TSetReg.Get_RomNum: integer;
  begin
    if (Value and SR_FL) = $0000 then
      Result := SR_FLASH and Value        // ROM Image is in FLASH
    else
      Result := SR_SRAM and Value;        // ROM Image is in SRAM
  end;

  procedure TSetReg.SetEnabled(val: boolean);
  begin
    if Val then
      Value := Value and (not SR_EN)      // set Enabled, bit 11 := 0
    else
      Value := Value or SR_EN;            // set Disabled, bit 11 := 1
  end;

  procedure TSetReg.SetFlash(val: boolean);
  begin
    if Val then
      Value := Value and (not SR_FL)      // set FLASH, bit 8 := 0
    else
      Value := Value or SR_FL;            // set SRAM, bit 8 := 1
  end;

  procedure TSetReg.SetnoIO(val: boolean);
  begin
    if Val then
      Value := Value or SR_IO             // set no I/O, bit 10 := 1
    else
      Value := Value and (not SR_IO);     // set I/O, bit 10 := 0
  end;


  procedure TSetReg.SetWEnabled(val: boolean);
  begin
    if Val then
      Value := Value and (not SR_WP)      // set Enabled, bit 11 := 0
    else
      Value := Value or SR_WP;            // set Protected, bit 11 := 1
  end;

  procedure TSetReg.SetFL_Addr(Addr: longword);
  begin
    Value := Value and (not SR_FLASH);   // clear address bits
    Value := Value or (Addr and SR_FLASH);
  end;

  procedure TSetReg.SetSR_Addr(Addr: longword);
  begin
    Value := Value and (not SR_SRAM);   // clear address bits
    Value := Value or (Addr and SR_SRAM);
  end;

  procedure TSetReg.SetValue(Val: word);
  begin
    Value := Val and $0FFF;   // Set value
  end;

end.
