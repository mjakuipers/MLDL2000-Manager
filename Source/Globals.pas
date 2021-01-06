unit Globals;

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
//  GLOBALS.PAS                                                              //
//  Various global constants and variables, see also GLOBALCONSTANTS.PAS     //
//  Ver  Date     Description                                                //
//  1.00 Nov 2005 First version for release                                  //
//  1.01 Nov 2006 Compiled for Turbo Explorer                                //
//                Added stuff for correct FLASH Erase bug                    //
//                Corrected typo                                             //
//                Added FLASH Type                                           //
//  1.20 Apr 2008 Added MODFile support                                      //
//  1.50 May 2008 Final release                                              //
//  1.60 Jan 2010 Modified for new disassembler                              //
//  1.70 Jul 2010 Adapted for Lazarus                                        //
//  1.80 Feb 2018 restarted development                                      //
//                removed Delphi compatibility                               //
//---------------------------------------------------------------------------//

// Definitions of MOD file format from V41 by Warren Furlow                  //

interface

const
  SQuote = '''';   // Single Quote, apostrophe
  DQuote = '"';    // Double quote
  HexSet: set of char = ['0'..'9', 'A'..'F'];
  HexChr: set of char = ['A'..'F'];
  DecSet: set of char = ['0'..'9'];
  BinSet: set of char = ['0'..'1'];
  QuoteSet: set of char = [SQuote, DQuote];

  Space  = ' ';
  Space2 = '  ';
  CR = ^M;
  LF = ^J;
  NL = '^M^J';
  TAB = #09;
  EmptyData  = ' ... ';
  EmptyData2 = ' ... ... ';
  EmptyData3 = ' ... ... ... ';


type
  UserType = record
    Name: string[10];
    Typ: Integer;
  end;
  HexArr = array[0..$FF] of string[10];
  TypArr = array[0..$FF] of integer;

  String2   = string[2];
  String3   = string[3];
  String4   = string[4];
  String5   = string[5];
  String6   = string[6];
  String8   = string[8];
  String10  = string[10];
  String12  = string[12];
  String16  = string[16];
  String80  = string[80];
  AnyString = string;

const
  RomSize   = $1FFF; // 4K Size of ROM
  Rom4KSize = $0FFF;
  Rom8KSize = $1FFF; // size of 8K ROM
  SRSize    = $003F; // 64 SR entries

type
  RomPage    = array[$0000..RomSize] of word;
//  Rom8KPage  = array[$0000..Rom8KSize] of Word;
  SRPage     = array[$0000..SRSize]  of word;
  SRComArr   = array[$0000..SRSize] of String;
  SRComPgArr = array[0..15] of String;
  FLASHType  = (FLASHUNKNOWN, TOPBOOT, BOTTOMBOOT);     // FLASH memory boot block
  SRAMSize   = (SRAMUNKNOWN, REGULAR, DOUBLE);  // SRAM size

  bfile = file of byte;



const
// Values returned by procedure DevType:
  FlashTypeTop    = $22C4;    // $22C4  Top Boot Block Device
  FlashTypeBottom = $2249;    // $2249  Bottom Boot Block Device

  // constant definitions for BASE64 decoding

  // coding array

  Base64Code = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrtsuvwxyz0123456789+/';

  Base64Decode : array[43..122] of integer =


  //   +               /   0   1   2   3   4   5   6   7   8   9
  //  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58
    ( 62, -1, -1, -1, 63, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1,

  //           =               A   B   C   D   E   F   G   H   I   J
  //  59  60  61  62  63  64  65  66  67  68  69  70  71  72  72  74
      -1, -1, 64, -1, -1, -1,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,

  //   K   L   M   N   O   P   Q   R   S   T   U   V   W   X   Y   Z
  //  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
      10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,

  //                           a   b   c   d   e   f   g   h   i   j
  //  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106
      -1, -1, -1, -1, -1, -1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,

  //   k   l   m   n   o   p   q   r   s   t   u   v   w   x   y   z
  // 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122
      36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51  );



var
  I_Error                 : word;          // Global variable for USB Error Status
  MLDL_Error              : word;          // Global variable for MLDL Errors
  MLDL_Enable_Error_Report : boolean;
  FLASH_TP                : FLASHType = FLASHUNKNOWN;
  SRAM_Sz                 : SRAMSize  = SRAMUNKNOWN;
  Firmware_ID             : string = '';
  MLDL_Is_Connected       : boolean;

  SRAM_Pages              : integer = 63;  // actual number of SRAM pages

  PageArray               : ROMPage;       // Stores current ROMPage
  MODArray                : ROMPage;
  SRArray                 : SRPage;        // Stores current SR page
  SRComment               : SRComArr;      // Stores SR Comments for above
  SRCommentPg             : SRComPgArr;    // Stores SR Comments per Page

  SRArrays                : array[0..7] of SRPage;  // Stores all SR Arrays

  Cur_Device_Description,
  Cur_Device_Serial       : String;

  ColSorted               : integer = 100;
  MODColSorted            : integer = 100;
  MODSortColumn           : integer;

  FATOnly                 : boolean = false;

  InsertOn                : boolean = true;

  SaveAutoFind            : boolean;
  SaveUpDnMax             : integer;
  SaveUpDn                : integer;

type
  IO_Array                = Array[0..255] of Byte;

  MemType                 = (FLASH, SRAM);

  MODHeader = record                       // Module Header
    FileFormat    : array[0..  3] of byte; // constant value defines file format and revision
    Title         : array[0.. 50] of byte; // the full module name (the short name is
                                           // the name of the file itself)
    Version       : array[0..  9] of byte; // module version, if any
    PartNumber    : array[0.. 19] of byte; // module part number
    Author        : array[0.. 49] of byte; // author, if any
    Copyright     : array[0.. 99] of byte; // copyright notice, if any
    License       : array[0..199] of byte; // license terms, if any
    Comments      : array[0..255] of byte; // free form comments, if any
    Category      : byte;                  // module category, see codes below
    Hardware      : byte;                  // defines special hardware that module contains
    MemModules    : byte;                  // defines number of main memory modules (0-4)
    XMemModules   : byte;                  // defines number of extended memory modules
                                           //   0 = none, 1 = Xfuns/XMem,
                                           //   2, 3 = one or two additional XMem modules)
    Original      : byte;                  // allows validation of original contents:
                                           //   1 = images and data are original,
                                           //   0 = this file has been updated by a user
                                           // application (data in RAM written back to MOD file, etc)
    AppAutoUpdate : byte;                  // tells any application to:
                                           //   1 = overwrite this file automatically when saving other data,
                                           //   0 = do not update
    NumPages      : byte;                  // the number of pages in this file
                                           //   0-256, but normally between 1-6
    HeaderCustom  : array[0.. 31] of byte; // for special hardware attributes
  end;

  MODFilePage = record                     // ModuleFilePage
    Name          : array[0.. 19] of byte; // normally the name of the original .ROM file, if any
    ID            : array[0..  8] of byte; // ROM ID code, normally two letters and a number
                                           // are ID and last letter is revision
    Page          : byte;                  // the page that this image must be in (0-F,
                                           // although 8-F is not normally used) or defines
                                           // each page's position relative to other images
                                           // in a page group, see codes below
    PageGroup     : byte;                  // 0 = not grouped, otherwise images with matching
                                           // PageGroup values (1..8) are grouped according
                                           // to POSITION code
    Bank          : byte;                  // the bank that this image must be in (1-4)
    BankGroup     : byte;                  // 0=not grouped, otherwise images with matching
                                           // BankGroup values (1..8) are bankswitched
                                           // with each other
    RAM           : byte;                  // 0 = ROM, 1 = RAM - normally RAM pages are all
                                           // blank if Original = 1
    WriteProtect  : byte;                  // 0 = No or N/A, 1 = protected - for HEPAX RAM
                                           // and others that might support it
    FAT           : byte;                  // 0 = no FAT, 1 = has FAT
    Image         : array[0..5119] of byte;// the image in packed format (.BIN file format)
    PageCustom    : array[0..  31] of byte;// for special hardware attributes
  end;


const

  CommandArray: array[$0..$17] of string = (
    'XCOMPLETE    ',  // $00
    'XTDOMASK     ',  // $01
    'XSIR         ',  // $02
    'XSDR         ',  // $03
    'XRUNTEST     ',  // $04
    'reserved $05 ',  // $05
    'reserved $06 ',  // $06
    'XREPEAT      ',  // $07
    'XSDRSIZE     ',  // $08
    'XSDRTDO      ',  // $09
    'XSETSDRMASKS ',  // $0A
    'XSDRINC      ',  // $0B
    'XSDRB        ',  // $0C
    'XSDRC        ',  // $0D
    'XSDRE        ',  // $0E
    'XSDRTDOB     ',  // $0F
    'XSDRTDOC     ',  // $10
    'XSDRTDOE     ',  // $11
    'XSTATE       ',  // $12
    'XENDIR       ',  // $13
    'XENDDR       ',  // $14
    'XSIR2        ',  // $15
    'XCOMMENT     ',  // $16
    'XWAIT        '   // $17
    );

  MOD_Category_Max = 8;
  MOD_Category_Array: array[0..MOD_Category_Max] of string = (
    'not categorized',
    'base Operating System for C,CV,CX',
    'HP Application PACs',
    'any HP-IL related modules and devices',
    'standard Peripherals: Wand, Printer, Card Reader, XFuns/Mem, Service, Time, IR Printer',
    'custom Peripherals: AECROM, CCD, HEPAX, PPC, ZENROM, etc',
    'BETA releases not fully debugged and finished',
    'test programs not meant for normal usage',
    'MLDL2000 Backup'
    );

  MOD_Hardware_Max = 10;
  MOD_Hardware_Array: array[0..MOD_Hardware_Max] of string = (
    'no additional hardware specified',
    '82143A Printer',
    '82104A Card Reader',
    '82182A Time Module or HP-41CX built in timer',
    '82153A Barcode Wand',
    '82160A HP-IL Module',
    '82242A Infrared Printer Module',
    'HEPAX Module - has special hardware features (write protect, relocation)',
    'W&W RAMBOX - has special hardware features (RAM block swap instructions)',
    'MLDL2000',
    'CLONIX-41 Module'
    );

    // Formal definition
    MOD_FORMAT             = 'MOD1'; // Current MODFile format

    // relative position codes- do not mix these in a group except ODD/EVEN and
    // UPPER/LOWER, ODD/EVEN, UPPER/LOWER can only place ROMS in 16K blocks
    POSITION_MIN           = $1f; // minimum POSITION_ define value
    POSITION_ANY           = $1f; // position in any port page (8-F)
    POSITION_LOWER         = $2f; // position in lower port page relative to
                                  // any upper image(s) (8-F)
    POSITION_UPPER         = $3f; // position in upper port page
    POSITION_EVEN          = $4f; // position in any even port page (8,A,C,E)
    POSITION_ODD           = $5f; // position in any odd port page (9,B,D,F)
    POSITION_ORDERED       = $6f; // position sequentially in MOD file order,
                                  // one image per page regardless of bank
    POSITION_MAX           = $6f; // maximum POSITION_ define value

    // CATEGORY DEFINITION
    CATEGORY_UNDEF         = 0;  // not categorized
    CATEGORY_OS            = 1;  // base Operating System for C,CV,CX
    CATEGORY_APP_PAC       = 2;  // HP Application PACs
    CATEGORY_HPIL_PERPH    = 3;  // any HP-IL related modules and devices
    CATEGORY_STD_PERPH     = 4;  // standard Peripherals: Wand, Printer, Card Reader,
                                 // XFuns/Mem, Service, Time, IR Printer
    CATEGORY_CUSTOM_PERPH  = 5;  // custom Peripherals: AECROM, CCD, HEPAX, PPC, ZENROM, etc
    CATEGORY_BETA          = 6;  // BETA releases not fully debugged and finished
    CATEGORY_EXPERIMENTAL  = 7;  // test programs not meant for normal usage
    CATEGORY_M2KBACKUP     = 8;  // MLDL2000 backup
    CATEGORY_MAX           = 8;  // maximum CATEGORY_ define value

    // Definition of Hardware Codes
    HARDWARE_NONE          =  0;  // no additional hardware specified
    HARDWARE_PRINTER       =  1;  // 82143A Printer
    HARDWARE_CARDREADER    =  2;  // 82104A Card Reader
    HARDWARE_TIMER         =  3;  // 82182A Time Module or HP-41CX built in timer
    HARDWARE_WAND          =  4;  // 82153A Barcode Wand
    HARDWARE_HPIL          =  5;  // 82160A HP-IL Module
    HARDWARE_INFRARED      =  6;  // 82242A Infrared Printer Module
    HARDWARE_HEPAX         =  7;  // HEPAX Module - has special hardware features
                                  // (write protect, relocation)
    HARDWARE_WWRAMBOX      =  8;  // W&W RAMBOX - has special hardware features
                                  // (RAM block swap instructions)
    HARDWARE_MLDL2000      =  9;  // MLDL2000
    HARDWARE_CLONIX        = 10;  // CLONIX-41 Module
    HARDWARE_MAX           = 10;  // maximum HARDWARE_ define value

    MODHeaderSize          = 729;
    MODFilePageSize        = 5188;

  var

    // Module Header
    MODFileFormat    : string;     // constant value defines file format and revision
    MODTitle         : string;     // the full module name (the short name is
                                   // the name of the file itself)
    MODVersion       : string;     // module version, if any
    MODPartNumber    : string;     // module part number
    MODAuthor        : string;     // author, if any
    MODCopyright     : string;     // copyright notice, if any
    MODLicense       : string;     // license terms, if any
    MODComments      : string;     // free form comments, if any
    MODCategory      : byte;       // module category, see codes below
    MODHardware      : byte;       // defines special hardware that module contains
    MODMemModules    : byte;       // defines number of main memory modules (0-4)
    MODXMemModules   : byte;       // defines number of extended memory modules
                                   //   0 = none, 1 = Xfuns/XMem,
                                   //   2, 3 = one or two additional XMem modules)
    MODOriginal      : byte;       // allows validation of original contents:
                                   //   1 = images and data are original,
                                   //   0 = this file has been updated by a user
                                   // application (data in RAM written back to MOD file, etc)
    MODAppAutoUpdate : byte;       // tells any application to:
                                   //   1 = overwrite this file automatically when saving other data,
                                   //   0 = do not update
    MODNumPages      : byte;       // the number of pages in this file
                                   //   0-256, but normally between 1-6
    MODHeaderCustom  : array[0..31] of byte;    // for special hardware attributes
    MODNumPagesCustom: word;       // expansion of MODNumPages for backup purposes 


    // ModuleFilePage
    MODName          : string;     // normally the name of the original .ROM file, if any
    MODID            : string;     // ROM ID code, normally two letters and a number
                                   // are ID and last letter is revision
    MODPage          : byte;       // the page that this image must be in (0-F,
                                   // although 8-F is not normally used) or defines
                                   // each page's position relative to other images
                                   // in a page group, see codes below
    MODPageGroup     : byte;       // 0 = not grouped, otherwise images with matching
                                   // PageGroup values (1..8) are grouped according
                                   // to POSITION code
    MODBank          : byte;       // the bank that this image must be in (1-4)
    MODBankGroup     : byte;       // 0=not grouped, otherwise images with matching
                                   // BankGroup values (1..8) are bankswitched
                                   // with each other
    MODRAM           : byte;       // 0 = ROM, 1 = RAM - normally RAM pages are all
                                   // blank if Original = 1
    MODWriteProtect  : byte;       // 0 = No or N/A, 1 = protected - for HEPAX RAM
                                   // and others that might support it
    MODFAT           : byte;       // 0 = no FAT, 1 = has FAT
    MODImage         : array[0..5119] of byte;  // the image in packed format (.BIN file format)
    MODPageCustom    : array[0..31] of byte;    // for special hardware attributes
    MODBufArray      : array[0..255] of byte;
    MODCurrent       : integer;

implementation


end.

