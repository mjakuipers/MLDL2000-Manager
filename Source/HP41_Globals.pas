unit HP41_Globals;


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
//  HP41_GLOBALS.PAS                                                         //
//  Globals for HP41 related functions, like the disassembler (future)       //
//  Menmonics table copied from Warren Furlow's V41 sources                  //
//  Ver  Date     Description                                                //
//  1.00 Nov 2005 First version for release                                  //
//  1.01 Nov 2006 Compiled for Turbo Explorer                                //
//  1.20 Feb 2008 Improved layout for HexTable                               //
//  1.20 Mar 2008 Modified Mnemonics table                                   //
//  1.50 May 2008 Final release                                              //
//  1.51 Dec 2008 Added mnemonics for SDK41 JDA mnemonics                    //
//  1.60 Jan 2010 Modified for new disassembler                              //
//  1.70 Jul 2010 Adapted for Lazarus                                        //
//  1.80 Feb 2018 restarted development                                      //
//                removed Delphi compatibility                               //
//  1.90 Mar 2020 added 1FC WCMD, HP41CL command                             //
//---------------------------------------------------------------------------//

interface

{ contains various global declarations }

const
  RomSize = $1FFF; { 8K Size of ROM }

type
  UserType = record
    Name: string[10];
    Typ: Integer;
  end;
  HexArr = array[0..$FF] of string[10];
  TypArr = array[0..$FF] of integer;

  String2 = string[2];
  String3 = string[3];
  String4 = string[4];
  String5 = string[5];
  String6 = string[6];
  String8 = string[8];
  String10 = string[10];
  String12 = string[12];
  String80 = string[80];
  AnyString = string;

  RomPage = array[$0000..RomSize] of word;

  // Type of instruction, result from Analyze or Disassembler
  InstrType = ( Tp_Undefined,
                Tp_XROMNumber,
                Tp_FATEntries,
                Tp_FATFunction,
                Tp_FATEnd,
                Tp_FunctionName,
                Tp_Data,
                Tp_Instruction1,
                Tp_Instruction2,
                Tp_Instruction3,
                Tp_UserProgram,
                Tp_EntryPoint,
                Tp_ROMRevision,
                Tp_Checksum,
                Tp_FirstTwo,
                Tp_StrDat,
                Tp_MESLString
                );

  // instrcutions for printing the disassembly line
  DisType    = ( Dt_Default,             // will handle as standard
                 Dt_EmptyLineAfter,      // to print an empty line after this line
                 Dt_EmptyLineBefore,     // print an empty line before this line
                 Dt_SkipLine,            // do not print this line (2nd or 3rd part of instruction
                 Dt_UserEnd,             // last line of User Program
                 Dt_EndPlaceHolder
               );

  FlowType   = ( Ft_Unkown,              // do not know yet
                 Ft_Data,                // data or non-executable
                 Ft_NormalEx,            // executable, never sets carry
                 Ft_CarryEx,             // executable, may set carry
                 Ft_CarryBranch,         // branches if carry set
                                         // instruction after is executable
                 Ft_AlwaysBranch,        // branches always
                                         // also when preceded by carry clearing op
                 Ft_2ndGOXQ,             // 2nd word of GO/XQ
                 Ft_3rdGOXQ,             // 3rd word of GO/XQ
                 Ft_PR_Sel,              // printer selected
                 Ft_IL_Sel               // HP-IL selected
               );

  // structure of disassembly
  RomWord    = record
                 Adr      : word;        // virtual address in ROM
                 HexCode  : word;        // hex code, or first hexcode of the line
                 HexCodes : string;      // may be multiple hex codes
                 WrdTp    : InstrType;   // Type of code
                 DisTp    : DisType;     // How to disassemble
                 Mnem     : string;      // first part of disassembled mnemonic
                                         // typically instruction or pseudo instruction
                                         // in case of JNC contains the jump distance
                 MnemArg1 : string;      // argument, or address referred to
                 MnemArg2 : string;      // 2nd part of mnemonic, used for MESL string
                 Lbl      : string;      // label of this address, if any
                 HasLabel : boolean;     // does this address has a label?
                 Comment  : string;      // comment, if any
                 RefLabel : boolean;     // does this line refer to a label?
                 RefAdr   : word;        // address the line refers to
                 RefLbl   : string;      // label this line refers to
                 UCodeLn  : integer;     // User Code line number, if user code
                 Warning  : string;      // reserved for future use
                 Flow     : FlowType;    // reserved for future use
               end;

  DisPageTp = array[0..$1FFF] of RomWord; // make this dynamic ??

const
  HexSet: set of char = ['0'..'9', 'A'..'F'];
  HexChr: set of char = ['A'..'F'];
  DecSet: set of char = ['0'..'9'];

  Space  = ' ';
  Space2 = '  ';
  CR = ^M;
  LF = ^J;
//  NL = ^M^J;
  EmptyData  = ' ... ';
  EmptyData2 = ' ... ... ';
  EmptyData3 = ' ... ... ... ';

  { definitions of opcode data type identification }
  Undefined    =  0; { type not yet decoded }
  XROMNumber   =  1; { XROM number at $X000 }
  FATEntries   =  2; { number of FAT entries at $X001 }
  FATFunction  =  3; { FAT function address entry }
  FATEnd       =  4; { the two nulls at the end of the FAT }
  FunctionName =  5; { data is part of a function name }
  Data         =  6; { opcode is not instruction, but data }
  Instruction1 =  7; { first byte of an instruction }
  Instruction2 =  8; { second byte of a multibyte instruction }
  Instruction3 =  9; { data, to be decoded with an instruction }
                     { "third" byte of an instruction }
  UserProgram  = 10; { program is written in usercode }
  EntryPoint   = 11; { this is one of the seven entry points }
  ROMRevision  = 12; { one of the four letters of the ROM revision code }
  CheckSum     = 13; { the ROM checksum at $XFFF }
  FirstTwo     = 14; { two words at beginning of usercode program }
  StrDat       = 15; { string type data, following ?NC XQ MESSL }

  Pause_Entry   = $0FF4;
  Prgm_Entry    = $0FF5;
  Sleep_Entry   = $0FF6;
  Off_Entry     = $0FF7;
  Service_Entry = $0FF8;
  On_Entry      = $0FF9;
  MemLost_Entry = $0FFA;
  ROMREV_Addr   = $0FFB;
  CHECKSUM_Addr = $0FFF;

  Mnemonics: array[0..300, 0..4] of string =
   // E	     JDA        HP          ZENCODE     SDK41           COMMENT
   // Index 0 - Class 0 instructions
  (('000', 'NOP'     , 'NOP'     , 'NOP'     , 'NOP'     ), // no operation
   ('030', 'ROMBLK'  , 'ROMBLK'  , 'ROMBLK'  , 'ROMBLK'  ), // HEPAX move ROM Block
   ('040', 'WROM'    , 'WMLDL'   , 'WMLDL'   , 'WROM'    ), // MLDL write C[2:0] to address C[6:3]
   ('100', 'ENBANK1' , 'ENROM1'  , 'ENBANK1' , 'ENROM1'  ), // enable ROM bank 1 for current ROM device
   ('180', 'ENBANK2' , 'ENROM2'  , 'ENBANK2' , 'ENROM2'  ), // enable ROM bank 2 for current ROM device
   ('140', 'ENBANK3' , 'ENROM3'  , 'ENBANK3' , 'ENROM3'  ), // enable ROM bank 3 for current ROM device
   ('1C0', 'ENBANK4' , 'ENROM4'  , 'ENBANK4' , 'ENROM4'  ), // enable ROM bank 4 for current ROM device
   ('130', 'LDI'     , 'LDI'     , 'LDI'     , 'LDIS&X'  ), // 000 to 3FF (hex), load next word in ROM into C[2:0]
   ('1F0', 'WPTOG'   , 'WPTOG'   , 'WPTOG'   , 'WPTOG'   ), // HEPAX toggle write protection
   ('3C4', 'ST=0'    , 'CLRST'   , 'ST=0'    , 'ST=0'    ), // clear lower CPU flags (7 0)
   ('3C8', 'CLRKEY'  , 'RSTKB'   , 'CLRKEY'  , 'CLRKEY'  ), // clear keydown flag if no key pressed and cl reg
   ('3CC', '?KEY'    , 'CHKKB'   , '?KEY'    , '?KEY'    ), // set carry if keydown flag is set
   ('3D4', 'R=R-1'   , 'DECPT'   , '-PT'     , 'R=R-1'   ), // decrement the active pointer
   ('3DC', 'R=R+1'   , 'INCPT'   , '+PT'     , 'R=R+1'   ), // increment the active pointer
   // Index 14:
   ('200', 'HPIL=C'  , 'HPIL=C'  , 'HPIL=C'  , 'HPIL=C'  ), // 0 to 7, copy C[1:0] to HP IL register
   // Index 15:
   ('080', 'UNDEF080', 'UNDEF080', 'UNDEF080', '#080'    ), // Instr Type 0: 080, 0C0, 240, 280, 2C0, 300, 340, 380, 3C0 are unused
   ('0C0', 'UNDEF0C0', 'UNDEF0C0', 'UNDEF0C0', '#0C0'    ), // Instr Type 0: 080, 0C0, 240, 280, 2C0, 300, 340, 380, 3C0 are unused
   ('240', 'HPIL=C'  , 'HPIL=C'  , 'HPIL=C'  , 'HPIL=C'  ),
   ('280', 'HPIL=C'  , 'HPIL=C'  , 'HPIL=C'  , 'HPIL=C'  ),
   ('2C0', 'HPIL=C'  , 'HPIL=C'  , 'HPIL=C'  , 'HPIL=C'  ),
   ('300', 'HPIL=C'  , 'HPIL=C'  , 'HPIL=C'  , 'HPIL=C'  ),
   ('340', 'HPIL=C'  , 'HPIL=C'  , 'HPIL=C'  , 'HPIL=C'  ),
   ('380', 'HPIL=C'  , 'HPIL=C'  , 'HPIL=C'  , 'HPIL=C'  ),
   ('3C0', 'HPIL=C'  , 'HPIL=C'  , 'HPIL=C'  , 'HPIL=C'  ),
   // Index 24
   ('004', 'CLRF'    , 'ST=0'    , 'CF'      , 'CLRF'    ), // 0 to 13 (dec) clear flag
   ('008', 'SETF'    , 'ST=1'    , 'SF'      , 'SETF'    ), // 0 to 13 (dec) set flag
   ('00C', '?FSET'   , 'ST=1?'   , '?FS'     , '?FSET'   ), // 0 to 13 (dec) set carry if flag set
   ('010', 'LD@R'    , 'LC'      , 'LC'      , 'LD@R'    ), // 0 to F (hex) load constant to C[PT], then decrement pointer
   ('014', '?R='     , '?PT='    , '?PT='    , '?R='     ), // 0 to 13 (dec) set carry if active pointer equal
   // Index 29: IDef6[InstrParam]
   ('018', 'UNDEF018', 'UNDEF018', 'UNDEF018', '#018'    ), //
   ('058', 'G=C'     , 'G=C'     , 'G=C'     , 'G=C'     ), // copy C[PT+1:PT] into G (if PT= 13, high byte is undefined)
   ('098', 'C=G'     , 'C=G'     , 'C=G'     , 'C=G'     ), // copy G into C[PT+1:PT] (if PT= 13, high byte is undefined)
   ('0D8', 'C<>G'    , 'CGEX'    , 'C<>G'    , 'C<>G'    ), // exchange C[PT+1:PT] and G (if PT= 13, high byte is undefined)
   ('118', 'UNDEF118', 'UNDEF118', 'UNDEF118', '#118'    ), //
   ('158', 'M=C'     , 'M=C'     , 'M=C'     , 'M=C'     ), // copy C[13:0] into M
   ('198', 'C=M'     , 'C=M'     , 'C=M'     , 'C=M'     ), // copy M[13:0] into C
   ('1D8', 'C<>M'    , 'CMEX'    , 'C<>M'    , 'C<>M'    ), // exchange C[13:0] and M
   ('218', 'UNDEF218', 'UNDEF218', 'UNDEF218', '#218'    ), //
   ('258', 'T=ST'    , 'F=SB'    , 'F=ST'    , 'T=ST'    ), // copy ST into beeper register
   ('298', 'ST=T'    , 'SB=F'    , 'ST=F'    , 'ST=T'    ), // copy beeper register into ST
   ('2D8', 'ST<>T'   , 'FEXSB'   , 'ST<>F'   , 'ST<>T'   ), // exchange ST and beeper register
   ('318', 'UNDEF318', 'UNDEF318', 'UNDEF318', '#318'    ), //
   ('358', 'ST=C'    , 'ST=C'    , 'ST=C'    , 'ST=C'    ), // copy C[1:0] into ST
   ('398', 'C=ST'    , 'C=ST'    , 'C=ST'    , 'C=ST'    ), // copy ST into C[1:0]
   ('3D8', 'C<>ST'   , 'CSTEX'   , 'C<>ST'   , 'C<>ST'   ), // exchange C[1:0] and ST
   // Index 45
   ('01C', 'R='      , 'PT='     , 'PT='     , 'R='      ), // 0 to 13 (dec) set active pointer
   // Index 46: IDef8[InstrParam]
   ('020', 'XQ>GO'   , 'SPOPND'  , 'CLRRTN'  , 'XQ>GO'   ), // pop first address off return stack
   ('060', 'POWOFF'  , 'POWOFF'  , 'POWOFF'  , 'POWOFF'  ), // halt the CPU
   ('0A0', 'SLCTP'   , 'SELP'    , 'PT=P'    , 'SLCTP'   ), // make P the active pointer
   ('0E0', 'SLCTQ'   , 'SELQ'    , 'PT=Q'    , 'SLCTQ'   ), // make Q the active pointer
   ('120', '?P=Q'    , '?P=Q'    , '?P=Q'    , '?P=Q'    ), // set carry if P=Q
   ('160', '?LOWBAT' , '?LLD'    , '?BAT'    , '?LOWBAT' ), // set carry if battery is low
   ('1A0', 'A=B=C=0' , 'CLRABC'  , 'ABC=0'   , 'A=B=C=0' ), // clear all nybbles of A,B,C registers
   ('1E0', 'GOTOADR' , 'GOTOC'   , 'GTOC'    , 'GOTOADR' ), // jump to the address in C[6:3]
   ('220', 'C=KEY'   , 'C=KEYS'  , 'C=KEY'   , 'C=KEY'   ), // copy key register to C[4:3]
   ('260', 'SETHEX'  , 'SETHEX'  , 'SETHEX'  , 'SETHEX'  ), // set hexadecimal mode
   ('2A0', 'SETDEC'  , 'SETDEC'  , 'SETDEC'  , 'SETDEC'  ), // set decimal mode
   ('2E0', 'DSPOFF'  , 'DISOFF'  , 'DISOFF'  , 'DSPOFF'  ), // turn display off
   ('320', 'DSPTOG'  , 'DISTOG'  , 'DISTOG'  , 'DSPTOG'  ), // toggle state of display
   ('360', '?C RTN'  , 'RTNC'    , 'CRTN'    , '?CRTN'   ), // return if carry set
   ('3A0', '?NC RTN' , 'RTNNC'   , 'NCRTN'   , '?NCRTN'  ), // return if carry not set
   ('3E0', 'RTN'     , 'RTN'     , 'RTN'     , 'RTN'     ), // unconditional return
   // Index 62
   ('024', 'SELPF'   , 'SELPF'   , 'PERTCT'  , 'SELPF'   ), // 0 to F (hex) allow peripheral to take control
   ('028', 'WRIT'    , 'REGN=C'  , 'REG=C'   , 'WRIT'    ), // 0 to F (hex) write to RAM register in selected chip
   ('02C', '?FI='    , 'FLG=1?'  , '?PF'     , '?FI'     ), // 0 to 13 (dec) set carry if peripheral flag set
   // Index 65: IDefC[InstrParam]
   ('030', 'ROMBLK'  , 'ROMBLK'  , 'ROMBLK'  , 'ROMBLK'  ), // HEPAX move ROM Block
   ('070', 'N=C'     , 'N=C'     , 'N=C'     , 'N=C'     ), // copy C[13:0] into N
   ('0B0', 'C=N'     , 'C=N'     , 'C=N'     , 'C=N'     ), // copy N into C[13:0]
   ('0F0', 'C<>N'    , 'CNEX'    , 'C<>N'    , 'C<>N'    ), // exchange C[13:0] and N
   ('130', 'LDI'     , 'LDI'     , 'LDI'     , 'LDI'     ), // 000 to 3FF (hex), load next word in ROM into C[2:0]
   ('170', 'PUSHADR' , 'STK=C'   , 'STK=C'   , 'PUSHADR' ), // push C[6:3] onto the return stack
   ('1B0', 'POPADR'  , 'C=STK'   , 'C=STK'   , 'POPADR'  ), // pop the return stack into C[6:3], put 0 in last location of stack
   ('1F0', 'WPTOG'   , 'WPTOG'   , 'WPTOG'   , 'WPTOG'   ), // HEPAX toggle write protection
   ('230', 'GTOKEY'  , 'GOKEYS'  , 'GTOKEY'  , 'GTOKEY'  ), // copy the key register into the low byte of PC
   ('270', 'RAMSLCT' , 'DADD=C'  , 'RAMSLCT' , 'RAMSLCT' ), // select the RAM chip addressed in C[2:0]
   ('2B0', 'UNDEF2B0', 'UNDEF2B0', 'UNDEF2B0', '#2B0'    ), //
   ('2F0', 'WRITDATA', 'DATA=C'  , 'WDATA'   , 'WRITDATA'), // writes C[13:0] to selected RAM register
   ('330', 'FETCH S&X','CXISA'   , 'RDROM'   , 'FETCHS&X'), // copies the ROM data in C[6:3] into C[2:0]
   ('370', 'C=C OR A', 'C=CORA'  , 'C=CORA'  , 'C=CORA'  ), // C[13:0] = C bitwise or A
   ('3B0', 'C=C AND A','C=C&A'   , 'C=CANDA' , 'C=CANDA' ), // C[13:0] = C bitwise and A
   ('3F0', 'PRPHSLCT', 'PFAD=C'  , 'PERSLCT' , 'PRPHSLCT'), // select the peripheral addressed in C[1:0]
   // Index 81
   ('038', 'READ'    , 'C=REGN'  , 'C=REG'   , 'READ '   ), // 0 to F (hex), read from RAM register in selected chip
   ('038', 'READDATA', 'C=DATA'  , 'RDATA'   , 'READDATA'), // writes selected RAM register to C[13:0]
   ('03C', 'RCR'     , 'RCR'     , 'RCR'     , 'RCR '    ), // 0 to 13 (dec), rotate C reg right by the nybble

   // Index 84, Class 1 instructions, LONG JUMPS
   ('001', '?NC XQ'  , 'GSUBNC'  , 'NCXQ'    , '?NCXQ'   ), // ADDRESS, execute on no carry
   ('001', '?C XQ'   , 'GSUBC'   , 'CXQ'     , '?CXQ'    ), // ADDRESS, execute on carry
   ('001', '?NC GO'  , 'GOLNC'   , 'NCGO'    , '?NCGO'   ), // ADDRESS, goto on no carry
   ('001', '?C GO'   , 'GOLC'    , 'CGO'     , '?CGO'    ), // ADDRESS, goto on carry
   ('349', 'GOSUB'   , 'GSB41C'  , 'NCXQREL' , '?NCXQREL'), // L ADDRESS, execute relative to current quad
   ('341', 'GOTO'    , 'GOL41C'  , 'NCGOREL' , '?NCGOREL'), // L ADDRESS, goto relative to current quad

   // Index 90, Class 2 instructions, TIME ENABLE FIELD INSTRUCTIONS
   ('002', 'A=0'     , 'A=0'     , 'A=0'     , 'A=0'     ), // TEF, clear A[TEF]
   ('022', 'B=0'     , 'B=0'     , 'B=0'     , 'B=0'     ), // TEF, clear B[TEF]
   ('042', 'C=0'     , 'C=0'     , 'C=0'     , 'C=0'     ), // TEF, clear C[TEF]
   ('062', 'A<>B'    , 'ABEX'    , 'A<>B'    , 'A<>B'    ), // TEF, exchange A[TEF] and B[TEF]
   ('082', 'B=A'     , 'B=A'     , 'B=A'     , 'B=A'     ), // TEF, copy A[TEF] into B[TEF]
   ('0A2', 'A<>C'    , 'ACEX'    , 'A<>C'    , 'A<>C'    ), // TEF, exchange A[TEF] and C[TEF]
   ('0C2', 'C=B'     , 'C=B'     , 'C=B'     , 'C=B'     ), // TEF, copy B[TEF] into C[TEF]
   ('0E2', 'B<>C'    , 'BCEX'    , 'B<>C'    , 'B<>C'    ), // TEF, exchange B[TEF] and C[TEF]
   ('102', 'A=C'     , 'A=C'     , 'A=C'     , 'A=C'     ), // TEF, copy C[TEF] into A[TEF]
   ('122', 'A=A+B'   , 'A=A+B'   , 'A=A+B'   , 'A=A+B'   ), // TEF, add B[TEF] to A[TEF]
   // Index 100
   ('142', 'A=A+C'   , 'A=A+C'   , 'A=A+C'   , 'A=A+C'   ), // TEF, add C[TEF] to A[TEF]
   ('162', 'A=A+1'   , 'A=A+1'   , 'A=A+1'   , 'A=A+1'   ), // TEF, add one to A[TEF]
   ('182', 'A=A-B'   , 'A=A-B'   , 'A=A-B'   , 'A=A-B'   ), // TEF, subtract B[TEF] from A[TEF]
   ('1A2', 'A=A-1'   , 'A=A-1'   , 'A=A-1'   , 'A=A-1'   ), // TEF, subtract one from A[TEF]
   ('1C2', 'A=A-C'   , 'A=A-C'   , 'A=A-C'   , 'A=A-C'   ), // TEF, subtract C[TEF] from A[TEF]
   ('1E2', 'C=C+C'   , 'C=C+C'   , 'C=C+C'   , 'C=C+C'   ), // TEF, double C[TEF]
   ('202', 'C=C+A'   , 'C=A+C'   , 'C=A+C'   , 'C=C+A'   ), // TEF, add A[TEF] to C[TEF]
   ('222', 'C=C+1'   , 'C=C+1'   , 'C=C+1'   , 'C=C+1'   ), // TEF, add one to C[TEF]
   ('242', 'C=A-C'   , 'C=A-C'   , 'C=A-C'   , 'C=A-C'   ), // TEF, subtract C[TEF] from A[TEF] store in C[TEF]
   ('262', 'C=C-1'   , 'C=C-1'   , 'C=C-1'   , 'C=C-1'   ), // TEF, subtract one from C[TEF]
   ('282', 'C=0-C'   , 'C=0-C'   , 'C=0-C'   , 'C=0-C'   ), // TEF, 16's complement of C if in hex mode; 10's complement if dec mode
   ('2A2', 'C=-C-1'  , 'C=-C-1'  , 'C=-C-1'  , 'C=-C-1'  ), // TEF, 15's complement if hex mode; 9's complement if dec mode
   ('2C2', '?B#0'    , '?B#0'    , '?B#0'    , '?B#0'    ), // TEF, set carry if B[TEF] is not equal to 0
   ('2E2', '?C#0'    , '?C#0'    , '?C#0'    , '?C#0'    ), // TEF, set carry if C[TEF] is not equal to 0
   ('302', '?A<C'    , '?A<C'    , '?A<C'    , '?A<C'    ), // TEF, set carry if A[TEF] is less than C[TEF]
   ('322', '?A<B'    , '?A<B'    , '?A<B'    , '?A<B'    ), // TEF, set carry if A[TEF] is less than B[TEF]
   ('342', '?A#0'    , '?A#0'    , '?A#0'    , '?A#0'    ), // TEF, set carry if A[TEF] is not equal to 0
   ('362', '?A#C'    , '?A#C'    , '?A#C'    , '?A#C'    ), // TEF, set carry if A[TEF] is not equal to C[TEF]
   ('382', 'RSHFA'   , 'ASR'     , 'ASR'     , 'RSHFA'   ), // TEF, shift A right by one nybble (leftmost byte set to 0)
   ('3A2', 'RSHFB'   , 'BSR'     , 'BSR'     , 'RSHFB'   ), // TEF, shift B right by one nybble (leftmost byte set to 0)
   ('3C2', 'RSHFC'   , 'CSR'     , 'CSR'     , 'RSHFC'   ), // TEF, shift C right by one nybble (leftmost byte set to 0)
   ('3E2', 'LSHFA'   , 'ASL'     , 'ASL'     , 'LSHFA'   ), // TEF, shift A left by one nybble (rightmost byte set to 0)

   // Index 122, Class 3 instructions, SHORT JUMPS
   ('007', 'JC'      , 'GOC'     , 'JC'      , 'JC'      ), // 64 to +63 (dec), short relative jump on carry
   ('003', 'JNC'     , 'GONC'    , 'JNC'     , 'JNC'     ), // 64 to +63 (dec), short relative jump on no carry

   // Index 124, PERIPHERAL FLAGS
   ('02C', '?FI= 3'  , '?F3=1'   , '?PF 3'   , '?FI= 3'  ), // set carry if peripheral flag 3 set
   ('06C', '?FI= 4'  , '?F4=1'   , '?PF 4'   , '?FI= 4'  ), // set carry if peripheral flag 4 set
   ('0AC', '?FI= 5'  , '?F5=1'   , '?EDAV'   , '?FI= 5'  ), // set carry if peripheral flag 5 set
   ('0EC', '?FI= 10' , '?F10=1'  , '?ORAV'   , '?FI= 10' ), // set carry if HP-IL output register available
   ('12C', '?FI= 8'  , '?F8=1'   , '?FRAV'   , '?FI= 8'  ), // set carry if HP-IL frame available
   ('16C', '?FI= 6'  , '?F6=1'   , '?IFCR'   , '?FI= 6'  ), // set carry if HP-IL interface clear received
   ('1AC', '?FI= 11' , '?F11=1'  , '?TFAIL'  , '?FI= 11' ), // set carry if timer clock access failure
   ('22C', '?FI= 2'  , '?F2=1'   , '?WNDB'   , '?FI= 2'  ), // set carry if wand has data in wand buffer
   ('26C', '?FI= 9'  , '?F9=1'   , '?FRNS'   , '?FI= 9'  ), // set carry if HP-IL frame not received as sent
   ('2AC', '?FI= 7'  , '?F7=1'   , '?SRQR'   , '?FI= 7'  ), // set carry if service request received
   ('2EC', '?FI= 13' , '?F13=1'  , '?SERV'   , '?FI= 13' ), // set carry if service request
   ('32C', '?FI= 1'  , '?F1=1'   , '?CRDR'   , '?FI= 1'  ), // set carry if card reader flag set
   ('36C', '?FI= 12' , '?F12=1'  , '?ALM'    , '?FI= 12' ), // set carry if alarm due
   ('3AC', '?FI= 0'  , '?F0=1'   , '?PBSY'   , '?FI= 0'  ), // set carry if peripheral flag 0 set

   // Index 138, DISPLAY INSTRUCTIONS, DISPLAY READING
   ('038', 'FLLDA'   , 'FLLDA'   , 'RDA12L'  , 'FLLDA'   ), //
   ('078', 'FLLDB'   , 'FLLDB'   , 'RDB12L'  , 'FLLDB'   ), //
   ('0B8', 'FLLDC'   , 'FLLDC'   , 'RDC12L'  , 'FLLDC'   ), //
   ('0F8', 'FLLDAB'  , 'FLLDAB'  , 'RDAB6L'  , 'FLLDAB'  ), //
   ('138', 'FLLABC'  , 'FLLABC'  , 'RDABC4L' , 'FLLABC'  ), //
   ('178', 'READEN'  , 'READEN'  , 'READAN'  , 'READEN'  ), // copy annunciators into C[2:0]
   ('1B8', 'FLSDC'   , 'FLSDC'   , 'RDC1L'   , 'FLSDC'   ), //
   ('1F8', 'FRSDA'   , 'FRSDA'   , 'RDA1R'   , 'FRSDA'   ), //
   ('238', 'FRSDB'   , 'FRSDB'   , 'RDB1R'   , 'FRSDB'   ), //
   ('278', 'FRSDC'   , 'FRSDC'   , 'RDC1R'   , 'FRSDC'   ), //
   ('2B8', 'FLSDA'   , 'FLSDA'   , 'RDA1L'   , 'FLSDA'   ), //
   ('2F8', 'FLSDB'   , 'FLSDB'   , 'RDB1L'   , 'FLSDB'   ), //
   ('338', 'FRSDAB'  , 'FRSDAB'  , 'RDAB1R'  , 'FRSDAB'  ), //
   ('378', 'FLSDAB'  , 'FLSDAB'  , 'RDAB1L'  , 'FLSDAB'  ), //
   ('3B8', 'RABCR'   , 'RABCR'   , 'RDABC1R' , 'RABCR'   ), //
   ('3F8', 'RABCL'   , 'RABCL'   , 'RDABC1L' , 'RABCL'   ), //

   // Index 154, DISPLAY WRITING
   ('028', 'SRLDA'   , 'SRLDA'   , 'WRA12L'  , 'SRLDA'   ), //
   ('068', 'SRLDB'   , 'SRLDB'   , 'WRB12L'  , 'SRLDB'   ), //
   ('0A8', 'SRLDC'   , 'SRLDC'   , 'WRC12L'  , 'SRLDC'   ), //
   ('0E8', 'SRLDAB'  , 'SRLDAB'  , 'WRAB6L'  , 'SRLDAB'  ), //
   ('128', 'SRLABC'  , 'SRLABC'  , 'WRABC4L' , 'SRLABC'  ), //
   ('168', 'SLLDAB'  , 'SLLDAB'  , 'WRAB6R'  , 'SLLDAB'  ), //
   ('1A8', 'SLLABC'  , 'SLLABC'  , 'WRABC4R' , 'SLLAC'   ), //
   ('1E8', 'SRSDA'   , 'SRSDA'   , 'WRA1L'   , 'SRSDA'   ), //
   ('228', 'SRSDB'   , 'SRSDB'   , 'WRB1L'   , 'SRSDB'   ), //
   ('268', 'SRSDC'   , 'SRSDC'   , 'WRC1L'   , 'SRSDC'   ), //
   ('2A8', 'SLSDA'   , 'SLSDA'   , 'WRA1R'   , 'SLSDA'   ), //
   ('2E8', 'SLSDB'   , 'SLSDB'   , 'WRB1R'   , 'SLSDB'   ), //
   ('328', 'SRSDAB'  , 'SRSDAB'  , 'WRAB1L'  , 'SRSDAB'  ), //
   ('368', 'SLSDAB'  , 'SLSDAB'  , 'WRAB1R'  , 'SLSDAB'  ), //
   ('3A8', 'SRSABC'  , 'SRSABC'  , 'WRABC1L' , 'SRSABC'  ), //
   ('3E8', 'SLSABC'  , 'SLSABC'  , 'WRABC1R' , 'SLSABC'  ), //
   ('2F0', 'WRTEN'   , 'WRTEN'   , 'WRITAN'  , 'WRTEN'   ), // copy bits from C[2:0] into annunciators

   // Index 171: TIME MODULE
   ('028', 'WRTIME'  , 'WRTIME'  , 'WTIME'   , 'WRTIME'  ), //
   ('068', 'WDTIME'  , 'WDTIME'  , 'WTIME'   , 'WDTIME'  ), //
   ('0A8', 'WRALM'   , 'WRALM'   , 'WALM'    , 'WRALM'   ), //
   ('0E8', 'WRSTS'   , 'WRSTS'   , 'WSTS'    , 'WRSTS'   ), //
   ('128', 'WRSCR'   , 'WRSCR'   , 'WSCR'    , 'WRSCR'   ), //
   ('168', 'WSINT'   , 'WSINT'   , 'WINTST'  , 'WSINT'   ), //
   ('1E8', 'STPINT'  , 'STPINT'  , 'STPINT'  , 'STPINT'  ), //
   ('228', 'DSWKUP'  , 'DSWKUP'  , 'WKUPOFF' , 'DSWKUP'  ), //
   ('268', 'ENWKUP'  , 'ENWKUP'  , 'WKUPON'  , 'ENWKUP'  ), //
   ('2A8', 'DSALM'   , 'DSALM'   , 'ALMOFF'  , 'DSALM'   ), //
   ('2E8', 'ENALM'   , 'ENALM'   , 'ALMON'   , 'ENALM'   ), //
   ('328', 'STOPC'   , 'STOPC'   , 'STOPC'   , 'STOPC'   ), //
   ('368', 'STARTC'  , 'STARTC'  , 'STARTC'  , 'STARTC'  ), //
   ('3A8', 'PT=B'    , 'PT=B'    , 'TIMER=A' , 'PT=B'    ), //
   ('3E8', 'PT=A'    , 'PT=A'    , 'TIMER=B' , 'PT=A'    ), //
   ('038', 'RDTIME'  , 'RDTIME'  , 'RTIME'   , 'RDTIME'  ), //
   ('078', 'RCTIME'  , 'RCTIME'  , 'RTIMEST' , 'RCTIME'  ), //
   ('0B8', 'RDALM'   , 'RDALM'   , 'RALM'    , 'RDALM'   ), //
   ('0F8', 'RDSTS'   , 'RDSTS'   , 'RSTS'    , 'RDSTS'   ), //
   ('138', 'RDSCR'   , 'RDSCR'   , 'RSCR'    , 'RDSCR'   ), //
   ('178', 'RDINT'   , 'RDINT'   , 'RINT'    , 'RDINT'   ), //

   // Index 192 CARD READER
   ('028', 'ENWRIT'  , 'ENWRIT'  , 'ENDWRIT' , 'ENWRIT'  ),
   ('068', 'STWRIT'  , 'STWRIT'  , 'STWRIT'  , 'STWRIT'  ),
   ('0A8', 'ENREAD'  , 'ENREAD'  , 'ENDREAD' , 'ENREAD'  ),
   ('0E8', 'STREAD'  , 'STREAD'  , 'STREAD'  , 'STREAD'  ),
   ('168', 'CRDWPF'  , 'CRDWPF'  , 'CRDWPF'  , 'CRDWPF'  ),
   ('1E8', 'CRDOHF'  , 'CRDOHF'  , 'CRDOHF'  , 'CRDOHF'  ),
   ('268', 'CRDINF'  , 'CRDINF'  , 'CRDINF'  , 'CRDINF'  ),
   ('2E8', 'TSTBUF'  , 'TSTBUF'  , 'TSTBUF'  , 'TSTBUF'  ),
   ('328', 'TRPCRD'  , 'TRPCRD'  , 'SETCTF'  , 'TRPCRD'  ),
   ('368', 'TCLCRD'  , 'TCLCRD'  , 'TCLCTF'  , 'TCLCRD'  ),
   ('3E8', 'CRDFLG'  , 'CRDFLG'  , 'CRDEXF'  , 'CRDFLG'  ),

   // Index 203, PRINTER
   ('003', 'BUSY?'   , 'BUSY?'   , 'BUSY?'   , 'BUSY?'   ), // set carry if printer busy
   ('083', 'ERROR?'  , 'ERROR?'  , 'ERROR?'  , 'ERROR?'  ), // set carry if printer error
   ('043', 'POWON?'  , 'POWON?'  , 'POWON?'  , 'POWON?'  ), // set carry if printer is on
   ('007', 'PRINT'   , 'PRINT'   , 'PRINT'   , 'PRINT'   ), // add C[1:0] to print buffer
   ('03A', 'STATUS'  , 'STATUS'  , 'STATUS'  , 'STATUS'  ), // copy printer status to C[13:10]
   ('005', 'RTNCPU'  , 'RTNCPU'  , 'RTNCPU'  , 'RTNCPU'  ), // return from PERTCT (only necessary for STATUS)

   // Index 209, VARIATIONS AND DUPLICATES
   ('2E2', '?C#0'    , 'C#0?'    , '?C#0'    , '?C#0'    ), // TEF
   ('2C2', '?B#0'    , 'B#0?'    , '?B#0'    , '?B#0'    ), // TEF
   ('302', '?A<C'    , 'A<C?'    , '?A<C'    , '?A<C'    ), // TEF
   ('322', '?A<B'    , 'A<B?'    , '?A<B'    , '?A<B'    ), // TEF
   ('342', '?A#0'    , 'A#0?'    , '?A#0'    , '?A#0'    ), // TEF
   ('362', '?A#C'    , 'A#C?'    , '?A#C'    , '?A#C'    ), // TEF
   ('370', 'C=CORA'  , 'C=C!A'   , 'C=CORA'  , 'C=COR'   ), //
   ('3B0', 'C=CANDA' , 'C=C.A'   , 'C=CANDA' , 'C=CANDA' ), //
   ('3B8', 'FRSABC'  , 'FRSABC'  , 'RDABC1R' , 'FRSABC'  ), //
   ('0EC', '?FI= 10' , 'ORAV?'   , '?ORAV'   , '?FI= 10' ), //
   ('12C', 'FRAV?'   , 'FRAV?'   , '?FRAV'   , '?FRAV'   ), //
   ('16C', '?FI= 6'  , 'IFCR?'   , '?IFCR'   , '?FI= 6'  ), //
   ('26C', '?FI= 9'  , 'FRNS?'   , '?FRNS'   , '?FI= 9'  ), //
   ('2AC', 'SRQR?'   , 'SRQR?'   , '?SRQR'   , '?SRQR'   ), //
   ('36C', '?FI= 12' , 'ALARM?'  , '?ALM'    , '?FI= 12' ), //
   ('160', '?LOWBAT' , 'LLD?'    , '?BAT'    , '?LOWBAT' ), //
   ('120', '?P=Q'    , 'P=Q?'    , '?P=Q'    , '?P=Q'    ), //
   ('014', '?R='     , 'PT=?'    , '?PT='    , '?R='     ), // 0 to 13 (dec)
   ('001', '?NCXQ'   , 'GOSUB'   , 'NCXQ'    , '?NCXQ'   ), // ADDRESS
   ('001', '?NCGO'   , 'GOLONG'  , 'NCGO'    , '?NCGO'   ), // ADDRESS
   ('003', 'JNC'     , 'GOTO'    , 'JNC'     , 'JNC'     ), // 64 to +63 (dec)
   ('024', 'SELPF'   , 'HPL=CH'  , 'PERTCT'  , 'SELPF'   ), // 0 to F (hex)
   // Index 231
   ('384', 'CLRF 0'  , 'S0= 0'   , 'CF 0'    , 'CLRF 0'  ), //
   ('304', 'CLRF 1'  , 'S1= 0'   , 'CF 1'    , 'CLRF 1'  ), //
   ('204', 'CLRF 2'  , 'S2= 0'   , 'CF 2'    , 'CLRF 2'  ), //
   ('004', 'CLRF 3'  , 'S3= 0'   , 'CF 3'    , 'CLRF 3'  ), //
   ('044', 'CLRF 4'  , 'S4= 0'   , 'CF 4'    , 'CLRF 4'  ), //
   ('084', 'CLRF 5'  , 'S5= 0'   , 'CF 5'    , 'CLRF 5'  ), //
   ('144', 'CLRF 6'  , 'S6= 0'   , 'CF 6'    , 'CLRF 6'  ), //
   ('284', 'CLRF 7'  , 'S7= 0'   , 'CF 7'    , 'CLRF 7'  ), //
   ('104', 'CLRF 8'  , 'S8= 0'   , 'CF 8'    , 'CLRF 8'  ), //
   ('244', 'CLRF 9'  , 'S9= 0'   , 'CF 9'    , 'CLRF 9'  ), //
   ('0C4', 'CLRF 10' , 'S10= 0'  , 'CF 10'   , 'CLRF 10' ), //
   ('184', 'CLRF 11' , 'S11= 0'  , 'CF 11'   , 'CLRF 11' ), //
   ('344', 'CLRF 12' , 'S12= 0'  , 'CF 12'   , 'CLRF 12' ), //
   ('2C4', 'CLRF 13' , 'S13= 0'  , 'CF 13'   , 'CLRF 13' ), //
   // 245
   ('388', 'SETF 0'  , 'S0= 1'   , 'SF 0'    , 'SETF 0'  ), //
   ('308', 'SETF 1'  , 'S1= 1'   , 'SF 1'    , 'SETF 1'  ), //
   ('208', 'SETF 2'  , 'S2= 1'   , 'SF 2'    , 'SETF 2'  ), //
   ('008', 'SETF 3'  , 'S3= 1'   , 'SF 3'    , 'SETF 3'  ), //
   ('048', 'SETF 4'  , 'S4= 1'   , 'SF 4'    , 'SETF 4'  ), //
   ('088', 'SETF 5'  , 'S5= 1'   , 'SF 5'    , 'SETF 5'  ), //
   ('148', 'SETF 6'  , 'S6= 1'   , 'SF 6'    , 'SETF 6'  ), //
   ('288', 'SETF 7'  , 'S7= 1'   , 'SF 7'    , 'SETF 7'  ), //
   ('108', 'SETF 8'  , 'S8= 1'   , 'SF 8'    , 'SETF 8'  ), //
   ('248', 'SETF 9'  , 'S9= 1'   , 'SF 9'    , 'SETF 9'  ), //
   ('0C8', 'SETF 10' , 'S10= 1'  , 'SF 10'   , 'SETF 10' ), //
   ('188', 'SETF 11' , 'S11= 1'  , 'SF 11'   , 'SETF 11' ), //
   ('348', 'SETF 12' , 'S12= 1'  , 'SF 12'   , 'SETF 12' ), //
   ('2C8', 'SETF 13' , 'S13= 1'  , 'SF 13'   , 'SETF 13' ), //
   // 259
   ('38C', '?FSET 0 ', '?S0=1'   , '?FS 0'   , '?FSET 0' ), //
   ('30C', '?FSET 1 ', '?S1=1'   , '?FS 1'   , '?FSET 1' ), //
   ('20C', '?FSET 2 ', '?S2=1'   , '?FS 2'   , '?FSET 2' ), //
   ('00C', '?FSET 3 ', '?S3=1'   , '?FS 3'   , '?FSET 3' ), //
   ('04C', '?FSET 4 ', '?S4=1'   , '?FS 4'   , '?FSET 4' ), //
   ('08C', '?FSET 5 ', '?S5=1'   , '?FS 5'   , '?FSET 5' ), //
   ('14C', '?FSET 6 ', '?S6=1'   , '?FS 6'   , '?FSET 6' ), //
   ('28C', '?FSET 7 ', '?S7=1'   , '?FS 7'   , '?FSET 7' ), //
   ('10C', '?FSET 8 ', '?S8=1'   , '?FS 8'   , '?FSET 8' ), //
   ('24C', '?FSET 9 ', '?S9=1'   , '?FS 9'   , '?FSET 9' ), //
   ('0CC', '?FSET 10', '?S10=1'  , '?FS 10'  , '?FSET 10'), // 0
   ('18C', '?FSET 11', '?S11=1'  , '?FS 11'  , '?FSET 11'), // 1
   ('34C', '?FSET 12', '?S12=1'  , '?FS 12'  , '?FSET 12'), // 2
   ('2CC', '?FSET 13', '?S13=1'  , '?FS 13'  , '?FSET 13'), // 3
   // 273, start of Pseudo Instructions and Directives
   // E	 JDA HP  ZENCODE SDK41  COMMENT
   (''   , '.FATFUN' , '.FATFUN' , '.FATFUN' , 'DEFR4K'  ), //
   (''   , '.FATFUN' , '.FATFUN' , '.FATFUN' , 'DEFR8K'  ),
   (''   , '.FATFUN' , '.FATFUN' , '.FATFUN' , 'U4KDEF'  ),
   (''   , '.FATFUN' , '.FATFUN' , '.FATFUN' , 'U8KDEF'  ),
   (''   , ''        , ''        , ''        , 'LC3'     ),
   (''   , '.DATA'   , '.DATA'   , '.DATA'   , 'CON'     ),
   (''   , ''        , ''        , ''        , 'A=B'     ),
   (''   , ''        , ''        , ''        , 'B=C'     ),
   (''   , ''        , ''        , ''        , 'C=A'     ),
   (''   , ''        , ''        , ''        , 'GOSUB'   ),
   (''   , '.XROM'   , '.XROM'   , '.XROM'   , 'XROM'    ),
   (''   , '.FCNS'   , '.FCNS'   , '.FCNS'   , 'FCNS'    ),
   (''   , ''        , ''        , ''        , '.TITLE'  ),
   (''   , ''        , ''        , ''        , '.HP'     ),
   (''   , ''        , ''        , ''        , '.ZENCODE'),
   (''   , ''        , ''        , ''        , '.JDA'    ),
   (''   , ''        , ''        , ''        , '.FILLTO' ),
   (''   , '.FILL'   , '.FILL'   , '.FILL'   , '.BSS'    ),
   (''   , '.FNAME'  , '.FNAME'  , '.FNAME'  , '.NAME'   ),
   (''   , '.MESSL'  , '.MESSL'  , '.MESSL'  , '.MESSL'  ),
   (''   , ''        , ''        , ''        , '.ORG'    ),
   (''   , ''        , ''        , ''        , '.EQU'    ),
   (''   , '.FATEND' , '.FATEND' , '.FATEND' , ''        ),
   (''   , '.ROMREV' , '.ROMREV' , '.ROMREV' , ''        ),
   (''   , '.CHKSUM' , '.CHKSUM' , '.CHKSUM' , 'CON'     ),
   (''   , '.UCODE'  , '.UCODE'  , '.UCODE'  , '.UCODE'  ),
   (''   , '.MCODE'  , '.MCODE'  , '.MCODE'  , '.MCODE'  ),
    // 299, additional instruction 41CL
   ('1FC', 'WCMD'    , 'WCMD'    , 'WCMD'    , '.WCMD'   )   // HP41CL
    // Index of last element: 300
  );

  MaskArr: array[$0..$F] of word = (                 // masks for flag register
    $0008, $0010, $0020, $0400, $0100, $0040, $0800, $4000,
    $0004, $0200, $0080, $2000, $0002, $1000, $0001, $8000);

  FlagArr: array[$0..$F] of integer = (
    3, 4, 5, 10, 8, 6, 11, 14, 2, 9, 7, 13, 1, 12, 0, 15);

  ParamDefReg: array[$0..$F] of char = (
    'T', 'Z', 'Y', 'X', 'L', 'M', 'N', 'O',
    'P', 'Q', '+', 'a', 'b', 'c', 'd', 'e');

  ParamDefFlag: array[0..$F] of string2 = (
    '3', '4', '5', '10', '8', '6', '11', '14',       // 14 and 15 are not always used
    '2', '9', '7', '13', '1', '12', '0', '15');

  ParamDefRegs: array[0..$F] of string5 = (
    '( 0)T', '( 1)Z', '( 2)Y', '( 3)X', '( 4)L', '( 5)M', '( 6)N', '( 7)O',
    '( 8)P', '( 9)Q', '(10)+', '(11)a', '(12)b', '(13)c', '(14)d', '(15)e');

  FieldDef: array[0..7] of string3 = (
    '@R', 'S&X', 'R<-', 'ALL', 'P-Q', 'XS', 'M', 'MS');

  FieldDefSDK41: array[0..7] of string3 = (
    '@R', 'S&X', 'R<', 'ALL', 'P-Q', 'XS', 'M', 'MS');

  IDef: array[0..31] of string6 = (
    'A=0   ', 'B=0   ', 'C=0   ', 'A<>B  ',
    'B=A   ', 'A<>C  ', 'C=B   ', 'C<>B  ',
    'A=C   ', 'A=A+B ', 'A=A+C ', 'A=A+1 ',
    'A=A-B ', 'A=A-1 ', 'A=A-C ', 'C=C+C ',
    'C=C+A ', 'C=C+1 ', 'C=A-C ', 'C=C-1 ',
    'C=0-C ', 'C=-C-1', '?B#0  ', '?C#0  ',
    '?A<C  ', '?A<B  ', '?A#0  ', '?A#C  ',
    'RSHFA ', 'RSHFB ', 'RSHFC ', 'LSHFA ');

  HPCharS1: string[64] =
    // maps the HP41 display characters
    '@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_ |"#$%&`()*+{-}/0123456789†,<=>?';

  HPCharS2: string[64] =
    // maps the HP41 special display characters
    '†abcde¯†††††µ†††††††††¯¯¯¯¯¯µ††††abcdefghijklmnopqrstuvwxyz†††††';


  HPNumChar: string[13] = '0123456789.E';

  CharS: array[0..127] of Integer = (
    // maps HP alpha-print characters to ASCII
    175, 134, 134, 171, 134, 223, 134, 134, 134, 134, 134, 134, 181, 134, 134, 134,
  //  ¯    †    †    «    †    ß    †    †    †    †    †    †    µ    †    †    †
    134, 134, 134, 197, 229, 196, 228, 214, 246, 220, 252, 198, 230, 134, 163, 134,
  //  †    †    †    Å    å    Ä    ä    Ö    ö    Ü    ü    Æ    æ    †    £    †
    032, 033, 034, 035, 036, 037, 038, 039, 040, 041, 042, 043, 044, 045, 046, 047,
  //(sp)   !    "    #    $    %    &    '    (    )    *    +    ,    -    .    /
    048, 049, 050, 051, 052, 053, 054, 055, 056, 057, 058, 059, 060, 061, 062, 063,
  //  0    1    2    3    4    5    6    7    8    9    :    ;    <    =    >    ?
    064, 065, 066, 067, 068, 069, 070, 071, 072, 073, 074, 075, 076, 077, 078, 079,
  //  @    A    B    C    D    E    F    G    H    I    J    K    L    M    N    O
    080, 081, 082, 083, 084, 085, 086, 087, 088, 089, 090, 091, 092, 093, 094, 095,
  //  P    Q    R    S    T    U    V    W    X    Y    Z    [    \    ]    ^    _
    096, 097, 098, 099, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
  //  `    a    b    c    d    e    f    g    h    i    j    k    l    m    n    o
    112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 134, 124, 155, 134, 134);
  //  p    q    r    s    t    u    v    w    x    y    z    †    |    ›    †    †

  // HP Characters maps to ascii and escape sequences
  CharSS: array[0..127] of string =(

  //   00      01      02      03      04      05      06      07
  //   08      09      0A      0B      0C      0D      0E      0F

    '\x00', '\x01', '\x02', '«',    '\x04', 'ß',    '\x06', '\x07',
    '\x08', '\x09', '\x0A', '\x0B', 'µ',    '\x0D', '\x0E', '\x0F',

    '\x10', '\x11', '\x12', 'Å',    'å',    'Ä',    'ä',    'Ö'   ,
    'ö',    'Ü',    'ü',    'Æ',    'æ',    '\x1D', '£',    '\x1F',

    ' '  ,  '!',    '"',    '#',    '$',    '%',    '&',    '''',
    '(',    ')',    '*',    '+',    ',',    '-',    '.',    '/',

    '0'  ,  '1',    '2',    '3',    '4',    '5',    '6',    '7',
    '8',    '9',    ':',    ';',    '<',    '=',    '>',    '?',

    '@'  ,  'A',    'B',    'C',    'D',    'E',    'F',    'G',
    'H',    'I',    'J',    'K',    'L',    'M',    'N',    'O',

    'P'  ,  'Q',    'R',    'S',    'T',    'U',    'V',    'W',
    'X',    'Y',    'Z',    '[',    '\\',   ']',    '^',    '_',

    '\x60', 'a',    'b',    'c',    'd',    'e',    'f',    'g',
    'h',    'i',    'j',    'k',    'l',    'm',    'n',    'o',

    'p'  ,  'q',    'r',    's',    't',    'u',    'v',    'w',
    'x',    'y',    'z',    '\x7B', '|',    '›',    '?',    '\x7F');



  // Addressing of (synthetic) registers and labels
  DatRegT: array[100..127] of String8 = (
    '100'    , '101'    , 'A (102)', 'B (103)', 'C (104)', 'D (105)', 'E (106)', 'F (107)',
    'G (108)', 'H (109)', 'I (110)', 'J (111)', 'T (0)'  , 'Z (1)'  , 'Y (2)'  , 'X (3)'  ,
    'L (4)'  , 'M (5)'  , 'N (6)'  , 'O (7)'  , 'P (8)'  , 'Q (9)'  , '+ (10)' , 'a (11)' ,
    'b (12)' , 'c (13)' , 'd (14)' , 'e (15)');

  OneByte   = 0;
  TwoByte   = 1;
  XROM      = 2;
  GTO       = 3;
  GLOBAL    = 4;
  ThreeByte = 5;
  Text      = 6;
  GotoText  = 7;
  GXIND     = 8;
  Number    = 9;

  HexTable: HexArr = (
    'NULL'   , 'LBL  00', 'LBL  01', 'LBL  02', 'LBL  03', 'LBL  04', 'LBL  05', 'LBL  06',
    'LBL  07', 'LBL  08', 'LBL  09', 'LBL  10', 'LBL  11', 'LBL  12', 'LBL  13', 'LBL  14',
    '0'      , '1'      , '2'      , '3'      , '4'      , '5'      , '6'      , '7'      ,
    '8'      , '9'      , '.'      , ' E'     , '-'      , 'GTO  "' , 'XEQ  "' , 'W    "' ,
    'RCL  00', 'RCL  01', 'RCL  02', 'RCL  03', 'RCL  04', 'RCL  05', 'RCL  06', 'RCL  07',
    'RCL  08', 'RCL  09', 'RCL  10', 'RCL  11', 'RCL  12', 'RCL  13', 'RCL  14', 'RCL  15',
    'STO  00', 'STO  01', 'STO  02', 'STO  03', 'STO  04', 'STO  05', 'STO  06', 'STO  07',
    'STO  08', 'STO  09', 'STO  10', 'STO  11', 'STO  12', 'STO  13', 'STO  14', 'STO  15',
    '+'      , '-'      , '*'      , '/'      , 'X<Y?'   , 'X>Y?'   , 'X«Y?'   , 'S+'     ,
    'S-'     , 'HMS+'   , 'HMS-'   , 'MOD'    , '%'      , '%CH'    , 'P-R'    , 'R-P'    ,
    'LN'     , 'X^2'    , 'SQRT'   , 'Y^X'    , 'CHS'    , 'E^X'    , 'LOG'    , '10^X'   ,
    'E^X-1'  , 'SIN'    , 'COS'    , 'TAN'    , 'ASIN'   , 'ACOS'   , 'ATAN'   , 'DEC'    ,
    '1/X'    , 'ABS'    , 'FACT'   , 'X#0?'   , 'X>0?'   , 'LN1+X'  , 'X<0?'   , 'X=0?'   ,
    'INT'    , 'FRC'    , 'D-R'    , 'R-D'    , '-HMS'   , '-HR'    , 'RND'    , 'OCT'    ,
    'CLS'    , 'X<>Y'   , 'PI'     , 'CLST'   , 'R^'     , 'RDN'    , 'LASTX'  , 'CLX'    ,
    'X=Y?'   , 'X#Y?'   , 'SIGN'   , 'X«0?'   , 'MEAN'   , 'SDEV'   , 'AVIEW'  , 'CLD'    ,
    //-------------------------------------------------------------------------------------
    'DEG'    , 'RAD'    , 'GRAD'   , 'ENTER^' , 'STOP'   , 'RTN'    , 'BEEP'   , 'CLA'    ,
    'ASHF'   , 'PSE'    , 'CLRG'   , 'AOFF'   , 'AON'    , 'OFF'    , 'PROMPT' , 'ADV'    ,
    'RCL  '  , 'STO  '  , 'ST+  '  , 'ST-  '  , 'ST*  '  , 'ST/  '  , 'ISG  '  , 'DSE  '  ,
    'VIEW '  , 'SREG '  , 'ASTO '  , 'ARCL '  , 'FIX  '  , 'SCI  '  , 'ENG  '  , 'TONE '  ,
    'XROM '  , 'XROM '  , 'XROM '  , 'XROM '  , 'XROM '  , 'XROM '  , 'XROM '  , 'XROM '  ,
    'SF   '  , 'CF   '  , 'FS?C '  , 'FC?C '  , 'FS?  '  , 'FC?  '  , ''       , 'SPARE'  ,
    'SPARE'  , 'GTO  00', 'GTO  01', 'GTO  02', 'GTO  03', 'GTO  04', 'GTO  05', 'GTO  06',
    'GTO  07', 'GTO  08', 'GTO  09', 'GTO  10', 'GTO  11', 'GTO  12', 'GTO  13', 'GTO  14',
    'LBL  "' , 'LBL  "' , 'LBL  "' , 'LBL  "' , 'LBL  "' , 'LBL  "' , 'LBL  "' , 'LBL  "' ,
    'LBL  "' , 'LBL  "' , 'LBL  "' , 'LBL  "' , 'LBL  "' , 'LBL  "' , 'X<>  '  , 'LBL  '  ,
    'GTO  '  , 'GTO  '  , 'GTO  '  , 'GTO  '  , 'GTO  '  , 'GTO  '  , 'GTO  '  , 'GTO  '  ,
    'GTO  '  , 'GTO  '  , 'GTO  '  , 'GTO  '  , 'GTO  '  , 'GTO  '  , 'GTO  '  , 'GTO  '  ,
    'XEQ  '  , 'XEQ  '  , 'XEQ  '  , 'XEQ  '  , 'XEQ  '  , 'XEQ  '  , 'XEQ  '  , 'XEQ  '  ,
    'XEQ  '  , 'XEQ  '  , 'XEQ  '  , 'XEQ  '  , 'XEQ  '  , 'XEQ  '  , 'XEQ  '  , 'XEQ  '  ,
    'TEXT0'  , '"'      , '"'      , '"'      , '"'      , '"'      , '"'      , '"'      ,
    '"'      , '"'      , '"'      , '"'      , '"'      , '"'      , '"'      , '"'       );

  TypTable: TypArr = (
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 7, 7, 7,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   {----------------------------------------------}
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 8, 1,
    1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 1, 1,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6);


  MSG_table: array[0..13, 0..2] of string12 = (
    ('018', 'MSGAD ', 'ALPHA DATA'),
    ('022', 'MSGDE ', 'DATA ERROR'),
    ('02D', 'MSGML ', 'MEMORY LOST'),
    ('038', 'MSGNE ', 'NONEXISTENT'),
    ('03C', 'MSGNL ', 'NULL'),
    ('043', 'MSGPR ', 'PRIVATE'),
    ('04F', 'MSGOF ', 'OUT OF RANGE'),
    ('056', 'MSGWR ', 'PACKING'),
    ('05F', 'MSGTA ', 'TRY AGAIN'),
    ('062', 'MSGYES', 'YES'),
    ('064', 'MSGNO ', 'NO'),
    ('067', 'MSGRAM', 'RAM'),
    ('06A', 'MSGROM', 'ROM'),
    ('3FF', 'wrong', 'argument'));
  MSG_maxentries = 13;

var
  Page: RomPage; { ROM to be disassembled }
  DisPage : DisPageTp;
  EndUs: Boolean;
  LinNum: word;
  DISFile: TextFile;
  ROMFat: Array[0..65] of String10;
  ROMFat8K: Array[0..65] of String10;
  ThisXROM: word;
  ThisXROM8K: word;
  Dis8K, In8K: boolean;
  ROMEnd: word;
  LabelTable: Array[0..$1FFF] of String10;
  DisasmPass: integer;
  AddEmptyLine: boolean;
  BasePage, BaseAddress, EndAddress: word;
  BasePage8K, BaseAddress8K, EndAddress8K: word;
  CurrentPage, CurrentBase: word;
  LabelFile, XROMFile: TextFile;
  LabelFileOpen, XROMFileOpen: boolean;
  MODFileBacked: boolean = false;
  DisString: array[1..11] of String;


implementation

end.

