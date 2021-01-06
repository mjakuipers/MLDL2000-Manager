{%RunFlags BUILD-}
unit RomDis;

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
//  ROMDIS.PAS                                                               //
//  HP41 ROM Disassembler                                                    //
//  Ver  Date     Description                                                //
//  0.01 Mar 2007 First draft integrated in M2kM based on old version        //
//  1.10 Apr 2007 User Code Function names supported                         //
//                Message given for Function Name in other Page              //
//  1.11 Feb 2008 Will now directly disassemble usercode before label        //
//                Disassembly prints * before label                          //
//                Will not attempt to disasssemble function names            //
//                Empty line inserted before start of function               //
//                Explains meaning of first two words before User Code       //
//                Improved layout of user code disassembly                   //
//                Added MainFrame labels, XROM, local labels, 8K dis         //
//  1.20 Apr 2008 Fixed some bugs, added 8K assembly                         //
//                Added SkipNOPs,                                            //
//       May 2008 Added text comments to XEQ and LBL                         //
//  1.50 May 2008 Final release                                              //
//  1.51 Aug 2008 Added Label search for local JC/JNC and GOTO/GOSUB         //
//       Nov 2008 Added correct disassembly for ERROR, MSG, MSGA             //
//  1.60 Jan 2010 Modified with new disassembler                             //
//  1.70 Jul 2010 Adapted for Lazarus                                        //
//  1.80 Feb 2018 restarted development                                      //
//                removed Delphi compatibility                               //
//  1.90 Mar 2020 added HP41CL WCMD instruction $1FC                         //
//       Oct 2020 fixed issue with sticky NOP comment                        //
//                                                                           //
//---------------------------------------------------------------------------//

interface

uses
  Globals, HP41_Globals, GlobalConstants, Procs, SysUtils, ListWindow;

var
  Mode, IClass, FirstWord, SecondWord, W, WW, A: word;
  StartAdr: LongWord;
  St, Hulp: string;
  LabLen: integer;


procedure AddLabel(Adr: word; Lab: string);
procedure FindMainFrameFile(Adr: word; var MainLabel, Comment: string);
procedure FindXROMFile(I, J: Integer; var S1, S2: string);
procedure NewDis(var Adr: word);
function GetIType(IType: InstrType): string;
function F(MAR: word): word;

implementation


function GetIType(IType: InstrType): string;
var
  S: string;
begin
  case Itype of
    Tp_Undefined    : S := 'Undef';
    Tp_XROMNumber   : S := 'XROM';
    Tp_FATEntries   : S := 'FATEnt';
    Tp_FATFunction  : S := 'FATFunc';
    Tp_FATEnd       : S := 'FATEnd';
    Tp_FunctionName : S := 'FunNam';
    Tp_Data         : S := 'Data';
    Tp_Instruction1 : S := 'Instr1';
    Tp_Instruction2 : S := 'Instr2';
    Tp_Instruction3 : S := 'Instr3';
    Tp_UserProgram  : S := 'UserP';
    Tp_EntryPoint   : S := 'EntryP';
    Tp_ROMRevision  : S := 'ROMRev';
    Tp_Checksum     : S := 'CheckS';
    Tp_FirstTwo     : S := 'First2';
    Tp_StrDat       : S := 'StrDat';
    Tp_MESLString   : S := 'MESSL';
  else                S := '?????';
  end;
  GetIType := S;
end;

function F(MAR: word): word;
// corrects the address for accessing the DisPage array
begin
  F := MAR and ROMEnd;
end;

procedure ClearMCodeLabel(Adr: word);
// Removes the label if it starts with LB_
begin
  if Pos('LB_', LabelTable[Adr and ROMEnd]) = 1 then
    LabelTable[Adr and ROMEnd] := '          ';
  // for NewDis
  if Pos('LB_', DisPage[Adr and ROMEnd].Lbl) = 1 then
    DisPage[Adr and ROMEnd].Lbl := '';
end;

procedure AddLabel(Adr: word; Lab: string);
// Adds a label to the table if that position was empty
begin
  // code for new disassembler
  if (DisPage[Adr and ROMEnd].Lbl = '') and PrefAutoLabel then
    // no alignment needed
    DisPage[Adr and ROMEnd].Lbl := Lab;
end;

procedure FindMainFrameFile(Adr: word; var MainLabel, Comment: string);
// Find MainFrame label from file
var
  S, LabelComment: string;
  CurrentPg: Char;
  Found: boolean;
  LabelAdr, L: integer;
begin
  MainLabel := '';
  Comment := '';
  LabelComment := '';
  CurrentPg := Hex1(Adr shr 12);
  if LabelFileOpen and PrefMainFrame then begin
    // file is open, so start looking ...
    Reset(LabelFile);        // back to beginning
    Found := false;
    while (not EOF(LabelFile)) and (not Found) do begin
      ReadLn(LabelFile, S);
      L := Length(S);
      if (L > 0) and (S[1] = CurrentPg) then begin
        // now we are in business
        LabelAdr := HexToInt(S);
        S := TrimLeft(S);
        L := Length(s);
        if (L > 0) and (S[1] = ':') then begin
          // This is the Marker for the ROM or Section title
          // Delete the : and get the string
          Delete(S, 1, 1);    // remove the :
          LabelComment := Trim(S);
        end else
          if (LabelAdr = Adr) then Found := true;
      end;
    end;
    if Found then begin
      // label address found, now parse Label name and comment
      MainLabel := GetFirstWord(S);   // leading spaces are already removed
      Comment := Trim(S);
      if Comment = '' then Comment := LabelComment;
      if Pos(';', Comment) = 1 then
        // remove leading ; and any spaces
        Delete(Comment, 1, 1);
      Comment := TrimLeft(Comment);
    end;
  end;
end;

procedure FindXROMFile(I, J: Integer; var S1, S2: string);
  // Finds XROM I, J from XROM.TXT
  // Returns: S1: function name, string is empty when not found
  //          S2: comments
var
  S, SS, SSS, XROMComment: string;
  S3: string5;
  S4: string2;
  Found: boolean;
  L, MatchXR: integer;
begin
  S1 := '';
  S2 := '';
  XROMComment := '';
  S3 := Int2(I) + ',' + Int2(J);       // Complete XROM number nn,mm
  S4 := Int2(I);                       // XROM ID only
  if XROMFileOpen and PrefXROM then begin
    Reset(XROMFile);
    Found := false;
    while (not EOF(XROMFile)) and (not Found) do begin
      ReadLn(XROMFile, S);
      L := Length(S);
      SS := GetFirstWord(S);    // read first word from line, could be XROM
      SSS := GetFirstWord(S);   // read 2nd word from line, could be xx,nn
      MatchXR := Pos(S4, SSS);
      if (L > 0) and (SS = 'XROM') and (MatchXR = 1) then begin
        // We are at least in the right XROM
        // First check if we have a comment
        L := Length(S);
        if (L > 0 ) and (S[1] = ':') then begin
          // this is the marker for the XROM name or comment
          Delete(S, 1, 1);  // remove the :
          XROMComment := Trim(S);
        end else if (SSS = S3) then begin
          // This is a real match for the complete XROM number
          S1 := GetFirstWord(S);  // also handle quotes
          S2 := Trim(S);          //this is the comment
          Found := true;
        end;
        if S2 = ''  then S2 := XROMComment;
        if Pos(';', S2) = 1 then
        // remove leading ; and any spaces
          Delete(S2, 1, 1);
        S2 := TrimLeft(S2);
      end;
    end;
  end;
end;

procedure NewDis(var Adr: word);
// This is the main loop of the New Disassembler
type
  TFunPos = (ThisROM, NextROM, PrevROM, OutSide);
var
  j: integer;
  Point, I: longword;
  S: string;
  Accumulator: word;
  prompt: word;
  Cmt: String;
  FunPos: TFunPos;

  procedure DecodeClass0(var Adr: word; I: word);
  // Class 0 instructions have the following form :
  //      -- -- -- -- -- -- -- -- 0 0
  //      P3 P2 P1 P0 I3 I2 I1 I0
  //          \/          \/
  //      parameter   instruction
  var
    InstrTyp, InstrParam: word;
    NextInstr, ThirdInstr: word;
    SaveAdr: word;
    Comment: String;
    NOPCounter: integer;
    TempID: InstrType;
    BeginNOP: word;
  begin
    DisPage[F(Adr)].HexCodes := Hex3(I) + ' ';
    DisPage[F(Adr)].WrdTp := Tp_Instruction1;
    InstrTyp := (I shr 2) and $000F;
    InstrParam := (I shr 6) and $000F;
    Comment := '';
    case I of
      $000: begin                                              // this is a NOP
              BeginNOP := Adr;
              DisPage[F(Adr)].Mnem := Mnemonics[0, PrefMnem + 1];
              // now check if this is the NOP after a function name
              // and if the second instruction is also a NOP
              NextInstr := DisPage[F(Adr + 1)].HexCode;
              ThirdInstr := DisPage[F(Adr + 2)].HexCode;
              if (Adr <> $0000) and ((Adr and $0FFF) <> $0FFF) then begin
                // Prevents disassembly outside the ROM
                TempID := DisPage[F(Adr - 1)].WrdTp;
                if TempID = Tp_FunctionName then begin
                  Comment := 'NOT programmable and ';
                  if NextInstr = $000 then
                    Comment := Comment + 'Immediate'
                  else
                    Comment := Comment + 'NULLable';
                end else if ((Adr and $0FFF) < $0FF4) then begin
                  // This was not a function name but a regular NOP
                  // if there are 3 or more consecutive NOPs without label, skip them
                  if (NextInstr = 0) and (ThirdInstr = 0) and
                     (DisPage[F(Adr + 1)].Lbl = '') and
                     (DisPage[F(Adr + 2)].Lbl = '') and
                     PrefSkipNOPs then begin
                    // now we have 3 NOPS without label!
                    NOPCounter := 1;
                    SaveAdr := Adr;
                    Adr := Adr + 1;
                    while (DisPage[F(Adr)].HexCode = 0) and
                          (DisPage[F(Adr)].Lbl = '') and
                          (Adr and $0FFF < $0FF4) do begin
                      DisPage[F(Adr)].DisTp := Dt_SkipLine;
                      Inc(Adr);
                      Inc(NOPCounter);
                    end;
                    Adr := Adr - 1;
                    //             8038 - 8057  000 NOP  54 *
                    DisPage[F(BeginNOP)].Mnem := Mnemonics[290, PrefMnem + 1];
                    Comment := SStr(NOPCounter) + ' consecutive NOPs from $';
                    Comment := Comment + Hex4(SaveAdr) + ' to $' + Hex4(Adr);
                    if PrefSDK41Mode then begin
                      DisPage[F(BeginNOP)].MnemArg1 := SStr(NOPCounter);
                    end else begin
                      DisPage[F(BeginNOP)].MnemArg2 := SStr(NOPCounter);
                      DisPage[F(BeginNOP)].MnemArg1 := 'NOP';
                    end;
                  end;
                end;
              end;
              DisPage[F(BeginNOP)].Comment := Comment;
            end;
      $030: DisPage[F(Adr)].Mnem := Mnemonics[1, PrefMnem + 1];   // HEPAX ROMBLK
      $040: DisPage[F(Adr)].Mnem := Mnemonics[2, PrefMnem + 1];   // MLDL WROM
      $100: DisPage[F(Adr)].Mnem := Mnemonics[3, PrefMnem + 1];   // ENBANK1
      $180: DisPage[F(Adr)].Mnem := Mnemonics[4, PrefMnem + 1];   // ENBANK2
      $140: DisPage[F(Adr)].Mnem := Mnemonics[5, PrefMnem + 1];   // ENBANK3
      $1C0: DisPage[F(Adr)].Mnem := Mnemonics[6, PrefMnem + 1];   // ENBANK4
      $130: begin                                                 // LDI S&X
              DisPage[F(Adr)].HexCodes := DisPage[F(Adr)].HexCodes +
                                      Hex3(DisPage[F(Adr +1)].HexCode) + ' ';
              DisPage[F(Adr)].Mnem := Mnemonics[7, PrefMnem + 1];
              Adr := Adr + 1;
              DisPage[F(Adr)].WrdTp := Tp_Instruction2;
              DisPage[F(Adr)].DisTp := Dt_SkipLine;
              DisPage[F(Adr - 1)].MnemArg1 := Hex3(DisPage[F(Adr)].HexCode);
        end;
      $1F0: DisPage[F(Adr)].Mnem := Mnemonics[8, PrefMnem + 1];   // HEPAX WPTOG
      $3C4: DisPage[F(Adr)].Mnem := Mnemonics[9, PrefMnem + 1];   // ST=0
      $3C8: DisPage[F(Adr)].Mnem := Mnemonics[10, PrefMnem + 1];  // CLRKEY
      $3CC: DisPage[F(Adr)].Mnem := Mnemonics[11, PrefMnem + 1];  // ?KEY
      $3D4: DisPage[F(Adr)].Mnem := Mnemonics[12, PrefMnem + 1];  // R=R-1
      $3DC: DisPage[F(Adr)].Mnem := Mnemonics[13, PrefMnem + 1];  // R=R+1
      $1FC: DisPage[F(Adr)].Mnem := Mnemonics[299, PrefMnem + 1]; // HP41CL WCMD

      $080, $0C0, $1C4, $1C8, $1CC, $1D4, $1DC, $1EC, $3FC:
            begin                                                 // various UNUSED
              if PrefSDK41Mode then
                DisPage[F(Adr)].Mnem := '#' + Hex3(I)
              else
                { TODO : UNDEF from menonics table }
                DisPage[F(Adr)].Mnem := 'UNDEF' + Hex3(I);
            end;
    else
      case InstrTyp of
        $0: begin                                                 // HPIL=C
              DisPage[F(Adr)].Mnem := Mnemonics[14, PrefMnem + 1];
              DisPage[F(Adr)].MnemArg1 := SStr(InstrParam - 8);
            end;
        $1: begin                                                 // CLRF
              DisPage[F(Adr)].Mnem := Mnemonics[24, PrefMnem + 1];
              DisPage[F(Adr)].MnemArg1 := ParamDefFlag[InstrParam];
            end;
        $2: begin                                                 // SETF
              DisPage[F(Adr)].Mnem := Mnemonics[25, PrefMnem + 1];
              DisPage[F(Adr)].MnemArg1 := ParamDefFlag[InstrParam];
            end;
        $3: begin                                                 // ?FSET
              DisPage[F(Adr)].Mnem := Mnemonics[26, PrefMnem + 1];
              DisPage[F(Adr)].MnemArg1 := ParamDefFlag[InstrParam];
            end;
        $4: begin                                                 // LD@R
              DisPage[F(Adr)].Mnem := Mnemonics[27, PrefMnem + 1];
              DisPage[F(Adr)].MnemArg1 := Hex1(InstrParam);
            end;
        $5: begin                                                 // ?R=
              DisPage[F(Adr)].Mnem := Mnemonics[28, PrefMnem + 1];
              DisPage[F(Adr)].MnemArg1 := ParamDefFlag[InstrParam];
            end;
        $6:                                                       // various
            DisPage[F(Adr)].Mnem := Mnemonics[29 + InstrParam, PrefMnem + 1];
        $7: begin                                                 // R=
              DisPage[F(Adr)].Mnem := Mnemonics[45, PrefMnem + 1];
              DisPage[F(Adr)].MnemArg1 := ParamDefFlag[InstrParam];
            end;
        $8:                                                       // various
            DisPage[F(Adr)].Mnem := Mnemonics[46 + InstrParam, PrefMnem + 1];
        $9: begin                                                 // SELPF
              DisPage[F(Adr)].Mnem := Mnemonics[62, PrefMnem + 1];
              DisPage[F(Adr)].MnemArg1 := Hex1(InstrParam);
              Comment := 'Peripheral ';
              case InstrParam of
                0: Comment := Comment + '0: HP-IL';   // info from EM41 sources
                1: Comment := Comment + '1: HP-IL';   // and HEPAX manual
                2: Comment := Comment + '2: HP-IL';
                3: Comment := Comment + '3: HP-IL';
                4: Comment := Comment + '4: HP-IL';
                5: Comment := Comment + '5: HP-IL';
                6: Comment := Comment + '6: HP-IL';
                7: Comment := Comment + '7: HP-IL';
                8: Comment := Comment + '8: HP-IL';
                9: Comment := Comment + '9: HP82143A PRINTER';
                else Comment := Comment + 'unknown peripheral';
              end;
              DisPage[F(Adr)].Comment	:= Comment;
            end;
        $A: begin                                                 // WRIT
              DisPage[F(Adr)].Mnem := Mnemonics[63, PrefMnem + 1];
              if PrefSDK41Mode then begin
                DisPage[F(Adr)].MnemArg1 := SStr(InstrParam);
                DisPage[F(Adr)].Comment :=  ParamDefRegs[InstrParam];
              end else
                DisPage[F(Adr)].MnemArg1 := ParamDefRegs[InstrParam];
            end;
        $B: begin                                                 // ?FI
              DisPage[F(Adr)].Mnem := Mnemonics[64, PrefMnem + 1];
              DisPage[F(Adr)].MnemArg1 := ParamDefFlag[InstrParam];
              Comment := 'Perpiheral Flag';
              case InstrParam of
                 2: Comment := '?EDAV, HP82242 IR led';           // ?FI  5
                 3: Comment := '?ORAV, HP-IL Output Reg';         // ?FI 10
                 4: Comment := '?FRAV, HP-IL Frame';              // ?FI  8
                 5: Comment := '?IFCR, HP-IL Interface Clear';    // ?FI  6
                 8: Comment := '?WNDB, WAND Buffer';              // ?FI  2
                 9: Comment := '?FRNS, HP-IL Frame Return';       // ?FI  9
                10: Comment := '?SRQR, HP-IL Service Request';    // ?FI  7
                11: Comment := '?SERV, Peripheral Service';       // ?FI 13
                12: Comment := '?CRDR, Card Reader';              // ?FI  1
                13: Comment := '?ALM, Timer Alarm';               // ?FI 12
                14: Comment := '?PBSY, HP82143 Printer busy';     // ?FI  0
              end;
              DisPage[F(Adr)].Comment	:= Comment;
            end;
        $C:                                                        // various
            DisPage[F(Adr)].Mnem := Mnemonics[65 + InstrParam, PrefMnem + 1];
        $D: begin                                                  // various UNUSED
              if PrefSDK41Mode then
                DisPage[F(Adr)].Mnem := '#' + Hex3(I)
              else
                DisPage[F(Adr)].Mnem := 'UNDEF' + Hex3(I);
            end;
        $E: begin                                                  // READ
              DisPage[F(Adr)].Mnem := Mnemonics[81, PrefMnem + 1];
              if PrefSDK41Mode then begin
                DisPage[F(Adr)].MnemArg1 := SStr(InstrParam);
                DisPage[F(Adr)].Comment :=  ParamDefRegs[InstrParam];
              end else
                DisPage[F(Adr)].MnemArg1 := ParamDefRegs[InstrParam];
            end;
        $F: begin                                                  // RCR
              DisPage[F(Adr)].Mnem := Mnemonics[83, PrefMnem + 1];
              DisPage[F(Adr)].MnemArg1 := ParamDefFlag[InstrParam];
            end;
      end;
    end;
  end; // DecodeClass0

  procedure DecodeClass1(var Adr: word; I: word);
  // Class 1 instructions have the following form :
  //      first word:   --- --- --- --- --- --- --- ---  0   1
  //                    A7  A6  A5  A4  A3  A2  A1  A0
  //      second word:  --- --- --- --- --- --- --- --- --- ---
  //                    A15 A14 A13 A12 A11 A10  A9  A8  G   C
  //                                \/                   /   \
  //                           address bits       GOTO/XEQ   condition
  //      G C
  //      0 0   ?NC XQ
  //      0 1   ?C  XQ
  //      1 0   ?NC GO
  //      1 1   ?C  GO
  var
    Dest, SecondWord, SW, ThirdWord, Point: word;
    FirstAdr: word;
    IsGOTO: boolean;
    Hulp, Hulp1, Hulp2, Hulp3, Comment: string;
    SecondID: InstrType;
    MSGIdx: integer;
  begin
    // Added to prevent disassembly of non-instruction
    FirstAdr := Adr;
    SecondID := DisPage[F(Adr + 1)].WrdTp;
    SecondWord := DisPage[F(Adr + 1)].HexCode;
    SW := SecondWord;
    IsGOTO := (SW and $002) = $002;      // is this GOTO or XEQ
    DisPage[F(Adr)].Comment := '';
    Comment := '';
    St := '';
    if (SecondID <> TP_Undefined) and (SecondID <> Tp_Instruction2) then begin
      // SecondWord is NOT the second word of an instruction, but something else
      // Current Word will be treated as data
      DisPage[F(Adr)].WrdTp := Tp_Data;
      St := '';
    end else begin
      Dest := Lo(I shr 2);
      Hulp3 := Hex3(DisPage[F(FirstAdr)].HexCode) + ' ' +
               Hex3(DisPage[F(FirstAdr + 1)].HexCode) + ' ';
        // used for building HexCodes
      Adr := Adr + 1;
      DisPage[F(FirstAdr)].WrdTp := Tp_Instruction1;
      DisPage[F(Adr)].WrdTp := Tp_Instruction2;
      DisPage[F(Adr)].DisTp := Dt_SkipLine;
      Hulp := Mnemonics[84 + (SecondWord and $0003), PrefMnem + 1] + Space;
      DisPage[F(FirstAdr)].Mnem := Hulp;
      SecondWord := Swap(Lo(SecondWord shr 2));
      Dest := Dest or SecondWord;

      // now see if it is a ?NC XQ to a certain address, so it is in fact
      // a three byte GOTO or GOSUB. If so, decode the third byte also.
      // addresses currently recognised:
      // label   address     action
      // -----   -------     ------
      // GOL0      23D0      relocatable GOTO 1st 1K of ROM
      // GOLSUB0   23D2      relocatable GOSUB 1st 1K of ROM
      // GOL1      23D9      relocatable GOTO 2nd 1K of ROM
      // GOLSUB1   23DB      relocatable GOSUB 2nd 1K of ROM
      // GOL2      23E2      relocatable GOTO 3rd 1K of ROM
      // GOLSUB2   23E4      relocatable GOSUB 3rd 1K of ROM
      // GOL3      23EB      relocatable GOTO 4th 1K of ROM
      // GOLSUB3   23ED      relocatable GOSUB 4th 1K of ROM
      // GOLONG    0FDA      relocatable GOTO same 1K of ROM (or 0FD9, GOLNGH)
      // GOSUB     0FDD      relocatable GOSUB same 1K of ROM (or 0FDE, GOSUBH)
      //
      // MESSL     07EF      left shift the following string into the display
      //                     from the right. The last character in the string
      //                     has $200 added to the character value
      // ?NC XQ    22F5      error display routine ERROR
      // ?NC XQ    1C6B      message display routine MSG
      // ?NC XQ    1C6C      message display routine MSGA

      if Dest = $23D0 then begin              // relocatable GOTO 1st 1K of ROM
        Adr := Adr + 1;
        Hulp3 := Hulp3 + Hex3(DisPage[F(Adr)].HexCode);
        DisPage[F(Adr)].WrdTp := Tp_Instruction3;
        DisPage[F(Adr)].DisTp := Dt_SkipLine;
        ThirdWord := DisPage[F(Adr)].HexCode + CurrentBase;
        AddLabel(ThirdWord, 'LB_' + Hex4(ThirdWord));
        Hulp2 := DisPage[F(ThirdWord)].Lbl;
        DisPage[F(FirstAdr)].Mnem := Mnemonics[89, PrefMnem + 1];
        DisPage[F(FirstAdr)].MnemArg1 := Hex4(ThirdWord);
        DisPage[F(FirstAdr)].RefLbl := Hulp2;
        DisPage[F(FirstAdr)].RefAdr := ThirdWord;
        DisPage[F(FirstAdr)].RefLabel := true;
        DisPage[F(FirstAdr)].Comment :=  Hulp + Hex4(Dest) + ', address in 1st Quad';

      end else if Dest = $23D2 then begin     // relocatable GOSUB 1st 1K of ROM
        Adr := Adr + 1;
        Hulp3 := Hulp3 + Hex3(DisPage[F(Adr)].HexCode);
        DisPage[F(Adr)].WrdTp := Tp_Instruction3;
        DisPage[F(Adr)].DisTp := Dt_SkipLine;
        ThirdWord := DisPage[F(Adr)].HexCode + CurrentBase;
        AddLabel(ThirdWord, 'LB_' + Hex4(ThirdWord));
        Hulp2 := DisPage[F(ThirdWord)].Lbl;
        DisPage[F(FirstAdr)].Mnem := Mnemonics[88, PrefMnem + 1];
        DisPage[F(FirstAdr)].MnemArg1 := Hex4(ThirdWord);
        DisPage[F(FirstAdr)].RefLbl := Hulp2;
        DisPage[F(FirstAdr)].RefAdr := ThirdWord;
        DisPage[F(FirstAdr)].RefLabel := true;
        DisPage[F(FirstAdr)].Comment :=  Hulp + Hex4(Dest) + ', address in 1st Quad';

      end else if Dest = $23D9 then begin     // relocatable GOTO 2nd 1K of ROM
        Adr := Adr + 1;
        Hulp3 := Hulp3 + Hex3(DisPage[F(Adr)].HexCode);
        DisPage[F(Adr)].WrdTp := Tp_Instruction3;
        DisPage[F(Adr)].DisTp := Dt_SkipLine;
        ThirdWord := DisPage[F(Adr)].HexCode + CurrentBase + $0400;
        AddLabel(ThirdWord, 'LB_' + Hex4(ThirdWord));
        Hulp2 := DisPage[F(ThirdWord)].Lbl;
        DisPage[F(FirstAdr)].Mnem := Mnemonics[89, PrefMnem + 1];
        DisPage[F(FirstAdr)].MnemArg1 := Hex4(ThirdWord);
        DisPage[F(FirstAdr)].RefLbl := Hulp2;
        DisPage[F(FirstAdr)].RefAdr := ThirdWord;
        DisPage[F(FirstAdr)].RefLabel := true;
        DisPage[F(FirstAdr)].Comment :=  Hulp + Hex4(Dest) + ', address in 2nd Quad';

      end else if Dest = $23DB then begin     // relocatable GOSUB 2nd 1K of ROM
        Adr := Adr + 1;
        Hulp3 := Hulp3 + Hex3(DisPage[F(Adr)].HexCode);
        DisPage[F(Adr)].WrdTp := Tp_Instruction3;
        DisPage[F(Adr)].DisTp := Dt_SkipLine;
        ThirdWord := DisPage[F(Adr)].HexCode + CurrentBase + $0400;
        AddLabel(ThirdWord, 'LB_' + Hex4(ThirdWord));
        Hulp2 := DisPage[F(ThirdWord)].Lbl;
        DisPage[F(FirstAdr)].Mnem := Mnemonics[88, PrefMnem + 1];
        DisPage[F(FirstAdr)].MnemArg1 := Hex4(ThirdWord);
        DisPage[F(FirstAdr)].RefLbl := Hulp2;
        DisPage[F(FirstAdr)].RefAdr := ThirdWord;
        DisPage[F(FirstAdr)].RefLabel := true;
        DisPage[F(FirstAdr)].Comment :=  Hulp + Hex4(Dest) + ', address in 2nd Quad';

      end else if Dest = $23E2 then begin     // relocatable GOTO 3rd 1K of ROM
        Adr := Adr + 1;
        Hulp3 := Hulp3 + Hex3(DisPage[F(Adr)].HexCode);
        DisPage[F(Adr)].WrdTp := Tp_Instruction3;
        DisPage[F(Adr)].DisTp := Dt_SkipLine;
        ThirdWord := DisPage[F(Adr)].HexCode + CurrentBase + $0800;
        AddLabel(ThirdWord, 'LB_' + Hex4(ThirdWord));
        Hulp2 := DisPage[F(ThirdWord)].Lbl;
        DisPage[F(FirstAdr)].Mnem := Mnemonics[89, PrefMnem + 1];
        DisPage[F(FirstAdr)].MnemArg1 := Hex4(ThirdWord);
        DisPage[F(FirstAdr)].RefLbl := Hulp2;
        DisPage[F(FirstAdr)].RefAdr := ThirdWord;
        DisPage[F(FirstAdr)].RefLabel := true;
        DisPage[F(FirstAdr)].Comment :=  Hulp + Hex4(Dest) + ', address in 3rd Quad';

      end else if Dest = $23E4 then begin     // relocatable GOSUB 3rd 1K of ROM
        Adr := Adr + 1;
        Hulp3 := Hulp3 + Hex3(DisPage[F(Adr)].HexCode);
        DisPage[F(Adr)].WrdTp := Tp_Instruction3;
        DisPage[F(Adr)].DisTp := Dt_SkipLine;
        ThirdWord := DisPage[F(Adr)].HexCode + CurrentBase + $0800;
        AddLabel(ThirdWord, 'LB_' + Hex4(ThirdWord));
        Hulp2 := DisPage[F(ThirdWord)].Lbl;
        DisPage[F(FirstAdr)].Mnem := Mnemonics[88, PrefMnem + 1];
        DisPage[F(FirstAdr)].MnemArg1 := Hex4(ThirdWord);
        DisPage[F(FirstAdr)].RefLbl := Hulp2;
        DisPage[F(FirstAdr)].RefAdr := ThirdWord;
        DisPage[F(FirstAdr)].RefLabel := true;
        DisPage[F(FirstAdr)].Comment :=  Hulp + Hex4(Dest) + ', address in 3rd Quad';

      end else if Dest = $23EB then begin     // relocatable GOTO 4th 1K of ROM
        Adr := Adr + 1;
        Hulp3 := Hulp3 + Hex3(DisPage[F(Adr)].HexCode);
        DisPage[F(Adr)].WrdTp := Tp_Instruction3;
        DisPage[F(Adr)].DisTp := Dt_SkipLine;
        ThirdWord := DisPage[F(Adr)].HexCode + CurrentBase + $0C00;
        AddLabel(ThirdWord, 'LB_' + Hex4(ThirdWord));
        Hulp2 := DisPage[F(ThirdWord)].Lbl;
        DisPage[F(FirstAdr)].Mnem := Mnemonics[89, PrefMnem + 1];
        DisPage[F(FirstAdr)].MnemArg1 := Hex4(ThirdWord);
        DisPage[F(FirstAdr)].RefLbl := Hulp2;
        DisPage[F(FirstAdr)].RefAdr := ThirdWord;
        DisPage[F(FirstAdr)].RefLabel := true;
        DisPage[F(FirstAdr)].Comment :=  Hulp + Hex4(Dest) + ', address in 4th Quad';

      end else if Dest = $23ED then begin     // relocatable GOSUB 4th 1K of ROM
        Adr := Adr + 1;
        Hulp3 := Hulp3 + Hex3(DisPage[F(Adr)].HexCode);
        DisPage[F(Adr)].WrdTp := Tp_Instruction3;
        DisPage[F(Adr)].DisTp := Dt_SkipLine;
        ThirdWord := DisPage[F(Adr)].HexCode + CurrentBase + $0C00;
        AddLabel(ThirdWord, 'LB_' + Hex4(ThirdWord));
        Hulp2 := DisPage[F(ThirdWord)].Lbl;
        DisPage[F(FirstAdr)].Mnem := Mnemonics[88, PrefMnem + 1];
        DisPage[F(FirstAdr)].MnemArg1 := Hex4(ThirdWord);
        DisPage[F(FirstAdr)].RefLbl := Hulp2;
        DisPage[F(FirstAdr)].RefAdr := ThirdWord;
        DisPage[F(FirstAdr)].RefLabel := true;
        DisPage[F(FirstAdr)].Comment :=  Hulp + Hex4(Dest) + ', address in 4th Quad';

      end else if ((Dest = $0FDA) or (Dest = $0FD9)) then begin
        // relocatable GOTO same 1K of ROM
        Adr := Adr + 1;
        Hulp3 := Hulp3 + Hex3(DisPage[F(Adr)].HexCode);
        DisPage[F(Adr)].WrdTp := Tp_Instruction3;
        DisPage[F(Adr)].DisTp := Dt_SkipLine;
        ThirdWord := DisPage[F(Adr)].HexCode + CurrentBase + (Adr and $0C00);
        AddLabel(ThirdWord, 'LB_' + Hex4(ThirdWord));
        Hulp2 := DisPage[F(ThirdWord)].Lbl;
        DisPage[F(FirstAdr)].Mnem := Mnemonics[89, PrefMnem + 1];
        DisPage[F(FirstAdr)].MnemArg1 := Hex4(ThirdWord);
        DisPage[F(FirstAdr)].RefLbl := Hulp2;
        DisPage[F(FirstAdr)].RefAdr := ThirdWord;
        DisPage[F(FirstAdr)].RefLabel := true;
        DisPage[F(FirstAdr)].Comment :=  Hulp + Hex4(Dest) + ', address in same Quad';

      end else if ((Dest = $0FDE) or (Dest = $0FDD)) then begin
        // relocatable GOSUB same 1K of ROM
        Adr := Adr + 1;
        Hulp3 := Hulp3 + Hex3(DisPage[F(Adr)].HexCode);
        DisPage[F(Adr)].WrdTp := Tp_Instruction3;
        DisPage[F(Adr)].DisTp := Dt_SkipLine;
        ThirdWord := DisPage[F(Adr)].HexCode + CurrentBase + (Adr and $0C00);
        AddLabel(ThirdWord, 'LB_' + Hex4(ThirdWord));
        Hulp2 := DisPage[F(ThirdWord)].Lbl;
        DisPage[F(FirstAdr)].Mnem := Mnemonics[88, PrefMnem + 1];
        DisPage[F(FirstAdr)].MnemArg1 := Hex4(ThirdWord);
        DisPage[F(FirstAdr)].RefLbl := Hulp2;
        DisPage[F(FirstAdr)].RefAdr := ThirdWord;
        DisPage[F(FirstAdr)].RefLabel := true;
        DisPage[F(FirstAdr)].Comment :=  Hulp + Hex4(Dest) + ', address in same Quad';

      end else if (Dest = $07EF) and not IsGOTO then begin
        // left shift the following string into the display
        DisPage[F(FirstAdr)].MnemArg1 := 'MESSL';
        DisPage[F(FirstAdr)].RefAdr := Dest;
        DisPage[F(FirstAdr)].RefLbl := 'MESSL';
        DisPage[F(FirstAdr)].RefLabel := true;
        Hulp2 := '';
        Point := Adr;
        St := St;
        repeat
          Point := Point + 1;
          DisPage[F(Point)].WrdTp := Tp_MESLString;
          DisPage[F(Point)].DisTp := Dt_SkipLine;
          St := St + HPChar(DisPage[F(Point)].HexCode);
          Hulp2 := Hulp2 + Hex3(DisPage[F(Point)].HexCode) + ' ';
        until ((DisPage[F(Point)].HexCode > $00FF) or
               (DisPage[F(Point)].HexCode = $0000));
        // Also stop when there is $000 in the string,
        DisPage[F(FirstAdr)].MnemArg2 := '"' + St + '"';
        // make the string a bit longer, better for the listing
        Adr := Point;

        if PrefMESLArg then begin
          DisPage[F(FirstAdr + 2)].DisTp := Dt_Default;
          DisPage[F(FirstAdr + 2)].Mnem := Mnemonics[273 +19, PrefMnem + 1];
          DisPage[F(FirstAdr + 2)].MnemArg1 := '"' + St + '"';
          DisPage[F(FirstAdr + 2)].Comment := Hulp2;
          DisPage[F(FirstAdr + 2)].HexCodes := Hulp2;
          DisPage[F(FirstAdr)].MnemArg2 := '';
        end else begin
          DisPage[F(Adr)].WrdTp := Tp_Instruction3;
          DisPage[F(Adr)].DisTp := Dt_SkipLine;
          DisPage[F(FirstAdr)].Comment := Hulp2;
          ExpandSpaces(DisPage[F(FirstAdr)].MnemArg2, 6);
          Hulp3 := Hulp3 + Hulp2;
        end;

      end else if ((Dest = $22F5) or (Dest = $1C6B) or (Dest = $1C6C)) and not IsGOTO then begin
        // ?NC XQ    22F5      error display routine ERROR
        // ?NC XQ    1C6B      message display routine MSG
        // ?NC XQ    1C6C      message display routine MSGA
        //   MSG_table: array[0..12, 0..2] of string12
        Adr := Adr + 1;
        ThirdWord := DisPage[F(Adr)].HexCode;
        DisPage[F(FirstAdr)].Mnem := Mnemonics[84, PrefMnem + 1];
        Hulp1 := Hex4(Dest);
        case Dest of
          $22F5: Hulp := 'ERROR';
          $1C6B: Hulp := 'MSG';
          $1C6C: Hulp := 'MSGA';
          else FindMainFrameFile(Dest, Hulp1, Comment);  // should never happen
        end;
        case ThirdWord of
          $018: MSGIdx :=  0; // 'MSGAD ', 'ALPHA DATA'
          $022: MSGIdx :=  1; // 'MSGDE ', 'DATA ERROR'),
          $02D: MSGIdx :=  2; // 'MSGML ', 'MEMORY LOST'),
          $038: MSGIdx :=  3; // 'MSGNE ', 'NONEXISTENT'),
          $03C: MSGIdx :=  4; // 'MSGNL ', 'NULL'),
          $043: MSGIdx :=  5; // 'MSGPR ', 'PRIVATE'),
          $04F: MSGIdx :=  6; // 'MSGOF ', 'OUT OF RANGE'),
          $056: MSGIdx :=  7; // 'MSGWR ', 'PACKING'),
          $05F: MSGIdx :=  8; // 'MSGTA ', 'TRY AGAIN'),
          $062: MSGIdx :=  9; // 'MSGYES', 'YES'),
          $064: MSGIdx := 10; // 'MSGNO ', 'NO'),
          $067: MSGIdx := 11; // 'MSGRAM', 'RAM'),
          $06A: MSGIdx := 12; // 'MSGROM', 'ROM'));
          else  MSGIdx := 13; // this is a situation with a wrong argument
        end;
        DisPage[F(FirstAdr)].MnemArg1 := Hex4(Dest);
        DisPage[F(FirstAdr)].RefLbl := Hulp;
        DisPage[F(FirstAdr)].RefAdr := Dest;
        DisPage[F(FirstAdr)].RefLabel := true;
        Comment := Hulp1 + ': ';
        if MSGIdx > MSG_maxentries then MSGIdx := MSG_maxentries;
        Hulp := Hulp + MSG_table[MSGIdx, 1];
        Comment := Comment + 'display message "' + MSG_table[MSGIdx, 2] + '"';
        DisPage[F(FirstAdr)].MnemArg2 := MSG_table[MSGIdx, 1];
        // DisPage[F(FirstAdr)].Comment := Comment;
        if PrefMESLArg then begin
          DisPage[F(Adr)].WrdTp := Tp_Data;
          DisPage[F(Adr)].MnemArg2 := MSG_table[MSGIdx, 1];
          DisPage[F(FirstAdr)].MnemArg2 := '';
          Adr := Adr - 1;
        end else begin
          DisPage[F(Adr)].WrdTp := Tp_Instruction3;
          Hulp3 := Hulp3 + Hex3(ThirdWord);
          DisPage[F(Adr)].DisTp := Dt_SkipLine;
          DisPage[F(FirstAdr)].MnemArg2 := MSG_table[MSGIdx, 1];
        end;

      end else begin
        // normal GO/XQ, find MainFrame Label
        St := St + EmptyData + Hulp;
        FindMainFrameFile(Dest, Hulp1, Comment);
        if Comment <> '' then Comment := Comment;

        // Add label to our collection if this in the same page
        // or in the same 8K block
        if (BaseAddress = (Dest and $F000)) or
            (Dis8K and (BaseAddress = (Dest and $E000))) then begin
          if Hulp1 = '' then Hulp1 := 'LB_' + Hex4(Dest);
          AddLabel(Dest, Hulp1);
        end;

        Hulp := Hulp1;

        DisPage[F(FirstAdr)].MnemArg1 := Hex4(Dest);
        DisPage[F(FirstAdr)].RefLbl := Hulp;
        DisPage[F(FirstAdr)].RefAdr := Dest;
        DisPage[F(FirstAdr)].RefLabel := (Hulp <> '');

      end;
    end;
    DisPage[F(FirstAdr)].Comment := DisPage[F(FirstAdr)].Comment + Comment;
    DisPage[F(FirstAdr)].HexCodes := Hulp3;

  end; // DecodeClass1

  procedure DecodeClass2(I: word);
  // Class 2 instructions have the following form :
  //      -- -- -- -- -- -- -- -- 1 0
  //      I4 I3 I2 I1 I0 F2 F1 F0
  //            \/          \/
  //       instruction     field
  var
    Field, Instruct: word;
  begin
    DisPage[F(Adr)].WrdTp := Tp_Instruction1;
    DisPage[F(Adr)].HexCodes := Hex3(DisPage[F(Adr)].HexCode);
    Field := (I shr 2) and $0007;
    Instruct := (I shr 5) and $001F;
    DisPage[F(Adr)].Mnem := Mnemonics[90 + Instruct, PrefMnem + 1];
    if PrefSDK41Mode then
      DisPage[F(Adr)].MnemArg1 := FieldDefSDK41[Field]
    else
      DisPage[F(Adr)].MnemArg1 := FieldDef[Field];
  end; // DecodeClass2

  procedure DecodeClass3(Adr: word; I: word);
  var
    // AdrI: Integer absolute Adr;
    // iI: Integer absolute I;
    Dest: word;
    Mn: String;
    // Class 3 instructions have the following form :
    //      -- -- -- -- -- -- -- -- 1 1
    //      D6 D5 D4 D3 D2 D1 D0  C
    //               \/           \
    //      signed displacement   condition: 0 - JNC, Jump if No Carry
    //                                       1 - JC,  Jump if Carry
  begin
    DisPage[F(Adr)].WrdTp := Tp_Instruction1;
    DisPage[F(Adr)].HexCodes := Hex3(DisPage[F(Adr)].HexCode);
    if ((I and $0004) = $0000) then
      Mn := Mnemonics[123, PrefMnem + 1]
    else
      Mn := Mnemonics[122, PrefMnem + 1];
    St := '';
    I := I shr 3;
    if ((I and $0040) = $0040) then begin      // the jump distance is negative
      I := not I;
      I := I and $003F;
      I := I + 1;
      Dest := Adr - I;  // Destination Address
      St := St + '-';
    end else begin                                           // jump is forward
      I := I and $003F;
      Dest := Adr + I;  // Destination Address
      St := St + '+';
    end;
    if PrefSDK41Mode then
      DisPage[F(Adr)].Mnem := ExpandSp(Mn, 5) + St + Int2(I)   // distance is decimal!
    else
      DisPage[F(Adr)].Mnem := ExpandSp(Mn, 5) + St + Hex2(I);  // distance is hex!
    DisPage[F(Adr)].MnemArg1 := Hex4(Dest);
    AddLabel((Dest), 'LB_' + Hex4(Dest));   // Add this label if it did not exist
    DisPage[F(Adr)].RefLbl := DisPage[F(Dest)].Lbl;
    DisPage[F(Adr)].RefAdr := Dest;
    DisPage[F(Adr)].RefLabel := true;
  end;                                                          // DecodeClass3

  procedure Dis;
  // does the normal disassembly of instructions, also called for Tp_Undefined
  begin
    I := DisPage[F(Adr)].HexCode;
    IClass := I and $0003;                         // extract Instruction Class
    case IClass of
      0: DecodeClass0(Adr, I);                            // misc. instructions
      1: DecodeClass1(Adr, I);                         // absolute GOTO/EXECUTE
      2: DecodeClass2(I);                      // arithmetic/logical operations
      3: DecodeClass3(Adr, I);                             // PC relative jumps
    end;
  end;                                                              // DisAssem

  function DatReg(Dat: word): string;
  begin
    Dat := Dat and $7F;
    if (Dat < 100) then DatReg := Int2(Dat) else DatReg := DatRegT[Dat];
  end;

  procedure FindXROM(I, J: Integer; var S1, S2: string);
  // Finds XROM I, J from own ROM or XROM.TXT
  // Returns: S1: function name, string is empty when not found
  //          S2: comments
  begin
    S1 := '';
    S2 := '';
    if (I = ThisXROM) then begin
      // XROM in this ROM
      S1 := ROMFat[j];
      S2 :='in [' + ROMFat[0] + '], this ROM';
      if Dis8K then S2 := S2 + ', 1st 4K'
    end else if (I = ThisXROM8K) and Dis8K then begin
      // XROM is 2nd 8K
      S1 := ROMFat8K[j];
      S2 := 'in [' + ROMFat8K[0] + '], this ROM, 2nd 4K'
    end else begin
      // find XROM name from external file
      FindXROMFile(I, J, S1, S2);      
    end;
  end;

  function UserDis(var Adr: word): string;
  // Disassembler for User Code
  var
    Dat, Dat1: word;
    PrevAdr, Disp, Disp2, TAdr, KA, Regs, Bytes: word;
    US_Code, US_Hex, US_Com: string;
    IsAppend: boolean;

    procedure DisUserCode;
    var
      I, j: Integer;
      S1, S2, S3, Lbl, LabNum: string;
      St_Adr: word;
    begin
      Dat := DisPage[F(Adr)].HexCode and $FF;
      Lbl := DisPage[F(Adr)].Lbl;
      // if the label starts with LB_, it is an MCode label, should not be here!
      if Pos('LB_', Lbl) = 1 then DisPage[F(Adr)].Lbl := '';

      US_Hex := Hex3(DisPage[F(Adr)].HexCode) + Space;           // track HexCodes
      DisPage[F(Adr)].UCodeLn := LinNum;                         // Line Number

      // US_Code := Int3(LinNum) + Space;              // used to build up instructions
      US_Code := '';
      US_Com := '';

      // Code added to put * before label, like in printer listings
      if (DisPage[F(Adr)].HexCode and $00FF) in [$01..$0F, $C0..$CD, $CF]  then begin
        US_Code := US_Code + '*';
      end else
        US_Code := US_Code + Space;

      US_Code := US_Code + HexTable[Dat];

      case TypTable[Dat] of
        OneByte: begin
            DisPage[F(Adr)].WrdTp := Tp_UserProgram;
          end;
        Number: begin
            // Added to allow number entries to be put on one line
            // note that first digit is already there!
            DisPage[F(Adr)].WrdTp := Tp_UserProgram;
            while ((DisPage[F(Adr + 1)].HexCode and $100) = $000) and
                  (TypTable[DisPage[F(Adr + 1)].HexCode and $FF] = Number) do begin
              // we are not at the end of the number
              Adr := Adr + 1;
              DisPage[F(Adr)].WrdTp := Tp_UserProgram;
              DisPage[F(Adr)].DisTp := Dt_SkipLine;
              US_Hex := US_Hex + Hex3(DisPage[F(Adr)].HexCode) + Space;
              US_Code := US_Code + HexTable[DisPage[F(Adr)].HexCode and $00FF];
            end;
          end;
        TwoByte: begin
            DisPage[F(Adr)].WrdTp := Tp_UserProgram;
            Adr := Adr + 1;
            DisPage[F(Adr)].DisTp := Dt_SkipLine;
            Dat := DisPage[F(Adr)].HexCode and $FF;
            if Dat < $80 then
              US_Code := US_Code + DatReg(Dat)
            else
              US_Code := US_Code + 'IND ' + DatReg(Dat);
            US_Hex := US_Hex + Hex3(DisPage[F(Adr)].HexCode) + Space;
            DisPage[F(Adr)].WrdTp := Tp_UserProgram;
          end;
        XROM: begin
            DisPage[F(Adr)].WrdTp := Tp_UserProgram;
            Adr := Adr + 1;
            US_Hex := US_Hex + Hex3(DisPage[F(Adr)].HexCode) + Space;
            Dat1 := DisPage[F(Adr)].HexCode and $FF;
            I := ((Dat - $A0) * 4) + (Dat1 div $40);
            j := Dat1 and $3F;
            FindXROM(I, j, S1, S2);
            if S1 <> '' then begin
              US_Code := US_Code + S1 ;
              US_Com := 'XROM ' + Int2(I) + ',' + Int2(j) + ' ' + S2;
            end else begin
              // no valid XROM name found
              US_Code := US_Code + Int2(I) + ',' + Int2(j);
              US_Com := 'no XROM name found';
            end;
            DisPage[F(Adr)].WrdTp := Tp_UserProgram;
            DisPage[F(Adr)].DisTp := Dt_SkipLine;
          end;
        GTO: begin             // 2-byte GTO
            // 2-byte GTO structure as follows
            //   BX YY   X=1; GTO 00, X=F: GTO 14
            //           YY=dbbb.bbbb, d = 0: decreasing address
            //                         d = 1: increasing address
            //               bbb bbbb  jump distance in bytes
            DisPage[F(Adr)].WrdTp := Tp_UserProgram;
            Adr := Adr + 1;
            US_Hex := US_Hex + Hex3(DisPage[F(Adr)].HexCode);
            DisPage[F(Adr)].WrdTp := Tp_UserProgram;
            LabNum := Copy(US_Code, 7, 2);
            DisPage[F(Adr)].DisTp := Dt_SkipLine;
            Disp := DisPage[F(Adr)].HexCode;   // jump distance in 2nd word
            if (Disp and $0080) = $0080 then begin
              // Jump distance forward
              Disp := (Disp and $007F);
              TAdr := Adr + 1 + Disp;
              S1 := '+';
            end else begin
              TAdr := Adr + 1 - Disp;
              S1 := '-';
            end;
            US_Com := 'LBL ' + LabNum + ' @' + Hex4(TAdr) + Space + '[' + S1 + Int3(Disp) + ' bytes]';
          end;
        GLOBAL: begin                                         // 192 = hex code
            // structure of a global label:
            //   C0..CD     Cx yy Fn zz [text] C0..CD
            //     x yy indicates distance to previous global label
            //       x = abcd, bytes = abc, regs = d.yy
            //     Fn indicates string, n=string siye + 1
            //     zz indicates key assignment, 00 means no key is assigned
            //     if Fn does not start with F, it is an END:
            //        xyy gives distance to previous END
            //        Fn meaning:
            DisPage[F(Adr)].WrdTp := Tp_UserProgram;
            St_Adr := Adr;
            Bytes := (DisPage[F(Adr)].HexCode and $0E) shr 1;   // jump distance bytes 1st nybble
            Regs :=  (DisPage[F(Adr)].HexCode and $01) shl 8;
            Adr := Adr + 1;
            US_Hex := US_Hex + Hex3(DisPage[F(Adr)].HexCode) + Space;
            DisPage[F(Adr)].WrdTp := Tp_UserProgram;
            DisPage[F(Adr)].DisTp := Dt_SkipLine;
            Regs := Regs + DisPage[F(Adr)].HexCode;         // jump distance regs in 2nd byte
            Disp := 7* Regs + Bytes;
            Adr := Adr + 1;
            US_Hex := US_Hex + Hex3(DisPage[F(Adr)].HexCode) + Space;
            DisPage[F(Adr)].WrdTp := Tp_UserProgram;            // 241+n=length
            DisPage[F(Adr)].DisTp := Dt_SkipLine;
            Hulp := '';
            if ((DisPage[F(Adr)].HexCode and $00F0) <> $00F0) then begin
              // final end of usercode program
              Delete(US_Code, Pos('*LBL  "', US_Code), 7);
              US_Code := US_Code + ' END';
              EndUs := True;
              DisPage[F(Adr - 2)].DisTp := Dt_EmptyLineAfter;
              S3 := 'END';
              US_Com := US_Com + Hulp;
              US_Com := US_Com + 'Program END, previous LBL @' + Hex4(St_Adr - Disp);
              US_Com := US_Com + ' [-' + Int3(Disp) + ' bytes]';
            end else begin
              // Real label
              j := (DisPage[F(Adr)].HexCode and $0F);    // num chars in string
              Adr := Adr + 1;
              KA := DisPage[F(Adr)].HexCode;                    // key assigned
              DisPage[F(Adr)].WrdTp := Tp_UserProgram;
              DisPage[F(Adr)].DisTp := Dt_SkipLine;
              S3 := '';
              Hulp := Hulp + Hex2(DisPage[F(Adr)].HexCode and $FF) + ' ';
              for I := 1 to j - 1 do begin
                Adr := Adr + 1;
                DisPage[F(Adr)].WrdTp := Tp_UserProgram;
                DisPage[F(Adr)].DisTp := Dt_SkipLine;
                S3 := S3 + HPUserChar(DisPage[F(Adr)].HexCode);
                Hulp := Hulp + Hex2(DisPage[F(Adr)].HexCode and $FF) + ' ';
                US_Hex := US_Hex + Hex3(DisPage[F(Adr)].HexCode) + ' ';
              end;
              US_Code := US_Code + S3 + '"';
              US_Com := US_Com + Hulp;
              US_Com := US_Com + ', previous LBL @' + Hex4(St_Adr - Disp);
              US_Com := US_Com + ' [-' + Int3(Disp) + ' bytes]';
              ReplaceSpaces(S3);
              DisPage[F(St_Adr)].Lbl := S3;
            end;
          end;
        ThreeByte: begin            // 3-byte GTO/XEQ
            // 3-byte GTO/XEQ structure:
            //   Dx.yy.zz
            //   Ex.yy.zz
            //     x.yy = distance to LBL
            //       zz = label, 1st bit is direction
            DisPage[F(Adr)].WrdTp := Tp_UserProgram;
            Disp := (DisPage[F(Adr)].HexCode and $0F) shl 8;   // jump distance 1st nybble
            Adr := Adr + 1;
            US_Hex := US_Hex + Hex3(DisPage[F(Adr)].HexCode);
            DisPage[F(Adr)].WrdTp := Tp_UserProgram;
            DisPage[F(Adr)].DisTp := Dt_SkipLine;
            Disp := Disp + DisPage[F(Adr)].HexCode;   // jump distance 2nd byte
            Adr := Adr + 1;
            US_Hex := US_Hex + Space + Hex3(DisPage[F(Adr)].HexCode) + Space;
            DisPage[F(Adr)].WrdTp := Tp_UserProgram;
            DisPage[F(Adr)].DisTp := Dt_SkipLine;
            Dat1 := DisPage[F(Adr)].HexCode and $FF;
            Disp2 := DisPage[F(Adr)].HexCode;         // jump distance in 2nd word
            if (Disp2 and $0080) = $0080 then begin
              // Jump distance forward
              // Disp := (Disp and $007F);
              TAdr := Adr - 1 + Disp;
              S1 := '+';
            end else begin
              TAdr := Adr - 1 - Disp;
              S1 := '-';
            end;
            LabNum := DatReg(Dat1 and $7F);
            US_Code := US_Code + DatReg(Dat1 and $7F);
            US_Com := 'LBL ' + LabNum + ' @' + Hex4(TAdr) + Space + '[' + S1 + Int3(Disp) + ' bytes]';

          end;
        Text: begin
            DisPage[F(Adr)].WrdTp := Tp_UserProgram;
            Hulp := '';
            if ((Dat and $000F) > 0) then begin
              // text is at least 1 char
              IsAppend := (DisPage[F(Adr + 1)].HexCode = $07F);
              for I := 1 to (Dat and $000F) do begin
                Adr := Adr + 1;
                DisPage[F(Adr)].WrdTp := Tp_UserProgram;
                DisPage[F(Adr)].DisTp := Dt_SkipLine;
                if (I = 1) and IsAppend then
                  US_Code := ' >"'
                else
                  US_Code := US_Code + HPUserCharS(DisPage[F(Adr)].HexCode);
                US_Com := US_Com + Hex2(DisPage[F(Adr)].HexCode and $FF) + ' ';
                US_Hex := US_Hex + Hex3(DisPage[F(Adr)].HexCode) + ' ';
              end;
              US_Code := US_Code + '"';
            end;
            if (Dat and $000F) = 0 then
              // zero length text
              US_Com := US_Com + 'zero-length text, 1-byte NOP'
            else
              US_Com := US_Com + Hulp;
          end;
        GotoText: begin
            DisPage[F(Adr)].WrdTp := Tp_UserProgram;
            Adr := Adr + 1;
            DisPage[F(Adr)].WrdTp := Tp_UserProgram;
            DisPage[F(Adr)].DisTp := Dt_SkipLine;
            Dat := DisPage[F(Adr)].HexCode and $FF;
            Hulp := '';
            if ((Dat and $000F) > 0) then
              for I := 1 to (Dat and $000F) do begin
                Adr := Adr + 1;
                DisPage[F(Adr)].WrdTp := Tp_UserProgram;
                DisPage[F(Adr)].DisTp := Dt_SkipLine;
                US_Code := US_Code + HPUserChar(DisPage[F(Adr)].HexCode);
                Hulp := Hulp + Hex2(DisPage[F(Adr)].HexCode and $FF) + ' ';
                US_Hex := US_Hex + Hex3(DisPage[F(Adr)].HexCode) + ' ';
              end;
            US_Code := US_Code + '"';
            US_Com := US_Com + Hulp;
          end;
        GXIND: begin
            DisPage[F(Adr)].WrdTp := Tp_UserProgram;
            Adr := Adr + 1;
            US_Hex := US_Hex + Hex3(DisPage[F(Adr)].HexCode) + Space;
            DisPage[F(Adr)].WrdTp := Tp_UserProgram;
            DisPage[F(Adr)].DisTp := Dt_SkipLine;
            Dat := DisPage[F(Adr)].HexCode and $FF;
            if Dat > $7F then
              US_Code := US_Code + 'XEQ  IND '
            else
              US_Code := US_Code + 'GTO  IND ';
            US_Code := US_Code + DatReg(Dat and $7F);
          end;
      end;
      LinNum := LinNum + 1;
    end; // DisUserCode

  begin {UserDis }
    LinNum := 1;
    EndUs := False;
    repeat
      PrevAdr := Adr;
      DisUserCode;
      DisPage[F(PrevAdr)].Comment := US_Com;
      DisPage[F(PrevAdr)].Mnem := US_Code;
      DisPage[F(PrevAdr)].HexCodes := US_Hex;
      Adr := Adr + 1;
    until EndUs;
    Adr := Adr - 1;
    UserDis := 'END OF USER CODE';
    DisPage[F(Adr + 1)].DisTp := Dt_EmptyLinebefore;
    DisPage[F(Adr)].DisTp := Dt_SkipLine;

  end; { UserDis }

begin { NewDis }

  case DisPage[F(Adr)].WrdTp of

    Tp_Undefined: begin                          // we do not know the type yet
        Dis;                      // just disassemble, this will do everything!
      end;

    Tp_XROMNumber: begin                                         // XROM Number
        DisPage[F(Adr)].Comment := 'XROM number %' + Int3(DisPage[F(Adr)].HexCode);
        if In8K then
          ThisXROM8K := DisPage[F(Adr)].HexCode
        else
          ThisXROM := DisPage[F(Adr)].HexCode;
        DisPage[F(Adr)].DisTp := Dt_EmptyLineAfter;
        // DisPage[F(Adr)].Mnem := 'XROM';                   // pseudo instruction
        DisPage[F(Adr)].Mnem := Mnemonics[273 + 10, PrefMnem + 1];
        if PrefSDK41Mode then
          DisPage[F(Adr)].MnemArg1 := SStr(DisPage[F(Adr)].HexCode)
        else
          DisPage[F(Adr)].MnemArg1 := '%' + Int3(DisPage[F(Adr)].HexCode);
        DisPage[F(Adr)].HexCodes := Hex3(DisPage[F(Adr)].HexCode);
        DisPage[F(Adr)].DisTp := Dt_EmptyLineAfter;
      end;

    Tp_FATEntries: begin                               // number of FAT entries
        DisPage[F(Adr)].Comment := '# FAT Entries  %' + Int3(DisPage[F(Adr)].HexCode);
        DisPage[F(Adr)].DisTp := Dt_EmptyLineAfter;
        DisPage[F(Adr)].Mnem := Mnemonics[273 + 11, PrefMnem + 1];
        if PrefSDK41Mode then
          DisPage[F(Adr)].MnemArg1 := SStr(DisPage[F(Adr)].HexCode)
        else
          DisPage[F(Adr)].MnemArg1 := '%' + Int3(DisPage[F(Adr)].HexCode);
        DisPage[F(Adr)].HexCodes := Hex3(DisPage[F(Adr)].HexCode);
        DisPage[F(Adr)].DisTp := Dt_EmptyLineAfter;
      end;

    Tp_FATFunction: begin              // find the function name and analyse it
        // work out XROM number for comment
        Cmt := ' XROM ' + Int2(DisPage[Adr and $1000].HexCode) + '.' +
                         Int2(((Adr and $0FFF) DIV 2) - 1);
        FirstWord := DisPage[F(Adr)].HexCode;
        if ((FirstWord and $0200) = $0200) then
          DisPage[F(Adr)].Mnem := Mnemonics[273 + 2, PrefMnem + 1]   // User Code
        else
          DisPage[F(Adr)].Mnem := Mnemonics[273 + 0, PrefMnem + 1];  // MCode
        Adr := Adr + 1;
        SecondWord := DisPage[F(Adr)].HexCode;
        DisPage[F(Adr)].DisTp := Dt_SkipLine;
        StartAdr := (SecondWord and $FF);
        StartAdr := StartAdr or ((FirstWord and $FF) shl 8);
        DisPage[F(Adr - 1)].HexCodes := Hex3(FirstWord) + Space + Hex3(SecondWord);

        // StartAdr can be one of the following:
        //     0xxx - Function Name in this 4K block
        //            - This is always valid
        //     1xxx - Function Name in next 4K block
        //            - valid only when we are doing the lower 4K
        //     Fxxx - Function Name in previous 4K block
        //            - valid only when we are doing the upper 4K

        // One of the following siutations may be true:
        //          disassembly in Page   Function in Page    StartAddr
        //     A -       lower 4K             lower 4K          0xxx
        //     B -       lower 4K             upper 4K          1xxx
        //     C -       upper 4K             lower 4K          Fxxx
        //     D -       upper 4K             lower 4K          0xxx
        //     E -       Function Name out of reach

        // First find out if we have a valid StartAdr for which we can decode the name
        if (StartAdr and $F000) = $0000 then begin
          // We are in our own Page
          FunPos := ThisROM;
          Point := (StartAdr and $0FFF) + CurrentBase;
        end else if Dis8K and ((StartAdr and $F000) = $1000) and (not in8K) then begin
          // we are in the lower 4K (even page) and the Function Name is in the upper 8K
          FunPos := NextROM;
          Point :=  (StartAdr and $0FFF) + BaseAddress + $1000;
          Cmt := Cmt + ' function in Upper 4K';
          if ((FirstWord and $0200) = $0200) then
            DisPage[F(Adr)].Mnem := Mnemonics[273 + 3, PrefMnem + 1]   // User Code
          else
            DisPage[F(Adr)].Mnem := Mnemonics[273 + 1, PrefMnem + 1];  // MCode
        end else if Dis8K and ((StartAdr and $F000) = $F000) and in8K then begin
          // we are in the upper 4K (odd page) and the Function Name is in the lower 4K
          FunPos := NextROM;
          Point :=  (StartAdr and $0FFF) + BaseAddress;
          Cmt := Cmt + ' function in Lower 4K';
          if ((FirstWord and $0200) = $0200) then
            DisPage[F(Adr)].Mnem := Mnemonics[273 + 2, PrefMnem + 1]   // User Code
          else
            DisPage[F(Adr)].Mnem := Mnemonics[273 + 0, PrefMnem + 1];  // MCode
        end else begin
          // we are outside our own Page, and cannot access the other page
          FunPos := OutSide;
          if (StartAdr and $F000) = $F000 then
            Point := (StartAdr and $0FFF) + BaseAddress - $1000
          else if (StartAdr and $1000) = $1000 then
            // in extreme cases this value may exceed the range of Point!
            // when disassembling a FAT which is not a real FAT
            // these variable are now LongWords, and the result is normalized
            Point := ((StartAdr + BaseAddress) and $FFFF)
          else
            Point := StartAdr and $FFFF;
        end;
        StartAdr := Point;
        if FunPos = OutSide then begin
          Cmt := Cmt + ' function outside this page';
          if ((FirstWord and $0200) = $0200) then
            //Usercode program
            Cmt := 'UCode' + Cmt
          else
            Cmt := 'MCode' + Cmt;
          DisPage[F(Adr - 1)].Comment := Cmt;
          DisPage[F(Adr - 1)].MnemArg1 := Hex4(StartAdr);
        end else begin
          // We have a valid Function Name address in Point
          // First sort out the function name
          if ((FirstWord and $0200) = $0200) then begin
            // This is a UserCode program
            Point := StartAdr + 2;                          // address of label
            LabLen := DisPage[F(Point)].HexCode and $000F;   // length of label
            DisPage[F(Adr - 1)].MnemArg1 := Hex4(StartAdr);
            Hulp := '';                                // Hulp is Function Name
            for j := 2 to LabLen do begin
              // get label string, same as Function Name
              W :=  DisPage[F(Point + j)].HexCode and $00FF;
              Hulp := Hulp + HPUserChar(W);
            end;
            S := S + '"' + Hulp + '"';              // build disassembly string
            // for j := LabLen to 10 do S := S + ' ';
            DisPage[F(Adr - 1)].Comment := 'UCode' + Cmt;
            // Now put the Label in the right location, name in Hulp
            ReplaceSpaces(Hulp);
            AddLabel(Point - 2, Hulp);
            // added to allow usercode before the Global Label to be disassembled
            j := 0;
            repeat
              // search backwards until FirstTwo are found, start with $2xx
              // and one word before that, start search at Label - 1
              j := j + 1;
              WW := DisPage[F(StartAdr - j)].HexCode and $0200;
              if WW <> $0200 then begin
                // normal User Code, set type
                DisPage[F(StartAdr - j)].WrdTp := Tp_UserProgram;
                // remove any generated mcode labels LB_xxxx there might be
                ClearMCodeLabel(StartAdr - 1);
              end;
            until (WW = $0200);
            // must have found start of User Code, set type
            DisPage[F(StartAdr - j - 1)].WrdTp := Tp_FirstTwo;
            DisPage[F(StartAdr - j)].WrdTp := Tp_FirstTwo;
            ClearMCodeLabel(StartAdr - j - 1);
            ClearMCodeLabel(StartAdr - j);
            // set User Code type for label for correct User Code disassembly
            for Point := StartAdr to StartAdr + 3 do begin
              DisPage[F(Point)].WrdTp := Tp_UserProgram;
              ClearMCodeLabel(Point);
            end;
            Cmt := Cmt + '  "' + Hulp + '"';
            ReplaceSpaces(Hulp);
            DisPage[F(StartAdr)].Lbl := Hulp;
            DisPage[F(Adr - 1)].RefLbl  := Hulp;
            DisPage[F(Adr - 1)].RefAdr  := StartAdr;
            DisPage[F(Adr - 1)].RefLabel := true;
          end else begin
            // This is an MCode program
            Point := StartAdr;
            // next statement inserted for correct disassembly of 8K roms
            if DisPage[F(Point)].WrdTp <> Tp_FunctionName then
              DisPage[F(Point)].WrdTp := Tp_Instruction1;
            Hulp := '';
            repeat
              Point := Point - 1;
              W := DisPage[F(Point)].HexCode;
              Hulp := Hulp + HPChar(W);
              DisPage[F(Point)].WrdTp := Tp_FunctionName;
            until ((DisPage[F(Point)].HexCode and $0080) <> 0) or (Point = $0000);
            Cmt := Cmt + '  "' + Hulp + '"';
            ReplaceSpaces(Hulp);
            DisPage[F(StartAdr)].Lbl := Hulp;
            DisPage[F(Adr - 1)].RefLbl  := Hulp;
            DisPage[F(Adr - 1)].RefAdr  := StartAdr;
            DisPage[F(Adr - 1)].RefLabel := true;
            DisPage[F(Adr - 1)].Comment := Cmt;
          end;

          if ((FirstWord and $0200) = $0200) then
            //Usercode program
            DisPage[F(Adr - 1)].Comment := 'UCode' + Cmt
          else
            DisPage[F(Adr - 1)].Comment := 'MCode' + Cmt;

          // The Function Name is now handled, only add to the correct XROM table
          if not in8K then
            ROMFat[(((Adr and $0FFF)- 1) DIV 2) - 1] := Hulp    // In lower page
          else
            ROMFat8K[(((Adr and $0FFF)- 1) DIV 2) - 1] := Hulp; // Points to upper page
        end;

      end;

    Tp_FATEnd: begin
        DisPage[F(Adr)].Comment := 'End of FAT';
        if PrefSDK41Mode then begin
          DisPage[F(Adr)].Mnem := '#' + Hex3(DisPage[F(Adr)].HexCode);
          DisPage[F(Adr + 1)].Mnem := '#' + Hex3(DisPage[F(Adr + 1)].HexCode);
          DisPage[F(Adr)].HexCodes := Hex3(DisPage[F(Adr)].HexCode);
          DisPage[F(Adr + 1)].HexCodes := Hex3(DisPage[F(Adr + 1)].HexCode);
          DisPage[F(Adr + 1)].DisTp := Dt_EmptyLineAfter;
        end else begin
          DisPage[F(Adr)].Mnem := Mnemonics[273 + 22, PrefMnem + 1];
          DisPage[F(Adr)].HexCodes := Hex3(DisPage[F(Adr)].HexCode) + Space +
                                      Hex3(DisPage[F(Adr + 1)].HexCode);
          DisPage[F(Adr)].DisTp := Dt_EmptyLineAfter;
          DisPage[F(Adr + 1)].DisTp := Dt_SkipLine;
        end;
        Adr := Adr + 1;
      end;

    Tp_FunctionName: begin
        A := Adr;                                              // Start address
        // first find out length of Function Name
        repeat
          A := A + 1;
        until ((DisPage[F(A)].HexCode and $0080) = $0080) or
              (DisPage[F(A)].WrdTp <> Tp_FunctionName);
        S := '';
        Hulp := '';

        for Point := A - 1 downto Adr do begin
          S := S + HPChar(DisPage[F(Point)].HexCode);
          Hulp := Hulp + Hex3(DisPage[F(Point)].HexCode) + ' ';
          DisPage[F(Point)].DisTp := Dt_SkipLine;
        end;
        Adr := A - 1;   // Adr now contains the first character of the function name
                        // A is the address of the Function Entry
        // in some cases Dt_SkipLine could be set if it was previously
        // disassembled as the 2nd word of an instruction, so fix this
        // exception is if the word is a function name, in some ROM's two
        // function names are directly adjacent, see Card Reader for example
        if DisPage[F(A)].WrdTp <> Tp_FunctionName then
          DisPage[F(A)].DisTp := Dt_Default;
        prompt := (DisPage[F(Adr)].HexCode and $0300) shr 8;
        DisPage[F(Adr)].DisTp := Dt_Default;
        if (DisPage[F(Adr)].HexCode and $0080) = $0000 then
          // Function name was > 1 character, normal case
          prompt := prompt or ((DisPage[F(Adr - 1)].HexCode and $0300) shr 4);
          // else Function name was 1 character, can this happen?
        case prompt of
          $00 : Cmt := '';            // no prompting
          $01 : Cmt := 'Prompt: Alpha (null input valid)';
          $02 : Cmt := 'Prompt: 2 digit ST INF IND_ST + - * /';
          $03 : Cmt := 'Prompt: 2 digit or non-null Alpha';
          $11 : Cmt := 'Prompt: 3 digits';
          $12 : Cmt := 'Prompt: 2 digits ST IND IND_ST';
          $13 : Cmt := 'Prompt: 2 digits IND IND_ST non-null Alpha';
          $21 : Cmt := 'Prompt: non-null Alpha';
          $22 : Cmt := 'Prompt: 2 digits IND IND_ST';
          $23 : Cmt := 'Prompt: 2 digits or non-null Alpha';
          $31 : Cmt := 'Prompt: 1 digit IND IND_ST';
          $32 : Cmt := 'Prompt: 2 digits IND IND_ST';
          $33 : Cmt := 'Prompt: 2 digits IND IND_ST non-null Alpha . ..';
          else  Cmt := 'Prompt: Invalid combination';
        end;
        if Cmt <> '' then Cmt := '/ ' + Cmt;
        DisPage[F(Adr)].Mnem := Mnemonics[273 +18, PrefMnem + 1];
        DisPage[F(Adr)].MnemArg1 := '"' + S +'"';
        DisPage[F(Adr)].HexCodes := Hulp;
        DisPage[F(Adr)].Comment := Hulp + ' ' + Cmt;
        DisPage[F(Adr)].DisTp := Dt_EmptyLineBefore;
      end;

    Tp_Data: begin
        if PrefAutoFixed then DisPage[F(Adr)].Lbl := '_DATA';
        DisPage[F(Adr)].Mnem := Mnemonics[273 +5, PrefMnem + 1];
        DisPage[F(Adr)].MnemArg1 := Hex3(DisPage[F(Adr)].HexCode);
        DisPage[F(Adr)].HexCodes := Hex3(DisPage[F(Adr)].HexCode);
      end;

    Tp_Instruction1: begin // normal instruction disassembly
        Dis;
      end;

    Tp_Instruction2: begin
        DisPage[F(Adr)].Mnem := 'Instruction2';
      end;

    Tp_Instruction3: begin
       DisPage[F(Adr)].Mnem := 'Instruction3';
      end;

    Tp_UserProgram: begin
        S := UserDis(Adr);
      end;

    Tp_EntryPoint: begin
        Dis;
        case (Adr and $0FFF) of
          $0FF4: begin
                   S := 'PAUSE interrupt';
                   DisPage[F(Adr)].DisTp := Dt_EmptyLineBefore;
                 end;
          $0FF5: S := 'After each program line';
          $0FF6: S := 'Awaken from DEEP SLEEP';
          $0FF7: S := 'Turn OFF by key or XEQ OFF';
          $0FF8: S := 'I/O service flag set';
          $0FF9: S := 'Awaken by pressing ON key';
          $0FFA: S := 'MEMORY LOST interrupt';
        else ;
        end;
        DisPage[F(Adr)].Comment := S;
      end;

    Tp_ROMRevision: begin
        S := '';
        S := HPChar(DisPage[F(Adr + 3)].HexCode) + HPChar(DisPage[F(Adr + 2)].HexCode);
        S := S + '-';
        S := S + HPChar(DisPage[F(Adr + 1)].HexCode) + HPChar(DisPage[F(Adr)].HexCode);
        Hulp := '';
        DisPage[F(Adr)].Mnem := Mnemonics[273 + 23, PrefMnem + 1];
        DisPage[F(Adr)].MnemArg1 := S;
        for j := 3 downto 0 do
          Hulp := Hulp + Hex3(DisPage[F(Adr + j)].HexCode) + ' ';
        DisPage[F(Adr)].HexCodes := Hulp;
        DisPage[F(Adr)].DisTp := Dt_EmptyLineBefore;
        if PrefSDK41Mode then begin
          for j := Adr to Adr + 3 do begin
            DisPage[F(j)].Mnem := '#' + Hex3(DisPage[F(j)].HexCode);
            DisPage[F(j)].MnemArg1 := '';
            DisPage[F(j)].HexCodes := Hex3(DisPage[F(j)].HexCode);
            DisPage[F(j)].Comment := '"' + HPChar(DisPage[F(j)].HexCode) + '"';
          end;
          DisPage[F(Adr)].Comment := DisPage[F(Adr)].Comment + ' ROM Revision ' + S;
        end;

        if not PrefSDK41Mode then begin
          DisPage[F(Adr + 1)].DisTp := Dt_SkipLine;
          DisPage[F(Adr + 2)].DisTp := Dt_SkipLine;
          DisPage[F(Adr + 3)].DisTp := Dt_SkipLine;
        end;
        Adr := Adr + 3;
      end;

    Tp_CheckSum: begin
        // Calculate checksum and compare with actual value
        Accumulator := 0;
        if in8K then begin
          for i := $1000 to $1FFE do begin
            Accumulator := Accumulator + (DisPage[F(i)].HexCode and $03FF);
            if Accumulator > $03FF then
              Accumulator := (Accumulator and $03FF) + 1;
          end;
        end else begin
          // We are in 1st 4K block
          for i := $0000 to $0FFE do begin
            Accumulator := Accumulator + (DisPage[F(i)].HexCode and $03FF);
            if Accumulator > $03FF then
              Accumulator := (Accumulator and $03FF) + 1;
          end;
        end;
        Accumulator := (-Accumulator) and $03FF;

        DisPage[F(Adr)].Mnem := Mnemonics[273 + 24, PrefMnem + 1];
        DisPage[F(Adr)].MnemArg1 := Hex3(DisPage[F(Adr)].HexCode);
        DisPage[F(Adr)].HexCodes := Hex3(DisPage[F(Adr)].HexCode);
        DisPage[F(Adr)].Comment := 'Calculated Checksum: ' + Hex3(Accumulator);
        DisPage[F(Adr)].DisTp := Dt_EmptyLineBefore;

      end;

    Tp_FirstTwo: begin
        DisPage[F(Adr)].DisTp := Dt_EmptyLineBefore;

        // if the label starts with LB_, it is an MCode label, should not be here!
        Hulp := DisPage[F(Adr)].Lbl;
        if Pos('LB_', Hulp) = 1 then Hulp := '';

        if PrefSDK41Mode then begin
          DisPage[F(Adr)].Mnem := '#' + Hex3(DisPage[F(Adr)].HexCode);
          DisPage[F(Adr + 1)].Mnem := '#' + Hex3(DisPage[F(Adr + 1)].HexCode)
        end else begin
          DisPage[F(Adr + 1)].DisTp := Dt_SkipLine;
          DisPage[F(Adr)].HexCodes := Hex3(DisPage[F(Adr)].HexCode) + ' ' +
                                      Hex3(DisPage[F(Adr + 1)].HexCode);

          DisPage[F(Adr)].Mnem := Mnemonics[273 + 25, PrefMnem + 1];
          DisPage[F(Adr)].MnemArg1 := DisPage[F(Adr)].HexCodes;
          DisPage[F(Adr + 1)].MnemArg1 := Hex3(DisPage[F(Adr + 1)].HexCode);
        end;

        // Added to explain meaning of first two bytes
        Hulp := 'UserCode: ';
        Hulp := Hulp + SStr(DisPage[F(Adr)].HexCode * 7 +
                (DisPage[F(Adr + 1)].HexCode and $00F0) shr 4);

        Hulp := Hulp + ' bytes (';
        Hulp := Hulp + SStr(DisPage[F(Adr)].HexCode) + ' regs + ';
        Hulp := Hulp + SStr((DisPage[F(Adr + 1)].HexCode and $00F0) shr 4) + ' bytes) ';

        if ((DisPage[F(Adr + 1)].HexCode and $0100) = $0100) then
          Hulp := Hulp + 'PRIVATE'
        else
          Hulp := Hulp + 'NONPRIVATE';

        DisPage[F(Adr)].Comment := Hulp;
        Adr := Adr + 1;
      end;

    Tp_StrDat: begin
        DisPage[F(Adr)].Mnem := Mnemonics[273 +5, PrefMnem + 1];
        DisPage[F(Adr)].MnemArg1 := Hex3(DisPage[F(Adr)].HexCode);
        DisPage[F(Adr)].Comment := HpChar(DisPage[F(Adr)].HexCode);
      end;

    else begin
        // unknown ID
        S := 'UNKNOWN CODE';

    end;
  end; { Case }

end;

end.

