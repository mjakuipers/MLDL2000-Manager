unit DisAssembler;

//---------------------------------------------------------------------------//
//    Copyright (c) 2010  Meindert Kuipers, Netherlands                      //
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
//  DISASSEMBLER.PAS                                                         //
//  Unit for the M2kM Rom Handler                                            //
//  MOD file routines based on V41 by Warren Furlow                          //
//  1.60 Jan 2010 Moved to seperate Unit from ROMHandler                     //
//  1.70 Jul 2010 Adapted for Lazarus                                        //
//  1.80 Feb 2018 restarted development                                      //
//                removed Delphi compatibility                               //
//  1.90 Feb 2020 Updated Lazarus                                            //
//                small change in display of version info in dis listing     //
//       Mar 2020 Added HP41CL FLDB disassembly                              //
//                Added HP41CL IMDB disassembly (in process)                 //
//                                                                           //
//---------------------------------------------------------------------------//

interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, Grids, Globals, GlobalConstants,
  Procs, Procs_MLDL, Tester, HP41_Globals, MemEdit, Menus, Romdis,
  ListWindow, Jtag, BitBang_Unit, About, D2XXUnit, Buttons, IO_Handler,
  db, sdfdata, TxtBase;

procedure AnalyseROM;
procedure DisAssemble;
procedure ListIMDB;

implementation

uses
  ROMHandler;

var
  S: string;

procedure AnalyseROM;
    { analyses the ROM code. If not yet coded with upper 6 bits, it will do so }
    { First it scans the FAT, function names, interrupt entries, ID-code and   }
    { checksum and will then try to disassemble the rest of the code           }
    { Pass 1 only analyses the fixed addresses in the ROM                      }
    { Pass 2 does a full analysis by calling the disassembler                  }
    { Use this function only for complete XROM's!                              }
var
  i, L, LabAdr, LLab: integer;
  Lab: string;
  P, P8: Char;
begin

  if BasePage < $A then begin
    // Use known MainFrame Labels when disassembling here
    // Fill LabelTable with known labels
    P := Hex1(BasePage);
    P8 := Hex1(BasePage + 1);
    if LabelFileOpen then begin
      // file is open, so look for labels
      Reset(LabelFile);        // back to beginning
      while (not EOF(LabelFile)) do begin
        ReadLn(LabelFile, S);
        L := Length(S);
        if ((L > 0) and (S[1] = P)) or
           ((L > 0) and Dis8K and (S[1] = P8)) then begin
          // we are in the right page!
          LabAdr := HexToInt(S);
          Lab := GetFirstWord(S);
          LLab := Length(Lab);
          if (LLab > 0) and (Lab[1] <> ':') then
            // This is a real Label, not the title
            AddLabel(LabAdr, Lab);
        end;
      end;
    end;
  end;

  if FrmROMHandler.ChkHasFAT.Checked then begin
    // ROM has FAT, so analyze it

    DisPage[$0000].WrdTp := Tp_XROMNumber;
    if PrefAutoFixed then AddLabel($0000,'_XROM');

    DisPage[$0001].WrdTp := Tp_FATEntries;
    if PrefAutoFixed then AddLabel($0001,'_FCNS');

    NumberOfFunctions := DisPage[$0001].HexCode;
    if (NumberOfFunctions <> 0) and (NumberOfFunctions < 66) then begin
      { there should be at least one entry }
      EndOfFAT := NumberOfFunctions * 2 + 2; { Address of first null at FAT end }
      if PrefAutoFixed then AddLabel(EndOfFAT, '_FATEND');
      if PrefAutoFixed then AddLabel(EndOfFAT + 1, '_FATEND');

      DisPage[EndOfFat].WrdTp := Tp_FATEnd;
      DisPage[EndOfFat + 1].WrdTp := Tp_FATEnd;

      for i := $02 to EndOfFAT -1 do begin
        DisPage[i].WrdTp := Tp_FATFunction;
        if not Odd(i) then
          if PrefAutoFixed then
            AddLabel(i, '_XR_' + Int3(DisPage[0].HexCode) + '.' + Int2(((i DIV 2) - 1)));
      end;
    end;

    // now set the ID's of some words at the end of the ROM
    for i := $0FF4 to $0FFA do begin
      case i of
          $0FF4: Lab := '_EN_PSE';
          $0FF5: Lab := '_EN_PRGM';
          $0FF6: Lab := '_EN_SLEEP';
          $0FF7: Lab := '_EN_OFF';
          $0FF8: Lab := '_EN_IOSVC';
          $0FF9: Lab := '_EN_ON';
          $0FFA: Lab := '_EN_MEMLST';
        else ;
      end;
      DisPage[i].WrdTp := Tp_EntryPoint;
      if PrefAutoFixed then AddLabel(i, Lab);
    end;

    for i := $0FFB to $0FFE do begin;
      DisPage[i].WrdTp := Tp_ROMRevision;
      if PrefAutoFixed then AddLabel(i, '_ROMREV');
    end;

    DisPage[$0FFF].WrdTp := Tp_CheckSum;
    if PrefAutoFixed then AddLabel($0FFF, '_CHKSUM');

    if Dis8K then begin
      // Do the same for the 2nd 4K ROM if we want this
      // We assume that the 2nd ROM always has a FAT is the 1st 4K has a FAT!

      DisPage[$1000].WrdTp := Tp_XROMNumber;
      if PrefAutoFixed then AddLabel($1000,'_XROM');

      DisPage[$1001].WrdTp := Tp_FATEntries;
      if PrefAutoFixed then AddLabel($1001,'_FCNS');

      NumberOfFunctions8K := DisPage[$1001].HexCode;
      if (NumberOfFunctions <> 0) and (NumberOfFunctions < 66) then begin
        { there should be at least one entry }
        EndOfFAT := $1000 + NumberOfFunctions8K * 2 + 2; { Address of first null at FAT end }
        if PrefAutoFixed then AddLabel(EndOfFAT, '_FATEND');
        if PrefAutoFixed then AddLabel(EndOfFAT + 1, '_FATEND');

        DisPage[EndOfFAT].WrdTp := Tp_FATEnd;
        DisPage[EndOfFAT + 1].WrdTp := Tp_FATEnd;
        for i := $1002 to EndOfFAT -1 do begin
           DisPage[i].WrdTp := Tp_FATFunction;
          if PrefAutoFixed then
            AddLabel(i, '_XR_' + Int3(DisPage[$1000].HexCode) + '.' + Int2((((i and $0FFF) DIV 2) - 1)));
        end;
      end;

      // now set the ID's of some words at the end of the ROM
      for i := $1FF4 to $1FFA do begin
        case i of
          $0FF4: Lab := '_EN_PSE';
          $0FF5: Lab := '_EN_PRGM';
          $0FF6: Lab := '_EN_SLEEP';
          $0FF7: Lab := '_EN_OFF';
          $0FF8: Lab := '_EN_IOSVC';
          $0FF9: Lab := '_EN_ON';
          $0FFA: Lab := '_EN_MEMLST';
        else ;
        end;
        DisPage[i].WrdTp := Tp_EntryPoint;
        if PrefAutoFixed then AddLabel(i, Lab);
      end;

      for i := $1FFB to $1FFE do begin
        DisPage[i].WrdTp := Tp_ROMRevision;
        if PrefAutoFixed then AddLabel(i, '_ROMREV');
      end;

      DisPage[$1FFF].WrdTp := Tp_CheckSum;
      if PrefAutoFixed then AddLabel($1FFF, '_CHKSUM');
    end;       // if Dis8K

  end else begin
    // ROM is FATless, so we have very little to do, only use checksum
    if PrefAutoFixed then AddLabel($0FFF, '_CHKSUM');
    DisPage[$0FFF].WrdTp := Tp_CheckSum;
    if Dis8K then begin
      if PrefAutoFixed then AddLabel($1FFF, '_CHKSUM');
      DisPage[$1FFF].WrdTp := Tp_CheckSum;
    end;
  end;
end; { AnalyseROM }


procedure DisAssemble;
// DISASM Button
var
  S, SS, SSS, S2, S3: string;
  LStr, AStr: string;
  Idx: integer;
  i, j, l: word;
  Address, W: word;
  EndOfDis, EmptyLine, InUserCode: boolean;
  LabelFileName, XROMFileName: String;
//  NewItem: TListItem;
  XRefList: TStringList;
  SysLabels: TtxtBase;
  res: boolean;

begin

  FrmROMHandler.PnlStatus.Caption := 'DISASSEMBLY PASS 1';
  Application.ProcessMessages;

  FrmROMHandler.SaveIni;
  FrmROMHandler.ReadIni;                      // get current preferences

  // first open files for Labels and XROM
  if not FATOnly then begin
    LabelFileOpen := false;
    XROMFileOpen := false;
    LabelFileName := ExtractFilePath(Application.ExeName) + 'SYSTEMLABELS.TXT';
    XROMFileName := ExtractFilePath(Application.ExeName) + 'XROM.TXT';

    // Create our Test Database
    SysLabels := TtxtBase.Create;
    SysLabels.Filename := 'SYSTEMLABELS.TXT';
    res := SysLabels.Init;                  // initialize and fill the database

    if FileExists(LabelFileName) then begin
      FileMode := fmOpenRead;
      AssignFile(LabelFile, LabelFileName);
      Reset(LabelFile);
      LabelFileOpen := true;
    end else
      MessageDlg('SYSTEMLABELS.TXT not found', mtWarning, [mbOK], 0);

    if FileExists(XROMFileName) then begin
      AssignFile(XROMFile, XROMFileName);
      XROMFileOpen := true;
      Reset(XROMFile);
    end else
      MessageDlg('XROM.TXT not found', mtWarning, [mbOK], 0);

    // Clear LabelTable
    for i := 0 to $1FFF do LabelTable[i] := '          ';

    Application.ProcessMessages;
  end;

  // Upper 8K should now already be copied
  Dis8K := FrmROMHandler.ChkDis8K.Checked;
  In8K := false;

  // Clean XROM Table
  for i := 0 to 65 do begin
    ROMFat[i] := '';
    ROMFat8K[i] := '';
  end;
  ThisXROM := 0;
  ThisXROM8K := 0;

  BasePage := FrmROMHandler.CmbBAsePg.ItemIndex;

  if Dis8K then begin
    // Adjust some Global Variables
    BasePage := BasePage and $000E;   // BasePage must be even!
    BaseAddress := (BasePage shl 12);
    Address := (BasePage shl 12);
    EndAddress := Address + $1FFF;
    BasePage8K := BasePage + 1;
    BaseAddress8K := BaseAddress + $1000;
    RomEnd := $1FFF
  end else begin
    BaseAddress := (BasePage shl 12);
    Address := (BasePage shl 12);
    EndAddress := Address + $0FFF;
    BasePage8K := 0;
    BaseAddress8K := 0;
    RomEnd := $0FFF;
  end;

  for i := 0 to $0FFF do
    Page[i] := Swap(PageArray[i]);

  for i := 0 to RomEnd do begin
    // Initialize and fill DisPage first
    with DisPage[i] do begin
      Adr := BaseAddress + i;
      HexCode := Page[i];
      HexCodes := '';
      WrdTp := Tp_Undefined;
      DisTp := Dt_Default;
      Mnem := '';
      MnemArg1 := '';
      MnemArg2 := '';
      Lbl := '';
      HasLabel := false;
      Comment := '';
      RefLabel := false;
      RefLbl := '';
      UCodeLn := 0;
      Warning := '';
    end;
  end;

  // Pass 0.5, Analyze basic ROM structure if a FAT is present
  if not FATOnly then AnalyseROM;

  FrmROMHandler.PnlStatus.Caption := 'DISASSEMBLY PASS 2';
  Application.ProcessMessages;

  // Pass 1, disassemble first pass to find structure and labels
  
  DisasmPass := 1;
  EndOfDis := false;

  repeat
    S := '';
    CurrentBase := Address and $F000;
    CurrentPage := CurrentBase shr 12;
    in8K := Dis8K and Odd(CurrentPage);

    NewDis(Address);
    if (Address < EndAddress) and (address <> $FFFF) then
      Address := Address + 1
    else
      EndOfDis := true;
  until EndOfDis;

  FrmROMHandler.PnlStatus.Caption := 'DISASSEMBLY PASS 3';
  Application.ProcessMessages;

  // Pass 2, final disassembly

  Address := (BasePage shl 12);

  DisasmPass := 2;
  EndOfDis := false;
  repeat
    S := '';
    AddEmptyLine := false;

    CurrentBase := Address and $F000;
    CurrentPage := CurrentBase shr 12;
    in8K := Dis8K and Odd(CurrentPage);

    NewDis(Address);

    if (Address < EndAddress) and (address <> $FFFF) then
      Address := Address + 1
    else
      EndOfDis := true;
  until EndOfDis;

  // Pass 3, generate listing, now things get interesting!

  FrmROMHandler.PnlStatus.Caption := 'GENERATE LISTING';
  Application.ProcessMessages;

  ListWindow.FrmLister.Hide;
  ListWindow.FrmLister.EdtList.Clear;

  EmptyLine := false;

  S := '; LISTING GENERATED: ';
  S := S + DateToStr(Now) + ' ' + TimeToStr(Now);
  S := S + ' by MLDL Manager version ' + AboutBox.GetVersion;
  ListWindow.FrmLister.EdtList.Lines.Add(S);
  S := '; OPEN FILE: ' + OpenROMFileNm;
  ListWindow.FrmLister.EdtList.Lines.Add(S);
  ListWindow.FrmLister.EdtList.Lines.Add('');

  if PrefSDK41Mode then begin
    // modify label settings for SDK41 disassembly
    PrefDisPos[ 1] := 18;                       // Pos_Lbl
    PrefDisPos[ 2] :=  2;                       // Pos_Adr
    PrefDisPos[ 3] :=  7;                       // Pos_HexCodes
    PrefDisPos[ 4] := 35;                       // Pos_Mnem
    PrefDisPos[ 5] := 47;                       // Pos_MnemArg1
    PrefDisPos[ 6] := 59;                       // Pos_MnemArg2
    PrefDisPos[ 7] := 59;                       // Pos_RefAdr
    PrefDisPos[ 8] :=  0;                       // Pos_RefLbl
    PrefDisPos[ 9] := 47;                       // Pos_UCodeLn
    PrefDisPos[10] :=  7;                       // Pos_HexCode
    PrefDisPos[11] := 75;                       // Pos_Comment
    PrefLabLst     := true;
  end;

  case PrefMnem of
                   // indicates Mnemonics type:  0 - Jacobs/De Arras, Default
                   //                            1 - HP
                   //                            2 - ZenCode
                   //                            3 - SDK41 (JDA only)
   0: begin
        SS := '.JDA';
        S2 := 'Jacobs/De Arras Mnemonics';
      end;
   1: begin
        SS := '.HP';
        S2 := 'HP Mnemonics';
      end;
   2: begin
        SS := '.ZENCODE';
        S2 := 'ZENCODE Mnemonics';
      end;
   3: begin
        SS := '.JDA';
        S2 := 'Jacobs/De Arras/SDK41 Mnemonics';
      end;
  end;
  if PrefSDK41Mode then S2 := 'SDK41 MODE, ' + S2;

  S := '';
  RepString(SS, S, PrefDisPos[4]);
  RepString('; ' + S2, S, PrefDisPos[11]);
  ListWindow.FrmLister.EdtList.Lines.Add(S);
  S := '';
  RepString(Mnemonics[299, PrefMnem + 1], S, PrefDisPos[4]);
  ListWindow.FrmLister.EdtList.Lines.Add(S);
  ListWindow.FrmLister.EdtList.Lines.Add('');
  InUserCode := false;

  // Now we can enter the disassembly listing loop
  for i := 0 to RomEnd do begin

    S := '';

    // Label
    if PrefDisPos[1] > 0 then begin
      S3 := DisPage[i].Lbl;
      if PrefSDK41Mode and (S3 <> '') then S3 := '[' + S3 + ']';
      RepString(S3, S, PrefDisPos[1]);
    end;

    // Address in ROM
    if (PrefDisPos[2] > 0) and (not PrefCleanList) and
        not (PrefSDK41Mode and ((DisPage[i].WrdTp = Tp_FunctionName) or
                               (DisPage[i].WrdTp = Tp_MESLString))) then
      RepString(Hex4(DisPage[i].Adr), S, PrefDisPos[2]);

    // HexCodes, limit this to 3 HexCodes only
    SS := DisPage[i].HexCodes;
    Delete(SS, 12, 100);
    if PrefCleanList then SS := '';
    if PrefSDK41Mode then RemoveSpaces(SS);
    if not (PrefSDK41Mode and ((DisPage[i].WrdTp = Tp_FunctionName) or
                               (DisPage[i].WrdTp = Tp_MESLString))) then begin
      if PrefOneLine then
        RepString(Hex3(DisPage[i].HexCode), S, PrefDisPos[3])
      else if PrefDisPos[3] > 0 then
        RepString(SS, S, PrefDisPos[3]);
    end;

    if (DisPage[i].WrdTp = Tp_FirstTwo) and (DisPage[F(i)].Comment <> '')
       and PrefSDK41Mode then begin
      // FirstTwo and print .UCODE
      SS := '';
      ListWindow.FrmLister.EdtList.Lines.Add('');
      RepString('*', SS, 1);
      RepString(Mnemonics[273 + 25, PrefMnem + 1], SS, PrefDisPos[4]);
      ListWindow.FrmLister.EdtList.Lines.Add(SS);
    end;

    if DisPage[i].WrdTp = Tp_UserProgram then begin
      // treat as UserCode
      InUserCode := true;
      // User Code Line Number
      if PrefSDK41Mode then begin
        // SDK41 Mode
        W := DisPage[i].HexCode;
        RepString(Hex3(W) + '        ', S, PrefDisPos[3]);
        RepString('#' + Hex3(W), S, PrefDisPos[4]);                // mnemonic
        // build comment string
        if (DisPage[i].UCodeLn <> 0) then
          SS := '; ' + Int3(DisPage[i].UCodeLn) + '  ' + DisPage[i].Mnem;
        RepString(SS, S, PrefDisPos[5]);
      end else begin
        // normal mode
        if PrefDisPos[9] > 0 then begin
          if (DisPage[i].UCodeLn <> 0) then
            RepString(Int3(DisPage[i].UCodeLn), S, PrefDisPos[9]);
          if PrefDisPos[4] > 0 then
            RepString(DisPage[i].Mnem, S, PrefDisPos[5] - 1);
        end else
          // no Line Number requested, just print User Code
          if PrefDisPos[4] > 0 then RepString(DisPage[i].Mnem, S, PrefDisPos[9]);
        // HexCode
        if (PrefDisPos[10] > 0) and not PrefCleanList then
          RepString(Hex3(DisPage[i].HexCode), S, PrefDisPos[10]);
      end;

    end else begin
      // do MCode line

      // prepare printing of the referred label and address
      LStr := DisPage[i].RefLbl;
      AStr := Hex4(DisPage[i].RefAdr);
      if PrefSDK41Mode then LStr := '[' + LStr + ']';
      
      if DisPage[i].RefLabel and PrefLabLst then begin
        // print the Label at position of MnemArg1, Address at position of MnemArg2
        DisPage[i].MnemArg1 := LStr;
        DisPage[i].MnemArg2 := AStr;
      end;
      if DisPage[i].RefLabel and not PrefLabLst then begin
        // print the Label at position of MnemArg1, Address at position of MnemArg2
        DisPage[i].MnemArg1 := AStr;
        DisPage[i].MnemArg2 := LStr;
      end;

      // Mnemmonic
      if PrefDisPos[4] > 0 then RepString(DisPage[i].Mnem, S, PrefDisPos[4]);

      // Mnemonic Argument1
      SS := DisPage[i].MnemArg1;
      if PrefDisPos[5] > 0 then RepString(SS, S, PrefDisPos[5]);

      // Mnemonic Argument2
      if PrefDisPos[6] > 0 then RepString(DisPage[i].MnemArg2, S, PrefDisPos[6]);

      if PrefSDK41Mode and (DisPage[i].Mnem = '.BSS') then begin
        // special case, remove address and hexcodes
        RepString('    ', S, PrefDisPos[2]);   // remove address
        RepString('    ', S, PrefDisPos[3]);   // remove hexcode
      end;

    end;       // MCode line

    // Comment
    if DisPage[i].Comment <> '' then
       if PrefDisPos[11] > 0 then RepString('; ' + DisPage[i].Comment, S, PrefDisPos[11]);

    if (DisPage[i].DisTp = Dt_EmptyLineBefore) and (not PrefOneLine) then begin
      if EmptyLine then EmptyLine := false else begin
        ListWindow.FrmLister.EdtList.Lines.Add('');
        EmptyLine := true;
      end;
    end;

    if (DisPage[i].WrdTp <> Tp_UserProgram) and
       (DisPage[i].WrdTp <> Tp_FirstTwo) and InUserCode then begin
      // switch back to .MCODE
      InUserCode := false;
      S2 := '';
      // ListWindow.FrmLister.EdtList.Lines.Add('');
      RepString(Mnemonics[299, PrefMnem + 1], S2, PrefDisPos[4]);
      ListWindow.FrmLister.EdtList.Lines.Add(S2);
      if DisPage[i + 1].DisTp <> Dt_SkipLine then
         ListWindow.FrmLister.EdtList.Lines.Add('');
    end;

    if (i = $1000) then begin
      ListWindow.FrmLister.EdtList.Lines.Add('');
      ListWindow.FrmLister.EdtList.Lines.Add('; start of 2nd 4K block, Upper Page');
      ListWindow.FrmLister.EdtList.Lines.Add('');
    end;

    if (DisPage[i].DisTp <> Dt_SkipLine) or (PrefOneLine) or
       (PrefSDK41Mode and InUserCode) then begin
      ListWindow.FrmLister.EdtList.Lines.Add(S);
      EmptyLine := false;
    end;

    if PrefSDK41Mode and (DisPage[i].WrdTp = Tp_FunctionName) and
       (DisPage[i].DisTp <> Dt_SkipLine) then begin
      // list individual characters
      j := Length(DisPage[i].MnemArg1) - 2;          // number of chars in function name
      for l := i - j + 1 to i do begin
        W := DisPage[l].HexCode;
        S := '*' + Hex4(DisPage[l].Adr) + ' ' + Hex3(W);
        RepString('#' + Hex3(W), S, PrefDisPos[4]);  // mnemonic
        S2 := '; ' + '"' + HPChar(W) + '"';
        if (W and $300) <> $000 then
          S2 := S2 + '  WARNING: function name has prompting bits set';
        RepString(S2, S, PrefDisPos[11]); // comment
        ListWindow.FrmLister.EdtList.Lines.Add(S);
      end;
    end;

    if PrefSDK41Mode and (DisPage[i].WrdTp = Tp_MESLString) and
       (DisPage[i].DisTp <> Dt_SkipLine) then begin
      // list individual characters
      l := i;
      repeat
        W := DisPage[l].HexCode;
        S := '*' + Hex4(DisPage[l].Adr) + ' ' + Hex3(W);
        RepString('#' + Hex3(W), S, PrefDisPos[4]);  // mnemonic
        S2 := '; ' + '"' + HPCharP(W) + '"';
        if (W and $0C0) <> $000 then
          S2 := S2 + '  WARNING: string has punctuation bits set';
        RepString(S2, S, PrefDisPos[11]); // comment
        ListWindow.FrmLister.EdtList.Lines.Add(S);
        L := L + 1;
      until (W and $200) = $200;

    end;

    if (DisPage[i].DisTp = Dt_EmptyLineAfter) and (not PrefOneLine) and (not InUserCode) then begin
      ListWindow.FrmLister.EdtList.Lines.Add('');
      EmptyLine := true;
    end;

  end;

  // now generate Cross Refence Table for the labels



  if PrefGenXRef then begin
    FrmROMHandler.PnlStatus.Caption := 'GENERATE XREF LISTING';

    ListWindow.FrmLister.EdtList.Lines.Add('');
    ListWindow.FrmLister.EdtList.Lines.Add('; Label Cross reference table');
    ListWindow.FrmLister.EdtList.Lines.Add('');
    ListWindow.FrmLister.EdtList.Lines.Add('; LOCAL LABELS');
    ListWindow.FrmLister.EdtList.Lines.Add('; SYMBOL--------ADDR----REFERENCES-------------');

    // initialize Reference Table
    XRefList := TStringList.Create;
    XRefList.Sorted := true;
    for i := 0 to RomEnd do begin
      S := '';
      SS := DisPage[i].Lbl;
      if SS <> '' then begin
       // this address has a label, add to the list
       RepString(Hex4(DisPage[i].Adr), SS, 15);
       XRefList.Add(SS);
      end;
    end;

    // we now have a sorted list, now walk through this and find the references
    if XRefList.Count = 0 then
      ListWindow.FrmLister.EdtList.Lines.Add('; Local Label Table is empty')
    else for i  := 0 to XRefList.Count - 1 do begin
      SSS := XRefList.Strings[i];
      SS := GetFirstWord(SSS);                           // get label
      S := '; ' + XRefList.Strings[i] + '    ';          // build line
      // now find the references
      for j := 0 to RomEnd do begin
        if (DisPage[j].RefLbl = SS) and
           (DisPage[j].RefAdr > DisPage[0].Adr) and     // local labels only
           (DisPage[j].RefAdr < DisPage[RomEnd].Adr) then
          S := S + Hex4(DisPage[j].Adr) + '  ';
      end;
      ListWindow.FrmLister.EdtList.Lines.Add(S);
    end;

    XRefList.Clear;

    // now do the external references
    ListWindow.FrmLister.EdtList.Lines.Add('');
    ListWindow.FrmLister.EdtList.Lines.Add('; EXTERNAL REFERENCES');
    ListWindow.FrmLister.EdtList.Lines.Add('; SYMBOL--------VALUE---REFERENCES-------------');

    for i := 0 to RomEnd do begin
      // first find all external reference labels
      S := '; ';
      SS := DisPage[i].RefLbl;
      if SS <> '' then begin
        // now find the label, if it goes outside the current range it is external
        if (DisPage[i].RefAdr < DisPage[0].Adr) or
           (DisPage[i].RefAdr > DisPage[RomEnd].Adr) then
          // this is an external reference, check if seen before
          if not XRefList.Find(SS, Idx) then XRefList.Add(SS);
      end;
    end;
    // all references are in the sorted list, now complete the table
    // list may be empty!
    if (XRefList.Count <> 0) then for i  := 0 to XRefList.Count - 1 do begin
      SS := XRefList.Strings[i];                         // get label
      S := '; ' + SS;
      SSS := '';
      // now find the references
      for j := 0 to RomEnd do begin
        if (DisPage[j].RefLbl = SS) and
           ((DisPage[j].RefAdr < DisPage[0].Adr) or
           (DisPage[j].RefAdr > DisPage[RomEnd].Adr)) then begin
          SSS := SSS + Hex4(DisPage[j].Adr) + '  ';
          S2 := Hex4(DisPage[j].RefAdr);
        end;
      end;
      RepString(S2, S, 17);
      S := S + '    ' + SSS;
      ListWindow.FrmLister.EdtList.Lines.Add(S);
    end else
      ListWindow.FrmLister.EdtList.Lines.Add('; No external references');

    XRefList.Destroy;
  end;

  Application.ProcessMessages;
  ListWindow.FrmLister.EdtList.SelStart := 0;
  ListWindow.FrmLister.Show;
  ListWindow.FrmLister.Update;
  FrmROMHandler.PnlStatus.Caption := 'DONE';

end;

procedure ListIMDB;
// list HP41CL IMDB or FLDB ROM image
var
  S, S_pg, S_crc, S_state: string;
  i: integer;
  Wd1, Wd2, Wd3, Wd4: Word;
  ValidLine1, ValidLine2 : boolean;

  Img_group, Img_type: byte;
  Img_page: word;

  Type_comment: string;

  ID, GRP, COMMENT: string;

  IMDB_Dataset     : TSdfDataSet;
  IMGTypes_DataSet : TSdfDataSet;
  CurrentRecNo: integer;

  Res: boolean;

const
  AdrChar:  string[32] = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ123459';  // address nybbles to char
  DispChar: string[43] = '0123456789.......ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  //                      0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ
  //                      |               |               |
  //                      $30             $40             $50
  //



begin

  //prepare datastructures and read info from file imdb.txt, implement later


  ListWindow.FrmLister.Hide;
  ListWindow.FrmLister.EdtList.Clear;

{  if FrmROMHandler.ChkIMDB.Checked then begin
    // Read database of ROM names


    // create CSV file of Image Types and comments
    // IMGTypes_DataSet := TCSVDataset.Create(nil);
    IMGTypes_DataSet := TSdfDataSet.Create(nil);
    IMGTypes_DataSet.FileName := 'imgtypes.csv';

//    IMGTypes_DataSet.FileMustExist := true; //don't require existing csv file
    IMGTypes_DataSet.ReadOnly:= true;
//    IMGTypes_DataSet.Delimiter := ';';
//       IMGTypes_DataSet.FirstLineAsFieldNames := True;

      IMGTypes_DataSet.Delimiter := ';' ;


    // Add field definitions
//    IMGTypes_DataSet.FieldDefs.Add('ID', ftString);  /
//    IMGTypes_DataSet.Schema.Add('ID');

//    IMGTypes_DataSet.FieldDefs.Add('GRP', ftString);
//    IMGTypes_DataSet.Schema.Add('GRP');

//    IMGTypes_DataSet.FieldDefs.Add('COMMENT', ftString);
//    IMGTypes_DataSet.Schema.Add('COMMENT');

    try
      IMGTypes_DataSet.Active := True;
      CurrentRecNo := 1;     // Sets the initial record
      IMGTypes_DataSet.First;

    IMGTypes_DataSet.FindFirst;   // go to first record
      // catch exception here if needed (database not available)
    except
      // check what happens
    end;

    // As a test to access records, list the database first

    while not IMGTypes_DataSet.EOF do begin
      ID      := IMGTypes_DataSet.Fields[0].AsString;
      GRP     := IMGTypes_DataSet.Fields[1].AsString;
      COMMENT := IMGTypes_DataSet.Fields[2].AsString;
      IMGTypes_DataSet.Next;
      S := ID + Space2 + GRP + Space2 + COMMENT;
      ListWindow.FrmLister.EdtList.Lines.Add(S);
    end;

    S := '; IMDB List GENERATED: ';
    S := S + DateToStr(Now) + ' ' + TimeToStr(Now);
    S := S + ' by MLDL Manager version ' + AboutBox.GetVersion;
    ListWindow.FrmLister.EdtList.Lines.Add(S);
    S := '; OPEN FILE: ' + OpenROMFileNm;
    ListWindow.FrmLister.EdtList.Lines.Add(S);
    ListWindow.FrmLister.EdtList.Lines.Add('');

    // format of IMDB is (from HP41CL manual page 70
    // 4 words per entry
    //
    // word#   adress LSB   digit 3   digit 2   digit 1      digit 0
    //   1       00         <- image group ->   image type   addr <5>
    //   2       01         pg restr  type mod  addr <4>     addr <3>
    //   3       10           $0        $0      < char 2 module id  >
    //   4       11           $0        $2      < char 3 module id  >
    // whan all words are $FF.FF the entry is empty (unprogrammed) and is skipped in the listing

    // Listing is done in the order as found in the IMDB rom

    // Adr  GROUP   Type  Restr  mod  Page  Fix  IDENT   Comment (from file)
    // ---  -- ---  ----  -----  ---  ----  ---  - -- -  ------------------------------------
    // 000  00 NUL    0     0     0    345     002  A VI A  AVIA, Aviation ROM, Group: Aviation

    S := 'Adr  GROUP   Type  Restr  mod  Page  Fix  IDENT   Comment (from file)' ;
    ListWindow.FrmLister.EdtList.Lines.Add(S);
    S := '---  -- ---  ----  -----  ---  ----  ---  - -- -  ------------------------------------';
    ListWindow.FrmLister.EdtList.Lines.Add(S);

    // Should get date from IMDB first

    for i := 0 to $3FF do begin
      // 4 words per entry, max $3FF entries (HP41CL V5)

      S := Hex3(i) + Space2;             // Address
      Wd1 := Swap(PageArray[i * 4]);
      Wd2 := Swap(PageArray[i * 4 + 1]);
      Wd3 := Swap(PageArray[i * 4 + 2]);
      Wd4 := Swap(PageArray[i * 4 + 3]);

      if (Wd1 = $FFFF) and (Wd2 = $FFFF) and (Wd3 = $FFFF) and (Wd4 = $FFFF) then
        // skip this entry
        S := S + 'Unprogrammed entry'
      else begin

        Img_group := (Wd1 shr 8) and $00FF;
        Img_type  := (Wd1 shr 4) and $000F;
        Img_page  := Wd1 and $000F;

        S := S + Hex2(Img_group) + Space;

        // now find Group description

        ID := Hex2(Img_Group);
        IMGTypes_DataSet.First;

        Res := IMGTypes_DataSet.Locate('ID', ID, []);

        if IMGTypes_DataSet.Locate('ID', ID, []) then begin
          GRP := IMGTypes_DataSet.Fields[1].AsString;
          COMMENT := IMGTypes_DataSet.Fields[2].AsString;
        end else begin
          GRP :=  '---';
          COMMENT := '';
          GRP := IMGTypes_DataSet.Fields[1].AsString;
          COMMENT := IMGTypes_DataSet.Fields[2].AsString;
        end;


        S := S + GRP + Space2;

        S := S + Hex1(Img_type) + Space2;


      end;

      ListWindow.FrmLister.EdtList.Lines.Add(S);

    end;

    IMGTypes_DataSet.Close;


  end else begin }

    // list FLDB with CRC entries
    // mostly like a Hex Listing

    S := '; FLDB List GENERATED: ';
    S := S + DateToStr(Now) + ' ' + TimeToStr(Now);
    S := S + ' by MLDL Manager version ' + AboutBox.GetVersion;
    ListWindow.FrmLister.EdtList.Lines.Add(S);
    S := '; OPEN FILE: ' + OpenROMFileNm;
    ListWindow.FrmLister.EdtList.Lines.Add(S);
    ListWindow.FrmLister.EdtList.Lines.Add('');

    // format of FLDB is:
    //          CRC      CFLDB updt     CRC      CFLDB updt     CRC      CFLDB updt     CRC      CFLDB updt
    //       $1234.5678  $FFFF.FFFF  $1234.5678  $FFFF.FFFF  $1234.5678  $FFFF.FFFF  $1234.5678  $FFFF.FFFF
    // page:    i*16                 $200+i*16               i*16+1                  $200+i*16+1
    //
    //       Listing is done in page order, 2 per line, 2 passes are done, printout looks like:
    //   Page  CRC         status          Page  CRC         status
    //   ----  ---------  ----------       ----  ---------   ----------
    //   000   1234.5678  unverified       001   1234.5678   unverified
    //
    //   status can be unverified/up-to-date/to update

    S := 'Page  CRC        status           Page  CRC        status' ;
    ListWindow.FrmLister.EdtList.Lines.Add(S);
    S := '----  ---------  ----------       ----  ---------  ----------';
    ListWindow.FrmLister.EdtList.Lines.Add(S);

    for i := 0 to $FF do begin
      // 16 words per line

      ValidLine1 := true;
      ValidLine2 := true;

      S_pg  := Hex3(i * 2) + '   ';
      Wd1   := Swap(PageArray[i * 16]);      // CRC LSW
      Wd2   := Swap(PageArray[i * 16 + 1]);  // CRC MSW
      S_crc := Hex4(Wd2) + '.' + Hex4(Wd1) + '  ';

      if (Wd1 = $FFFF) and (Wd2 = $FFFF) then ValidLine1 := false;

      Wd1   := Swap(PageArray[i * 16 + 2]);  // status LSW
      Wd2   := Swap(PageArray[i * 16 + 3]);  // status MSW

      if (Wd1 = $FFFF) and (Wd2 = $FFFF) then
        S_state := 'unverified' else
        if (Wd1 = $0000) and (Wd2 = $0000) then
          S_state := 'up-to-date' else
            S_state := '! update !';

      S := S_Pg + S_crc + S_state + '       ';

      S_pg  := Hex3(i * 2 + 1) + '   ';
      Wd1   := Swap(PageArray[i * 16 + 8]);  // CRC LSW
      Wd2   := Swap(PageArray[i * 16 + 9]);  // CRC MSW
      S_crc := Hex4(Wd2) + '.' + Hex4(Wd1) + '  ';

      if (Wd1 = $FFFF) and (Wd2 = $FFFF) then ValidLine2 := false;

      Wd1   := Swap(PageArray[i * 16 + $A]);  // status LSW
      Wd2   := Swap(PageArray[i * 16 + $B]);  // status MSW

      if (Wd1 = $FFFF) and (Wd2 = $FFFF) then
        S_state := 'unverified' else
        if (Wd1 = $0000) and (Wd2 = $0000) then
          S_state := 'up-to-date' else
            S_state := '! update !';

      // if (Wd1      else           S_state := '! update !';

      S := S + S_Pg + S_crc + S_state;

      if ValidLine1 or ValidLine2 then ListWindow.FrmLister.EdtList.Lines.Add(S);

    end;

    // 2nd pass for page > $1FF

    S := '';
    ListWindow.FrmLister.EdtList.Lines.Add(S);
    S := 'Page  CRC        status           Page  CRC        status' ;
    ListWindow.FrmLister.EdtList.Lines.Add(S);
    S := '----  ---------  ----------       ----  ---------  ---------- ';
    ListWindow.FrmLister.EdtList.Lines.Add(S);

    for i := 0 to $FF do begin
      // 16 words per line
      ValidLine1 := true;
      ValidLine2 := true;
      S_pg  := Hex3(i * 2 + $200) + '   ';
      Wd1   := Swap(PageArray[i * 16 + 4]);      // CRC LSW
      Wd2   := Swap(PageArray[i * 16 + 5]);  // CRC MSW
      S_crc := Hex4(Wd2) + '.' + Hex4(Wd1) + '  ';

      if (Wd1 = $FFFF) and (Wd2 = $FFFF) then ValidLine1 := false;

      Wd1   := Swap(PageArray[i * 16 + 6]);  // status LSW
      Wd2   := Swap(PageArray[i * 16 + 7]);  // status MSW

      if (Wd1 = $FFFF) and (Wd2 = $FFFF) then
        S_state := 'unverified' else
        if (Wd1 = $0000) and (Wd2 = $0000) then
          S_state := 'up-to-date' else
            S_state := '! update !';

      S := S_Pg + S_crc + S_state + '       ';

      S_pg  := Hex3(i * 2 + $201) + '   ';
      Wd1   := Swap(PageArray[i * 16 + $C]);  // CRC LSW
      Wd2   := Swap(PageArray[i * 16 + $D]);  // CRC MSW
      S_crc := Hex4(Wd2) + '.' + Hex4(Wd1) + '  ';

      if (Wd1 = $FFFF) and (Wd2 = $FFFF) then ValidLine2 := false;

      Wd1   := Swap(PageArray[i * 16 + $E]);  // status LSW
      Wd2   := Swap(PageArray[i * 16 + $F]);  // status MSW

      if (Wd1 = $FFFF) and (Wd2 = $FFFF) then
        S_state := 'unverified' else
        if (Wd1 = $0000) and (Wd2 = $0000) then
          S_state := 'up-to-date' else
            S_state := '! update !';

      // if (Wd1      else           S_state := '! update !';

      S := S + S_Pg + S_crc + S_state;

      if ValidLine1 or ValidLine2 then ListWindow.FrmLister.EdtList.Lines.Add(S);

    // end;

  end;

  Application.ProcessMessages;
  ListWindow.FrmLister.EdtList.SelStart := 0;
  ListWindow.FrmLister.Show;
  ListWindow.FrmLister.Update;


end;



end.
