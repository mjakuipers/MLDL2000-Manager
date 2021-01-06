unit Tester;

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
//  TESTER.PAS                                                               //
//  Unit for production testing of M2kM, note that absolute paths are used   //
//  Ver  Date     Description                                                //
//  1.00 Nov 2005 First version for release                                  //
//  1.01 Nov 2006 Compiled for Turbo Explorer                                //
//  1.20 Apr 2008 Added code to save window positions                        //
//  1.50 May 2008 Final release                                              //
//  1.51 Sep 2008 Changed saving of screen position                          //
//  1.70 Jul 2010 Adapted for Lazarus                                        //
//  1.80 Feb 2018 restarted development                                      //
//                removed Delphi compatibility                               //
//---------------------------------------------------------------------------//

interface

uses
  LCLIntf, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, LResources, GlobalConstants, Globals, Procs_MLDL,
  Procs, Math, StrUtils, IniFiles;

type
  TFrm_Tester = class(TForm)
    BtnSRAM: TButton;
    BtnFLASH: TButton;
    StaticText1: TStaticText;
    Memo1: TMemo;
    RadType: TRadioGroup;
    RadTest: TRadioGroup;
    StaticText2: TStaticText;
    ChkDoubleMem: TCheckBox;
    BtnTestAll: TButton;
    ChkDetail: TCheckBox;
    BTNQuickSRAM: TButton;
    BtnQuickALL: TButton;
    BtnQuickFLASH: TButton;
    BtnSTOP: TButton;
    BtnProduction: TButton;
    ChkSRAMOnly: TCheckBox;
    procedure BtnSRAMClick(Sender: TObject);
    procedure BtnFLASHClick(Sender: TObject);
    procedure BtnSTOPClick(Sender: TObject);
    procedure BTNQuickSRAMClick(Sender: TObject);
    procedure BtnQuickFLASHClick(Sender: TObject);
    procedure BtnTestAllClick(Sender: TObject);
    procedure BtnQuickALLClick(Sender: TObject);
    procedure BtnProductionClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Frm_Tester : TFrm_Tester;

  TestSTOP   : boolean;

implementation




procedure MemoryTester(MemTp: MemType; TestSize: integer);
// Starts testing Memory, Random test only
var
  RomNum, i: integer;
  S: String;
  Test: Word;
  SRAMSize : integer;
  ErrorFound, ErrorsFound : boolean;
begin

  TestSTOP := false;
  DisableMLDL;
  if MemTp = SRAM then S := 'Start testing SRAM' else S := 'Start testing FLASH';
  Frm_Tester.Memo1.Lines.Add(S);

  if Frm_Tester.ChkDoubleMem.Checked and (MemTp = SRAM) then SRAMSize := 255 else SRAMSize := 127;
  if MemTp = FLASH then SRAMSize := 255;
  if TestSize = 1 then SRAMSize := 1;

  ErrorsFound := false;

  if MemTp = FLASH then begin
    // Erase FLASH memory
    Frm_Tester.Memo1.Lines.Add('First Erase FLASH');
    MLDL_FLASHReset;
    if TestSize = 1 then MLDL_FLASHEraseSector(0) else MLDL_FLASHEraseAll;
    MLDL_FLASHReset;
    Frm_Tester.Memo1.Lines.Add('FLASH erased, now start test');
  end;

  if (Frm_Tester.RadTest.ItemIndex = 0) or (Frm_Tester.RadTest.ItemIndex = 2) then begin
    if MemTp = SRAM then RandSeed := 0 else RandSeed := 1;
    for RomNum := 0 to SRAMSize do begin
      S := '  ROM Number ' + SStr(RomNUm) + ' @' + Hex5(RomNum * (Range_4k + 1)) + '  Writing';
      Frm_Tester.Memo1.Lines.Add(S);
      // First fill WordArray
      for i := 0 to Range_4k do WordArray[i] := RandomRange(0, Range_64k);
      MLDL_WriteBlock(MemTp, RomNum * (Range_4k + 1), Range_4k + 1);
      Application.ProcessMessages;
      if TestSTOP = true then Break;
    end;
    if (TestStop = true) then Frm_Tester.Memo1.Lines.Add('Test Interrupted');
  end;

  if (Frm_Tester.RadTest.ItemIndex = 1) or (Frm_Tester.RadTest.ItemIndex = 2) then begin
    Frm_Tester.Memo1.Lines.Add('Start testing');
    if MemTp = SRAM then RandSeed := 0 else RandSeed := 1;
    for RomNum := 0 to SRAMSize do begin
      if TestStop = true then Break;
      ErrorFound := false;
      S := '  ROM Number ' + SStr(RomNUm) + ' @' + Hex5(RomNum * (Range_4k + 1)) + '  Testing';
      Frm_Tester.Memo1.Lines.Add(S);
      MLDL_ReadBlock(MemTp, RomNum * (Range_4k + 1), Range_4k + 1);
      Application.ProcessMessages;
      for i := 0 to Range_4k do begin
        if TestSTOP = true then Break;
        Test := RandomRange(0, Range_64k);
        if Test <> WordArray[i] then begin
          ErrorFound := true;
          ErrorsFound := true;
          if Frm_Tester.ChkDetail.Checked then begin
            S := 'ERROR: $' + Hex5(RomNum * (Range_4k + 1) + i) + ' ';
            S := S + '$' + Hex4(WordArray[i]) + '  should be $' + Hex4(Test);
            Frm_Tester.Memo1.Lines.Add(S);
          end;
          Application.ProcessMessages;
        end;
      end;
      if (not Frm_Tester.ChkDetail.Checked) and ErrorFound then Frm_Tester.Memo1.Lines.Add('Error found');
    end;
    if TestStop = true then S := 'Test Interrupted' else S := 'Memory test ready';
    Frm_Tester.Memo1.Lines.Add(S);
    if ErrorsFound then Frm_Tester.Memo1.Lines.Add('ERRORS FOUND!') else Frm_Tester.Memo1.Lines.Add('ALL OK!');
  end;
  EnableMLDL;
end;

procedure TFrm_Tester.BtnSRAMClick(Sender: TObject);
begin
  Frm_Tester.Memo1.Lines.Clear;
  MemoryTester(SRAM, 127);
end;

procedure TFrm_Tester.BtnFLASHClick(Sender: TObject);
begin
  Frm_Tester.Memo1.Lines.Clear;
  MemoryTester(FLASH, 255);
end;

procedure TFrm_Tester.BtnSTOPClick(Sender: TObject);
begin
  TestSTOP := true;
end;

procedure TFrm_Tester.BTNQuickSRAMClick(Sender: TObject);
begin
  Frm_Tester.Memo1.Lines.Clear;
  MemoryTester(SRAM, 1);
end;

procedure TFrm_Tester.BtnQuickFLASHClick(Sender: TObject);
begin
  Frm_Tester.Memo1.Lines.Clear;
  MemoryTester(FLASH,1);
end;

procedure TFrm_Tester.BtnTestAllClick(Sender: TObject);
begin
  Frm_Tester.Memo1.Lines.Clear;
  MemoryTester(SRAM, 127);
  MemoryTester(FLASH, 255);
end;


procedure TFrm_Tester.BtnQuickALLClick(Sender: TObject);
begin
  Frm_Tester.Memo1.Lines.Clear;
  MemoryTester(SRAM, 1);
  MemoryTester(FLASH, 1);
end;


procedure TFrm_Tester.BtnProductionClick(Sender: TObject);
// Shipping Configuration for MLDL2000
var
  ROMfile: File;
  SRFile: TextFile;
  NumRead, i: integer;

  procedure ParseSRFile;
  var
    j, IntVal, Position: integer;
    Line: string;
  begin
    FileMode := 0;
    Reset(SRFile);
    j := 0;
    while (not Eof(SRFile)) and (j <= SRSize) do begin
      // read and parse one line of the SR File
      Readln(SRFile, Line);
      if (Length(Line) > 0) and (Line[1] in HexSet) then begin
        // valid hex character, parse line
        IntVal := HexToInt(Line);
        if IntVal >= 0 then begin
          SRArray[j] := IntVal;
          // read comment, first find "--"
          Position := Pos('--', Line);
          if Position = 0 then
            SRComment[j] := ''
          else
            SRComment[j] := MidStr(Line, Position + 3, 100);
          j := j + 1;
        end;
      end;
    end;
    CloseFile(SRFile);
  end; // ParseSRFile


begin   // BtnProductionClick
  DisableMLDL;
  Frm_Tester.Memo1.Lines.Clear;
  Frm_Tester.Memo1.Lines.Add('Create Shipping Configuration for MLDL2000');

  // First do a quick final check
  MemoryTester(SRAM, 1);
  if not ChkSRAMOnly.Checked then MemoryTester(FLASH, 1);

  // Erase all FLASH

  if not ChkSRAMOnly.Checked then begin
    Frm_Tester.Memo1.Lines.Add('Erase ALL FLASH, please wait 30-60 seconds');
    MLDL_FLASHReset;
    MLDL_FLASHEraseAll;
    MLDL_FLASHReset;
  end;

  // open .ROM file
  Frm_Tester.Memo1.Lines.Add('Opening file: M2K ROM.rom');
  AssignFile(ROMFile, 'C:\Xilinx\Designs\MLDL2K\Software\ROM Repository\Released\M2K ROM.rom');
  FileMode := 0;
  Reset(ROMFile, 1);
  BlockRead(ROMFile, PageArray, SizeOf(PageArray), NumRead);
  CloseFile(ROMFile);
  for i := 0 to Range_4k do WordArray[i] := swap(PageArray[i]);

  // Write .ROM file to SRAM and FLASH
  if not ChkSRAMOnly.Checked then begin
    Frm_Tester.Memo1.Lines.Add('Writing M2K ROM to FLASH');
    MLDL_WriteBlock(FLASH, 0, Range_4k + 1);
  end;

  Frm_Tester.Memo1.Lines.Add('Writing M2K ROM to SRAM');
  MLDL_WriteBlock(SRAM, 0, Range_4k + 1);

  // CCD ROM L to SRAM block 1
  Frm_Tester.Memo1.Lines.Add('Opening file: CCDL-1B.rom');
  AssignFile(ROMFile, 'C:\Xilinx\Designs\MLDL2K\Software\ROM Repository\Released\CCDL-1B.rom');
  FileMode := 0;
  Reset(ROMFile, 1);
  BlockRead(ROMFile, PageArray, SizeOf(PageArray), NumRead);
  CloseFile(ROMFile);
  for i := 0 to Range_4k do WordArray[i] := swap(PageArray[i]);
  Frm_Tester.Memo1.Lines.Add('Writing CCDL-1B.rom to SRAM');
  MLDL_WriteBlock(SRAM, 1 * (Range_4k + 1), Range_4k + 1);

  // CCD ROM U to SRAM block 2
  Frm_Tester.Memo1.Lines.Add('Opening file: CCDU-2B.rom');
  AssignFile(ROMFile, 'C:\Xilinx\Designs\MLDL2K\Software\ROM Repository\Released\CCDU-2B.rom');
  FileMode := 0;
  Reset(ROMFile, 1);
  BlockRead(ROMFile, PageArray, SizeOf(PageArray), NumRead);
  CloseFile(ROMFile);
  for i := 0 to Range_4k do WordArray[i] := swap(PageArray[i]);
  Frm_Tester.Memo1.Lines.Add('Writing CCDU-2B.rom to SRAM');
  MLDL_WriteBlock(SRAM, 2 * (Range_4k + 1), Range_4k + 1);

  // DAVID ASSEMBLER to SRAM block 3
  Frm_Tester.Memo1.Lines.Add('Opening file: David-2C.rom');
  AssignFile(ROMFile, 'C:\Xilinx\Designs\MLDL2K\Software\ROM Repository\Released\David-2C.rom');
  FileMode := 0;
  Reset(ROMFile, 1);
  BlockRead(ROMFile, PageArray, SizeOf(PageArray), NumRead);
  CloseFile(ROMFile);
  for i := 0 to Range_4k do WordArray[i] := swap(PageArray[i]);
  Frm_Tester.Memo1.Lines.Add('Writing David-2C.rom to SRAM');
  MLDL_WriteBlock(SRAM, 3 * (Range_4k + 1), Range_4k + 1);


  // Open and program the .SR files
  if not ChkSRAMOnly.Checked then begin
    Frm_Tester.Memo1.Lines.Add('Opening and writing file: test version SR0 Flash.sr');
    AssignFile(SRFile, 'C:\Xilinx\Designs\MLDL2K\Software\ROM Repository\Released\test version SR0 Flash.sr');
    ParseSRFile;
    for i := 0 to Range_SR do WordArray[i] := SRArray[i];
    MLDL_WriteBlock(FLASH, SR0_Base, Range_SR + 1);

    Frm_Tester.Memo1.Lines.Add('Opening and writing file: test version SR1 Flash.sr');
    AssignFile(ROMFile, 'C:\Xilinx\Designs\MLDL2K\Software\ROM Repository\Released\test version SR1 Flash.sr');
    ParseSRFile;
    for i := 0 to Range_SR do WordArray[i] := SRArray[i];
    MLDL_WriteBlock(FLASH, SR1_Base, Range_SR + 1);

    Frm_Tester.Memo1.Lines.Add('Opening and writing file: test version SR2 Flash.sr');
    AssignFile(ROMFile, 'C:\Xilinx\Designs\MLDL2K\Software\ROM Repository\Released\test version SR2 Flash.sr');
    ParseSRFile;
    for i := 0 to Range_SR do WordArray[i] := SRArray[i];
    MLDL_WriteBlock(FLASH, SR2_Base, Range_SR + 1);

    Frm_Tester.Memo1.Lines.Add('Opening and writing file: test version SR3 Flash.sr');
    AssignFile(ROMFile, 'C:\Xilinx\Designs\MLDL2K\Software\ROM Repository\Released\test version SR3 Flash.sr');
    ParseSRFile;
    for i := 0 to Range_SR do WordArray[i] := SRArray[i];
    MLDL_WriteBlock(FLASH, SR3_Base, Range_SR + 1);

  end;

  // Test Configuration to SRAM
  Frm_Tester.Memo1.Lines.Add('Opening and writing file: test version SR0 SRAM.sr');
  AssignFile(SRFile, 'C:\Xilinx\Designs\MLDL2K\Software\ROM Repository\Released\test version SR0 SRAM.sr');
  ParseSRFile;
  for i := 0 to Range_SR do WordArray[i] := SRArray[i];
  MLDL_WriteBlock(SRAM, SR0_Base, Range_SR + 1);

  Frm_Tester.Memo1.Lines.Add('Opening and writing file: test version SR1 SRAM.sr');
  AssignFile(SRFile, 'C:\Xilinx\Designs\MLDL2K\Software\ROM Repository\Released\test version SR1 SRAM.sr');
  ParseSRFile;
  for i := 0 to Range_SR do WordArray[i] := SRArray[i];
  MLDL_WriteBlock(SRAM, SR1_Base, Range_SR + 1);

  Frm_Tester.Memo1.Lines.Add('Opening and writing file: test version SR2 SRAM.sr');
  AssignFile(SRFile, 'C:\Xilinx\Designs\MLDL2K\Software\ROM Repository\Released\test version SR2 SRAM.sr');
  ParseSRFile;
  for i := 0 to Range_SR do WordArray[i] := SRArray[i];
  MLDL_WriteBlock(SRAM, SR2_Base, Range_SR + 1);

  Frm_Tester.Memo1.Lines.Add('Opening and writing file: test version SR3 SRAM.sr');
  AssignFile(SRFile, 'C:\Xilinx\Designs\MLDL2K\Software\ROM Repository\Released\test version SR3 SRAM.sr');
  ParseSRFile;
  for i := 0 to Range_SR do WordArray[i] := SRArray[i];
  MLDL_WriteBlock(SRAM, SR3_Base, Range_SR + 1);

  Frm_Tester.Memo1.Lines.Add('DONE');

  EnableMLDL;

end;


procedure TFrm_Tester.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
const
  T = -1;  // default Top
  L = -1;  // default Left
begin
  Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
  try
    Frm_Tester.Top:= Ini.ReadInteger( 'WindowsPositions', 'TesterTop', T);
    Frm_Tester.Left:= Ini.ReadInteger( 'WindowsPositions', 'TesterLeft', L);

    if not WinVisible(Frm_Tester) then begin
      // window outside current screen? Set to defaults
      Frm_Tester.Top    := T;
      Frm_Tester.Left   := L;
    end;

  finally
    Ini.Free
  end;
end;




procedure TFrm_Tester.FormDestroy(Sender: TObject);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.INI' ));
  try
    Ini.WriteInteger( 'WindowsPositions', 'TesterTop', Frm_Tester.Top);
    Ini.WriteInteger( 'WindowsPositions', 'TesterLeft', Frm_Tester.Left);
  finally
    Ini.Free;
  end;
end;

initialization
  {$i Tester.lrs}

end.
