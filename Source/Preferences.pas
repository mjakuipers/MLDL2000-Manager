unit Preferences;

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
//  PREFERENCES.PAS                                                          //
//  Handling of Preferences and .INI file                                    //
//  Ver  Date     Description                                                //
//  0.10 Mar 2008 First Version                                              //
//  1.20 Apr 2008 Changes structure, added some variables                    //
//       Apr 2008 Added SkipNOPS                                             //
//  1.50 May 2008 Final release                                              //
//  1.51 Sep 2008 Changed saving of screen position                          //
//  1.60 Jan 2010 Modified for new disassembler options                      //
//  1.70 Jul 2010 Adapted for Lazarus                                        //
//  1.80 Feb 2018 restarted development                                      //
//                removed Delphi compatibility                               //
//---------------------------------------------------------------------------//


interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Forms,
  Dialogs, StdCtrls, ExtCtrls, Globals, GlobalConstants, IniFiles, Procs,
  BitBang_Unit, About, MemEdit, Procs_MLDL, Jtag, Tester, Romdis,
  ListWindow, Controls, LResources, HP41_Globals;

type
  TFrmPreferences = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ChkComments: TCheckBox;
    ChkAutoLabel: TCheckBox;
    ChkMainFrame: TCheckBox;
    ChkXROM: TCheckBox;
    GrpMnem: TRadioGroup;
    EdtCommSpeed: TEdit;
    EdtJTAGSpeed: TEdit;
    EdtUSBTO: TEdit;
    EdtFLASHTO: TEdit;
    EdtSecTO: TEdit;
    EdtALLTO: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    ChkAutoVerify: TCheckBox;
    ChkConfDown: TCheckBox;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnApply: TButton;
    LblMPSSE: TLabel;
    LblJTAG: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    CmbBlockSize: TComboBox;
    ChkAutoFind: TCheckBox;
    ChkSRAMDefault: TCheckBox;
    ChkSkipNOPS: TCheckBox;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit14: TEdit;
    Edit15: TEdit;
    Edit16: TEdit;
    Edit17: TEdit;
    Edit18: TEdit;
    Edit19: TEdit;
    Edit20: TEdit;
    Edit21: TEdit;
    Edit22: TEdit;
    ChkLabLst: TCheckBox;
    ChkMESLArg: TCheckBox;
    ChkOneLine: TCheckBox;
    Label24: TLabel;
    Label26: TLabel;
    ChkSDK41Mode: TCheckBox;
    Label25: TLabel;
    ChkXRef: TCheckBox;
    ChkAutoFixed: TCheckBox;
    ChkCleanList: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure EdtCommSpeedChange(Sender: TObject);
    procedure EdtJTAGSpeedChange(Sender: TObject);
    procedure SaveIni;
    procedure ReadIni;
    procedure FormDestroy(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmPreferences: TFrmPreferences;
  MPSSEValue, JTAGValue: integer;

implementation

uses ROMHandler;


procedure TFrmPreferences.SaveIni;
var
  Ini: tMemIniFile;
begin
  Ini := tMemIniFile.Create(ChangeFileExt(Application.ExeName, '.INI' ));
  try
    // Disassembler section
    Ini.WriteBool('Disassembler', 'Comments', PrefComments);
                   // if true, will generate auto comments in
    Ini.WriteBool('Disassembler', 'AutoLabel', PrefAutoLabel);
                   // if true, will generate auto labels for jump addresses
    Ini.WriteBool('Disassembler', 'MainFrame', PrefMainFrame);
                   // if true, will use Mainframe label file
    Ini.WriteBool('Disassembler', 'XROM', PrefXROM);
                   // if true, will use XROM file
    Ini.WriteBool('Disassembler', 'SkipNOPS', PrefSkipNOPs);
                   // if true, skips NOPs
    Ini.WriteBool('Disassembler', 'LabLst', PrefLabLst);
                   // if true, skips NOPs
    Ini.WriteBool('Disassembler', 'MESLArg', PrefMESLArg);
                   // if true, skips NOPs
    Ini.WriteInteger('Disassembler', 'Mnemonics', PrefMnem);
                   // indicates Mnemonics type:  0 - Jacobs/De Arras, Default
                   //                            1 - HP
                   //                            2 - ZenCode
                   //                            3 - SDK41 (JDA only)
    Ini.WriteBool('Disassembler', 'OneLine', PrefOneLine);
                   // if true, prints a line for every word
    Ini.WriteBool('Disassembler', 'SDK41Mode', PrefSDK41Mode);
                   // if true, generates listing for SDK41 re-assembly
    Ini.WriteBool('Disassembler', 'GenXRef', PrefGenXRef);
                   // Generates label Cross Refenence table
    Ini.WriteBool('Disassembler', 'AutoFixed', PrefAutoFixed);
                   // Generates Auto LAbels for fixed XROM addresses
    Ini.WriteBool('Disassembler', 'CleanListing', PrefCleanList);
                   // Do not print adress or hexcodes in listing


    Ini.WriteInteger('DisAssembler', 'Pos_Lbl'      ,PrefDisPos[ 1]);
    Ini.WriteInteger('DisAssembler', 'Pos_Adr'      ,PrefDisPos[ 2]);
    Ini.WriteInteger('DisAssembler', 'Pos_HexCodes' ,PrefDisPos[ 3]);
    Ini.WriteInteger('DisAssembler', 'Pos_Mnem'     ,PrefDisPos[ 4]);
    Ini.WriteInteger('DisAssembler', 'Pos_MnemArg1' ,PrefDisPos[ 5]);
    Ini.WriteInteger('DisAssembler', 'Pos_MnemArg2' ,PrefDisPos[ 6]);
    Ini.WriteInteger('DisAssembler', 'Pos_RefAdr'   ,PrefDisPos[ 7]);
    Ini.WriteInteger('DisAssembler', 'Pos_RefLbl'   ,PrefDisPos[ 8]);
    Ini.WriteInteger('DisAssembler', 'Pos_UCodeLn'  ,PrefDisPos[ 9]);
    Ini.WriteInteger('DisAssembler', 'Pos_HexCode'  ,PrefDisPos[10]);
    Ini.WriteInteger('DisAssembler', 'Pos_Comment'  ,PrefDisPos[11]);

    // Communication Section
    Ini.WriteInteger('Communication', 'CommSpeed', PrefCommSpeed);
                      // MPSSE Communication Speed $0000 - $FFFF
    Ini.WriteInteger('Communication', 'JTAGSpeed', PrefJTAGSpeed);
                      // MPSSE Speed for use with JTAG: $0000 - $FFFF
    Ini.WriteInteger('Communication', 'USBTimeOut', PrefUSBTO);
                      // regular USB timeout in msec
    Ini.WriteInteger('Communication', 'FLASHTimeout', PrefFLASHTO);
                      // single byte FLASH write timeout in msec
    Ini.WriteInteger('Communication', 'SectorTimeout', PrefSecTO);
                      // FLASH Sector Erase timeout in msec
    Ini.WriteInteger('Communication', 'EraseAllTimeout', PrefALLTO);
                      // FLASH ALl Erase timeout in seconds !
    Ini.WriteInteger('Communication', 'Blocksize', PrefBlockSize);
                      // MPSSE Communication block size

    // User Interface Section
    // Enable Automatic Verify after read/write
    Ini.WriteBool('UserInterface', 'AutoVerify', PrefAutoVerify);
    // Enable Confirm on Download
    Ini.WriteBool('UserInterface', 'ConfirmDownload', PrefConfDown);
    Ini.WriteBool('UserInterface', 'AutoFindROM', PrefAutoFind);
    Ini.WriteBool('UserInterface', 'AutoFindROM', PrefMemType);

    // Remember Windows positions
    Ini.WriteInteger( 'WindowsPositions', 'PrefTop', FrmPreferences.Top);
    Ini.WriteInteger( 'WindowsPositions', 'PrefLeft', FrmPreferences.Left);

    Ini.UpdateFile;

  finally
    Ini.Destroy;
  end;
end;

procedure TFrmPreferences.ReadIni;
  var
  Ini: tMemIniFile;
begin
  Ini := tMemIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
  try
    // Disassembler section
    PrefComments   := Ini.ReadBool('Disassembler', 'Comments', true);
                     // if true, will generate auto comments in
    PrefAutoLabel  := Ini.ReadBool('Disassembler', 'AutoLabel', true);
                     // if true, will generate auto labels for jump addresses
    PrefMainFrame  := Ini.ReadBool('Disassembler', 'MainFrame', false);
                     // if true, will use Mainframe label file
    PrefXROM       := Ini.ReadBool('Disassembler', 'XROM', true);
                     // if true, will use XROM file
    PrefSkipNOPs   := Ini.ReadBool('Disassembler', 'SkipNOPs', true);
    PrefLabLst     := Ini.ReadBool('Disassembler', 'LabLst', true);
    PrefMESLArg    := Ini.ReadBool('Disassembler', 'MESLArg', true);

    PrefMnem       := Ini.ReadInteger('Disassembler', 'Mnemonics', 0);
                   // indicates Mnemonics type:  0 - Jacobs/De Arras, Default
                   //                            1 - HP
                   //                            2 - ZenCode
                   //                            3 - SDK41 (JDA only)

    PrefOneLine    := Ini.ReadBool('Disassembler', 'OneLine', false);
                   // if true, prints a line for every word
    PrefSDK41Mode  := Ini.ReadBool('Disassembler', 'SDK41Mode', false);
                   // if true, generates listing for SDK41 re-assembly
    PrefGenXRef    := Ini.ReadBool('Disassembler', 'GenXRef', true);
                   // Generates label Cross Refenence table
    PrefAutoFixed  := Ini.ReadBool('Disassembler', 'AutoFixed', true);
                   // Generates Auto LAbels for fixed XROM addresses
    PrefCleanList  := Ini.ReadBool('Disassembler', 'CleanListing', false);
                   // Generates Auto LAbels for fixed XROM addresse

    PrefDisPos[ 1] := Ini.ReadInteger('Disassembler', 'Pos_Lbl'      , 13);
    PrefDisPos[ 2] := Ini.ReadInteger('Disassembler', 'Pos_Adr'      ,  6);
    PrefDisPos[ 3] := Ini.ReadInteger('Disassembler', 'Pos_HexCodes' , 13);
    PrefDisPos[ 4] := Ini.ReadInteger('Disassembler', 'Pos_Mnem'     , 10);
    PrefDisPos[ 5] := Ini.ReadInteger('Disassembler', 'Pos_MnemArg1' , 10);
    PrefDisPos[ 6] := Ini.ReadInteger('Disassembler', 'Pos_MnemArg2' , 10);
    PrefDisPos[ 7] := Ini.ReadInteger('Disassembler', 'Pos_RefAdr'   ,  7);
    PrefDisPos[ 8] := Ini.ReadInteger('Disassembler', 'Pos_RefLbl'   , 20);
    PrefDisPos[ 9] := Ini.ReadInteger('Disassembler', 'Pos_UCodeLn'  ,  4);
    PrefDisPos[10] := Ini.ReadInteger('Disassembler', 'Pos_HexCode'  ,  5);
    PrefDisPos[11] := Ini.ReadInteger('Disassembler', 'Pos_Comment'  ,  0);

    // Communication Section
    MPSSEValue  := Ini.ReadInteger('Communication', 'CommSpeed', MPSSE_Speed);
                     // MPSSE Communication Speed $0000 - $FFFF
    if (MPSSEValue >= 0) and (MPSSEValue < 65535) then
      PrefCommSpeed  := MPSSEValue
    else
      PrefCommSpeed  := MPSSE_Speed;

    JTAGValue        := Ini.ReadInteger('Communication', 'JTAGSpeed', 20);
                     // MPSSE Speed for use with JTAG: $0000 - $FFFF
    if (JTAGValue >= 0) and (JTAGValue < 65535) then
      PrefJTAGSpeed  := JTAGValue
    else
      PrefJTAGSpeed  := JTAGValue;

    PrefUSBTO      := Ini.ReadInteger('Communication', 'USBTimeOut', TimeOut_USB);
                     // regular USB timeout in msec
    PrefFLASHTO    := Ini.ReadInteger('Communication', 'FLASHTimeout', TimeOut_Flash_Byte);
                     // single byte FLASH write timeout in msec
    PrefSecTO      := Ini.ReadInteger('Communication', 'SectorTimeout', TimeOut_FLASH_Sector);
                     // FLASH Sector Erase timeout in msec
    PrefALLTO      := Ini.ReadInteger('Communication', 'EraseAllTimeout', TimeOut_FLASH_Erase);
                     // FLASH ALl Erase timeout in seconds !
    PrefBlockSize  := Ini.ReadInteger('Communication', 'Blocksize', 256);
                     // MPSSE Communication block size

    // User Interface Section
    PrefAutoVerify := Ini.ReadBool('UserInterface', 'AutoVerify', true);
    // Enable Automatic Verify after read/write
    PrefConfDown   := Ini.ReadBool('UserInterface', 'ConfirmDownload', true);
    // Enable Confirm on Download
    PrefAutoFind   := Ini.ReadBool('UserInterface', 'AutoFindROM', true);
    PrefMemType    := Ini.ReadBool('UserInterface', 'MemoryTypeSRAM', true);

  finally
    Ini.Free
  end;
end;

procedure TFrmPreferences.BtnApplyClick(Sender: TObject);
// Apply Changes and write these back to the INI file
var
  Code, i: integer;
begin
  PrefComments   := ChkComments.Checked;
  PrefAutoLabel  := ChkAutoLabel.Checked;
  PrefMainFrame  := ChkMainFrame.Checked;
  PrefXROM       := ChkXROM.Checked;
  PrefSkipNOPs   := ChkSkipNOPs.Checked;
  PrefLabLst     := ChkLabLst.Checked;
  PrefMESLArg    := ChkMESLArg.Checked;
  PrefMnem       := GrpMnem.ItemIndex;
  PrefOneLine    := ChkOneLine.Checked;
  PrefSDK41Mode  := ChkSDK41Mode.Checked;
  PrefGenXRef    := ChkXRef.Checked;
  PrefAutoFixed  := ChkAutoFixed.Checked;
  PrefCleanList  := ChkCleanList.Checked;

  PrefAutoVerify := ChkAutoVerify.Checked;
  PrefConfDown   := ChkConfDown.Checked;
  PrefAutoFind   := ChkAutoFind.Checked;
  PrefMemType    := ChkSRAMDefault.Checked;

  PrefDisPos[ 1] := VVal(Edit12.Text);
  PrefDisPos[ 2] := VVal(Edit13.Text);
  PrefDisPos[ 3] := VVal(Edit14.Text);
  PrefDisPos[ 4] := VVal(Edit15.Text);
  PrefDisPos[ 5] := VVal(Edit16.Text);
  PrefDisPos[ 6] := VVal(Edit17.Text);
  PrefDisPos[ 7] := VVal(Edit18.Text);
  PrefDisPos[ 8] := VVal(Edit19.Text);
  PrefDisPos[ 9] := VVal(Edit20.Text);
  PrefDisPos[10] := VVal(Edit21.Text);
  PrefDisPos[11] := VVal(Edit22.Text);

  if PrefMemType then with FrmROMHandler do begin
    // Memory Type is SRAM
    MemTp := SRAM;
    PnlMemType.Caption := 'SRAM';
    Val(EdtROMNum.Text, I, Code);
    if I > 63 then EdtROMNum.Text := '63';
    UpDnNumber.Max := 63;
  end else with FrmROMHandler do begin
    // Memory Type is FLASH
    MemTp := FLASH;
    PnlMemType.Caption := 'FLASH';
    UpDnNumber.Max := 255;
  end;

  if (MPSSEValue >= 0) and (MPSSEValue < 65535) then
    PrefCommSpeed  := MPSSEValue
  else
    PrefCommSpeed  := MPSSE_Speed;

  // Now change the MPSSE Speed
  if PortIsOpen then begin
    CloseMLDLPort;
    if OpenMLDLPort then InitMLDLComm;
  end;

  if (JTAGValue >= 0) and (JTAGValue < 65535) then
    PrefJTAGSpeed  := JTAGValue
  else
    PrefJTAGSpeed  := JTAGValue;

  Val(EdtUSBTO.Text, PrefUSBTO, Code);
  Val(EdtFLASHTO.Text, PrefFLASHTO, Code);
  Val(EdtSECTO.Text, PrefSecTO, Code);
  Val(EdtALLTO.Text, PrefALLTO, Code);

  case CmbBlockSize.ItemIndex of
    0:   PrefBlockSize :=   16;
    1:   PrefBlockSize :=   32;
    2:   PrefBlockSize :=   64;
    3:   PrefBlockSize :=  128;
    4:   PrefBlockSize :=  256;
    5:   PrefBlockSize :=  512;
    6:   PrefBlockSize := 1024;
    else PrefBlockSize :=  256;
  end;

  if PrefUSBTO     = 0 then PrefUSBTO     := TimeOut_USB;
  if PrefFLASHTO   = 0 then PrefFLASHTO   := TimeOut_Flash_Byte;
  if PrefSecTO     = 0 then PrefSecTO     := TimeOut_FLASH_Sector;
  if PrefALLTO     = 0 then PrefALLTO     := TimeOut_FLASH_Erase;
  if PrefBlockSize = 0 then PrefBlockSize := 256;

  // SaveIni;
end;

procedure TFrmPreferences.BtnCancelClick(Sender: TObject);
// Close window, first read old values from INI file
begin
  ReadIni;
  FrmPreferences.Close;
end;

procedure TFrmPreferences.BtnOKClick(Sender: TObject);
begin
  FrmPreferences.BtnApplyClick(Sender);
  SaveIni;
  FrmPreferences.Close;
end;

procedure TFrmPreferences.EdtCommSpeedChange(Sender: TObject);
// Called when the Edit changes, show calculated bit rate
var
  bitrate: real;
  Code: integer;
  S, SS: string;
begin
  Val(EdtCommSpeed.Text, MPSSEValue, Code);
  if (Code = 0) and (MPSSEValue >= 0) and (MPSSEValue <= 65353) then begin
    // Value was OK
    bitrate := 12 / (2 * (1 + MPSSEValue));
    if bitrate > 1 then begin
      SS := 'mbit/s'
    end else begin
      bitrate := bitrate * 1000;
      SS := 'kbit/s';
    end;
    if bitrate < 1 then begin
      bitrate := bitrate * 1000;
      SS := 'bit/s';
    end;
    //    if bitrate >  then
    Str(bitrate: 5:2, S);
    LblMPSSE.Caption := S + ' ' + SS;
  end else begin
    // Code was not correct, remove offending character
    LblMPSSE.Caption := 'wrong value';
    MPSSEValue := -1;
  end;
end;

procedure TFrmPreferences.EdtJTAGSpeedChange(Sender: TObject);
var
  bitrate: real;
  Code: integer;
  S, SS: string;
begin
  Val(EdtJTAGSpeed.Text, JTAGValue, Code);
  if (Code = 0) and (JTAGValue >= 0) and (JTAGValue <= 65353) then begin
    // Value was OK
    bitrate := 12 / (2 * (1 + JTAGValue));
    if bitrate > 1 then begin
      SS := 'mbit/s'
    end else begin
      bitrate := bitrate * 1000;
      SS := 'kbit/s';
    end;
    if bitrate < 1 then begin
      bitrate := bitrate * 1000;
      SS := 'bit/s';
    end;
    //    if bitrate >  then
    Str(bitrate: 5:2, S);
    LblJTAG.Caption := S + ' ' + SS;
  end else begin
    // Code was not correct, remove offending character
    LblJTAG.Caption := 'wrong value';
    JTAGValue := -1;
  end;
end;

procedure TFrmPreferences.FormShow(Sender: TObject);
// Show current values in the Checkboxes
begin
  ReadIni;
  ChkComments.Checked   := PrefComments;
  ChkAutoLabel.Checked  := PrefAutoLabel;
  ChkMainFrame.Checked  := PrefMainFrame;
  ChkXROM.Checked       := PrefXROM;
  ChkSkipNOPs.Checked   := PrefSkipNOPs;
  ChkLabLst.Checked     := PrefLabLst;
  ChkMESLArg.Checked    := PrefMESLArg;
  GrpMnem.ItemIndex     := PrefMnem;
  ChkOneLine.Checked    := PrefOneLine;
  ChkSDK41Mode.Checked  := PrefSDK41Mode;
  ChkXRef.Checked       := PrefGenXRef;
  ChkAutoFixed.Checked  := PrefAutoFixed;
  ChkCleanList.Checked  := PrefCleanList;

  Edit12.Text := SStr(PrefDisPos[ 1]);
  Edit13.Text := SStr(PrefDisPos[ 2]);
  Edit14.Text := SStr(PrefDisPos[ 3]);
  Edit15.Text := SStr(PrefDisPos[ 4]);
  Edit16.Text := SStr(PrefDisPos[ 5]);
  Edit17.Text := SStr(PrefDisPos[ 6]);
  Edit18.Text := SStr(PrefDisPos[ 7]);
  Edit19.Text := SStr(PrefDisPos[ 8]);
  Edit20.Text := SStr(PrefDisPos[ 9]);
  Edit21.Text := SStr(PrefDisPos[10]);
  Edit22.Text := SStr(PrefDisPos[11]);

  EdtCommSpeed.Text     := SStr(PrefCommSpeed);
  EdtJTAGSpeed.Text     := SStr(PrefJTAGSPeed);

  ChkAutoVerify.Checked := PrefAutoVerify;
  ChkConfDown.Checked   := PrefConfDown;
  ChkAutoFind.Checked   := PrefAutoFind;

  case PrefBlockSize of
      16: CmbBlockSize.ItemIndex := 0;
      32: CmbBlockSize.ItemIndex := 1;
      64: CmbBlockSize.ItemIndex := 2;
     128: CmbBlockSize.ItemIndex := 3;
     256: CmbBlockSize.ItemIndex := 4;
     512: CmbBlockSize.ItemIndex := 5;
    1024: CmbBlockSize.ItemIndex := 6;
    else  CmbBlockSize.ItemIndex := 4;
  end;

  EdtUSBTO.Text   := SStr(PrefUSBTO);
  EdtFLASHTO.Text := SStr(PrefFLASHTO);
  EdtSECTO.Text   := SStr(PrefSecTO);
  EdtALLTO.Text   := SStr(PrefALLTO);
end;


procedure TFrmPreferences.FormCreate(Sender: TObject);
// Read Values from INI File into Global Variables
// Creates the file if it does not exist
var
  Ini: tMemIniFile;
const
  T = -1;  // default Top
  L = -1;  // default Left

begin
  Ini := tMemIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
  try
    FrmPreferences.Top  := Ini.ReadInteger( 'WindowsPositions', 'PrefTop', -1);
    FrmPreferences.Left := Ini.ReadInteger( 'WindowsPositions', 'PrefLeft', -1);

    if not WinVisible(FrmPreferences) then begin
      // window outside current screen? Set to defaults
      FrmPreferences.Top    := T;
      FrmPreferences.Left   := L;
    end;
  finally
    Ini.Free
  end;
end;


procedure TFrmPreferences.FormDestroy(Sender: TObject);
var
  Ini: tMemIniFile;
begin
//  Ini := tMemIniFile.Create(ChangeFileExt(Application.ExeName, '.INI' ));
//  try
    // Remember Windows positions
//    Ini.WriteInteger( 'WindowsPositions', 'PrefTop', FrmPreferences.Top);
//    Ini.WriteInteger( 'WindowsPositions', 'PrefLeft', FrmPreferences.Left);
//  finally
//    Ini.Free;
//  end;
end;

initialization
  {$i Preferences.lrs}

end.
