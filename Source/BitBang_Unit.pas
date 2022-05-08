unit BitBang_Unit;

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
//  BITBANG.PAS                                                              //
//  Used for controlling individual signals on the FT2232C                   //
//  Ver  Date     Description                                                //
//  1.00 Nov 2005 First version for release                                  //
//  1.01 Nov 2006 Compiled for Turbo Explorer                                //
//                Removed the display functions, never used                  //
//  1.02 Oct 2007 Added warning, changes layout of buttons a bit             //
//  1.20 Apr 2008 Added code to save window positions                        //
//  1.50 May 2008 Final release                                              //
//  1.51 Sep 2008 Changed saving of screen position                          //
//  1.70 Jul 2010 Adapted for Lazarus                                        //
//  1.80 Feb 2018 restarted development                                      //
//                removed Delphi compatibility                               //
//  1.90 Mar 2020 window never remains hidden on non-visible monitor         //
//---------------------------------------------------------------------------//

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, LResources, Globals, GlobalConstants, Procs,
  Procs_MLDL, D2XXUnit, IniFiles;

type

  { TBitBang }

  TBitBang = class(TForm)
    StatusBar: TStatusBar;
    Pnl_MLDLStatus: TPanel;
    Label1: TLabel;
    BtnInitMLDL: TButton;
    Panel_OutputControl: TPanel;
    Label2: TLabel;
    BtnX_EN: TButton;
    BtnX_CLK: TButton;
    BtnX_DIN: TButton;
    BtnX_STROBE: TButton;
    Label3: TLabel;
    Label4: TLabel;
    PnlX_EN: TPanel;
    PnlX_CLK: TPanel;
    PnlX_DIN: TPanel;
    PnlX_STROBE: TPanel;
    PnlStatusMLDL: TPanel;
    Pnl_PortB: TPanel;
    Label11: TLabel;
    Label12: TLabel;
    PnlStatusHEXPortB: TPanel;
    PnlStatusBINPortB: TPanel;
    Label13: TLabel;
    BtnReadB: TButton;
    BtnWriteB: TButton;
    EdtOutput: TEdit;
    BtnContOut: TButton;
    BtnTestWrite: TButton;
    BtnContCount: TButton;
    TmrIO: TTimer;
    BtnContRead: TButton;
    PnlAccessCount: TPanel;
    BtnX_PE: TButton;
    PnlX_PE: TPanel;
    BtnJTAGEnabled: TButton;
    PnlX_BSY: TPanel;
    Label8: TLabel;
    PnlX_DAV: TPanel;
    Label6: TLabel;
    PnlX_DOUT: TPanel;
    Label9: TLabel;
    BtnRefresh: TButton;
    BtnContIn: TButton;
    PnlX_SPARE0: TPanel;
    PnlX_SPARE1: TPanel;
    PnlX_SPARE2: TPanel;
    PnlX_SPARE3: TPanel;
    Label5: TLabel;
    Label7: TLabel;
    Label10: TLabel;
    Btn_CPLD: TButton;
    Label14: TLabel;
    Label15: TLabel;
    procedure BtnInitMLDLClick(Sender: TObject);
    procedure BtnWriteBClick(Sender: TObject);
    procedure BtnRefreshClick(Sender: TObject);
    procedure BtnContInClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure TmrIOTimer(Sender: TObject);
    procedure BtnX_ENClick(Sender: TObject);
    procedure BtnX_CLKClick(Sender: TObject);
    procedure BtnX_DINClick(Sender: TObject);
    procedure BtnX_STROBEClick(Sender: TObject);
    procedure BtnContOutClick(Sender: TObject);
    procedure BtnContCountClick(Sender: TObject);
    procedure BtnTestWriteClick(Sender: TObject);
    procedure BtnContReadClick(Sender: TObject);
    procedure EdtOutputKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BtnReadBClick(Sender: TObject);
    procedure BtnX_PEClick(Sender: TObject);
    procedure BtnJTAGEnabledClick(Sender: TObject);
    procedure Btn_CPLDClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BitBang: TBitBang;


implementation


var
  CountVal : integer;
  OutputArrayAA55, OutputArrayCount : IO_Array;
  ContIn, ContToggle, ContAA55, ContCount, ContRead : boolean;


procedure UpdateStatus;
begin
  if MLDL_Open then begin             // is port to MLDL USB open
    BitBang.PnlStatusMLDL.Caption := 'ENABLED';
    BitBang.PnlStatusMLDL.Font.Color := clLime;
    BitBang.StatusBar.SimpleText := 'FT2232C: ' + FT_Device_String;
  end else begin
    BitBang.PnlStatusMLDL.Caption := 'DISABLED';
    BitBang.PnlStatusMLDL.Font.Color := clRed;
    BitBang.StatusBar.SimpleText := 'FT2232C: Not Open';
  end;
end;


procedure InitArray;
var
  i : integer;
begin
  for i := 0 to 255 do begin
    OutputArrayCount[i] := i;
    if Odd(i) then OutputArrayAA55[i] := $AA else OutputArrayAA55[i] := $55;
  end;
end;

procedure UpdateIOState(InValueLo, InValueHi: byte);
begin
  with BitBang do begin
    if (InValueLo and X_CLK) = 0 then PnlX_CLK.Caption :='0' else PnlX_CLK.Caption :='1';
    if (InValueLo and X_DOUT) = 0 then PnlX_DOUT.Caption := '0' else PnlX_DOUT.Caption :='1';
    if (InValueLo and X_DIN) = 0 then PnlX_DIN.Caption :='0' else PnlX_DIN.Caption :='1';
    if (InValueLo and X_STROBE) = 0 then PnlX_STROBE.Caption := '0' else PnlX_STROBE.Caption :='1';
    if (InValueLo and X_EN) = 0 then PnlX_EN.Caption :='0' else PnlX_EN.Caption :='1';
    if (InValueLo and X_PE) = 0 then PnlX_PE.Caption := '0' else PnlX_PE.Caption :='1';
    if (InValueLo and X_DAV) = 0 then PnlX_DAV.Caption := '0' else PnlX_DAV.Caption :='1';
    if (InValueLo and X_BSY) = 0 then PnlX_BSY.Caption := '0' else PnlX_BSY.Caption :='1';
    if (InValueHi and X_SPARE0) = 0 then PnlX_SPARE0.Caption := '0' else PnlX_SPARE0.Caption :='1';
    if (InValueHi and X_SPARE1) = 0 then PnlX_SPARE1.Caption := '0' else PnlX_SPARE1.Caption :='1';
    if (InValueHi and X_SPARE2) = 0 then PnlX_SPARE2.Caption := '0' else PnlX_SPARE2.Caption :='1';
    if (InValueHi and PWR_CPLD) = 0 then PnlX_SPARE3.Caption := '0' else PnlX_SPARE3.Caption :='1';
    Update;
  end;
end;

procedure TBitBang.BtnInitMLDLClick(Sender: TObject);
begin
  // Let's assume that our device is already open!
  // Initialze various variables
  TmrIO.Enabled := false;
  TmrIO.Interval := 100;
  CountVAl := 0;
  ContIn := false;
  ContToggle := false;
  ContAA55 := false;
  ContCount := false;
  ContRead := false;
  BtnContIn.Caption := 'CONT';
  BtnContOut.Caption := 'CONT Toggle';
  BtnContCount.Caption := 'CONT Count';
  BtnContRead.Caption := 'CONT Read';
  InitArray;
  InitMLDLComm;
  UpDateStatus;
  UpdateIOState(RPinsLo, RPinsHi);
end;


procedure TBitBang.BtnRefreshClick(Sender: TObject);
var
  InValueLo, InValueHi : byte;
begin
  InValueLo := RPinsLo;
  InValueHi := RPinsHi;
  UpdateIOState(InValueLo, InValueHi);
end;


procedure TBitBang.BtnContInClick(Sender: TObject);
begin
  if ContIn then begin
    ContIn := false;
    BtnContIn.Caption := 'CONT';
  end else begin
    ContIn := true;
    BtnContIn.Caption := 'OFF';
  end;
  TmrIO.Enabled := ContIn or ContToggle or ContAA55 or ContCount or ContRead;
end;



procedure TBitBang.BtnContOutClick(Sender: TObject);
// Continuos toggle of all outputs
begin
  if ContToggle then begin
    ContToggle := false;
    BtnContOut.Caption := 'CONT Toggle';
  end else begin
    ContToggle := true;
    BtnContOut.Caption := 'Toggle OFF';
  end;
  TmrIO.Enabled := ContIn or ContToggle or ContAA55 or ContCount or ContRead;
end;


procedure TBitBang.BtnContCountClick(Sender: TObject);
var
  i: integer;
begin
  if ContCount then begin
    ContCount := false;
    BtnContCount.Caption := 'CONT Count';
  end else begin
    ContCount := true;
    BtnContCount.Caption := 'Count OFF';
    OutIndex := 0;
    for i := 0 to 255 do AddToBuffer(i);
  end;
  TmrIO.Enabled := ContIn or ContToggle or ContAA55 or ContCount or ContRead;
end;


procedure TBitBang.BtnTestWriteClick(Sender: TObject);
// Test Memory Write sequence
var
  i : integer;
begin

  // Start with begin value X_CLK    = 0
  //                        X_DIN    = 0
  //                        X_STROBE = 1
  //                        X_EN     = 0
  for i := 0 to 1000 do begin
    OutIndex := 0;

    AddToBuffer(MPSSE_80);    // Set outputs
    AddToBuffer(MPSSE_Start+X_DIN+X_CLK);
    AddToBuffer(MPSSE_MaskLo);
    AddToBuffer(MPSSE_80);    // Set outputs
    AddToBuffer(MPSSE_Start);
    AddToBuffer(MPSSE_MaskLo);

    // Now clock out one byte for testing
    AddToBuffer(MPSSE_19);         //Write LSB First on -ve edge start clock at 0
    AddToBuffer($00);
    AddToBuffer($00);
    AddToBuffer($81);

    // Set outputs back to default
    AddToBuffer(MPSSE_80);    // Set outputs
    AddToBuffer(MPSSE_Start);
    AddToBuffer(MPSSE_MaskLo);
    SendBytes(OutIndex);
    sleep(4);
  end;
end;


procedure TBitBang.BtnContReadClick(Sender: TObject);
begin
  if ContRead then begin
    ContRead := false;
    BtnContRead.Caption := 'CONT Read';
  end else begin
    ContRead := true;
    BtnContRead.Caption := 'Read OFF';
  end;
  TmrIO.Enabled := ContIn or ContToggle or ContAA55 or ContCount or ContRead;
end;


procedure TBitBang.TmrIOTimer(Sender: TObject);
// Timer for continuous updating of I/O Status
// Works 10* per second
var
  InValueLo, InValueHi : byte;
  S : string;
  i: integer;
begin
  if ContIn then begin
    InValueLo := RPinsLo;
    InValueHi := RPinsHi;
    UpdateIOState(InValueLo, InValueHi);
  end;
  if ContToggle then begin
    TogglePinLo(X_EN + X_CLK + X_DIN + X_STROBE + X_PE);
    UpdateIOState(RPinsLo, RPinsHi);
  end;
  if ContAA55 then begin
    i := Write_USB_Device_Buffer(256);
  end;
  if ContCount then begin
    i := Write_USB_Device_Buffer(256);
  end;
  if ContRead then begin
    InValueLo := RPinsLo;
    PnlStatusHEXPortB.Caption := Hex2(InValueLo);
    PnlStatusBINPortB.Caption := Bin8(InValueLo);
    PnlStatusHEXPortB.Update;
    PnlStatusBINPortB.Update;
  end;
  CountVal := CountVal + 1;
  Str(CountVal, S);
  PnlAccessCount.Caption := S;
  PnlAccessCount.Update;
end;


procedure OutputByteB;
var
  S : string;
  OutVal : Byte;
  Result : boolean;
begin
  with BitBang do begin
    S := EdtOutput.Text;
    OutVal := HexToByte(S, Result);
    if Result then begin
      WPinsLo(OutVal);
      PnlStatusHEXPortB.Caption := Hex2(OutVal);
      PnlStatusBINPortB.Caption := Bin8(OutVal);
      PnlStatusHEXPortB.Update;
      PnlStatusBINPortB.Update;
    end;
  end;
end;


procedure TBitBang.EdtOutputKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
// processes input data in EdtOutput and sends it to the output
  if Key = VK_RETURN then begin
    OutputByteB;
    EdtOutput.SelectAll;
  end;
end;


procedure TBitBang.BtnWriteBClick(Sender: TObject);
begin
  OutputByteB;
end;


procedure TBitBang.BtnReadBClick(Sender: TObject);
var
  InValue : byte;
begin
  InValue := RPinsLo;
  PnlStatusHEXPortB.Caption := Hex2(InValue);
  PnlStatusBINPortB.Caption := Bin8(InValue);
  PnlStatusHEXPortB.Update;
  PnlStatusBINPortB.Update;
end;


procedure TBitBang.BtnX_ENClick(Sender: TObject);
// Toggle X_EN
begin
  TogglePinLo(X_EN);
  UpdateIOState(RPinsLo, RPinsHi);
end;


procedure TBitBang.BtnX_CLKClick(Sender: TObject);
// Toggle X_CLK
begin
  TogglePinLo(X_CLK);
  UpdateIOState(RPinsLo, RPinsHi);
end;


procedure TBitBang.BtnX_DINClick(Sender: TObject);
// Toggle X_DIN
begin
  TogglePinLo(X_DIN);
  UpdateIOState(RPinsLo, RPinsHi);
end;


procedure TBitBang.BtnX_STROBEClick(Sender: TObject);
// Toggle X_STROBE
begin
  TogglePinLo(X_STROBE);
  UpdateIOState(RPinsLo, RPinsHi);
end;


procedure TBitBang.BtnX_PEClick(Sender: TObject);
// Toggle X_PE
begin
  TogglePinLo(X_PE);
  UpdateIOState(RPinsLo, RPinsHi);
end;

procedure TBitBang.Btn_CPLDClick(Sender: TObject);
begin
  TogglePinHi(PWR_CPLD);
  UpdateIOState(RPinsLo, RPinsHi);
end;


procedure TBitBang.BtnJTAGEnabledClick(Sender: TObject);
// Set all pins to input except PE for correct programming of CPLD
// Drive PE high
begin
  InitJTAGMode(false);
end;


procedure TBitBang.FormCreate(Sender: TObject);
var
  Ini: TMemIniFile;
const
  T = -1;  // default Top
  L = -1;  // default Left
begin
  Ini := TMemIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
  try
    BitBang.Top:= Ini.ReadInteger( 'WindowsPositions', 'BitBangTop', T);
    BitBang.Left:= Ini.ReadInteger( 'WindowsPositions', 'BitBangLeft', L);

    if not WinVisible(BitBang) then begin
      // window outside current screen? Set to defaults
      BitBang.Top    := T;
      BitBang.Left   := L;
    end;

  finally
    Ini.Free
  end;
end;


procedure TBitBang.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.INI' ));
  try
    Ini.WriteInteger( 'WindowsPositions', 'BitBangTop', BitBang.Top);
    Ini.WriteInteger( 'WindowsPositions', 'BitBangLeft', BitBang.Left);
  finally
    Ini.Free;
  end;
end;



initialization
  {$i BitBang_Unit.lrs}

end.
