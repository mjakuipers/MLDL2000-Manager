unit MemEdit;

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
//  MEMEDIT.PAS                                                              //
//  SOurce for the Memory Editor                                             //
//  Ver  Date     Description                                                //
//  1.00 Nov 2005 First version for release                                  //
//  1.01 Nov 2006 Compiled for Turbo Explorer                                //
//                GaugeProgress commented out, will not replace              //
//  1.02 Oct 2007 Removed Slow Read and option to disable autoread           //
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
  Grids, StdCtrls, Buttons, ExtCtrls, LResources, Globals, GlobalConstants,
  Procs, Procs_MLDL, IniFiles;

type
  TFrmMemEdit = class(TForm)
    BtnUp: TBitBtn;
    BtnDn: TBitBtn;
    EdtAddr: TEdit;
    GrdEdit: TStringGrid;
    BtnUp16: TBitBtn;
    BtnDn16: TBitBtn;
    BtnRead: TBitBtn;
    Grp_FLSR: TRadioGroup;
    Label1: TLabel;
    BtnWrite: TBitBtn;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    StaticText6: TStaticText;
    StaticText7: TStaticText;
    StaticText8: TStaticText;
    StaticText9: TStaticText;
    StaticText10: TStaticText;
    StaticText11: TStaticText;
    StaticText12: TStaticText;
    StaticText13: TStaticText;
    StaticText14: TStaticText;
    StaticText15: TStaticText;
    StaticText16: TStaticText;
    GrdAddr: TStringGrid;
    BtnUp4K: TBitBtn;
    BtnDn4K: TBitBtn;
    procedure BtnReadClick(Sender: TObject);
    procedure BtnUp16Click(Sender: TObject);
    procedure BtnDn16Click(Sender: TObject);
    procedure BtnUp4KClick(Sender: TObject);
    procedure BtnDn4KClick(Sender: TObject);
    procedure BtnUpClick(Sender: TObject);
    procedure BtnDnClick(Sender: TObject);
    procedure BtnWriteClick(Sender: TObject);
    procedure DoRead(Sender: TObject);
    procedure OnCreate(Sender: TObject);
    procedure EdtAddrKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
//    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);

//    procedure EdtAddrChange(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmMemEdit: TFrmMemEdit;


implementation

var
  StartAddr : LongWord;


procedure TFrmMemEdit.OnCreate(Sender: TObject);
var
  i: integer;
  Ini: TIniFile;
const
  T = -1;  // default Top
  L = -1;  // default Left
begin
  for i := 0 to 15 do
    GrdAddr.Cells[0, i] := Hex5(16*i);
  Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
  try
    FrmMemEdit.Top:= Ini.ReadInteger( 'WindowsPositions', 'MemEditTop', T);
    FrmMemEdit.Left:= Ini.ReadInteger( 'WindowsPositions', 'MemEditLeft', L);

    if not WinVisible(FrmMemEdit) then begin
      // window outside current screen? Set to defaults
      FrmMemEdit.Top  := T;
      FrmMemEdit.Left := L;
    end;

  finally
    Ini.Free
  end;
end;


procedure TFrmMemEdit.DoRead(Sender: TObject);
var
  MemoryType : MemType;
  S          : string;
  i, j       : integer;
begin
  if PortIsOpen then begin
    // first validate address
    S := FrmMemEdit.EdtAddr.Text;
    StartAddr := HexToInt(S);
    StartAddr := StartAddr and $FFFF0;
    FrmMemEdit.EdtAddr.Text := Hex5(StartAddr);
    if FrmMemEdit.Grp_FLSR.ItemIndex = 0 then MemoryType := SRAM else MemoryType := FLASH;
    DisableMLDL;

    MLDL_ReadBlock(MemoryType, StartAddr, 256);

    for i := 0 to 15 do begin
      FrmMemEdit.GrdAddr.Cells[0, i] := Hex5(StartAddr + 16*i);
      for j := 0 to 15 do
        FrmMemEdit.GrdEdit.Cells[j, i] := Hex4(WordArray[16*i + j]);
    end;

    EnableMLDL;
  end else begin
    // MLDL not connected anymore?
    // MessageBox(0, 'MLDL NOT CONNECTED?', nil, MB_ICONEXCLAMATION + MB_APPLMODAL);
    MessageBox(0, 'MLDL NOT CONNECTED?', nil, MB_ICONEXCLAMATION);
  end;

end;

procedure TFrmMemEdit.BtnReadClick(Sender: TObject);
// Read from MLDL2000 memory
begin
  DoRead(Sender);
end;


procedure TFrmMemEdit.BtnUp16Click(Sender: TObject);
begin
  if StartAddr < Range_FLASH - 255 - 256 then
    StartAddr := StartAddr + 256
  else
    StartAddr := $FFF00;
  StartAddr := StartAddr and $FFFF0;
  EdtAddr.Text := Hex5(StartAddr);
  DoRead(Sender);
end;


procedure TFrmMemEdit.BtnDn16Click(Sender: TObject);
begin
  if StartAddr < 255 then
    StartAddr := 0
  else
    StartAddr := StartAddr - 256;
  StartAddr := StartAddr and $FFFF0;
  EdtAddr.Text := Hex5(StartAddr);
  DoRead(Sender);
end;

procedure TFrmMemEdit.BtnUp4KClick(Sender: TObject);
begin
  if StartAddr < Range_FLASH - Range_4K - Range_4K + 1 then
    StartAddr := StartAddr + Range_4K + 1
  else
    StartAddr := $FF000;
  StartAddr := StartAddr and $FF000;
  EdtAddr.Text := Hex5(StartAddr);
  DoRead(Sender);
end;


procedure TFrmMemEdit.BtnDn4KClick(Sender: TObject);
begin
  if StartAddr < Range_4K then
    StartAddr := 0
  else
    StartAddr := StartAddr - Range_4K + 1;
  StartAddr := StartAddr and $FF000;
  EdtAddr.Text := Hex5(StartAddr);
  DoRead(Sender);
end;



procedure TFrmMemEdit.BtnUpClick(Sender: TObject);
begin
  if StartAddr < Range_FLASH - 255 then
    StartAddr := StartAddr + 16
  else
    StartAddr := $FFF00;
  StartAddr := StartAddr and $FFFF0;
  EdtAddr.Text := Hex5(StartAddr);
  DoRead(Sender);
end;


procedure TFrmMemEdit.BtnDnClick(Sender: TObject);
begin
  if StartAddr > 255 + 16 then
    StartAddr := StartAddr - 16
  else
    StartAddr := 0;
  StartAddr := StartAddr and $FFFF0;
  EdtAddr.Text := Hex5(StartAddr);
  DoRead(Sender);
end;


procedure TFrmMemEdit.BtnWriteClick(Sender: TObject);
var
  S    : String;
//  Data : Word;
  i, j : integer;
  MemoryType : MemType;
begin
  if PortIsOpen then begin
    // Read from Grid
    for i := 0 to 15 do begin
      for j := 0 to 15 do begin
        S := GrdEdit.Cells[j, i];
        WordArray[16*i + j] := HexToInt(S);
      end;
    end;
    // Now write data
    S := EdtAddr.Text;
    StartAddr := HexToInt(S);
    StartAddr := StartAddr and $FFFF0;
    EdtAddr.Text := Hex5(StartAddr);

    if Grp_FLSR.ItemIndex = 0 then MemoryType := SRAM else MemoryType := FLASH;

    DisableMLDL;

    MLDL_WriteBlock(MemoryType, StartAddr, 256);

    // Read back written data
    if MLDL_Error = 0 then
      DoRead(Sender);

    EnableMLDL;

    if MLDL_Error <> 0 then
      // MessageBox(0, 'TIMEOUT', nil, MB_ICONEXCLAMATION + MB_APPLMODAL);
      MessageBox(0, 'TIMEOUT', nil, MB_ICONEXCLAMATION);
  end else begin
    // MLDL not connected anymore?
    // MessageBox(0, 'MLDL NOT CONNECTED?', nil, MB_ICONEXCLAMATION + MB_APPLMODAL);
    MessageBox(0, 'MLDL NOT CONNECTED?', nil, MB_ICONEXCLAMATION);
  end;
end;


procedure TFrmMemEdit.EdtAddrKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then DoRead(Sender);
end;


procedure TFrmMemEdit.FormDestroy(Sender: TObject);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.INI' ));
  try
    Ini.WriteInteger( 'WindowsPositions', 'MemEditTop', FrmMemEdit.Top);
    Ini.WriteInteger( 'WindowsPositions', 'MemEditLeft', FrmMemEdit.Left);
  finally
    Ini.Free;
  end;
end;


initialization
  {$i MemEdit.lrs}

end.
