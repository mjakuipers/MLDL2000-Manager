unit IO_Handler;

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
//  Source for the I/O Handler                                               //
//  Ver  Date     Description                                                //
//  1.60 Sep 2008 First version                                              //
//  1.70 Jul 2010 Adapted for Lazarus                                        //
//  1.80 Feb 2018 restarted development                                      //
//                removed Delphi compatibility                               //
//  1.90 Mar 2020 window never remains hidden on non-visible monitor         //
//                                                                           //
//---------------------------------------------------------------------------//


interface

uses
  LCLIntf, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LResources, GlobalConstants, Procs_MLDL, Globals, Procs,
  IniFiles;

type
  TFrm_IOHandler = class(TForm)
    BtnWrite: TButton;
    BtnRead: TButton;
    BtnStop: TButton;
    BtnContRead: TButton;
    MemData: TMemo;
    XB: TLabel;
    Label1: TLabel;
    Lbl_XBSY: TLabel;
    Lbl_XDAV: TLabel;
    EdtHex: TEdit;
    LblDataRead: TLabel;
    MemAlpha: TMemo;
    Label2: TLabel;
    procedure BtnWriteClick(Sender: TObject);
    procedure BtnReadClick(Sender: TObject);
    procedure BtnContReadClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure OnCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Frm_IOHandler: TFrm_IOHandler;

  ContRead: boolean = false;

implementation



procedure TFrm_IOHandler.OnCreate(Sender: TObject);
var
  Ini: TIniFile;
const
  T = -1;  // default Top
  L = -1;  // default Left
  W = -1;
  H = -1;
begin
  Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
  try
    Frm_IOHandler.Top:= Ini.ReadInteger( 'WindowsPositions', 'IOHandlerTop', T);
    Frm_IOHandler.Left:= Ini.ReadInteger( 'WindowsPositions', 'IOHandlerLeft', L);
    Frm_IOHandler.Width:= Ini.ReadInteger( 'WindowsPositions', 'IOHandlerWidth', W);
    Frm_IOHandler.Height:= Ini.ReadInteger( 'WindowsPositions', 'IOHandlerHeight', H);

    if not WinVisible(Frm_IOHandler) then begin
      // window outside current screen? Set to defaults
      Frm_IOHandler.Top    := T;
      Frm_IOHandler.Left   := L;
    end;
  finally
    Ini.Free
  end;
end;


procedure TFrm_IOHandler.FormDestroy(Sender: TObject);
var
  Ini: TIniFile;
begin
  ContRead := false;
  Ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.INI' ));
  try
    Ini.WriteInteger( 'WindowsPositions', 'IOHandlerTop', Frm_IOHandler.Top);
    Ini.WriteInteger( 'WindowsPositions', 'IOHandlerLeft', Frm_IOHandler.Left);
    Ini.WriteInteger( 'WindowsPositions', 'IOHandlerWidth', Frm_IOHandler.Width);
    Ini.WriteInteger( 'WindowsPositions', 'IOHandlerHeight', Frm_IOHandler.Height);
  finally
    Ini.Free;
  end;
end;


procedure Update_Status;
begin
  with Frm_IOHandler do begin
    if MLDL_XDAV then Lbl_XDAV.Caption := 'Hi' else Lbl_XDAV.Caption := 'Lo';
    if MLDL_XBSY then Lbl_XBSY.Caption := 'Hi' else Lbl_XBSY.Caption := 'Lo';
  end;
end;


procedure TFrm_IOHandler.BtnContReadClick(Sender: TObject);
// Continuous read from HP41 IO Port
var
  ReadWord: Word;
  S, Sa: string;
begin
  ContRead := true;
  MemData.Clear;
  MemAlpha.Clear;
  Update_Status;
  Sa := '';
  while ContRead do begin
    while (not MLDL_XDAV) and ContRead do begin
      sleep(1);         // wait until data is available
//      Update_Status;
      Application.ProcessMessages;
    end;
    MLDL_ReadIO(ReadWord);
    if ((ReadWord and $1000) = $1000) then begin
      // from ALPHA register
      ReadWord := ReadWord and $00FF;
      if ((ReadWord = $000D) or (ReadWord = $0000)) then begin         // carriage return
        MemAlpha.Lines.Add(Sa);
        Sa := '';
      end else if ReadWord <> $0000 then begin
        Sa := Sa + char(ReadWord);
      end;
    end else begin
      S := Hex4(ReadWord);
      MemData.Lines.Add(S);
    end;
  end;
end;


procedure TFrm_IOHandler.BtnStopClick(Sender: TObject);
begin
  ContRead := false;
end;


procedure TFrm_IOHandler.BtnReadClick(Sender: TObject);
// Read one byte from the HP41
var
  ReadWord: word;
begin
  LblDataRead.Caption := 'WAIT';
  while (not MLDL_XDAV) and ContRead do begin
    sleep(1);         // wait until data is available
    Update_Status;
    Application.ProcessMessages;
  end;
  MLDL_ReadIO(ReadWord);
  LblDataRead.Caption := Hex4(ReadWord);
  Update_Status;
end;


procedure TFrm_IOHandler.BtnWriteClick(Sender: TObject);
// Write bytes from EdtHex to the HP41, one byte at a time until field is empty
// EdtHex is updated with bytes written
var
  S: string;
  SendByte: Byte;
  Rslt: boolean;
begin
  S := EdtHex.Text;
  ContRead := true;
  while S <> '' do begin
    SendByte := HexToByte(S, Rslt);
    while MLDL_XBSY and ContRead do begin
      sleep(1);         // wait until X_BSY is low
      Update_Status;
      Application.ProcessMessages;
    end;
    MLDL_WritIO(SendByte);               // write data
    EdtHex.Text := S;
  end;
  Update_Status;
end;

initialization
  {$i IO_Handler.lrs}

end.
