unit ListWindow;

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
//  LISTWINDOW.PAS                                                           //
//  Unit for displaying text output in a seperate window                     //
//  Ver  Date     Description                                                //
//  1.00 Apr 2007 First Version                                              //
//  1.01 Apr 2008 Added saving of window position                            //
//                Replaced RichEdit by Memo                                  //
//  1.20 Apr 2008 Added CTRL-A function                                      //
//  1.50 May 2008 Final release                                              //
//  1.51 Sep 2008 Changed saving of screen position                          //
//  1.70 Jul 2010 Adapted for Lazarus                                        //
//  1.80 Feb 2018 restarted development                                      //
//                removed Delphi compatibility                               //
//  1.90 Mar 2020 window never remains hidden on non-visible monitor         //
//                disabled worp wrap                                         //
//---------------------------------------------------------------------------//

interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, LResources, IniFiles, Procs;

type

  { TFrmLister }

  TFrmLister = class(TForm)
    EdtList: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure EdtListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmLister: TFrmLister;

implementation


procedure TFrmLister.FormCreate(Sender: TObject);
var
  Ini: TMemIniFile;
const
  T = -1;  // default Top
  L = -1;  // default Left
  W = 500; // default Width
  H = 600; // default Height

begin
  Ini := TMemIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
  try
    FrmLister.Top    := Ini.ReadInteger( 'WindowsPositions', 'ListerTop', T);
    FrmLister.Left   := Ini.ReadInteger( 'WindowsPositions', 'ListerLeft', L);
    FrmLister.Width  := Ini.ReadInteger( 'WindowsPositions', 'ListerWidth', W);
    FrmLister.Height := Ini.ReadInteger( 'WindowsPositions', 'ListerHeight', H);

    if not WinVisible(FrmLister) then begin
      // window outside current screen? Set to defaults
      FrmLister.Top    := T;
      FrmLister.Left   := L;
      FrmLister.Width  := W;
      FrmLister.Height := H;
    end;

  finally
    Ini.Free
  end;
end;

procedure TFrmLister.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.INI' ));
  try
    Ini.WriteInteger( 'WindowsPositions', 'ListerTop', FrmLister.Top);
    Ini.WriteInteger( 'WindowsPositions', 'ListerLeft', FrmLister.Left);
    Ini.WriteInteger( 'WindowsPositions', 'ListerWidth', FrmLister.Width);
    Ini.WriteInteger( 'WindowsPositions', 'ListerHeight', FrmLister.Height);
  finally
    Ini.Free;
  end;
end;


procedure TFrmLister.EdtListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((ssCtrl in Shift) AND (Key = ord('A'))) then begin
    // CONTROL-A pressed
    EdtList.SelectAll;
    Key := 0;
  end;
end;

initialization
  {$i ListWindow.lrs}

end.
