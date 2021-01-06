unit About;

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
//  ABOUT.PAS                                                                //
//  Shows the information screen                                             //
//  Ver  Date     Description                                                //
//  1.00 Nov 2005 First version for release                                  //
//  1.01 Nov 2006 Compiled for Turbo Explorer                                //
//  1.10 Apr 2007 Added Disassembler and MLDL Contents Lister                //
//  1.20 Oct 2007 Complete JTAG programming and various smaller changes      //
//  1.20 Mar 2008 Added picture and better version information               //
//                Photograph by Matthias Wehrli, www.hp-collection.org       //
//  1.50 May 2008 Final release                                              //
//  1.70 Jul 2010 Adapted for Lazarus                                        //
//  1.80 Feb 2018 restarted development                                      //
//                added version info from executable (in work)               //
//                removed Delphi compatibility                               //
//  1.90 Feb 2020 Fixed version info read from .exe                          //
//                Increased picture size                                     //
//                Fixed disassembly listing version                          //
//                Fixed text alignment                                       //
//---------------------------------------------------------------------------//

interface

uses LCLIntf, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, LResources, Globals,
  fileinfo,
  winpeimagereader;                 {need this for reading exe info}


type

  { TAboutBox }

  TAboutBox = class(TForm)
    Panel1: TPanel;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    Comments: TLabel;
    OKButton: TButton;
    Label1: TLabel;
    Image1: TImage;
    procedure FormShow(Sender: TObject);
    // procedure OKButtonClick(Sender: TObject);
    function GetVersion: string;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

{$R MLDL2K.res}

implementation


function TAboutBox.GetVersion: string;
var
  FileVerInfo: TFileVersionInfo;

begin
  Result := 'Unkown';
  FileVerInfo:=TFileVersionInfo.Create(nil);

  try
    FileVerInfo.ReadFileInfo;
    Result := FileVerInfo.VersionStrings.Values['FileVersion'];

  finally
    FileVerInfo.Free;
  end;

end;


procedure TAboutBox.FormShow(Sender: TObject);
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo:=TFileVersionInfo.Create(nil);

  try
    FileVerInfo.ReadFileInfo;
    Version.Caption := 'Version: ' + FileVerInfo.VersionStrings.Values['FileVersion'];
    Comments.Caption := FileVerInfo.VersionStrings.Values['Comments'];
    ProductName.Caption := FileVerInfo.VersionStrings.Values['FileDescription'];
    Copyright.Caption := FileVerInfo.VersionStrings.Values['LegalCopyright'];

  finally
    FileVerInfo.Free;
  end;

end;

initialization
  {$i About.lrs}

end.

