program MLDL2K;

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
//  MLDL2K.DPR                                                               //
//  Main program for M2kM                                                    //
//  Ver  Date     Description                                                //
//  1.00 Nov 2005 First version for release                                  //
//  1.50 May 2008 Final release                                              //
//  1.70 Jul 2010 Adapted for Lazarus                                        //
//  1.80 Feb 2018 restarted development                                      //
//                removed Delphi compatibility                               //
//                compiled with Lazarus 1.8.0, FPC 3.0.4                     //
//  1.90 Jan 2021 Final and last version                                     //
//  1.91 May 2022 Final version, issue fixed, see RomHandler comments	     //
//                                                                           //
//---------------------------------------------------------------------------//

uses
{$IFDEF win32}
  ShareMem,
{$ENDIF}
  Forms, sdflaz, Interfaces,
  BitBang_Unit in 'BitBang_Unit.pas' {BitBang},
  Globals in 'Globals.pas',
  // GlobalConstants in 'GlobalConstants.pas',
  About in 'About.pas' {AboutBox},
  MemEdit in 'MemEdit.pas' {FrmMemEdit},
  Procs_MLDL in 'procs_mldl.pas',
  D2XXUnit in 'D2XXUnit.pas',
  // PROCS in 'procs.pas',
  HP41_Globals in 'HP41_Globals.pas',
  Jtag in 'Jtag.pas' {Frm_JTAG},
  Tester in 'Tester.pas' {Frm_Tester},
  Jtag_constants in 'Jtag_constants.pas',
  Romdis in 'Romdis.pas',
  ListWindow in 'ListWindow.pas' {FrmLister},
  Preferences in 'Preferences.pas' {FrmPreferences},
  ROMHandler in 'ROMHandler.pas' {FrmROMHandler},
  IO_Handler in 'IO_Handler.pas' {Frm_IOHandler},
  DisAssembler in 'DisAssembler.pas';


// {$R *.res}

begin
  Application.Title:='MLDL2000 Manager';
  Application.Initialize;
  Application.CreateForm(TFrmROMHandler, FrmROMHandler);
  Application.CreateForm(TBitBang, BitBang);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TFrmMemEdit, FrmMemEdit);
  Application.CreateForm(TFrm_JTAG, Frm_JTAG);
  Application.CreateForm(TFrm_Tester, Frm_Tester);
  Application.CreateForm(TFrmLister, FrmLister);
  Application.CreateForm(TFrmPreferences, FrmPreferences);
  Application.CreateForm(TFrm_IOHandler, Frm_IOHandler);
  Application.Run;
end.
