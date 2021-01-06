unit ROMHandler;

//---------------------------------------------------------------------------//
//    Copyright (c) 2009  Meindert Kuipers, Netherlands                      //
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
//  ROMHANDLER.PAS                                                           //
//  Unit for the M2kM Rom Handler                                            //
//  MOD file routines based on V41 by Warren Furlow                          //
//  HEX file conversion by Diego Diaz
//  Ver  Date     Description                                                //
//  1.00 Nov 2005 First version for release                                  //
//  1.01 Nov 2006 Compiled for Turbo Explorer                                //
//                GaugeProgress Commented out, replaced by TProgressBar      //
//                Added ID read of FLASH, FLASH ERASE bug fixed              //
//  1.10 Apr 2007 Integrated Disassembler                                    //
//                Added ROM Name in ROM Info Panel, also User Code names     //
//                Now Reads MLDL contents                                    //
//  1.20 Apr 2008 Many functional changes: MOD file, save environment etc    //
//       May 2008 Added support for .HEX files (from Clonix)                 //
//                Backup and Restore functions completed                     //
//                AutoLoad completed                                         //
//  1.50 May 2008 Final release                                              //
//  1.51 Jul 2008 Changed ReadSRAMTyp to prevent overwrite of SRAM $00000    //
//       Oct 2008 Added I/O FLASH/SRAM, added this info to TreeView          //
//  1.51 Jul 2009 Modified TreeView to show EMPTY page                       //
//                Added seperate SR pulldown                                 //
//  1.60 Jan 2010 Modified disassembler, moved to seperate Unit              //
//  1.70 Jul 2010 Changed to Lazarus from Delphi                             //
//       Oct 2010 Updated to 12-bit SR's                                     //
//                Added functions for initializing SRAM                      //
//  1.80 Feb 2018 restarted development                                      //
//                removed Delphi compatibility                               //
//  1.90 Feb 2020 disabled all JTAG functions, must review code              //
//                fixed code, enabled again                                  //
//                fixed initialization of S in DisplayROM                    //
//       Mar 2020 upper node correctly displays bank 0 ROM name in SR view   //
//                fixed "/" at end of page listing                           //
//                fixed Range Check Error on 8K ROM Header listing           //
//                added 41CL YCRC in ROM header listing, 41CL prep           //
//                more compact ListMLDL, added Checksum + CRC                //
//                improved ListMLDL ROM name                                 //
//                added HexDump function with extra button                   //
//                window never remains hidden on non-visible monitor         //
//       Oct 2020 Added import of DM41X .RAM files                           //
//                Added import of i41CX .RAM files (renamed to .RMA!)        //
//                                                                           //
//---------------------------------------------------------------------------//

interface

uses
  LCLIntf, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  IniFiles, Dialogs, StdCtrls, ComCtrls, ExtCtrls, Grids, Globals,
  GlobalConstants, Procs, Procs_MLDL, Tester, HP41_Globals, MemEdit, StrUtils,
  Menus, Romdis, ListWindow, Jtag, BitBang_Unit, About, D2XXUnit, Buttons,
  LResources, Preferences, IO_Handler, DisAssembler, Windows;

type

  { TFrmROMHandler }

  TFrmROMHandler = class(TForm)
    BtnEraseAllSRAM: TButton;
    BtnInitSRAMSR: TButton;
    BtnHexDump: TButton;
    ChkFLDB: TCheckBox;
    ChkIMDB: TCheckBox;
    CheckLongInfo: TCheckBox;

    // Main Menu, to be renamed

    MainMenu: TMainMenu;

    MnMainF: TMenuItem;             // File Menu
    MnMainFOpen: TMenuItem;
    MnMainFSaveAs: TMenuItem;
    MnMainFNewMod: TMenuItem;
    MnDivider: TMenuItem;
    MnMainFExit: TMenuItem;

    MnMainM: TMenuItem;             // MLDL2000 Menu
    MnMainMDownload: TMenuItem;
    MnMainMDisconnect: TMenuItem;
    MnMainMUpload: TMenuItem;
    MnMainMConnect: TMenuItem;
    MnMainMVerify: TMenuItem;

    MnMainT: TMenuItem;             // Tools Menu
    MnMainTMemEdit: TMenuItem;
    MnMainTIO: TMenuItem;
    MnMainTBitbang: TMenuItem;
    MnMainTTest: TMenuItem;
    MnMainTCPLD: TMenuItem;

    MnMainPref: TMenuItem;          // Preferences Menu

    MnMainAbout: TMenuItem;         // About Menu
    // Connect: TMenuItem;         replaced with MnMainMConnect
    MainOpen: TMenuItem;
    MainSave: TMenuItem;
    MainVerify: TMenuItem;
    NewMODFile: TMenuItem;
    IOHandler1: TMenuItem;

    PopListMLDL: TPopupMenu;

    PopDownload: TMenuItem;
    PopSaveAs: TMenuItem;

    PopEraseFlash: TPopupMenu;

    PopEraseSector: TMenuItem;
    PopEmptyCheck: TMenuItem;

    N1: TMenuItem;

    N2: TMenuItem;

    PopSR: TPopupMenu;
    PopCollapseAll: TMenuItem;
    PopExpandAll: TMenuItem;
    PopCollapse07: TMenuItem;
    PopCollapse8F: TMenuItem;
    PopExpand07: TMenuItem;
    PopExpand8F: TMenuItem;
    PopRefresh: TMenuItem;
    PopMOD: TPopupMenu;
    PopSRAMWipe: TMenuItem;
    ExtracttoROMHandler: TMenuItem;

    DeleteSelectedPages: TMenuItem;
    InsertBeforeSelected: TMenuItem;
    ROM2MODAppnd: TMenuItem;
    ROM2MODOvr: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    RereadMODfile: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    Extract2ROMfile: TMenuItem;
    MODSelectALL: TMenuItem;
    MODUnselectALL: TMenuItem;

    N8: TMenuItem;

    ExtracttoSRHandler: TMenuItem;
    Extract_SR0: TMenuItem;
    Extract_SR1: TMenuItem;
    Extract_SR2: TMenuItem;
    Extract_SR3: TMenuItem;
    Extract4K: TMenuItem;
    Extract8K: TMenuItem;
    ExtractSRFile: TMenuItem;
    PopAppendtoMODfile: TMenuItem;
    N7: TMenuItem;
    MODUpload: TMenuItem;

    PopMemo: TPopupMenu;
    PopClearText: TMenuItem;

    // end of menu items

    LblCustomH: TLabel;
    BtnROMHeader: TButton;
    PnlTop: TPanel;
    BtnOpen: TButton;
    BtnSave: TButton;
    BtnUpload: TButton;
    BtnDownload: TButton;
    RadioFLASH: TRadioButton;
    RadioSRAM: TRadioButton;
    PnlBottom: TPanel;
    PnlFilename: TPanel;
    PnlMemType: TPanel;
    PnlStatus: TPanel;
    UpDnNumber: TUpDown;
    EdtROMNum: TEdit;
    StaticText1: TStaticText;
    DlgOpen: TOpenDialog;
    DlgSave: TSaveDialog;
    PageControl: TPageControl;
    ROMSheet: TTabSheet;
    SRSheet: TTabSheet;
    MemoROM: TMemo;
    EdtROMAddr: TEdit;
    EdtROMNumber: TEdit;
    UpDnROMNum: TUpDown;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    BtnNew: TButton;
    LblHex: TLabel;
    ua: TGroupBox;
    ChkEnable: TCheckBox;
    ChkFlash: TCheckBox;
    ChkIO: TCheckBox;
    ChkWProtect: TCheckBox;
    LblBin: TLabel;
    StaticText4: TStaticText;
    EdtROMAddress: TEdit;
    EdtComment: TEdit;
    StaticText5: TStaticText;
    PnlRight: TPanel;
    FLASHSheet: TTabSheet;
    LstFLASHErase: TListBox;
    BtnEraseSector: TButton;
    BtnEraseAll: TButton;
    BtnResetFLASH: TButton;
    BtnVerify: TButton;
    BtnCheckEmptySector: TButton;
    BtnCheckEmptyAll: TButton;
    BtnExpand: TButton;
    BtnDefault: TButton;
    GaugeProgress: TProgressBar;
    LblType: TLabel;
    BtnReadType: TButton;
    BtnDisAsm: TButton;
    LstContents: TListView;
    TreeViewSR: TTreeView;
    Panel1: TPanel;
    Splitter: TSplitter;
    BtnContents: TButton;
    BtnBackup: TButton;
    BtnRestore: TButton;
    Label2: TLabel;
    Label3: TLabel;
    LblSRAMType: TLabel;
    ChkHasFAT: TCheckBox;
    CmbBasePg: TComboBox;
    Label4: TLabel;
    StatusBar: TStatusBar;
    BtnMove8K: TButton;
    ChkDis8K: TCheckBox;
    BtnClearMemo: TButton;
    GrpDisassembler: TGroupBox;
    GrpBackup: TGroupBox;
    PnlUtil: TPanel;
    Panel2: TPanel;
    MODSheet: TTabSheet;
    LstMOD: TListView;
    Panel4: TPanel;
    GrpMODFile: TGroupBox;
    GroupBox2: TGroupBox;
    Label5: TLabel;
    LblMODFormat: TLabel;
    Label6: TLabel;
    EdtTitle: TEdit;
    Label7: TLabel;
    EdtVersion: TEdit;
    Label8: TLabel;
    EdtPartNum: TEdit;
    Label9: TLabel;
    EdtAuthor: TEdit;
    Label10: TLabel;
    EdtCopyright: TEdit;
    Label11: TLabel;
    EdtLicense: TEdit;
    Label12: TLabel;
    EdtComments: TEdit;
    Label13: TLabel;
    CmbCategory: TComboBox;
    Label14: TLabel;
    CmbHardware: TComboBox;
    ChkUpdate: TCheckBox;
    Label16: TLabel;
    CmbMEM: TComboBox;
    Label17: TLabel;
    CmbXMEM: TComboBox;
    ChkOriginal: TCheckBox;
    Label18: TLabel;
    LblNumPages: TLabel;
    Label19: TLabel;
    EdtCustomHdr: TEdit;
    Label27: TLabel;
    EdtROMName: TEdit;
    Label24: TLabel;
    EdtROMRev: TEdit;
    Label20: TLabel;
    CmbROMPage: TComboBox;
    Label23: TLabel;
    CmbROMPgGroup: TComboBox;
    ChkROMRAM: TCheckBox;
    Label22: TLabel;
    CmbROMBk: TComboBox;
    CmbROMBkGroup: TComboBox;
    ChkROMWP: TCheckBox;
    Label21: TLabel;
    Label28: TLabel;
    EdtROMCustomHdr: TEdit;
    ChkROMFAT: TCheckBox;
    Panel3: TPanel;
    Label31: TLabel;
    LblXROM: TLabel;
    LBLFCNS: TLabel;
    LBLREV: TLabel;
    LBLNAME: TLabel;
    LBLCHK: TLabel;
    LbLCALCCHK: TLabel;
    Label15: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Button1: TButton;
    BtnSaveEdit: TButton;
    ChkAuto: TCheckBox;
    Image1: TImage;
    BtnApplyEdit: TButton;
    BtnCancelEdit: TButton;
    BtnReRead: TButton;
    BtnNewMOD: TButton;
    ChkBakValid: TCheckBox;
    BtnROM2MOD: TButton;
    BtnDelMOD: TButton;
    BtnInsMOD: TButton;
    ChkOvrMOD: TCheckBox;
    Button2: TButton;
    LblCustom: TLabel;
    BtnCloseMOD: TButton;
    BtnUploadMOD: TButton;
    RadMODUpload: TRadioGroup;
    ChkIncSramROM: TCheckBox;
    ChkIncFlashROM: TCheckBox;
    ChkIncFlashSR: TCheckBox;
    ChkIncSramSR: TCheckBox;
    ChkBakVerify: TCheckBox;
    BtnBakVerify: TButton;
    ComSRPick: TComboBox;
    StatSR: TStaticText;

    procedure BtnEraseAllSRAMClick(Sender: TObject);
    procedure ChkDis8KChange(Sender: TObject);
    procedure ChkFLDBChange(Sender: TObject);
    procedure ChkHasFATChange(Sender: TObject);
    procedure ChkIMDBChange(Sender: TObject);
    procedure InitForm(Sender: TObject);
    procedure BtnOpenClick(Sender: TObject);
    procedure BtnInitSRAMSRClick(Sender: TObject);
    procedure MnMainMConnectClick(Sender: TObject);
    procedure MnMainMDisconnectClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure TreeViewClick(Sender: TObject);
    procedure TreeViewSRChange(Sender: TObject; Node: TTreeNode);
    procedure RadioClick(Sender: TObject);
    procedure BtnNewClick(Sender: TObject);
    procedure EdtROMNumChange(Sender: TObject);
    procedure RadioSRAMClick(Sender: TObject);
    procedure RadioFLASHClick(Sender: TObject);
    procedure RadioMemClick(Sender: TObject);
    procedure EdtROMNumberChange(Sender: TObject);
    procedure EdtCommentChange(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnUploadClick(Sender: TObject);
    procedure BtnDownloadClick(Sender: TObject);
    procedure BtnEraseSectorClick(Sender: TObject);
    procedure BtnEraseAllClick(Sender: TObject);
    procedure BtnResetFLASHClick(Sender: TObject);
    procedure BtnReadTypeClick(Sender: TObject);
    procedure BtnVerifyClick(Sender: TObject);
    procedure BtnExpandClick(Sender: TObject);
    procedure BtnDisable(Sender: TObject);
    procedure BtnDisAsmClick(Sender: TObject);
    procedure BtnContentsClick(Sender: TObject);
    procedure LstContentsColumnClick(Sender: TObject; Column: TListColumn);
    procedure ContentsClick(Sender: TObject);
    procedure BtnBackupClick(Sender: TObject);
    // procedure ConnectClick(Sender: TObject);
    procedure MLDLTestClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    procedure MemoryEditorClick(Sender: TObject);
    procedure CPLDUpgradeClick(Sender: TObject);
    procedure BitBangClick(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure PreferencesClick(Sender: TObject);
    procedure BtnHexDumpClick(Sender: TObject);
    procedure BtnClearMemoClick(Sender: TObject);
    procedure CmbCategoryEnter(Sender: TObject);
    procedure CmbCategoryExit(Sender: TObject);
    procedure EdtMouseEnter(Sender: TObject);
    procedure EdtAuthorMouseLeave(Sender: TObject);
    procedure FormClose(Sender: TObject; var Act: TCloseAction);
    procedure BtnCheckEmptySectorClick(Sender: TObject);
    procedure BtnCheckEmptyAllClick(Sender: TObject);
    procedure EmptyCheckPage(Tp: MemType; RomNum: integer;
                             UseBar: boolean; var Result: integer; var ErrAdr: longword);
    procedure EmptyCheckSR(Tp: MemType; RomNum: integer;
                           UseBar: boolean; var Result: integer; var ErrAdr: longword);
    procedure ReadFlashType;
    procedure ReadSRAMType;
    procedure CmbCategoryChange(Sender: TObject);
    procedure PopCollapseAllClick(Sender: TObject);
    procedure PopExpandAllClick(Sender: TObject);
    procedure PopCollapse07Click(Sender: TObject);
    procedure PopCollapse8FClick(Sender: TObject);
    procedure PopExpand07Click(Sender: TObject);
    procedure PopExpand8FClick(Sender: TObject);
    procedure PopDownloadClick(Sender: TObject);
    procedure PopSaveAsClick(Sender: TObject);
    procedure PopRefreshClick(Sender: TObject);
    procedure LstMODColumnClick(Sender: TObject; Column: TListColumn);
    procedure LstMODDblClick(Sender: TObject);
    procedure BtnListFAT(Sender: TObject);
    procedure BtnSaveEditClick(Sender: TObject);
    procedure ChkAutoClick(Sender: TObject);
    procedure PopSRAMWipeClick(Sender: TObject);
    procedure BtnROMHeaderClick(Sender: TObject);
    procedure BtnMove8KClick(Sender: TObject);
    procedure ExtracttoROMHandlerClick(Sender: TObject);
    procedure ExtracttoROMHandler8KClick(Sender: TObject);
    procedure FindFreeROM(MemoryType: MemType);
    procedure BtnApplyEditClick(Sender: TObject);
    procedure BtnCancelEditClick(Sender: TObject);
    procedure MemoROMKeyDown(Sender: TObject; var Key: Word;
                             Shift: TShiftState);
    procedure BtnReReadClick(Sender: TObject);
    procedure BtnNewMODClick(Sender: TObject);
    procedure BtnROM2MODClick(Sender: TObject);
    procedure BtnDelMODClick(Sender: TObject);
    procedure BtnInsMODClick(Sender: TObject);
    procedure ROM2MODAppndClick(Sender: TObject);
    procedure ROM2MODOvrClick(Sender: TObject);
    procedure Extract2ROMfileClick(Sender: TObject);
    procedure MODSelectALLClick(Sender: TObject);
    procedure MODUnselectALLClick(Sender: TObject);
    procedure BtnCloseMODClick(Sender: TObject);
    procedure Extract_SR0Click(Sender: TObject);
    procedure Extract_SR1Click(Sender: TObject);
    procedure Extract_SR2Click(Sender: TObject);
    procedure Extract_SR3Click(Sender: TObject);
    procedure ExtractSR(Sender: TObject; SRSet: integer);
    procedure UpdateTreeView(Sender: TObject);
    procedure ExtractSRFileClick(Sender: TObject);
    procedure PopAppendtoMODfileClick(Sender: TObject);
    procedure BtnRestoreClick(Sender: TObject);
    procedure LstMODSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure BtnBakVerifyClick(Sender: TObject);
    function OpenMODFile(FileNm: String; ChangeTab: boolean): integer;
    procedure IOHandler1Click(Sender: TObject);
    // procedure UARTHandler1Click(Sender: TObject);
    procedure ComSRPickChange(Sender: TObject);
    procedure ReadIni;
    procedure SaveIni;

  private
   { Private declarations }
  public
   { Public declarations }
  end;

var
  FrmROMHandler: TFrmROMHandler;
  Mode: (SR, ROM, FL, MODF);
  MemTp : MemType;
  OpenROMFileNm, OpenMODFileNm, OpenSRFileNm: String;
  ROMNum, SRNum, ROMNumber, SRNumber : integer;
  ROMFile: file of byte;
  MODFile: bfile;
  FileID: string;
  I: word;
  ch: Char;
  S: string;
  Analysed: Boolean;
  XROM, NumberOfFunctions, EndOfFAT,
  NumberOfFunctions8K: word;
  CodeOnly: Boolean;
  FileName: string;
  // State_SR_EN,            // true = 0 = ENABLED, false = 1 = DISABLED
  // State_SR_FL,            // true = 0 = FLASH,   false = 1 = SRAM
  // State_SR_SR,            // true = 0 = no I/O,  false = 1 = I/O
  // State_SR_WE: Boolean;   // true = 0 = ENABLED, false = 1 = PROTECTED
  // State_AddrFL, State_AddrSR: LongWord;

  Node: TTreeNode;

  SortColumn: integer;

  BufArray : array[0..255] of byte;

  InUpdateTreeView: boolean = false;

type
  MemPair = record
              Address : Longword;
              Data : Byte;
            end;

var
  StartAddr : LongWord;
  Rslt      : word;


implementation



procedure TFrmROMHandler.SaveIni;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.INI' ));
  try
    // Remember Windows positions
    Ini.WriteInteger( 'WindowsPositions', 'ROMHandlerTop', FrmROMHandler.Top);
    Ini.WriteInteger( 'WindowsPositions', 'ROMHandlerLeft', FrmROMHandler.Left);
    Ini.WriteInteger( 'WindowsPositions', 'ROMHandlerWidth', FrmROMHandler.Width);
    Ini.WriteInteger( 'WindowsPositions', 'ROMHandlerHeight', FrmROMHandler.Height);
    // Ini.WriteInteger( 'WindowsPositions', 'ROMHandlerSplitter', FrmROMHandler.PnlRight.Left );

    Ini.WriteInteger( 'WindowsPositions', 'ROMHandlerSplitter', FrmROMHandler.PnlRight.Width);
    Ini.WriteBool('UserInterface', 'AutoFindROM', FrmROMHandler.ChkAuto.Checked);
    Ini.WriteBool('UserInterface', 'MemoryTypeSRAM', FrmROMHandler.RadioSRAM.Checked);
  finally
    Ini.Free;
  end;
end;


procedure TFrmROMHandler.ReadIni;
var
  Ini: TIniFile;
  Code, i: integer;
const
  T = -1;  // default Top
  L = -1;  // default Left
  W = 500; // default Width
  H = 600; // default Height
begin
  Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
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
                   //                            3 - SDK41
    PrefOneLine    := Ini.ReadBool('Disassembler', 'OneLine', false);
                   // if true, prints a line for every word
    PrefSDK41Mode  := Ini.ReadBool('Disassembler', 'SDK41Mode', false);
                   // if true, generates listing for SDK41 re-assembly
    PrefGenXRef    := Ini.ReadBool('Disassembler', 'GenXRef', true);
                   // if true, generates the label cross refence table
    PrefAutoFixed  := Ini.ReadBool('Disassembler', 'AutoFixed', true);
                   // Generates Auto LAbels for fixed XROM addresses
    PrefCleanList  := Ini.ReadBool('Disassembler', 'CleanListing', false);
                   // Do not print adress or hexcodes in listing

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

    PrefUSBTO      := Ini.ReadInteger('Communication', 'USBTimeOut', 10);
                     // regular USB timeout in msec
    PrefFLASHTO    := Ini.ReadInteger('Communication', 'FLASHTimeout', 10);
                     // single byte FLASH write timeout in msec
    PrefSecTO      := Ini.ReadInteger('Communication', 'SectorTimeout', 10);
                     // FLASH Sector Erase timeout in msec
    PrefALLTO      := Ini.ReadInteger('Communication', 'EraseAllTimeout', 10);
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

    if PrefMemType then with FrmROMHandler do begin
      // Memory Type is SRAM
      MemTp := SRAM;
      PnlMemType.Caption := 'SRAM';
      Val(EdtROMNum.Text, I, Code);
      RadioSRAM.Checked := true;
      // if I > 63 then EdtROMNum.Text := '63';
      UpDnNumber.Max := SRAM_Pages;
    end else with FrmROMHandler do begin
      // Memory Type is FLASH
      MemTp := FLASH;
      PnlMemType.Caption := 'FLASH';
      UpDnNumber.Max := 255;
      RadioFLASH.Checked := true;
    end;

    // Remember Windows positions
    FrmROMHandler.Top  := Ini.ReadInteger( 'WindowsPositions', 'ROMHandlerTop', -1);
    FrmROMHandler.Left := Ini.ReadInteger( 'WindowsPositions', 'ROMHandlerLeft', -1);
    FrmROMHandler.Width := Ini.ReadInteger( 'WindowsPositions', 'ROMHandlerWidth', -1);
    FrmROMHandler.Height := Ini.ReadInteger( 'WindowsPositions', 'ROMHandlerHeight', -1);
    FrmROMHandler.PnlRight.Width := Ini.ReadInteger( 'WindowsPositions', 'ROMHandlerSplitter', -1);

    if not WinVisible(FrmROMHandler) then begin
      // window outside current screen? Set to defaults
      FrmROMHandler.Top    := T;
      FrmROMHandler.Left   := L;
      FrmROMHandler.Width  := W;
      FrmROMHandler.Height := H;
      FrmROMHandler.PnlRight.Width := -1;
    end;

  finally
    Ini.Free
  end;
end;

procedure UpdateControl;
// Update dynamic elements in user interface after connect/disconnect
var
  i: integer;
begin
  with FrmRomHandler do begin
    // Update Status bar with relevant information
    if MLDL_Is_Connected then begin
      FrmROMHandler.StatusBar.Panels[0].Text := 'MLDL2000 CONNECTED';
      FrmROMHandler.StatusBar.Panels[1].Text := 'S/N: ' + MLDL_Serial;
      FrmROMHandler.StatusBar.Panels[2].Text := Firmware_ID;
      if FLASH_Tp = TOPBOOT then
        FrmROMHandler.StatusBar.Panels[3].Text := 'TOP Boot Flash'
      else
        FrmROMHandler.StatusBar.Panels[3].Text := 'BOT Boot Flash';
      if SRAM_Sz = REGULAR then
        FrmROMHandler.StatusBar.Panels[4].Text := '512K SRAM '
      else
        FrmROMHandler.StatusBar.Panels[4].Text := '1024K SRAM';
    end else begin
      StatusBar.Panels[0].Text := 'MLDL2000 DISCONNECTED';
      StatusBar.Panels[1].Text := 'S/N: unknown';
      StatusBar.Panels[2].Text := 'FIRMWARE ????';
      StatusBar.Panels[3].Text := '?? FLASH TYPE';
      StatusBar.Panels[4].Text := '????K SRAM';
    end;

    // Enable or disable all menuitems and button for communicating with MLDL200
    FrmRomHandler.MnMainMConnect.Enabled := not MLDL_Is_Connected;
    FrmRomHandler.MnMainMDisconnect.Enabled := MLDL_Is_Connected;
    BtnUpload.Enabled := MLDL_Is_Connected;
    BtnDownload.Enabled := MLDL_Is_Connected;
    BtnVerify.Enabled := MLDL_Is_Connected;
    BtnBackup.Enabled := MLDL_Is_Connected;
    BtnRestore.Enabled := MLDL_Is_Connected;
    BtnBakVerify.Enabled := MLDL_Is_Connected;
    BtnContents.Enabled := MLDL_Is_Connected;
    BtnEraseSector.Enabled := MLDL_Is_Connected;
    BtnEraseAll.Enabled := MLDL_Is_Connected;
    BtnEraseAllSRAM.Enabled := MLDL_Is_Connected;
    BtnInitSRAMSR.Enabled := MLDL_Is_Connected;
    BtnResetFLASH.Enabled := MLDL_Is_Connected;
    BtnCheckEmptySector.Enabled := MLDL_Is_Connected;
    BtnCheckEmptyAll.Enabled := MLDL_Is_Connected;
    BtnReadType.Enabled := MLDL_Is_Connected;
    ChkAuto.Checked := PrefAutoFind;
    ChkAuto.Enabled := MLDL_Is_Connected;
    with PopEraseFlash.Items do begin
      for i := 0 to Count - 1 do
        Items[i].Enabled := MLDL_Is_Connected;
    end;
    with PopListMLDL.Items do begin
      for i := 0 to Count - 1 do
        PopListMLDL.Items[i].Enabled := MLDL_Is_Connected;
    end;
    FrmRomHandler.MnMainMDownload.Enabled := MLDL_Is_Connected;
    FrmRomHandler.MnMainMUpload.Enabled := MLDL_Is_Connected;
    FrmRomHandler.MnMainMVerify.Enabled := MLDL_Is_Connected;
    MODUpload.Enabled := MLDL_Is_Connected;
    BtnUploadMOD.Enabled := MLDL_Is_Connected;
  end;
end;


procedure InitMLDL;
var
  S: string;
  Rslt: integer;
begin
  if OpenMLDLPort then begin
    // MLDL2000 now opened
    MLDL_Is_Connected := true;
    InitMLDLComm;
    Frm_JTAG.ReadFWID(S, Firmware_ID, Rslt) ;
    UpdateControl;
    FrmROMHandler.ReadFlashType;
    FrmROMHandler.ReadSRAMType;
  end else begin
    // Device not opened for whatever reason
    MLDL_Is_Connected := false;
    Firmware_ID := 'Firmware ID: unknown';
    UpdateControl;
  end;
end;


procedure CloseMLDL;
begin
  CloseMLDLPort;
  MLDL_Is_Connected := false;
  UpdateControl;
end;


//---------------------------------------------------------------------------//
//                                                                           //
//  Main Menu Functions                                                      //
//                                                                           //
//---------------------------------------------------------------------------//


procedure TFrmROMHandler.CmbCategoryChange(Sender: TObject);
// Category ComboBox OnChange event
begin
  with (Sender as TComboBox) do begin
    Hint := Items[ItemIndex];
  end;
end;

procedure TFrmROMHandler.CmbCategoryEnter(Sender: TObject);
// Category ComboBox OnEnter event
begin
  with (Sender as TComboBox) do begin
    Left := Left - 150;
    Width := Width + 150;
  end;
end;

procedure TFrmROMHandler.CmbCategoryExit(Sender: TObject);
// Category ComboBox OnExit event
begin
  with (Sender as TComboBox) do begin
    Left := Left + 150;
    Width := Width - 150;
  end;
end;

procedure TFrmROMHandler.EdtMouseEnter(Sender: TObject);
// Edt OnEnter event
begin
  with (Sender as TEdit) do begin
    Width := Width + 100;
  end;
end;

procedure TFrmROMHandler.EdtAuthorMouseLeave(Sender: TObject);
// Edt OnLeave event
begin
  with (Sender as TEdit) do begin
    Width := Width - 100;
  end;
end;

procedure TFrmROMHandler.ExitClick(Sender: TObject);
begin
  SaveIni;
  Application.Terminate;
end;

procedure TFrmROMHandler.FormClose(Sender: TObject; var Act: TCloseAction);
begin
  SaveIni;
end;

procedure TFrmROMHandler.MemoROMKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((ssCtrl in Shift) AND (Key = ord('A'))) then begin
    // CONTROL-A pressed
    MemoROM.SelectAll;
    Key := 0;
  end;
end;

procedure TFrmROMHandler.MemoryEditorClick(Sender: TObject);
begin
  FRmMemEdit.Show;
end;

procedure TFrmROMHandler.MLDLTestClick(Sender: TObject);
begin
  Frm_Tester.Show;
end;

procedure TFrmROMHandler.CPLDUpgradeClick(Sender: TObject);
begin
  Frm_JTAG.Show;
end;

procedure TFrmROMHandler.BitBangClick(Sender: TObject);
begin
  BitBang.Show;
end;

procedure TFrmROMHandler.AboutClick(Sender: TObject);
begin
  AboutBox.Caption := 'About ' + Application.Title;
  AboutBox.ShowModal;
end;

procedure TFrmROMHandler.PreferencesClick(Sender: TObject);
begin
  FrmPreferences.Show;
end;

procedure TFrmROMHandler.IOHandler1Click(Sender: TObject);
begin
  Frm_IOHandler.Show;
end;


//procedure TFrmROMHandler.EditKeyDown(Sender: TObject; var Key: Word;
//  Shift: TShiftState);
//begin
//  if (Key = VK_INSERT) and (Shift = []) then with Sender as TEdit do begin
//    InsertOn := not InsertOn;
//
//  end;
//end;
//
//procedure TFrmROMHandler.EditKeyPress(Sender: TObject; var Key: Char);
//begin
//  with Sender as TEdit do
//    if ((SelLength = 0) and (not InsertOn)) then
//      SelLength := 1;
//end;


procedure NewSRArray;
var
  i: integer;
  S2: String;
begin
  // first initialze the SR array
  for i := 0 to SRSize do begin
    SRArray[i] := $FFF;
    SRComment[i] := '';
  end;
  // Initialize Page Comments
  for i := 0 to 15 do begin
    case i of
       0 : S2 := 'Reserved for System ROM 0';
       1 : S2 := 'Reserved for System ROM 1';
       2 : S2 := 'Reserved for System ROM 2';
       3 : S2 := 'Reserved for 41CX XFunction ROM';
       4 : S2 := 'Reserved for Service ROM';
       5 : S2 := 'Reserved for Timer ROM + 41CX XFunction';
       6 : S2 := 'Reserved for Printer ROM';
       7 : S2 := 'Reserved for HP-IL Module';
       8 : S2 := 'Port 1 Lower page';
       9 : S2 := 'Port 1 Upper page';
      10 : S2 := 'Port 2 Lower page';
      11 : S2 := 'Port 2 Upper page';
      12 : S2 := 'Port 3 Lower page';
      13 : S2 := 'Port 3 Upper page';
      14 : S2 := 'Port 4 Lower page';
      15 : S2 := 'Port 4 Upper page';
    end; //case
    SRCommentPg[i] := S2;
  end;
end;


procedure NewSR;
// Create the TreeviewSR with all SR's set to $FFF
var
  i, j: integer;
  S1, S2: string;
begin
  FrmROMHandler.TreeViewSR.Items.Clear;
  NewSRArray;
  for i := 0 to 15 do begin
    S1 := 'Page ' + Hex1(i);
    S2 := SRCommentPg[i];
    Node := FrmROMHandler.TreeViewSR.Items.Add(nil, S1 + ' -- ' + S2);
    with Node do begin
      // Create the Class for the Page (only Comment is used!)
      Data := TSetReg.Create;
      TSetReg(Data).Value   := $FFF;
      TSetReg(Data).Bank    := 0;
      TSetReg(Data).Page    := i;
      TSetReg(Data).Comment := SRCommentPG[i];
    end;
    for j := 0 to 3 do begin
      S1 := 'Bank ' + Hex1(j) + '   $FFF';
      with FrmROMHandler.TreeViewSR.Items.AddChild(Node, S1) do begin
        // Create the Class for the Banks
        Data := TSetReg.Create;
        TSetReg(Data).Value   := $FFF;
        TSetReg(Data).Bank    := j;
        TSetReg(Data).Page    := i;
        TSetReg(Data).Comment := SRComment[j];
      end;
    end;
  end;
  FrmROMHandler.LblHex.Caption      := '$xxx';
  FrmROMHandler.LblBin.Caption      := '............';
  FrmROMHandler.ChkEnable.Checked   := false;
  FrmROMHandler.ChkEnable.Enabled   := false;
  FrmROMHandler.ChkFlash.Checked    := false;
  FrmROMHandler.ChkFlash.Enabled    := false;
  FrmROMHandler.ChkIO.Checked       := false;
  FrmROMHandler.ChkIO.Enabled       := false;
  FrmROMHandler.ChkWProtect.Checked := false;
  FrmROMHandler.ChkWProtect.Enabled := false;
  FrmROMHandler.EdtROMAddr.Text     := '';
  FrmROMHandler.EdtROMNumber.Text   := '';
  FrmROMHandler.TreeViewSR.Update;
end;


procedure TFrmROMHandler.InitForm(Sender: TObject);
//  Initialize variables
// This procedure is called during the OnCreate Event
begin
  ReadIni;
  I_Error := 0;                    // Initialize Error variable
  FrmROMHandler.StatusBar.Panels[0].Text := 'MLDL2000 DISCONNECTED';
  MLDL_Enable_Error_Report := true; // Enable Error Windows
  FT_Enable_Error_Report := true;
  MLDL_Is_Connected := false;
  InitMLDL;
  Mode := ROM;

  //default settings
  EdtROMNum.Text := '0';
  EdtROMNum.Text := SStr(ROMNum);
  EdtROMAddress.Text := '$' + Hex5(ROMNum*Range_4k);
  SRNum := 0;
  OpenROMFileNm := 'no open file';
  OpenSRFileNm := 'no open file';
  OpenMODFIleNm := 'no open file';
  PnlFileName.Caption := 'no open file';
  PageControl.ActivePage := ROMSheet;
  StaticText1.Caption := 'ROM #';
  if MemTp = SRAM then begin
    PnlMemType.Caption := 'SRAM';
    UpDnNumber.Max := SRAM_Pages;
  end else begin
    PnlMemType.Caption := 'FLASH';
    UpDnNumber.Max := 255;
  end;
  PnlStatus.Caption := 'OK';
  GaugeProgress.Position := 0;
  GaugeProgress.Max := 100;
  MemoROM.Lines.Clear;
  EdtComment.Text := '';
  NewSR;
  
  ChkAuto.Checked := PrefAutoFind;
  ChkAuto.Enabled := MLDL_Is_Connected;
  if FrmROMHandler.ChkAuto.Checked and MLDL_Is_Connected then begin
    if FrmROMHandler.RadioSRAM.Checked then
      FrmROMHandler.FindFreeROM(SRAM)
    else
      FrmROMHandler.FindFreeROM(FLASH);
  end;
end;

procedure TFrmROMHandler.BtnEraseAllSRAMClick(Sender: TObject);
// Erases all of the SRAM, initializes to $A5A5 (easy to recognize)
var
  i : integer;
  // Adr: longword;
begin
  DisableMLDL;
  PnlStatus.Caption := 'ERASING SRAM';
  Rslt := 0;
  GaugeProgress.Position := 0;
  GaugeProgress.Max := SRAM_Pages;

  if MessageDlg('ERASE ALL SRAM?', mtConfirmation, [mbYes, mbNo], 0) = mrYes  then begin
    // Now start the process

    for i := 0 to Size_4K - 1 do WordArray[i] := $A5A5;   // create page with all $A5A5

    for i := 0 to SRAM_Pages do begin
      GaugeProgress.StepIt;
      Application.ProcessMessages;
      MLDL_WriteBlock(SRAM, i * Size_4K, Size_4K);
      if MLDL_Error <> Err_MLDL_OK then begin
        MessageDlg('Error Erasing SRAM', mtError, [mbOk], 0);
        Break;
      end;
    end;
    PnlStatus.Caption := 'DONE';
    BtnInitSRAMSRClick(Sender);      // now initialize SR's in SRAM
  end;
  EnableMLDL;

end;

procedure TFrmROMHandler.ChkDis8KChange(Sender: TObject);
begin
  // when it becomes checked, uncheck IMDB and FLDB
  if ChkDis8K.Checked then begin
    ChkIMDB.Checked   := false;
    ChkFLDB.Checked := false;
  end;
end;

procedure TFrmROMHandler.ChkFLDBChange(Sender: TObject);
begin
  // when it becomes checked, uncheck FAT, 8K and IMDB
  if ChkFLDB.Checked then begin
    ChkHasFAT.Checked := false;
    ChkDis8K.Checked  := false;
    ChkIMDB.Checked   := false;
  end;

end;

procedure TFrmROMHandler.ChkHasFATChange(Sender: TObject);
begin
  // When it becomes check, uncheck IMDB and FLDB CheckBox
  if ChkHasFAT.Checked then begin
    ChkIMDB.Checked := false;
    ChkFLDB.Checked := false;
  end;
end;

procedure TFrmROMHandler.ChkIMDBChange(Sender: TObject);
begin
  // when it becomes checked, uncheck FAT, 8K and FLDB
  if ChkIMDB.Checked then begin
    ChkHasFAT.Checked := false;
    ChkDis8K.Checked  := false;
    ChkFLDB.Checked   := false;
  end;
end;





procedure TFrmROMHandler.BtnInitSRAMSRClick(Sender: TObject);
// Initializes the SR's in SRAM to $0FFF
var
  i : integer;
begin
  DisableMLDL;
  for i := 0 to 4 * (Range_SR + 1) - 1 do WordArray[i] := $0FFF;   // create page with all 0's
  MLDL_WriteBlock(SRAM, SR0_Base, 4 * (Range_SR + 1));
  EnableMLDL;
end;


procedure TFrmROMHandler.LstContentsColumnClick(Sender: TObject; Column: TListColumn);
// used for Sorting items in the Contents tab
var
   sl: TStringList;
   counter, IndexOfCurrentColumn: integer;
   NumItems: integer;
begin
  sl:= Tstringlist.Create;                   // create new stringlist
  sl.Sorted:= true;
  sl.Duplicates:= dupAccept;                 // this is really needed!

  SortColumn := Column.Index;                // trick for descending/ascending sort
  if ColSorted = SortColumn then
    ColSorted := 100
  else
    ColSorted := SortColumn;

  NumItems := LstContents.Items.Count;
  LstContents.Selected := nil;

  try
    IndexOfCurrentColumn := Column.Index;    // column to sort

    if IndexOfCurrentColumn = 0 then
      // Column 0 is special
      for counter := 0 to NumItems - 1 do
        sl.AddObject(LstContents.Items[counter].Caption, LstContents.Items[counter])
    else
      // any other column
      for counter := 0 to NumItems - 1 do
        sl.AddObject(LstContents.Items[counter].SubItems[IndexOfCurrentColumn-1],
        LstContents.Items[counter]);

    // we now have a sorted list, return the results to the original list
    if ColSorted = SortColumn then
      // this column was just sorted, sort descending
      for counter := 0 to NumItems - 1 do
        LstContents.Items[counter] := TListItem(sl.Objects[counter])
    else
      // not sorted earlier, sort ascending
      for counter := 0 to NumItems - 1 do
        LstContents.Items[counter] := TListItem(sl.Objects[NumItems - 1 - counter]);

  finally
    sl.Free;
  end;

end;


function FindListItem(lv: TListView; const S: string;
                      column: integer; select: boolean): TListItem;
// Finds text in a ListView
//       lv     : ListView to be searched
//       S      : text to be found
//       column : column of the ListView to search in
//       select : select item if found
// Returns the Item that the text was found in, returns nil if not found
//  Operation is case sensitive
var
  i: integer;
  found: boolean;
begin
  Assert(Assigned(lv));
  Assert((lv.viewstyle = vsReport) or (column = 0));
  Assert(S<>'');
  for i := 0 to lv.Items.Count - 1 do begin
    Result := lv.Items[i];

    if column = 0 then
      found := CompareStr(Result.Caption, S) = 0
    else if column > 0 then
      found := CompareStr(Result.SubItems[column - 1], S) = 0
    else
      found := false;
    if found then Exit;
  end;
  Result := nil;    // if no result
end;


function PageCRC(var PgArray: ROMPage): LongWord;
// calculates a CRC checksum over PageArray
// Derived from Basic program from Monte Dalrymple as used for the HP41CL

var
  crcL, crcH,                 // CRC accumulator
  fbL, fbH,                   // primes
  byteL, byteH: longword;     // byte values
  crycL, crycH,
  cry, fb,
  crybL, crybH: longword;     // crc helpers
  i, bits: integer;           // index
  Res: LongWord;

begin
  crcL := $FFFF;
  crcH := $FFFF;
  fbL  := $1DB7;              // dec 7607
  fbH  := $04C1;              // dec 1217

  for i := 0 to $0FFF do begin
    byteH := PgArray[i] and $00FF;
    byteL := (PgArray[i] shr 8) and $00FF;

    for bits := 0 to 15 do begin
      // shift the crc state bit left one bit
      crycL := (crcL + crcL) and $10000;
      crycH := (crcH + crcH) and $10000;
      crcL  := (crcL + crcL) and $0FFFF;
      crcH  := (crcH + crcH) and $0FFFF;
      if crycL <> 0 then crcH := crcH + 1;

      // shift the data word left one bit
      crybL := (byteL + byteL) and $0100;
      crybH := (byteH + byteH) and $0100;
      byteL := (byteL + byteL) and $00FF;
      byteH := (byteH + byteH) and $00FF;
      if crybL <> 0 then byteH := byteH + 1;
      cry := 0;
      if crybH <> 0 then cry := $10000;
      fb := cry xor crycH;

      if fb <> 0 then crcL := crcL xor fbL;
      if fb <> 0 then crcH := crcH xor fbH;

    end;    // for bits ...

  end;      // for i ...

  Res := (crcH shl 16) + crcL;
  PageCRC := Res;

end;

procedure DisplayROM;
var
  S: String;
  Accumulator : LongWord;
  i: integer;
  FunAdr, CharPointer, W, LabLen: word;
  NonValid: boolean;
  NOPCount: integer;
  CRCres: Cardinal;
  Is16bit: boolean;

begin
  S := '';                               // may not be initialized in some cases

  // check for 16 bit values, may be HP41CL related ROM
  Is16bit := false;
  for i := 0 to $0FFF do begin
    Is16bit := ((Swap(PageArray[i]) and $FC00) <> $0000) or Is16bit;
  end;

  FrmROMHandler.MemoROM.Lines.Add('------------------hex---dec----------------');
  FrmROMHandler.MemoROM.Lines.Add('  XROM          : $'
                                  + Hex3(Swap(PageArray[0])) + '  '
                                  + Int3(Swap(PageArray[0])));
  FrmROMHandler.MemoROM.Lines.Add('  # Functions   : $'
                                  + Hex3(Swap(PageArray[1])) + '  '
                                  + Int3(Swap(PageArray[1])));

  // Find out ROM Name (first function in ROM)
  FunAdr := (Swap(PageArray[$0002]) and $00FF) shl 8;
  FunAdr := FunAdr or (Swap(PageArray[$0003]) and $00FF);
  NonValid := false;
  if (Swap(PageArray[$0002]) and $0200) = $0200 then begin
    // this is a UserCode Program Name
    // check if there is a valid Global Label structure
    if FunAdr > $0FFF then begin
      S := 'Function Name in other Page (UCode)';
    end else begin
      if (Swap(PageArray[FunAdr]) and $FFF0) <> $01C0 then NonValid := true;
      FunAdr := FunAdr + 2;               // here is the Label length
      // Check if this is not the .END. or something else
      if (Swap(PageArray[FunAdr]) and $FFF0) <> $00F0 then NonValid := true;
      LabLen := Swap(PageArray[FunAdr]) and $0000F;
      S := 'LBL "';
      for i := 2 to LabLen do begin
        W := Swap(PageArray[FunAdr + i]) and $00FF;
        S := S + HPUserChar(W);
      end;
      if not NonValid then
        S := S + '" (User Code) (first function name)'
      else
        S := ' No valid ROM Name found';
    end;
  end else begin
    // this is Microcode, normal situation
    if (FunAdr > $0FFF) or (FunAdr < $0002) then begin
      // Function Name is in other Page
      S := 'Function Name in other Page (MCode)';
    end else begin
      // normal situation, decode Function Name
      CharPointer := FunAdr;
      repeat
        CharPointer := CharPointer - 1;
        W := Swap(PageArray[CharPointer]);
        S := S + HPChar(W);
      until ((W and $0080) <> 0) or (W = $0) or (W > $03FF);
      if (W = $0) or (W > $03FF) then
        S := 'No valid ROM Name found'
    end;
  end;
  FrmROMHandler.MemoROM.Lines.Add('  ROM Name      : ' + S + '  (first function name)');

  FrmROMHandler.MemoROM.Lines.Add('  Pause Entry   : $'
                                  + Hex3(Swap(PageArray[Pause_Entry])));
  FrmROMHandler.MemoROM.Lines.Add('  Program Entry : $'
                                  + Hex3(Swap(PageArray[Prgm_Entry])));
  FrmROMHandler.MemoROM.Lines.Add('  Sleep Entry   : $'
                                  + Hex3(Swap(PageArray[Sleep_Entry])));
  FrmROMHandler.MemoROM.Lines.Add('  OFF Entry     : $'
                                  + Hex3(Swap(PageArray[Off_Entry])));
  FrmROMHandler.MemoROM.Lines.Add('  Service Entry : $'
                                  + Hex3(Swap(PageArray[Service_Entry])));
  FrmROMHandler.MemoROM.Lines.Add('  ON Entry      : $'
                                  + Hex3(Swap(PageArray[On_Entry])));
  FrmROMHandler.MemoROM.Lines.Add('  MemLost Entry : $'
                                  + Hex3(Swap(PageArray[MemLost_Entry])));
  FrmROMHandler.MemoROM.Lines.Add('  ROM Revision  : '
                                  + HPChar(Swap(PageArray[ROMREV_Addr + 3]))
                                  + HPChar(Swap(PageArray[ROMREV_Addr + 2 ])) + '-'
                                  + HPChar(Swap(PageArray[ROMREV_Addr + 1 ]))
                                  + HPChar(Swap(PageArray[ROMREV_Addr + 0 ])));
  // Calculate Checksum
  Accumulator := 0;
  for i := 0 to Rom4KSize - 1 do begin
    Accumulator := Accumulator + Swap(PageArray[i]);
    if Accumulator > $03FF then begin
      Accumulator := (Accumulator and $03FF) + 1;
    end;
  end;
  Accumulator := (-Accumulator) and $03FF;
  if Accumulator = Swap(PageArray[CHECKSUM_Addr]) then
    S := 'CHECKSUM OK'
  else
    S := 'CHECKSUM NOT GOOD, should be $' + Hex3(Accumulator);
  FrmROMHandler.MemoROM.Lines.Add('  Checksum      : $'
                                  + Hex3(Swap(PageArray[CHECKSUM_Addr]))
                                  + '  ' + S);

  NOPCount := 0;
  for i := 0 to $0FFF do
    if (Swap(PageArray[i]) and $03FF) = $000 then Inc(NOPCount);
  FrmROMHandler.MemoROM.Lines.Add('  NOPs Counted  : ' + SStr(NOPCount));

  CRCRes := PageCRC(PageArray);
  FrmROMHandler.MemoROM.Lines.Add('  41CL YCRC     : $' + Hexn(CRCRes, 8));

  if Is16bit then
    FrmROMHandler.MemoROM.Lines.Add('  WARNING       : ROM may contain HP41CL contents');

  FrmROMHandler.MemoROM.Update;
end;


procedure UpdateSR;
// Replace current TreeViewSR with SR settings from SRArray
var
  Pg, Bk, i : integer;
  SR_Contents: word;
  Node, CNode : TTreeNode;
  S, SS, S2, Cmt, TempS, TempSS: String;
  ItemFound: TListItem;
begin
  Node := FrmROMHandler.TreeViewSR.Items.GetFirstNode;
  Pg := 0;

  while Node <> nil do begin
    CNode := Node.GetFirstChild;
    Cmt := '';

    while CNode <> nil do begin
      with CNode do begin
        Pg := TSetReg(Data).Page;
        Bk := TSetReg(Data).Bank;
        if SRComment[Pg*4+Bk] <> '' then
          // get rid of "/" at end of SR line
          S := ' / ' + SRComment[Pg*4+Bk]
        else
          S := '';

        // Get ROM info from contents
        SR_Contents := SRArray[Pg*4+Bk] and $0FFF;
        Text := 'Bank ' + Hex1(Bk) + ' [$' + Hex3(SR_Contents) + ']';
        TSetReg(Data).Value   := SRArray[Pg*4+Bk] and SR_MASK;
        TSetReg(Data).Comment := SRComment[Pg*4+Bk];

        // Find out ROM Name and properties
        if TSetReg(Data).Flash_SR then
          SS := 'FL-'
        else
          SS := 'SR-';

        if TSetReg(Data).noIO_SR then
          S2 := '  '
        else
          S2 := 'IO';

        if TSetReg(Data).WEnabled_SR then
          S2 := S2 + ' [write enabled]'
        else
          S2 := S2 + ' [write protected]';

        if TSetReg(Data).Enabled_SR then
          S2 := S2 + ' [enabled]'
        else
          S2 := S2 + ' [disabled]';

        SS := SS + Int3(TSetReg(Data).Get_RomNum);
        ItemFound := FindListItem(FrmROMHandler.LstContents, SS, 4, false);


        if SR_Contents = $0FFF  then begin
          SS := ' EMPTY';
          Text := Text + SS;
        end else begin
          if ItemFound <> nil then with ItemFound do begin
            SS := SubItems[3];              // SRAM/FLASH ROM Number
            SS := SS + ' / ' + Caption;     // XROM Number
            SS := SS + ' / ' + SubItems[0]; // ROM Name
            SS := SS + ' / ' + SubItems[1]; // Rom Revision

            // Info from Bank 0 goes to parent Node as well
            if Bk = 0 then
              Cmt := SS + S;

          end;
          Text := Text + ' ' + SS + ' / ' + S2 + S;
        end;
      end;
      CNode := CNode.GetNextSibling;
    end;

    with Node do begin
      // Info from Bank 0 goes to parent Node as well
      // if there is a ':' in the text, there was a ROM listed
      TempS := TSetReg(Data).Comment;
      i := Pos(' :', TempS);
      if i = 0 then
        TempS := TempS + ' :'
      else begin
        TempSS := LeftStr(TempS, i);
        TempS := TempSS;
      end;
      Text := 'Page ' + Hex1(Pg) + ' -- ' + TempS + ' ' + Cmt;
      Cmt := '';
    end;

    // Go to next Node
    Node := Node.GetNextSibling;
  end;
end;


procedure TFrmROMHandler.ComSRPickChange(Sender: TObject);
// new SR set chosen
begin

  SRArray := SRArrays[ComSRPick.ItemIndex];  // copy active SR block into SRArray
  UpdateSR;
  UpdateTreeView(Sender);
end;


procedure ReadNullStr(var Fl: bfile; L: integer; var S: string; var NumRead: integer);
//  L : number of bytes to read
//  S : resulting string
//  NumRead: actual number of characters read
var
  i: integer;
begin
  S := '';
  BlockRead(Fl, BufArray, L, NumRead);
  for i := 0 to L - 1 do
    if BufArray[i] <> $00 then S := S + Chr(BufArray[i]);
end;


procedure WriteNullStr(var Fl: bfile; L: integer; S: string; var NumRead: integer);
//  L : number of bytes to read
//  S : resulting string
//  NumRead: actual number of characters read
var
  i, Ln: integer;
begin
  Ln := Length(S);
  for i := 0 to L - 1 do BufArray[i] := 0;
  if Ln > L then Ln := L;
  for i := 0 to Ln - 1 do   // write the string
    BufArray[i] := Ord(S[i + 1]);
  BlockWrite(Fl, BufArray, L, NumRead);
end;



procedure TFrmROMHandler.LstMODColumnClick(Sender: TObject; Column: TListColumn);
// Sort Columns in the MOD Handler
var
   sl: TStringList;
   counter, IndexOfCurrentColumn: integer;
   NumItems: integer;
begin
  sl:= Tstringlist.Create;                   // create new stringlist
  sl.Sorted:= true;
  sl.Duplicates:= dupAccept;                 // this is really needed!

  SortColumn := Column.Index;                // trick for descending/ascending sort
  if ColSorted = SortColumn then
    ColSorted := 100
  else
    ColSorted := SortColumn;

  NumItems := LstMOD.Items.Count;
  LStMOD.Selected := nil;

  try
    IndexOfCurrentColumn := Column.Index;    // column to sort

    if IndexOfCurrentColumn = 0 then
      // Column 0 is special
      for counter := 0 to NumItems - 1 do
        sl.AddObject(LstMOD.Items[counter].Caption, LstMOD.Items[counter])

    else
      // any other column
      for counter := 0 to NumItems - 1 do
        sl.AddObject(LstMOD.Items[counter].SubItems[IndexOfCurrentColumn-1],
        LstMOD.Items[counter]);

    // we now have a sorted list, return the results to the original list

    if ColSorted = SortColumn then
      // this column was just sorted, sort descending
      for counter := 0 to NumItems - 1 do
        LstMOD.Items[counter] := TListItem(sl.Objects[counter])
    else
      // not sorted earlier, sort ascending
      for counter := 0 to NumItems - 1 do
        LstMOD.Items[counter] := TListItem(sl.Objects[NumItems - 1 - counter]);

  finally
    sl.Free;
  end;

end;

procedure InitMODVarsHeader;
var
  i: integer;
begin
  MODFileFormat    := 'MOD1';
  MODTitle         := '';
  MODVersion       := '';
  MODPartNumber    := '';
  MODAuthor        := '';
  MODCopyright     := '';
  MODLicense       := '';
  MODComments      := '';
  MODCategory      := 0;
  MODHardware      := 0;
  MODMemModules    := 0;
  MODXMemModules   := 0;
  MODOriginal      := 0;
  MODAppAutoUpdate := 0;
  MODNumPages      := 0;
  MODNumPagesCustom := 0;
  for i := 0 to 31 do MODHeaderCustom[i]  := 0;
  for i := 0 to 255 do MODBufArray[i]  := 0;
end;


procedure InitMODVarsPage;
var
  i: integer;
begin
  MODName          := '';
  MODID            := '';
  MODPage          := 0;
  MODPageGroup     := 0;
  MODBank          := 0;
  MODBankGroup     := 0;
  MODRAM           := 0;
  MODWriteProtect  := 0;
  MODFAT           := 1;
  for i := 0 to 5119 do MODImage[i]  := 0;
  for i := 0 to 31 do MODPageCustom[i]  := 0;
  for i := 0 to 255 do MODBufArray[i]  := 0;
end;


procedure InitMODVars;
begin
  InitMODVarsHeader;
  InitMODVarsPage;
end;


procedure UnCompressBin;
// Uncompresses the .BIN image in the array MODImage to PageArray
var
  i: integer;
  // Wrd0, Wrd1, Wrd2, Wrd3, Wrd4: word;
  // W10, W11, W12, W13: word;
begin
//  BIN - This format is used by Emu41 (J-F Garnier) and HP41EPC (HrastProgrammer).
//        Note: HP41EPC uses BIN format but names them .ROM files.
//        All bits are packed into 5120 bytes, but several consecutive pages may
//        occupy the same file, so the file size could be a multiple of 5120.
//        4 machine words are packed into 5 bytes:
//          Byte0 = Word0[7-0]
//          Byte1 = Word1[5-0] << 2 | Word0[9-8]
//          Byte2 = Word2[3-0] << 4 | Word1[9-6]
//          Byte3 = Word3[1-0] << 6 | Word2[9-4]
//          Byte4 = Word3[9-2]
//    for (i=0;i<5120;i+=5)
//      {
//      *ptr++=((BIN[i+1]&0x03)<<8) | BIN[i];
//      *ptr++=((BIN[i+2]&0x0F)<<6) | ((BIN[i+1]&0xFC)>>2);
//      *ptr++=((BIN[i+3]&0x3F)<<4) | ((BIN[i+2]&0xF0)>>4);
//      *ptr++=(BIN[i+4]<<2) | ((BIN[i+3]&0xC0)>>6);
//      }
  for i := 0 to ($1000 div 4) - 1 do begin
      MODArray[i*4]   := swapw(((MODImage[i*5+1] and $03) shl 8) or MODImage[i*5]);
      MODArray[i*4+1] := swapw(((MODImage[i*5+2] and $0F) shl 6) or
                              ((MODImage[i*5+1] and $FC) shr 2));
      MODArray[i*4+2] := swapw(((MODImage[i*5+3] and $3F) shl 4) or
                              ((MODImage[i*5+2] and $F0) shr 4));
      MODArray[i*4+3] := swapw(((MODImage[i*5+4]) shl 2) or
                              ((MODImage[i*5+3] and $C0) shr 6));
  end;
end;


procedure CompressBin;
// Compresses the .ROM image to .BIN
var
  i: integer;
begin
//  for (i=0,j=0;i<0x1000;i+=4)
//    {
//    BIN[j++]=ROM[i]&0x00FF;
//    BIN[j++]=((ROM[i+1]&0x003F)<<2) | ((ROM[i]&0x0300)>>8);
//    BIN[j++]=((ROM[i+2]&0x000F)<<4) | ((ROM[i+1]&0x03C0)>>6);
//    BIN[j++]=((ROM[i+3]&0x0003)<<6) | ((ROM[i+2]&0x03F0)>>4);
//    BIN[j++]=(ROM[i+3]&0x03FC)>>2;
//    }
  for i := 0 to ($1000 div 4) - 1 do begin
    MODImage[i*5]   := (  MODArray[i*4  ] and $00FF)    ;
    MODImage[i*5+1] := (((MODArray[i*4  ] and $0300) shr 8) or
                            ((MODArray[i*4+1] and $003F) shl 2));
    MODImage[i*5+2] := (((MODArray[i*4+1] and $03C0) shr 6) or
                            ((MODArray[i*4+2] and $000F) shl 4));
    MODImage[i*5+3] := (((MODArray[i*4+2] and $03F0) shr 4) or
                            ((MODArray[i*4+3] and $0003) shl 6));
    MODImage[i*5+4] := (( MODArray[i*4+3] and $03FC) shr 2);
  end;
end;


procedure ReadMODFileHeader(var Fl: bfile);
var
  NumRead: integer;
begin
  // Read the Main Header
  ReadNullStr(Fl,   5, MODFileFormat, NumRead);
  ReadNullStr(Fl,  50, MODTitle,      NumRead);
  ReadNullStr(Fl,  10, MODVersion,    NumRead);
  ReadNullStr(Fl,  20, MODPartNumber, NumRead);
  ReadNullStr(Fl,  50, MODAuthor,     NumRead);
  ReadNullStr(Fl, 100, MODCopyRight,  NumRead);
  ReadNullStr(Fl, 200, MODLicense,    NumRead);
  ReadNullStr(Fl, 255, MODComments,   NumRead);

  Read(Fl, MODCategory, MODHardware, MODMemModules, MODXMemModules,
                MODOriginal, MODAppAutoUpdate, MODNumPages);
  BlockRead(Fl, MODHeaderCustom, 32, NumRead);

end;


procedure DisplayModFileHeader;
// Reads the Main MOD Header from the open file and displays it is the TAB
var
  NumItems, MaxIndex, i: integer;
  S: string;
begin
  // ReadMODFileHeader;

  // Display Information:
  FrmROMHandler.LblMODFormat.Caption := MODFileFormat;
  FrmROMHandler.EdtTitle.Text        := MODTitle;
  FrmROMHandler.EdtVersion.Text      := MODVersion;
  FrmROMHandler.EdtPartNum.Text      := MODPartNumber;
  FrmROMHandler.EdtAuthor.Text       := MODAuthor;

  FrmROMHandler.EdtCopyRight.Text    := MODCopyRight;
//  FrmROMHandler.EdtCopyRight.Hint    := MODCopyRight;

  FrmROMHandler.EdtLicense.Text      := MODLicense;
//  FrmROMHandler.EdtLicense.Hint      := MODLicense;

  FrmROMHandler.EdtComments.Text     := MODComments;
//  FrmROMHandler.EdtComments.Hint     := MODComments;

  with FrmROMHandler.CmbCategory do begin
    NumItems := Items.Count;    // Number of items in list, last one is always undefined
    MaxIndex := NumItems + 1;   // Index of last item
    if MODCategory >= MaxIndex then
      // Index is larger than number of items in list, this is undefined
      ItemIndex := MaxIndex
    else
      ItemIndex := MODCategory;
    Hint := Items[ItemIndex];
  end;

  with FrmROMHandler.CmbHardware do begin
    NumItems := Items.Count;    // Number of items in list, last one is always undefined
    MaxIndex := NumItems + 1;   // Index of last item
    if MODHardware >= MaxIndex then
      // Index is larger than number of items in list, this is undefined
      ItemIndex := MaxIndex
    else
      ItemIndex := MODHardware;
    Hint := Items[ItemIndex];
  end;

  with FrmROMHandler.CmbMem do begin
    NumItems := Items.Count;    // Number of items in list, last one is always undefined
    MaxIndex := NumItems + 1;   // Index of last item
    if MODMemModules >= MaxIndex then
      // Index is larger than number of items in list, this is undefined
      ItemIndex := MaxIndex
    else
      ItemIndex := MODMemModules;
    Hint := Items[ItemIndex];
  end;

  with FrmROMHandler.CmbXMEM do begin
    NumItems := Items.Count;    // Number of items in list, last one is always undefined
    MaxIndex := NumItems + 1;   // Index of last item
    if MODXMemModules >= MaxIndex then
      // Index is larger than number of items in list, this is undefined
      ItemIndex := MaxIndex
    else
      ItemIndex := MODXMemModules;
    Hint := Items[ItemIndex];
  end;

  FrmROMHandler.ChkOriginal.Checked := not (MODOriginal = 0);
  FrmROMHandler.ChkUpdate.Checked := (MODAppAutoUpdate = 1);
  FrmROMHandler.LblNumPages.Caption := SStr(MODNumPages);

  S := '';
  for i := 0 to 31 do S := S + Hex2(MODHeaderCustom[i]) + '.';
  FrmROMHandler.EdtCustomHdr.Text := S;

  // Definition of MODHeaderCustom, in case of backup
  //  Byte 0: $A5               code to recognize Backup
  //  Byte 1:                   high byte of number of pages
  //  Byte 2:                   low byte of number of pages

  if (MODCategory = CATEGORY_M2KBACKUP) and (MODHeaderCustom[0] = $A5) then begin
     // This is an MLDL2000 backup file
    MODNumPagesCustom := $100 * MODHeaderCustom[1] + MODHeaderCustom[2];
    S := 'MLDL2000 Backup, ' + Int3(MODNumPagesCustom) + ' pages';
    FrmROMHandler.LblCustomH.Caption := S;
    if MODNumPages = 255 then
      FrmROMHandler.LblNumPages.Caption := SStr(MODNumPagesCustom) + '*';
  end else
    FrmROMHandler.LblCustomH.Caption := 'Unknown Custom Header';

end;


procedure GetModFileHeader;
// Reads the Main MOD Header from the Display and returns to variables
var
  i: integer;
  S: string;
  Res: boolean;
begin
  // Display Information:
  MODFileFormat := FrmROMHandler.LblMODFormat.Caption;
  MODTitle      := FrmROMHandler.EdtTitle.Text;
  MODVersion    := FrmROMHandler.EdtVersion.Text;
  MODPartNumber := FrmROMHandler.EdtPartNum.Text;
  MODAuthor     := FrmROMHandler.EdtAuthor.Text;

  MODCopyRight  := FrmROMHandler.EdtCopyRight.Text;
//  FrmROMHandler.EdtCopyRight.Hint    := MODCopyRight;

  MODLicense    := FrmROMHandler.EdtLicense.Text;
//  FrmROMHandler.EdtLicense.Hint      := MODLicense;

  MODComments   :=FrmROMHandler.EdtComments.Text;
//  FrmROMHandler.EdtComments.Hint     := MODComments;

  MODCategory   := FrmROMHandler.CmbCategory.ItemIndex;

  MODHardware   := FrmROMHandler.CmbHardware.ItemIndex;

  MODMemModules := FrmROMHandler.CmbMem.ItemIndex;

  MODXMemModules := FrmROMHandler.CmbXMEM.ItemIndex;

  if FrmROMHandler.ChkOriginal.Checked then
    MODOriginal := 1
  else
    MODOriginal := 0;

  if FrmROMHandler.ChkUpdate.Checked then
    MODAppAutoUpdate := 1
  else
    MODAppAutoUpdate := 0;

//  MODNumPages := VVal(FrmROMHandler.LblNumPages.Caption);

  S := FrmROMHandler.EdtCustomHdr.Text;
  for i := 0 to 31 do begin
    MODHeaderCustom[i] := HexToByte(S, Res);
    if not Res then MODHeaderCustom[i] := 0;
  end;
end;


procedure ReadMODPage(var Fl: bfile);
var
  NumRead: integer;
begin
  // Read Page information from current open file
  ReadNullStr(Fl, 20, MODName,       NumRead);
  ReadNullStr(Fl,  9, MODID,         NumRead);
  Read(Fl, MODPage, MODPageGroup, MODBank, MODBankGroup, MODRAM,
                MODWriteProtect, MODFAT);
  BlockRead(Fl, MODImage, 5120, NumRead);
  BlockRead(Fl, MODPageCustom, 32, NumRead);
end;


procedure ClearMODFilePage;
begin
  FrmROMHandler.CmbROMPage.ItemIndex := -1;
  FrmROMHandler.CmbROMPgGroup.ItemIndex := -1;
  FrmROMHandler.CmbROMBk.ItemIndex := -1;
  FrmROMHandler.CmbROMBkGroup.ItemIndex := -1;
  FrmROMHandler.ChkROMRAM.Checked := false;
  FrmROMHandler.ChkROMFAT.Checked := true;
  FrmROMHandler.ChkROMWP.Checked := false;
  FrmROMHandler.EdtROMCustomHdr.Text := 'n/a';
  FrmROMHandler.EdtROMName.Text := 'n/a';
  FrmROMHandler.EdtROMRev.Text := 'n/a';
  FrmROMHandler.LblXROM.Caption := 'n/a';
  FrmROMHandler.LblFCNS.Caption := 'n/a';
  FrmROMHandler.LblNAME.Caption := 'n/a';
  FrmROMHandler.LblREV.Caption := 'n/a';
  FrmROMHandler.LblCHK.Caption := 'n/a';
  FrmROMHandler.LblCALCCHK.Caption := 'n/a';
  FrmROMHandler.GroupBox2.Caption := 'Module Characteristics';
end;


procedure DisplayMODFilePage;
var
  Index, NumItems, MaxIndex: integer;
  S: string;
  Accumulator : LongWord;
  i: integer;
  FunAdr, CharPointer, W, LabLen: word;
  NonValid: boolean;
begin
  if MODNumPages = 0 then begin
    ClearMODFilePage;
    // Exit;
  end;
  
  FrmROMHandler.EdtROMName.Text := MODName;
  FrmROMHandler.EdtROMRev.Text  := MODID;

  if MODPage > $10 then begin
    case MODPage of
      $1F: Index := 16;
      $2F: Index := 17;
      $3F: Index := 18;
      $4F: Index := 19;
      $5F: Index := 20;
      $6F: Index := 21;
      else Index := 22;
    end;
  end else
    Index := MODPage;

  with FrmROMHandler.CmbROMPage do begin
    ItemIndex := Index;
    Hint := Items[ItemIndex];
  end;

  with FrmROMHandler.CmbROMPgGroup do begin
    NumItems := Items.Count;    // Number of items in list, last one is always undefined
    MaxIndex := NumItems - 1;   // Index of last item
    if MODPageGroup >= MaxIndex then
      // Index is larger than number of items in list, this is undefined
      ItemIndex := MaxIndex
    else
      ItemIndex := MODPageGroup;
    Hint := Items[ItemIndex];
  end;

  Index := MODBank - 1;
  with FrmROMHandler.CmbROMBk do begin
    NumItems := Items.Count;    // Number of items in list, last one is always undefined
    MaxIndex := NumItems - 1;   // Index of last item
    if (Index >= MaxIndex) then
      // Index is larger than number of items in list, this is undefined
      ItemIndex := MaxIndex
    else
      ItemIndex := Index;
    { TODO : This does not seem to work when ItemIndex = -1 }
    if (ItemIndex < 0) then
      Hint := Items[MaxIndex]
    else
      Hint := Items[ItemIndex];
  end;

  with FrmROMHandler.CmbROMBkGroup do begin
    NumItems := Items.Count;    // Number of items in list, last one is always undefined
    MaxIndex := NumItems - 1;   // Index of last item
    if MODBankGroup >= MaxIndex then
      // Index is larger than number of items in list, this is undefined
      ItemIndex := MaxIndex
    else
      ItemIndex := MODBankGroup;
    Hint := Items[ItemIndex];
  end;

  FrmROMHandler.ChkROMRAM.Checked := not (MODRAM = 0);
  FrmROMHandler.ChkROMFAT.Checked := not (MODFAT = 0);
  FrmROMHandler.ChkROMWP.Checked := not (MODWriteProtect = 0);

  S := '';
  for i := 0 to 31 do S :=  S + Hex2(MODPageCustom[i]) + '.';
  FrmROMHandler.EdtROMCustomHdr.Text := S;

  // Definition of MODPageCustom:
  //  Byte 0: $A5               code to recognize Backup
  //  Byte 1: $01  SRAM         bit 1 or 2 always set
  //          $02  FLASH
  //          $04  ROM          bit 3 or 4 always set
  //          $08  SR
  //  Byte 2: Page number       not used when SR
  //          $FF when FLASH was found empty

  S := '';
  if MODPageCustom[0] = $A5 then begin
    // this is the marker for the MLDL2000 backup
    case MODPageCustom[1] of
      $05: S := 'SRAM  ROM Page ' + Int3(MODPageCustom[2]) + ' backup';
      $06: begin
             S := 'FLASH ROM Page ';
             if MODPageCustom[2] = $FF then
               S := S + ' backup of ERASED Page'
             else
               S := S + Int3(MODPageCustom[2]) + ' backup';
           end;
      $09: S := 'SRAM  SR backup';
      $0A: S := 'FLASH SR backup';
      else S := 'Unknown Custom Header';
    end;
  end else if True then
    S := 'Unknown Custom Header';
  FrmROMHandler.LblCustom.Caption := S;

  // Now display some of the contents of the ROM:
  //     XROM, #FUNCS, ROMREV, ROMNAME, Checksum, calculated checksum
  // These cannot be edited

  UnCompressBin;

  if MODFAT <> 0 then begin
    // Image has FAT
    FrmROMHandler.LblXROM.Caption := Int3(Swap(MODArray[0]));
    FrmROMHandler.LblFCNS.Caption := Int3(Swap(MODArray[1]));

    // Get Function Name
    FunAdr := (Swap(MODArray[$0002]) and $00FF) shl 8;
    FunAdr := FunAdr or (Swap(MODArray[$0003]) and $00FF);
    NonValid := false;
    S := '';
    if (Swap(MODArray[$0002]) and $0200) = $0200 then begin
      // this is a UserCode Program Name
      // check if there is a valid Global Label structure
      if FunAdr > $0FFF then begin
        S := 'outside Page (UC)';
      end else begin
        if (Swap(MODArray[FunAdr]) and $FFF0) <> $01C0 then NonValid := true;
        FunAdr := FunAdr + 2;
        if (Swap(MODArray[FunAdr]) and $FFF0) <> $00F0 then NonValid := true;
        LabLen := Swap(MODArray[FunAdr]) and $0000F;
        S := 'LBL "';
        for i := 2 to LabLen do begin
          W := Swap(MODArray[FunAdr + i]) and $00FF;
          S := S + HPUserChar(W);
        end;
        if not NonValid then
          S := S + '" (UC)'
        else
          S := 'No ROM Name';
      end;
    end else begin
      // this is Microcode, normal situation
      if (FunAdr > $0FFF) or (FunAdr < $0002) then begin
        // Function Name is in other Page
        S := 'outside Page (MC)';
      end else begin
        // normal situation, decode Function Name
        CharPointer := FunAdr;
        repeat
          CharPointer := CharPointer - 1;
          W := Swap(MODArray[CharPointer]);
          S := S + HPChar(W);
        until ((W and $0080) <> 0) or (W = $0) or (W > $03FF);
        if (W = $0) or (W > $03FF) then
          S := 'No ROM Name'
      end;
    end;
    FrmROMHandler.LblNAME.Caption := S;
  end else begin
    // Image has no FAT
    FrmROMHandler.LblXROM.Caption := 'n/a';
    FrmROMHandler.LblFCNS.Caption := 'n/a';
    FrmROMHandler.LblNAME.Caption := 'n/a';
  end;

  FrmROMHandler.LblREV.Caption := HPChar(Swap(MODArray[ROMREV_Addr + 3]))
                                + HPChar(Swap(MODArray[ROMREV_Addr + 2 ])) + '-'
                                + HPChar(Swap(MODArray[ROMREV_Addr + 1 ]))
                                + HPChar(Swap(MODArray[ROMREV_Addr + 0 ]));


  // Calculate Checksum
  Accumulator := 0;
  for i := 0 to Rom4KSize - 1 do begin
    Accumulator := Accumulator + Swap(MODArray[i]);
    if Accumulator > $03FF then begin
      Accumulator := (Accumulator and $03FF) + 1;
    end;
  end;
  Accumulator := (-Accumulator) and $03FF;
  FrmROMHandler.LblCHK.Caption := Hex3(Swap(MODArray[CHECKSUM_Addr]));
  FrmROMHandler.LblCALCCHK.Caption := Hex3(Accumulator);

  S := 'Module Characteristics - Page ' + SStr(MODCurrent) + ' of ';
  if MODNumPagesCustom = 0 then
    S := S + SStr(MODNumPages)
  else
    S := S + SStr(MODNumPagesCustom);
  FrmROMHandler.GroupBox2.Caption := S;
end;


procedure GetMODFilePage;
var
  Index: integer;
  S: string;
  i: integer;
  Res: boolean;
begin
  if MODNumPages = 0 then Exit;

  MODName := FrmROMHandler.EdtROMName.Text;
  MODID   := FrmROMHandler.EdtROMRev.Text;

  Index := FrmROMHandler.CmbROMPage.ItemIndex;
  case Index of
    16: MODPage := $1F;
    17: MODPage := $2F;
    18: MODPage := $3F;
    19: MODPage := $4F;
    20: MODPage := $5F;
    21: MODPage := $6F;
    22: MODPage := $00;   // this is undefined!
    else MODPage := Index;
  end;

  MODPageGroup := FrmROMHandler.CmbROMPgGroup.ItemIndex;

  MODBank      := FrmROMHandler.CmbROMBk.ItemIndex + 1;

  MODBankGroup := FrmROMHandler.CmbROMBkGroup.ItemIndex;

  if FrmROMHandler.ChkROMRAM.Checked then
    MODRAM := 1
  else
    MODRAM := 0;

  if FrmROMHandler.ChkROMFAT.Checked then
    MODFAT := 1
  else
    MODFAT := 0;

  if FrmROMHandler.ChkROMWP.Checked then
    MODWriteProtect := 1
  else
    MODWriteProtect := 0;

  S := FrmROMHandler.EdtROMCustomHdr.Text;
  for i := 0 to 31 do begin
    MODPageCustom[i] := HexToByte(S, Res);
    if not Res then MODPageCustom[i] := 0;
  end;
end;


procedure TFrmROMHandler.LstMODDblClick(Sender: TObject);
var
  Idx: integer;
begin
  if (LstMOD.Selected = nil) or (LstMOD.Items.Count = 1) then Exit;
    // No node selected or only one item in the file
  Idx := VVal(LstMOD.Selected.Caption) - 1;
  Seek(MODFile, MODHeaderSize + Idx * MODFilePageSize);
  ReadMODPage(MODFile);
  MODCurrent := Idx + 1;
  DisplayMODFilePage;
end;


procedure TFrmROMHandler.LstMODSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  LstMODDblClick(Sender);
end;

procedure TFrmROMHandler.BtnApplyEditClick(Sender: TObject);
// Applies all Edits, save changes in form to variables and update form
begin
  GetMODFileHeader;                // Read header display to variables
  DisplayMODFileHeader;            // Refresh header display
  GetMODFilePage;                  // Read page display to variables
  DisplayMODFilePage;              // Refresh header display
end;

procedure TFrmROMHandler.BtnCancelEditClick(Sender: TObject);
// Cancel all edits
begin
  DisplayMODFileHeader;            // Refresh header display
  DisplayMODFilePage;              // Refresh header display
end;


procedure WriteMODHeader(var Fl: bfile);
var
  NumRead: integer;
begin
  // Write the Main Header
  WriteNullStr(Fl,   5, MODFileFormat, NumRead);
  WriteNullStr(Fl,  50, MODTitle,      NumRead);
  WriteNullStr(Fl,  10, MODVersion,    NumRead);
  WriteNullStr(Fl,  20, MODPartNumber, NumRead);
  WriteNullStr(Fl,  50, MODAuthor,     NumRead);
  WriteNullStr(Fl, 100, MODCopyRight,  NumRead);
  WriteNullStr(Fl, 200, MODLicense,    NumRead);
  WriteNullStr(Fl, 255, MODComments,   NumRead);
  Write(Fl, MODCategory, MODHardware, MODMemModules, MODXMemModules,
                 MODOriginal, MODAppAutoUpdate, MODNumPages);
  BlockWrite(Fl, MODHeaderCustom, 32, NumRead);
end;


procedure WriteMODPage(var Fl: bfile);
var
  NumRead: integer;
begin
  // Read Page information from current open file
  WriteNullStr(Fl, 20, MODName,       NumRead);
  WriteNullStr(Fl,  9, MODID,         NumRead);
  Write(Fl, MODPage, MODPageGroup, MODBank, MODBankGroup, MODRAM,
                 MODWriteProtect, MODFAT);
  BlockWrite(Fl, MODImage, 5120, NumRead);
  BlockWrite(Fl, MODPageCustom, 32, NumRead);
end;


procedure TFrmROMHandler.BtnSaveEditClick(Sender: TObject);
// Writes back changes in the MOD for the MODFile Header and Page
begin
  BtnApplyEditClick(Sender);
  {$I-}
  Seek(MODFile, 0);              // go to start of file
  {$I+}
  if IOResult <> 0 then begin
    MessageDlg('No MODFILE open', mtWarning, [mbOK], 0);
  end else begin
    WriteMODHeader(MODFile);                // Write Header
    if MODNumPages <> 0 then begin
      // go to position of Current MOD Page
      Seek(MODFile, MODHeaderSize + (MODCurrent - 1) * MODFilePageSize);
      WriteMODPage(MODFile);                  // Write the page
    end;
  end;
end;


function CreateMODFile(Fname: string): integer;
begin
  MODFileBacked := false;
  AssignFile(MODFile, Fname);
  FileMode := 2;  // Open for read and write
  ReWrite(MODFile);
  Reset(MODFile);
  OpenMODFileNm := Fname;
  InitMODVars;
  Result := 1;
end;


procedure TFrmROMHandler.BtnNewMODClick(Sender: TObject);
// Create a new MODFile
var
  FileNm : string;
begin
  // First close any open MODFile
  {$I-}
  CloseFile(MODFile);
  if IOResult <> 0 then Null;
  {$I+}
  DlgSave.Filter := 'MOD File|*.mod';
  DlgSave.FileName := OpenMODFileNm;
  DlgSave.DefaultExt := 'mod';
  if DlgSave.Execute then begin
    FileNm := DlgSave.Filename;
    if CreateMODFile(FileNm) > 0 then begin
      PnlFileName.Caption := ExtractFileName(FileNm);
      DisplayMODFileHeader;
      DisplayMODFilePage;
      LstMOD.Clear;
    end else
      PnlFileName.Caption := 'Error saving file';
  end;
end;


procedure ReadMODFile;
var
  NewItem: TListItem;
  i, NumPages: integer;
  xr: word;
begin
  with FrmROMHandler do begin
    {$I-}
    Seek(MODFile, 0);              // go to start of file
    {$I+}
    if IOResult <> 0 then begin
      MessageDlg('No MODFILE open', mtWarning, [mbOK], 0);
      Exit;
    end;
    // Read File Header and display in Form
    ReadMODFileHeader(MODFile);
    DisplayMODFileHeader;
    LstMOD.Clear;

    if MODNumPagesCustom <> 0 then
      NumPages := MODNumPagesCustom
    else
      NumPages := MODNumPages;

    if NumPages <> 0 then begin
      for i := 1 to NumPages do begin
        // Loop through Pages in MOD File, add Page parameters to List
        ReadMODPage(MODFile);
        NewItem := LstMOD.Items.Add;
        NewItem.Caption := Int3(i);      // Indexnumber in MOD File
        NewItem.SubItems.Add(MODName);   // Name of image
        NewItem.SubItems.Add(MODID);     // Revision

        // get XROM number
        xr := ((MODImage[1] and $03) shl 8) or MODImage[0];
        NewItem.SubItems.Add(Int3(xr));
      end;
    end;
    Seek(MODFile, MODHeaderSize);
    if MODNumPages <> 0 then begin
      ReadMODPage(MODFile);
      DisplayMODFilePage;
      MODCurrent := 1;
      GroupBox2.Caption := 'Module Characteristics - Page 1 of ' + SStr(NumPages);
    end else
      ClearMODFilePage;
  end;
end;


procedure TFrmROMHandler.BtnROM2MODClick(Sender: TObject);
// Add current ROM File to open MODFile
// Overwrites selected page when ChkOvrMOD is checked
var
  i, Idx: integer;
begin
  {$I-}
  Seek(MODFile, 0);              // go to start of file
  {$I+}
  if IOResult <> 0 then begin
    MessageDlg('No MODFILE open', mtWarning, [mbOK], 0);
    Exit;
  end;
  if MODNumPages >254 then begin
    // this prevents adding pages to a backupfile with >255 pages!
    MessageDlg('Too many images', mtWarning, [mbOK], 0);
    Exit;
  end;

  if MODCategory = CATEGORY_M2KBACKUP then begin
    // this prevents adding pages to a backupfile with >255 pages!
    MessageDlg('Adding pages to backup not allowed', mtWarning, [mbOK], 0);
    Exit;
  end;


  // First copy data from ROM to MODArray and compress
  for i := 0 to $0FFF do MODArray[i] := swap(PageArray[i]);
  CompressBin;

  if not ChkOvrMOD.Checked then begin
    // Append new page to MOD file
    Inc(MODNumPages);
    DisplayMODFilePage;                    // Display parameters, mainly to get ROM name and ID
    MODName := LblName.Caption;
    MODID   := LBLRev.Caption;
    Seek(MODFile, 0);                      // go to start of file
    WriteMODHeader(MODFile);               // update header with NumPages

    Seek(MODFile, FileSize(MODFile));      // go to end of File
    WriteMODPage(MODFile);                 // Write new info
    ReadMODFile;                           // Re-read MODFile
  end else begin
    // Overwrite selected page
    if (LstMOD.Selected = nil) or (LstMOD.SelCount <> 1) then Exit;
    DisplayMODFilePage;                    // Display parameters, mainly to get ROM name and ID
    MODName := LblName.Caption;
    MODID   := LBLRev.Caption;
    // No node selected or multiple items selected
     Idx := VVal(LstMOD.Selected.Caption) - 1;
     Seek(MODFile, MODHeaderSize + Idx * MODFilePageSize);
     WriteMODPage(MODFile);                 // Write new info
     ReadMODFile;                           // Re-read MODFile
  end;
  ChkOvrMOD.Checked := false;
end;


procedure TFrmROMHandler.ROM2MODAppndClick(Sender: TObject);
begin
  ChkOvrMOD.Checked := false;
  BtnROM2MODClick(Sender);
end;


procedure TFrmROMHandler.ROM2MODOvrClick(Sender: TObject);
begin
  ChkOvrMOD.Checked := true;
  BtnROM2MODClick(Sender);
  ChkOvrMOD.Checked := false;
end;


procedure TFrmROMHandler.MODSelectALLClick(Sender: TObject);
var
  cnt, idx: integer;
begin
  // LstMOD.SelectAll;         // this does not work in Lazarus
  with LstMOD do begin
    cnt := Items.Count;         // number of items in the list
    if cnt <> 0 then for idx := 0 to cnt - 1 do
      Items[idx].Selected := true;
  end;
end;


procedure TFrmROMHandler.MODUnselectALLClick(Sender: TObject);
var
  cnt, idx: integer;
begin
  // LstMOD.ClearSelection;     // this does not work in Lazarus
  with LstMOD do begin
    cnt := Items.Count;         // number of items in the list
    if cnt <> 0 then for idx := 0 to cnt - 1 do
      Items[idx].Selected := false;
  end;
end;


procedure TFrmROMHandler.BtnDelMODClick(Sender: TObject);
// Remove selected Pages from MOD file
var
  i, j, Idx, OldNumPages, ByteCount, Rslt: integer;
  TempFileNm, SaveMODFileNm: string;
  TempFile: bfile;
  Found: boolean;
  TempBuf: array[0..4095] of byte;
begin
  {$I-}
  Seek(MODFile, 0);              // go to start of file
  {$I+}
  if IOResult <> 0 then begin
    MessageDlg('No MODFILE open', mtWarning, [mbOK], 0);
    Exit;
  end;

  if LstMOD.Selected = nil then
    // none selected
    Exit
  else with LstMOD do begin
    if MessageDlg('Delete pages from MOD file', mtConfirmation, [mbYES, mbNO], 0) = mrNO then Exit;
    FrmROMHandler.PnlStatus.Caption := 'Deleting pages ...';
    Application.ProcessMessages;
    // Create new temp file for saving changed file
    SaveMODFileNm := OpenMODFileNm;
    TempFileNm := ChangeFileExt(OpenMODFileNm, '.TMP');
    AssignFile(TempFile, TempFileNm);
    FileMode := 2;            // Open for read and write
    ReWrite(TempFile);        // Clear Temp file

    ReadMODFileHeader(MODFile);

    // take care to handle backupfiles!
    if MODNumPagesCustom = 0 then begin
      // standard situation
      OldNumPages := MODNumPages;
      MODNumPages := MODNumPages - SelCount;  // new value
    end else begin
      OldNumPages := MODNumPagesCustom;
      MODNumPagesCustom := MODNumPagesCustom - SelCount;  // new value
      MODHeaderCustom[1] := MODNumPagesCustom div $100;
      MODHeaderCustom[2] := MODNumPagesCustom mod $100;
      if MODNumPagesCustom < 255 then
        // it now becomes a 'normal' MOD file, we leave CustomHeader intact!
        MODNumPages := MODNumPagesCustom;
     end;

    // Copy MOD Header to TempFile
    WriteMODHeader(TempFile);

    for i := 1 to OldNumPages do begin
      // walk through the old file and copy all pages that are NOT selected
      Found := false;
      j := 0;
      while (not Found) and (j < LstMOD.Items.Count) do begin
        Idx := VVal(Items[j].Caption);  // file index of current item
        if (Items[j].Selected) and (i = Idx) then Found := true;
        Inc(j);
      end;
//      ReadMODPage(MODFile);
      if not Found then begin
        ReadMODPage(MODFile);
        WriteMODPage(TempFile);  // Page may be copied, was not selected
      end else
        ReadMODPage(MODFile);
    end;

    // LstMOD.ClearSelection;       // get rid of our list before closing the file
    LstMOD.Clear;

    // finished with TempFile, now erase original file, copy back TempFile
    ReWrite(MODFile);             // Clear our original file
    Seek(TempFile, 0);
    Seek(MODFile, 0);
    while not Eof(TempFile) do begin
      // copy TempFile to MODFile
      BlockRead(TempFile, TempBuf, SizeOf(TempBuf), ByteCount);
      BlockWrite(MODFile, TempBuf, ByteCount, Rslt);
    end;
    CloseFile(TempFile);
    Erase(TempFile);              // Remove TempFile
    ReadModFile;                  // re-read MOD file
    FrmROMHandler.PnlStatus.Caption := 'Done';
  end;
end;


procedure TFrmROMHandler.BtnInsMODClick(Sender: TObject);
// Insert an empty page before the selected page
// Only one page may be selected, order should be in file order!
var
  i, j, Idx, OldNumPages: integer;
  TempFileNm: string;
  TempFile: bfile;
  Found: boolean;
begin
  {$I-}
  Seek(MODFile, 0);              // go to start of file
  {$I+}
  if IOResult <> 0 then begin
    MessageDlg('No MODFILE open', mtWarning, [mbOK], 0);
    Exit;
  end;

  if MODNumPages >254 then begin
    // this prevents adding pages to a backupfile with >255 pages!
    MessageDlg('Too many images', mtWarning, [mbOK], 0);
    Exit;
  end;

  if MODCategory = CATEGORY_M2KBACKUP then begin
    // this prevents adding pages to a backupfile with >255 pages!
    MessageDlg('Adding pages to backup not allowed', mtWarning, [mbOK], 0);
    Exit;
  end;

  if (LstMOD.Selected = nil) or (LstMOD.SelCount <> 1) then
    // none selected or multiple selected
    Exit
  else with LstMOD do begin
    // Create new temp file for saving changed file
    TempFileNm := ChangeFileExt(OpenMODFileNm, '.TMP');
    AssignFile(TempFile, TempFileNm);
    FileMode := 2;            // Open for read and write
    ReWrite(TempFile);        // Clear Temp file

    ReadMODFileHeader(MODFile);
    OldNumPages := MODNumPages;
    MODNumPages := MODNumPages + SelCount;  // new value

    // Copy MOD Header to TempFile
    WriteMODHeader(TempFile);

    for i := 0 to OldNumPages - 1 do begin
      // walk through the old file and copy all pages that are NOT selected
      Idx := i;   // this is the current page number, chick if this is selected
      Found := false;
      j := 0;
      while (not Found) and (j < LstMOD.Items.Count) do begin
        if (Items[j].Selected) and (j = Idx) then Found := true;
        Inc(j);
      end;
      if Found then begin
        // Selected page found, now create a new (empty) page
        InitMODVarsPage;
        WriteMODPage(TempFile);
      end;

      // and copy the original page
      ReadMODPage(MODFile);
      WriteMODPage(TempFile);
    end;

    // finished with TempFile, now erase original file, copy back TempFile

    ReWrite(MODFile);             // Erase our original file
    Seek(TempFile, 0);
    ReadMODFileHeader(TempFile);  // copy header
    WriteMODHeader(MODFile);
    for i := 0 to MODNumPages - 1 do begin
      // Copy pages
      ReadMODPage(TempFile);
      WriteMODPage(MODFile);
    end;
    CloseFile(TempFile);
    Erase(TempFile);
    ReadModFile;                  // re-read MODFile

  end;
end;


procedure TFrmROMHandler.Extract2ROMfileClick(Sender: TObject);
// Extracts .ROM file from selected page
// ROM name will be used as filename, if file exists, this will be prompted for
var
  i, Idx, Idxx, Cnt, NumRead: integer;
  ls: TListItem;
  RFile: bfile;
  FPath, RName, NewName: string;
  NoSave: boolean;
begin
  if (LstMOD.Selected <> nil) then with LstMOD do begin
    FPath := ExtractFilePath(OpenMODFileNm);        // Path of current MOD file
    Cnt := Items.Count;        // number of items in the list
    if Cnt > 0 then for Idxx := 0 to Cnt - 1 do begin
      // go through all items
      ls := Items[Idxx];
      if ls.Selected then begin
        // item is selected, now do real work
        Idx := VVal(ls.Caption) - 1;           // get index in file
        Seek(MODFile, MODHeaderSize + Idx * MODFilePageSize);
        ReadMODPage(MODFile);
        MODCurrent := Idx + 1;
        DisplayMODFilePage;
        NoSave := false;
        // We now have the page, prepare the file
        if MODName <>'' then begin
          // there is a known name, make sure it is valid
          NewName := MakeValidFileName(MODName);
          RName := FPath + MODName + '.rom';
          if FileExists(RName) then begin
            S := 'File ' + MODName + '.rom Exists, Overwrite?';
            if MessageDlg(S, mtConfirmation, [mbYES, mbNO], 0) = mrNO then NoSave := true;
          end else if (NewName <> MODName) then begin
            S := 'Filename ' + MODName + ' change to ' + NewName + '.rom, Create?';
            if MessageDlg(S, mtConfirmation, [mbYES, mbNO], 0) = mrNO then
              NoSave := true
            else
              RName := FPath + NewName + '.rom';
          end;
        end else begin
          // the ROM has no valid name, ask for it in Save Dialog
          DlgSave.Filter := 'ROM File|*.rom';
          DlgSave.FileName := ExtractFileName(OpenMODFileNm);
          DlgSave.FileName := ChangeFileExt(DlgSave.FileName, '.rom');
          DlgSave.DefaultExt := 'rom';
          if DlgSave.Execute then
            RName := DlgSave.Filename
          else
            NoSave := true;
        end;

        if not NoSave then begin
          // all is OK, now create and save the ROM file
          for i := 0 to $0FFF do PageArray[i] := MODArray[i];
          AssignFile(RFile, RName);
          Rewrite(RFile);
          BlockWrite(RFile, PageArray, SizeOf(PageArray), NumRead);
          CloseFile(RFile);
          if NumRead <> SizeOf(PageArray) then
            MessageDlg('Error saving file', mtWarning, [mbOK], 0);
        end;
      end;
    end;
  end;
end;


procedure TFrmROMHandler.ExtractToROMHandlerClick(Sender: TObject);
var
  Idx, i: integer;
  ls: TListItem;
begin
  if (LstMOD.Selected = nil) then Exit;          // No node selected
  with LstMOD.Selected do begin
    Idx := VVal(LstMOD.Items[LstMOD.Selected.Index].Caption) - 1;
    Seek(MODFile, MODHeaderSize + Idx * MODFilePageSize);
    ReadMODPage(MODFile);
    MODCurrent := Idx + 1;
    DisplayMODFilePage;
    for i := 0 to $0FFF do PageArray[i] := MODArray[i];
//    if LstMOD.SelCount > 1 then begin
//      //if more pages are selected, copy the 2nd to the 8K block
//      { TODO : GetNextItem not supported in Lazarus }
//      // ls := LstMOD.GetNextItem(LstMOD.Selected, sdAll, [isSelected]);
//      Idx := VVal(ls.Caption) - 1;
//      Seek(MODFile, MODHeaderSize + Idx * MODFilePageSize);
//      ReadMODPage(MODFile);
//      MODCurrent := Idx + 1;
//      DisplayMODFilePage;
//      for i := 0 to $0FFF do Page[i + $1000] := swap(MODArray[i]);
//    end;
  end;
end;


procedure TFrmROMHandler.ExtractSR(Sender: TObject; SRSet: integer);
var
  Idx, i, BaseSR: integer;
begin
  if (LstMOD.Selected = nil) then Exit;          // No node selected
  with LstMOD.Selected do begin
    Idx := VVal(LstMOD.Items[LstMOD.Selected.Index].Caption) - 1;
    Seek(MODFile, MODHeaderSize + Idx * MODFilePageSize);
    ReadMODPage(MODFile);
    MODCurrent := Idx + 1;
    DisplayMODFilePage;
    // if MODPage = $8F then begin
    if true then begin
      // this is an SR backup set
      BaseSR := SRSet * $40;
      for i := 0 to Range_SR do
        SRArray[i] := MOD2WordArray(MODArray[BaseSR + 2 * i], MODArray[BaseSR + 2 * i + 1]);
      UpdateSR;
      UpDateTreeView(Sender);
    end;
  end;
end;


procedure TFrmROMHandler.ExtractSRFileClick(Sender: TObject);
// Extract all SR's to 4 SR files, ask for name, append SR number from backup file
// Extracts .ROM file from selected page
// ROM name will be used as filename, if file exists, this will be prompted for
var
  i, j, Idx: integer;
  ls: TListItem;
  RFile: Textfile;
  FPath, RName, SRName, S: string;
  NoSave: boolean;
begin
  if (LstMOD.Selected <> nil) then with LstMOD do begin
    ls := Selected;
    FPath := ExtractFilePath(OpenMODFileNm);        // Path of current MOD file

    // get index in file
    Idx := VVal(ls.Caption) - 1;
    Seek(MODFile, MODHeaderSize + Idx * MODFilePageSize);
    ReadMODPage(MODFile);
    MODCurrent := Idx + 1;
    DisplayMODFilePage;
    NoSave := false;
    // We now have the page, prepare the file
    if MODName <>'' then begin
      // there is a known name
      RName := FPath + MODName + '_sr0.sr';
      if FileExists(RName) then begin
        S := 'File ' + MODName + '.sr Exists, Overwrite?';
        if MessageDlg(S, mtConfirmation, [mbYES, mbNO], 0) = mrNO then NoSave := true;
      end;
    end else begin
      // the ROM has no valid name, ask for it in Save Dialog
      DlgSave.Filter := 'SR File|*.sr';
      DlgSave.FileName := ExtractFileName(OpenMODFileNm) + '_sr0';
      DlgSave.FileName := ChangeFileExt(DlgSave.FileName, '.sr');
      DlgSave.DefaultExt := 'sr';
      if DlgSave.Execute then
        RName := DlgSave.Filename
      else
        NoSave := true;
    end;

    i := Pos('_sr0.sr', RName);
    if i = 0 then
      // must fix filename, should end with _sr0
      RName := ExtractFilePath(RName) + ExtractFileName(RName + '_sr0.sr');
    i := Pos('_sr0.sr', RName);
    Delete(RName, i, 20);     // and delete this part

    if not NoSave then begin
      // all is OK, now create and save the ROM file
      for j := 0 to 3 do begin
        for i := 0 to Range_SR do SRArray[i] := swap(MODArray[(j * $40) + i]);
        SRName := RName + '_sr' + Hex1(j) + '.sr';
        AssignFile(RFile, SRName);
        Rewrite(RFile);
        for i := 0 to SRSize do begin
          S := '  -- ' + SRComment[i];
          WriteLn(RFile, Hex3(SRArray[i]) + S);
        end;
        CloseFile(RFile);
      end;
    end;
  end;
end;

procedure TFrmROMHandler.Extract_SR0Click(Sender: TObject);
begin
  ExtractSR(Sender, 0);
end;


procedure TFrmROMHandler.Extract_SR1Click(Sender: TObject);
begin
  ExtractSR(Sender, 1);
end;


procedure TFrmROMHandler.Extract_SR2Click(Sender: TObject);
begin
  ExtractSR(Sender, 2);
end;


procedure TFrmROMHandler.Extract_SR3Click(Sender: TObject);
begin
  ExtractSR(Sender, 3);
end;


procedure TFrmROMHandler.ExtractToROMHandler8KClick(Sender: TObject);
var
  Idx, i: integer;
begin
  if (LstMOD.Selected = nil) then Exit;          // No node selected
  with LstMOD.Selected do begin
    Idx := VVal(LstMOD.Items[LstMOD.Selected.Index].Caption) - 1;
    Seek(MODFile, MODHeaderSize + Idx * MODFilePageSize);
    ReadMODPage(MODFile);
    MODCurrent := Idx + 1;
    DisplayMODFilePage;
    for i := 0 to $0FFF do Page[i + $1000] := swap(MODArray[i]);
  end;
end;


procedure TFrmROMHandler.BtnReReadClick(Sender: TObject);
begin
  ReadModFile;
end;


procedure CloseMODFile;
// Closes the currently open MODFile without saving edits
begin
  {$I-}
  CloseFile(MODFile);
  {$I+}
  OpenROMFileNm := '';
  InitMODVars;
  DisplayMODFileHeader;
  DisplayMODFilePage;
  FrmROMHandler.LstMOD.Clear;
end;


procedure TFrmROMHandler.BtnCloseMODClick(Sender: TObject);
// Closes the MODfile
begin
  BtnSaveEditClick(Sender);
  CloseMODFile;
end;


function TFrmROMHandler.OpenMODFile(FileNm: String; ChangeTab: boolean): integer;
//  FileNm is the file name, we know it exists
//  ChangeTab indicates if the active tab should be changed
//  This procedure parses the .MOD file and displays the info
var
  BakFileNm: string;
  BakFile: bfile;
  FSize, Rslt: integer;
  SizeOK: boolean;
  Attr: integer;
begin
  // open .MOD file, after closing any open MODfile
  CloseMODFile;

  if IOResult <> 0 then Null;
  {$I+}

  // Open MODFile
  Attr := FileGetAttr(FileNm);
  if ((Attr and faReadOnly) <> 0) then begin
    // File is read only, canot support this on Vista
    MessageDlg('File is read-only, cannot open', mtWarning, [mbOK], 0);
    Exit;
  end;

  AssignFile(MODFile, FileNm);
  FileMode := 2;  // Open for read and write
  Reset(MODFile);
  OpenMODFileNm := FileNm;

  // make backup copy of MOD File
  MODFileBacked := false;
  BakFileNm := ChangeFileExt(FileNm, '.bak');
  //  FileCopy(FileNm, BakFileNm);
  FileMode := 2;
  AssignFile(BakFile, BakFileNm);
  ReWrite(BakFile);
  while not Eof(MODFile) do begin
    BlockRead(MODFile, MODImage, 1000, Rslt);
    BlockWrite(BakFile, MODImage, Rslt);
  end;
  CloseFile(BakFile);
  MODFileBacked := true;

  InitMODVars;

  // Switch to MOD Handler TAB and show results
  with FrmROMHandler do begin
    if ChangeTab then begin
      PageControl.ActivePage := FrmROMHandler.PageControl.Pages[3];
      StaticText1.Caption := 'ROM #';
      Mode := ModF;
      OpenROMFileNm := FileNm;
    end;

    FSize := FileSize(MODFile);
    FSize := FSize - MODHeaderSize;
    SizeOK := ((FSize MOD MODFilePageSize) = 0);

    if SizeOK then ReadMODFile;

    if (MODFileFormat <> 'MOD1') or (not SizeOK) then begin
      // Format is wrong, we do not know this file
      MessageDlg('Wrong File Format', mtWarning, [mbOK], 0);
      CloseFile(MODFile);
      OpenROMFileNm := '';
      Result := 0;
      InitMODVars;
      DisplayMODFileHeader;
      DisplayMODFilePage;
      LstMOD.Clear;
      Exit;
    end;
    DisplayMODFileHeader;
    Seek(MODFile, MODHeaderSize);
    if MODNumPages <> 0 then begin
      ReadMODPage(MODFile);
      DisplayMODFilePage;
      MODCurrent := 1;
      GroupBox2.Caption := 'Module Characteristics - Page 1 of ' + SStr(MODNumPages);
    end else
      ClearMODFilePage;
  end;
  OpenModFile := 1;
end;


procedure UploadMOD;
// Starts the (automatic) upload of the selected ROM pages in MOD mode
// SR's are not uploaded, must be done through the SR handler
// When in manual mode, only one ROM (the first selected) will be uploaded
//
//    ChkAuto       - Controls Auto/Manual mode (in combination with 1st radio)
//                    when Manual is selected, RadMODUpload is ignored!
//    RadMODUpload  - Manual/Auto from button bar (in combination with ChkAuto)
//                    will upload selected ROMs to memory type indicated
//                  - Auto from MOD contents, automatic, will place RAM pages in SRAM
//                    other ROMs according to ChkAuto settings
//                  - Auto from MOD backup, use for selective restoring of ROMs
//                    uses Custom Hdr fields as written during backup

var
  i, Idx, Idxx, NumPages, Rslt: integer;
  ls: TListItem;
  S: string;
  AbortLoad, StopLoad, SkipLoad, MemFull: boolean;
  Tp: MemType;
  LastFreeFLASH, LastFreeSRAM: integer;
  LoadAdr, Adr: longword;


  procedure FindNextFreeROM(Typ: MemType);
  // Finds the next free page in FLASH or SRAM
  // Result is stored in LastFreeFLASH or LastFreeSRAM
  // if this is < 0, then no free space is found!
  var
    i, NumPages, RomNum, StartPage: integer;
    EmptyFlash: boolean;
    StartAddr: longword;
  begin
    NumPages := 254;
    StartPage := LastFreeFLASH;
    if Typ = SRAM then begin
      NumPages := SRAM_Pages;
      StartPage := LastFreeSRAM;
    end;
    if StartPage < 0 then Exit;
    RomNum := StartPage;
    repeat
      StartAddr := RomNum * (Range_4k + 1);
      MLDL_ReadBlock(Typ, StartAddr, 16);
      EmptyFlash := true;
      for i := 0 to 15 do begin
        if Typ = FLASH then
          EmptyFlash := EmptyFlash and (WordArray[i] = $FFFF)
        else
          EmptyFlash := EmptyFlash and ((WordArray[i] and $FC00) <> $0000);
      end;
      if not EmptyFlash then Inc(RomNum);
      if RomNum > NumPages then RomNum := -1;
    until EmptyFlash or (RomNum < 0);
    if Typ = SRAM then
      LastFreeSRAM := RomNum
    else
      LastFreeFLASH := RomNum;
  end; // of FindNextFreeROM

begin
  StopLoad := false;
  AbortLoad := false;
  LastFreeFLASH := 0;
  LastFreeSRAM := 0;

  if (FrmROMHandler.LstMOD.Selected <> nil) then with FrmROMHandler.LstMOD do begin
    DisableMLDL;
    FrmROMHandler.PnlStatus.Caption := 'Programming ...';
    Application.ProcessMessages;

    FrmROMHandler.GaugeProgress.Position := 0;

    // Determine number of Items in the list
    NumPages := Items.Count;
    FrmROMHandler.GaugeProgress.Max := NumPages;

    if NumPages > 0 then for Idxx := 0 to NumPages - 1 do begin
      ls := Items[Idxx];
      FrmROMHandler.GaugeProgress.StepIt;
      Application.ProcessMessages;

      if ls.Selected then begin
        // now we can really start, we have a selected item
        SkipLoad := false;
        // get index in file
        Idx := VVal(ls.Caption) - 1;
        Seek(MODFile, MODHeaderSize + Idx * MODFilePageSize);
        ReadMODPage(MODFile);             // read the page
        MODCurrent := Idx + 1;
        DisplayMODFilePage;               // work out the contents

        if (MODPageCustom[0] = $A5) and ((MODPageCustom[1] and $08) = $08) then
          // check for SR, then we skip this page
          SkipLoad := true;

        if not (SkipLoad or StopLoad or AbortLoad) then with FrmROMHandler do begin
          // we now have relevant information, so do the upload here!
          // determine first where the image has to go
          if ChkAuto.Checked then begin
            // Autoload pages
            case RadMODUpload.ItemIndex of
              0,
              1: begin
                   // 0: Manual/Auto from button bar:
                   //    will upload selected ROMs to ChkAuto settings indicated
                   // 1: Auto from MOD contents:
                   //    will place RAM pages in SRAM, other ROMs according to ChkAuto
                   //    this is basically the same as the first option, only this
                   //    choice forces RAM (MODRAM in the Header = 1) pages to go in SRAM

                   if (RadioFLASH.Checked and (RadMODUpload.ItemIndex = 0)) or
                      ((RadMODUpload.ItemIndex = 1) and (MODRAM = 0)) then begin
                     // We are loading to FLASH memory
                     Tp := FLASH;
                     MemFull := false;
                     repeat
                       FindNextFreeROM(Tp);
                       if LastFreeFLASH < 0 then
                         MemFull := true
                       else
                         // Check if FLASH is really erased
                         EmptyCheckPage(Tp, LastFreeFLASH, false, Rslt, Adr);
                     until MemFull or (Rslt = 0) ;
                     if (Rslt = 0) and (not MemFull) then begin
                       PnlStatus.Caption := 'Uploading FLASH #' + Int3(LastFreeFLASH);
                       Application.ProcessMessages;
                       LoadAdr := LastFreeFLASH * (Range_4k + 1);
                     end else begin
                       StopLoad := true;
                       MessageDlg('No more free FLASH', mtWarning, [mbOk], 0);
                     end;
                   end else begin
                     // Must load to SRAM
                     Tp := SRAM;
                     FindNextFreeROM(Tp);
                     if LastFreeSRAM < 0 then begin
                       StopLoad := true;
                       MessageDlg('No more free SRAM', mtWarning, [mbOk], 0);
                     end else begin
                       PnlStatus.Caption := 'Uploading SRAM #' + Int3(LastFreeSRAM);
                       Application.ProcessMessages;
                       LoadAdr := LastFreeSRAM * (Range_4k + 1);
                     end;
                   end;
                 end;

              2: begin
                   // Auto from MOD backup, use for selective restoring of ROMs
                   // uses Custom Hdr fields as written during backup
                   // if there is no valid info this Page will be skipped
                   LoadAdr := MODPageCustom[2] * (Range_4k + 1);
                   PnlStatus.Caption := 'Uploading ROM #' + Int3(LastFreeFLASH);
                   if (MODPageCustom[0] = $A5) and (MODPageCustom[1] = $06) then
                     Tp := FLASH
                   else if (MODPageCustom[0] = $A5) and (MODPageCustom[1] = $05) then
                     Tp := SRAM
                   else begin
                     // this is not an MLDL2000 backup or SR pages, so abort
                     AbortLoad := true;
                     MessageDlg('This is not an MLDL2000 backup', mtWarning, [mbOk], 0);
                   end;
                   if not AbortLoad then
                     PnlStatus.Caption := 'Uploading ROM #' + Int3(MODPageCustom[2]);
                 end
              else begin
                // invalid option
                MessageDlg('Invalid option', mtWarning, [mbOk], 0);
                AbortLoad := true;
              end;
            end;    // of the case statement

          end else begin
            // Manual load page, load only first page!
            StopLoad := true;                  // will prevent any further uploads
            S := EdtROMAddress.Text;
            LoadAdr := HexToInt(S);
            RomNum := LoadAdr shr 12;
            if RadioFLASH.Checked then
              Tp := FLASH
            else
              Tp := SRAM;
            if Tp = FLASH then begin
              PnlStatus.Caption := 'FLASH EMPTY CHECK';
              EmptyCheckPage(Tp, RomNum, true, Rslt, Adr);
              PnlStatus.Caption := 'Uploading ROM ...';
              if Rslt <> 0 then begin
                // FLASH Page was NOT empty!
                MessageDlg('FLASH is not empty', mtWarning, [mbOk], 0);
                AbortLoad := true;
              end;
            end;
          end;

          // Figured out location, done emptycheck, now do the upload!
          if not (AbortLoad or SkipLoad) then begin
            for i := 0 to Range_4k do WordArray[i] := swap(MODArray[i]);
            MLDL_WriteBlock(Tp, LoadAdr, (Range_4k + 1));
          end;
          AbortLoad := StopLoad;
          SkipLoad := false;
          if AbortLoad then Break;  // get out of for ... loop
        end;
      end;
    end;

  end;
  EnableMLDL;
  FrmROMHandler.GaugeProgress.Position := 0;
  FrmROMHandler.GaugeProgress.Max := 100;
end;

procedure Import_RAW(var FileNm: string; ChangeTab: boolean);
// import .RAW user code file
// will overwrite the current ROM page from $x100
var
  RAWFile: bfile;
  W: word;
  i: integer;
  Ch: byte;
begin
  AssignFile(RAWFile, FileNm);
  FileMode := 0;
  Reset(RAWFile);

  i := $100;
  { TODO : must check this Read, why? }
  // Read(RAWFile);
  while (not Eof(RAWFile)) and (i < $0EF0) do begin
    Read(RAWFile, Ch);
    W := Ch;
    PageArray[i] := swap(W);
    Inc(i);
  end;

  CloseFile(RAWFile);
  FrmROMHandler.PageControl.ActivePage := FrmROMHandler.PageControl.Pages[0];
  FrmROMHandler.StaticText1.Caption := 'ROM #';
  Mode := ROM;
  OpenROMFileNm := FileNm;
  FrmROMHandler.MemoROM.Lines.Add('');
  FrmROMHandler.MemoROM.Lines.Add('ROM Filename    : ' + OpenROMFileNm);
  DisplayROM;

end;


procedure Import_i41CX(var FileNm: string; ChangeTab: boolean);
// imports files mailed with i41CX MAILMDO function
// file should be saved as .html, .htm or .txt
// a new .MOD file will be created, any existing .MOD file may be overwritten
// a dialog will confirm this
var
  ModFileNm, BakFileNm, ModFileNmNm: string;
  BakFile: bfile;
  i41CXFile: bfile;
  FSize, Rslt: integer;
  SizeOK: boolean;
  Attr, Len, i, ChNum, ChRes: integer;
  Ch: byte;
  OutBuf: array[0..2] of byte;     // output buffer
  StartPos: longint;
  EndFound: boolean;
  HEPRAM: boolean;
const
  SrchString = 'i41CX://post?saveModule';
  SrchString2 = 'i41CX://post?saveHEPAXRAM';

begin
  // first find the targetstring in the file
  HEPRAM := false;
  StartPos := ScanFile(FileNm, SrchString, false);
  if StartPos < 0 then begin
    StartPos := ScanFile(FileNm, SrchString2, false);
    if StartPos < 0 then begin
      MessageDlg('i41CX embedded MOD or HEPAX file not found', mtWarning, [mbOK], 0);
      Exit;
    end else HEPRAM := true;
  end;

  StartPos := StartPos + Length(SrchString);
  // open the file and extract MOD filename
  AssignFile(i41CXFile, FileNm);
  Reset(i41CXFile);
  Seek(i41CXFile, StartPos + 1);
  // now read until we find a # character, end of MOD name

  ModFileNm := 'i41_';
  Read(i41CXFile, Ch);
  while Ch <> Ord('#') do begin
    ModFileNm := ModFileNm + Char(Ch);
    Read(i41CXFile, Ch);
    Inc(StartPos);
  end;

  if HEPRAM then begin
    {$I-} CloseFile(ROMFile);
    if IOResult <> 0 then Null;
    {$I+}

  end else begin
    // Close any open MODFile
    {$I-}
    CloseFile(MODFile);
    if IOResult <> 0 then Null;
    {$I+}
    ModFileNm := ChangeFileExt(ModFileNm, '.mod');

    // and create a new empty MOD file
    FrmROMHandler.DlgSave.Filter := 'MOD File|*.mod';
    FrmROMHandler.DlgSave.FileName := ModFileNm;
    FrmROMHandler.DlgSave.DefaultExt := 'mod';
    if FrmROMHandler.DlgSave.Execute then begin
      ModFileNm := FrmROMHandler.DlgSave.Filename;
      if CreateMODFile(ModFileNm) > 0 then
        FrmROMHandler.PnlFileName.Caption := ExtractFileName(ModFileNm)
      else begin
        FrmROMHandler.PnlFileName.Caption := 'Error saving file';
        CloseFile(i41CXFile);
        Exit;
      end;
    end;
  end;

  // now start the real work, decode the Base64 encoded MOD file
  // ignore CR and LF, the closing quote " stops the process

  Len := 0;      // to keep track of byte count
  i := 0;        // counts blocks of 4 input characters
  EndFound := false;

  while not eof(i41CXFile) and not EndFound do begin
    Read(i41CXFile, Ch);                               // read one char from file
    EndFound := (Ch = Ord('"'));
    if (Ch >= 43) and (Ch < 123) and (Base64Decode[Ch] >= 0) and not EndFound then begin
      // number is in the right range and valid
      ChRes := Base64Decode[Ch];
      if (ChRes = 64) or (Ch = 61) then begin
        // handle case of =, this is a filler
        // check if there is a second = sign
        Read(i41CXFile, Ch);
        if (Ch = 61) then begin
          // second = found
          Write(MODFile, OutBuf[0]);
          EndFound := true;                     // force exit
        end else begin
          Write(MODFile, OutBuf[0], OutBuf[1]);
          EndFound := true;                     // force exit
        end;
      end else begin
        // normal valid character, so handle
        case i of
          0: begin
               OutBuf[0] := (ChRes shl 2) and $00FF;
             end;
          1: begin
               OutBuf[0] := (OutBuf[0] + (ChRes shr 4)) and $00FF;
               OutBuf[1] := (ChRes shl 4) and $00FF;
             end;
          2: begin
               OutBuf[1] := (OutBuf[1] + (ChRes shr 2)) and $00FF;
               OutBuf[2] := (ChRes shl 6) and $00FF;
             end;
          3: begin
               // this is the last char of the group of 4, write to file
               OutBuf[2] := (OutBuf[2] + ChRes) and $00FF;
               Write(MODFile, OutBuf[0], OutBuf[1], OutBuf[2]);
             end;
        end;  // case i of  ...
        Inc(i);
        Len := Len + 1;
        if i > 3 then i := 0;
      end;
    end;
  end;

  // new MODfile is now ready, so close the original file
  CloseFile(i41CXFile);
  InitMODVars;

  FileNm := MODFileNm;


  // Switch to MOD Handler TAB and show results
  with FrmROMHandler do begin
    if ChangeTab then begin
      PageControl.ActivePage := FrmROMHandler.PageControl.Pages[3];
      StaticText1.Caption := 'ROM #';
      Mode := ModF;
      OpenROMFileNm := FileNm;
    end;

    FSize := FileSize(MODFile);
    FSize := FSize - MODHeaderSize;
    SizeOK := ((FSize MOD MODFilePageSize) = 0);

    if SizeOK then ReadMODFile;

    if (MODFileFormat <> 'MOD1') or (not SizeOK) then begin
      // Format is wrong, we do not know this file
      MessageDlg('Wrong File Format', mtWarning, [mbOK], 0);
      CloseFile(MODFile);
      OpenROMFileNm := '';
      // Result := 0;
      InitMODVars;
      DisplayMODFileHeader;
      DisplayMODFilePage;
      LstMOD.Clear;
      Exit;
    end;
    DisplayMODFileHeader;
    Seek(MODFile, MODHeaderSize);
    if MODNumPages <> 0 then begin
      ReadMODPage(MODFile);
      DisplayMODFilePage;
      MODCurrent := 1;
      GroupBox2.Caption := 'Module Characteristics - Page 1 of ' + SStr(MODNumPages);
    end else
      ClearMODFilePage;
  end;

end;


function OpenRomFile(FileNm: string): integer;
//Open ROM or SR file
const
  RAMImagePagesSz = 5120;
var
  Extension: string;
  Line, HEXLine: string;
  NewFName: string;
  NumRead, i, j, Position: integer;
  IntVal : integer;
  Val: word;
  SRFile, HEXFile: TextFile;
  RAMFile, NewRomFile: file of byte;
  Found, Rslt : boolean;
  RAMImage : array[0..RAMImagePagesSz - 1] of byte;  // ROM image in .RAM format
  FSize, FPages: integer;
  SizeOK: boolean;
  XtraBytes: integer;
  FirstByte: byte;
begin
  NumRead := 0;
  FileName := FileNm;
  if FileExists(FileNm) then begin
    Extension := UpperCase(ExtractFileExt(FileNm));
    if Extension = '.ROM' then begin
      // open .ROM file
      AssignFile(ROMFile, FileNm);
      FileMode := 0;
      Reset(ROMFile);
      BlockRead(ROMFile, PageArray, $2000, NumRead);
      CloseFile(ROMFile);
      FrmROMHandler.PageControl.ActivePage := FrmROMHandler.PageControl.Pages[0];
      FrmROMHandler.StaticText1.Caption := 'ROM #';
      Mode := ROM;
      OpenROMFileNm := FileNm;
      FrmROMHandler.MemoROM.Lines.Add('');
      FrmROMHandler.MemoROM.Lines.Add('ROM Filename    : ' + OpenROMFileNm);
      DisplayROM;
    end else if Extension = '.ASC' then begin
       // open .ASC file
      AssignFile(HEXFile, FileNm);
      FileMode := 0;
      Reset(HEXFile);
      for i := 0 to ROM4kSize do
        PageArray[i] := $000;
      i := 0;
      Readln(HEXFile, Line);
      while (not Eof(HEXFile)) and (i < ROMSize) do begin
        Val := HexToInt(Line);
        PageArray[i] := swap(Val);
        i := i + 1;
        Readln(HEXFile, Line);
      end;
      CloseFile(HEXFile);
      FrmROMHandler.PageControl.ActivePage := FrmROMHandler.PageControl.Pages[0];
      FrmROMHandler.StaticText1.Caption := 'ROM #';
      Mode := ROM;
      OpenROMFileNm := FileNm;
      FrmROMHandler.MemoROM.Lines.Add('');
      FrmROMHandler.MemoROM.Lines.Add('ROM Filename    : ' + OpenROMFileNm);
      DisplayROM;

    end else if Extension = '.RAM' then begin
      // open .RAM file, generated by saving the RAM pages in an DM41X
      // file format is multiple pages in .BIN format (same as internal in a MOD file)
      // maximum is 8 pages of 5120 bytes each
      // the .RAM files is split in parts into a ROM file with the same name and the
      // number appended to it with an underscore: _n.rom in the same directory
      // results are listed in the status window and the first image is opened as
      // ROM file
      AssignFile(RAMFile, FileNm);
      FileMode := 0;      // ReadOnly
      Reset(RAMFile);
      i := 0;

      // first check filesize, must be multiple of 5120, max 8 pages
      FSize  := FileSize(RAMFile);
      FPages := FSize DIV RAMImagePagesSz;
      SizeOK := ((FSize MOD RAMImagePagesSz) = 0);

      // Active ROM tab for messages
      FrmROMHandler.PageControl.ActivePage := FrmROMHandler.PageControl.Pages[0];
      FrmROMHandler.MemoROM.Lines.Add('');
      FrmROMHandler.MemoROM.Lines.Add('Reading RAM File: ' + FileNm);
      FrmROMHandler.MemoROM.Lines.Add('       File size: ' + SStr(FSize) + 'bytes');
      FrmROMHandler.MemoROM.Lines.Add('       RAM pages: ' + SStr(FPages));

      if SizeOK then
        FrmROMHandler.MemoROM.Lines.Add('  File Size OK, start reading')
      else
        FrmROMHandler.MemoROM.Lines.Add('  File may be corrupted, will only read complete RAM images');

      for i :=0 to (FPages - 1) do begin
        // now read the RAM images, convert and store as .ROM
        for j:= 0 to (RAMImagePagesSz -1) do RAMImage[j] := $00;  // clear array
        BlockRead(RAMFile, RAMImage, RAMImagePagesSz, NumRead);

        // decode contents and store in currentPageArray
        for j := 0 to ($1000 div 4) - 1 do begin
            PageArray[j*4]   := swapw(((RAMImage[j*5+1] and $03) shl 8) or RAMImage[j*5]);
            PageArray[j*4+1] := swapw(((RAMImage[j*5+2] and $0F) shl 6) or
                                    ((RAMImage[j*5+1] and $FC) shr 2));
            PageArray[j*4+2] := swapw(((RAMImage[j*5+3] and $3F) shl 4) or
                                    ((RAMImage[j*5+2] and $F0) shr 4));
            PageArray[j*4+3] := swapw(((RAMImage[j*5+4]) shl 2) or
                                    ((RAMImage[j*5+3] and $C0) shr 6));
        end;
        // and save this ROM page as filename_n.ROM

        NewFName := FileNm;
        Delete (NewFName, length (NewFName) - 3, 4);  // delete last 4 chars from FileNm

        NewFName := NewFName + '_' + SStr(i) + '.rom';

        FrmROMHandler.MemoROM.Lines.Add('  writing file: ' + NewFname);
        AssignFile(NewRomFile, NewFName);
        Rewrite(NewRomFile);
        BlockWrite(NewRomFile, PageArray, $2000, NumRead);
        CloseFile(NewRomFile);

        // and disply the characteristics of the saved .ROM file
        DisplayRom;
      end;

      // all ROM images are now saved, close the .RAM file and open
      // the first ROM image

      CloseFile(RAMFile);

      NewFName := FileNm;
      Delete (NewFName, length (NewFName) - 3, 4);  // delete last 4 chars from FileNm

      FileNm := NewFName + '_0' + '.rom';

      OpenROMFileNm := FileNm;
      AssignFile(ROMFile, OpenROMFileNm);
      FileMode := 0;
      Reset(ROMFile);
      BlockRead(ROMFile, PageArray, SizeOf(PageArray), NumRead);
      CloseFile(ROMFile);
      FrmROMHandler.PageControl.ActivePage := FrmROMHandler.PageControl.Pages[0];
      FrmROMHandler.StaticText1.Caption := 'ROM #';
      Mode := ROM;
      OpenROMFileNm := FileNm;
      FrmROMHandler.MemoROM.Lines.Add('');
      FrmROMHandler.MemoROM.Lines.Add('ROM Filename    : ' + OpenROMFileNm);
      DisplayROM;

    end else if Extension = '.RMA' then begin
      // open. RMA file, renamed .RAM files from i41CX HEPAX mailed RAM file
      // remove first byte, them it is a byte swapped ROM file
      // there may be multiple ROM images, so handle like .RAM file from DM41X

      AssignFile(RAMFile, FileNm);
      FileMode := 0;      // ReadOnly
      Reset(RAMFile);
      i := 0;

      // first check filesize, must be multiple of 5120, max 8 pages
      FSize  := FileSize(RAMFile);
      FPages := FSize DIV $2000;
      XtraBytes := FSize MOD $2000;
      SizeOK := (FPages = Xtrabytes);

      // Active ROM tab for messages
      FrmROMHandler.PageControl.ActivePage := FrmROMHandler.PageControl.Pages[0];
      FrmROMHandler.MemoROM.Lines.Add('');
      FrmROMHandler.MemoROM.Lines.Add('Reading RMA File: ' + FileNm);
      FrmROMHandler.MemoROM.Lines.Add('       File size: ' + SStr(FSize) + 'bytes');
      FrmROMHandler.MemoROM.Lines.Add('       RAM pages: ' + SStr(FPages));

      if SizeOK then
        FrmROMHandler.MemoROM.Lines.Add('  File Size OK, start reading')
      else
        FrmROMHandler.MemoROM.Lines.Add('  File may be corrupted, will only read complete RAM images');

      for i :=0 to (FPages - 1) do begin
        // now read the RAM images, convert and store as .ROM
        // first read one byte
        BlockRead(RAMFile, FirstByte, 1, j);  // read filler byte
        BlockRead(RAMFile, PageArray, $2000, NumRead);

        // decode contents and store in currentPageArray
        for j := 0 to $0FFF do PageArray[j] := SwapW(PageArray[j]);

        // and save this ROM page as filename_n.ROM
        NewFName := FileNm;
        Delete (NewFName, length (NewFName) - 3, 4);  // delete last 4 chars from FileNm

        NewFName := NewFName + '_' + SStr(i) + '.rom';

        FrmROMHandler.MemoROM.Lines.Add('  writing file: ' + NewFname);
        AssignFile(NewRomFile, NewFName);
        Rewrite(NewRomFile);
        BlockWrite(NewRomFile, PageArray, $2000, NumRead);
        CloseFile(NewRomFile);

        // and disply the characteristics of the saved .ROM file
        DisplayRom;
      end;

      // all ROM images are now saved, close the .RAM file and open
      // the first ROM image

      CloseFile(RAMFile);

      NewFName := FileNm;
      Delete (NewFName, length (NewFName) - 3, 4);  // delete last 4 chars from FileNm

      FileNm := NewFName + '_0' + '.rom';

      OpenROMFileNm := FileNm;
      AssignFile(ROMFile, OpenROMFileNm);
      FileMode := 0;
      Reset(ROMFile);
      BlockRead(ROMFile, PageArray, SizeOf(PageArray), NumRead);
      CloseFile(ROMFile);
      FrmROMHandler.PageControl.ActivePage := FrmROMHandler.PageControl.Pages[0];
      FrmROMHandler.StaticText1.Caption := 'ROM #';
      Mode := ROM;
      OpenROMFileNm := FileNm;
      FrmROMHandler.MemoROM.Lines.Add('');
      FrmROMHandler.MemoROM.Lines.Add('ROM Filename    : ' + OpenROMFileNm);
      DisplayROM;

             
    end else if Extension = '.SR' then begin
      // open .SR file, this is a textfile with possible comments
      AssignFile(SRFile, FileNm);
      FileMode := 0;
      Reset(SRFile);
      i := 0;
      while (not Eof(SRFile)) and (i <= SRSize) do begin
        // read and parse one line of the SR File
        Readln(SRFile, Line);
        if (Length(Line) > 0) and (Line[1] in HexSet) then begin
          // valid hex character, parse line
          IntVal := HexToInt(Line);
          if IntVal >= 0 then begin
            SRArray[i] := IntVal;
            // read comment, first find "--"
            Position := Pos('--', Line);
            if Position = 0 then
              SRComment[i] := ''
            else
              SRComment[i] := MidStr(Line, Position + 3, 100);
            i := i + 1;
          end;
        end;
      end;

      // copy current SR into SRArrays, current selected item
      SRArrays[FrmROMHandler.ComSRPick.ItemIndex] := SRArray;
      
      CloseFile(SRFile);
      NumRead := i + 1;
      FrmROMHandler.PageControl.ActivePage := FrmROMHandler.PageControl.Pages[1];
//    FrmROMHandler.StaticText1.Caption := 'SR #';
      Mode := SR;
      OpenSRFileNm := FileName;
      UpdateSR;
    // line below was to fix issues with .MOD files on some systems, now removed
    // end else if (Extension = '.MOD') or (Extension = '.MDO') then begin
    end else if (Extension = '.MOD') then begin
      NumRead := FrmROMHandler.OpenModFile(FileNm, true);
    end else if ((Extension = '.HTML') or (Extension = '.HTM') or (Extension = '.TXT')) then begin
      Import_i41CX(FileNm, true);
      NumRead := FrmROMHandler.OpenModFile(FileNm, true);
    end else if (Extension = '.RAW') then begin
      Import_RAW(FileNm, true);
      NumRead := 1;
    end else if Extension = '.HEX' then begin

      //     Code for reading Clonix generated .HEX file, by Diego Diaz
      //    '***************************************************
      //    '* Converter for .HEX RAMDumps from NoVRAM modules
      //    '*
      //    '*  Into .ROM file.
      //    '***************************************************
      //
      //    '*************** USER INPUT: SOURCE & DESTINATION FILES
      //
      //            CLS
      //            LOCATE 12, 24
      //            INPUT "Enter .HEX source file:"; Sour$
      //            LOCATE 15, 24
      //            INPUT "Enter .ROM destination file:"; Dest$
      //
      //    '**************** OPEN BOTH FILES
      //
      //            OPEN Sour$ + ".HEX" FOR INPUT AS #1
      //            OPEN Dest$ + ".ROM" FOR OUTPUT AS #2
      //
      //    '****** SEEKS SOURCE FILE FOR FIRST LINE AT ADDRESS H'2000
      //
      //    Seek2000:
      //            LINE INPUT #1, IncData$
      //            IF LEFT$(IncData$, 9) <> ":10200000" THEN GOTO Seek2000
      //            GOSUB ROMWrite          '**** READS FIRST LINE DATA AND
      //                                    '**** WRITES IT INTO DESTINATION FILE
      //
      //    '***** LOOP READS THE REMAINING 511 LINES (512*16=8.192bytes)
      //
      //            FOR LinCount% = 1 TO 511
      //            LINE INPUT #1, IncData$
      //            GOSUB ROMWrite
      //            NEXT
      //
      //    '********* JOB DONE!, CLOSE FILES AND QUIT
      //            CLOSE
      //            END
      //
      //    '****** CONVERTS ASCII TEXT LINE IN .HEX FILE INTO BINARY DATA
      //    '****** AND WRITES IT INTO .ROM FILE
      //
      //    ROMWrite:
      //            Dat8$ = MID$(IncData$, 10, 41)
      //            FOR DatCount% = 1 TO 32 STEP 2
      //            Data$ = CHR$(VAL('&H' + MID$(Dat8$, DatCount%, 2)))
      //            PRINT #2, Data$;
      //            NEXT DatCount%
      //
      //            RETURN
      //
      //    '************ END OF CODE ************************

      AssignFile(HEXFile, FileNm);
      FileMode := 0;
      Reset(HEXFile);
      repeat
        ReadLn(HEXFile, HEXLine);
        Found := (Pos(':10200000', HEXLine) <> 0);
      until Eof(HEXFile) or Found;

      if Found then begin
        // correct entry found, decode first line
        NumRead := 1;
        for i := 0 to 511 do begin
          Delete(HEXLine, 1, 9);  // get rid of the first part of the string
          for j := 0 to 7 do
            PageArray[i * 8 + j] := HexToByte(HEXLine, Rslt) +
                                    HexToByte(HEXLine, Rslt) * 256;
          ReadLn(HEXFile, HEXLine);
        end;
        DisplayRom;
      end else MessageDlg('Wrong HEX file', mtWarning, [mbOK], 0);
    end else begin
      MessageDlg('Wrong Extension', mtWarning, [mbOK], 0);
    end;
  end else begin
    MessageDlg('File Nonexistent', mtWarning, [mbOK], 0);
  end;
  OpenRomFile := NumRead;
end;


procedure FillSRArray;
// Walk through SR Tree and Fill SRArray;
var
  Pg, Bk : integer;
  Node, CNode : TTreeNode;
begin
  Node := FrmROMHandler.TreeViewSR.Items.GetFirstNode;
  while Node <> nil do begin
    CNode := Node.GetFirstChild;
    while CNode <> nil do begin
      with CNode do begin
        Pg := TSetReg(Data).Page;
        Bk := TSetReg(Data).Bank;
        SRArray[Pg*4+Bk] := TSetReg(Data).Value;
        SRComment[Pg*4+Bk] := TSetReg(Data).Comment;
      end;
      CNode := CNode.GetNextSibling;
    end;
    Node := Node.GetNextSibling;
  end;
end;


function SaveRomFile(FileNm: string): integer;
// Save ROM or SR file
var
  S: string;
  NumRead, i : integer;
  SRFile: TextFile;
  FileExt: string;
begin
  NumRead := 0;
  FileName := FileNm;
  if Mode = SR then begin  // Save SR file with comments
    // First walk through TreeView and put data in Array
    FillSRArray;
    FileNm := ChangeFileExt(FileNm, '.sr');
    AssignFile(SRFile, FileNm);
    Rewrite(SRFile);
    for i := 0 to SRSize do begin
       S := '  -- ' + SRComment[i];
       WriteLn(SRFile, Hex3(SRArray[i]) + S);
    end;
    CloseFile(SRFile);
  end else if Mode = ROM then begin            // Save ROM file
    FileExt := ExtractFileExt(FileNm);
    if FileExt = '.rom' then begin
      FileNm := ChangeFileExt(FileNm, '.rom');
      AssignFile(ROMFile, FileNm);
      Rewrite(ROMFile);
      BlockWrite(ROMFile, PageArray, $2000, NumRead);
      CloseFile(RomFile);
    end else
      NumRead := 0;                    // Wrong extension
  end;
  SaveRomFile := NumRead;
end;


procedure MLDL_Restore(Restore: boolean; UseOpenFile: boolean);
// Restore or Verify a backupfile
// Restore - true when really doing the restore
//         - false when doing a verify
var
  i, RomNum: integer;
  FileNm, S, MemTp : string;
  Tp: MemType;
  StartAdr: longword;
  ContBak, ContBaks, ErrFound: boolean;
  ErrAdr: longword;
  ErrRslt: integer;
  W1, W2: word;
begin

  // Open MOD file, First close any open MODFile

  ErrFound := false;

  if not UseOpenFile then begin
    {$I-}
    CloseFile(MODFile);
    if IOResult <> 0 then Null;
    {$I+}

    // Open backup file
    FrmROMHandler.DlgOpen.Filter := 'MOD File|*.mod';
    FrmROMHandler.DlgOpen.DefaultExt := 'mod';

    if FrmROMHandler.DlgOpen.Execute then begin
      FileNm := FrmROMHandler.DlgOpen.Filename;
      if FrmROMHandler.OpenMODFile(FileNm, false) > 0 then
        FrmROMHandler.PnlFileName.Caption := ExtractFileName(FileNm)
      else begin
        FrmROMHandler.PnlFileName.Caption := 'Error opening file';
        Exit;
      end;
    end else
      Exit;

    ErrFound := false;
  end;

  if Restore then
    if MessageDlg('Restoring Backup will overwrite MLDL Contents',
                   mtConfirmation, [mbYes, mbNo], 0) = mrNo  then Exit;

  // File is now open, check contents
  if (MODCategory <> CATEGORY_M2KBACKUP) or
     (MODHardware <> HARDWARE_MLDL2000) or
     (MODHeaderCustom[0] <> $A5)  then begin
    MessageDlg('Not a valid backup file', mtConfirmation, [mbOK], 0);
    Exit;
  end;

  FrmROMHandler.MemoROM.Lines.Add('');
  FrmROMHandler.MemoROM.Lines.Add(MODComments);
  FrmROMHandler.MemoROM.Lines.Add('------------------------------------');

  FrmROMHandler.GaugeProgress.Max := MODNumPages;
  FrmROMHandler.GaugeProgress.Position := 0;

  RomNum := 0;
  DisableMLDL;

  // Start the restore
  while RomNum < MODNumPages do with FrmROMHandler do begin

    // Read Page from MOD file
    Seek(MODFile, MODHeaderSize + RomNum * MODFilePageSize);
    ReadMODPage(MODFile);
    DisplayMODFilePage;
    GaugeProgress.StepIt;

    // Check parameters and value of MODPageCustom:
    //  Byte 0: $A5               code to recognize Backup
    //  Byte 1: $01  SRAM         bit 1 or 2 always set
    //          $02  FLASH
    //          $04  ROM          bit 3 or 4 always set
    //          $08  SR
    //  Byte 2: Page number
    //          $00 when SR
    //          $FF when FLASH Page was found empty when creating the backup
    ContBak := true;

    S := '';

    if MODPageCustom[0] <> $A5 then begin
      // magic code for backups does not match
      ContBak := false;
      S := S + ' Skip page, Custom Header[0] <> $A5';
    end;

    if (MODPageCustom[1] and $02) = $02 then begin
      Tp := FLASH;
      MemTp := 'FLASH';
    end else begin
      Tp := SRAM;
      MemTp := 'SRAM ';
    end;

    S := MemTp + ' ';
    if (MODPageCustom[1] and $08) = $08 then
      S := S + 'Settings Register  '
    else
      S := S + ' ROM #' + Int3(MODPageCustom[2]) + '  ';

    ContBaks := true;
    if (MODPageCustom[1] = $05) and (not ChkIncSramROM.Checked ) then ContBaks := false;
    if (MODPageCustom[1] = $09) and (not ChkIncSramSR.Checked  ) then ContBaks := false;
    if (MODPageCustom[1] = $06) and (not ChkIncFlashROM.Checked) then ContBaks := false;
    if (MODPageCustom[1] = $0A) and (not ChkIncFlashSR.Checked ) then ContBaks := false;

    if not ContBaks then begin
      ContBak := false;
      S := S + 'Skip due to user settings';
    end;

    if ContBak and ((MODPageCustom[1] and $04) = $04) and (MODPageCustom[2] = $FF) then begin
      // do not restore empty ROM (FLASH or SRAM)
      ContBak := false;
      if Restore then
        S := S + 'Skip page, restore empty FLASH or SRAM manually'
      else
        S := S + 'Skip page, cannot verify empty FLASH or SRAM';
    end;

    if ((MODPageCustom[1] and $02) = $02) and ContBak and Restore then begin
      // we are restoring to FLASH, check if page is empty
      if (MODPageCustom[1] and $04) = $04 then begin
        // this is ROM, do an emptpycheck
        EmptyCheckPage(Tp, MODPageCustom[2], false, ErrRslt, ErrAdr);
        if ErrRslt <> 0 then begin
          ContBak := false;
          S := S + 'Skip page, FLASH not erased @ $' + Hex5(ErrAdr);
        end;
      end else if (MODPageCustom[1] and $08) = $08 then begin
        // this is SR, do an emptycheck
        EmptyCheckSR(Tp, MODPageCustom[2], false, ErrRslt, ErrAdr);
        if ErrRslt <> 0 then begin
          ContBak := false;
          S := S + 'Skip SR, FLASH not erased @ $' + Hex5(ErrAdr);
        end;
      end;
    end;

    if ContBak then begin
      // we can now start the actual restore process
      if (MODPageCustom[1] and $04) = $04 then begin
        // this is ROM, write it back
        for i := 0 to Range_4k do WordArray[i] := swap(MODArray[i]);
        StartAdr := MODPageCustom[2] * (Range_4k + 1);
        S := S + '  "' + MODName + '"';
        ExpandSpaces(S, 40);
        S := S + MODID + '  ';
        if Restore then
          MLDL_WriteBlock(Tp, StartAdr, (Range_4k + 1))
        else begin
          // do the verify here!
          MLDL_ReadBlock(Tp, StartAdr, (Range_4k + 1));
          i := 0;
          while (i < $1000) and (WordArray[i] = swap(MODArray[i])) do
            Inc(i);
          if i < $1000 then begin
            // Verify Error
            S := S + ' Verify Error @ $' + Hex4(i);
            ErrFound := true;
          end else
            S := S + ' Verify OK';
        end;
      end else begin
        // Restore SR
        // for i := 0 to $FF do WordArray[i] := swap(MODArray[i]);

        for i := 0 to $FF do
          WordArray[i] := MOD2WordArray(MODArray[2 * i], MODArray[2 * i + 1]);


        if Restore then
          MLDL_WriteBlock(Tp, SR0_Base, $100)
        else begin
          // do the verify here!
          MLDL_ReadBlock(Tp, SR0_Base, $100);
          i := 0;
          while (WordArray[i] = MOD2WordArray(MODArray[2 * i], MODArray[2 * i + 1])) and (i < $100) do
            Inc(i);
          if i < $100 then begin
            // Verify Error
            S := S + ' Verify Error @ $' + Hex4(i);
            ErrFound := true;
          end else
            S := S + ' Verify OK';
        end;
      end;
      if Restore then S := S + '  Restored';
    end;
    MemoROM.Lines.Add(S);
    Inc(RomNum);
  end;
  FrmROMHandler.GaugeProgress.Max := 100;
  FrmROMHandler.GaugeProgress.Position := 0;
  EnableMLDL;
  if ErrFound then MessageDlg('Verify Errors', mtWarning, [mbOK], 0);
end;


procedure MLDL_Backup;
var
  NumImages, i, RomNum, NumROMs: integer;
  FileNm, S, MemTp : string;
  StartAddr: longword;
  EmptyFlash: boolean;
  MemoryType: MemType;
begin
  // Create MOD file, First close any open MODFile
  {$I-}
  CloseFile(MODFile);
  if IOResult <> 0 then Null;
  {$I+}
  FrmROMHandler.DlgSave.Filter := 'MOD File|*.mod';
  FrmROMHandler.DlgSave.FileName := OpenMODFileNm;
  FrmROMHandler.DlgSave.DefaultExt := 'mod';
  if FrmROMHandler.DlgSave.Execute then begin
    FileNm := FrmROMHandler.DlgSave.Filename;
    if CreateMODFile(FileNm) > 0 then
      FrmROMHandler.PnlFileName.Caption := ExtractFileName(FileNm);
  end else begin
    FrmROMHandler.PnlFileName.Caption := 'Error creating file';
    Exit;
  end;

  // File is created, now fill initial header and write it, will do this again later ??
  MODFileFormat := 'MOD1';
  MODCategory := CATEGORY_M2KBACKUP;
  MODHardware := HARDWARE_MLDL2000;
  MODComments := 'MLDL2000 Backup, s/n ' + MLDL_Serial + '  ' + DateToStr(Now) + '/' + TimeToStr(Now);
  MODTitle := MODComments;
  MODNumPages := 0;
  NumROMS := 0;

  WriteMODHeader(MODFile);

  FrmROMHandler.MemoROM.Lines.Add('');
  FrmROMHandler.MemoROM.Lines.Add(MODComments);
  FrmROMHandler.MemoROM.Lines.Add('------------------------------------');

  DisableMLDL;

  for MemoryType := FLASH to SRAM do with FrmROMHandler do begin
    NumImages := 0;
    if (MemoryType = FLASH) and ChkIncFlashROM.Checked then NumImages := ROMImages_FLASH - 1;
    if (MemoryType = SRAM ) and ChkIncSRAMROM.Checked  then NumImages := SRAM_Pages -1;

    i := 2;
    if ChkIncFlashROM.Checked then i := i + ROMImages_FLASH - 1;
    if ChkIncSRAMROM.Checked then i := i + SRAM_Pages -1;
    
    GaugeProgress.Max := i;

    if NumImages <> 0 then for RomNum := 0 to NumImages do begin

      MemTp := 'SRAM ';
      if MemoryType = FLASH then MemTp := 'FLASH';

      GaugeProgress.StepIt;
      Application.ProcessMessages;

      S := 'Backup ' + MemTp + ' ROM #' + Int3(RomNum) + '  ';
      StartAddr := RomNum * (Range_4k + 1);
      MLDL_ReadBlock(MemoryType, StartAddr, 16);

      // Check if FLASH or SRAM is empty
      EmptyFlash := (MemoryType = FLASH);
      for i := 0 to 15 do begin
        if MemoryType = FLASH then
          EmptyFlash := EmptyFlash and (WordArray[i] = $FFFF)
        else
          EmptyFlash := EmptyFlash or ((WordArray[i] and $FC00) <> $0000)
      end;

      if EmptyFlash and ChkBakValid.Checked then begin
        // skip this page
        S := S + 'Empty, will skip';
      end else begin
        // can start with backup
        MLDL_ReadBlock(MemoryType, StartAddr, $1000);       // Download ROM
        // First copy data from ROM to MODArray and compress
        for i := 0 to $0FFF do MODArray[i] := WordArray[i];
        CompressBin;
        if EmptyFlash then MODFAT := 0 else MODFAT := 1;
        DisplayMODFilePage;

        if EmptyFlash then begin
          S := S + 'Empty, saving anyway';
          MODName := 'EMPTY PAGE';
          MODID   := 'n/a';
        end else begin
          S := S + 'Saving ';
          MODName := LblName.Caption;
          MODID   := LBLRev.Caption;
          S := S + '"' + MODName + '"';
          ExpandSpaces(S, 51);
          S := S + MODID;
        end;

        // MemoROM.Lines.Add(S);

        // Definition of MODPageCustom:
        //  Byte 0: $A5               code to recognize Backup
        //  Byte 1: $01  SRAM         bit 1 or 2 always set
        //          $02  FLASH
        //          $04  ROM          bit 3 or 4 always set
        //          $08  SR
        //  Byte 2: Page number
        //          $00 when SR
        //          $FF when this was an empty page (FLASH only)
        MODPageCustom[0] := $A5;
        if MemoryType = SRAM then
          MODPageCustom[1] := $05
        else
          MODPageCustom[1] := $06;
        if EmptyFlash then
          MODPageCustom[2] := $FF
        else
          MODPageCustom[2] := Byte(RomNum);

        Inc(NumROMs);                            // Will write header at the end

        Seek(MODFile, FileSize(MODFile));        // go to end of File
        WriteMODPage(MODFile);                   // Write new info
      end;  // of saving single ROM
      MemoROM.Lines.Add(S);
    end;    // going through the NumImages

    // now ready to save the SR
    ClearMODFilePage;

    if (ChkIncFlashSR.Checked and (MemoryType = FLASH)) or
       (ChkIncSRAMSR.Checked) and (MemoryType = SRAM) then begin
      // checkboxes for the SR are checked

      if MemoryType = SRAM then
        MemTp := 'SRAM '
      else
        MemTp := 'FLASH';
      
      MemoROM.Lines.Add('Saving ' + MemTp + ' Settings Registers');
      MLDL_ReadBlock(MemoryType, SR0_Base, $100);

      // modified for 12-bit SR's
      // the SR's are now all in WordArray, we need to split these
      for i := 0 to $FF do begin
        MODArray[2 * i]     :=  WordArray[i] and $00FF;
        MODArray[2 * i + 1] := (WordArray[i] shr 8) and $00FF;
      end;

      for i := $200 to $0FFF do MODArray[i] := $0000;

      CompressBin;
      MODName := MemTp + ' SR';
      MODPage := $8F;
      MODID   := 'n/a';
      MODFAT  := 0;

      MODPageCustom[0] := $A5;
      if MemoryType = SRAM then
        MODPageCustom[1] := $09
      else
        MODPageCustom[1] := $0A;
      MODPageCustom[2] := $00;

      Inc(NumROMs);
      Seek(MODFile, FileSize(MODFile));           // go to end of File
      WriteMODPage(MODFile);                      // Write new info
    end;   // of handling SR

  end;  // of main backup loop

  // last task is to write the Header

  MODNumPagesCustom := NumROMs;
  if NumROMs > 255 then
    MODNumPages := 255
  else
    MODNumPages := NumROMs;

  MODHeaderCustom[0] := $A5;
  MODHeaderCustom[1] := MODNumPagesCustom div $100;
  MODHeaderCustom[2] := MODNumPagesCustom mod $100;
  Seek(MODFile, 0);                           // go to start of file
  WriteMODHeader(MODFile);                    // update header with NumPages

  ReadMODFile;
  FrmROMHandler.GaugeProgress.Position := 0;
  FrmROMHandler.GaugeProgress.Max := 100;
  EnableMLDL;
end;


procedure TFrmROMHandler.BtnBackupClick(Sender: TObject);
// Backup MLDL2000
begin
  MLDL_Backup;
  if ChkBakVerify.Checked then MLDL_Restore(false, true);
end;


procedure TFrmROMHandler.BtnRestoreClick(Sender: TObject);
// Restore to MLDL2000
begin
  MLDL_Restore(true, false);
end;


procedure TFrmROMHandler.BtnBakVerifyClick(Sender: TObject);
// Verify backup against MLDL2000
begin
  MLDL_Restore(false, false);
end;


procedure TFrmROMHandler.BtnOpenClick(Sender: TObject);
var
  FileNm : string;
begin
  FrmROMHandler.DlgSave.Filter := 'ROM / SR / MOD file format|*.rom;*.sr;*.mod;*.hex|Settings Register format|*.sr|ROM format|*.rom|MOD format|*.mod|HEX format (Clonix)|*.hex';
  FrmROMHandler.DlgSave.FileName := OpenMODFileNm;
  FrmROMHandler.DlgSave.DefaultExt := 'rom';
  if DlgOpen.Execute then begin
    FileNm := DlgOpen.Filename;
    if OpenRomFile(FileNm) > 0 then
      PnlFileName.Caption := ExtractFileName(FileNm)
    else
      PnlFileName.Caption := 'Error opening file';
  end;
end;


procedure TFrmROMHandler.MnMainMConnectClick(Sender: TObject);
begin
  InitMLDL;
end;

procedure TFrmROMHandler.MnMainMDisconnectClick(Sender: TObject);
begin
  CloseMLDL;
end;

procedure SetFlashType;
// prepares LstFLASHErase
var
  DeviceID: word;
  TypeName: String;
  i: integer;
begin
  with FrmROMHandler.LstFLASHErase.Items do begin
    DeviceID := 0;
    if FLASH_tp = TOPBOOT then begin
      TypeName := 'Top Boot Block';
      Strings[ 0] := '00000-07FFF  SECTOR 00   ROM  00- 07';
      Strings[ 1] := '08000-0FFFF  SECTOR 01   ROM  08- 15';
      Strings[ 2] := '10000-17FFF  SECTOR 02   ROM  16- 23';
      Strings[ 3] := '18000-1FFFF  SECTOR 03   ROM  24- 31';
      Strings[ 4] := '20000-27FFF  SECTOR 04   ROM  32- 39';
      Strings[ 5] := '28000-2FFFF  SECTOR 05   ROM  40- 47';
      Strings[ 6] := '30000-37FFF  SECTOR 06   ROM  48- 55';
      Strings[ 7] := '38000-3FFFF  SECTOR 07   ROM  56- 63';
      Strings[ 8] := '40000-47FFF  SECTOR 08   ROM  64- 71';
      Strings[ 9] := '48000-4FFFF  SECTOR 09   ROM  72- 79';
      Strings[10] := '50000-57FFF  SECTOR 10   ROM  80- 87';
      Strings[11] := '58000-5FFFF  SECTOR 11   ROM  88- 95';
      Strings[12] := '60000-67FFF  SECTOR 12   ROM  96-103';
      Strings[13] := '68000-6FFFF  SECTOR 13   ROM 104-111';
      Strings[14] := '70000-77FFF  SECTOR 14   ROM 112-119';
      Strings[15] := '78000-7FFFF  SECTOR 15   ROM 120-127';
      Strings[16] := '80000-87FFF  SECTOR 16   ROM 129-135';
      Strings[17] := '88000-8FFFF  SECTOR 17   ROM 136-143';
      Strings[18] := '90000-97FFF  SECTOR 18   ROM 144-151';
      Strings[19] := '98000-9FFFF  SECTOR 19   ROM 152-159';
      Strings[20] := 'A0000-A7FFF  SECTOR 20   ROM 160-167';
      Strings[21] := 'A8000-AFFFF  SECTOR 21   ROM 168-175';
      Strings[22] := 'B0000-B7FFF  SECTOR 22   ROM 176-183';
      Strings[23] := 'B8000-BFFFF  SECTOR 23   ROM 184-191';
      Strings[24] := 'C0000-C7FFF  SECTOR 24   ROM 192-199';
      Strings[25] := 'C8000-CFFFF  SECTOR 25   ROM 200-207';
      Strings[26] := 'D0000-D7FFF  SECTOR 26   ROM 208-215';
      Strings[27] := 'D8000-DFFFF  SECTOR 27   ROM 216-223';
      Strings[28] := 'E0000-E7FFF  SECTOR 28   ROM 224-231';
      Strings[29] := 'E8000-EFFFF  SECTOR 29   ROM 232-239';
      Strings[30] := 'F0000-F7FFF  SECTOR 30   ROM 240-247';
      Strings[31] := 'F8000-FBFFF  SECTOR 31   ROM 248-251';
      Strings[32] := 'FC000-FCFFF  SECTOR 32   ROM 252';
      Strings[33] := 'FD000-FDFFF  SECTOR 33   ROM 253';
      Strings[34] := 'FE000-FFFFF  SECTOR 34   ROM 254 + SR''s';

  end else if FLASH_tp = BOTTOMBOOT then begin
      TypeName := 'Bottom Boot Block';
      Strings[ 0] := '00000-01FFF  SECTOR 00   ROM  00- 01';
      Strings[ 1] := '02000-02FFF  SECTOR 01   ROM  02';
      Strings[ 2] := '03000-03FFF  SECTOR 02   ROM  03';
      Strings[ 3] := '04000-07FFF  SECTOR 03   ROM  04- 07';
      Strings[ 4] := '08000-0FFFF  SECTOR 04   ROM  08- 15';
      Strings[ 5] := '10000-17FFF  SECTOR 05   ROM  16- 23';
      Strings[ 6] := '18000-1FFFF  SECTOR 06   ROM  24- 31';
      Strings[ 7] := '20000-27FFF  SECTOR 07   ROM  32- 39';
      Strings[ 8] := '28000-2FFFF  SECTOR 08   ROM  40- 47';
      Strings[ 9] := '30000-37FFF  SECTOR 09   ROM  48- 55';
      Strings[10] := '38000-3FFFF  SECTOR 10   ROM  56- 63';
      Strings[11] := '40000-47FFF  SECTOR 11   ROM  64- 71';
      Strings[12] := '48000-4FFFF  SECTOR 12   ROM  72- 79';
      Strings[13] := '50000-57FFF  SECTOR 13   ROM  80- 87';
      Strings[14] := '58000-5FFFF  SECTOR 14   ROM  88- 95';
      Strings[15] := '60000-67FFF  SECTOR 15   ROM  96-103';
      Strings[16] := '68000-6FFFF  SECTOR 16   ROM 104-111';
      Strings[17] := '70000-77FFF  SECTOR 17   ROM 112-119';
      Strings[18] := '78000-7FFFF  SECTOR 18   ROM 120-127';
      Strings[19] := '80000-87FFF  SECTOR 19   ROM 128-135';
      Strings[20] := '88000-8FFFF  SECTOR 20   ROM 136-143';
      Strings[21] := '90000-97FFF  SECTOR 21   ROM 144-151';
      Strings[22] := '98000-9FFFF  SECTOR 22   ROM 152-159';
      Strings[23] := 'A0000-A7FFF  SECTOR 23   ROM 160-167';
      Strings[24] := 'A8000-AFFFF  SECTOR 24   ROM 168-175';
      Strings[25] := 'B0000-B7FFF  SECTOR 25   ROM 176-183';
      Strings[26] := 'B8000-BFFFF  SECTOR 26   ROM 184-191';
      Strings[27] := 'C0000-C7FFF  SECTOR 27   ROM 192-199';
      Strings[28] := 'C8000-CFFFF  SECTOR 28   ROM 200-207';
      Strings[29] := 'D0000-D7FFF  SECTOR 29   ROM 208-215';
      Strings[30] := 'D8000-DFFFF  SECTOR 30   ROM 216-223';
      Strings[31] := 'E0000-E7FFF  SECTOR 31   ROM 224-231';
      Strings[32] := 'E8000-EFFFF  SECTOR 32   ROM 232-239';
      Strings[33] := 'F0000-F7FFF  SECTOR 33   ROM 240-247';
      Strings[34] := 'F8000-FFFFF  SECTOR 34   ROM 248-254 + SR''s';

    end else begin
      TypeName := 'Error in Device Type';
      for i := 0 to 34 do Strings[ 0] := 'Error';

    end;
    FrmROMHandler.LblType.Caption := 'Type: '+ Hex4(DeviceID) + ' ' + TypeName;

  end;
end;


procedure SetSRAMType;
var
  S : string;
begin
  if SRAM_Sz = SRAMUNKNOWN then S := 'SRAM Size: UNKNOWN';
  if SRAM_Sz = DOUBLE then S := 'DOUBLE SRAM Size';
  if SRAM_Sz = REGULAR then S := 'SINGLE SRAM Size';
  FrmROMHandler.LblSRAMType.Caption := S;
end;


procedure TFrmROMHandler.ReadFlashType;
var
  FlashTp  : word;
begin
  DisableMLDL;
  // Determine FLASH Memory Type
  FLASH_tp := FLASHUNKNOWN;
  MLDL_FLASHReadID(FlashTp);
  if (FlashTp = FlashTypeTop) then begin
    FLASH_TP := TOPBOOT
  end else if (FlashTp = FlashTypeBottom) then begin
    FLASH_TP := BOTTOMBOOT;
  end;
  EnableMLDL;
  SetFlashType;
end;


procedure TFrmROMHandler.ReadSRAMType;
begin
  DisableMLDL;
  SRAM_Sz := SRAMUNKNOWN;
  ReadSRAMTyp;
  EnableMLDL;
  SetSRAMType;
end;


procedure TFrmROMHandler.PageControlChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  PageControlChange(Sender);
end;


procedure TFrmROMHandler.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePage = ROMSheet then begin
    PnlStatus.Caption := 'ROM';
    PnlFileName.Caption := ExtractFileName(OpenROMFileNm);
    StaticText1.Caption := 'ROM #';
    Mode := ROM;
    UpDnNumber.Max := SaveUpDnMax;
    EdtROMNum.Text := SStr(ROMNum);
    EdtROMAddress.Text := '$' + Hex5(ROMNum*(Range_4k + 1));
    ChkAuto.Enabled := MLDL_Is_Connected;
  end else if PageControl.ActivePage = SRSheet then begin
    PnlStatus.Caption := 'SR';
    PnlFileName.Caption := ExtractFileName(OpenSRFileNm);
    Mode := SR;
    ChkAuto.Enabled := MLDL_Is_Connected;
  end else if PageControl.ActivePage = FLASHSheet then begin       // FLASHSheet
    PnlStatus.Caption := 'FLASH';
    PnlFileName.Caption := '';
    StaticText1.Caption := 'ROM #';
    Mode := FL;
    UpDnNumber.Max := SaveUpDnMax;
    EdtROMNum.Text := SStr(ROMNum);
    EdtROMAddress.Text := '$' + Hex5(ROMNum*(Range_4k + 1));
    // now check FLASH type, only when connected
    if MLDL_Is_Connected then begin
      ReadFlashType;
      ReadSRAMType;
    end;
    ChkAuto.Enabled := MLDL_Is_Connected;
  end else if PageControl.ActivePage = MODSheet then begin
    PnlStatus.Caption := 'MODFile';
    PnlFileName.Caption := ExtractFileName(OpenMODFileNm);
    StaticText1.Caption := 'ROM #';
    Mode := MODF;
    UpDnNumber.Max := SaveUpDnMax;
    EdtROMNum.Text := SStr(ROMNum);
    EdtROMAddress.Text := '$' + Hex5(ROMNum*(Range_4k + 1));
    ChkAuto.Enabled := MLDL_Is_Connected;
    DisplayMODFileHeader;
    DisplayMODFilePage;
  end;
end;



procedure TFrmROMHandler.UpdateTreeView(Sender: TObject);
var
  S1, SS, S2 : String;
  ItemFound: TListItem;
begin
  with FrmROMHandler do begin
    InUpdateTreeView := true;
    if (TreeViewSR.Selected = nil) then Exit;  // No node selected!
    // Find the node in the TreeView that corresponds to this Item
    if TreeViewSR.Selected.Level = 0 then begin
      with TreeViewSR.Selected do begin
        LblHex.Caption := '$xxx';
        LblBin.Caption := '............';
        ChkEnable.Checked    := false;
        ChkEnable.Enabled    := false;
        ChkFlash.Checked     := false;
        ChkFlash.Enabled     := false;
        ChkIO.Checked        := false;
        ChkIO.Enabled        := false;
        ChkWProtect.Checked  := false;
        ChkWProtect.Enabled  := false;
        EdtROMAddr.Text      := '';
        EdtROMNumber.Enabled := false;
        EdtROMNumber.Text    := '';
        EdtComment.Text      := TSetReg(Data).Comment;
        EdtComment.Enabled   := false;
      end;
    end else begin
      with TreeViewSR.Selected do begin

        LblHex.Caption := '$' + Hex3(TSetReg(Data).Value);
        LblBin.Caption := Bin12(TSetReg(Data).Value);
        S1 := TSetReg(Data).Comment;
        S2 := '';
        // Find out ROM Name and properties
        if TSetReg(Data).Flash_SR then
          SS := 'FL-'
        else
          SS := 'SR-';

        if TSetReg(Data).noIO_SR then
          S2 := '  '
        else
          S2 := 'IO';

        if TSetReg(Data).WEnabled_SR then
          S2 := S2 + ' [write enabled]'
        else
          S2 := S2 + ' [write protected]';



        if TSetReg(Data).Enabled_SR then
          S2 := S2 + ' [enabled]'
        else
          S2 := S2 + ' [disabled]';

        SS := SS + Int3(TSetReg(Data).Get_RomNum);

        ItemFound := FindListItem(FrmROMHandler.LstContents, SS, 4, false);
        if ItemFound <> nil then with ItemFound do begin
          SS := SubItems[3];              // SRAM/FLASH ROM Number
          SS := SS + ' / ' + Caption;     // XROM Number
          SS := SS + ' / ' + SubItems[0]; // ROM Name
          SS := SS + ' / ' + SubItems[1]; // Rom Revision
          // and select item
          LstContents.Selected := nil;             // unselect any other items
          LstContents.Selected := ItemFound;
          LstContents.HideSelection := false;
        end else LstContents.Selected := nil;

        if TSetReg(Data).Value = $0FFF  then begin
          SS := 'EMPTY';
          Text := LeftStr(Text, 9) + Hex3(TSetReg(Data).Value) + ']' + ' ' + SS;
        end else begin
          Text := LeftStr(Text, 9) + Hex3(TSetReg(Data).Value) + ']' + ' ' + SS;
          Text := Text + ' / ' + S2 + ' / ' + S1; // Comment is in S1
        end;

        ChkEnable.Enabled := true;
        ChkEnable.Checked := TSetReg(Data).Enabled_SR;

        ChkFlash.Enabled  := true;
        ChkFlash.Checked  := TSetReg(Data).Flash_SR;

        EdtROMNumber.Enabled := true;
        EdtComment.Enabled := true;
        EdtComment.Text := S1;

        ChkIO.Enabled     := true;
        ChkIO.Checked     := not TSetReg(Data).noIO_SR;

        ChkWProtect.Enabled := true;
        ChkWProtect.Checked := not TSetReg(Data).WEnabled_SR;



        {
          if not TSetReg(Data).Flash_SR then begin
            ChkIO.Checked     := not TSetReg(Data).noIO_SR;
            ChkIO.Enabled     := true;
            // if ChkIO.Checked then
            //   ChkWProtect.Caption := 'WRITE PROTECT'
            // else
            //   ChkWProtect.Caption := 'SRAM I/O (else FLASH)';
            ChkWProtect.Checked := not TSetReg(Data).WEnabled_SR;
            // ChkWProtect.Enabled := true;
            EdtROMAddr.Text     := '$' + Hex5(TSetReg(Data).SR_Addr);
            EdtROMNumber.Text   := SStr(TSetReg(Data).SRRomNum);
          end else begin
            ChkIO.Enabled     := false;
            ChkWProtect.Enabled := false;
            ChkWProtect.Caption := 'WRITE PROTECT';
            EdtROMAddr.Text     := '$' + Hex5(TSetReg(Data).FL_Addr);
            EdtROMNumber.Text   := SStr(TSetReg(Data).FLRomNum);
          end;
        }


      end;
    end;
    FillSRArray;
    InUpdateTreeView := false;
  end;
end;


procedure TFrmROMHandler.TreeViewClick(Sender: TObject);
begin
  UpdateTreeView(Sender);
end;


procedure TFrmROMHandler.TreeViewSRChange(Sender: TObject; Node: TTreeNode);
begin
  UpdateTreeView(Sender);
end;


procedure TFrmROMHandler.RadioClick(Sender: TObject);
begin
  if (ChkEnable.Focused) or (ChkFlash.Focused) or
     (ChkIO.Focused) or (ChkWProtect.Focused) then begin
    with FrmROMHandler do begin
      if (TreeViewSR.Selected = nil) then Exit;  // No node selected!
      // Find the node in the TreeView that corresponds to this Item
      if TreeViewSR.Selected.Level = 0 then begin
      // Pnlhint.Caption := 'Level 0  ' + TreeViewSR.Selected.Text;
      end else begin
        with TreeViewSR.Selected do begin
          TSetReg(Data).SetEnabled(ChkEnable.Checked);
          TSetReg(Data).SetFlash(ChkFlash.Checked);
          TSetReg(Data).SetnoIO(not ChkIO.Checked);
          TSetReg(Data).SetWEnabled(not ChkWProtect.Checked);
          if TSetReg(Data).Flash_SR then
            UpDnRomNum.Max := 255
          else
            UpDnRomNum.Max := SRAM_Pages;
        end;
        if not InUpdateTreeView then
          UpdateTreeView(Sender);
      end;
    end;
  end;
end;


procedure TFrmROMHandler.BtnNewClick(Sender: TObject);
// initialze the SR with all bits set to '1'
begin
  NewSRArray;
  UpdateSR;
  UpdateTreeView(Sender);
end;


procedure TFrmROMHandler.BtnClearMemoClick(Sender: TObject);
begin
  FrmROMHandler.MemoROM.Lines.Clear;
end;


procedure TFrmROMHandler.BtnContentsClick(Sender: TObject);
// List MLDL2000 Contents
var
  S, S2: string;
  i, j: integer;
  StartAddr, Adr: LongWord;
  EmptyFlash: boolean;
  RubbishFlash: boolean;
  NonValid: boolean;
  TempArray: array[0..31] of Word;
  TempRom: RomPage;
  RomNum: integer;
  NumImages: integer;
  MemoryType: MemType;
  FunAdr, LabLen, CharPointer, W: Word;
  NewItem: TListItem;
  ROM_XROM, ROM_Name, ROM_Rev, ROM_Fcns, ROM_MemTp, ROM_Num,
  ROM_Chk, ROM_CRC: string;
  Accumulator: integer;
  CRCRes : LongWord;
begin
  FrmROMHandler.PnlStatus.Caption := 'READING MLDL CONTENTS';
  Application.ProcessMessages;

  GaugeProgress.Position := 0;
  GaugeProgress.Max := ROMImages_FLASH + SRAM_Pages;
  FrmROMHandler.Update;
  DisableMLDL;
  LstContents.Clear;

  for MemoryType := FLASH to SRAM do begin
    if MemoryType = FLASH then
      NumImages := ROMImages_FLASH
    else
      NumImages := SRAM_Pages;

    for RomNum := 0 to NumImages - 1 do begin
      // List contents of FLASH, one line at a time
      // ROMS that appears to be empty or invalid are skipped
      StartAddr := RomNum * (Range_4k + 1);

      MLDL_ReadBlock(MemoryType, StartAddr, 16);

      GaugeProgress.StepIt;
      Application.ProcessMessages;

      // Check if FLASH empty if first 15 words are $FFFF
      EmptyFlash := true;
      for i := 0 to 15 do begin
        TempArray[i] := WordArray[i];
        EmptyFlash := EmptyFlash and (TempArray[i] = $FFFF);
      end;

      RubbishFlash := false;
      // Check if FLASH is rubbish if most significant nybble <> 0;
      if not EmptyFlash then for i := 0 to 15 do
        RubbishFlash := RubbishFlash or ((TempArray[i] and $FC00) <> $0000);

      // Create List Item
      ROM_XROM  := '';
      ROM_Name  := '';
      ROM_Rev   := '';
      ROM_Fcns  := '';
      ROM_MemTp := '';
      ROM_Num   := '';
      ROM_Chk   := '';
      ROM_CRC   := '';

      // Display relevant information
      if MemoryType = FLASH then begin
        S := 'FLASH #';
        ROM_MemTp := 'FLASH';
        ROM_Num := 'FL-';
      end else begin
        S := 'SRAM  #';
        ROM_MemTp := 'SRAM ';
        ROM_Num := 'SR-';
      end;

      S := S + Int3(RomNum) + '  ';
      ROM_Num := ROM_Num + Int3(RomNum);

      FrmROMHandler.PnlStatus.Caption := 'READING MLDL ' + ROM_Num;

      if EmptyFlash then begin
        S := S + 'EMPTY';
        ROM_Name := '[EMPTY FL]';
      end else if RubbishFlash then begin
        S := S +'RANDOM DATA ??';
        ROM_Name := '[INVALID]';
      end else begin
        // contents are valid, display
        S := S + 'XROM ' + Int3(TempArray[0]);
        S := S + '  #Fcns ' + Int3(TempArray[1]);

        ROM_XROM := Int3(TempArray[0]);
        ROM_Fcns := Int3(TempArray[1]);

        // Rom Revision
        MLDL_ReadBlock(MemoryType, StartAddr + ROMREV_Addr, 4);

        for i := 3 downto 0 do ROM_Rev := ROM_Rev + HPChar(WordArray[i]);
        S := S + '  ' + ROM_Rev + '  ';;

        // Find out ROM Name (first function in ROM)
        FunAdr := (TempArray[$0002] and $00FF) shl 8;
        FunAdr := FunAdr or (TempArray[$0003] and $00FF);
        NonValid := false;
        S2 := '';

        if (TempArray[$0002] and $0200) = $0200 then begin
          // this is a UserCode Program Name
          // Read ROM contents
          MLDL_ReadBlock(MemoryType, StartAddr + FunAdr, 20);
          for i := 0 to 15 do TempArray[i] := WordArray[i];

          // check if there is a valid Global Label structure
          if (TempArray[0] and $FFF0) <> $01C0 then NonValid := true;
          // Check if this is not the .END. or something else
          if (TempArray[2] and $FFF0) <> $00F0 then NonValid := true;

          LabLen := TempArray[2] and $0000F;

          S2 := S2 + 'LBL "';
          for i := 4 to LabLen + 2 do begin
            W := TempArray[i] and $00FF;
            S2 := S2 + HPUserChar(W);
          end;
          if not NonValid then begin
            S2 := S2 + '" (User Code)';
            ROM_Name := S2;
          end else begin
            S2 := ' No valid ROM Name found';
            ROM_Name := '[NO NAME]';
          end;
            S := S + S2;
        end else begin
          // this is Microcode, normal situation
          if FunAdr > $0FFF then begin
            // Function Name is in other Page
            S2 := ' Function Name in other Page';
            ROM_Name := '[OTHER PG]';
          end else begin
            // normal situation, decode Function Name
            // read ROM contents ahead of Function Address

            if funadr = 0 then
              for i := 0 to 31 do TempArray[i] := 0
            else
              MLDL_ReadBlock(MemoryType, StartAddr + FunAdr - 20, 20);

            for i := 0 to 31 do TempArray[i] := WordArray[i];
            CharPointer := 20;
            repeat
              CharPointer := CharPointer - 1;
              W := TempArray[CharPointer];
              S2 := S2 + HPChar(W);
            until ((W and $0080) <> 0) or (W = $0)
                  or (W > $03FF) or (CharPointer = 0);
            ROM_Name := S2;
            if (W = $0) or (W > $03FF) or (CharPointer = 0) then begin
              ROM_Name := '[NO NAME]';
              S2 := ' No valid ROM Name found';
            end;
          end;
          S := S + S2;
        end;

      end;

      if not (EmptyFlash or RubbishFlash) then begin
        if CheckLongInfo.Checked then begin
          // ROM Checksum abd CRC are requested, so read complete ROM in WordArray
          MLDL_ReadBlock(MemoryType, StartAddr, $1000);

          // Calculate and check ROM Checksum
           Accumulator := 0;
           for i := 0 to Rom4KSize - 1 do begin
             // WordArray[i] := Swap(WordArray[i]);
             Accumulator := Accumulator + WordArray[i];
             if Accumulator > $03FF then begin
               Accumulator := (Accumulator and $03FF) + 1;
             end;
           end;
           Accumulator := (-Accumulator) and $03FF;

           if Accumulator = WordArray[CHECKSUM_Addr] then
             ROM_Chk := Hex3(WordArray[CHECKSUM_Addr]) + '  OK'
           else
             ROM_Chk := Hex3(WordArray[CHECKSUM_Addr]) + ' NOK';

          // Calculate 41CL CRC
          for i := 0 to Rom4KSize do WordArray[i] := Swap(WordArray[i]);
          CRCRes := PageCRC(WordArray);
          ROM_CRC := Hexn(CRCRes, 8);

        end;

      end;

      if not (EmptyFlash or RubbishFlash) then begin
        NewItem := LstContents.Items.Add;
        NewItem.Caption := ROM_XROM;
        NewItem.SubItems.Add(ROM_Name);
        NewItem.SubItems.Add(ROM_Rev);
        NewItem.SubItems.Add(ROM_Fcns);
        NewItem.SubItems.Add(ROM_Num);
        NewItem.SubItems.Add(ROM_Chk);
        NewItem.SubItems.Add(ROM_CRC);
      end;
    end;  // FLASH - SRAM loop
  end;

  // now read in Settings Registers
  Adr := SR0_Base;
  MLDL_ReadBlock(SRAM, Adr , 4 * (Range_SR + 1));
  for i := 0 to 3 do
    for j := 0 to Range_SR do
      SRArrays[i, j] := WordArray[i * (Range_SR + 1) + j];

  // now get FLASH SR's
  Adr := SR0_Base;
  MLDL_ReadBlock(FLASH, Adr , 4 * (Range_SR + 1));
  for i := 0 to 3 do
    for j := 0 to Range_SR do
      SRArrays[i + 4, j] := WordArray[i * (Range_SR + 1) + j];

  // now copy active SR block into SRArray
  SRArray := SRArrays[ComSRPick.ItemIndex];

  FrmROMHandler.PnlStatus.Caption := 'DONE';
  GaugeProgress.Position := 0;
  GaugeProgress.Max := 100;
  UpdateSR;
  Application.ProcessMessages;
  EnableMLDL;
end;


procedure TFrmROMHandler.BtnDisable(Sender: TObject);
// initialze only current SR with all bits set to '1'
begin
  with FrmROMHandler do begin
    if (TreeViewSR.Selected = nil) then Exit;  // No node selected!
    // Find the node in the TreeView that corresponds to this Item
    if TreeViewSR.Selected.Level = 0 then begin
//    Pnlhint.Caption := 'Level 0  ' + TreeViewSR.Selected.Text;
    end else begin
      with TreeViewSR.Selected do
        TSetReg(Data).SetValue($0FFF);
      UpdateTreeView(Sender);
    end;
  end;
end;


procedure TFrmROMHandler.EdtROMNumChange(Sender: TObject);
// Called on change of EdtROMNum
var
  Code: Integer;
begin
  Val(EdtROMNum.Text, ROMNum, Code);
  if RomNum > 254 then begin
    RomNum := 254;
    EdtROMNum.Text := '254';
  end;
  EdtROMAddress.Text := '$' + Hex5(ROMNum*(Range_4k + 1));
end;


procedure TFrmROMHandler.RadioMemClick(Sender: TObject);
// Change selection of target device
var
  I, Code: integer;
begin
  if RadioSRAM.Checked then begin
    MemTp := SRAM;
    PnlMemType.Caption := 'SRAM';
    Val(EdtROMNum.Text, I, Code);
    // if I > 63 then EdtROMNum.Text := '63';
    UpDnNumber.Max := SRAM_Pages - 1;
    SaveUpDnMax := SRAM_Pages - 1;
  end else begin
    MemTp := FLASH;
    PnlMemType.Caption := 'FLASH';
    UpDnNumber.Max := 254;
    SaveUpDnMax := 254;
  end;

  if MLDL_Is_Connected and ChkAuto.Checked then
    ChkAutoClick(Sender);

end;

procedure TFrmROMHandler.RadioSRAMClick(Sender: TObject);
// Change selection of target device
var
  I, Code: integer;
begin
  MemTp := SRAM;
  PnlMemType.Caption := 'SRAM';
  Val(EdtROMNum.Text, I, Code);
  // if I > 63 then EdtROMNum.Text := '63';
  UpDnNumber.Max := SRAM_Pages - 1;
  SaveUpDnMax := SRAM_Pages - 1;

  if MLDL_Is_Connected and ChkAuto.Checked then
    ChkAutoClick(Sender);
end;


procedure TFrmROMHandler.RadioFLASHClick(Sender: TObject);
// Change selection of target device
begin
  MemTp := FLASH;
  PnlMemType.Caption := 'FLASH';
  UpDnNumber.Max := 254;
  SaveUpDnMax := 254;
  if MLDL_Is_Connected and ChkAuto.Checked then
    ChkAutoClick(Sender);
end;


procedure TFrmROMHandler.EdtROMNumberChange(Sender: TObject);
// Called on change of EdtROMNumber
var
  Code, Value: Integer;
begin
  with FrmROMHandler do begin
    if (TreeViewSR.Selected = nil) then Exit;
    if TreeViewSR.Selected.Level = 1 then with TreeViewSR.Selected do begin
      Val(EdtROMNumber.Text, Value, Code);
      if TSetReg(Data).Flash_SR then begin       // SR indicates FLASH
        TSetReg(Data).SetFL_Addr(Value);
        EdtROMAddr.Text := '$' + Hex5(TSetReg(Data).FL_Addr);
      end else begin                             // SR indicates SRAM
        TSetReg(Data).SetSR_Addr(Value);
        EdtROMAddr.Text := '$' + Hex5(TSetReg(Data).SR_Addr);
      end;
      LblHex.Caption := '$' + Hex3(TSetReg(Data).Value);
      LblBin.Caption := Bin12(TSetReg(Data).Value);
      UpdateTreeView(Sender);
    end;
  end;
end;


procedure TFrmROMHandler.EdtCommentChange(Sender: TObject);
// Called on change of Comment
begin
  with FrmROMHandler do begin
    if (TreeViewSR.Selected = nil) then Exit;
    with TreeViewSR.Selected do begin
      TSetReg(Data).Comment := EdtComment.Text;
      UpdateTreeView(Sender);
    end;
  end;
end;


procedure TFrmROMHandler.BtnSaveClick(Sender: TObject);
// Save(as) SR or ROM file
var
  FileNm : string;
begin
  if Mode = SR then begin
    DlgSave.Filter := 'SR File|*.sr';
    DlgSave.DefaultExt := 'sr';
    DlgSave.FileName := OpenSRFileNm;
  end else if Mode = ROM then begin
    DlgSave.Filter := 'ROM File|*.rom';
    DlgSave.FileName := OpenROMFileNm;
    DlgSave.DefaultExt := 'rom';
  end;
  if DlgSave.Execute then begin
    FileNm := DlgSave.Filename;
    if SaveRomFile(FileNm) > 0 then
      PnlFileName.Caption := ExtractFileName(FileNm)
    else
      PnlFileName.Caption := 'Error saving file';
  end;
end;


procedure TFrmROMHandler.FindFreeROM(MemoryType: MemType);
var
  NumImages,
  i: integer;
  EmptyFlash: boolean;
begin
  if MemoryType = FLASH then
    NumImages := ROMImages_FLASH - 1
  else
    NumImages := SRAM_Pages - 1;

  GaugeProgress.Max := NumImages;

  RomNum := 0;
  DisableMLDL;
  repeat
    // List contents of FLASH, one line at a time
    // ROMS that appears to be empty or invalid are skipped
    StartAddr := RomNum * (Range_4k + 1);
    MLDL_ReadBlock(MemoryType, StartAddr, 16);

    GaugeProgress.StepIt;
    Application.ProcessMessages;

    EmptyFlash := true;
    for i := 0 to 15 do begin
      if MemoryType = FLASH then
        EmptyFlash := EmptyFlash and (WordArray[i] = $FFFF)
      else
        EmptyFlash := EmptyFlash and ((WordArray[i] and $FC00) <> $0000);
    end;

    Inc(RomNum);
  until EmptyFlash or (RomNum > NumImages);
  if EmptyFlash then begin
    ROMNum := ROMNum - 1;
    FrmROMHandler.EdtROMNum.Text := SStr(RomNum);
  end else
    // No empty pages found, display message
    MessageDlg('No empty page found', mtWarning, [mbOk], 0);
  GaugeProgress.Position := 0;
  GaugeProgress.Max := 100;
  EnableMLDL;
end;


procedure TFrmROMHandler.ChkAutoClick(Sender: TObject);
// Click Checkbox for Auto Find ROM
// This is enable only when the MLDL2000 is connected
begin
  // When enabled, the ROM Number will be disabled, but the chosen ROM will be shown
  if MLDL_Is_Connected then begin
    if ChkAuto.Checked then begin
      // It is Checked, so disable the ROM edit
      EdtROMNum.Enabled := false;
      UpDnNumber.Enabled := false;
      // Now find first possible ROM
      PnlStatus.Caption := 'CHECKING FREE SPACE';
      if RadioSRAM.Checked then begin
        FindFreeROM(SRAM);
        MemTp := SRAM;
      end else begin
        FindFreeROM(FLASH);
        MemTp := FLASH;
      end;
      PnlStatus.Caption := 'CHECKING DONE';
    end else begin
      EdtROMNum.Enabled := true;
      UpDnNumber.Enabled := true;
    end;
  end;
end;


procedure VerifySR;
var
  Adr : LongWord;
  Data : Word;
  i : Integer;
  Res : boolean;
  Memt : MemType;
begin
  DisableMLDL;
  Res := true;
  i := 0;
  FrmROMHandler.PnlStatus.Caption := 'VERIFY BUSY';
  FrmROMHandler.GaugeProgress.Position := 0;
  FrmROMHandler.GaugeProgress.Max := Range_SR;

  SRArray := SRArrays[FrmROMHandler.ComSRPick.ItemIndex];
  Adr := SR0_Base;

  if FrmROMHandler.ComSRPick.ItemIndex > 3 then begin
    MemT := FLASH;
    Adr := Adr + (FrmROMHandler.ComSRPick.ItemIndex - 4) * (Range_SR + 1);
  end else begin
    MemT := SRAM;
    Adr := Adr + FrmROMHandler.ComSRPick.ItemIndex * (Range_SR + 1);
  end;

  repeat
    MLDL_ReadByte(MemT, Adr + i, Data);
    FrmROMHandler.GaugeProgress.StepIt;
    if (Data <> SRArray[i]) then begin
      Res := false;
      FrmROMHandler.PnlStatus.Caption := 'VERIFY ERROR at $' + Hex5(Adr + i);
    end;
    Inc(i);
  until (i > Range_SR) or (not Res);

  if not Res then
    MessageDlg('SR did not verify', mtWarning, [mbOk], 0);
  FrmROMHandler.GaugeProgress.Position := 0;
  FrmROMHandler.GaugeProgress.Max := 100;
  FrmROMHandler.PnlStatus.Caption := 'VERIFY DONE';
  EnableMLDL;
end;


procedure VerifyROM;
var
  Adr : LongWord;
  i, j : Integer;
  Res : boolean;
  S : string;
begin
  DisableMLDL;
  S := FrmROMHandler.EdtROMAddress.Text;
  Adr := HexToInt(S);
  Res := true;
  i := 0;
  FrmROMHandler.PnlStatus.Caption := 'VERIFY BUSY';
  FrmROMHandler.GaugeProgress.Position := 0;
  FrmROMHandler.GaugeProgress.Max := Size_4K div PrefBlockSize;
  FrmROMHandler.Update;

  repeat
    MLDL_ReadBlock(MemTp, Adr + (i * PrefBlockSize), PrefBlockSize);
    FrmROMHandler.GaugeProgress.StepIt;
    Application.ProcessMessages;

    for j := 0 to PrefBlockSize - 1 do begin
      if WordArray[j] <> swap(PageArray[(i * PrefBlockSize) + j]) then begin
        Res := false;
        FrmROMHandler.PnlStatus.Caption := 'VERIFY ERROR at $' +
                                           Hex5(Adr + (i * PrefBlockSize) + j);
        FrmROMHandler.Update;
      end;
    end;
    Inc(i);
  until (i > (Size_4K div PrefBlockSize) - 1) or (not Res);

  if not Res then
    MessageDlg('ROM did not verify', mtWarning, [mbOK], 0);

  FrmROMHandler.GaugeProgress.Position := 0;
  FrmROMHandler.GaugeProgress.Max := 100;
  FrmROMHandler.PnlStatus.Caption := 'VERIFY DONE';
  EnableMLDL;
end;


procedure TFrmROMHandler.BtnUploadClick(Sender: TObject);
// This is the UPLOAD button, programs current ROM or SR to SRAM/FLASH
var
  i, j : integer;
  Adr, Adr1 : LongWord;
  RomNum, Rslt: integer;
  S : string;
  MemT: MemType;
begin
  // First validate the parameters
  DisableMLDL;
  Rslt := 0;
  GaugeProgress.Position := 0;
  if Mode = SR then begin
    // Program current SR
    if MessageDlg('Program Current SR?', mtConfirmation, [mbYes, mbNo], 0) = mrYes  then begin
      // Now start programming the SR
      FillSRArray;
      PnlStatus.Caption := 'PROGRAMMING SR';

      Adr := SR0_Base;
      if ComSRPick.ItemIndex > 3 then begin
        MemT := FLASH;
        Adr := Adr + (ComSRPick.ItemIndex - 4) * (Range_SR + 1);
      end else begin
        MemT := SRAM;
        Adr := Adr + ComSRPick.ItemIndex * (Range_SR + 1);
      end;
      SRArrays[ComSRPick.ItemIndex] := SRArray;

      GaugeProgress.Max := 100;
      FrmROMHandler.Update;
      for i := 0 to Range_SR do WordArray[i] := SRArray[i];
      GaugeProgress.Position := 100;
      MLDL_WriteBlock(MemT, Adr, Range_SR + 1);
      GaugeProgress.Position := 0;
      VerifySR;
    end;
  end else if Mode = ROM then begin
    // Program current ROM
    S := EdtROMAddress.Text;   // Address is already checked
    Adr := HexToInt(S);
    GaugeProgress.Max := Size_4K div PrefBlockSize;
    if MemTP = FLASH then begin
      // Check if this FLASH page is empty!
      RomNum := Adr shr 12;
      PnlStatus.Caption := 'FLASH EMPTY CHECK';
      EmptyCheckPage(MemTp, RomNum, true, Rslt, Adr1);
      GaugeProgress.StepIt;
      if Rslt <> 0 then
        // FLASH Page was NOT empty!
        MessageDlg('FLASH is not empty', mtWarning, [mbOk], 0);
    end;
    if Rslt = 0 then
      if MessageDlg('Program Current ROM?', mtConfirmation, [mbYes, mbNo], 0) = mrYes
      then begin
      // Now start programming the ROM
      PnlStatus.Caption := 'PROGRAMMING ROM';
      FrmROMHandler.Update;
      for i := 0 to (Size_4K div PrefBlockSize) - 1 do begin  // do 64 blocks of 64 words
        for j := 0 to PrefBlockSize - 1 do WordArray[j] := swap(PageArray[PrefBlockSize * i + j]);
        MLDL_WriteBlock(MemTp, Adr + (PrefBlockSize * i), PrefBlockSize);
        GaugeProgress.StepIt;
        Application.ProcessMessages;
        if MLDL_Error <> Err_MLDL_OK then begin
          MessageDlg('FLASH Programming Error', mtError, [mbOk], 0);
          Break;
        end;
        if MLDL_Error <> Err_MLDL_OK then Break;
      end;
      VerifyROM;
      ChkAutoClick(Sender);
    end;

  end else if Mode = MODF then begin
    // Upload from MODfile!
    if MessageDlg('Auto Program MLDL2000?', mtConfirmation, [mbYes, mbNo], 0) = mrYes  then begin
      UploadMod;
      ChkAutoClick(Sender);
    end;
  end;

  GaugeProgress.Position := 0;
  GaugeProgress.Max := 100;

  if  Rslt = 0 then
    PnlStatus.Caption := 'PROGRAMMING DONE'
  else
    PnlStatus.Caption := 'FLASH NOT EMPTY';

  EnableMLDL;
end;

procedure TFrmROMHandler.BtnMove8KClick(Sender: TObject);
// Moves current 4K ROM to the upper 4K of the 8K ROM Page
var
  i: integer;
begin
  for i := 0 to $0FFF do begin
    Page[i + $1000] := Swap(PageArray[i]);
    DisPage[i + $1000].HexCode := DisPage[i].HexCode;  // was dec 1000!!
  end;

  FrmROMHandler.MemoROM.Lines.Add('');
  FrmROMHandler.MemoROM.Lines.Add('ROM moved to upper 4K of 8K block');
  FrmROMHandler.MemoROM.Lines.Add('');
end;

procedure TFrmROMHandler.BtnHexDumpClick(Sender: TObject);
// Create HexDump in ListWindow
var
  i, j: integer;
  S, S_Adr, S_Hex, S_Asc, S_HP: string;

  Wd: Word;
  NumLines: integer;

begin

  ListWindow.FrmLister.Hide;
  ListWindow.FrmLister.EdtList.Clear;

  S := '; HexDump GENERATED: ';
  S := S + DateToStr(Now) + ' ' + TimeToStr(Now);
  S := S + ' by MLDL Manager version ' + AboutBox.GetVersion;
  ListWindow.FrmLister.EdtList.Lines.Add(S);
  S := '; OPEN FILE: ' + OpenROMFileNm;
  ListWindow.FrmLister.EdtList.Lines.Add(S);
  ListWindow.FrmLister.EdtList.Lines.Add('');

  S := '                                                                                       ASCII             HP Chars' ;
  ListWindow.FrmLister.EdtList.Lines.Add(S);
  S := 'ADR   0000 0001 0002 0003 0004 0005 0006 0007 0008 0009 000A 000B 000C 000D 000E 000F  0123456789ABCDEF  0123456789ABCDEF' ;
  ListWindow.FrmLister.EdtList.Lines.Add(S);
  S := '----  ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----  ----------------  ----------------';
  ListWindow.FrmLister.EdtList.Lines.Add(S);

  NumLines := $FF;

  BasePage := CmbBAsePg.ItemIndex;
  BaseAddress := (BasePage shl 12);
  BaseAddress8K := BaseAddress + $1000;

  for i := 0 to NumLines do begin
    // 16 words per line
    S_Adr := Hex4(i * 16 + BaseAddress) + '  ';

    S_Hex := '';
    S_Asc := '';
    S_HP  := '';

    for j := 0 to $F do begin
      Wd := Swap(PageArray[i*16 + j]);
      S_Hex := S_Hex + Hex4(Wd) + ' ';
      S_Asc := S_Asc + Asc(Byte(Wd));
      S_HP  := S_HP  + HPCharD(Wd);
    end;

    S := S_Adr + S_Hex + ' ' + S_Asc + '  ' + S_HP;
    ListWindow.FrmLister.EdtList.Lines.Add(S);

  end;

  // check for 8K ROM
  if ChkDis8K.Checked then begin
    ListWindow.FrmLister.EdtList.Lines.Add('');
    S := '                                                                                       ASCII             HP Chars' ;
    ListWindow.FrmLister.EdtList.Lines.Add(S);
    S := 'ADR   0000 0001 0002 0003 0004 0005 0006 0007 0008 0009 000A 000B 000C 000D 000E 000F  0123456789ABCDEF  0123456789ABCDEF' ;
    ListWindow.FrmLister.EdtList.Lines.Add(S);
    S := '----  ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----  ----------------  ----------------';
    ListWindow.FrmLister.EdtList.Lines.Add(S);

    for i := 0 to NumLines do begin
      // 16 words per line
      S_Adr := Hex4(i * 16 + BaseAddress8K) + '  ';

      S_Hex := '';
      S_Asc := '';
      S_HP  := '';

      for j := 0 to $F do begin
        Wd := Swap(Page[i*16 + j]);
        S_Hex := S_Hex + Hex4(Wd) + ' ';
        S_Asc := S_Asc + Asc(Byte(Wd));
        S_HP  := S_HP  + HPCharD(Wd);
      end;

      S := S_Adr + S_Hex + ' ' + S_Asc + '  ' + S_HP;
      ListWindow.FrmLister.EdtList.Lines.Add(S);

    end;
  end;

  Application.ProcessMessages;
  ListWindow.FrmLister.EdtList.SelStart := 0;
  ListWindow.FrmLister.Show;
  ListWindow.FrmLister.Update;

end;

procedure TFrmROMHandler.BtnROMHeaderClick(Sender: TObject);
var
  i: integer;
  w: word;
  TempPage: ROMPage;
begin
  FrmROMHandler.MemoROM.Lines.Add('');
  FrmROMHandler.MemoROM.Lines.Add('ROM Filename    : ' + OpenROMFileNm);
  DisplayROM;
  if ChkDis8K.Checked then begin
    // temporary copy high part of Page
    for i := 0 to $0FFF do begin
      TempPage[i] := PageArray[i];
      w := Page[i + $1000] and $03FF;
      PageArray[i] := Swap(w);
      // PageArray[i] := swap(Page[i + $1000] and $03FF);
      // this gave a Range Check error on i = 0
    end;
    FrmROMHandler.MemoROM.Lines.Add('');
    FrmROMHandler.MemoROM.Lines.Add('ROM Filename    : ' + OpenROMFileNm);
    DisplayROM;
    for i:= 0 to $0FFF do PageArray[i] := TempPage[i];  // copy back
  end;
end;

procedure TFrmROMHandler.BtnListFAT(Sender: TObject);
// List FAT of current ROM
var
  S, SS: string;
  i: word;
  Address, PrevA: word;
  EndOfDis, UCode: boolean;
  NumFATentries: integer;
  PrefAutoLabelSave: boolean;
begin

  // if ROM does not have a FAT do not generate listing
  if not ChkHasFAT.Checked then Exit;

  PrefAutoLabelSave := PrefAutoLabel;
  PrefAutoLabel := true;

  FATOnly := true;

  // Upper 8K should now already be copied
  Dis8K := ChkDis8K.Checked;
  In8K := false;

  if Dis8K then begin
    RomEnd := $1FFF;
  end else
    RomEnd := $0FFF;

  // Clean XROM Table
  for i := 0 to 65 do begin
    ROMFat[i] := '';
    ROMFat8K[i] := '';
  end;
  ThisXROM := 0;
  ThisXROM8K := 0;

  for i := 0 to $1FFF do LabelTable[i] := '';

  BasePage := CmbBAsePg.ItemIndex;

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
    RomEnd := $0FFF
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
    end;
  end;

  // First go through file and Analyze it
  AnalyseROM;

  // We skip Pass 1, go straight to Pass 2, final disassembly
  Address := (BasePage shl 12);
  DisasmPass := 2;
  EndOfDis := false;

  // Disassemble 1st 4K, determine number of FAT entries
  NumFATentries := DisPage[$0001].HexCode;
  EndAddress := BaseAddress + 2 * NumFATentries;
  Address := Address + 2;

  MemoROM.Lines.Add('');
  MemoROM.Lines.Add('-XROM----ADDR--FUNCTION --------------');

  if NumFATEntries <> 0 then repeat
    // disassemble the FAT only
    CurrentBase := Address and $F000;
    CurrentPage := CurrentBase shr 12;
    in8K := Dis8K and Odd(CurrentPage);
    PrevA := Address;
    NewDis(Address);

    if DisPage[F(PrevA)].DisTp <> Dt_SkipLine then begin
      Ucode := (DisPage[F(PrevA)].HexCode and $200 = $200);
      S := DisPage[F(PrevA)].Lbl;
      Delete(S, 1, 4);
      S := ' ' + S;
      S := S + '   ' + (Hex4(DisPage[F(PrevA)].RefAdr)) + '  ';
      SS := DisPage[F(PrevA)].RefLbl;
      if Ucode then SS := '"' + SS;
      S := S + ExpandSP(SS, 12);
      S := S + DisPage[F(PrevA)].Comment;
      MemoROM.Lines.Add(S);
    end;

    if (Address < EndAddress) and (address <> $FFFF) then
      Address := Address + 1
    else
      EndOfDis := true;
  until EndOfDis;

  // Now do 2nd 4K block if required
  if Dis8K then begin
    Address := BaseAddress8K + 2;
    NumFATentries := DisPage[F(BaseAddress8K + $0001)].HexCode;
    EndAddress := BaseAddress8K + 2 * NumFATentries;
    MemoROM.Lines.Add('');
    MemoROM.Lines.Add('-XROM----ADDR--FUNCTION --------------');
    EndOfDis := false;

    if NumFATEntries <> 0 then repeat

      CurrentBase := Address and $F000;
      CurrentPage := CurrentBase shr 12;

      in8K := Dis8K and Odd(CurrentPage);

      PrevA := Address;
      NewDis(Address);

      if DisPage[F(PrevA)].DisTp <> Dt_SkipLine then begin
        Ucode := (DisPage[F(PrevA)].HexCode and $200 = $200);
        S := DisPage[F(PrevA)].Lbl;
        Delete(S, 1, 4);
        S := ' ' + S;
        S := S + '   ' + (Hex4(DisPage[F(PrevA)].RefAdr)) + '  ';
        SS := DisPage[F(PrevA)].RefLbl;
        if Ucode then SS := '"' + SS;
        S := S + ExpandSP(SS, 12);
        S := S + DisPage[F(PrevA)].Comment;
        MemoROM.Lines.Add(S);
      end;

      if (Address < EndAddress) and (address <> $FFFF) then
        Address := Address + 1
      else
        EndOfDis := true;
    until EndOfDis;
  end;

  PnlStatus.Caption := 'DONE';

  FATOnly := false;
  PrefAutoLabel := PrefAutoLabelSave;

end;


procedure TFrmROMHandler.BtnDisAsmClick(Sender: TObject);
// DISASM Button
begin
  // Find out if 41CL IMDB or FLDB list is needed
  if FrmROMHandler.ChkIMDB.Checked or FrmROMHandler.ChkFLDB.Checked then
    // create IMDB or FLDB listing
    ListIMDB
  else
    // normal disassembly
    DisAssemble;
end;


procedure TFrmROMHandler.BtnDownloadClick(Sender: TObject);
// This is the DOWLOAD button
var
  i, j : integer;
  Adr : LongWord;
  DownOK: boolean;
  S : string;
begin
  DisableMLDL;
  GaugeProgress.Position := 0;
  // First validate the parameters
  DownOK := true;
  if Mode = SR then begin
    // Program current SR
    if PrefConfDown then
      DownOK := MessageDlg('Download SR?', mtConfirmation, [mbYes, mbNo], 0) = mrYes;
    if DownOK then begin
      // Now start downloading the SR
      GaugeProgress.Max := 100;
      PnlStatus.Caption := 'DOWNLOADING SR';

      // download ALL SR's into SRArrays, first SRAM
      Adr := SR0_Base;
      MLDL_ReadBlock(SRAM, Adr , 4 * (Range_SR + 1));
      for i := 0 to 3 do
        for j := 0 to Range_SR do
          SRArrays[i, j] := WordArray[i * (Range_SR + 1) + j];

      // now get FLASH SR's
      Adr := SR0_Base;
      MLDL_ReadBlock(FLASH, Adr , 4 * (Range_SR + 1));
      for i := 0 to 3 do
        for j := 0 to Range_SR do
          SRArrays[i + 4, j] := WordArray[i * (Range_SR + 1) + j];

      // now copy active SR block into SRArray
      SRArray := SRArrays[ComSRPick.ItemIndex];

      if PrefAutoVerify then VerifySR;
      OpenSRFileNm := 'Downloaded SR';
      UpdateSR;
      UpDateTreeView(Sender);
    end;
  end else if Mode = ROM then begin
    // Program current ROM
    if PrefConfDown then
      DownOK := MessageDlg('Download ROM?', mtConfirmation, [mbYes, mbNo], 0) = mrYes;
    if DownOK then begin
      // Now start programming the ROM
      PnlStatus.Caption := 'DOWNLOADING ROM';
      S := EdtROMAddress.Text;   // Address is already checked
      Adr := HexToInt(S);
      GaugeProgress.Max := Size_4K div PrefBlockSize;
      FrmROMHandler.Update;
      for i := 0 to (Size_4K div PrefBlockSize) - 1 do begin  // do 64 blocks of 64 words
        MLDL_ReadBlock(MemTp, Adr + (PrefBlockSize * i), PrefBlockSize);
        for j := 0 to PrefBlockSize - 1 do
          PageArray[PrefBlockSize * i + j] := swap(WordArray[j]);
        GaugeProgress.StepIt;
        Application.ProcessMessages;
      end;
      if PrefAutoVerify then VerifyROM;
      OpenROMFileNm := 'Downloaded ROM';
      PnlFileName.Caption := OpenROMFileNm;
      FrmROMHandler.MemoROM.Clear;
      DisplayROM;
    end;
  end;
  GaugeProgress.Position := 0;
  GaugeProgress.Max := 100;
  PnlStatus.Caption := 'DOWNLOADING DONE';
  EnableMLDL;
end;


procedure TFrmROMHandler.BtnEraseSectorClick(Sender: TObject);
// Erase sector from FLASH Memory
var
  S : string;
  Address : LongWord;
begin
  DisableMLDL;
  with LstFLASHErase do begin
    if ItemIndex >= 0 then begin
      S := Items.Strings[ItemIndex];
      if MessageDlg('Erase ' + S + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes
      then begin
        // Now it can be erased
        Address := HexToInt(S);
        PnlStatus.Caption := 'ERASING SECTOR';
        PnlStatus.Update;
        MLDL_FLASHEraseSector(Address);
        if MLDL_Error = Err_MLDL_OK then
          PnlStatus.Caption := 'ERASING DONE'
        else begin
          PnlStatus.Caption := 'ERASE ERROR';
          MLDL_Error_Report('Erase Sector ');
        end;
      end;
    end;
  end;
  EnableMLDL;
end;


procedure TFrmROMHandler.EmptyCheckPage(Tp: MemType; RomNum: integer;
                         UseBar: boolean; var Result: integer; var ErrAdr: longword);
// Check if a Page of memory is empty (all words are $FFFF)
//   MemType:  FLASH or SRAM
//   RomNum:   Number of ROM
//   UseBar:   Use progressbar when true
//   Result:   Returns 0 if ROM was empty
//   ErrAdr:   Contains the offending address if ROM was not empty
// MLDL must be disable when calling this function!
var
  A, A1, A2: longword;
  Erased: boolean;
  i : integer;
begin
  A1 := RomNum * $1000;
  A2 := A1 + $0FFF;
  A  := A1;
  if UseBar then begin
    FrmRomHandler.GaugeProgress.Position := 0;
    FrmRomHandler.GaugeProgress.Max := PrefBlockSize DIV $1000;
  end;
  repeat
    MLDL_ReadBlock(Tp, A, PrefBlockSize);
    i := 0;
    repeat
      Erased := (WordArray[i] = $FFFF);
      Inc(i);
    until (i > (PrefBlockSize - 1)) or (not Erased);
    if UseBar then
      FrmRomHandler.GaugeProgress.StepIt;
    Application.ProcessMessages;
    A := A + PrefBlockSize;
  until ((not Erased) or (A > A2));

  if not Erased then begin
    ErrAdr := A - PrefBlockSize + i - 1;
    Result := 1;
  end else
    Result := 0;
  if UseBar then begin
    FrmRomHandler.GaugeProgress.Position := 0;
    FrmRomHandler.GaugeProgress.Max := 100;
  end;
end;


procedure TFrmROMHandler.EmptyCheckSR(Tp: MemType; RomNum: integer;
                         UseBar: boolean; var Result: integer; var ErrAdr: longword);
// Check if the SR part of memory is empty (all words are $FFFF)
//   MemType:  FLASH or SRAM
//   RomNum:   Number of ROM (ignored)
//   UseBar:   Use progressbar when true (ignored)
//   Result:   Returns 0 if ROM was empty
//   ErrAdr:   Contains the offending address if ROM was not empty
// MLDL must be disable when calling this function!
var
  A: longword;
  Erased: boolean;
  i : integer;
begin
  A := $FFF00;
  if Tp = SRAM then
    A := $7FF00;

  MLDL_ReadBlock(Tp, A, $100);
  i := 0;
  repeat
    Erased := (WordArray[i] = $FFFF);
    Inc(i);
  until (i > $FF) or (not Erased);

  if not Erased then begin
    ErrAdr := A + i - 1;
    Result := 1;
  end else
    Result := 0;
end;



procedure TFrmROMHandler.BtnCheckEmptyAllClick(Sender: TObject);
//Alternative version
var
  S, SS : string;
  Address, Address1, Address2, Range: LongWord;
  Erased: boolean;
  i, j: integer;
begin
  DisableMLDL;
  FrmRomHandler.GaugeProgress.Position := 0;
  FrmRomHandler.GaugeProgress.Max := 100;
  MLDL_FLASHReset;
  PnlStatus.Caption := 'CHECKING SECTOR';
  with LstFLASHErase do begin
    for j := 0 to Count - 1 do begin
      ItemIndex := j;
      PnlStatus.Update;
      S := Items.Strings[j];
      SS := S;
      ExpandSpaces(SS, 48);
      Delete(SS, 50, 30);
      SS := SS + 'CHECKING';
      Items.Strings[j] := SS;
      PnlStatus.Update;
      // Now it can be checked
      Address1 := HexToInt(S);
      Address2 := HexToInt(S);
      Range := Address2 - Address1;
      Address  := Address1;
      repeat
        MLDL_ReadBlock(FLASH, Address, PrefBlockSize);
        i := 0;
        repeat
          Erased := (WordArray[i] = $FFFF);
          Inc(i);
        until (i > (PrefBlockSize - 1)) or (not Erased);
        GaugeProgress.Position := ((Address - Address1) * 100) div Range;
        Application.ProcessMessages;
        Address := Address + PrefBlockSize;
      until ((Erased = false) or (Address > Address2));
      Delete(SS, 50, 30);
      if Erased = false then begin
        SS := SS + 'NOT ERASED @ $' + Hex5(Address - PrefBlockSize + i - 1);
        Items.Strings[j] := SS;
      end else begin
        SS := SS + 'ERASED';
        Items.Strings[j] := SS;
      end;
      GaugeProgress.Position := 0;
    end;
  end;
  EnableMLDL;
end;


procedure TFrmROMHandler.BtnCheckEmptySectorClick(Sender: TObject);
// Check if sector is empty
var
  S : string;
  Address, Address1, Address2, Range : LongWord;
  Erased: boolean;
  i: integer;
begin
  DisableMLDL;
  MLDL_FLASHReset;
  with LstFLASHErase do begin
    if ItemIndex >= 0 then begin
      S := Items.Strings[ItemIndex];
      PnlStatus.Caption := 'CHECKING SECTOR';
      PnlStatus.Update;
      // Now it can be checked
      Address1 := HexToInt(S);
      Address2 := HexToInt(S);
      Range := Address2 - Address1;
      Address  := Address1;
      repeat
        MLDL_ReadBlock(FLASH, Address, PrefBlockSize);
        i := 0;
        repeat
          Erased := (WordArray[i] = $FFFF);
          Inc(i);
        until (i > (PrefBlockSize - 1)) or (not Erased);
        GaugeProgress.Position := ((Address - Address1) * 100) div Range;
        Application.ProcessMessages;
        Address := Address + PrefBlockSize;
      until ((Erased = false) or (Address > Address2));
      if Erased = false then
        PnlStatus.Caption := 'NOT ERASED @ $' + Hex5(Address - PrefBlockSize + i - 1)
      else begin
        PnlStatus.Caption := 'SECTOR ERASED';
      end;
      GaugeProgress.Position := 0;
    end;
  end;
  EnableMLDL;
end;


procedure TFrmROMHandler.BtnEraseAllClick(Sender: TObject);
// Erase the complete FLASH memory
begin
  DisableMLDL;
  if MessageDlg('Erase All FLASH? This may take 30 seconds!',
                 mtConfirmation, [mbYes, mbNo], 0) = mrYes  then begin
    if MessageDlg('DO YOU REALLY WANT TO ERASE ALL FLASH?',
                   mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
      // Now it can be erased
      PnlStatus.Caption := 'ERASING ALL FLASH';
      PnlStatus.Update;
      MLDL_FLASHEraseAll;
      if MLDL_Error = Err_MLDL_OK then
        PnlStatus.Caption := 'ERASING DONE'
      else begin
        PnlStatus.Caption := 'ERASE ERROR';
        MLDL_Error_Report('Erase All ');
      end;
    end;
  end;
  EnableMLDL;
end;


procedure TFrmROMHandler.BtnResetFLASHClick(Sender: TObject);
// Resets the FLASH Memory
begin
  DisableMLDL;
  MLDL_FLASHReset;
  EnableMLDL;
end;


procedure TFrmROMHandler.BtnReadTypeClick(Sender: TObject);
// Read Flash Memory Type
begin
  if PortIsOpen then begin
    ReadFlashType;
    ReadSRAMType;
  end else
    MessageDlg('MLDL NOT CONNECTED', mtWarning, [mbOK], 0);
end;


procedure TFrmROMHandler.BtnVerifyClick(Sender: TObject);
begin
  DisableMLDL;
  if Mode = ROM then VerifyROM else if Mode = SR then VerifySR;
  EnableMLDL;
end;


procedure TFrmROMHandler.ContentsClick(Sender: TObject);
// Called when double clicked on item in LstContents
// Get info from selected item and adapt SR settings
var
  RomNum, Code: integer;
  S: string;
begin
  if (TreeViewSR.Selected = nil) or (not PortIsOpen ) then Exit;  // No node selected!
  // Find the node in the TreeView that corresponds to this Item
  if (TreeViewSR.Selected.Level = 0) or (LstContents.SelCount <> 1) then begin
    // Multiple nodes selected, or level = 0
    Exit;
    // Pnlhint.Caption := 'Level 0  ' + TreeViewSR.Selected.Text;
  end else with LstContents.Selected do begin
    S := LeftStr(SubItems[3], 2);
    if S = 'FL' then begin
      // This is FLASH
      ChkEnable.Checked := true;
      ChkFlash.Checked := true;
      ChkIO.Checked := false;
      ChkWProtect.Checked := true;      // does not really matter
      with TreeViewSR.Selected do begin
        TSetReg(Data).SetEnabled(true);
        TSetReg(Data).SetFlash(true);
      end;
    end;
    if S = 'SR' then begin
      ChkEnable.Checked := true;
      ChkFLASH.Checked := false;
      ChkIO.Checked := false;           // This is no I/O
      ChkWProtect.Checked := false;     // Assume Write Protect is default
      with TreeViewSR.Selected do begin
        TSetReg(Data).SetEnabled(true);
        TSetReg(Data).SetFlash(false);
        TSetReg(Data).SetNoIO(true);
        TSetREg(Data).SetWEnabled(false);
      end;
    end;
    Val(MidStr(SubItems[3], 4, 3), RomNum, Code);   // Must remove leading zero's
    S := IntToStr(RomNum);
    EdtROMNumber.Text := S;
    with TreeViewSR.Selected do begin
      if TSetReg(Data).Flash_SR then
        UpDnRomNum.Max := 255
      else
        UpDnRomNum.Max := 63;
      UpdateTreeView(Sender);
    end;
  end;
end;


procedure TFrmROMHandler.BtnExpandClick(Sender: TObject);
begin
  if BtnExpand.Caption = 'Expand All' then begin
    TreeViewSR.FullExpand;
    BtnExpand.Caption := 'Collapse All';
  end else begin
    TreeViewSR.FullCollapse;
    BtnExpand.Caption := 'Expand All';
  end;
end;


procedure TFrmROMHandler.PopCollapse07Click(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to (8 * 5 - 1) do TreeViewSR.Items[i].Collapse(false);
end;

procedure TFrmROMHandler.PopCollapse8FClick(Sender: TObject);
var
  i: integer;
begin
  for i := (8 * 5) to ( 8 * 10 - 1) do TreeViewSR.Items[i].Collapse(false);
end;

procedure TFrmROMHandler.PopExpand07Click(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to (8 * 5 - 1) do TreeViewSR.Items[i].Expand(false);
end;

procedure TFrmROMHandler.PopExpand8FClick(Sender: TObject);
var
  i: integer;
begin
  for i := (8 * 5) to ( 8 * 10 - 1) do TreeViewSR.Items[i].Expand(false);
end;


procedure TFrmROMHandler.PopExpandAllClick(Sender: TObject);
begin
  TreeViewSR.FullExpand;
  BtnExpand.Caption := 'Collapse All';
end;


procedure TFrmROMHandler.PopRefreshClick(Sender: TObject);
// Same as List
begin
  BtnContentsClick(Sender);
end;

procedure TFrmROMHandler.PopSaveAsClick(Sender: TObject);
// Download ROM and save
begin
  FrmROMHandler.PopDownloadClick(Sender);
  FrmROMHandler.BtnSaveClick(Sender);
end;

procedure TFrmROMHandler.PopCollapseAllClick(Sender: TObject);
begin
  TreeViewSR.FullCollapse;
  BtnExpand.Caption := 'Expand All';
end;


procedure TFrmROMHandler.PopDownloadClick(Sender: TObject);
// Download selected ROM
var
  Code: integer;
  S: string;
begin
  if (LstContents.Selected <> nil) and (LstContents.SelCount = 1) then with LstContents.Selected do begin
    // only one item may be selected
    S := LeftStr(SubItems[3], 2);
    Val(MidStr(SubItems[3], 4, 3), RomNum, Code);   // Must remove leading zero's
    // Disable autofind ROM when downloading
    ChkAuto.Checked := false;
    if S = 'FL' then begin
      MemTp := FLASH;
      PnlMemType.Caption := 'FLASH';
      RadioSRAM.Checked := false;
      RadioFLASH.Checked := true;
      UpDnNumber.Max := 255;
    end else if S = 'SR' then begin
      MemTp := SRAM;
      PnlMemType.Caption := 'SRAM';
      RadioSRAM.Checked := true;
      RadioFLASH.Checked := false;
      UpDnNumber.Max := SRAM_Pages - 1;
    end;
    S := IntToStr(RomNum);
    EdtROMNumber.Text := S;
    PageControl.ActivePage := ROMSheet;
    FrmROMHandler.PageControlChange(Sender);
    FrmROMHandler.BtnDownloadClick(Sender);
  end;
end;


procedure TFrmROMHandler.PopSRAMWipeClick(Sender: TObject);
// Invalidates the first 16 words of the current block of SRAM
var
  i, Code, idx, TtlItems: integer;
  Adr: longword;
  ls: TListItem;
  S: string;
  // StopLoop: Boolean;
begin
  if LstContents.Selected <> nil then with LstContents do begin
    TtlItems := Items.Count;
    PnlStatus.Caption := 'WIPING SRAM';
    FrmROMHandler.Update;
    // go through selected items
    DisableMLDL;
    if TtlItems >0 then for idx := 0 to TtlItems - 1 do begin
      ls := Items[idx];
      if ls.Selected then begin
        // item is selected, now check for SRAM and wipe
        S := LeftStr(ls.SubItems[3], 2);                   // SR or FL
        Val(MidStr(ls.SubItems[3], 4, 3), RomNum, Code);   // get ROM Number
        if S = 'SR' then begin                             // wiping of FLASH is ignored
          Adr := RomNum * $1000;
          for i := 0 to 15 do WordArray[i] := $FFFF;
          MLDL_WriteBlock(SRAM, Adr, 16);
        end;
      end;
    end;
    EnableMLDL;
  end;
  PnlStatus.Caption := 'DONE';
  FrmROMHandler.Update;
end;


procedure TFrmROMHandler.PopAppendtoMODfileClick(Sender: TObject);
// Append all selected to current MOD file
var
  i, Code, RomNum, idx, TtlItems: integer;
  Adr: longword;
  ls: TListItem;
  S: string;
  MemT: MemType;
begin
  {$I-}
  Seek(MODFile, 0);                     // go to start of file
  {$I+}
  if IOResult <> 0 then begin
    MessageDlg('No MODFILE open', mtWarning, [mbOK], 0);
    Exit;
  end;
  if LstContents.Selected <> nil then with LstContents do begin
    PnlStatus.Caption := 'ADDING TO MOD FILE';
    ChkAuto.Checked := false;           // disable AutoFind when downloading
    FrmROMHandler.Update;
    GaugeProgress.Position := 0;
    GaugeProgress.Max := SelCount;
    TtlItems := Items.Count;
    // go through selected items
    DisableMLDL;

    if TtlItems >0 then for idx := 0 to TtlItems - 1 do begin
      ls := Items[idx];
      if ls.Selected then begin

        if MODNumPages >254 then begin
          MessageDlg('Too many images', mtWarning, [mbOK], 0);
          EnableMLDL;
          Exit;
        end;

        GaugeProgress.StepIt;

        // Download the ROM image
        S := LeftStr(ls.SubItems[3], 2);                   // SR or FL
        Val(MidStr(ls.SubItems[3], 4, 3), RomNum, Code);   // get ROM Number
        if S = 'SR' then
          MemT := SRAM
        else
          MemT := FLASH;
        Adr := RomNum * $1000;
        MLDL_ReadBlock(MemT, Adr, $1000);                  // read block

        // Copy data from ROM to MODArray and compress
        for i := 0 to $0FFF do MODArray[i] := WordArray[i];
        CompressBin;

        DisplayMODFilePage;   // Display parameters, mainly to get ROM name and ID
        MODName := LblName.Caption;
        MODID   := LBLRev.Caption;

        // Definition of MODPageCustom:
        //  Byte 0: $A5               code to recognize Backup
        //  Byte 1: $01  SRAM         bit 1 or 2 always set
        //          $02  FLASH
        //          $04  ROM          bit 3 or 4 always set
        //          $08  SR
        //  Byte 2: Page number       not used when SR
        MODPageCustom[0] := $A5;
        if MemT = SRAM then
          MODPageCustom[1] := $05
        else
          MODPageCustom[1] := $06;
        MODPageCustom[2] := Byte(RomNum);

        // Now save data to MOD file
        Inc(MODNumPages);
        Seek(MODFile, 0);                      // go to start of file
        WriteMODHeader(MODFile);               // update header with NumPages

        Seek(MODFile, FileSize(MODFile));      // go to end of File
        WriteMODPage(MODFile);                 // Write new info

      end;
    end;
    EnableMLDL;

  end;
  PnlStatus.Caption := 'DONE';
  ReadMODFile;                                          // Re-read MODFile
  GaugeProgress.Max:= 100;
  GaugeProgress.Position := 0;
  FrmROMHandler.Update;
end;


initialization
  {$i ROMHandler.lrs}

end.


