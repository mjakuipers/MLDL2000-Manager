unit Jtag;

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
//  JTAG.PAS                                                                 //
//  JTAG functions for programming the CPLD on the M2kM                      //
//  Ver  Date     Description                                                //
//  0.10 Nov 2005 In development                                             //
//  0.11 Nov 2006 Compiled for Turbo Explorer                                //
//  0.99 Jun 2007 Code working, exception handling must be added             //
//  1.20 Oct 2007 Final version, added exceptions, IDCODE and UES readout    //
//  1.20 Apr 2008 Added code to save window positions                        //
//  1.50 May 2008 Final release                                              //
//  1.51 Sep 2008 Changed saving of screen position                          //
//  1.70 Jul 2010 Adapted for Lazarus                                        //
//  1.80 Feb 2018 restarted development                                      //
//                removed Delphi compatibility                               //
//  1.90 Feb 2020 fixed array variable referencing for data_type             //
//  1.90 Mar 2020 window never remains hidden on non-visible monitor         //
//---------------------------------------------------------------------------//

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Procs_MLDL, Procs, ExtCtrls, Jtag_constants,
  ComCtrls, LResources, IniFiles;

type

  { TFrm_JTAG }

  TFrm_JTAG = class(TForm)
    BtnEnable: TButton;
    BtnProgUES: TButton;
    DlgOpen: TOpenDialog;
    BtnOpen: TButton;
    BtnProgram: TButton;
    EdtUES: TEdit;
    PnlBottom: TPanel;
    ChkSimulate: TCheckBox;
    ChkLog: TCheckBox;
    ChkUSBProg: TCheckBox;
    ChkDebug: TCheckBox;
    MemoJTAG: TMemo;
    BtnNext: TButton;
    ChkTrace: TCheckBox;
    PnlStatus: TPanel;
    ProgressJTAG: TProgressBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PnlError: TPanel;
    BtnDevID: TButton;
    BtnFWID: TButton;
    BtnDbgEnable: TButton;
    GroupBox1: TGroupBox;
    procedure BtnEnableClick(Sender: TObject);
    procedure BtnOpenClick(Sender: TObject);
    procedure BtnProgramClick(Sender: TObject);
    procedure BtnDevIDClick(Sender: TObject);
    procedure BtnFWIDClick(Sender: TObject);
    procedure BtnDbgEnableClick(Sender: TObject);
    procedure BtnProgUESClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ReadFWID(var IDCODE: string; var FWID: string; var Rslt: integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Frm_JTAG: TFrm_JTAG;
  JTAG_Enabled: boolean = false;
  Debug_Enabled: boolean = false;
  JTAGFile: File;
  SimFile: Text;
  FileName, OpenJTAGFileName: string;
  JTAGFileOpen: boolean;
  Simulate, LogFile : boolean;
  CancelFileRead: boolean;
  Cmd: byte;
  FromDebug: boolean;
  JTAGFileSize, BytesRead, ErrCount: integer;

  // Values to remember
  Val_XREPEAT   : byte = 32;
  Val_XSDRSIZE,
  Val_XTDOMASK  : LongWord;
  Val_XRUNTEST  : LongWord = 0;
  State_XENDIR,
  State_XENDDR  : jtag_state;
  XTDO_Expected : data_type;
  XTDO_Mask     : data_type;
  TDI_Array     : data_type;
  TDO_Array     : data_type;


implementation



procedure TFrm_JTAG.BtnEnableClick(Sender: TObject);
begin
  if JTAG_Enabled then begin
    JTAG_Enabled := false;
    BtnEnable.Caption := 'Enable JTAG';
    InitDefault;                         // Outputs to default value
  end else begin
    JTAG_Enabled := true;
    BtnEnable.Caption := 'Disable JTAG';
    InitJTAGMode(ChkUSBProg.Checked);    // Now go to JTAG mode
  end;
  Update;
end;


function OpenJTAGFile(FileNm: string): integer;
//Open JTAG *.XSVF file
var
  Extension: string;
  NumRead: integer;
begin
  NumRead := 0;
  FileName := FileNm;
  if FileExists(FileNm) then begin
    Extension := UpperCase(ExtractFileExt(FileNm));
    if Extension = '.XSVF' then begin
      // open JTAG file
      AssignFile(JTAGFile, FileNm);
      FileMode := 0;
      Reset(JTAGFile, 1);
      BytesRead := 0;
      ErrCount := 0;
      Frm_JTAG.PnlError.Caption := '';
      Frm_JTAG.ProgressJTAG.Position := 0;
      JTAGFileSize := FileSize(JTAGFile);
      OpenJTAGFileName := FileNm;
      NumRead := 1;
      JTAGFileOpen := true;
    end else MessageBox(0, 'Wrong Extension', nil, MB_ICONEXCLAMATION);
  end else MessageBox(0, 'File Nonexistent', nil, MB_ICONEXCLAMATION);
  OpenJTAGFile := NumRead;
end;


procedure X_ILLEGAL;
// Command not recognized or XSVF file out of sync
begin
  CancelFileRead := true;
  if LogFile then WriteLn(SimFile, 'X_ILLEGAL $' + Hex2(Cmd));
  if Frm_JTAG.ChkTrace.Checked then
    Frm_JTAG.MemoJTAG.Lines.Add('X_ILLEGAL $' + Hex2(Cmd));
  CancelFileRead := true;
  // now do real programming
  // nothing to do with ILLEGAL COMMAND
end;


procedure X_COMPLETE;
begin
  if LogFile then WriteLn(SimFile, 'XCOMPLETE');
  if Frm_JTAG.ChkTrace.Checked then begin
    Frm_JTAG.MemoJTAG.Lines.Add('X_COMPLETE');
    Frm_JTAG.MemoJTAG.Lines.Add('--------------------------------');
    Frm_JTAG.MemoJTAG.Lines.Add('');
  end;
  // now do real programming, but nothing to do
end;


procedure X_TDOMASK;
// XTDOMASK value<“length” bits>
// XTDOMASK sets the TDO mask which masks the value of all TDO values from the
//   SDR instructions. Length is defined by the last XSDRSIZE instruction.
//   XTDOMASK can be used multiple times in the XSVF file if the TDO mask
//   changes for various SDR instructions
var
  XSDRSIZE_bytes,
  XSDRSIZE_rem,
  numbytes, i       : integer;
  S: string;
//  Rslt: boolean;
begin
  // Val_XSDRSIZE contains the wordlength in bits
  // work out byte size
  XSDRSIZE_bytes := Val_XSDRSIZE div 8;
  XSDRSIZE_rem   := Val_XSDRSIZE mod 8;
  numbytes := XSDRSIZE_bytes;
  if XSDRSIZE_rem <> 0 then inc(numbytes);
  BlockRead(JTAGFile, XTDO_Mask, numbytes);
  BytesRead := BytesRead + NumBytes;

  if Frm_JTAG.ChkTrace.Checked then begin
    S := 'XTDOMASK   $';
    for i := 0 to (numbytes - 1) do S := S + Hex2(XTDO_Mask[i]);
    Frm_JTAG.MemoJTAG.Lines.Add(S);
  end;

  if LogFile then begin
    Write(SimFile, 'XTDOMASK   $');
    for i := 0 to (numbytes - 1) do Write(SimFile, Hex2(XTDO_Mask[i]));
    WriteLn(SimFile);
  end;
  // Now do the real work
  // There is nothing really to do, the TDO mask is XTDO_Mask
end;


procedure X_SIR;
// XSIR length<1 byte> TDIValue<“length” bits>
// Go to the Shift-IR state and shift in the TDIValue. If the last XRUNTEST time
//   is non-zero, go to the Run-Test/Idle state and wait for the last specified
//   XRUNTEST time. Otherwise, go to the last specified XENDIR state.
var
  Len: byte;
  Val_TDI: data_type;
  Temp_TDI: data_type;
  L_bytes, L_rem, numbytes, i: integer;
  end_state: jtag_state;
  S: string;
//  Rslt: boolean;
begin
  BlockRead(JTAGFile, Len, 1);
  BytesRead := BytesRead + 1;
  L_bytes := Len div 8;
  L_rem   := Len mod 8;
  numbytes := L_bytes;
  if L_rem <> 0 then inc(numbytes);
  BlockRead(JTAGFile, Val_TDI, numbytes);
  BytesRead := BytesRead + NumBytes;
  if Frm_JTAG.ChkTrace.Checked then begin
    S := 'XSIR       $' + Hex2(Len) + ' $';
    for i := 0 to (numbytes - 1) do S := S + Hex2(Val_TDI[i]);
    Frm_JTAG.MemoJTAG.Lines.Add(S);
  end;
  if LogFile then begin
    Write(SimFile, 'XSIR       $');
    Write(SimFile, Hex2(Len));
    for i := 0 to (numbytes - 1) do Write(SimFile, ' $' + Hex2(Val_TDI[i]));
    WriteLn(SimFile);
  end;

  // Now do the real work
  if not Simulate then begin
    if Val_XRUNTEST <> 0 then
      end_state := run_test_idle
    else
      end_state := State_XENDIR;
    // Modify array, LSB goes first!
    for i := 0 to numbytes - 1 do Temp_TDI[i] := Val_TDI[i];
    for i := 0 to numbytes - 1 do Val_TDI[i] := Temp_TDI[numbytes - i - 1];
    JTAG_sout(instruction_register, Len, Val_TDI, end_state);
    // wait for specified XRUNTEST time
    if Val_XRUNTEST <> 0  then sleep(Val_XRUNTEST DIV 1000);
  end;
end;


procedure X_RUNTEST;
// XRUNTEST time<4 bytes>
// Defines the amount of time (in microseconds) the device should sit in the
//   Run-Test/Idle state after each visit to the SDR state. The initial XRUNTEST
//   time is zero microseconds.
var
  ValArray: array [0..3] of byte;
  i: integer;
  S: string;
begin
  BlockRead(JTAGFile, ValArray, 4);
  BytesRead := BytesRead + 4;
  Val_XRUNTEST := ValArray[3] +              256 * ValArray[2] +
                                       256 * 256 * ValArray[1] +
                                 256 * 256 * 256 * ValArray[0];
  if Frm_JTAG.ChkTrace.Checked then begin
    S := 'XRUNTEST   $';
    for i := 0 to 3 do S := S + Hex2(ValArray[i]);
    Frm_JTAG.MemoJTAG.Lines.Add(S);
  end;
  if LogFile then begin
    Write(SimFile, 'XRUNTEST   $');
    for i := 0 to 3 do Write(SimFile, Hex2(ValArray[i]));
    WriteLn(SimFile)
  end;
  // now do real programming, nothing to do, value is stored
end;


procedure X_REPEAT;
// XREPEAT times<1 byte>
// Defines the number of times that TDO is tested against the expected value
//   before the ISP operation is considered a failure. By default, a device can
//   fail an XSDR instruction 32 times before the ISP operation is terminated
//   as a failure. This instruction is optional.
begin
  BlockRead(JTAGFile, Val_XREPEAT, 1);
  BytesRead := BytesRead + 1;
  if LogFile then WriteLn(SimFile, 'XREPEAT    $' + Hex2(Val_XREPEAT));
  if Frm_JTAG.ChkTrace.Checked then
    Frm_JTAG.MemoJTAG.Lines.Add('XREPEAT    $' + Hex2(Val_XREPEAT));
  // now do real programming, nothing to do, just store the value
end;


procedure X_SDRSIZE;
// XSDRSIZE length<4 bytes>
// Specifies the length of all XSDR/XSDRTDO records that follow
var
  ValArray: array [0..3] of byte;
  i: integer;
  S: string;
begin
  BlockRead(JTAGFile, ValArray, 4);
  BytesRead := BytesRead + 4;
  if LogFile then begin
    Write(SimFile, 'XSDRSIZE   $');
    for i := 0 to 3 do Write(SimFile, Hex2(ValArray[i]));
    WriteLn(SimFile)
  end;
  if Frm_JTAG.ChkTrace.Checked then begin
    S := 'XSDRSIZE   $';
    for i := 0 to 3 do S := S + Hex2(ValArray[i]);
    Frm_JTAG.MemoJTAG.Lines.Add(S);
  end;
  Val_XSDRSize := ValArray[3] +            256 * ValArray[2] +
                                     256 * 256 * ValArray[1] +
                               256 * 256 * 256 * ValArray[0];
  // now do real programming, nothing to do, value is stored
end;


procedure X_SIR_core(NumBits: Byte; end_state: jtag_state);
//   NumBite: number of bits to be used
//   end_state: state that operation should be ended in
//   TDI_Array (global variable) should contain the bits to be shifted, LSB first!
begin
  JTAG_sout(instruction_register, NumBits, TDI_Array, end_state);
end;


procedure X_SDRTDO_core(NumBits: integer; end_state: jtag_state; var Result: integer;
                        var Retries: integer);
//  NumBits: number of bits to be used
//  end_state: state that operation should end in
//  Result returns the number of byte errors that was found during the last retry
//  Retries returns the number of retries that were done
//
//  TDI_Array     (global variable) should contain the bits to be shifted in, LSB first!
//  XTDO_Expected (global variable) contains the expected value
//  TDO_Array     (global variable) contains the result
//  XTDO_Mask     (global variable) contains the bit mask to be applied
//  Val_XRUNTEST  (global variable) time to wait
//  Val_XREPEAT   (global variable) number of times to retry
var
  XSDRSIZE_bytes,
  XSDRSIZE_rem,
  numbytes, i : integer;
  temp_XTDI, temp_data_in: byte;
  byte_err_count, ErrLoopCount : integer;
  RUNTEST_exception: integer;
begin
  // Val_XSDRSIZE contains the wordlength in bits
  // work out byte size
  XSDRSIZE_bytes := NumBits div 8;
  XSDRSIZE_rem   := NumBits mod 8;
  numbytes := XSDRSIZE_bytes;
  if XSDRSIZE_rem <> 0 then inc(numbytes);
  RUNTEST_Exception := Val_XRUNTEST;
  ErrLoopCount := 0;

  // Now do the real work
  repeat          // loop for exception handling
    JTAG_sio(data_register, NumBits, TDI_array, data_in, end_state, true);
    for i := 0 to numbytes - 1 do TDO_Array[i] := data_in[numbytes - i - 1];
    // check read value with expected value
    byte_err_count := 0;
    for i := 0 to numbytes - 1 do begin
      temp_XTDI    := XTDO_Expected[i] and XTDO_Mask[i];
      temp_data_in := TDO_Array[i] and XTDO_Mask[i];
      if temp_XTDI <> temp_data_in then inc(byte_err_count)
    end;
    ErrCount := byte_err_count;
    if ErrCount <> 0 then begin
      Inc(ErrLoopCount);
      // this is the exception handling when errors are encountered
      if current_state <> exit1_dr then JTAG_change_state(exit1_dr);
      JTAG_change_state(pause_dr);
      JTAG_change_state(exit2_dr);
      JTAG_change_state(shift_dr);
      JTAG_change_state(exit1_dr);
      JTAG_change_state(update_dr);
      JTAG_change_state(run_test_idle);
      // Increase XRUNTEST Time by 25%
      RUNTEST_Exception := RUNTEST_Exception + (RUNTEST_Exception DIV 100) * 25;
    end else
      // wait for specified XRUNTEST time
      if Val_XRUNTEST <> 0  then sleep(Val_XRUNTEST DIV 1000);
  until (ErrLoopCount > Val_XREPEAT) or (ErrCount = 0);
                                            // end of exception handling loop
  Result := ErrCount;
  Retries := ErrLoopCount;
end;


procedure X_SDRTDO;
// XSDRTDO TDIValue<“length” bits> TDOExpected<“length” bits>
// Go to the Shift-DR state and shift in TDIValue; compare the TDOExpected value
//   against the TDO value that was shifted out (use the TDOMask which was
//   generated by the last XTDOMASK instruction). Length comes from the XSDRSIZE
//   instruction. If the TDO value does not match TDOExpected, perform the
//   exception-handling sequence described in the XC9500 programming algorithm
//   section. If TDO is wrong more than the maximum number of times specified by
//   the XREPEAT instruction, then the ISP operation is determined to have
//   failed. If the last XRUNTEST time is zero, then go to XENDDR state.
//   Otherwise, go to the Run_Test/Idle state and wait for the XRUNTEST time.
//   The TDOExpected Value is used in all successive XSDR instructions until the
//   next XSDR instruction is given.
var
  XSDRSIZE_bytes,
  XSDRSIZE_rem,
  numbytes, i       : integer;
  end_state: jtag_state;
  Temp_array: data_type;
  In_array: data_type;
  temp_XTDI, temp_data_in: byte;
  byte_err_count, ErrLoopCount : integer;
  S: string;
  RUNTEST_exception: integer;
begin
  // Val_XSDRSIZE contains the wordlength in bits
  // work out byte size
  XSDRSIZE_bytes := Val_XSDRSIZE div 8;
  XSDRSIZE_rem   := Val_XSDRSIZE mod 8;
  numbytes := XSDRSIZE_bytes;
  if XSDRSIZE_rem <> 0 then inc(numbytes);
  RUNTEST_Exception := Val_XRUNTEST;
  ErrLoopCount := 0;
  BlockRead(JTAGFile, TDI_array,     numbytes);
  BlockRead(JTAGFile, XTDO_Expected, numbytes);
  BytesRead := BytesRead + NumBytes + NumBytes;
  if Frm_JTAG.ChkTrace.Checked then begin
    S := 'XSDRTDO    $';
    for i := 0 to (numbytes - 1) do S := S + Hex2(TDI_array[i]);
    Frm_JTAG.MemoJTAG.Lines.Add(S);
    S := '           $';
    for i := 0 to (numbytes - 1) do S := S + Hex2(XTDO_Expected[i]);
    Frm_JTAG.MemoJTAG.Lines.Add(S);
  end;

  if LogFile then begin
    Write(SimFile, 'XSDRTDO    $');
    for i := 0 to (numbytes - 1) do Write(SimFile, Hex2(TDI_array[i]));
    WriteLn(SimFile);
    Write(SimFile, '           $');
    for i := 0 to (numbytes - 1) do Write(SimFile, Hex2(XTDO_Expected[i]));
    WriteLn(SimFile);
  end;

  // correct for byte ordering, LSB goes first!
  for i := 0 to numbytes - 1 do Temp_Array[i] := TDI_array[i];
  for i := 0 to numbytes - 1 do TDI_array[i] := Temp_Array[numbytes - i - 1];

  // Now do the real work
  if not Simulate then begin
    repeat          // loop for exception handling
      if Val_XRUNTEST <> 0 then
        end_state := run_test_idle
      else
        end_state := State_XENDDR;
      JTAG_sio(data_register, Val_XSDRSIZE, TDI_array, data_in, end_state, true);
      for i := 0 to numbytes - 1 do In_array[i] := data_in[numbytes - i - 1];

      if LogFile  then begin
        Write(SimFile, 'XSDRTDO RD $');
        for i := 0 to numbytes - 1 do Write(SimFile, Hex2(In_array[i]));
        WriteLn(SimFile);
      end;

      if Frm_JTAG.ChkTrace.Checked then begin
        S := 'XSDRTDO RD $';
        for i := 0 to numbytes - 1 do S := S + Hex2(In_array[i]);
        Frm_JTAG.MemoJTAG.Lines.Add(S);
      end;

      // check read value with expected value
      byte_err_count := 0;
      for i := 0 to numbytes - 1 do begin
        temp_XTDI    := XTDO_Expected[i] and XTDO_Mask[i];
        temp_data_in := In_array[i] and XTDO_Mask[i];
        if temp_XTDI <> temp_data_in then
          inc(byte_err_count)
      end;
      ErrCount := byte_err_count;
      if LogFile  then WriteLn(SimFile, 'XSDRTDO ERRORS: ', byte_err_count);

      if Frm_JTAG.ChkTrace.Checked then begin
        S := 'XSDRTDO ERRORS: ' + IntToSTr(byte_err_count);
        Frm_JTAG.MemoJTAG.Lines.Add(S);
      end;

      if ErrCount <> 0 then begin
        Inc(ErrLoopCount);
        // this is the exception handling when errors are encountered
        if current_state <> exit1_dr then JTAG_change_state(exit1_dr);
        JTAG_change_state(pause_dr);
        JTAG_change_state(exit2_dr);
        JTAG_change_state(shift_dr);
        JTAG_change_state(exit1_dr);
        JTAG_change_state(update_dr);
        JTAG_change_state(run_test_idle);
        // Increase XRUNTEST Time by 25%
        RUNTEST_Exception := RUNTEST_Exception + (RUNTEST_Exception DIV 100) * 25;
      end else
        // wait for specified XRUNTEST time
        if Val_XRUNTEST <> 0  then sleep(Val_XRUNTEST DIV 1000);
    until (ErrLoopCount > Val_XREPEAT) or (ErrCount = 0);
                                            // end of exception handling loop
    if ErrLoopCount <> 0 then begin
      if Frm_JTAG.ChkTrace.Checked then begin
        S :=  SStr(ErrLoopCount) + ' ERROR(S) FOUND';
        if ErrLoopCount > Val_XREPEAT then S := S + '  CRITICAL!!';
        Frm_JTAG.MemoJTAG.Lines.Add(S);
      end;
      if ErrLoopCount > Val_XREPEAT then CancelFileRead := true;
    end;
  end;
end;


procedure X_STATE;
// XSTATE state <1 byte>
// If state is zero, force TAP to Test-Logic-Reset state by holding TMS High
//   and applying 5 TCK cycles. If state is one, transition TAP from
//   Test-Logic-Reset to Run-Test/Idle.
var
  Val_XSTATE: byte;
  S: string;
begin
  BlockRead(JTAGFile, Val_XSTATE, 1);
  BytesRead := BytesRead + 1;

  if LogFile then begin
    WriteLn(SimFile, 'XSTATE     $' + Hex2(Val_XSTATE));
//    if ((Val_XSTATE >= $00) and (Val_XSTATE <= $0F)) then
//      WriteLn(SimFile, '        ' + TapArray[Val_XSTATE])
//    else
//      WriteLn(SimFile, '        Illegal Parameter');
  end;

  if Frm_JTAG.ChkTrace.Checked then begin
    S := 'XSTATE     $' + Hex2(Val_XSTATE);
//    if ((Val_XSTATE >= $00) and (Val_XSTATE <= $0F)) then
//      S := S + ' ' + TapArray[Val_XSTATE]
//    else
//      S := S + '        Illegal Parameter';
    Frm_JTAG.MemoJTAG.Lines.Add(S);
  end;
  // now do real programming
  if not Simulate then JTAG_change_state(Val_XSTATE);
end;


procedure X_ENDIR;
// XENDIR state <1 byte>
// Set the XSIR end state to Run-Test/Idle (0) or Pause-IR (1). The default is
//   Run-Test/Idle.
var
  Val_XENDIR: byte;
  S: string;
begin
  BlockRead(JTAGFile, Val_XENDIR, 1);
  BytesRead := BytesRead + 1;
  if LogFile then begin
    Write(SimFile, 'XENDIR     $' + Hex2(Val_XENDIR));
    case Val_XENDIR of
      $00: WriteLn(SimFile, '        Run-Test/Idle');
      $01: WriteLn(SimFile, '        Pause-IR');
      else begin
             WriteLn(SimFile, '        Illegal Parameter');
             CancelFileRead := true;
      end;
    end;
  end;

  if Frm_JTAG.ChkTrace.Checked then begin
    S := 'XENDIR     $' + Hex2(Val_XENDIR);
    case Val_XENDIR of
      $00: S := S + '        Run-Test/Idle';
      $01: S := S + '        Pause-IR';
      else S := S + '        Illegal Parameter';
    end;
    Frm_JTAG.MemoJTAG.Lines.Add(S);
  end;

  // now do real programming
  if not Simulate then begin
    case Val_XENDIR of
      $00: // set XSIR end state to Run-Test/Idle
           State_XENDIR := run_test_idle;
      $01: // set XSIR end state to Pause-IR
           State_XENDIR := pause_ir;
      else begin
             State_XENDIR := undefined_jtag_state;
             CancelFileRead := true;
      end;
    end;
  end;
  // now do real programming, nothing to do, end state is stored
end;


procedure X_ENDDR;
// XENDDR state <1 byte>
// Set the XSDR and XSDRTDO end state to Run-Test/Idle (0) or Pause-DR (1). The
//   default is Run-Test/Idle.
var
  Val_XENDDR: byte;
  S: string;
begin
  BlockRead(JTAGFile, Val_XENDDR, 1);
  BytesRead := BytesRead + 1;
  if LogFile then begin
    Write(SimFile, 'XENDDR     $' + Hex2(Val_XENDDR));
    case Val_XENDDR of
      $00: WriteLn(SimFile, '        Run-Test/Idle');
      $01: WriteLn(SimFile, '        Pause-DR');
      else begin
             WriteLn(SimFile, '         Illegal Parameter');
             CancelFileRead := true;
      end;
    end;
  end;

  if Frm_JTAG.ChkTrace.Checked then begin
    S := 'XENDDR     $' + Hex2(Val_XENDDR);
    case Val_XENDDR of
      $00: S := S + '        Run-Test/Idle';
      $01: S := S + '        Pause-DR';
      else S := S + '        Illegal Parameter';
    end;
    Frm_JTAG.MemoJTAG.Lines.Add(S);
  end;

  if not Simulate then begin
    case Val_XENDDR of
      $00: // set XSDRand XSDRTDO end state to Run-Test/Idle
           State_XENDDR := run_test_idle;
      $01: // set XSDRand XSDRTDO end state to Pause-DR
           State_XENDDR := pause_dr;
      else begin
             State_XENDDR := undefined_jtag_state;
             CancelFileRead := true;
      end;
    end;
  end;
  // now do real programming, nothing to do, end state is stored
end;


procedure X_WAIT;
// XWAIT State1<1 byte> State2<1 byte> Time<4 bytes>
// If not already in State1 then go to State1, wait for Time microseconds and
// finally go to State2
var
  State1, State2: byte;
  begin_state, end_state: jtag_state;
  ValArray: array[0..3] of byte;
  Val_Wait: LongWord;
  I: integer;
  S: string;
begin
  BlockRead(JTAGFile, State1, 1);
  BlockRead(JTAGFile, State2, 1);
  BlockRead(JTAGFile, ValArray, 4);
  BytesRead := BytesRead + 1 + 1 + 4;

  if LogFile then begin
    Write(SimFile, 'XWAIT      $' + Hex2(State1) + ' $' + Hex2(State2));
    Write(SimFile, '  ' + TapArray[State1] + ' ' + TapArray[State2] + ' $');
    for i := 0 to 3 do Write(SimFile, Hex2(ValArray[i]));
    WriteLn(SimFile)
  end;

  if Frm_JTAG.ChkTrace.Checked then begin
    S := 'XWAIT      $' + Hex2(State1) + ' $' + Hex2(State2);
    S := S + '  ' + TapArray[State1] + ' ' + TapArray[State2] + ' $';
    for i := 0 to 3 do S := S + Hex2(ValArray[i]);
    Frm_JTAG.MemoJTAG.Lines.Add(S);
  end;

  Val_Wait := ValArray[3] +             256 * ValArray[2] +
                                  256 * 256 * ValArray[1] +
                            256 * 256 * 256 * ValArray[0];
  // now do real work

  if not Simulate then begin
    begin_state := jtag_state(State1);
    end_state := jtag_state(State2);
    if current_state <> begin_state then JTAG_go_state(begin_state);
    sleep(Val_Wait DIV 1000);
    if current_state <> end_state then JTAG_go_state(end_state);
  end;
end;


procedure X_WAIT_core(begin_state, end_state: jtag_state; Val_Wait: LongWord);
// XWAIT State1<1 byte> State2<1 byte> Time<4 bytes>
// If not already in State1 then go to State1, wait for Time microseconds and
// finally go to State2
begin
    if current_state <> begin_state then JTAG_go_state(begin_state);
    sleep(Val_Wait DIV 1000);
    if current_state <> end_state then JTAG_go_state(end_state);
end;

// All following instructions are not used in the MLDL2000

procedure X_SDR;
//   Command not used in the MLDL2000!
begin
  CancelFileRead := true;
end;

procedure X_RESERVED05;
//   Command not used in the MLDL2000!
begin
end;

procedure X_RESERVED06;
//   Command not used in the MLDL2000!
begin
end;

procedure X_SETSDRMASKS;
//   Command not used in the MLDL2000!
begin
end;


procedure X_SDRINC;
//   Command not used in the MLDL2000!
begin
end;


procedure X_SDRB;
//   Command not used in the MLDL2000!
begin
end;


procedure X_SDRC;
//   Command not used in the MLDL2000!
begin
end;


procedure X_SDRE;
//   Command not used in the MLDL2000!
begin
end;


procedure X_SDRTDOB;
//   Command not used in the MLDL2000!
begin
end;


procedure X_SDRTDOC;
//   Command not used in the MLDL2000!
begin
end;


procedure X_SDRTDOE;
//   Command not used in the MLDL2000!
begin
end;


procedure X_SIR2;
//   Command not used in the MLDL2000!
begin
end;


procedure X_COMMENT;
//   Command not used in the MLDL2000!
begin
end;


procedure TFrm_JTAG.BtnProgramClick(Sender: TObject);
// this procedure parses the .XSVF file and executes the XSVF file
var
  SimName: string;
begin
  if not (MessageDlg('DO YOU REALLY WANT TO CHANGE FIRMWARE?',
                      mtConfirmation, [mbYes, mbNo], 0) = mrYes) then Exit;
  CancelFileRead := false;
  if ChkSimulate.Checked then Simulate := true else Simulate := false;
  if ChkLog.Checked then LogFile := true else LogFile := false;

  if JTAGFileOpen then begin
    Reset(JTAGFile, 1);
    BytesRead := 0;
    ProgressJTAG.Position := 0;
    ErrCount := 0;
    Frm_JTAG.PnlError.Caption := 'BUSY';
    Application.ProcessMessages;
  end else begin
    MessageBox(0, 'NO FILE OPEN', nil, MB_OK);
    Exit;
  end;

  if LogFile then begin
    SimName := 'JTAGSimFile.txt';
    AssignFile(SimFile, SimName);
    Rewrite(SimFile);
    Writeln(SimFile, 'Start of JTAG Logging');
  end;

  // Add code to save SRAM block $00
  if not Simulate then ReadSRAMTyp;  // maybe this works ???

  Frm_JTAG.PnlError.Caption := 'BUSY';
  if ChkTrace.Checked then MemoJTAG.Clear;
  if not ChkSimulate.Checked then InitJtagMode(true);
  while (not CancelFileRead) and (not eof(JTAGFile)) do begin
    if ChkDebug.Checked then MessageBox(0, 'NEXT', nil, MB_OK);
    BlockRead(JTAGFile, cmd, 1);
    BytesRead := BytesRead + 1;
    case cmd of
      XCOMPLETE      : X_COMPLETE;                //  $00
      XTDOMASK       : X_TDOMASK;                 //  $01
      XSIR           : X_SIR;                     //  $02
      XSDR           : CancelFileRead := true;    //  $03, not used in MLDL2000
      XRUNTEST       : X_RUNTEST;                 //  $04
      XRESERVED05    : CancelFileRead := true;    //  $05, not used in MLDL2000
      XRESERVED06    : CancelFileRead := true;    //  $06, not used in MLDL2000
      XREPEAT        : X_REPEAT;                  //  $07
      XSDRSIZE       : X_SDRSIZE;                 //  $08
      XSDRTDO        : X_SDRTDO;                  //  $09
      XSETSDRMASKS   : CancelFileRead := true;    //  $0A, not used in MLDL2000
      XSDRINC        : CancelFileRead := true;    //  $0B, not used in MLDL2000
      XSDRB          : CancelFileRead := true;    //  $0C, not used in MLDL2000
      XSDRC          : CancelFileRead := true;    //  $0D, not used in MLDL2000
      XSDRE          : CancelFileRead := true;    //  $0E, not used in MLDL2000
      XSDRTDOB       : CancelFileRead := true;    //  $0F, not used in MLDL2000
      XSDRTDOC       : CancelFileRead := true;    //  $10, not used in MLDL2000
      XSDRTDOE       : CancelFileRead := true;    //  $11, not used in MLDL2000
      XSTATE         : X_STATE;                   //  $12
      XENDIR         : X_ENDIR;                   //  $13
      XENDDR         : X_ENDDR;                   //  $14
      XSIR2          : CancelFileRead := true;    //  $15, not used in MLDL2000
      XCOMMENT       : CancelFileRead := true;    //  $16, not used in MLDL2000
      XWAIT          : X_WAIT;                    //  $17
      else             X_ILLEGAL;
    end; // case

    ProgressJTAG.Position := (BytesRead * 100) DIV JTAGFileSize;
    if ErrCount <> 0 then Frm_JTAG.PnlError.Caption := 'Errors: ' + SStr(ErrCount);
  end;

  // Add code to restore SRAM block $00

  if not ChkSimulate.Checked then InitDefault;

  if CancelFileRead then
    // MessageBox(0, 'CRITICAL JTAG ERROR', nil, MB_ICONEXCLAMATION + MB_APPLMODAL)
    MessageBox(0, 'CRITICAL JTAG ERROR', nil, MB_ICONEXCLAMATION)
  else
    Frm_JTAG.PnlError.Caption := 'DONE';

  ProgressJTAG.Position := 0;
  if LogFile then CloseFile(SimFile);
end;


procedure TFrm_JTAG.BtnOpenClick(Sender: TObject);
var
  FileNm : string;
begin
  if JTAGFileOpen then CloseFile(JTAGFile);
  JTAGFileOpen := false;
  if DlgOpen.Execute then begin
    FileNm := DlgOpen.Filename;
    if OpenJTAGFile(FileNm) > 0 then begin
      PnlStatus.Caption := ExtractFileName(FileNm);
      JTAGFileOpen := true;
    end else
      PnlStatus.Caption := 'Error opening file';
  end;
end;

procedure TFrm_JTAG.BtnDbgEnableClick(Sender: TObject);
// Enable or disbale Debug Functions
// Disabled by default
begin
  if Debug_enabled then begin
    // Debug functions are enabled so now disable
    BtnNext.Enabled := false;
    BtnEnable.Enabled := false;
    ChkSimulate.Enabled := false;
    ChkTrace.Enabled := false;
    ChkDebug.Enabled := false;
    ChkLog.Enabled := false;
    ChkUSBProg.Enabled := false;
    BtnDbgEnable.Caption := 'Enable Debug';
    Debug_Enabled := false;
    // also bring states back to default
    ChkSimulate.Checked := false;
    ChkTrace.Checked := false;
    ChkDebug.Checked := false;
    ChkLog.Checked := false;
    ChkUSBProg.Checked := true;
  end else begin
    // Debug functions are disabled, so now enable
    BtnNext.Enabled := true;
    BtnEnable.Enabled := true;
    ChkSimulate.Enabled := true;
    ChkTrace.Enabled := true;
    ChkDebug.Enabled := true;
    ChkLog.Enabled := true;
    ChkUSBProg.Enabled := true;
    BtnDbgEnable.Caption := 'Disable Debug';
    Debug_Enabled := true;
  end;
end;

procedure TFrm_JTAG.BtnProgUESClick(Sender: TObject);
// Program UES Firware ID
var
  Rslt, Result, Retries: integer;
  i: integer;
  S: string;
begin
  S := 'Firmware version string: "' + EdtUES.Text;
  MemoJTAG.Lines.Add(S);

  DisableMLDL;
  InitJtagMode(true);

  // now program string .....

  // first get IDCode

  Val_XREPEAT := 0;
  JTAG_go_state(test_logic_reset);
  JTAG_go_state(run_test_idle);

  TDI_Array[0] := $01;            //  IDCODE Instruction
  X_SIR_core(5, run_test_idle);

  TDI_Array[0] := 0;
  TDI_Array[1] := 0;
  TDI_Array[2] := 0;
  TDI_Array[3] := 0;

  XTDO_Mask[0] := $0F;
  XTDO_Mask[1] := $FF;
  XTDO_Mask[2] := $80;
  XTDO_Mask[3] := $01;

  XTDO_Expected[0] := $F4;
  XTDO_Expected[1] := $95;
  XTDO_Expected[2] := $FF;
  XTDO_Expected[3] := $FF;

  X_SDRTDO_core($20, run_test_idle, Result, Retries);

  // JTAG_go_state(test_logic_reset);

  TDI_Array[0] := $1F;
  X_SIR_core(5, run_test_idle);

  TDI_Array[0] := $01;            //  IDCODE Instruction
  X_SIR_core(5, run_test_idle);

  X_SDRTDO_core($20, run_test_idle, Result, Retries);

  TDI_Array[0] := $1F;                                 // XSIR $05 $1F
  X_SIR_core(5, run_test_idle);

  TDI_Array[0] := $09;                                 // XSIR $05 $09
  X_SIR_core(5, run_test_idle);

  TDI_Array[0] := $0B;                                 // XENDIR Pause-IR
  X_SIR_core(5, pause_ir);                             // XSIR $05 $0C

  for i := 0 to 63 do begin
    XTDO_Mask[i]     := $00;
    TDI_Array[i]     := $FF;    // specific entries to be filled in
    XTDO_Expected[i] := $00;
  end;

  TDI_Array[0] := $09;
  TDI_Array[1] := $D7;
  TDI_Array[2] := $FF;
  TDI_Array[3] := $F4;
  TDI_Array[4] := $14;
  TDI_Array[5] := $3F;
  TDI_Array[6] := $FF;
  TDI_Array[7] := $FF;
  TDI_Array[8] := $FF;
  TDI_Array[9] := $FF;
  TDI_Array[10] := $FF;
  TDI_Array[11] := $FF;
  TDI_Array[12] := $FF;
  TDI_Array[13] := $FF;
  TDI_Array[14] := $FF;
  TDI_Array[15] := $FF;
  TDI_Array[16] := $FF;
  TDI_Array[17] := $FF;
  TDI_Array[18] := $FF;
  TDI_Array[19] := $FF;
  TDI_Array[20] := $FF;
  TDI_Array[21] := $90;
  TDI_Array[22] := $D1;
  TDI_Array[23] := $11;
  TDI_Array[24] := $51;
  TDI_Array[25] := $91;
  TDI_Array[26] := $D2;
  TDI_Array[27] := $12;
  TDI_Array[28] := $52;
  TDI_Array[29] := $FF;
  TDI_Array[30] := $FF;
  TDI_Array[31] := $FF;
  TDI_Array[32] := $FF;
  TDI_Array[33] := $FF;
  TDI_Array[34] := $FF;
  TDI_Array[35] := $FF;
  TDI_Array[36] := $FF;
  TDI_Array[37] := $FF;
  TDI_Array[38] := $FF;
  TDI_Array[39] := $FF;
  TDI_Array[40] := $FF;
  TDI_Array[41] := $FF;
  TDI_Array[42] := $FF;
  TDI_Array[43] := $FF;
  TDI_Array[44] := $FF;
  TDI_Array[45] := $FF;
  TDI_Array[46] := $FF;
  TDI_Array[47] := $92;
  TDI_Array[48] := $D3;
  TDI_Array[49] := $13;
  TDI_Array[50] := $53;
  TDI_Array[51] := $93;
  TDI_Array[52] := $D4;
  TDI_Array[53] := $14;
  TDI_Array[54] := $54;
  TDI_Array[55] := $94;
  TDI_Array[56] := $D5;
  TDI_Array[57] := $15;
  TDI_Array[58] := $55;
  TDI_Array[59] := $95;
  TDI_Array[60] := $D6;
  TDI_Array[61] := $16;
  TDI_Array[62] := $56;
  TDI_Array[63] := $98;
  TDI_Array[64] := $58;

  X_SDRTDO_core($204, run_test_idle, Result, Retries);
    // aantal bits $204 =516 bits = 65 bytes

  Rslt := Result + Retries;

  X_WAIT_core(run_test_idle, run_test_idle, $2710);

  TDI_Array[0] := $09;
  TDI_Array[1] := $DF;
  TDI_Array[2] := $FF;
  TDI_Array[3] := $F9;
  TDI_Array[4] := $8D;
  TDI_Array[5] := $9F;
  TDI_Array[6] := $FF;
  TDI_Array[7] := $FF;
  TDI_Array[8] := $FF;
  TDI_Array[9] := $FF;
  TDI_Array[10] := $FF;
  TDI_Array[11] := $FF;
  TDI_Array[12] := $FF;
  TDI_Array[13] := $FF;
  TDI_Array[14] := $FF;
  TDI_Array[15] := $FF;
  TDI_Array[16] := $FF;
  TDI_Array[17] := $FF;
  TDI_Array[18] := $FF;
  TDI_Array[19] := $FF;
  TDI_Array[20] := $FF;
  TDI_Array[21] := $C6;
  TDI_Array[22] := $56;
  TDI_Array[23] := $66;
  TDI_Array[24] := $76;
  TDI_Array[25] := $86;
  TDI_Array[26] := $96;
  TDI_Array[27] := $A6;
  TDI_Array[28] := $B6;
  TDI_Array[29] := $FF;
  TDI_Array[30] := $FF;
  TDI_Array[31] := $FF;
  TDI_Array[32] := $FF;
  TDI_Array[33] := $FF;
  TDI_Array[34] := $FF;
  TDI_Array[35] := $FF;
  TDI_Array[36] := $FF;
  TDI_Array[37] := $FF;
  TDI_Array[38] := $FF;
  TDI_Array[39] := $FF;
  TDI_Array[40] := $FF;
  TDI_Array[41] := $FF;
  TDI_Array[42] := $FF;
  TDI_Array[43] := $FF;
  TDI_Array[44] := $FF;
  TDI_Array[45] := $FF;
  TDI_Array[46] := $FF;
  TDI_Array[47] := $C6;
  TDI_Array[48] := $D6;
  TDI_Array[49] := $6E;
  TDI_Array[50] := $6F;
  TDI_Array[51] := $70;
  TDI_Array[52] := $71;
  TDI_Array[53] := $72;
  TDI_Array[54] := $73;
  TDI_Array[55] := $94;
  TDI_Array[56] := $74;
  TDI_Array[57] := $75;
  TDI_Array[58] := $76;
  TDI_Array[59] := $77;
  TDI_Array[60] := $78;
  TDI_Array[61] := $79;
  TDI_Array[62] := $7A;
  TDI_Array[63] := $30;
  TDI_Array[64] := $31;

  X_SDRTDO_core($204, run_test_idle, Result, Retries);

  X_WAIT_core(run_test_idle, run_test_idle, $2710);

  TDI_Array[0] := $0D;                                 // XSIR $05 $0D
  X_SIR_core(5, run_test_idle);

  X_WAIT_core(run_test_idle, run_test_idle, $C8);

  TDI_Array[0] := $10;                                 // XSIR $05 $10
  X_SIR_core(5, run_test_idle);

  X_WAIT_core(run_test_idle, run_test_idle, $64);

  TDI_Array[0] := $1F;                                 // XSIR $05 $1F
  X_SIR_core(5, run_test_idle);

  Val_XREPEAT := $20;
  JTAG_go_state(test_logic_reset);
  JTAG_go_state(run_test_idle);

  TDI_Array[0] := $1F;                                 // XSIR $05 $1F
  X_SIR_core(5, run_test_idle);

  XTDO_Mask[0]     := $00;
  TDI_Array[0]     := $00;    // specific entries to be filled in
  XTDO_Expected[0] := $00;

  X_SDRTDO_core($1, run_test_idle, Result, Retries);

  X_COMPLETE;

  InitDefault;
  EnableMLDL;


end;

procedure TFrm_JTAG.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  Ini: tMemIniFile;
begin
  Ini := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.INI' ));
  try
    Ini.WriteInteger( 'WindowsPositions', 'JTAGTop', Frm_JTAG.Top);
    Ini.WriteInteger( 'WindowsPositions', 'JTAGLeft', Frm_JTAG.Left);
  finally
    Ini.Free;
  end;
end;




procedure TFrm_JTAG.BtnDevIDClick(Sender: TObject);
// Read CPLD Device ID
var
  Result, Retries: integer;
  i: integer;
  S: string;
begin
  // Set JTAG Mode
  DisableMLDL;
  InitJtagMode(true);

  // First reset JTAG STate
  current_state := undefined_jtag_state;
  JTAG_go_state(test_logic_reset);

  Val_XREPEAT := 0;

  JTAG_go_state(run_test_idle);

  TDI_Array[0] := $01;            //  IDCODE Instruction
  X_SIR_core(5, run_test_idle);

  TDI_Array[0] := 0;
  TDI_Array[1] := 0;
  TDI_Array[2] := 0;
  TDI_Array[3] := 0;

  XTDO_Mask[0] := $0F;
  XTDO_Mask[1] := $FF;
  XTDO_Mask[2] := $80;
  XTDO_Mask[3] := $01;

  XTDO_Expected[0] := $F4;
  XTDO_Expected[1] := $95;
  XTDO_Expected[2] := $FF;
  XTDO_Expected[3] := $FF;

  X_SDRTDO_core($20, run_test_idle, Result, Retries);

  JTAG_go_state(test_logic_reset);

  S := 'IDCODE      : $';
  for i := 0 to 3 do S := S + Hex2(TDO_Array[i]);
  S := S + '   Retries: ' + SStr(Retries);
  S := S + '   Errors : ' + SStr(Result);
  Frm_JTAG.MemoJTAG.Lines.Add(S);

  InitDefault;
  EnableMLDL;
end;


procedure TFrm_JTAG.ReadFWID(var IDCODE: string; var FWID: string; var Rslt: integer);
// Returns the following values:
//   IDCODE: Device ID Code (should always be $0495B093)
//   FWID  : Firmware version, this is the UES string
//   Rslt  : 0 if no errors occured, otherwise contains the number of JTAG errors
//           if there are errors the strings will usually be empty
var
  Result, Retries, Retr, Err: integer;
  i: integer;
  S: string;
  UES_array: array[0..100] of word;
begin
  // Set JTAG Mode
  IDCODE := '';
  FWID := '';
  DisableMLDL;
  InitJtagMode(true);
  Retr := 0;
  Retries := 0;
  Err := 0;


  // First reset JTAG STate
  current_state := undefined_jtag_state;
  JTAG_go_state(test_logic_reset);

  Val_XREPEAT := 0;

  JTAG_go_state(run_test_idle);

  TDI_Array[0] := $01;            //  IDCODE Instruction
  X_SIR_core(5, run_test_idle);

  Val_XRUNTEST := 0;

  TDI_Array[0] := 0;
  TDI_Array[1] := 0;
  TDI_Array[2] := 0;
  TDI_Array[3] := 0;

  XTDO_Mask[0] := $0F;
  XTDO_Mask[1] := $FF;
  XTDO_Mask[2] := $80;
  XTDO_Mask[3] := $01;

  XTDO_Expected[0] := $F4;
  XTDO_Expected[1] := $95;
  XTDO_Expected[2] := $FF;
  XTDO_Expected[3] := $FF;

  X_SDRTDO_core($20, run_test_idle, Result, Retries);

  S := '$';
  for i := 0 to 3 do S := S + Hex2(TDO_Array[i]);
  Rslt := Retries + Result;

  TDI_Array[0] := $1F;                                 // XSIR $05 $1F
  X_SIR_core(5, run_test_idle);

  TDI_Array[0] := $09;                                 // XSIR $05 $09
  X_SIR_core(5, run_test_idle);

  TDI_Array[0] := $0C;                                 // XENDIR Pause-IR
  X_SIR_core(5, pause_ir);                             // XSIR $05 $0C

  TDI_Array[01] := $01;
  TDI_Array[00] := $3A;

  XTDO_Mask[0] := $00;
  XTDO_Mask[1] := $00;

  XTDO_Expected[0] := $00;
  XTDO_Expected[1] := $00;

  X_SDRTDO_core($09, pause_dr, Result, Retries);       //XSRTDO $013A $0000

  Rslt := Rslt + Result + Retries;

  X_WAIT_core(pause_dr, pause_dr, $14);

  X_WAIT_core(run_test_idle, run_test_idle, 1);

  TDI_Array[0] := $7F;
  XTDO_Mask[0] := $00;
  XTDO_Expected[0] := $00;
  for i := 1 to 63 do begin
    TDI_Array[i] := $FF;
    XTDO_Mask[i] := $00;
    XTDO_Expected[i] := $00;
  end;

  X_SDRTDO_core($1FB, run_test_idle, Result, Retries);

  UES_array[00] := ((TDO_Array[2] and $0F) shl 4) or ((TDO_Array[3] and $F0) shr 4);
  UES_array[01] := ((TDO_Array[3] and $0F) shl 4) or ((TDO_Array[4] and $E0) shr 4)
                     or ((TDO_Array[20] and $40) shr 6);
  for i := 2 to 8 do begin
    UES_array[i] := ((TDO_Array[i + 18] and $3F) shl 2) or ((TDO_Array[i + 19] and $C0) shr 6);
  end;
  // fix last byte
  UES_array[9] := ((TDO_Array[27] and $3F) shl 2) or ((TDO_Array[46] and $C0) shr 6);
  for i := 10 to 26 do begin
    UES_array[i] := ((TDO_Array[i + 36] and $3F) shl 2) or ((TDO_Array[i + 37] and $C0) shr 6);
  end;
  // Last 6 bits are the start of the next string, so keep these ....
  UES_array[27] := (TDO_Array[63] and $3F) shl 2;

  Rslt := Rslt + Result + Retries;

  X_WAIT_core(run_test_idle, run_test_idle, 1);

  TDI_Array[01] := $01;
  TDI_Array[00] := $3B;

  XTDO_Mask[0] := $00;
  XTDO_Mask[1] := $00;

  XTDO_Expected[0] := $00;
  XTDO_Expected[1] := $00;

  X_SDRTDO_core($09, run_test_idle, Result, Retries);

  TDI_Array[0] := $7F;
  XTDO_Mask[0] := $00;
  XTDO_Expected[0] := $00;
  for i := 1 to 63 do begin
    TDI_Array[i] := $FF;
    XTDO_Mask[i] := $00;
    XTDO_Expected[i] := $00;
  end;

  X_SDRTDO_core($1FB, run_test_idle, Result, Retries);

  UES_array[27] := UES_array[27] or ((TDO_Array[2] and $0C) shr 2);
  UES_array[28] := ((TDO_Array[2] and $03) shl 6) or ((TDO_Array[3] and $FC) shr 2);
  UES_array[29] := ((TDO_Array[3] and $03) shl 6) or ((TDO_Array[4] and $E0) shr 2)
                   or ((TDO_Array[20] and $70) shr 4) ;
  for i := 30 to 36 do
    UES_array[i] := ((TDO_Array[i -10 ] and $0F) shl 4)
                    or ((TDO_Array[i - 9] and $F0) shr 4);
  // fix last byte
  UES_array[37] := ((TDO_Array[27] and $0F) shl 4) or ((TDO_Array[46] and $F0) shr 4);
  for i := 38 to 53 do
    UES_array[i] := ((TDO_Array[i + 8] and $0F) shl 4)
                    or ((TDO_Array[i + 9] and $F0) shr 4);


  Retr := Retr + Retries;
  Err := Err + Result;
  Rslt := Rslt + Retr + Err;

  S := '';
  for i := 0 to 53 do
    if ((UES_array[i] > 31) and (UES_array[i] < 127)) then S := S + Chr(UES_array[i]);

  FWID := S;

  X_WAIT_core(run_test_idle, run_test_idle, $64);

  TDI_Array[0] := $0D;
  X_SIR_core(5, run_test_idle);

  X_WAIT_core(run_test_idle, run_test_idle, $C8);

  TDI_Array[0] := $10;
  X_SIR_core(5, run_test_idle);

  X_WAIT_core(run_test_idle, run_test_idle, $64);

  TDI_Array[0] := $1F;
  X_SIR_core(5, run_test_idle);

  Val_XREPEAT := $00;

  Val_XREPEAT := $20;

  JTAG_go_state(test_logic_reset);

  JTAG_go_state(run_test_idle);

  InitDefault;
  EnableMLDL;
end;



procedure TFrm_JTAG.BtnFWIDClick(Sender: TObject);
// Read Firmware Version: Xilinx User String
var
  Result, Retries, Retr, Err: integer;
  i: integer;
  S, SS: string;
  UES_array: array[0..100] of byte;
begin
  // Set JTAG Mode
  DisableMLDL;
  InitJtagMode(true);

  // First reset JTAG STate
  current_state := undefined_jtag_state;
  JTAG_go_state(test_logic_reset);

  Val_XREPEAT := 0;

  JTAG_go_state(run_test_idle);

  TDI_Array[0] := $01;            //  IDCODE Instruction
  X_SIR_core(5, run_test_idle);

  Val_XRUNTEST := 0;

  TDI_Array[0] := 0;
  TDI_Array[1] := 0;
  TDI_Array[2] := 0;
  TDI_Array[3] := 0;

  XTDO_Mask[0] := $0F;
  XTDO_Mask[1] := $FF;
  XTDO_Mask[2] := $80;
  XTDO_Mask[3] := $01;

  XTDO_Expected[0] := $F4;
  XTDO_Expected[1] := $95;
  XTDO_Expected[2] := $FF;
  XTDO_Expected[3] := $FF;

  X_SDRTDO_core($20, run_test_idle, Result, Retries);

  S := '';
  Frm_JTAG.MemoJTAG.Lines.Add(S);
  S := 'IDCODE      : $';
  for i := 0 to 3 do S := S + Hex2(TDO_Array[i]);
  S := S + '   Retries: ' + SStr(Retries);
  S := S + '   Errors : ' + SStr(Result);

  Frm_JTAG.MemoJTAG.Lines.Add(S);

  TDI_Array[0] := $1F;
  X_SIR_core(5, run_test_idle);

  TDI_Array[0] := $09;
  X_SIR_core(5, run_test_idle);

  TDI_Array[0] := $0C;
  X_SIR_core(5, pause_ir);

  TDI_Array[01] := $01;
  TDI_Array[00] := $3A;

  XTDO_Mask[0] := $00;
  XTDO_Mask[1] := $00;

  XTDO_Expected[0] := $00;
  XTDO_Expected[1] := $00;

  X_SDRTDO_core($09, pause_dr, Result, Retries);

  X_WAIT_core(pause_dr, pause_dr, $14);

  X_WAIT_core(run_test_idle, run_test_idle, 1);

  TDI_Array[0] := $7F;
  XTDO_Mask[0] := $00;
  XTDO_Expected[0] := $00;
  for i := 1 to 63 do begin
    TDI_Array[i] := $FF;
    XTDO_Mask[i] := $00;
    XTDO_Expected[i] := $00;
  end;

  X_SDRTDO_core($1FB, run_test_idle, Result, Retries);

  UES_array[00] := ((TDO_Array[2] and $0F) shl 4) or ((TDO_Array[3] and $F0) shr 4);
  UES_array[01] := ((TDO_Array[3] and $0F) shl 4) or ((TDO_Array[4] and $E0) shr 4)
                     or ((TDO_Array[20] and $40) shr 6);
  for i := 2 to 8 do begin
    UES_array[i] := ((TDO_Array[i + 18] and $3F) shl 2) or ((TDO_Array[i + 19] and $C0) shr 6);
  end;
  // fix last byte
  UES_array[9] := ((TDO_Array[27] and $3F) shl 2) or ((TDO_Array[46] and $C0) shr 6);
  for i := 10 to 26 do begin
    UES_array[i] := ((TDO_Array[i + 36] and $3F) shl 2) or ((TDO_Array[i + 37] and $C0) shr 6);
  end;
  // Last 6 bits are the start of the next string, so keep these ....
  UES_array[27] := (TDO_Array[63] and $3F) shl 2;

  Retr := Retries;
  Err := Result;

  S := 'Firmware ID : "';

  X_WAIT_core(run_test_idle, run_test_idle, 1);

  TDI_Array[01] := $01;
  TDI_Array[00] := $3B;

  XTDO_Mask[0] := $00;
  XTDO_Mask[1] := $00;

  XTDO_Expected[0] := $00;
  XTDO_Expected[1] := $00;

  X_SDRTDO_core($09, run_test_idle, Result, Retries);

  TDI_Array[0] := $7F;
  XTDO_Mask[0] := $00;
  XTDO_Expected[0] := $00;
  for i := 1 to 63 do begin
    TDI_Array[i] := $FF;
    XTDO_Mask[i] := $00;
    XTDO_Expected[i] := $00;
  end;

  X_SDRTDO_core($1FB, run_test_idle, Result, Retries);

  UES_array[27] := UES_array[27] or ((TDO_Array[2] and $0C) shr 2);
  UES_array[28] := ((TDO_Array[2] and $03) shl 6) or ((TDO_Array[3] and $FC) shr 2);
  UES_array[29] := ((TDO_Array[3] and $03) shl 6) or ((TDO_Array[4] and $E0) shr 2)
                   or ((TDO_Array[20] and $70) shr 4) ;
  for i := 30 to 36 do
    UES_array[i] := ((TDO_Array[i -10 ] and $0F) shl 4)
                    or ((TDO_Array[i - 9] and $F0) shr 4);
  // fix last byte
  UES_array[37] := ((TDO_Array[27] and $0F) shl 4) or ((TDO_Array[46] and $F0) shr 4);
  for i := 38 to 53 do
    UES_array[i] := ((TDO_Array[i + 8] and $0F) shl 4)
                    or ((TDO_Array[i + 9] and $F0) shr 4);

  Retr := Retr + Retries;
  Err := Err + Result;
  SS := 'Read Results: Retries: ' + SStr(Retr);
  SS := SS + '   Errors : ' + SStr(Err);
  Frm_JTAG.MemoJTAG.Lines.Add(SS);
  for i := 0 to 53 do S := S + Asc(UES_array[i]);
  S := S + '"';
  Frm_JTAG.MemoJTAG.Lines.Add(S);

  X_WAIT_core(run_test_idle, run_test_idle, $64);

  TDI_Array[0] := $0D;
  X_SIR_core(5, run_test_idle);

  X_WAIT_core(run_test_idle, run_test_idle, $C8);

  TDI_Array[0] := $10;
  X_SIR_core(5, run_test_idle);

  X_WAIT_core(run_test_idle, run_test_idle, $64);

  TDI_Array[0] := $1F;
  X_SIR_core(5, run_test_idle);

  Val_XREPEAT := $00;

  Val_XREPEAT := $20;

  JTAG_go_state(test_logic_reset);

  JTAG_go_state(run_test_idle);

  InitDefault;
  EnableMLDL;
end;


procedure TFrm_JTAG.FormCreate(Sender: TObject);
 var
  Ini: TMemIniFile;
const
  T = -1;  // default Top
  L = -1;  // default Left
begin
  Ini := TMemIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
  try
    Frm_JTAG.Top:= Ini.ReadInteger( 'WindowsPositions', 'JTAGTop', T);
    Frm_JTAG.Left:= Ini.ReadInteger( 'WindowsPositions', 'JTAGLeft', L);

    if not WinVisible(Frm_JTAG) then begin
      // window outside current screen? Set to defaults
      Frm_JTAG.Top  := T;
      Frm_JTAG.Left := L;
    end;

  finally
    Ini.Free
  end;
end;


procedure TFrm_JTAG.FormDestroy(Sender: TObject);
var
  Ini: TMemIniFile;
begin
  if JTAGFileOpen then CloseFile(JTAGFile);
  JTAGFileOpen := false;
end;

initialization
  {$i Jtag.lrs}

end.
