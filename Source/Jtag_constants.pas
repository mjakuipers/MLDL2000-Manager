unit Jtag_constants;

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
//  JTAG_CONSTANTS.PAS                                                       //
//  Constants and vriables used for JTAG programming                         //
//  Ver  Date     Description                                                //
//  1.00 Nov 2005 First version for release                                  //
//  1.01 Nov 2006 Compiled for Turbo Explorer                                //
//  1.50 May 2008 Final release                                              //
//  1.70 Jul 2010 Adapted for Lazarus                                        //
//  1.80 Feb 2018 restarted development                                      //
//                removed Delphi compatibility                               //
//  1.90 Feb 2020 Fixed array referencing data_type                          //
//---------------------------------------------------------------------------//

interface

uses Globals, Types;

const USBBuffSize : integer = $8000;

  //======================================================================
  // go to ->                                    tlr rti pdr pir sdr sir
  //======================================================================
  from_test_logic_reset : array[0..5] of byte = ($01,$00,$0a,$16,$02,$06);
  from_run_test_idle    : array[0..5] of byte = ($07,$00,$05,$0b,$01,$03);
  from_pause_dr         : array[0..5] of byte = ($1f,$03,$17,$2f,$01,$0f);
  from_pause_ir         : array[0..5] of byte = ($1f,$03,$17,$2f,$07,$01);
  from_shift_dr         : array[0..5] of byte = ($1f,$03,$01,$2f,$00,$00);
  from_shift_ir         : array[0..5] of byte = ($1f,$03,$17,$01,$00,$00);
  //======================================================================
  // with this number of clocks
  //======================================================================
  from_test_logic_resetc : array[0..5] of byte = (1,1,5,6,4,5);
  from_run_test_idlec    : array[0..5] of byte = (3,5,4,5,3,4);
  from_pause_drc         : array[0..5] of byte = (5,3,6,7,2,6);
  from_pause_irc         : array[0..5] of byte = (5,3,6,7,5,2);
  from_shift_drc         : array[0..5] of byte = (5,3,2,7,0,0);
  from_shift_irc         : array[0..5] of byte = (5,4,6,2,0,0);

  ir_bdry_scan  = $00;
  ir_samp_scan  = $82;
  ir_bypass     = $04;

  //======================================================================

// XSVF Instructions

  XCOMPLETE      = $00;
  XTDOMASK       = $01;
  XSIR           = $02;
  XSDR           = $03;
  XRUNTEST       = $04;
  XRESERVED05    = $05;
  XRESERVED06    = $06;
  XREPEAT        = $07;
  XSDRSIZE       = $08;
  XSDRTDO        = $09;
  XSETSDRMASKS   = $0A;
  XSDRINC        = $0B;
  XSDRB          = $0C;
  XSDRC          = $0D;
  XSDRE          = $0E;
  XSDRTDOB       = $0F;
  XSDRTDOC       = $10;
  XSDRTDOE       = $11;
  XSTATE         = $12;
  XENDIR         = $13;
  XENDDR         = $14;
  XSIR2          = $15;
  XCOMMENT       = $16;
  XWAIT          = $17;

  CommandArray: array[$0..$17] of string = (
    'XCOMPLETE    ',  // $00
    'XTDOMASK     ',  // $01
    'XSIR         ',  // $02
    'XSDR         ',  // $03
    'XRUNTEST     ',  // $04
    'reserved $05 ',  // $05
    'reserved $06 ',  // $06
    'XREPEAT      ',  // $07
    'XSDRSIZE     ',  // $08
    'XSDRTDO      ',  // $09
    'XSETSDRMASKS ',  // $0A
    'XSDRINC      ',  // $0B
    'XSDRB        ',  // $0C
    'XSDRC        ',  // $0D
    'XSDRE        ',  // $0E
    'XSDRTDOB     ',  // $0F
    'XSDRTDOC     ',  // $10
    'XSDRTDOE     ',  // $11
    'XSTATE       ',  // $12
    'XENDIR       ',  // $13
    'XENDDR       ',  // $14
    'XSIR2        ',  // $15
    'XCOMMENT     ',  // $16
    'XWAIT        '   // $17
    );

  TapArray: array[$0..$0F] of string = (
    'RESET       ',   // $00
    'RUNTEST/IDLE',   // $01
    'DRSELET     ',   // $02
    'DRCAPTURE   ',   // $03
    'DRSHIFT     ',   // $04
    'DREXIT1     ',   // $05
    'DRPAUSE     ',   // $06
    'DREXIT2     ',   // $07
    'DRUPDATE    ',   // $08
    'IRSELECT    ',   // $09
    'IRCAPTURE   ',   // $0A
    'IRSHIFT     ',   // $0B
    'IREXIT1     ',   // $0C
    'IRPAUSE     ',   // $0D
    'IREXIT2     ',   // $0E
    'IRUPDATE    '    // $0F
    );

type
  jtag_state = byte;

const
  test_logic_reset      = $00;
  run_test_idle         = $01;
  select_dr             = $02;
  capture_dr            = $03;
  shift_dr              = $04;
  exit1_dr              = $05;
  pause_dr              = $06;
  exit2_dr              = $07;
  update_dr             = $08;
  select_ir             = $09;
  capture_ir            = $0A;
  shift_ir              = $0B;
  exit1_ir              = $0C;
  pause_ir              = $0D;
  exit2_ir              = $0E;
  update_ir             = $0F;
  undefined_jtag_state  = $FF;


type
  shift_register = (instruction_register, data_register);
  data_type      = array[0..$fffe] of byte;
  data_type_ptr  = ^data_type;
  Names          = array[1..20] of string;
  Names_Ptr      = ^Names;

var
  My_Names       : Names;
  PortAIsOpen    : Boolean;
  OutIndex       : integer;
  ImageData      : array[0..63] of word;
  CalTime        : boolean;
  SavedSpeed     : string;
  SavedBits      : string;

  UARTIsOpen     : Boolean;
  FT_HANDLE_JTAG : DWord = 0;
  FT_HANDLE_UART : DWord = 0;

  //======================================================================
  current_state  : jtag_state = $FF;
  data_out,
  data_in        : data_type;    // changed to fix compile error
  SavedLowVal    : integer;
  SavedLowDir    : integer;
  SavedHighVal   : integer;
  SavedHighDir   : integer;

  //======================================================================

{
  Test-Logic-Reset	TLR
  Run-Test-Idle     RTI
  Select-DR-Scan    SDS
  Capture-DR        CDR
  Shift-DR          SDR
  Exit1-DR          E1D
  Pause-DR          PDR
  Exit2-DR          E2D
  Update-DR         UDR
  Select-IR-Scan    SIS
  Capture-IR        CIR
  Shift-IR          SIR
  Exit1-IR          E1I
  Pause-IR          PIR
  Exit2-IR          E2I
  Update-IR         UIR

}

type
  StateArray = array[0..15, 0..15] of byte;

const
  StArray: StateArray =
    //      TLR RTI SDS CDR SDR E1D PDR E2D  UDR SIS CIR SIR E1I PIR E2I UIR
    {TLR}	(($01,$00,$02,$02,$02,$0A,$0A,$2A, $1A,$06,$06,$06,$16,$16,$56,$36),
    {RTI}	 ($07,$00,$01,$01,$01,$05,$05,$15, $0D,$03,$03,$03,$0B,$0B,$2B,$1B),
    {SDS}	 ($03,$03,$0B,$00,$00,$02,$02,$0A, $06,$01,$01,$01,$05,$05,$15,$0D),
    {CDR}	 ($1F,$03,$0B,$0B,$00,$01,$01,$05, $03,$0F,$0F,$0F,$2F,$2F,$AF,$6F),
    {SDR}	 ($1F,$03,$07,$07,$00,$01,$01,$05, $03,$0F,$0F,$0F,$2F,$2F,$AF,$6F),
    {E1D}	 ($0F,$01,$05,$05,$02,$0A,$00,$02, $01,$07,$07,$07,$17,$17,$57,$37),
    {PDR}	 ($1F,$03,$07,$07,$01,$05,$00,$01, $03,$0F,$0F,$0F,$2F,$2F,$AF,$6F),
    {E2D}	 ($0F,$01,$03,$03,$00,$02,$02,$0A, $01,$07,$07,$07,$17,$17,$57,$37),
    {UDR}	 ($07,$00,$01,$01,$01,$05,$05,$15, $0D,$03,$03,$03,$0B,$0B,$2B,$1B),
    {SIS}	 ($01,$01,$05,$05,$05,$15,$15,$55, $35,$0D,$00,$00,$02,$02,$0A,$06),
    {CIR}	 ($1F,$03,$07,$07,$07,$17,$17,$57, $37,$0F,$0F,$00,$01,$01,$05,$03),
    {SIR}	 ($1F,$03,$07,$07,$07,$17,$17,$57, $37,$0F,$0F,$00,$01,$01,$05,$03),
    {E1I}	 ($0F,$01,$03,$03,$03,$0B,$0B,$2B, $1B,$07,$07,$02,$0A,$00,$02,$01),
    {PIR}	 ($1F,$03,$07,$07,$07,$17,$17,$57, $37,$0F,$0F,$01,$05,$00,$01,$03),
    {E2I}	 ($0F,$01,$03,$03,$03,$0B,$0B,$2B, $1B,$07,$07,$00,$02,$02,$0A,$01),
    {UIR}	 ($07,$00,$01,$01,$01,$05,$05,$15, $0D,$03,$03,$03,$0B,$0B,$2B,$1B));

  NumBitsArray: StateArray =
    //	    TLR RTI SDS CDR SDR E1D PDR E2D  UDR SIS CIR SIR E1I PIR E2I UIR
    {TLR}	(( 1 , 1 , 2 , 3 , 4 , 4 , 5 , 6 ,	5	, 3	, 4	, 5	, 5	, 6	, 7	, 6 ),
    {RTI}	 ( 3 , 1 , 1 , 2 , 3 , 3 , 4 , 5 ,	4	, 2	, 3	, 4	, 4	, 5	, 6	, 5 ),
    {SDS}	 ( 2 , 3 , 4 , 1 , 2 , 2 , 3 , 4 ,	3	, 1	, 2	, 3	, 3	, 4	, 5 , 4 ),
    {CDR}	 ( 5 , 3 , 4 , 5 , 1 , 1 , 2 , 3 ,	2	, 4	, 5	, 6	, 6	, 7	, 8	, 7 ),
    {SDR}	 ( 5 , 3 , 3 , 4 , 1 , 1 , 2 , 3 ,	2	, 4	, 5	, 6	, 6	, 7	, 8	, 7 ),
    {E1D}	 ( 4 , 2 , 3 , 4 , 3 , 4 , 1 , 2 ,	1	, 3	, 4	, 5	, 5	, 6	, 7	, 6 ),
    {PDR}	 ( 5 , 3 , 3 , 4 , 2 , 3 , 1 , 1 ,	2	, 4	, 5	, 6	, 6	, 7	, 8	, 7 ),
    {E2D}	 ( 4 , 2 , 2 , 3 , 1 , 2 , 3 , 4 ,	1	, 3	, 4	, 5	, 5	, 6	, 7	, 6 ),
    {UDR}	 ( 3 , 1 , 1 , 2 , 3 , 3 , 4 , 5 ,	4	, 2	, 3	, 4	, 4	, 5	, 6	, 5 ),
    {SIS}	 ( 1 , 2 , 3 , 4 , 5 , 5 , 6 , 7 ,	6	, 4	, 1	, 2	, 2	, 3	, 4	, 3 ),
    {CIR}	 ( 5 , 3 , 3 , 4 , 5 , 5 , 6 , 7 ,	6	, 4	, 5	, 1	, 1	, 2	, 3	, 2 ),
    {SIR}	 ( 5 , 3 , 3 , 4 , 5 , 5 , 6 , 7 ,	6	, 4	, 5	, 1	, 1	, 2	, 3	, 2 ),
    {E1I}	 ( 4 , 2 , 2 , 3 , 4 , 4 , 5 , 6 ,	5	, 3	, 4	, 3	, 4	, 1	, 2	, 1 ),
    {PIR}	 ( 5 , 3 , 3 , 4 , 5 , 5 , 6 , 7 ,	6	, 4	, 5	, 2	, 3	, 1	, 1	, 2 ),
    {E2I}	 ( 4 , 2 , 2 , 3 , 4 , 4 , 5 , 6 ,	5	, 3	, 4	, 1	, 2	, 3	, 4	, 1 ),
    {UIR}	 ( 3 , 1 , 1 , 2 , 3 , 3 , 4 , 5 ,	4	, 2	, 3	, 4	, 4	, 5	, 6	, 5 ));


implementation

end.
