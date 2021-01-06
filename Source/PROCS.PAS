unit Procs;

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
//  PROCS.PAS                                                                //
//  Various functions and procedures used in M2kM                            //
//  Ver  Date     Description                                                //
//  1.00 Nov 2005 First version for release                                  //
//  1.01 Nov 2006 Compiled for Turbo Explorer                                //
//  1.02 Apr 2007 Added function for HP User Characters                      //
//  1.20 Apr 2008 Added functions to assist disassembler comments            //
//                Added FileCopy                                             //
//  1.50 May 2008 Final release                                              //
//  1.51 Sep 2009 Added variation of ExpandSpaces                            //
//  1.60 Jan 2010 Modified for new disassembler                              //
//  1.61 Jan 2010 Changes char mapping functions for halfnut support         //
//  1.70 Jul 2010 Adapted for Lazarus                                        //
//  1.80 Feb 2018 restarted development                                      //
//                removed Delphi compatibility                               //
//  1.90 Mar 2020 added WinVisible function                                  //
//---------------------------------------------------------------------------//

interface

uses
  Forms, LCLIntf, Controls, Globals, SysUtils, StrUtils, Classes, HP41_Globals,
  Windows, Graphics, Dialogs, StdCtrls, ComCtrls, LResources;

function StrStr(Len: Integer; ch: Char): AnyString;
// Returns a string wil 'Len' times the character 'ch'

function Hex1(Nyb: longword): Char;
// Returns one Hex character representing the value of 'Nyb'

function Hex2(Hex: word): String2;
// Returns 2 Hex characters representing the value of 'Hex'

function Hex3(Hex: word): String3;
// Returns 3 Hex characters representing the value of 'Hex'

function Hex4(Hex: word): String4;
// Returns 4 Hex characters representing the value of 'Hex'

function Hex5(Hex: LongWord): String;
// Returns 5 Hex characters representing the value of 'Hex'

function HexN(Hex: Longword; N: integer): String;
// Returns N Hex characters representing the value of 'Hex'

function SStr(I: Integer): String;
// wrapper for the Str procedure

function VVal(S: string): integer;
// wrapper for the Val procedure

function Int3(I: Integer): String3;
// Returns a string with the value of I with 3 digits

function Int2(I: Integer): String2;
// Returns a string with the value of I with 2 digits

function Bin4(Hex: word): String4;
// Returns a binary string with the value of Hex with 4 bits

function Bin8(Hex: word): String8;
// Returns a binary string with the value of Hex with 8 bits

function Bin16(Hex: word): String16;
// Returns a binary string with the value of Hex with 16 bits and a '.' as divider

function Bin10(Hex: word): String10;
// Returns a binary string with the value of Hex with 10 bits

function Bin12(Hex: word): String12;
// Returns a binary string with the value of Hex with 12 bits

function Asc(Hex: Byte): Char;
// Returns a printable Character for the value Hex and a '.' if non-printable

function HPChar(W: word): string;
// Returns a HP Character for the value Hex

function HPCharP(W: word): string;
// Returns a HP Character for the value Hex, including punctuation bits

function HPCharD(W: word): string;
// same as HPCharP, but returns . for non printable chars

function HPUserChar(W: word): Char;
// Returns a HP Character for the Value HEX for UserCode

function HPUserCharS(W: word): string;
// as above, returnes hexcodes for non-printeable characters

function HexToInt(var Line: string): Integer;
// Converts a string, representing a hex number to its integer value
// HexToInt will find the first valid hex digit and continue until the
// first non-valid hex digit. Hex digits may be in upper- or lowercase.
// returns -1 in case of an error
// leading spaces and control characters are ignored
// the hex digits will be removed from the string

function HexToByte(var Line: string; var Rslt: boolean): Byte;
// converts the first valid hex characters in a string to a byte
// removes everything inluding the two byte from the string
// If there is only one hex digit this wil be converted

function BinToInt(var Line: string): Integer;
// Converts a string, representing a binary number to its integer value
// HexToInt will find the first valid binary digit and continue until the
// first non-valid binary digit.
// returns -1 in case of an error
// leading spaces and control characters are ignored
// the binary digits will be removed from the string

function IntToHex(I: integer): String;
// Converts a number to its hexadecimal representation
// I is the number to be converted, N the number of characters

function GetFirstWord(var Line: string): string;
// Returnes the first complete word

procedure ExpandSpaces(var Line: string; N: integer);
// expands the string Line with spaces until the length is N
// will do nothing if string is already longer or when N >255

function ExpandSp(Line: string; N: integer): string;
// expands the string Line with spaces until the length is N
// will do nothing if string is already longer or when N>255 or N=0
// when <0 will truncate the line to the specified number of chars

procedure RepString(Source: string; var Dest: string; Pos: integer);

procedure ReplaceSpaces(var Line: string);
// replace all spaces in a string by underscores to clean up labels

procedure RemoveSpaces(var Line: string);
// removes all spaces from a string

procedure FileCopy(const FSrc, FDst: string);
// Copies a file from FSrc to FDst

function TrueFalse(B: boolean): string;

function ScanFile(const FileName: string; const forString: string; caseSensitive: Boolean): Longint;
// returns position of string in file or -1, if not found

function SwapW(const AValue: Word): Word;

function MakeValidFileName(S: string): string;

function Mod2WordArray(W1, W2: word): word;

function WinVisible(Win: TForm): boolean;
// checks if a window is hidden on a non-active monitor


implementation

function SwapW(const AValue: Word): Word;
begin
  Result := Word((AValue shr 8) or (AValue shl 8));
end;

function StrStr(Len: Integer; ch: Char): AnyString;
begin
  StrStr := StringOfChar(ch, Len);
end; { StrStr }

function Hex1(Nyb: longword): Char;
const
  N: string[16] = '0123456789ABCDEF';
begin
  Hex1 := N[(Nyb and $000F) + 1];
end; { Nybble }

function Hex2(Hex: word): String2;
var
  S: String2;
begin
  S[0] := #02;
  S[1] := Hex1(Hex shr 4);
  S[2] := Hex1(Hex);
  Hex2 := S;
end; { Hex2 }

function Hex3(Hex: word): String3;
var
  S: String3;
begin
  S[0] := #03;
  S[1] := Hex1(Hex shr 8);
  S[2] := Hex1(Hex shr 4);
  S[3] := Hex1(Hex);
  Hex3 := S;
end; { Hex3 }

function Hex4(Hex: word): String4;
var
  S: String4;
begin
  S[0] := #04;
  S[1] := Hex1(Hex shr 12);
  S[2] := Hex1(Hex shr 8);
  S[3] := Hex1(Hex shr 4);
  S[4] := Hex1(Hex);
  Hex4 := S;
end; { Hex4 }

function Hex5(Hex: LongWord): String;
var
  S: String;
begin
  SetLength(S, 5);
  S[1] := Hex1(Hex shr 16);
  S[2] := Hex1(Hex shr 12);
  S[3] := Hex1(Hex shr 8);
  S[4] := Hex1(Hex shr 4);
  S[5] := Hex1(Hex);
  Hex5 := S;
end;

function HexN(Hex: Longword; N: integer): String;
var
  S: String;
  i: integer;
begin
  S := '';
  for i := (N - 1) downto 0 do S := S + Hex1(Hex shr (i * 4));
  HexN := S;
end;

function SStr(I: Integer): String;
// wrapper for the Str procedure
var
  Hulp: String;
begin
  Str(I, Hulp);
  SStr := Hulp;
end;

function VVal(S: string): integer;
// wrapper for the Val procedure
var
  I, Code: integer;
begin
  Val(S, I, Code);
  VVal := I;
end;

function Int2(I: Integer): String2;
var
  Hulp: String2;
begin
  Str(I: 2, Hulp);
  if Hulp[1] = ' ' then Hulp[1] := '0';
  Int2 := Hulp;
end;

function Int3(I: Integer): String3;
var
  Hulp: String3;
begin
  if I > 999 then I := 999;
  Str(I: 3, Hulp);
  if Hulp[1] = ' ' then Hulp[1] := '0';
  if Hulp[2] = ' ' then Hulp[2] := '0';
  Int3 := Hulp;
end;

function Bin4(Hex: word): String4;
// Returns a binary string with the value of I with 4 bits
var
  Hulp: String4;
begin
  Hex := Hex and $000F;
  case Hex of
    $0000 : Hulp := '0000';
    $0001 : Hulp := '0001';
    $0002 : Hulp := '0010';
    $0003 : Hulp := '0011';
    $0004 : Hulp := '0100';
    $0005 : Hulp := '0101';
    $0006 : Hulp := '0110';
    $0007 : Hulp := '0111';
    $0008 : Hulp := '1000';
    $0009 : Hulp := '1001';
    $000A : Hulp := '1010';
    $000B : Hulp := '1011';
    $000C : Hulp := '1100';
    $000D : Hulp := '1101';
    $000E : Hulp := '1110';
    $000F : Hulp := '1111';
  end;
  Bin4 := Hulp;
end;

function Bin8(Hex: word): String8;
// Returns a binary string with the value of I with 8 bits
begin
  Bin8 := Bin4(Hex shr 4) + Bin4(Hex);
end;

function Bin16(Hex: word): String16;
// Returns a binary string with the value of I with 16 bits and a '.' as divider
begin
  Bin16 := Bin8(Hex shr 8) + '.' + Bin8(Hex);
end;

function Bin10(Hex: word): String10;
// Returns a binary string with the value of I with 10 bits
var S : string16;
begin
  S := Bin8(Hex shr 8) + Bin8(Hex);
  Bin10 := RightStr(S, 10);
end;

function Bin12(Hex: word): String12;
// Returns a binary string with the value of I with 10 bits
var S : string16;
begin
  S := Bin8(Hex shr 8) + Bin8(Hex);
  Bin12 := RightStr(S, 12);
end;

function Asc(Hex: Byte): Char;
begin
  if ((Hex > 31) and (Hex < 127)) then Asc := Chr(Hex)
  else Asc := '.';
end; { Asc }

function HPCharP(W: word): string;
// used for mapping HP41 display characters and function names in ROM
// handles punctuation
var
  H: string;
  P: word;
begin
  if (W and $100) = $100 then
    // this is a special character
    H:= HPCharS2[(W and $03F) + 1]
  else
    H:= HPCharS1[(W and $03F) + 1];
  P := W and $0C0;   // mask out punctuation
  case P of
    $040: H := H + '.';
    $080: H := H + ':';
    $0C0: H := H + ',';
  end;
  HPCharP := H;
end;

function HPCharD(W: word): string;
// used for mapping HP41 display characters and function names in ROM
// skips non printable characters for HexDump
var
  H: string;
  P: word;
begin
  if (W and $100) = $100 then
    // this is a special character
    H:= HPCharS2[(W and $03F) + 1]
  else
    H:= HPCharS1[(W and $03F) + 1];

  if (W and $FF) > $3F then H := '.';
  HPCharD := H;
end;

function HPChar(W: word): string;
// used for mapping HP41 display characters and function names in ROM
// ignores punctuation
var
  H: string;
begin
  W := W and $7F;
  if (W and $100) = $100 then
    // this is a special character
    H:= HPCharS2[(W and $03F) + 1]
  else
    H:= HPCharS1[(W and $03F) + 1];
  HPChar := H;
end;

function HPUserChar(W: word): Char;
// used for mapping ASCII characters and USER Code strings
begin
  HPUserChar := Char(CharS[(W and $007F)]);
end;

function HPUserCharS(W: word): string;
// used for mapping ASCII characters and USER Code strings
// handles non-printable chars
begin
  if W > $7F then
    HPUserCharS := '\x' + Hex2(W)
  else
    HPUserCharS := CharSS[W];
end;

function HexToInt(var Line: string): integer;
  { Converts a string, representing a hex number to its integer value
    HexToInt will find the first valid hex digit and continue until the
    first non-valid hex digit. Hex digits may be in upper- or lowercase.
    returns -1 in case of an error
    leading spaces and control characters are ignored
    the hex digits will be removed from the string }
var
  L, Res, i: integer;
  D: Char;
  ErrFlag, EndFlag: boolean;
begin
  Res := -1;
  ErrFlag := False;
  EndFlag := False;
  L := Length(Line);
  i := 0;
  if L > 0 then begin
    Line := TrimLeft(Line); { remove leading spaces and control characters }
    i := 1;
    while (i <= L) and not EndFlag do begin
      D := UpCase(Line[i]);
      if D in HexSet then begin
        if Res < 0 then Res := 0;
        if D in DecSet then begin
          Res := Res * 16 + Ord(D) - Ord('0');
        end else if D in HexChr then begin
          Res := Res * 16 + Ord(D) - Ord('A') + 10;
        end;
      end else if Res >= 0 then EndFlag := true;
      i := i + 1;
    end;
  end else ErrFlag := True; // in case of empty string or no hex characters found
  if ErrFlag then Res := -1 else Delete(Line, 1, i - 1);
  HexToInt := Res;
end;

function GetFirstWord(var Line: string): string;
// Reads the first complete word from a string, removes the word from the original string
// if the word is surrounded by quotes, will go on to next quote to include spaces
// quotes can be "double" or 'single' quotes
// if quotes are used, a quote must be the first character
// only spaces and tabs count as separator, all other ascii chars count!
// if string was empty, or no valid word found, an empty string will be returned
var
  L, i: integer;
  S: string;
  EndFlag, UseQuote: boolean;
  D, Q: Char;
begin
  S := '';                    // Our result string
  Line := TrimLeft(Line);  // remove all leading spaces and control characters
  L := Length(Line);
  UseQuote := false;
  EndFlag := false;
  Q := Char(0);
  if L > 0 then begin
    i := 1;
    while (i <= L) and (not EndFlag) do begin
      D := Line[i];
      if D in QuoteSet then begin
        // string is quoted, so be careful
        if UseQuote and (D = Q) then begin
          EndFlag := true; // this is our closing quote
          inc(i);
        end else begin
          // first quote in string
          UseQuote := true;
          Q := D;         // to remember quote character
          inc(i);
        end;
      end else begin
        // not a quote character
        if (D = SPACE) or (D = TAB) then
          EndFlag := true
        else begin
          // no space or tab, so this is real!
          S := S + D;
          inc(i);
        end;
      end;
    end;
    Delete(Line, 1, i - 1);  // remove string (and quotes) from the original string
    Line := TrimLeft(Line);  // remove any other rubbish from our string
  end;
  GetFirstWord := S;
end;

function HexToByte(var Line: string; var Rslt: boolean): Byte;
// converts the two first valid hex characters in a string to a byte
// removes everything including the two byte from the string
// If there is only one hex digit this wil be converted
var
  L, Res, i, FirstHex: integer;
  D: Char;
  ErrFlag, EndFlag: boolean;
begin
  Res := 0;
  D := Chr(0);
  Rslt := false;
  FirstHex := 0;
  ErrFlag := False;
  EndFlag := False;
  L := Length(Line);
  i := 0;
  if L > 0 then begin
    Line := TrimLeft(Line); { remove leading spaces and control characters }
    i := 1;
    while (i <= L) and not EndFlag do begin
      D := UpCase(Line[i]);
      if D in HexSet then begin  // first hex character found
        FirstHex := i;
        EndFlag := true;
      end else
        i := i + 1;
    end;
    // First Hex is now found or we have an error or end of line
    if FirstHex > 0 then begin
      // Hex digit found!
      if D in DecSet then
        Res := Ord(D) - Ord('0')
      else
        Res := Ord(D) - Ord('A') + 10;
      // process second charachter
      i := i + 1;
      if i <= L then
        D := UpCase(Line[i]);
        if D in HexSet then begin
          i := i + 1;
          if D in DecSet then
             Res := 16 * Res + Ord(D) - Ord('0')
          else
             Res := 16 * Res + Ord(D) - Ord('A') + 10;
        end;
    end else ErrFlag := True; // in case of empty string or no hex characters found
  end else ErrFlag := true;
  if ErrFlag then Rslt := false else begin
    Delete(Line, 1, i - 1);
    Rslt := true;
  end;
  HexToByte := Res;
end;

function BinToInt(var Line: string): Integer;
// Converts a string, representing a binary number to its integer value
// HexToInt will find the first valid binary digit and continue until the
// first non-valid binary digit.
// returns -1 in case of an error
// leading spaces and control characters are ignored
// the binary digits will be removed from the string
var
  L, Res, i: integer;
  D: Char;
  ErrFlag, EndFlag: boolean;
begin
  Res := -1;
  ErrFlag := False;
  EndFlag := False;
  L := Length(Line);
  i := 0;
  if L > 0 then begin
    Line := TrimLeft(Line); { remove leading spaces and control characters }
    i := 1;
    while (i <= L) and not EndFlag do begin
      D := Line[i];
      case D of
        '0': if Res < 0 then Res := 0 else Res := Res * 2;
        '1': Res := Res * 2 + 1;
        else if Res >= 0 then EndFlag := true;
      end;
      i := i + 1;
    end;
  end else ErrFlag := True; // in case of empty string or no bin characters found
  if ErrFlag then Res := -1 else Delete(Line, 1, i - 1);
  BinToInt := Res;
end;

function IntToHex(I: integer): String;
// Converts a number to its hexadecimal representation
// I must be positive
var
  S: String;
begin
  S := '';
  while I > 0 do begin
    Insert(Hex1(I mod 16), S, 1);
    I := I div 16;
  end;
  IntToHex := S;
end;

procedure ExpandSpaces(var Line: string; N: integer);
// expands the string Line with spaces until the length is N
// will do nothing if string is already longer or when N >255
var
  i, L: integer;
begin
  L := Length(Line);
  if (L > N) or (N > 255) or (N = 0) then
    Exit
  else if N > 0 then
    for i := L to N do Line := Line + ' '
  else begin      // N < 0
    N := Abs(N);
    for i := L to N do Line := Line + ' ';
    Delete(Line, N, 255);
  end;
end;

function ExpandSp(Line: string; N: integer): string;
// expands the string Line with spaces until the length is N
// will do nothing if string is already longer or when N >255
var
  i, L: integer;
begin
  L := Length(Line);
  if ((L >= Abs(N)) and (N > 0)) or (N > 255) or (N = 0) then begin
    // do nothing
    ExpandSp := Line;
    Exit;
  end else if (N > 0) then
    for i := L to N - 1 do Line := Line + ' '
  else begin
    // N := abs(N);
    // for i := L to N do Line := Line + ' ';
    // Delete(Line, N + 1, 255)
  end;
  ExpandSp := Line;
end;

procedure ReplaceSpaces(var Line: string);
// replace all spaces in a string by underscores to clean up labels
var
  P: integer;
begin
  P := Pos(' ', Line);
  while P <> 0 do begin
    Line[P] := '_';
    P := Pos(' ', Line);
  end;
end;

procedure RemoveSpaces(var Line: string);
// remove all spaces from a string
var
  P: integer;
begin
  P := Pos(' ', Line);
  while P <> 0 do begin
    Delete(Line, P, 1);
    P := Pos(' ', Line);
  end;
end;


procedure RepString(Source: string; var Dest: string; Pos: integer);
// Inserts string Source in string Dest, overwriting anything already there
// if the part of the string is empty, or the string is shorter, Dest will
// be expanded
// when Pos = 0, do nothing
var
  L_Source, L_Dest: integer;
  i: integer;
begin
  if Pos = 0 then Exit;
  L_Source := Length(Source);
  L_Dest := Length(Dest);
  if Pos >= L_Dest then begin
    // Expand string and add
    Dest := ExpandSp(Dest, Pos - 1) + Source;
  end else if L_Source > L_Dest - Pos then begin
    // string must be somwhat expanded
    Dest := ExpandSp(Dest, Pos + L_Dest + 1);
    for i := 0 to L_Source - 1 do
      Dest[Pos + i] := Source[i + 1];
  end else
    // Dest is already long enough
    for i := 0 to L_Source - 1 do
      Dest[Pos + i] := Source[i + 1];
end;

function TrueFalse(B: boolean): string;
// returns the string YES if B is true, otherwise NO
begin
  if B then TrueFalse := 'YES' else TrueFalse := 'NO';
end;

procedure FileCopy(const FSrc, FDst: string);
// Copy a file FSrc to FDst
var
  sStream, dStream: TFileStream;
begin
  sStream := TFileStream.Create(FSrc, fmOpenRead);
  try
    dStream := TFileStream.Create(FDst, fmCreate);
    try
      dStream.CopyFrom(sStream, 0);
    finally
      dStream.Free;
    end;
  finally
    sStream.Free;
  end;
end;

function ScanFile(const FileName: string; const forString: string; caseSensitive: Boolean): Longint;
// returns position of string in file or -1, if not found
const
  BufferSize = $8001;                                             // 32K+1 bytes
var
  pBuf, pEnd, pScan, pPos: PChar;
  filesize: LongInt;
  bytesRemaining: LongInt;
  bytesToRead: Word;
  F: file;
  SearchFor: PChar;
  oldMode: Word;
begin
  Result := -1;                                                 //assume failure
  if (Length(forString) = 0) or (Length(FileName) = 0) then Exit;
  SearchFor := nil;
  pBuf      := nil;

  // open file as binary, 1 byte recordsize
  AssignFile(F, FileName);
  oldMode  := FileMode;
  FileMode := 0;                                             // read-only access
  Reset(F, 1);
  FileMode := oldMode;
  try                       //allocate memory for buffer and pchar search string
    SearchFor := StrAlloc(Length(forString) + 1);
    StrPCopy(SearchFor, forString);
    if not caseSensitive then SearchFor := StrUpper(SearchFor); // convert to upper case
    GetMem(pBuf, BufferSize);
    filesize       := System.Filesize(F);
    bytesRemaining := filesize;
    pPos           := nil;
    while bytesRemaining > 0 do begin  // calc how many bytes to read this round
      if bytesRemaining >= BufferSize then
        bytesToRead := Pred(BufferSize)
      else
        bytesToRead := bytesRemaining;

      // read a buffer full and zero-terminate the buffer
      BlockRead(F, pBuf^, bytesToRead);
      pEnd  := @pBuf[bytesToRead];
      pEnd^ := #0;
      // scan the buffer. Problem: buffer may contain #0 chars! So we
      // treat it as a concatenation of zero-terminated strings.
      pScan := pBuf;
      while pScan < pEnd do begin
        if not caseSensitive then pScan := StrUpper(pScan);        // convert to upper case
        pPos := StrPos(pScan, SearchFor);                // search for substring
        if pPos <> nil then begin                                   // Found it!
          Result := FileSize - bytesRemaining + Longint(pPos) - Longint(pBuf);
          Break;
        end;
        pScan := StrEnd(pScan);
        Inc(pScan);
      end;
      if pPos <> nil then Break;
      bytesRemaining := bytesRemaining - bytesToRead;
      if bytesRemaining > 0 then begin
        // no luck in this buffers load. We need to handle the case of
        // the search string spanning two chunks of file now. We simply
        // go back a bit in the file and read from there, thus inspecting
        // some characters twice
        Seek(F, FilePos(F) - Length(forString));
        bytesRemaining := bytesRemaining + Length(forString);
      end;
    end; // while
  finally
    CloseFile(F);
    if SearchFor <> nil then StrDispose(SearchFor);
    if pBuf <> nil then FreeMem(pBuf, BufferSize);
  end;
end; // ScanFile


function MakeValidFileName(S: string): string;
const
  // ShortForbiddenChars : set of Char = [';', '=', '+', '<', '>', '|',
  //                                      '"', '[', ']', '\', '/', ''''];
  // LongForbiddenChars  : set of Char = ['<', '>', '|', '"', '\', '/', ':', '*', '?'];
  ForbiddenChars : set of Char =
    [';', '=', '+', '<', '>', '|', '"', '[', ']', '\', '/', '''', ':', '*', '?'];
var
  i, L: integer;
begin
  Result := '';
  L := Length(S);
  if L > 0 then for i := 1 to L do begin
    if S[i] in ForbiddenChars then
      Result := Result + '_'
    else
      Result := Result + S[i];
  end;
end;

function Mod2WordArray(W1, W2: word): word;
// takes two words from MODArray and converts to format for WordArray
begin
  Result := swap(W1);
  Result := Result + ((swap(W2) shl 8) and $FF00);
end;

function WinVisible(Win: TForm): boolean;
var
  i  : Integer;
  Ri : TRect;
  Visible: boolean;
const
  Margin = 10;  // Number of pixels to be seen, at least
begin
  i := 0;
  Visible := true;
  while i < Screen.MonitorCount do begin
    // Compute the intersection between screen and form
    Windows.IntersectRect(Ri, Win.BoundsRect, Screen.Monitors[i].BoundsRect);
    // Check the intersection is large enough
    if (Ri.Width > Margin) and (Ri.Height > Margin) then Break;
    Inc(i);
  end;
  if i >= Screen.MonitorCount then Visible := false;
    // Form is outside of any active monitor
    WinVisible := Visible;
end;



end.

