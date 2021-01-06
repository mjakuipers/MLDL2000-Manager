//---------------------------------------------------------------------------//
//    Copyright (c) 2020  Meindert Kuipers, Netherlands                      //
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
//  CSVBase.PAS                                                              //
//  Unit for the M2kM Rom Handler                                            //
//  CSV Databse routines, wrapper for CSVDataSet                             //
//  1.90 Mar 2020 First version                                              //
//                                                                           //
//                                                                           //
//---------------------------------------------------------------------------//

unit TxtBase;

{$mode objfpc}

interface


// Implements functions for in-memory funtions for simple database tasks
// for .txt files with lists of text-base data. Read only database!
// Main functions:
//      Settings:   sets data characteristics for reading text file
//                  record types
//                  delimiter
//                  quote char
//                  remove starting and trailing spaces
//      Initialize: opens file and reads data in variable array
//      Record:     current record pointer
//      Next, Prev: move pointer to next or previous record
//      Beg, End:   move pointer to beginning or end
//      Goto:       move pointer to certain field
//      Get:        get record/field, retunr contents
//      Find:       find record with certain contents, move pointer
//      RecCount:   number of records
//


uses
  Classes, SysUtils;

type

  TxtRecord = record
    Idx     : integer;                    // Index
    Strings : array[0..6] of string;      // holds the strings
    Nums    : array[0..6] of longword;    // holds numeric values
    Floats  : array[0..6] of single;
  end;

  TField    = (IntField,                  // field type intger (or Hex if preceded by $ or 0x
               StrField,                  // string type, must have quotes if it contains other delimiters
               FltField,                  // float type, no thousands separator, . or , is decimal separator
               NoField );                 // no field, must always be at the end

  TtxtBase = class                        // Class for Text Base Database-like functions

  private
    F           : TextFile;               // Textfile with items
    fFilename   : string;                 // name of file with extension
    aTxtBase    : array of TxtRecord;     // our database, dynamic array
    FieldArray  : array[0..9] of TField;
                                          // defines the structure of the CSV file

  public
    FileName    : string;                 // name of file
    FileOpen    : boolean;                // is there an open file
    RecCount    : integer;                // number of entries used
    RecPoint    : integer;                // points to active record
    Delim       : char;                   // actual delimiter, default Space
    DelimAlt1   : char;                   // alternate delimiter, default ;
    DelimAlt2   : char;                   // alternate delimiter, default ,
    Quote       : char;                   // quote char, default "
    QuoteAlt1   : char;                   // alternate quote, default '
    QuoteAlt2   : char;                   // default empty
    CmtChar     : char;                   // default comment char {
    CmtCharAlt1 : char;                   // default comment char //
    CleanSpaces : boolean;                // clean leading and trailing spaces
    CurRec      : TxtRecord;              // current record for direct access

    function Init: Boolean;               // returns true if succesful
                                          // opens file and reads data
    procedure SetField(Fld: integer; T: TField);
    procedure NextRec;
    procedure PrevRec;
    procedure FirstRec;
    procedure LastRec;
    procedure GotoRec(Rec: integer);
    procedure GetRec;
    procedure FindRec;

    constructor Create;
    destructor Destroy; override;


  end;

implementation


function TtxtBase.Init: Boolean;      // returns true if succesful
                                      // opens file and reads data
begin
  Init := true;
end;

procedure TtxtBase.SetField(Fld: integer; T: TField);
// sets the expected field types
begin
  // if (Fld >= 0) and (Fld < 10) then FieldArray[Fld] := T;
end;

procedure TtxtBase.NextRec;
begin
  // increment record Pointer
  if RecPoint < RecCount then begin
    Inc(RecPoint);
    CurRec := aTxtBase[RecPoint];
  end;
end;

procedure TtxtBase.PrevRec;
begin
  //decrement record Pointer
  if RecPoint > 0 then begin
    Dec(RecPoint);
    CurRec := aTxtBase[RecPoint];
  end;
end;

procedure TtxtBase.FirstRec;
begin
  // go to first Record (0)
  RecPoint := 0;
  CurRec := aTxtBase[0];
end;

procedure TtxtBase.LastRec;
begin
  // go to last Record
  RecPoint := RecCount - 1;
  CurRec := aTxtBase[RecPoint];

end;

procedure TtxtBase.GotoRec(Rec: integer);
begin
  // go to indicated record if it exists, otherwise there is no change
  if (Rec >= 0) and (Rec < RecCount) then begin
    RecPoint := RecCount - 1;
    CurRec := aTxtBase[RecPoint];
  end;
end;

procedure TtxtBase.GetRec;
begin
  // does nothing, Current Record is always valid

end;

procedure TtxtBase.FindRec;
begin
  // find a record based on a field value and move the pointer there

end;


constructor TtxtBase.Create;
var
  i : integer;
begin
  // set default values
  for i := 0 to 9 do FieldArray[1] := NoField;

  Delim       := ' ';                  // actual delimiter, default Space
  DelimAlt1   := ';';                  // alternate delimiter, default ;
  DelimAlt2   := ',';                  // alternate delimiter, default ,
  Quote       := '"';                  // quote char, default "
  QuoteAlt1   := '''';                 // alternate quote, default '
  QuoteAlt2   := #00;                  // default empty
  CmtChar     := '{';                  // default comment char { only at the start of a line
  CmtCharAlt1 := '/';                  // default comment char // only at the start of a line
  CleanSpaces := true;                 // clean leading and trailing spaces


end;


destructor TtxtBase.Destroy;
begin
  inherited Destroy;

end;

end.

