unit OCRUtils;
{==============================================================================]
  Author: Jarl K. Holta
  Project: RSWalker 
  Project URL: https://github.com/WarPie/RSWalker
  License: GNU GPL (http://www.gnu.org/licenses/gpl.html)
  
  Utils needed by the ocr engine.
[==============================================================================}
{$mode objfpc}{$H+}
{$macro on}
{$inline on}
 
interface

uses
  SysUtils, OCRTypes;

procedure Exch(var A,B:UInt8); Inline; overload;
procedure Exch(var A,B:Int32); Inline; overload;
procedure Exch(var A,B:TPoint); Inline; overload;

function TPABounds(const TPA: TPointArray): TBox; Inline;
function CombineTPA(const TPA1,TPA2: TPointArray): TPointArray;
function InvertTPA(const TPA:TPointArray): TPointArray;
procedure OffsetTPA(var TPA: TPointArray; SX,SY:Integer);
procedure InsSortTPA(var Arr:TPointArray; Weight:TIntArray; Left, Right:Int32); Inline;
procedure SortTPAbyColumn(var Arr: TPointArray);

function ListDir(Path:String): TStringArray;
function FindFontPath(Font:String): String;

//-----------------------------------------------------------------------------
implementation 

procedure Exch(var A,B:UInt8);
var t:UInt8;
begin 
  t := A; A := B; B := t; 
end;

procedure Exch(var A,B:Int32);
var t:Int32;
begin 
  t := A; A := B; B := t; 
end;

procedure Exch(var A,B:TPoint);
var t:TPoint;
begin 
  t := A; A := B; B := t; 
end;

//Return the largest and the smallest numbers for x, and y-axis in TPA.
function TPABounds(const TPA: TPointArray): TBox;
var
  I,L : Integer;
begin
  FillChar(Result, SizeOf(TBox), 0);
  L := High(TPA);
  if (l < 0) then Exit;
  Result.x1 := TPA[0].x;
  Result.y1 := TPA[0].y;
  Result.x2 := TPA[0].x;
  Result.y2 := TPA[0].y;
  for I:= 1 to L do
  begin
    if TPA[i].x > Result.x2 then
      Result.x2 := TPA[i].x
    else if TPA[i].x < Result.x1 then
      Result.x1 := TPA[i].x;
    if TPA[i].y > Result.y2 then
      Result.y2 := TPA[i].y
    else if TPA[i].y < Result.y1 then
      Result.y1 := TPA[i].y;
  end;
end;

{*
 Unite two TPAs into one
 ... While also removing all duplicates if `RemoveDupes` is set, so it wont be any overlapping.
*}
function CombineTPA(const TPA1, TPA2: TPointArray): TPointArray;
begin
  if (High(TPA1) = -1) then Exit(TPA2)
  else if (High(TPA2) = -1) then Exit(TPA1);
  SetLength(Result, High(TPA1) + High(TPA2) + 2);
  Move(TPA1[Low(TPA1)], Result[Low(Result)],  Length(TPA1)*SizeOf(TPA1[0]));
  Move(TPA2[Low(TPA2)], Result[Length(TPA1)], Length(TPA2)*SizeOf(TPA2[0]));
end; 


{*
 Returns the points not in the TPA within the area the TPA covers.
*}
function InvertTPA(const TPA:TPointArray): TPointArray;
var
  Matrix: TIntMatrix;
  i,h,x,y: Integer;
  Area: TBox;
begin
  Area := TPABounds(TPA);
  Area.X2 := (Area.X2-Area.X1);
  Area.Y2 := (Area.Y2-Area.Y1);
  SetLength(Matrix, Area.Y2+1, Area.X2+1);

  H := High(TPA);
  for i:=0 to H do
    Matrix[TPA[i].y-Area.y1][TPA[i].x-Area.x1] := 1;

  SetLength(Result, (Area.X2+1)*(Area.Y2+1) - H);
  i := 0;
  for y:=0 to Area.Y2 do
    for x:=0 to Area.X2 do
      if Matrix[y][x] <> 1 then
      begin
        Result[i] := Point(x+Area.x1,y+Area.y1);
        Inc(i);
      end;
  SetLength(Result, i);
  SetLength(Matrix, 0);
end;

{*
 Moves the TPA by SX, and SY points.
*}
procedure OffsetTPA(var TPA: TPointArray; SX,SY:Integer);
var
  I,L : Integer;
begin;
  L := High(TPA);
  if (L < 0) then Exit;
  for I:=0 to L do begin
    TPA[i].x := TPA[i].x + SX;
    TPA[i].y := TPA[i].y + SY;
  end;
end;


//Fast TPointArray sorting for small arrays.
procedure InsSortTPA(var Arr:TPointArray; Weight:TIntArray; Left, Right:Int32); Inline;
var i, j:Int32;
begin
  for i := Left to Right do
    for j := i downto Left + 1 do begin
      if not (Weight[j] < Weight[j - 1]) then Break;
      Exch(Arr[j-1], Arr[j]);
      Exch(Weight[j-1], Weight[j]);
    end;
end;


//Sort small TPA by Column.
procedure SortTPAbyColumn(var Arr: TPointArray);
var
  i,Hi: Int32;
  Weight:TIntArray;
  Area : TBox;
begin
  Hi := High(Arr);
  if Hi < 0 then Exit;
  Area := TPABounds(Arr);
  SetLength(Weight, Hi+1);
  for i := 0 to Hi do
    Weight[i] := (Arr[i].x * (Area.Y2-Area.Y1) + Arr[i].y);
  InsSortTPA(Arr, Weight, 0, Hi);
  SetLength(Weight, 0);
end;



function ListDir(Path:String): TStringArray;
var
  l: Int32;
  SR : TSearchRec;
begin
  l := 0;
  if FindFirst(Path + '*', faAnyFile and faDirectory, SR) = 0 then
  begin
    repeat
      if (SR.Name <> '.') and (SR.Name <> '..') then
      begin
        inc(l);
        SetLength(Result, l);
        Result[l-1] := SR.Name;
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

function FindFontPath(Font:String): String;
begin
  if not(Font[Length(Font)] = '\') then Font += '\';
  if DirectoryExists(Font) then Exit(Font);
  if DirectoryExists('Fonts\'+Font) then Exit('Fonts\'+Font);
  if DirectoryExists('..\Fonts\'+Font) then Exit('..\Fonts\'+Font);
  if DirectoryExists('..\..\Fonts\'+Font) then Exit('..\..\Fonts\'+Font);
  if DirectoryExists('..\..\..\Fonts\'+Font) then Exit('..\..\..\Fonts\'+Font);

  if DirectoryExists('Includes\'+Font) then Exit('Includes\'+Font);
  if DirectoryExists('..\Includes\'+Font) then Exit('..\Includes\'+Font);
  if DirectoryExists('..\..\Includes\'+Font) then Exit('..\..\Includes\'+Font);
  Result := Font;
end;

end.
