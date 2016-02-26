unit OCREngine;
{==============================================================================]
  Author: Jarl K. Holta
  Project: SimpleOCR 
  Project URL: https://github.com/WarPie/SimpleOCR
  License: GNU GPL v3 (http://www.gnu.org/licenses/gpl.html)
[==============================================================================}
{$mode objfpc}{$H+}
{$macro on}
{$inline on}
{$modeswitch advancedrecords}
{$rangechecks on}

{$DEFINE callconv:=
  {$IFDEF WINDOWS}{$IFDEF CPU32}cdecl;{$ELSE}{$ENDIF}{$ENDIF}
  {$IFDEF LINUX}{$IFDEF CPU32}cdecl;{$ELSE}{$ENDIF}{$ENDIF}
}
interface

uses
  SysUtils,
  OCRBitmap, 
  OCRUtils,
  OCRTypes;

type
  PFontChar = ^TFontChar;
  TFontChar = packed record
    FChar:AnsiChar;
    FWidth,FHeight:Int32;
    loaded, hasShadow:LongBool;
    PTS: TPointArray;
    Shadow: TPointArray;
    Bad: TPointArray;
  end;
  TFontChars = Array of TFontChar;

  PFontSet = ^TFontSet;
  TFontSet = packed record
    FData: TFontChars;
    SpaceWidth: Int32;
    
    procedure Load(Font:String; Space:Int32=4);
    procedure Free();
  end;

  PCompareRules = ^TCompareRules;
  TCompareRules = packed record
    Color, ColorMaxDiff: Int32; //-1 = any color
    UseShadow: LongBool;
    ShadowMaxValue:Int32;
    Threshold: Int32;
    ThreshInv: LongBool;
  end;

  PSimpleOCR = ^TSimpleOCR;
  TSimpleOCR = packed record
    IsLoaded: LongBool;
    FontData: TFontSet;
    ClientID: TTarget;
    Client:   TIntMatrix;
    __maxShadowBr: Int32;
    __debugging: LongBool;
    
    procedure Init(Font:String; Space:Int32; AClient:TTarget);
    procedure Init(Font:TFontSet; AClient:TTarget); overload;
    procedure Free();

    procedure SetFont(Font:String; Space:Int32=4);
    procedure SetFont(Font:TFontSet); overload;
    procedure InitClient(B:TBox; var Filter:TCompareRules);
    function CompareChar(chr:TFontChar; offset:TPoint; Info:TCompareRules): Int32;
    function Recognize(B:TBox; Filter:TCompareRules; MaxWalk:Int32): String;
    function RecognizeEx(AClient:TIntMatrix; Filter:TCompareRules; MaxWalk:Int32): String;
  end;


procedure TFontSet_Load(var Self:TFontSet; Font:AnsiString; SpaceWidth: Int32); callconv export;
procedure TFontSet_Free(var Self:TFontSet); callconv export;

procedure TSimpleOCR_Init(const Params: PParamArray); callconv export;
procedure TSimpleOCR_Init2(const Params: PParamArray); callconv export;
procedure TSimpleOCR_Free(const Params: PParamArray); callconv export;
procedure TSimpleOCR_SetFont(const Params: PParamArray); callconv export;
procedure TSimpleOCR_SetFont2(const Params: PParamArray); callconv export;
procedure TSimpleOCR_Recognize(const Params: PParamArray; const Result:Pointer); callconv export;
procedure TSimpleOCR_RecognizeEx(const Params: PParamArray; const Result:Pointer); callconv export;

implementation 

uses
  StrUtils, Math;


//--| TFontSet |--------------------------------------------------------------\\
procedure TFontSet.Load(Font:String; Space:Int32=4);
var
  chars: TStringArray;
  TPA:TPointArray;
  i: Int32;
  BMP:TMiniBitmap;
  charid: String;
  pts_box,shd_box: TBox;
begin
  Self.Free();
  Font := FindFontPath(Font);
  if not DirectoryExists(Font) then
  begin
    WriteLn('TFontSet.Load: Can''t find font ('+Font+')');
    raise Exception.Create('TFontSet.Load: Can''t find font  ('+Font+')');
  end;
  SpaceWidth := Space;
  
  chars := ListDir(Font);
  SetLength(FData, Length(chars));
  for i:=0 to High(Chars) do
  begin
    if not AnsiEndsText('.bmp', chars[i]) then
      continue;
    BMP.Loaded := False;
    BMP.Open(Font+Chars[i]);
    charid := Copy(chars[i], 0, length(chars[i])-4);
    if StrToIntDef(charid, -1) = -1 then
    begin
      BMP.Free();
      continue;
    end;
    FData[i].Loaded := BMP.FindColor(FData[i].PTS, $FFFFFF);

    if FData[i].Loaded then
    begin
      FData[i].FChar := chr(StrToInt(charid));
      FData[i].HasShadow := BMP.FindColor(FData[i].Shadow, 255);

      shd_box := TPABounds(FData[i].Shadow);
      pts_box := TPABounds(FData[i].PTS);
      if pts_box.x1 > 0 then
      begin
        OffsetTPA(FData[i].PTS, -pts_box.x1,0);
        SortTPAByColumn(FData[i].PTS);
        if FData[i].HasShadow then
          OffsetTPA(FData[i].shadow, -(pts_box.x1), 0);

        TPA := CombineTPA(FData[i].PTS, FData[i].Shadow);
        FData[i].Bad  := InvertTPA(TPA);
      end;

      FData[i].FWidth  := Max(pts_box.x2-pts_box.x1, shd_box.x2-shd_box.x1)+1;
      FData[i].FHeight := Max(pts_box.y2, shd_box.y2)+1;
    end;

    BMP.Free();
  end;
end;

procedure TFontSet.Free();
begin
  if Length(Self.FData) > 0 then 
  begin
    SetLength(Self.FData, 0);
    Self.SpaceWidth := 0;
  end;
end;


//--| SimpleOCR |-------------------------------------------------------------\\
procedure TSimpleOCR.Init(Font:String; Space:Int32; AClient:TTarget);
begin
  ClientID := AClient;
  FontData.Load(Font, Space);
  IsLoaded := Length(FontData.FData) > 0;
  __debugging := False;
  __maxShadowBr := 85;
end;

procedure TSimpleOCR.Init(Font:TFontSet; AClient:TTarget); overload;
begin
  ClientID := AClient;
  FontData := Font;
  IsLoaded := Length(FontData.FData) > 0;
  __debugging := False;
  __maxShadowBr := 85;
end;

procedure TSimpleOCR.Free();
begin
  if IsLoaded then FontData.Free();
end;

procedure TSimpleOCR.SetFont(Font:String; Space:Int32=4);
begin
  FontData.Load(Font, Space);
  IsLoaded := Length(FontData.FData) > 0;
end;

procedure TSimpleOCR.SetFont(Font:TFontSet); overload;
begin
  FontData := Font;
  IsLoaded := Length(FontData.FData) > 0;
end;

procedure TSimpleOCR.InitClient(B:TBox; var Filter:TCompareRules);
var
  BMP: TMiniBitmap;
begin
  BMP.Loaded := False;
  BMP.FromClient(ClientID, B);
  if (Filter.Color = -1) and not(Filter.UseShadow) then
  begin
    BMP.ThresholdAdaptive(0, 255, Filter.ThreshInv, tmMean, Filter.Threshold);
    Client := BMP.ToMatrix();
    Filter.Color := 255;
  end else
    Client := BMP.ToMatrix();

  BMP.Free();
end;


function TSimpleOCR.CompareChar(chr:TFontChar; offset:TPoint; Info:TCompareRules): Int32;
var
  i,test,any:Int32;
  maxshd:Single;
  first,color:TRGB32;
  pt:TPoint;
begin
  i := 0; test := 0; any := 0;
  if (chr.FHeight > Length(Client)) then Exit(-1);
  if (Info.Color = -1) then
  begin
    first := TRGB32(Client[chr.pts[0].y, chr.pts[0].x+offset.x]);
    if Info.UseShadow then
    begin
      maxshd := 2*Info.ShadowMaxValue;
      if ((first.r+first.g+first.b) div 3 < self.__maxShadowBr) and
         ((first.R<maxshd) and (first.G<maxshd) and (first.B<maxshd)) then
        Exit(-1);
    end;
  end else
    first := TRGB32(Info.Color);


  //count hits for the character
  for i:=0 to High(chr.pts) do
  begin
    pt := chr.pts[i];
    pt.x += offset.x;
    color := TRGB32(Client[pt.y,pt.x]);
    if not( Sqr(color.R-first.R) + Sqr(color.B-first.B) + Sqr(color.G-first.G) <= Info.ColorMaxDiff ) then
      Exit(-1)
    else
      Inc(test,2);
  end;
  if test < length(chr.pts) then Exit(-1); //<50% match.


  if not Info.UseShadow then
  begin
    //counts hits for the points that should not have equal color to character
    //not needed for shadow-fonts
    for i:=0 to High(chr.bad) do
    begin
      pt := chr.bad[i];
      pt.x += offset.x;
      color := TRGB32(Client[pt.y,pt.x]);
      if Sqr(color.R-first.R) + Sqr(color.B-first.B) + Sqr(color.G-first.G) > Info.ColorMaxDiff then
        Inc(any)
      else
        Dec(test);
    end;
    if (length(chr.bad) > 0) and (any <= (length(chr.bad) div 2)) then
      Exit(-1) //<=50% match.
    else
      Inc(test, any);
  end else
  begin
    //count hits for font-shadow
    for i:=0 to High(chr.shadow) do
    begin
      pt := chr.shadow[i];
      pt.x += offset.x;
      //try
      color := TRGB32(Client[pt.y,pt.x]);
      //except
      //  raise Exception.Create('Out of range: '+Format('(x = %d,y = %d) || (W = %d,H = %d)', [pt.y, pt.x,length(client[0]),length(client)]));
      //end;
      if not((color.R < Info.ShadowMaxValue) and
             (color.G < Info.ShadowMaxValue) and
             (color.B < Info.ShadowMaxValue)) then
        Exit(-1)
      else
        Inc(test);
    end;
  end;
  Result := test;
end;

function GetTallestChar(chars:TFontChars): Int32;
var i:Int32;
begin
  Result := 0;
  for i:=0 to High(chars) do
    if chars[i].FHeight > Result then
      Result := chars[i].FHeight;
end;


{$DEFINE MACRO_OCR_BODY :=
  Result := '';
  if Length(Client) = 0 then
  begin
    WriteLn('TSimpleOCR.Recognize -> Client image is empty');
    Exit();
  end;

  width := Length(Client[1]);
  height := Length(Client);

  if Length(FontData.FData) = 0 then
  begin
    WriteLn('TSimpleOCR.Recognize -> Fontset appears to be empty');
    Exit();
  end;

  if (__debugging) and (GetTallestChar(FontData.FData) > Length(Client)) then
  begin
    WriteLn('TSimpleOCR.Recognize -> Client does not appear to be tall enough: ',height, #13#10 +
            '                        Tallest char: ', GetTallestChar(FontData.FData));
  end;

  while (x < width) and (space < maxWalk) do
  begin
    bestID := -1;
    bestCount := 0;

    for i:=0 to High(FontData.FData) do
    begin
      if not(FontData.FData[i].loaded) or (width-x < FontData.FData[i].FWidth) then
        continue;

      try
        hits := Self.CompareChar(FontData.FData[i], Point(x,0), Filter);
      except
        hits := 0;
        (* Ignore RangeErrors *)
      end;
      if hits > bestCount then
      begin
        bestID := i;
        bestCount := hits;
      end;
    end;

    if (bestID > -1) and (bestCount > 0) then
    begin
      if (space >= FontData.SpaceWidth) and (Result <> '') then
        Result += #32;
      space := 0;
      Inc(x,FontData.FData[bestid].FWidth);
      Result += FontData.FData[bestid].FChar;
      continue;
    end else
      Inc(space);
    Inc(x);
  end;

  if not(__debugging) then
    SetLength(Client, 0,0);
}

function TSimpleOCR.Recognize(B:TBox; Filter:TCompareRules; MaxWalk:Int32=50): String;
var
  space,i,x,width,height:Int32;
  hits,bestid,bestcount:Int32;
begin
  Space := 0;
  x := 0;
  Filter.ColorMaxDiff := Sqr(Filter.ColorMaxDiff);
  Self.InitClient(B, Filter);

  MACRO_OCR_BODY
end;


function TSimpleOCR.RecognizeEx(AClient:TIntMatrix; Filter:TCompareRules; MaxWalk:Int32=50): String;
var
  space,i,x,width,height:Int32;
  hits,bestid,bestcount:Int32;
begin
  Space := 0;
  x := 0;
  Filter.ColorMaxDiff := Sqr(Filter.ColorMaxDiff);
  Self.Client := AClient;

  MACRO_OCR_BODY
end;


//------------------------------------------------------------------------------
procedure TFontSet_Load(var Self:TFontSet; Font:AnsiString; SpaceWidth: Int32); callconv export;
begin
  Self.Load(Font, SpaceWidth);
end;

procedure TFontSet_Free(var Self:TFontSet); callconv export;
begin
  Self.Free();
end;


//------------------------------------------------------------------------------
procedure TSimpleOCR_Init(const Params: PParamArray); callconv export;
begin
  PSimpleOCR(Params^[0])^.Init(PFontSet(Params^[1])^, PTarget(Params^[2])^);
end;

procedure TSimpleOCR_Init2(const Params: PParamArray); callconv export;
begin
  PSimpleOCR(Params^[0])^.Init(PAnsiString(Params^[1])^, PInteger(Params^[2])^, PTarget(Params^[3])^);
end;

procedure TSimpleOCR_Free(const Params: PParamArray); callconv export;
begin
  PSimpleOCR(Params^[0])^.Free();
end;

procedure TSimpleOCR_SetFont(const Params: PParamArray); callconv export;
begin
  PSimpleOCR(Params^[0])^.SetFont(PFontSet(Params^[1])^);
end;

procedure TSimpleOCR_SetFont2(const Params: PParamArray); callconv export;
begin
  PSimpleOCR(Params^[0])^.SetFont(PAnsiString(Params^[1])^, PInteger(Params^[2])^);
end;

procedure TSimpleOCR_Recognize(const Params: PParamArray; const Result:Pointer); callconv export;
begin
  PAnsiString(Result)^ := PSimpleOCR(Params^[0])^.Recognize(PBox(Params^[1])^,
                                                            PCompareRules(Params^[2])^,
                                                            PInteger(Params^[3])^);
end;

procedure TSimpleOCR_RecognizeEx(const Params: PParamArray; const Result:Pointer); callconv export;
begin
  PAnsiString(Result)^ := PSimpleOCR(Params^[0])^.RecognizeEx(PIntMatrix(Params^[1])^,
                                                              PCompareRules(Params^[2])^,
                                                              PInteger(Params^[3])^);
end;

end.



