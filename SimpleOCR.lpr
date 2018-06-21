library SimpleOCR;
{==============================================================================]
  Copyright (c) 2016, Jarl `slacky` Holta
  Project: SimpleOCR
  Project URL: https://github.com/WarPie/SimpleOCR
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
[==============================================================================}
{$mode objfpc}{$H+}
{$macro on}
{$inline on}

{$DEFINE callconv :=
  {$IFDEF WINDOWS}{$IFDEF CPU32}cdecl;{$ELSE}{$ENDIF}{$ENDIF}
  {$IFDEF LINUX}{$IFDEF CPU32}cdecl;{$ELSE}{$ENDIF}{$ENDIF}
}

uses
  SysUtils,
  Math,
  Classes,
  OCRBitmap,
  OCRTypes,
  OCREngine,
  SimbaPlugin;


// ------------------------------------------------------------------------------------------------
// Export definitions
// ------------------------------------------------------------------------------------------------
procedure TFontSet_Load(const Params: PParamArray); callconv export;
begin
  TFontSet(Params^[0]^).Load(AnsiString(Params^[1]^), Int32(Params^[2]^));
end;

procedure TFontSet_Free(const Params: PParamArray); callconv export;
begin
  TFontSet(Params^[0]^).Free();
end;


procedure TSimpleOCR_Init(const Params: PParamArray); callconv export;
begin
  TSimpleOCR(Params^[0]^).Init(TFontSet(Params^[1]^));
end;

procedure TSimpleOCR_Init2(const Params: PParamArray); callconv export;
begin
  TSimpleOCR(Params^[0]^).Init(AnsiString(Params^[1]^), Int32(Params^[2]^));
end;

procedure TSimpleOCR_Free(const Params: PParamArray); callconv export;
begin
  TSimpleOCR(Params^[0]^).Free();
end;

procedure TSimpleOCR_SetFont(const Params: PParamArray); callconv export;
begin
  TSimpleOCR(Params^[0]^).SetFont(PFontSet(Params^[1])^);
end;

procedure TSimpleOCR_SetFont2(const Params: PParamArray); callconv export;
begin
  TSimpleOCR(Params^[0]^).SetFont(AnsiString(Params^[1]^), Int32(Params^[2]^));
end;

procedure TSimpleOCR_Recognize(const Params: PParamArray; const Result:Pointer); callconv export;
begin
  AnsiString(Result^) := TSimpleOCR(Params^[0]^).Recognize(TBox(Params^[1]^),
                                                            TCompareRules(Params^[2]^),
                                                            Int32(Params^[3]^));
end;

procedure TSimpleOCR_RecognizeEx(const Params: PParamArray; const Result:Pointer); callconv export;
begin
  AnsiString(Result^) := TSimpleOCR(Params^[0]^).RecognizeEx(TIntMatrix(Params^[1]^),
                                                             TCompareRules(Params^[2]^),
                                                             Int32(Params^[3]^));
end;



// ------------------------------------------------------------------------------------------------
// Export to Simba
// ------------------------------------------------------------------------------------------------
begin
  AddGlobalType('TFontChar',
                'packed record'                  +LineEnding+
                '  FChar:AnsiChar;'              +LineEnding+
                '  FWidth,FHeight:Int32;'        +LineEnding+
                '  Loaded, HasShadow:LongBool;'  +LineEnding+
                '  PTS,Shadow,Bad:TPointArray;'  +LineEnding+
                'end;');
  AddGlobalType('TFontset',
                'packed record'                  +LineEnding+
                '  FData: Array of TFontChar;'   +LineEnding+
                '  SpaceWidth: Int32;'           +LineEnding+
                'end;');
  AddGlobalType('TCompareRules',
                'packed record'                  +LineEnding+
                '  Color, ColorMaxDiff: Int32;'  +LineEnding+
                '  UseShadow: LongBool;'         +LineEnding+
                '  ShadowMaxValue:Int32;'        +LineEnding+
                '  Threshold: Int32;'            +LineEnding+
                '  ThreshInv: LongBool;'         +LineEnding+
                'end;');
  AddGlobalType('TSimpleOCR',
                'packed record'                  +LineEnding+
                '  IsLoaded: LongBool;'          +LineEnding+
                '  FontData: TFontSet;'          +LineEnding+
                '  ClientID: TTarget_Exported;'  +LineEnding+
                '  Client:   T2DIntArray;'       +LineEnding+
                '  __maxShadowAvg: Int32;'       +LineEnding+
                '  __debugging: LongBool;'       +LineEnding+
                'end;');


  AddLPCMethod(@TFontSet_Load,         'procedure TFontSet.Load(Font:AnsiString; Space:Int32=4);');
  AddLPCMethod(@TFontSet_Free,         'procedure TFontSet.Free();');
  AddLPCMethod(@TSimpleOCR_Init,       'procedure TSimpleOCR.Init(FontPath: TFontSet);');
  AddLPCMethod(@TSimpleOCR_Init2,      'procedure TSimpleOCR.Init(Font: AnsiString; SpaceWidth: Int32=4); overload;');
  AddLPCMethod(@TSimpleOCR_SetFont,    'procedure TSimpleOCR.SetFont(FontPath:TFontSet);');
  AddLPCMethod(@TSimpleOCR_SetFont2,   'procedure TSimpleOCR.SetFont(Font:AnsiString; SpaceWidth:Int32=4); overload;');
  AddLPCMethod(@TSimpleOCR_Free,       'procedure TSimpleOCR.Free();');
  AddLPCMethod(@TSimpleOCR_Recognize,  'function TSimpleOCR.Recognize(B:TBox; Filter:TCompareRules; MaxWalk:Int32=40): AnsiString;');
  AddLPCMethod(@TSimpleOCR_RecognizeEx,'function TSimpleOCR.RecognizeEx(AClient:T2DIntArray; Filter:TCompareRules; MaxWalk:Int32=40): AnsiString;');
end.
