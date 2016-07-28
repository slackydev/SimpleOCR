{==============================================================================]
  Copyright (c) 2016, Jarl `slacky` Holta
  Project: SimpleOCR
  Project URL: https://github.com/WarPie/SimpleOCR
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
[==============================================================================}
var
  Methods: array of record procAddr: Pointer; ProcDef:PChar; end;
  TypeDefs: array of record TypeName, TypeDef:PChar; end;
  TypesLoaded: Boolean = False;
  MethodsLoaded: Boolean = False;
  
  OldMemoryManager: TMemoryManager;
  MemIsset: Boolean = False;


procedure AddMethod(ProcDef: PChar; ProcAddr: Pointer);
var L: Integer;
begin
  L := Length(Methods);
  SetLength(Methods, L + 1);
  Methods[l].ProcAddr := ProcAddr;
  Methods[l].ProcDef := ProcDef;
end;

procedure AddType(TypeName, TypeDef: PChar);
var L: Integer;
begin
  L := Length(TypeDefs);
  SetLength(TypeDefs, L + 1);
  TypeDefs[l].TypeName := TypeName;
  TypeDefs[l].TypeDef := TypeDef;
end;

procedure LoadExports;
begin
  addType('TFontChar',
          'packed record'                  +#13#10+
          '  FChar:AnsiChar;'              +#13#10+
          '  FWidth,FHeight:Int32;'        +#13#10+
          '  loaded, hasShadow:LongBool;'  +#13#10+
          '  pts,shadow,bad:TPointArray;'  +#13#10+
          'end;');
  addType('TFontset',
          'packed record'                  +#13#10+
          '  FData: Array of TFontChar;'   +#13#10+
          '  SpaceWidth: Int32;'           +#13#10+
          'end;');
  addType('TCompareRules',
          'packed record'                  +#13#10+
          '  Color, ColorMaxDiff: Int32;'  +#13#10+
          '  UseShadow: LongBool;'         +#13#10+
          '  ShadowMaxValue:Int32;'        +#13#10+
          '  Threshold: Int32;'            +#13#10+
          '  ThreshInv: LongBool;'         +#13#10+
          'end;');
  addType('TSimpleOCR',
          'packed record'                  +#13#10+
          '  IsLoaded: LongBool;'          +#13#10+
          '  FontData: TFontSet;'          +#13#10+
          '  ClientID: TTarget_Exported;'  +#13#10+
          '  Client:   T2DIntArray;'       +#13#10+
          '  __maxShadowAvg: Int32;'       +#13#10+
          '  __debugging: LongBool;'       +#13#10+
          'end;');


  addMethod('procedure TFontSet.Load(Font:AnsiString; Space:Int32=4);', @TFontSet_Load);
  addMethod('procedure TFontSet.Free();', @TFontSet_Free);
  addMethod('procedure TSimpleOCR.Init(FontPath:TFontSet; AClient:TTarget_Exported); native;', @TSimpleOCR_Init);
  addMethod('procedure TSimpleOCR.Init(Font:AnsiString; SpaceWidth:Int32=4; AClient:TTarget_Exported); overload; native;', @TSimpleOCR_Init2);
  addMethod('procedure TSimpleOCR.SetFont(FontPath:TFontSet); native;', @TSimpleOCR_SetFont);
  addMethod('procedure TSimpleOCR.SetFont(Font:AnsiString; SpaceWidth:Int32=4); overload; native;', @TSimpleOCR_SetFont2);
  addMethod('procedure TSimpleOCR.Free(); native;', @TSimpleOCR_Free);
  addMethod('function TSimpleOCR.Recognize(B:TBox; Filter:TCompareRules; MaxWalk:Int32=40): AnsiString; native;', @TSimpleOCR_Recognize);
  addMethod('function TSimpleOCR.RecognizeEx(AClient:T2DIntArray; Filter:TCompareRules; MaxWalk:Int32=40): AnsiString; native;', @TSimpleOCR_RecognizeEx);

  MethodsLoaded := True;
  TypesLoaded := True;
end;

procedure FreeMethods;
begin
  SetLength(Methods, 0);
  MethodsLoaded := False;
end;


procedure FreeTypes;
begin
  SetLength(TypeDefs, 0);
  TypesLoaded := False;
end;
