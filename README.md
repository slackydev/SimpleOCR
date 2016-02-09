# OSR-OCR
Simple OCR engine to read text at static coordinates in the game Oldschool RuneScape 

Usage:
------

The following line should be added to your script, or include:
```pascal
function TSimpleOCR.Recognize(B:TBox; Filter:TCompareRules; MaxWalk:Int32=40): String; override;
begin
  Self.ClientID := ExportImageTarget();
  Result := Inherited(B, Filter, MaxWalk);
end;
```

Now it's just a matter of loading up your font, and start recognizing text:
```pascal
var 
  OCR:TSimpleOCR;
  str:String;
begin
  OCR.Init(FontPath+'UpCharsEx');
  str := OCR.Recognize(IntToBox(10,10,500,30);
  WriteLn(str);
end;
```
