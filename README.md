# OSR-OCR
Simple OCR engine to read text at static coordinates in "RuneScape"

------

Usage:
------
The following line should be added to your script, or include, it's used to keep the OCR engine up to date with the scripts client.

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
  filterReules:TCompareRules = [-1, 85, True, 55]; (* any color, 85 tolerance, Use shadow!, shadow not brigther than 55! *) 
begin
  OCR.Init(FontPath+'UpCharsEx');
  str := OCR.Recognize(IntToBox(7,7,500,30), filterReules);
  WriteLn(str);
end;
```
Keep in mind that the engine expects pixel precision, so, `TBox.x1`, and specially `TBox.y1` should be set perferectly.

-------

A useful thing to familiarize yourself with is `TCompareRules`.
```pascal
  packed record
    Color, ColorMaxDiff: Int32; //color and tolerance
    
    UseShadow: LongBool;        //rely on shadow? if yes then we can safely ignore colors if wanted (color = -1)
    ShadowMaxValue:Int32;       //max brightness of the shadow (0..255)
    
    Threshold: Int32;           //we can use threshold instead tho to get it working with most colors.
    ThreshInv: LongBool;        //? invert the threshold, so that dark = bright, bright = dark?
  end;
```
Notice that if the color is set to `-1` either shadow, or Threshold must be defined.
