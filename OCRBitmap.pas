unit OCRBitmap;
{==============================================================================]
  Copyright (c) 2016, Jarl `slacky` Holta
  Project: SimpleOCR
  Project URL: https://github.com/WarPie/SimpleOCR
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
[==============================================================================}
{$mode objfpc}{$H+}
{$macro on}
{$inline on}
{$modeswitch advancedrecords}

interface

uses
  SysUtils,
  Graphics,
  LCLType,
  LCLIntf,
  FPImage,
  IntfGraphics,
  graphtype, 
  OCRTypes;
  
type
  TThreshMethod = (tmMean, tmMinMax);

  TMiniBitmap = record
    Loaded: Boolean;
    Width, Height:Int32;
    FData: PRGB32;
    
    function Open(f:String): Boolean;
    function FromClient(constref Client:TTarget; B:TBox): Boolean;
    function FromWindow(wnd:Int32; B:TBox): Boolean;
    procedure Free();
    
    function GetPixel(x,y:Int32): TRGB32; inline;
    procedure SetPixel(x,y:Int32; Color:TRGB32); inline;
    
    property Pixel[x,y:Int32]: TRGB32 read GetPixel write SetPixel; default;
    
    function FindColor(var TPA:TPointArray; Color:Int32): Boolean;
    procedure ThresholdAdaptive(Alpha, Beta: Byte; InvertIt: Boolean; Method: TThreshMethod; C: Integer);
    function ToMatrix(): TIntMatrix;
  end;

function BGRToRGB(BGR: TRGB32): Int32;
function NewBitmap(): TMiniBitmap;

implementation   

uses
  OCRUtils{$IFDEF WINDOWS}, Windows{$ENDIF};
  
function NewBitmap(): TMiniBitmap;
begin
  Result.FData := nil;
  Result.Loaded:= False;
  Result.Height:= 0;
  Result.Width := 0;
end;

function BGRToRGB(BGR: TRGB32): Int32;
begin;
  Result := BGR.R or BGR.g shl 8 or BGR.b shl 16;
end;


function TMiniBitmap.Open(f:String): Boolean;
var
  Image : TLazIntfImage;
  Desc : TRawImageDescription;
begin
  Self.Free();
  Result := FileExists(f);
  if Result then
  begin
    Image := TLazIntfImage.Create(0,0);
    Desc.Init_BPP32_B8G8R8_BIO_TTB(Image.Width, Image.Height);
    Image.DataDescription := Desc;
    Image.LoadFromFile(f);
    Self.Width := Image.Width;
    Self.Height := Image.Height;
    Self.FData := GetMem(Self.Width * Self.Height * SizeOf(TRGB32));
    Move(Image.PixelData[0], Self.FData[0], Self.Width * Self.Height * SizeOf(TRGB32));
    Image.Free;
  end else
    WriteLn('TMiniBitmap.Open -> Unable to load file: ', f);
  Loaded := Result;
end;


function TMiniBitmap.FromClient(constref Client:TTarget; B:TBox): Boolean;
var
  RetData:TRetData;
  y:Int32;
begin
  Self.Free();
  Width := B.x2-B.x1+1;
  Height := B.y2-B.y1+1;
  try
    {note: check if dimensions are within range}
    RetData := Client.ReturnData(Client.Target, B.x1, B.y1, width, height);
    Self.FData := GetMem(Width*Height * SizeOf(TRGB32));
    for y:=0 to height-1 do
      Move(RetData.Ptr[y*RetData.rowLen], Self.FData[y*width], Width * SizeOf(TRGB32));
    Loaded := True;
  except on E:Exception do
    begin
      WriteLn('Exception ('+ E.ClassName +', E: '+ E.Message +')');
      raise Exception.Create('Exception ('+ E.ClassName +', E: '+ E.Message +')');
    end;
  end;
  Client.FreeReturnData(Client.Target);
  Result := Loaded;
  if not(Result) then
    Self.Free();
end;


function TMiniBitmap.FromWindow(wnd:Int32; B:TBox): Boolean;
{$IFDEF WINDOWS}
var
  handle: HWND;
  winDC,cDC: HDC;
  BMP: HBITMAP;
begin
  Self.Free();
  Width := B.x2-B.x1+1;
  Height := B.y2-B.y1+1;

  handle := HWND(wnd);
  winDC := Windows.GetWindowDC(handle);
  cDC := Windows.CreateCompatibleDC(winDC);
  bmp := Windows.CreateCompatibleBitmap(winDC, Width,Height);
  Windows.SelectObject(cDC, bmp);
  Windows.BitBlt(cDC, 0,0, Width,Height, winDC, B.x1,B.y1, SRCCOPY);

  // from bitmapbuffer
  Self.FData := GetMem(Self.Width * Self.Height * SizeOf(TRGB32));
  Result := Windows.GetBitmapBits(bmp, width*height*SizeOf(TRGB32), FData) > 0; 
  if not(Result) then
    Self.Free();
  Loaded := Result;

  // free
  Windows.DeleteObject(Windows.SelectObject(cDC, bmp));
  Windows.DeleteDC(cDC);
  Windows.ReleaseDC(handle, winDC);
end;
{$ELSE}
begin
  Self.Free();
  //...
end;
{$ENDIF}

procedure TMiniBitmap.Free();
begin
  if (Self.FData <> nil) and Self.Loaded then
  begin
    FreeMem(Self.FData);
    Self.FData := nil;
  end;
  Self.Width := 0;
  Self.Height := 0;
  Loaded := False;
end;

function TMiniBitmap.GetPixel(x,y:Int32): TRGB32;
begin
  Result := FData[y*Width+x];
end;

procedure TMiniBitmap.SetPixel(x,y:Int32; Color:TRGB32);
begin
  FData[y*Width+x] := Color;
end;


function TMiniBitmap.FindColor(var TPA:TPointArray; Color:Int32): Boolean;
var
  x,y,idx,c:Int32;
  Target: TRGB32;
begin
  Target := RGB32(Color);
  c := 0;
  idx := 0;
  SetLength(TPA, Width*Height);
  for y:=0 to Height-1 do
    for x:=0 to Width-1 do
    begin
      if (FData[idx].R = Target.R) and (FData[idx].G = Target.G) and (FData[idx].B = Target.B) then
      begin
        TPA[c].x := x;
        TPA[c].y := y;
        Inc(c);
      end;
      Inc(idx);
    end;
  SetLength(TPA, c);
  Result := c > 0;
end;


procedure TMiniBitmap.ThresholdAdaptive(Alpha, Beta: Byte; InvertIt: Boolean; Method: TThreshMethod; C: Integer);
var
  i,size: Int32;
  upper: PtrUInt;
  vMin,vMax,threshold: UInt8;
  Counter: Int64;
  Tab: Array [0..256] of UInt8;
  ptr: PRGB32;
begin
  if Alpha = Beta then Exit;
  if InvertIt then Exch(Alpha, Beta);

  size := (Self.Width * Self.Height) - 1;
  upper := PtrUInt(@Self.FData[size]);
  //Finding the threshold - While at it set blue-scale to the RGB mean (needed for later).
  Threshold := 0;
  case Method of
    //Find the Arithmetic Mean / Average.
    tmMean:
    begin
      Counter := 0;
      ptr := Self.FData;
      while PtrUInt(Ptr) <= upper do
      begin
        Ptr^.B := (Ptr^.B + Ptr^.G + Ptr^.R) div 3;
        Counter += Ptr^.B;
        Inc(Ptr);
      end;
      Threshold := (Counter div size) + C;
    end;

    //Middle of Min- and Max-value
    tmMinMax:
    begin
      vMin := 255;
      vMax := 0;
      ptr := Self.FData;
      while PtrUInt(Ptr) <= upper do
      begin
        ptr^.B := (ptr^.B + ptr^.G + ptr^.R) div 3;
        if ptr^.B < vMin then
          vMin := ptr^.B
        else if ptr^.B > vMax then
          vMax := ptr^.B;
        Inc(ptr);
      end;
      Threshold := ((vMax+Int32(vMin)) shr 1) + C;
    end;
  end;

  for i:=0 to (Threshold-1) do Tab[i] := Alpha;
  for i:=Threshold to 255 do Tab[i] := Beta;

  ptr := Self.FData;
  while PtrUInt(Ptr) <= upper do
  begin
    ptr^.R := Tab[Ptr^.B];
    ptr^.G := 0;
    ptr^.B := 0;
    ptr^.A := 0;
    Inc(ptr);
  end;
end;


function TMiniBitmap.ToMatrix(): TIntMatrix;
var
  i,x,y: Integer;
begin
  SetLength(Result, Height, Width);
  i := 0;
  for y:=0 to Height-1 do
    for x:=0 to Width-1 do
    begin
      Result[y,x] := BGRToRGB(Self.FData[i]);
      Inc(i);
    end;
end;


end.
