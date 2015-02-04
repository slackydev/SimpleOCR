unit OCRTypes;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
{$inline on}
 
interface

uses
  SysUtils;
  
type
  PParamArray = ^TParamArray;
  TParamArray = array[Word] of Pointer;
  
  PPoint = ^TPoint;
  TPoint = packed record x,y:Int32; end;
  PPointArray = ^TPointArray;
  TPointArray = array of TPoint;

  PIntArray = ^TIntArray;
  TIntArray = array of Int32;
  PIntMatrix = ^TIntMatrix;
  TIntMatrix = array of TIntArray;
  
  TBoolArray = Array of ByteBool;
  TBoolMatrix = Array of TBoolArray;
  
  TStringArray = Array of String;

  PBox = ^TBox;
  TBox = packed record
    X1,Y1,X2,Y2: Int32;
  end;

  PRGB32 = ^TRGB32;
  TRGB32 = packed record 
    B,G,R,A: UInt8; 
  end;
  
  //Simba client data --------->>>
  TRetData = record
    Ptr: PRGB32;
    IncPtrWith: Int32;
    RowLen: Int32;
  end;  
  
  PTarget = ^TTarget;
  TTarget = record
    Target: Pointer;
    //client
    GetTargetDim:   procedure(target: Pointer; var w,h: Int32); stdcall;
    GetTargetPos:   procedure(target: Pointer; var top,left: Int32); stdcall;
    GetColor:       function (target: Pointer; x,y: Int32): Int32; stdcall;
    ReturnData:     function (target: Pointer; xs,ys, width,height: Int32): TRetData; stdcall;
    FreeReturnData: procedure(target: Pointer); stdcall;
    //mouse
    GetMousePos:  procedure(target: Pointer; var x,y: Int32); stdcall;
    MoveMouse:    procedure(target: Pointer; x,y: Int32); stdcall;
    ScrollMouse:  procedure(target: Pointer; x,y: Int32; Lines: Int32); stdcall;
    HoldMouse:    procedure(target: Pointer; x,y: Int32; left: Boolean); stdcall;
    ReleaseMouse: procedure(target: Pointer; x,y: Int32; left: Boolean); stdcall;
    //keybd
    SendString: procedure(target: Pointer; str: PChar; keywait,keymodwait: Int32); stdcall;
    HoldKey:    procedure(target: Pointer; key: Int32); stdcall;
    ReleaseKey: procedure(target: Pointer; key: Int32); stdcall;
    IsKeyHeld:  function (target: Pointer; key: Int32): Boolean; stdcall;
    GetKeyCode: function (target: Pointer; C: Char): Int32; stdcall;
  end;
  //end
  
function RGB32(RGB:Int32): TRGB32; inline;
function Point(x,y:Int32): TPoint; inline; 
function Box(x1,y1,x2,y2:Int32): TBox; inline; 


implementation


function RGB32(RGB:Int32): TRGB32;
begin
  Result.R := RGB and $ff;
  Result.G := RGB shr 8 and $ff;
  Result.B := RGB shr 16 and $ff;
  Result.A := 0;
end;

function Point(x,y:Int32): TPoint;
begin
  Result.x := x;
  Result.y := y;
end;

function Box(x1,y1,x2,y2:Int32): TBox; 
begin
  Result.x1 := x1;
  Result.y1 := y1;
  Result.x2 := x2;
  Result.y2 := y2;
end;

end.
