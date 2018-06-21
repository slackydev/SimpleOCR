unit SimbaPlugin;
{==============================================================================]
  Copyright © 2018, Jarl Krister Holta
  
  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

  -----

  Standalone unit for exporting methods to Simba using version 2 of Simba ABI
[==============================================================================}
interface
{$mode objfpc}{$H+}
{$macro on}

{$DEFINE callconv :=
  {$IFDEF WINDOWS}{$IFDEF CPU32}cdecl;{$ELSE}{$ENDIF}{$ENDIF}
  {$IFDEF LINUX}{$IFDEF CPU32}cdecl;{$ELSE}{$ENDIF}{$ENDIF}
}

uses SysUtils;

var
  Methods: array of record ProcAddr: Pointer; ProcDef: AnsiString; end;
  TypeDefs: array of record TypeName, TypeDef: AnsiString; end;
  OldMemoryManager: TMemoryManager;
  MemIsset: Boolean = False;

// for methods using FFI to handle calling conversion
procedure AddFFIMethod(ProcAddr: Pointer; ProcDef: AnsiString);

// for methods using Lape's own higher level calling conversion (LapeCall/lpc)
procedure AddLPCMethod(ProcAddr: Pointer; ProcDef: AnsiString);

// for all type declarations
procedure AddGlobalType(TypeName, TypeDef: AnsiString);


implementation


procedure AddFFIMethod(ProcAddr: Pointer; ProcDef: AnsiString);
var L: Integer;
begin
  L := Length(Methods);
  SetLength(Methods, L + 1);
  Methods[l].ProcAddr := ProcAddr;
  Methods[l].ProcDef  := ProcDef + #0;
end;

procedure AddLPCMethod(ProcAddr: Pointer; ProcDef: AnsiString);
var L: Integer;
begin
  L := Length(Methods);
  SetLength(Methods, L + 1);
  Methods[l].ProcAddr := ProcAddr;
  if ProcDef[Length(ProcDef)] <> ';' then ProcDef += ';';
  Methods[l].ProcDef  := ProcDef + ' native;' + #0;
end;



procedure AddGlobalType(TypeName, TypeDef: AnsiString);
var L: Integer;
begin
  L := Length(TypeDefs);
  SetLength(TypeDefs, L + 1);
  TypeDefs[l].TypeName := TypeName + #0;
  TypeDefs[l].TypeDef  := TypeDef  + #0;
end;



// ----------------------------------------------------------------------------
// Methods exported to simba for getting plugin's methods & types

function GetPluginABIVersion: Integer; callconv export;
begin
  Result := 2;
end;

procedure SetPluginMemManager(MemMgr: TMemoryManager); callconv export;
begin
  if not MemIsset then
  begin
    GetMemoryManager(OldMemoryManager);
    SetMemoryManager(MemMgr);
    MemIsset := True;
  end;
end;

function GetFunctionCount: Integer; callconv export;
begin
  Result := Length(Methods);
end;

function GetFunctionInfo(x: Integer; var ProcAddr: Pointer; var ProcDef: PChar): Integer; callconv export;
begin
  Result := x;
  if (x > -1) and (x < Length(Methods)) then
  begin
    ProcAddr := Methods[x].ProcAddr;
    Move(Methods[x].ProcDef[1], ProcDef^, Length(Methods[x].ProcDef));
  end;
end;

function GetTypeCount: Integer; callconv export;
begin
  Result := Length(TypeDefs);
end;

function GetTypeInfo(x: Integer; var TypeName, TypeDef: PChar): Integer; callconv export;
begin
  Result := x;
  if (x > -1) and (x < Length(TypeDefs)) then
  begin
    Move(TypeDefs[x].TypeName[1], TypeName^, Length(TypeDefs[x].TypeName));
    Move(TypeDefs[x].TypeDef [1], TypeDef^,  Length(TypeDefs[x].TypeDef));
  end;
end;

procedure OnDetach; callconv export;
begin
  SetMemoryManager(OldMemoryManager);
end;


exports GetPluginABIVersion;
exports SetPluginMemManager;
exports GetFunctionCount;
exports GetFunctionInfo;
exports GetTypeCount;
exports GetTypeInfo;
//exports OnAttach;
exports OnDetach;



end.
