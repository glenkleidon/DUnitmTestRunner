unit Delphi.Versions;

interface

uses SysUtils;

type
  TDelphiVersion = Record
    ProductName: string;
    ShortName: string;
    BDSVersion: integer;
    DelphiVersion: integer;
  End;

const
  EMBARCADERO_KEY = 'Software\Embarcadero\BDS\%u.0';
  CODEGEAR_KEY = 'Software\CodeGear\BDS\%u.0';
  BORLAND_KEY = 'Software\Borland\Delphi\%u.0';

  MAX_VERSION = 12;
  MAX_SUPPORTED_VERSION = 12;

  ALL_DELPHI_VERSIONS: array [0 .. MAX_VERSION] of TDelphiVersion =
    ((ProductName: 'Rio'; ShortName: 'Delphi10.3'; BDSVersion: 20;
    DelphiVersion: 26), (ProductName: 'Tokyo'; ShortName: 'Delphi10.2';
    BDSVersion: 19; DelphiVersion: 25), (ProductName: 'Berlin';
    ShortName: 'Delphi10.1'; BDSVersion: 18; DelphiVersion: 24),
    (ProductName: 'Seattle'; ShortName: 'Delphi10'; BDSVersion: 17;
    DelphiVersion: 23), (ProductName: 'XE8'; ShortName: 'XE8'; BDSVersion: 16;
    DelphiVersion: 22), (ProductName: 'XE7'; ShortName: 'XE7'; BDSVersion: 15;
    DelphiVersion: 21), (ProductName: 'XE6'; ShortName: 'XE6'; BDSVersion: 14;
    DelphiVersion: 20), (ProductName: 'XE5'; ShortName: 'XE5'; BDSVersion: 12;
    DelphiVersion: 19), (ProductName: 'XE4'; ShortName: 'XE4'; BDSVersion: 11;
    DelphiVersion: 18), (ProductName: 'XE3'; ShortName: 'XE3'; BDSVersion: 10;
    DelphiVersion: 17), (ProductName: 'XE2'; ShortName: 'XE2'; BDSVersion: 9;
    DelphiVersion: 16), (ProductName: 'XE'; ShortName: 'XE'; BDSVersion: 8;
    DelphiVersion: 15), (ProductName: '2010'; ShortName: 'D2010'; BDSVersion: 7;
    DelphiVersion: 14));

function FindDelphiIndex(ASearchText: string): integer;
function FindDelphi(ASearchText: string): TDelphiVersion;
function DelphiVersion(AProductName: string): integer; overload;
function DelphiVersion(ABDSVersion: integer): integer; overload;
function BDSVersion(AProductName: string): integer; overload;
function BDSVersion(ADelphiVersion: integer): integer; overload;
function DelphiProductByDelphiVersion(AVersion: integer): string;
function DelphiProductByBDSVersion(AVersion: integer): string;
function DelphiProductByShortName(AShortName: string): string;
function DelphiShortNameByDelphiVersion(AVersion: integer): string;
function DelphiShortNameByBDSVersion(AVersion: integer): string;
function DelphiShortNameByProductName(AVersion: string): string;
Function DelphiIndexByProductName(AProductName: string): integer;
Function DelphiIndexByShortName(AShortName: string): integer;
Function DelphiIndexByBDSVersion(ABDSVersion: integer): integer;
Function DelphiIndexByVersion(AVersion: integer): integer;

function FindDelphiRegKey(ASearchText: string): string;
Function DelphiRegKeyByDelphiVersion(ADelphiVersion: integer): string;
Function DelphiRegKeyByBDSVersion(ABDSVersion: integer): string;
Function DelphiRegKeyByProductName(AProductName: String): string;
Function DelphiRegKeyByShortName(AShortName: String): string;

var DefaultDelphiVersion : integer;

implementation

Function DelphiIndexByProductName(AProductName: string): integer;
var
  I: integer;
begin
  result := -1;
  for I := 0 to MAX_VERSION do
    if (SameText(ALL_DELPHI_VERSIONS[I].ProductName, AProductName)) then
    begin
      result := I;
      exit;
    end;
end;

Function DelphiIndexByShortName(AShortName: string): integer;
var
  I: integer;
begin
  result := -1;
  for I := 0 to MAX_VERSION do
    if (SameText(ALL_DELPHI_VERSIONS[I].ShortName, AShortName)) then
    begin
      result := I;
      exit;
    end;
end;

Function DelphiIndexByBDSVersion(ABDSVersion: integer): integer;
var
  I: integer;
begin
  result := -1;
  for I := 0 to MAX_VERSION do
    if (ALL_DELPHI_VERSIONS[I].BDSVersion = ABDSVersion) then
    begin
      result := I;
      exit;
    end;
end;

Function DelphiIndexByVersion(AVersion: integer): integer;
var
  I: integer;
begin
  result := -1;
  for I := 0 to MAX_VERSION do
    if (ALL_DELPHI_VERSIONS[I].DelphiVersion = AVersion) then
    begin
      result := I;
      exit;
    end;
end;

function FindDelphiIndex(ASearchText: string): integer;
var
  I: integer;
  lSearchString: string;
begin
  result := -1;
  // exact Match first
  for I := 0 to MAX_VERSION do
    if (SameText(ALL_DELPHI_VERSIONS[I].ProductName, ASearchText)) or
      (SameText(ALL_DELPHI_VERSIONS[I].ShortName, ASearchText)) then
    begin
      result := I;
      exit;
    end;

  // Partial Match
  lSearchString := lowercase(ASearchText);
  for I := 0 to MAX_VERSION do
    if (pos(lSearchString, lowercase(ALL_DELPHI_VERSIONS[I].ProductName)) > 0)
      or (pos(lSearchString, lowercase(ALL_DELPHI_VERSIONS[I].ShortName)) > 0)
    then
    begin
      result := I;
      exit;
    end

end;

function FindDelphi(ASearchText: string): TDelphiVersion;
var
  lIndex: integer;
begin
  result.ProductName := '';
  result.ShortName := '';
  result.BDSVersion := -1;
  result.DelphiVersion := -1;

  lIndex := FindDelphiIndex(ASearchText);
  if lIndex >= 0 then
    result := ALL_DELPHI_VERSIONS[lIndex];

end;

function DelphiVersion(AProductName: string): integer;
var
  lIndex: integer;
begin
  result := -1;
  lIndex := DelphiIndexByProductName(AProductName);
  if lIndex >= 0 then
    result := ALL_DELPHI_VERSIONS[lIndex].DelphiVersion;
end;

function DelphiVersion(ABDSVersion: integer): integer; overload;
var
  lIndex: integer;
begin
  result := -1;
  lIndex := DelphiIndexByBDSVersion(ABDSVersion);
  if lIndex >= 0 then
    result := ALL_DELPHI_VERSIONS[lIndex].DelphiVersion;
end;

function BDSVersion(AProductName: string): integer; overload;
var
  lIndex: integer;
begin
  result := -1;
  lIndex := DelphiIndexByProductName(AProductName);
  if lIndex >= 0 then
    result := ALL_DELPHI_VERSIONS[lIndex].BDSVersion;
end;

function BDSVersion(ADelphiVersion: integer): integer; overload;
var
  lIndex: integer;
begin
  result := -1;
  lIndex := DelphiIndexByVersion(ADelphiVersion);
  if lIndex >= 0 then
    result := ALL_DELPHI_VERSIONS[lIndex].BDSVersion;

end;

function DelphiProductByDelphiVersion(AVersion: integer): string;
var
  lIndex: integer;
begin
  result := '';
  lIndex := DelphiIndexByVersion(AVersion);
  if lIndex >= 0 then
    result := ALL_DELPHI_VERSIONS[lIndex].ProductName;
end;

function DelphiProductByBDSVersion(AVersion: integer): string;
var
  lIndex: integer;
begin
  result := '';
  lIndex := DelphiIndexByBDSVersion(AVersion);
  if lIndex >= 0 then
    result := ALL_DELPHI_VERSIONS[lIndex].ProductName;
end;

function DelphiProductByShortName(AShortName: string): string;
var
  lIndex: integer;
begin
  result := '';
  lIndex := DelphiIndexByShortName(AShortName);
  if lIndex >= 0 then
    result := ALL_DELPHI_VERSIONS[lIndex].ProductName;
end;

function DelphiShortNameByDelphiVersion(AVersion: integer): string;
var
  lIndex: integer;
begin
  result := '';
  lIndex := DelphiIndexByVersion(AVersion);
  if lIndex >= 0 then
    result := ALL_DELPHI_VERSIONS[lIndex].ShortName;
end;

function DelphiShortNameByBDSVersion(AVersion: integer): string;
var
  lIndex: integer;
begin
  result := '';
  lIndex := DelphiIndexByBDSVersion(AVersion);
  if lIndex >= 0 then
    result := ALL_DELPHI_VERSIONS[lIndex].ShortName;
end;

function DelphiShortNameByProductName(AVersion: string): string;
var
  lIndex: integer;
begin
  result := '';
  lIndex := DelphiIndexByProductName(AVersion);
  if lIndex >= 0 then
    result := ALL_DELPHI_VERSIONS[lIndex].ShortName;
end;

function DelphiRegKey(ADelphiVersion: TDelphiVersion): string;
begin

end;

function FindDelphiRegKey(ASearchText: string): string;
var
  lDelphiVersion: TDelphiVersion;
begin
  result := '';
  lDelphiVersion := FindDelphi(ASearchText);

end;

Function DelphiRegKeyByDelphiVersion(ADelphiVersion: integer): string;
var
  lKey: string;
  lVersion: Integer;
begin
  result := '';
  case ADelphiVersion of
    1 .. 8:
      begin
        lKey := BORLAND_KEY;
      end;
    9 .. 11:
      begin
        lKey := CODEGEAR_KEY;
        lVersion := BDSVersion(ADelphiVersion);
      end;
  else
    begin
      lKey := EMBARCADERO_KEY;
      lVersion := BDSVersion(ADelphiVersion);
    end;
  end;

  result := format(lKey, [lVersion]);

end;

Function DelphiRegKeyByBDSVersion(ABDSVersion: integer): string;
var
  lKey: string;
begin
  result := '';
  case ABDSVersion of
    8 .. 11:
      begin
        lKey := BORLAND_KEY;
      end;
    12:
      begin
        lKey := CODEGEAR_KEY;
      end;
    13 .. 99:
      begin
        lKey := EMBARCADERO_KEY;
      end;
  end;
  result := format(lKey, [ABDSVersion]);
end;

Function DelphiRegKeyByProductName(AProductName: String): string;
begin
  result := DelphiRegKeyByDelphiVersion(DelphiVersion(AProductName));
end;

Function DelphiRegKeyByShortName(AShortName: String): string;
var
  lIndex: integer;
begin
  result := '';
  lIndex := DelphiIndexByShortName(AShortName);
  if lIndex>=0 then
    result := DelphiRegKeyByDelphiVersion(ALL_DELPHI_VERSIONS[lIndex].DelphiVersion);
end;

initialization
{$IFDEF VER80} DefaultDelphiVersion:=1; {$ENDIF}
{$IFDEF VER90} DefaultDelphiVersion:=2; {$ENDIF}
{$IFDEF VER100} DefaultDelphiVersion:=3; {$ENDIF}
{$IFDEF VER120} DefaultDelphiVersion:=4; {$ENDIF}
{$IFDEF VER130} DefaultDelphiVersion:=5; {$ENDIF}
{$IFDEF VER140} DefaultDelphiVersion:=6; {$ENDIF}
{$IFDEF VER150} DefaultDelphiVersion:=7; {$ENDIF}
{$IFDEF VER160} DefaultDelphiVersion:=8; {$ENDIF}
{$IFDEF VER170} DefaultDelphiVersion:=9; {$ENDIF}
{$IFDEF VER180} DefaultDelphiVersion:=10; {$ENDIF}
{$IFDEF VER180} DefaultDelphiVersion:=11; {$ENDIF}
{$IFDEF VER185} DefaultDelphiVersion:=11; {$ENDIF}
{$IFDEF VER200} DefaultDelphiVersion:=12; {$ENDIF}
{$IFDEF VER210} DefaultDelphiVersion:=14; {$ENDIF}
{$IFDEF VER220} DefaultDelphiVersion:=15; {$ENDIF}
{$IFDEF VER230} DefaultDelphiVersion:=16; {$ENDIF}
{$IFDEF VER240} DefaultDelphiVersion:=17; {$ENDIF}
{$IFDEF VER250} DefaultDelphiVersion:=18; {$ENDIF}
{$IFDEF VER260} DefaultDelphiVersion:=19; {$ENDIF}
{$IFDEF VER270} DefaultDelphiVersion:=20; {$ENDIF}
{$IFDEF VER280} DefaultDelphiVersion:=21; {$ENDIF}
{$IFDEF VER290} DefaultDelphiVersion:=22; {$ENDIF}
{$IFDEF VER300} DefaultDelphiVersion:=23; {$ENDIF}
{$IFDEF VER310} DefaultDelphiVersion:=24; {$ENDIF}
{$IFDEF VER320} DefaultDelphiVersion:=25; {$ENDIF}



end.
