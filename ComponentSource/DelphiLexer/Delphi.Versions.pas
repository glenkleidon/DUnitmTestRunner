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
Function DelphiRegKeyByBDSVersion(ADelphiVersion: integer): string;
Function DelphiRegKeyByProductName(ADelphiVersion: integer): string;
Function DelphiRegKeyByShortName(ADelphiVersion: integer): string;

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
  lVersion: string;
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

Function DelphiRegKeyByBDSVersion(ADelphiVersion: integer): string;
begin

end;

Function DelphiRegKeyByProductName(ADelphiVersion: integer): string;
begin

end;

Function DelphiRegKeyByShortName(ADelphiVersion: integer): string;
begin

end;

end.
