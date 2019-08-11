unit uTestBuilder.ProjectInfo;

interface

uses SysUtils;

Type

  TUnitClauseType = (ucDPR, ucInterface, ucImplementation);
  TUnitClauseTypes = set of TUnitClauseType;

  TTestProjectInfo = Record
    IsTestProject: boolean;
    ProjectSearchPath: String;
    Titles: string;
    TestUnitList: string;
    ProjectBasedCases: string;
  End;

  TTestProjectInfoHelper = Record Helper for TTestProjectInfo
  public
    Procedure Init;
  End;

function GetTestProjectInfo(AProjectFile: string): TTestProjectInfo;

implementation

uses Classes, DUnitm.Constants, Delphi.Lexer;


Function FirstWordOfLine(ALine: string): String;
begin

end;

Procedure StripLeadingComments(APASFile: TStrings);
begin
  while True do

end;

Function ExtractUsesClause(AProjectFile: TStrings;
  AClauseTypes: TUnitClauseTypes): String;
begin
  if (ucDPR in AClauseTypes) then
  begin
    // do we find the
  end;

end;

Function LocateMiniTestFrameworkUnit(AProjectFile: TStrings): integer;
begin
  Result := pos(lowercase(UNIT_NAME_MINITESTFRAMEWORK),
    lowercase(AProjectFile.Text));
  if Result = 0 then
    exit;

  // Is it actually in the uses Clause

end;

{ TTestProjectInfoHelper }

procedure TTestProjectInfoHelper.Init;
begin
  self.IsTestProject := false;
  self.Titles := '';
  self.ProjectSearchPath := '';
  self.TestUnitList := '';
  self.ProjectBasedCases := '';
end;

function GetTestProjectInfo(AProjectFile: string): TTestProjectInfo;
var
  lDPRFile: TStringlist;
begin
  Result.Init;

  if not fileexists(AProjectFile) then
    exit;

  Assert(sameText(ExtractFileExt(AProjectFile), '.dpr'),
    'Test Project Files must be DPR files');

  lDPRFile := TStringlist.Create;
  try
    lDPRFile.LoadFromFile(AProjectFile);
    if not LocateMiniTestFrameworkUnit(lDPRFile) then

    finally
      Freeandnil(lDPRFile);
    end;

  end;


end.
