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

Function ExtractUsesClause(AProjectFile: TStrings;
  AClauseTypes: TUnitClauseTypes): String;
var
  lUses: string;
  lImplementationToken: TTokenInfo;

begin
  result := '';
  if (ucInterface in AClauseTypes) or (ucDPR in AClauseTypes) then
  begin
    result := result + TextBeweenTokens('uses', dtKeyWord, ';', dtSeparator,
      AProjectFile.Text);
  end;
  lImplementationToken := LocateToken('implementation', dtKeyword,
    AProjectFile.Text);
  if not((ucImplementation in AClauseTypes) and (lImplementationToken.isToken))
  then
    exit;
  // locate inner Uses Clause
  result := result + TextBeweenTokens('uses', dtUnknown, ';', dtSeparator,
    AProjectFile.Text);

end;

Function LocateMiniTestFrameworkUnit(AProjectFile: TStrings): boolean;
var
  lUses: string;
begin
  result := pos(lowercase(UNIT_NAME_MINITESTFRAMEWORK),
    lowercase(AProjectFile.Text)) > 0;
  if not result then
    exit;
  // Is it actually in the uses Clause
  lUses := ExtractUsesClause(AProjectFile, [ucDPR]);

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
  result.Init;

  if not fileexists(AProjectFile) then
    exit;

  Assert(sameText(ExtractFileExt(AProjectFile), '.dpr'),
    'Test Project Files must be DPR files');

  lDPRFile := TStringlist.Create;
  try
    lDPRFile.LoadFromFile(AProjectFile);
    if not LocateMiniTestFrameworkUnit(lDPRFile) then exit;
    result.IsTestProject := true;

    writeln('Still need to get Titles, ProjectSearchPath, TestUnitList');
    writeln('  and projectbasedCases for', AProjectFile );

  finally
    Freeandnil(lDPRFile);
  end;

end;

end.
