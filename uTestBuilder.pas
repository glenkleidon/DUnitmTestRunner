unit uTestBuilder;

interface

uses SysUtils, Classes;

Type
  TBuildResult = Record
    ProjectCount: Integer;
    TestCount: Integer;
    TestPass: Integer;
    TestFail: Integer;
    TestSkip: Integer;
    TestError: Integer;
    Projects: String;
    TestResults: string;
  End;

  TDUnitmTestBuilder = Class
  private
    fProjectList: string;
    Function GetTestProjects: string;
    function GetProjectList: String;
  public
    Function BuildAndRunTests(AStartFolder: string): TBuildResult;
    Property ProjectList: String read GetProjectList;
  End;

  TBuildResultHelper = Record Helper for TBuildResult
  public
    Procedure Init;
  End;

var
  TestBuilderRootFolder: string = '';

function ThisTestRunnerProjectName: string;

implementation

uses RecursiveFolderSearch, uTestBuilder.ProjectInfo;

var
  TestRunnerProjectName: string;

function ThisTestRunnerProjectName: string;
begin
  if length(TestRunnerProjectName) = 0 then
    TestRunnerProjectName := ChangeFileExt(ExtractFileName(paramstr(0)
      ), '.dpr');
  result := TestRunnerProjectName;
end;

{ TDUnitmTestBuilder }

function TDUnitmTestBuilder.BuildAndRunTests(AStartFolder: string)
  : TBuildResult;
begin
  Assert(length(TestBuilderRootFolder) <> 0,
    'Root folder (uTestBuilder.TestBuilderRootFolder) must not be empty');
end;

function TDUnitmTestBuilder.GetProjectList: String;
begin
  if length(fProjectList) = 0 then
    fProjectList := GetTestProjects;
  result := fProjectList;
end;

function TDUnitmTestBuilder.GetTestProjects: string;
var
  lList: TStringlist;
  I: Integer;
  lProjectFile: string;
begin
  lList.text := SearchFolderForFiles('*.dpr', TestBuilderRootFolder);
  for I := 0 to lList.Count - 1 do
  begin
    lProjectFile := lList[I];
    if sametext(lProjectFile, ThisTestRunnerProjectName) then
      continue;

  end;
end;

{ TBuildResultHelper }

procedure TBuildResultHelper.Init;
begin
  Self.ProjectCount := 0;
  Self.TestCount := 0;
  Self.TestPass := 0;
  Self.TestFail := 0;
  Self.TestSkip := 0;
  Self.TestError := 0;

end;

end.
