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
    fProjectList: TStringList;
    fRootFolder: string;
    Function GetTestProjects: string;
    function GetProjectList: String;
    function BuildandRun: TBuildResult;
    function GetRootFolder: String;
    procedure SetRootFolder(const Value: String);
  public
    constructor Create;
    destructor Destroy; override;
    class Function BuildAndRunTests(AStartFolder: string): TBuildResult;
    Property ProjectList: String read GetProjectList;
    Property RootFolder: String read GetRootFolder write SetRootFolder;
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

function TDUnitmTestBuilder.BuildandRun: TBuildResult;
var
  lList: TStringList;
begin
  result.Init;
  GetTestProjects;
end;

Class function TDUnitmTestBuilder.BuildAndRunTests(AStartFolder: string)
  : TBuildResult;
var
  lTestBuilder: TDUnitmTestBuilder;
begin
  Assert(length(TestBuilderRootFolder) <> 0,
    'Root folder (uTestBuilder.TestBuilderRootFolder) must not be empty');
  lTestBuilder := TDUnitmTestBuilder.Create;
  try
    lTestBuilder.RootFolder := AStartFolder;
    result := lTestBuilder.BuildandRun;
  finally
    freeandnil(lTestBuilder);
  end;
end;

constructor TDUnitmTestBuilder.Create;
begin
  self.fProjectList := TStringList.Create;
end;

destructor TDUnitmTestBuilder.Destroy;
begin
  freeandnil(self.fProjectList);
  inherited;
end;

function TDUnitmTestBuilder.GetProjectList: String;
begin
  if length(fProjectList.text) = 0 then
    result := GetTestProjects
  else result := fProjectList.text;
end;

function TDUnitmTestBuilder.GetRootFolder: String;
begin
  if length(self.fRootFolder) = 0 then
    fRootFolder := TestBuilderRootFolder;
  result := self.fRootFolder;
end;

function TDUnitmTestBuilder.GetTestProjects: string;
var
  I: Integer;
  lProjectFile: string;
  lProjectInfo: TTestProjectInfo;
begin
  self.fProjectList.text := SearchFolderForFiles('*.dpr', RootFolder);
  for I := fProjectList.Count - 1 downto 0 do
  begin
    lProjectFile := fProjectList[I];
    if sametext(lProjectFile, ThisTestRunnerProjectName) then
    begin
      fProjectList.Delete(I);
      continue;
    end;

    // Extract Test ProjectInfo
    lProjectInfo := GetTestProjectInfo(lProjectFile);
    if not lProjectInfo.IsTestProject then
    begin
      fProjectList.Delete(I);
      continue;
    end;
    writeln(lProjectFile, ' is a test file');
  end;
  Result := Self.fProjectList.text;
end;

procedure TDUnitmTestBuilder.SetRootFolder(const Value: String);
begin
  self.fRootFolder := Value;
end;

{ TBuildResultHelper }

procedure TBuildResultHelper.Init;
begin
  self.ProjectCount := 0;
  self.TestCount := 0;
  self.TestPass := 0;
  self.TestFail := 0;
  self.TestSkip := 0;
  self.TestError := 0;

end;

end.
