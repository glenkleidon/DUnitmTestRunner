unit uTestBuilder;

interface
 {$IFDEF CONDITIONALEXPRESSIONS}
 {$IF CompilerVersion >= 17.0}
      {$DEFINE HAS_INLINE}
      {$DEFINE HAS_HELPERS}
 {$IFEND}
 {$ENDIF}

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
    function BuildandRun(ADelphiVersion: string=''): TBuildResult;
    function GetRootFolder: String;
    procedure SetRootFolder(const Value: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RefreshProjectList;
    class Function BuildAndRunTests(AStartFolder: string): TBuildResult;
    Property ProjectList: String read GetProjectList;
    Property RootFolder: String read GetRootFolder write SetRootFolder;
  End;

  {$IFDEF HAS_HELPERS}
  TBuildResultHelper = Record Helper for TBuildResult
  public
    Procedure Init;
  End;
  {$ENDIF}

var
  TestBuilderRootFolder: string = '';
  TestBuilderProject : string = '';

function ThisTestRunnerProjectName: string;
Procedure BuildResultInit(var ABuildResult: TBuildREsult);

implementation

uses RecursiveFolderSearch, uTestBuilder.ProjectInfo, uTestBuilder.Scripts;

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

function TDUnitmTestBuilder.BuildandRun(ADelphiVersion: string=''): TBuildResult;
var
  lProjectList, lScript: TStringList;
  i: Integer;
  lProject: string;
  lTestSection : string;
  lBuilderName: string;
begin
  BuildResultInit(result);
  lScript := TStringlist.Create;
  lProjectList := TStringList.Create;
  try
    lProjectList.text := GetTestProjects;
    for i := 0 to lProjectList.Count - 1 do
    begin
     lProject := lProjectList[i];
     if (length(lProject)=0) or
        (sameText(ExtractFileName(lProject), TestBuilderProject)) then continue;
     lTestSection := lTestSection +
       Commandline(lProject, ADelphiVersion, DEFAULT_DUNITM_SCRIPT_COMMAND);
    end;

    lScript.Text := stringReplace(DUNITM_TEST_RUNNER_SCRIPT, '<TESTEXECUTION>', lTestSection,[rfIgnoreCase]);
    lScript.Text := StringReplace(lScript.Text, '<ROOTPATH>', includeTrailingPathdelimiter(RootFolder), [rfIgnoreCase, rfReplaceAll]);

    lScript.SaveToFile(includeTrailingPathdelimiter(RootFolder) + 'Test_Runner.bat');
  finally
    freeandnil(lProjectList);
    Freeandnil(lScript);
  end;

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
  else
    result := fProjectList.text;
end;

function TDUnitmTestBuilder.GetRootFolder: String;
begin
  if length(self.fRootFolder) = 0 then
    fRootFolder := TestBuilderRootFolder;
  result := self.fRootFolder;
end;

function TDUnitmTestBuilder.GetTestProjects: string;
var
  i: Integer;
  lProjectFile: string;
  lProjectInfo: TTestProjectInfo;
begin
  self.fProjectList.text := SearchFolderForFiles('*.dpr', RootFolder);
  for i := fProjectList.Count - 1 downto 0 do
  begin
    lProjectFile := fProjectList[i];
    if sametext(lProjectFile, ThisTestRunnerProjectName) then
    begin
      fProjectList.Delete(i);
      continue;
    end;

    // Extract Test ProjectInfo
    lProjectInfo := GetTestProjectInfo(lProjectFile);
    if not lProjectInfo.IsTestProject then
    begin
      fProjectList.Delete(i);
      continue;
    end;
    writeln(lProjectFile, ' is a test file');
  end;
  result := self.fProjectList.text;
end;

procedure TDUnitmTestBuilder.RefreshProjectList;
begin
  GetTestProjects;
end;

procedure TDUnitmTestBuilder.SetRootFolder(const Value: String);
begin
  self.fRootFolder := Value;
end;

{ TBuildResultHelper }
Procedure BuildResultInit(var ABuildResult: TBuildREsult);
begin
  ABuildResult.ProjectCount := 0;
  ABuildResult.TestCount := 0;
  ABuildResult.TestPass := 0;
  ABuildResult.TestFail := 0;
  ABuildResult.TestSkip := 0;
  ABuildResult.TestError := 0;

end;
{$IFDEF HAS_HELPERS}
procedure TBuildResultHelper.Init;
begin
  BuildResultInit(self);
end;
{$ENDIF}

initialization
 TestBuilderProject := ChangeFileExt(ExtractFileName(Paramstr(0)),'.dpr');


end.
