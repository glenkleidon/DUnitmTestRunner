unit uTestBuilder;

interface
 {$IFDEF CONDITIONALEXPRESSIONS}
 {$IF CompilerVersion >= 17.0}
      {$DEFINE HAS_INLINE}
      {$DEFINE HAS_HELPERS}
 {$IFEND}
 {$ENDIF}

uses SysUtils, Classes;
const
   DEFAULT_SCRIPT_NAME = 'TestRunner.bat';

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
    fOwnsInclude: boolean;
    fOwnsExclude: boolean;
    fIncludeList: TStrings;
    fExcludeList: TStrings;
    fProjectList: TStringList;
    fRootFolder: string;
    Function GetTestProjects: string;
    function GetProjectList: String;
    function BuildandRun(ADelphiVersion: string=''): TBuildResult;
    function GetRootFolder: String;
    procedure SetRootFolder(const Value: String);
    function GetIncludeList: TStrings;
    function GetExcludeList: TStrings;
    Procedure SetIncludeList(const AValue: TStrings);
    Procedure SetExcludeList(const AValue: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RefreshProjectList;
    class Function BuildAndRunTests(AStartFolder: string; AVersion: string; AExclude: string='';
      AInclude: string = ''): TBuildResult;
    Property ProjectList: String read GetProjectList;
    Property RootFolder: String read GetRootFolder write SetRootFolder;
    Property ExcludeList: TStrings read GetExcludeList write SetExcludeList;
    Property IncludeList: TStrings read GetIncludeList write SetIncludeList;
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
  ScriptName: string = '';

function ThisTestRunnerProjectName: string;
Procedure BuildResultInit(var ABuildResult: TBuildREsult);
function GetIncludeListCmd: string;
function GetExcludeListCmd: string;
function GetScriptNameCmd: string;
function GetVersionCmd: string;
function GetOpenWithCMD: string;
function GetStartFolder: string;
function HelpMessage: string;


implementation

uses RecursiveFolderSearch, uTestBuilder.ProjectInfo, uTestBuilder.Scripts;
const HELP_MESSAGE =
//   12345678901234567890123456789012345678901234567890123456789012345678901234567890
   '>TestBuilder [-|/][?|help] | [[-|/]b] [[-|/][EXCLUDE[:]<[*]|Project;Project...>]'#13#10 +
   '     [[-|/][INCLUDE[:]<Project1;Project2...>] [[-|/]S:<ScriptName>] '#13#10+
   '     [[-|/]V:<DelphiVersion>] [[-|/]O:<OpenWith>]'#13#10+
   '     <StartInFolder>'#13#10#13#10+
   ' ?|help           : This Message'#13#10+
   ' b                : Build SCRIPT ONLY, do not RUN'#13#10 +
   ' EXCLUDE          : List of Project Names to exclude ("*"=All)'#13#10+
   '                    eg: /EXCLUDE:TestMyFunc;TestMyProc'#13#10 +
   ' INCLUDE          : List of Project Names to include (if otherwise excluded)'#13#10+
   '                    eg: /INCLUDE:TestMyFunc;TestMyProc'#13#10 +
   ' V:<DelphiVersion>: The version of delphi to build. Use short name or VersionId'#13#10+
   '                    eg: -v:D7 -v:Delphi7 -v:XE2 -v:Berlin -v:Seattle etc'#13#10+
   ' O:<OpenWith>     : Open the test result with a specific program default (none)'#13#10+
   '                    eg: -O:Notepad - opens the Test results in notepad'#13#10+
   ' <ScriptName>     : Full pathname to Test Runner Script '#13#10+
   '                  (<StartInFolder>'+DEFAULT_SCRIPT_NAME+' if not supplied)'#13#10 +
   ' <StartInFolder>  : (Required) Folder to recursively search for test projects.';

var
  TestRunnerProjectName: string;

function HelpMessage: string;
begin
  writeln;
  writeln(HELP_Message);
end;

// For backward compatibility, primitive search is used instead of
// FindCmdSwitch which was not very rich in earler versions of Delphi
function GetCMDlineSwitch(ASwitchChars: string; ASwitch: String): string;
var
  i,l: integer;
  lSwitchChar : char;
  lSwitch: string;
begin
  result := '';
  l := length(ASwitch);
  for i := 1 to ParamCount do
  begin
    if length(Paramstr(i))=0 then continue;
    lSwitchChar := char(Paramstr(i)[1]);
    if pos(lSwitchChar,ASwitchChars)=0 then continue;
    lSwitch := copy(Paramstr(i),2,l);
    if sametext(ASwitch, lSwitch) then
    begin
     result := Copy(Paramstr(i),l+2,MaxInt);
     exit;
    end;
  end;
end;

function GetCommandLineValue(ASwitch: string): string;
begin
  result := GetCMDlineSwitch('/-',ASwitch);
  if copy(Result,1,1)=':' then result := copy(Result,2,Maxint);
end;

function GetIncludeListCmd: string;
begin
  result := GetCommandLineValue('INCLUDE');
end;

function GetExcludeListCmd: string;
begin
  result := GetCommandLineValue('EXCLUDE');
end;

function GetScriptNameCmd: string;
begin
  result := GetCommandLineValue('S');
end;

function GetVersionCmd: string;
begin
  result := GetCommandLineValue('V');
end;

function GetOpenWithCMD: string;
begin
  result := GetCommandLineValue('O');
end;

function GetStartFolder: string;
var
  i: integer;
  lSwitchChar : char;
begin
  result := '';
  for i := 1 to ParamCount do
  begin
    if length(Paramstr(i))=0 then continue;
    lSwitchChar := char(Paramstr(i)[1]);
    if pos(lSwitchChar,'/-')>0 then continue;
    result := paramstr(i);
    exit;
  end;
end;

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

  ////
  // Ensure that this project is a target
  ///
  function SkipProject: boolean;
  var
    lProjectLabel: string;
  begin
     lProjectLabel := changeFileExt(ExtractFileName(lProject),'');
     result :=
       (length(lProjectLabel)=0) or
       (sameText(lProjectLabel+'.dpr', TestBuilderProject)) or
       (
         (Self.ExcludeList.IndexOf('*')>=0) and
         (
           (Self.IncludeList.IndexOf(lProjectLabel)<0) and
           (Self.IncludeList.IndexOf(lProjectLabel+'.dpr')<0)
         )
       ) or
       (Self.ExcludeList.IndexOf(lProjectLabel)>=0) or
       (Self.ExcludeList.IndexOf(lProjectLabel+'.dpr')>=0);
  end;
begin
  BuildResultInit(result);
  lScript := TStringlist.Create;
  lProjectList := TStringList.Create;
  try
    lProjectList.text := GetTestProjects;
    for i := 0 to lProjectList.Count - 1 do
    begin
     lProject := lProjectList[i];
     if SkipProject then Continue;
     writeln(copy(lProject,length(RootFolder),MaxInt));

     lTestSection := lTestSection +
       Commandline(lProject, ADelphiVersion, DEFAULT_DUNITM_SCRIPT_COMMAND);
    end;

    lScript.Text := stringReplace(DUNITM_TEST_RUNNER_SCRIPT, '<TESTEXECUTION>', lTestSection,[rfIgnoreCase]);
    lScript.Text := StringReplace(lScript.Text, '<ROOTPATH>', includeTrailingPathdelimiter(RootFolder), [rfIgnoreCase, rfReplaceAll]);
    if length(ScriptName)=0 then
      ScriptName := DEFAULT_SCRIPT_NAME;
    if pos('\',ScriptName)=0 then
      ScriptName := includeTrailingPathdelimiter(RootFolder) + ScriptName;
    lScript.SaveToFile(ScriptName);
  finally
    freeandnil(lProjectList);
    Freeandnil(lScript);
  end;

end;

Class function TDUnitmTestBuilder.BuildAndRunTests(AStartFolder: string; AVersion: string;
   AExclude: string=''; AInclude: string = '')
  : TBuildResult;
var
  lTestBuilder: TDUnitmTestBuilder;
begin
  Assert(length(TestBuilderRootFolder) <> 0,
    'Root folder (uTestBuilder.TestBuilderRootFolder) must not be empty');
  lTestBuilder := TDUnitmTestBuilder.Create;
  try
    lTestBuilder.RootFolder := AStartFolder;
    lTestBuilder.IncludeList.Text := StringReplace(AInclude,';',
       #13#10,[rfReplaceAll]);
    lTestBuilder.ExcludeList.Text := StringReplace(AExclude,';',
       #13#10,[rfReplaceAll]);
    result := lTestBuilder.BuildandRun(AVersion);
  finally
    freeandnil(lTestBuilder);
  end;
end;

constructor TDUnitmTestBuilder.Create;
begin
  self.fProjectList := TStringList.Create;
  self.fOwnsInclude := false;
  self.fOwnsExclude := false;
  Self.fExcludeList := nil;
  self.fIncludeList := nil;
end;

destructor TDUnitmTestBuilder.Destroy;
begin
  if self.fOwnsInclude then freeandnil(self.fIncludeList);
  if Self.fOwnsExclude then freeandnil(Self.fExcludeList);
  freeandnil(self.fProjectList);
  inherited;
end;

function TDUnitmTestBuilder.GetExcludeList: TStrings;
begin
  if self.fExcludeList=nil then
  begin
    self.fExcludeList := TStringlist.create;
    self.fOwnsExclude := True;
  end;
  result := Self.fExcludeList;
end;

function TDUnitmTestBuilder.GetIncludeList: TStrings;
begin
  if self.fIncludeList=nil then
  begin
    self.fIncludeList := TStringlist.create;
    self.fOwnsInclude := True;
  end;
  result := Self.fIncludeList;

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
   // writeln(copy(lProjectFile,length(RootFolder),MaxInt));
  end;
  result := self.fProjectList.text;
end;

procedure TDUnitmTestBuilder.RefreshProjectList;
begin
  GetTestProjects;
end;

procedure TDUnitmTestBuilder.SetExcludeList(const AValue: TStrings);
begin
  if self.fOwnsExclude then freeandnil(Self.fExcludeList);
  self.fOwnsExclude := false;
  Self.fExcludeList := AValue;
end;

procedure TDUnitmTestBuilder.SetIncludeList(const AValue: TStrings);
begin
  if self.fOwnsInclude then freeandnil(Self.fIncludeList);
  self.fOwnsInclude := false;
  Self.fIncludeList := AValue;
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
