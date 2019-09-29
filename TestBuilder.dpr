program TestBuilder;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  RecursiveFolderSearch in 'RecursiveFolderSearch.pas',
  MiniTestFramework in 'ComponentSource\DUnitm\MiniTestFramework.pas',
  uTestBuilder in 'uTestBuilder.pas',
  uTestBuilder.ProjectInfo in 'uTestBuilder.ProjectInfo.pas',
  DUnitm.Constants in 'DUnitm.Constants.pas',
  Delphi.Lexer in 'ComponentSource\DelphiLexer\Delphi.Lexer.pas',
  uTestBuilder.Scripts in 'uTestBuilder.Scripts.pas',
  Delphi.Versions in 'ComponentSource\DelphiLexer\Delphi.Versions.pas',
  Delphi.DProj in 'ComponentSource\DelphiLexer\Delphi.DProj.pas',
  XMLNodeReader in 'ComponentSource\DelphiLexer\XMLNodeReader.pas',
  TestDelphiDProj in 'Tests\TestDelphiDProj.pas',
  windows,
  ShellApi;

var lStartFolder: string;
begin
  try
    // Show Help
    if FindCmdLineSwitch('?') or findCmdLineSwitch('help') then
    begin
      HelpMessage;
      exit;
    end;

    // Get the Builders start folder
    TestBuilderRootFolder := extractFilePath(Paramstr(0));

    // Get the Start in folder.
    lStartFolder := GetStartFolder;
    if (copy(lStartfolder,1,1)='.') then
      lStartFolder := Expandfilename(TestBuilderRootFolder+lStartFolder);
    if length(lStartFolder)=0 then
    begin
      writeln('Syntax Error: Start in Folder not specified');
      HelpMessage;
      exit;
    end;

    // Set the Script Name
    ScriptName := GetScriptNameCmd;

    // Build the Script
    TDUnitmTestBuilder.BuildAndRunTests(lStartFolder, GetVersionCmd,
       GetExcludeListCmd, GetIncludeListCmd);

    // Optionally run the Script.
    if FindCmdLineSwitch('b') then
    begin
      Writeln('Wrote Test Runner script to ',scriptName);
      exit;
    end;
    Writeln('Building and Executing Tests');
    ShellExecute(0, 'open', pchar(ScriptName),Pchar(GetOpenWithCMD),0,sw_hide);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
