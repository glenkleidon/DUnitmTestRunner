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
  Delphi.Lexer in 'ComponentSource\DelphiLexer\Delphi.Lexer.pas';

  var lStartFolder: string;
begin
  try
    TestBuilderRootFolder := extractFilePath(Paramstr(0));
    lStartFolder := Paramstr(1);
    if (copy(lStartfolder,1,1)='.') then
      lStartFolder := Expandfilename(TestBuilderRootFolder+lStartFolder);
    TDUnitmTestBuilder.BuildAndRunTests(lStartFolder);
    if FindCmdLineSwitch('p') then readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
