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

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
