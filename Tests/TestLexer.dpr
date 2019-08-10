program TestLexer;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  MiniTestFramework,
  TestDelphiLexer in 'TestDelphiLexer.pas',
  Delphi.Lexer in '..\ComponentSource\DelphiLexer\Delphi.Lexer.pas';

begin
  try
    Title('Unit Test Cases for Delphi Lexer Project');

    // Note we are using the new Unit based Test Pattern,
    // So the Unit Test Sets are encapsulated entirely in the Test Unit.
    // -- ie we dont defined the sets in the DPR any more,
    //    they are added in the initialization section of the
    //    Test unit.

    RunTestSets;
    TestSummary;

    if FindCmdLineSwitch('p') then
      readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
