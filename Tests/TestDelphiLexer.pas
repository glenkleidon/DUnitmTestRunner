unit TestDelphiLexer;

interface

uses SysUtils, Classes, MiniTestFramework, Delphi.Lexer;

const
  CR = #13#10;
  CR2 = #13#10;

  TEST_DATA_DPR_FILE = '// Test data for DPR FIle' + CR2 +
    'program DummyTestFile;' + CR2 + '{$APPTYPE CONSOLE}' + CR2 + '{$R *.res}' +
    CR2 + 'uses' + CR + '  SysUtils,' + CR2 + '  MiniTestFramework,' + CR2 +
    '  TestDummyFile in ''TestDummyFile.pas'',' + CR2 + 'begin' + CR + '  try' +
    CR + '    Title(''Unit Test Cases for Delphi Lexer Project'');' + CR2 +
    '    // Note we are using the new Unit based Test Pattern,' + CR +
    '    // So the Unit Test Sets are encapsulated entirely in the Test Unit.' +
    CR + '    // -- ie we dont defined the sets in the DPR any more,' + CR +
    '    //    they are added in the initialization section of the' + CR +
    '    //    Test unit.' + CR2 + '    RunTestSets;' + CR + '    TestSummary;'
    + CR2 + '    if FindCmdLineSwitch(''p'') then' + CR + '      readln;' + CR2
    + '  except' + CR + '    on E: Exception do' + CR +
    '      Writeln(E.ClassName, '': '', E.Message);' + CR2 + '  end;' + CR2 +
    'end.' + CR;

implementation

var
  PasFile: TStringlist;

Procedure Prepare;
begin
  PasFile := TStringlist.Create;
end;

Procedure Finalise;
begin
  freeandnil(PasFile);
end;

procedure Check_Comment_Returns_False_For_Empty_string;
var
  lResult: TokenInfo;
begin
  NewTest('Empty string does cause exception');
  try
    lResult := CheckForComment('');
    checkException(Nil);
  except
    on e: exception do
      checkException(e);
  end;
  NewTest('IsComment is expected to be false');
  CheckIsFalse(lResult.isComment, 'IsComment is Unexpectedly true!');
end;

Procedure Check_Comment_Locates_EOL_Comment;
var
  lText: string;
  lResult: TokenInfo;
begin
  PasFile.Text := TEST_DATA_DPR_FILE;

  lText := PasFile[0]; // first row of Comment EOL Comments

  NewTest('Find // in first row without leading spaces');
  lResult := CheckForComment(lText);
  CheckisTrue(lresult.isComment,'isComment is unexpectedly FALSE');

  NewTest('Start char is position 1');
  checkIsEqual(1, lResult.StartPos);

  NewTest('End Pos is to END OF LINE');
  checkIsEqual(length(lText), lResult.EndPos);

  NewTest('Comment is equal to the first line of text');
  checkIsEqual(lText, lResult.Token);


end;

Procedure Check_Comment_Locates_BRACE_Comment;
begin
  notImplemented;
end;

Procedure Check_Comment_Locates_BRACKETSTART_Comment;
begin
  notImplemented;
end;

initialization

NewSet('Delphi Lexer Tests in TestDelphiLexer.pas');

PrepareSet(Prepare);

AddTestCase('Check Comment returns false for empty string',
  Check_Comment_Returns_False_For_Empty_string);

AddTestCase('EOL Comments Located as expected',
  Check_Comment_Locates_EOL_Comment);

AddTestCase('Brace Comments Located as expected',
  Check_Comment_Locates_BRACE_Comment);

AddTestCase('BracketStar (* *) Comments Located as expected',
  Check_Comment_Locates_BRACKETSTART_Comment);

FinaliseSet(Finalise);

end.
