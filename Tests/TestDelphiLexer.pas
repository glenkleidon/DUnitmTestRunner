
unit TestDelphiLexer;

interface

uses SysUtils, Classes, MiniTestFramework, Delphi.Lexer;

const
  CR = #13#10;
  CR2 = #13#10;

  TEST_DATA_DPR_FILE = '// Test data for DPR File' + CR2 +
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
    ' (* Final Comment ...' + CR + ' *)' + CR + 'end.' + CR;

  TEST_DATA_COMMENT_BRACE_WITHIN_BRACKETSTAR = 'library DummyTestDLL' + CR +
    '  (*' + '    This is a comment with a second {redundant comment}' + CR +
    '    inside it' + CR + '*)';

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

  // first row of Comment EOL Comments
  lText := PasFile[0];

  NewTest('Find // in first row without leading spaces');
  lResult := CheckForComment(lText);
  CheckisTrue(lResult.isComment, 'isComment is unexpectedly FALSE');

  NewTest('Start char is position 1');
  checkIsEqual(1, lResult.StartPos);

  NewTest('End Pos is to END OF LINE');
  checkIsEqual(length(lText), lResult.EndPos);

  NewTest('Comment is equal to the first line of text');
  checkIsEqual(lText, lResult.Token);

  // first row with leading Whitespace
  lText := '     '#9 + PasFile[0];
  NewTest('Find // in first row without leading spaces');
  lResult := CheckForComment(lText);
  CheckisTrue(lResult.isComment and lResult.isToken,
    'isComment is unexpectedly FALSE');

  NewTest('Start char is position 7');
  checkIsEqual(7, lResult.StartPos);

  NewTest('End Pos is to END OF LINE');
  checkIsEqual(length(lText), lResult.EndPos);

  NewTest('Comment is equal to the first line of text less whitespace');
  checkIsEqual(copy(lText, 7, MAXINT), lResult.Token);

  // Whole DPR File
  lText := PasFile.Text;
  NewTest('Find // in first row without leading spaces');
  lResult := CheckForComment(lText);
  CheckisTrue(lResult.isComment, 'isComment is unexpectedly FALSE');

  NewTest('Start char is position 1');
  checkIsEqual(1, lResult.StartPos);

  lText := PasFile[0];
  NewTest('End Pos is to END OF LINE');
  checkIsEqual(length(lText), lResult.EndPos);

  NewTest('Comment is equal to the first line of text');
  checkIsEqual(lText, lResult.Token);

end;

Procedure Check_Comment_Locates_BRACE_Comment;
begin
  notImplemented;
end;

Procedure Check_Comment_Locates_BRACKETSTAR_Comment;
var
  lText: string;
  lResult: TokenInfo;
begin

  lText := PasFile[24] + CR + PasFile[25];

  NewTest('BracketStar Comment Block located');
  lResult := CheckForComment(lText);
  CheckisTrue(lResult.isComment and lResult.isToken,
    'isComment is unexpectedly FALSE');

  NewTest('Start char is position 2');
  checkIsEqual(2, lResult.StartPos);

  NewTest('End Pos is to END OF BLOCK');
  checkIsEqual(length(lText), lResult.EndPos);

  NewTest('Comment is equal to the first line of text');
  checkIsEqual(copy(lText, 2, MAXINT), lResult.Token);

  NewTest('Embedded Brace is returned in Token');
  lText := copy(TEST_DATA_COMMENT_BRACE_WITHIN_BRACKETSTAR, 22, MAXINT);
  lResult := CheckForComment(lText);
  CheckisTrue(lResult.isComment and (pos('{redundant comment}', lResult.Token) >
    0), 'Embedded Brace is not present as expected: ' + CR + lResult.Token);

end;

Procedure Check_Comment_Locates_Compiler_Directive;
var
  lText: string;
  lResult: TokenInfo;
  i: integer;
begin

  lText := '';
  for i := 2 to 4 do
    lText := lText + PasFile[i] + CR;

  NewTest('Compiler Directive located');
  lResult := CheckForComment(lText);
  CheckisTrue(lResult.isCompilerDirective and lResult.isToken,
    'isCompilerDirective is unexpectedly FALSE');

  NewTest('Start char is position 1');
  checkIsEqual(1, lResult.StartPos);

  NewTest('End Pos is to END OF LINE');
  checkIsEqual(18, lResult.EndPos);

  NewTest('Directive is App Console');
  checkIsEqual('{$APPTYPE CONSOLE}', lResult.Token);

  lResult := CheckForComment(lText, lResult.EndPos+1);
  CheckisTrue(lResult.isCompilerDirective and lResult.isToken,
    'isCompilerDirective is unexpectedly FALSE');

  NewTest('Directive is Resource wildcard');
  checkIsEqual('{$R *.res}', lResult.Token);


end;

Procedure Check_Comment_Locates_Complex_BRACKETSTAR_Comment;
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

AddTestCase('Compiler Directive Located as expected',
  Check_Comment_Locates_Compiler_Directive);

AddTestCase('Brace Comments Located as expected',
  Check_Comment_Locates_BRACE_Comment);

AddTestCase('BracketStar (* *) Comments Located as expected',
  Check_Comment_Locates_BRACKETSTAR_Comment);

AddTestCase('Complex BracketStar (* *) Comments Located as expected',
  Check_Comment_Locates_Complex_BRACKETSTAR_Comment);

FinaliseSet(Finalise);

end.
