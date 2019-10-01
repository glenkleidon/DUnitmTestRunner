unit TestDelphiLexer;

interface

uses SysUtils, Classes, MiniTestFramework, Delphi.Lexer;

const
  CR = #13#10;
  CR2 = #13#10;

  TEST_DATA_DPR_FILE = '// Test data for DPR File' + CR2 +
    'program DummyTestFile;' + CR2 + '{$APPTYPE CONSOLE}' + CR2 + '{$R *.res}' +
    CR2 + 'uses' + CR + '  SysUtils,' + CR2 + '  MiniTestFramework,' + CR2 +
    '  TestDummyFile in ''TestDummyFile.pas'';' + CR2 + 'begin' + CR + '  try' +
    CR + '    Title(''Unit Test Cases for Delphi Lexer Project'');' + CR2 +
    '    // Note we are using the new Unit based Test Pattern,' + CR +
    '    // So the Unit Test Sets are encapsulated entirely in the Test Unit.' +
    CR + '    // -- ie we dont defined the sets in the DPR any more,' + CR +
    '    //    they are added in the initialization section of the' + CR +
    '    //    Test unit.' + CR2 + '{ Run the tests}' + CR + '    RunTestSets; '
    + CR + '  {Output the Summary to Screen} ' + CR + '    TestSummary;' + CR2 +
    '    if FindCmdLineSwitch(''p'') then' + CR + '      readln;' + CR2 +
    '  except' + CR + '    on E: Exception do' + CR +
    '      Writeln(E.ClassName, '': '', E.Message);' + CR2 + '  end;' + CR2 +
    ' (* Final Comment ...' + CR + ' *)' + CR + 'end.' + CR;

  TEST_DATA_UNIT_1 =
  'unit DummyUnit;'+CR2+
  'interface'+CR2+
  ' uses SysUtils, SomeUnit, SomeOtherUnit;'+CR2+
  'const'+CR +
  '  SOME_CONSTANT=2;'+CR2+
  'implementation'+CR2+
  ' uses SomeThirdUnit, SomeLastUnit;'+CR2+
  'Function Abc: string;'+CR+
  'begin'+CR+
  '  result := ''ABC'';'+CR+
  'end;'+CR2+
  'initialization'+CR2+
  ' DoNothing;'+CR2+
  'end.';






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
  lResult: TTokenInfo;
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
  CheckIsFalse(lResult.TokenType = dtComment,
    'IsComment is Unexpectedly true!');
end;

Procedure Check_Comment_Locates_EOL_Comment;
var
  lText: string;
  lResult: TTokenInfo;
begin
  PasFile.Text := TEST_DATA_DPR_FILE;

  // first row of Comment EOL Comments
  lText := PasFile[0];

  NewTest('Find // in first row without leading spaces');
  lResult := CheckForComment(lText);
  CheckisTrue(lResult.TokenType = dtComment, 'isComment is unexpectedly FALSE');

  NewTest('Start char is position 1');
  checkIsEqual(1, lResult.StartPos);

  NewTest('End Pos is to END OF LINE');
  checkIsEqual(length(lText)+1, lResult.EndPos);

  NewTest('Comment is equal to the first line of text');
  checkIsEqual(lText, lResult.Token);

  // first row with leading Whitespace
  lText := '     '#9 + PasFile[0];
  NewTest('Find // in first row without leading spaces');
  lResult := CheckForComment(lText);
  CheckisTrue((lResult.TokenType = dtComment) and lResult.isToken,
    'isComment is unexpectedly FALSE');

  NewTest('Start char is position 7');
  checkIsEqual(7, lResult.StartPos);

  NewTest('End Pos is to END OF LINE');
  checkIsEqual(length(lText)+1, lResult.EndPos);

  NewTest('Comment is equal to the first line of text less whitespace');
  checkIsEqual(copy(lText, 7, MAXINT), lResult.Token);

  // Whole DPR File
  lText := PasFile.Text;
  NewTest('Find // in first row without leading spaces');
  lResult := CheckForComment(lText);
  CheckisTrue(lResult.TokenType = dtComment, 'isComment is unexpectedly FALSE');

  NewTest('Start char is position 1');
  checkIsEqual(1, lResult.StartPos);

  lText := PasFile[0];
  NewTest('End Pos is to END OF LINE');
  checkIsEqual(length(lText), lResult.EndPos);

  NewTest('Comment is equal to the first line of text');
  checkIsEqual(lText, lResult.Token);

end;

Procedure Check_Comment_Locates_BRACE_Comment;
var
  lText: string;
  lResult: TTokenInfo;
begin
  PasFile.Text := TEST_DATA_DPR_FILE;

  lText := PasFile[18] + CR;

  NewTest('Brace Comment Block located');
  lResult := CheckForComment(lText);
  CheckisTrue((lResult.TokenType = dtComment) and lResult.isToken,
    'isComment is unexpectedly FALSE');

  NewTest('Start char is position 2');
  checkIsEqual(3, lResult.StartPos);

  NewTest('End Pos is to END OF BLOCK');
  checkIsEqual(length(lText) - 3, lResult.EndPos);

  NewTest('Comment is equal to the first line of text');
  checkIsEqual(copy(lText, 3, 30), lResult.Token);

end;

Procedure Check_Comment_Locates_BRACKETSTAR_Comment;
var
  lText: string;
  lResult: TTokenInfo;
begin
  PasFile.Text := TEST_DATA_DPR_FILE;

  lText := PasFile[26] + CR + PasFile[27];

  NewTest('BracketStar Comment Block located');
  lResult := CheckForComment(lText);
  CheckisTrue((lResult.TokenType = dtComment) and lResult.isToken,
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
  CheckisTrue((lResult.TokenType = dtComment) and
    (pos('{redundant comment}', lResult.Token) > 0),
    'Embedded Brace is not present as expected: ' + CR + lResult.Token);

end;

Procedure Check_Comment_Locates_Compiler_Directive;
var
  lText: string;
  lResult: TTokenInfo;
  i: integer;
begin

  lText := '';
  for i := 2 to 4 do
    lText := lText + PasFile[i] + CR;

  NewTest('Compiler Directive located');
  lResult := CheckForComment(lText);
  CheckisTrue((lResult.TokenType=dtCompilerDirective) and lResult.isToken,
    'isCompilerDirective is unexpectedly FALSE');

  NewTest('Start char is position 1');
  checkIsEqual(1, lResult.StartPos);

  NewTest('End Pos is to END OF LINE');
  checkIsEqual(18, lResult.EndPos);

  NewTest('Directive is App Console');
  checkIsEqual('{$APPTYPE CONSOLE}', lResult.Token);

  lResult := CheckForComment(lText, lResult.EndPos + 1);
  CheckisTrue((lResult.TokenType=dtCompilerDirective) and lResult.isToken,
    'isCompilerDirective is unexpectedly FALSE');

  NewTest('Directive is Resource wildcard');
  checkIsEqual('{$R *.res}', lResult.Token);

end;

Procedure Check_Comment_Locates_Complex_BRACKETSTAR_Comment;
begin
  notImplemented;
end;

Procedure Check_NextToken_Returns_each_token_in_Turn;
var
  lToken: TTokenInfo;
  lPos: integer;
begin
  lPos := 1;
  // Locate the Compiler Directive
  NewTest('Skip Over the Comments, locating "Program" Token');
  Repeat
    lToken := NextToken(PasFile.Text, lPos);
    lPos := lToken.EndPos + 1;
  Until lToken.TokenType<>dtComment;
  checkIsEqual('program', lToken.Token);

  lToken := NextToken(PasFile.Text, lPos);
  lPos := lToken.EndPos + 1;

  NewTest('locate Program Name "DummyTestFile"');
  checkIsEqual('DummyTestFile', lToken.Token);

  NewTest('Locate Compiler Directive');
  Repeat
    lToken := NextToken(PasFile.Text, lPos);
    lPos := lToken.EndPos + 1;
  Until (lToken.TokenType=dtCompilerDirective) or (lPos>length(PasFile.Text));
  CheckisTrue(lToken.TokenType=dtCompilerDirective,
    'Token was expected to be a Compiler Directive or comment but was not');
  checkIsEqual('{$APPTYPE CONSOLE}', lToken.Token);

  NewTest('Locate 2nd Compiler Directive');
  lToken := NextToken(PasFile.Text, lPos);
  lPos := lToken.EndPos + 1;
  CheckisTrue(lToken.TokenType=dtCompilerDirective);

  NewTest('Locate Uses Directive');
  lToken := NextToken(PasFile.Text, lPos);
  lPos := lToken.EndPos + 1;
  CheckisEqual('uses',lToken.Token);

end;

Procedure Check_Locate_Token_returns_correct_token;
var
  lToken: TTokenInfo;
begin

  newTest('Locate Uses Clause');
  lToken := LocateToken('uses', dtKeyword,TEST_DATA_DPR_FILE, 1, 'Implementation' );
  CheckIsEqual('uses',lToken.Token);
  CheckisTrue(ltoken.TokenType=dtKeyWord);

  newTest('Dont find non existent token in Interface section');
  lToken := LocateToken('nonexistent', dtunknown,TEST_DATA_DPR_FILE, 1, 'Implementation' );
  CheckIsTrue(length(lToken.Token)=0);
  CheckIsTrue(lToken.TokenType=dtUnknown);

  NewTest('Locate Uses Clause in Implementation');
  lToken := LocateToken('uses', dtKeyword, TEST_DATA_UNIT_1,1, 'Implementation');
  lToken := LocateToken('uses', dtKeyword, TEST_DATA_UNIT_1,lToken.EndPos, 'initialization');
  CheckIsEqual('uses',lToken.Token);
  CheckisTrue(ltoken.TokenType=dtKeyword);
  lToken := NextToken(TEST_DATA_UNIT_1, lToken.EndPos+1);
  checkisEqual('SomeThirdUnit', lToken.Token);
  NewTest('Deliberate Failure');
  checkisTrue(false);
end;

Procedure Check_TextBetweenTokens_Returns_Correct_result;
var lResult: string;
begin
  NewTest('Located the Uses Clause text for the Test Unit');
  lResult := TextBeweenTokens('uses', dtKeyWord, ';', dtSeparator,
    TEST_DATA_UNIT_1);
  DisplayModeRows(true);
  checkIsEqual(' SysUtils, SomeUnit, SomeOtherUnit',lResult);




end;

initialization

NewSet('Delphi Lexer Tests in TestDelphiLexer.pas');

PrepareSet(Prepare);
// Comments
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

// Next Token.

AddTestCase('Find Tokens in the File',
  Check_NextToken_Returns_each_token_in_Turn);
AddTestCase('Find Tokens using LocateToken',
  Check_Locate_Token_returns_correct_token);
AddTestCase('Find Text between Tokens "Uses" and ","',
  Check_TextBetweenTokens_Returns_Correct_result);

FinaliseSet(Finalise);

end.
