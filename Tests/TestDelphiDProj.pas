unit TestDelphiDProj;

interface

uses
  SysUtils,
  minitestframework,
  Delphi.DProj;

const
  TEST_DATA_DPROJ_2003 =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">' +
    '  <PropertyGroup>' +
    '      <ProjectGuid>{E1C2E563-4870-44D1-9394-BAE6D5485E96}</ProjectGuid>' +
    '      <ProjectVersion>16.1</ProjectVersion>' +
    '      <FrameworkType>None</FrameworkType>' +
    '      <MainSource>TestRunnerTests.dpr</MainSource>' +
    '      <Base>True</Base>' +
    '      <Config Condition="`$(Config)`==``">Debug</Config>' +
    '      <Platform Condition="`$(Platform)`==``">Win32</Platform>' +
    '      <TargetedPlatforms>3</TargetedPlatforms>' +
    '      <AppType>Console</AppType>' + '  </PropertyGroup>' +
    '  <PropertyGroup Condition="`$(Config)`==`Base` or `$(Base)`!=``">' +
    '      <Base>true</Base>' + '  </PropertyGroup>' + '</Project>';

implementation

uses classes;

procedure Prepare;
begin

end;

procedure Finalise;
begin

end;

Function RemoveBackTicks(AText: string): string;
begin
  result := stringreplace(AText, '`', '''', [rfReplaceAll]);
end;

// Comments
Procedure StripWhiteSpace_Works_as_expected;
var
  lResult, lExpected, lTestMessage: string;
begin
  newTest('Empty String returns empty');
  lExpected := '';
  lResult := stripWhiteSpace('');
  checkisEqual(lExpected, lResult);

  newTest('String without whitespace returns unchanged');
  lExpected := 'X';
  lResult := stripWhiteSpace('X');
  checkisEqual(lExpected, lResult);

  lExpected :=
    'LSKDJFLJSDFLjpoUSOPEDRFUO(SDUF)(S*DF)*SD)F*)SD*F)*DSFZXCvxcvzxcvX';
  lResult := stripWhiteSpace(lExpected);
  checkisEqual(lExpected, lResult);

  newTest('All Whitespace returns empty');
  lExpected := '';
  lResult := stripWhiteSpace(XML_WHITESPACE);
  checkisEqual(lExpected, lResult);

  newTest('Simple text returns without whitespace');
  lExpected := 'thisistextwithoutspaces.';
  lResult := stripWhiteSpace('this is text without spaces.');
  checkisEqual(lExpected, lResult);

  newTest('Text with trailing whitespace returns without whitespace');
  lExpected := 'thisistextwithoutwhitespace.';
  lResult := stripWhiteSpace('this is text without white space. '#9' '#13);
  checkisEqual(lExpected, lResult);

  newTest('Text with leading whitespace returns without whitespace');
  lExpected := 'thisistextwithoutwhitespace.';
  lResult := stripWhiteSpace
    (#11#12#13#10'   this is text without white space. ');
  checkisEqual(lExpected, lResult);

  newTest('Text with multiple lines returns without whitespace');
  lExpected := 'thisistextwithoutwhitespace.';
  lResult := stripWhiteSpace
    ('   this is'#13#10' text without'#13#10#13#10' white space. '#13#10#13#10);
  checkisEqual(lExpected, lResult);
end;

procedure ResetXMLNode_Works_as_expected;
var
  lResult, lExpected: TXMLNode;
begin
  newTest('Empty result is assigned default values');
  lResult := ResetXMLNode;
  checkisEqual('', lResult.Path);
  checkisEqual('', lResult.Name);
  checkisEqual('', lResult.Value);
  checkisFalse(lResult.HasValue);
  newTest('Empty Attributes in result has length of zero');
  checkisEqual(0, length(lResult.Attributes));

  lResult.Path := '/some/node';
  lResult.Value := 'not empty';
  lResult.Name := 'not empty name';
  setlength(lResult.Attributes, 1);
  lResult.Attributes[0].Name := 'Not Empty Attribute name';
  lResult.Attributes[0].Value := 'Not an empty attribute value';
  lResult.Value := 'not an empty value';
  lResult.HasValue := length(lResult.Value) > 0;

  newTest('Confirm result has content');
  checkisFalse(length(lResult.Path) = 0);
  checkisFalse(length(lResult.Name) = 0);
  checkisFalse(length(lResult.Attributes) = 0);
  checkisFalse(length(lResult.Attributes[0].Name) = 0);
  checkisFalse(length(lResult.Attributes[0].Value) = 0);
  checkisFalse(length(lResult.Value) = 0);
  checkistrue(lResult.HasValue);

  newTest('Confirm ResetXMLNode empties the values');
  lResult := ResetXMLNode;
  checkisEqual('', lResult.Path);
  checkisEqual('', lResult.Name);
  checkisEqual('', lResult.Value);
  checkisFalse(lResult.HasValue);
  newTest('Empty Attributes in result has length of zero');
  checkisEqual(0, length(lResult.Attributes));
end;

procedure GetXMLAttributes_Works_as_Expected;
var
  lAttributes: TXMLAttributes;
begin

  newTest('Empty string returns empty set');
  lAttributes := GetXMLAttributes('');
  checkistrue(length(lAttributes) = 0);

  newTest('invalid text returns a comment set');
  lAttributes := GetXMLAttributes(' not any attributes ');
  checkistrue(length(lAttributes) = 1);
  checkisEqual(XML_ATTRIBUTE_COMMENT_NAME, lAttributes[0].Name);
  checkisEqual('not any attributes', lAttributes[0].Value);

  newTest('Simple attribute returns as expected');
  lAttributes := GetXMLAttributes('Condition="simple condition" ');
  checkistrue(length(lAttributes) = 1);
  checkisEqual(DPROJ_CONDITION_ATTRIBUTE, lAttributes[0].Name);
  checkisEqual('simple condition', lAttributes[0].Value);

  newTest('Multiple simple attribute returns as expected');
  lAttributes := GetXMLAttributes
    ('Condition="simple condition1" AnotherCondition="simple condition2"');
  checkistrue(length(lAttributes) = 2);
  checkisEqual(DPROJ_CONDITION_ATTRIBUTE, lAttributes[0].Name);
  checkisEqual('simple condition1', lAttributes[0].Value);
  checkisEqual('Another' + DPROJ_CONDITION_ATTRIBUTE, lAttributes[1].Name);
  checkisEqual('simple condition2', lAttributes[1].Value);

  newTest('Actual DPROJ Condition returns expected results');
  lAttributes := GetXMLAttributes
    ('Condition="`$(Config)`==`Base` or `$(Base)`!=``"');
  checkisEqual(DPROJ_CONDITION_ATTRIBUTE, lAttributes[0].Name);
  checkisEqual('`$(Config)`==`Base` or `$(Base)`!=``', lAttributes[0].Value);

  newTest('Actual DPROJ Condition with extra whitespace returns expected results');
  lAttributes := GetXMLAttributes
    ('  Condition = "`$(Config)`==`Base` or `$(Base)`!=``"'#13#10);
  checkisEqual(DPROJ_CONDITION_ATTRIBUTE, lAttributes[0].Name);
  checkisEqual('`$(Config)`==`Base` or `$(Base)`!=``', lAttributes[0].Value);

end;

procedure XMLChunker_Initialises_as_Expected;
var
  lChunker: IXMLChunker;
  lList: TStringlist;
  lFilePath: string;
begin
  lChunker := TXMLChunker.Create(RemoveBackTicks(TEST_DATA_DPROJ_2003));

  newTest('Content is visible');
  checkisEqual(RemoveBackTicks(TEST_DATA_DPROJ_2003), lChunker.Content);

  newTest('Position reads as expected');
  checkisEqual(1, lChunker.Position);

  newTest('Done is correct');
  checkisFalse(lChunker.Done);

  newTest('Path is empty');
  checkistrue(length(lChunker.Path) = 0);

  newTest('Load chuncker from file');
  lList := TStringlist.Create;
  lFilePath := GetEnvironmentVariable('TEMP') + 'xmlchunkertest.dproj';
  deleteFile(lFilePath);

  lList := TStringlist.Create;
  try
    lList.Text := TEST_DATA_DPROJ_2003;
    lList.SaveToFile(lFilePath);
    lChunker := TXMLChunker.CreateFromFile(lFilePath);
  finally
    freeandnil(lList);
    deleteFile(lFilePath);
  end;
  newTest('File Content is visible');
  checkisEqual(TEST_DATA_DPROJ_2003+#13#10, lChunker.Content);

end;

Procedure XMLChunker_NextNode_Works_as_Expected;
var
  lChunker: IXMLChunker;
  lNode: TXMLNode;
begin
  lChunker := TXMLChunker.Create(RemoveBackTicks(TEST_DATA_DPROJ_2003));

  newtest('Get First Node');
  lNode := lChunker.NextNode;

  newTest('Node Path');
  checkisEqual('Project',lNode.Path);
  newTest('Node Name');
  checkisEqual('Project',lNode.Name);
  newTest('Node Value');
  checkisEqual('', lNode.Value);
  newTest('Attribute Count');
  checkisEqual(1, length(lNode.Attributes));
  newTest('Attribute Name');
  checkisEqual('xmlns', lNode.Attributes[0].Name);
  newTest('Attribute Value');
  checkisEqual('http://schemas.microsoft.com/developer/msbuild/2003', lNode.Attributes[0].value);

end;

initialization

NewSet('Delphi DPROJ Parser');

PrepareSet(Prepare);
// Comments
AddTestCase('Test Strip White Space', StripWhiteSpace_Works_as_expected);
AddTestCase('Test Reset XML Node', ResetXMLNode_Works_as_expected);
AddTestCase('Test GetXMLAttributes', GetXMLAttributes_Works_as_Expected);
AddTestCase('Test XML Chunker Class', XMLChunker_Initialises_as_Expected);
AddTestCase('Test XML CHunker Next Node', XMLChunker_NextNode_Works_as_Expected);

FinaliseSet(Finalise);

finalization

end.