unit TestDelphiDProj;

interface

uses
  SysUtils,
  minitestframework,
  XMLNodeReader,
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
    '      <Base>true</Base>' + '      <Empty/>' + '      <Empty2 />' +
    '  </PropertyGroup>' +
    '  <ProjectExtensions>'+
    '    <Borland.Personality>Delphi.Personality.12</Borland.Personality>'+
    '    <Borland.ProjectType>Application</Borland.ProjectType>'+
    '    <BorlandProject>'+
    '        <Delphi.Personality>'+
    '            <Source>'+
    '                <Source Name="MainSource">TestRunnerTests.dpr</Source>'+
    '            </Source>'+
    '        </Delphi.Personality>'+
    '    </BorlandProject>'+
    '  </ProjectExtensions>'+
     '</Project>';

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

procedure XMLNodeReader_Initialises_as_Expected;
var
  lNodeReader: IXmlNodeReader;
  lList: TStringlist;
  lFilePath: string;
begin
  lNodeReader := TXmlNodeReader.Create(RemoveBackTicks(TEST_DATA_DPROJ_2003));

  newTest('Content is visible');
  checkisEqual(RemoveBackTicks(TEST_DATA_DPROJ_2003), lNodeReader.Content);

  newTest('Position reads as expected');
  checkisEqual(1, lNodeReader.Position);

  newTest('Done is correct');
  checkisFalse(lNodeReader.Done);

  newTest('Path is empty');
  checkistrue(length(lNodeReader.Path) = 0);

  newTest('Load chuncker from file');
  lList := TStringlist.Create;
  lFilePath := GetEnvironmentVariable('TEMP') + 'XMLNodeReadertest.dproj';
  deleteFile(lFilePath);

  lList := TStringlist.Create;
  try
    lList.Text := TEST_DATA_DPROJ_2003;
    lList.SaveToFile(lFilePath);
    lNodeReader := TXmlNodeReader.CreateFromFile(lFilePath);
  finally
    freeandnil(lList);
    deleteFile(lFilePath);
  end;
  newTest('File Content is visible');
  checkisEqual(TEST_DATA_DPROJ_2003 + #13#10, lNodeReader.Content);

end;

Procedure XMLNodeReader_NextNode_Works_as_Expected;
var
  lNodeReader: IXmlNodeReader;
  lNode: TXMLNode;
begin
  lNodeReader := TXmlNodeReader.Create(RemoveBackTicks(TEST_DATA_DPROJ_2003));

  newTest('Get First Node');
  lNode := lNodeReader.NextNode;
  // <Project ...
  newTest('Node Path');
  checkisEqual('/Project', lNode.Path);
  newTest('Node Name');
  checkisEqual('Project', lNode.Name);
  newTest('Node Value');
  checkisEqual('', lNode.Value);
  newTest('Attribute Count');
  checkisEqual(1, length(lNode.Attributes));
  newTest('Attribute Name');
  checkisEqual('xmlns', lNode.Attributes[0].Name);
  newTest('Attribute Value');
  checkisEqual('http://schemas.microsoft.com/developer/msbuild/2003',
    lNode.Attributes[0].Value);
  newTest('Updated Path');
  checkisEqual('/Project', lNodeReader.Path);

  // <PropertyGroup>
  lNode := lNodeReader.NextNode;
  newTest('Property Group returns empty with no attributes');
  checkisEqual('/Project/PropertyGroup', lNode.Path);
  checkisEqual('PropertyGroup', lNode.Name);
  checkisEqual('', lNode.Value);
  checkisEqual(0, length(lNode.Attributes));
  checkisEqual('/Project/PropertyGroup', lNodeReader.Path);

  // <ProjectGuid>
  lNode := lNodeReader.NextNode;
  newTest('Project Guid returns with a value but no attributes');
  checkisEqual('/Project/PropertyGroup/ProjectGuid', lNode.Path);
  checkisEqual('ProjectGuid', lNode.Name);
  checkisEqual('{E1C2E563-4870-44D1-9394-BAE6D5485E96}', lNode.Value);
  checkisEqual(0, length(lNode.Attributes));
  newTest('Project GUID closes - Path shrinks back by one node');
  checkisEqual('/Project/PropertyGroup', lNodeReader.Path);

  // <ProjectVersion>
  lNode := lNodeReader.NextNode;
  newTest('Project Guid returns with a value but no attributes');
  checkisEqual('/Project/PropertyGroup/ProjectVersion', lNode.Path);
  checkisEqual('ProjectVersion', lNode.Name);
  checkisEqual('16.1', lNode.Value);
  checkisEqual(0, length(lNode.Attributes));
  newTest('Project GUID closes - Path shrinks back by one node');
  checkisEqual('/Project/PropertyGroup', lNodeReader.Path);

  newTest('Iterate over nodes until we reach the end of this node');
  while (lNode.Name <> 'AppType') and (not lNodeReader.Done) do
    lNode := lNodeReader.NextNode;
  lNode := lNodeReader.NextNode;
  checkisEqual('/Project/PropertyGroup', lNode.Path);
  checkisEqual(1, length(lNode.Attributes));
  checkisEqual('PropertyGroup', lNode.Name);
  checkisEqual(DPROJ_CONDITION_ATTRIBUTE, lNode.Attributes[0].Name);
  checkisEqual(RemoveBackTicks('`$(Config)`==`Base` or `$(Base)`!=``'),
    lNode.Attributes[0].Value);

  newTest('Handle Self closed nodes in the form <xyz/>');
  lNode := lNodeReader.NextNode; // Base
  lNode := lNodeReader.NextNode;
  checkisEqual('Empty', lNode.Name);
  checkisEqual(0, length(lNode.Attributes));
  checkisEqual('', lNode.Value);

  newTest('Handle Self closed nodes in the form <xyz />');
  lNode := lNodeReader.NextNode;
  checkisEqual('Empty2', lNode.Name);
  checkisEqual(0, length(lNode.Attributes));
  checkisEqual('', lNode.Value);

end;

Procedure Simple_Conditions_Parse_Correctly;
var
  lConditions: TProjectConditions;
begin
  newTest('Empty Condition returns empty set');
  lConditions := ParseCondition('');
  checkisEqual(0, length(lConditions));

  newTest('Single Condition');
  lConditions := ParseCondition(RemoveBackTicks('`$(Config)`==`Base`'));
  checkisEqual(1, length(lConditions));
  checkisEqual('$(Config)', lConditions[0].FirstArgument);
  checkisEqual(coEquals, lConditions[0].ConditionalOperator);
  checkisEqual('Base', lConditions[0].SecondArgument);
  checkisEqual('0.0', lConditions[0].Group);

  newTest('Two conditions OR at the same level');
  lConditions := ParseCondition
    (RemoveBackTicks('`$(Config)`==`Base` or `$(Base)`!=``'));
  checkisEqual(3, length(lConditions));
  checkisEqual('$(Config)', lConditions[0].FirstArgument);
  checkisEqual(coEquals, lConditions[0].ConditionalOperator);
  checkisEqual('Base', lConditions[0].SecondArgument);
  checkisEqual('0.0', lConditions[0].Group);

  checkisEqual('0.0', lConditions[1].FirstArgument);
  checkisEqual(coOr, lConditions[1].ConditionalOperator);
  checkisEqual('', lConditions[1].SecondArgument);
  checkisEqual('0.0', lConditions[1].Group);

  checkisEqual('$(Base)', lConditions[2].FirstArgument);
  checkisEqual(coNotEquals, lConditions[2].ConditionalOperator);
  checkisEqual('', lConditions[2].SecondArgument);
  checkisEqual('0.0', lConditions[2].Group);

  newTest('3 conditions First 2 and and Or''ed with last');
  lConditions := ParseCondition
    (RemoveBackTicks
    ('(`$(Platform)`==`Win32` and `$(Base)`==`true`) or `$(Base_Win32)`!=``'));
  checkisEqual(5, length(lConditions));
  newTest('Expression 0 is platform');
  checkisEqual('$(Platform)', lConditions[0].FirstArgument);
  checkisEqual(coEquals, lConditions[0].ConditionalOperator);
  checkisEqual('Win32', lConditions[0].SecondArgument);
  checkisEqual('1.1', lConditions[0].Group);

  newTest('Expression 1 is And');
  checkisEqual('1.1', lConditions[1].FirstArgument);
  checkisEqual(coAnd, lConditions[1].ConditionalOperator);
  checkisEqual('', lConditions[1].SecondArgument);
  checkisEqual('1.1', lConditions[1].Group);

  newTest('Expression 2 is Base');
  checkisEqual('$(Base)', lConditions[2].FirstArgument);
  checkisEqual(coEquals, lConditions[2].ConditionalOperator);
  checkisEqual('true', lConditions[2].SecondArgument);
  checkisEqual('1.1', lConditions[2].Group);

  newTest('Expression 3 is OR');
  checkisEqual('.1', lConditions[3].FirstArgument);
  checkisEqual(coOr, lConditions[3].ConditionalOperator);
  checkisEqual('', lConditions[3].SecondArgument);
  checkisEqual('2.0', lConditions[3].Group);

  newTest('Expression 4 is Base_Win32');
  checkisEqual('$(Base_Win32)', lConditions[4].FirstArgument);
  checkisEqual(coNotEquals, lConditions[4].ConditionalOperator);
  checkisEqual('', lConditions[4].SecondArgument);
  checkisEqual('2.0', lConditions[4].Group);

end;

Procedure PathDepth_Works_As_Expected;
begin
  newTest('Empty string returns -1');
  checkisEqual(-1, PathDepth(''));

  newTest('Non empty string without / returns -1');
  checkisEqual(-1, PathDepth('string without a slash in it'));

  newTest('Single / returns 0');
  checkisEqual(0, PathDepth('/'));

  newTest('Single / with text returns 1');
  checkisEqual(1, PathDepth('/Text'));

  newTest('Text with 2 / and no extra text returns 1');
  checkisEqual(1, PathDepth('/Text/'));

  newTest('Text with 2 / and trailing text returns 2');
  checkisEqual(2, PathDepth('/Text/Text2'));

  newTest('Text with 3 / and no extra text returns 2');
  checkisEqual(2, PathDepth('/Text/Text2/'));

  newTest('Text with 3 / and trailing text returns 3');
  checkisEqual(3, PathDepth('/Text/Text2/Text3'));

  newTest('Text with 7 / and trailing text returns 7');
  checkisEqual(7, PathDepth('/Text/Text2/Text3/Text4/Text5/Text6/Text7'));

end;

Procedure ConditionsAreMet_Works_Crrectly;
begin
  notImplemented;
end;

Procedure Properties_are_set_as_expected;
var
  lProject: IDelphiProjectProperties;
begin
  lProject := TDelphiProjectProperties.Create;

  lProject.ExtractProperties(TEST_DATA_DPROJ_2003);
  newTest('Properties have been extract');
  CheckIsTrue(lProject.Properties.Count>0);

  newTest('Unconditional Properties are populated');
  checkisEqual('{E1C2E563-4870-44D1-9394-BAE6D5485E96}',
    lProject.properties.Values['ProjectGuid']);
  checkisEqual('16.1', lProject.properties.Values['ProjectVersion']);
  checkisEqual('None', lProject.properties.Values['FrameworkType']);
  checkisEqual('TestRunnerTests.dpr', lProject.properties.Values['MainSource']);
  checkisEqual('True', lProject.properties.Values['Base']);
  checkisEqual('3', lProject.properties.Values['TargetedPlatforms']);
  checkisEqual('Console', lProject.properties.Values['AppType']);
  checkisEqual('Delphi.Personality.12',lProject.properties.Values['Borland.Personality']);
  checkisEqual('Application',lProject.properties.Values['Borland.ProjectType']);
  checkisEqual('TestRunnerTests.dpr',lProject.properties.Values['Project.ProjectExtensions.BorlandProject.Delphi.Personality.Source.Source']);

 // print(lProject.Properties.Text);

  newTest('Conditional Properties are populated');
  checkisEqual('Debug', lProject.properties.Values['Config']);
  checkisEqual('Win32', lProject.properties.Values['Platform']);

end;

initialization

NewSet('XML Node Reader');
PrepareSet(Nil);
// Comments
AddTestCase('Test Strip White Space', StripWhiteSpace_Works_as_expected);
AddTestCase('Test Reset XML Node', ResetXMLNode_Works_as_expected);
AddTestCase('Test GetXMLAttributes', GetXMLAttributes_Works_as_Expected);
AddTestCase('Test XML Chunker Class', XMLNodeReader_Initialises_as_Expected);
AddTestCase('Test PathDepth', PathDepth_Works_As_Expected);
AddTestCase('Test XML Chunker Next Node',
  XMLNodeReader_NextNode_Works_as_Expected);
FinaliseSet(Nil);

NewSet('MSBuild Condition Logic');
PrepareSet(Prepare);
AddTestCase('Test Parse Simple Conditions', Simple_Conditions_Parse_Correctly);
AddTestCase('Test Conditions are Met', ConditionsAreMet_Works_Crrectly);
AddTestCase('Test Properties are populated', Properties_are_set_as_expected);
FinaliseSet(Finalise);

finalization

end.
