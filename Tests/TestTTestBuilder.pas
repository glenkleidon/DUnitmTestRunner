unit TestTTestBuilder;

interface

uses SysUtils, MiniTestFramework,
  uTestBuilder,
  uTestBuilder.ProjectInfo;

implementation

var
  CaseBuilder: TDUnitmTestBuilder;

procedure Prepare;
begin
  CaseBuilder := TDUnitmTestBuilder.create;
end;

procedure Finalise;
begin
  freeandnil(CaseBuilder);
end;

procedure Check_The_List_of_Test_Projects;
var
lExeProject: string;
begin
  CaseBuilder.RootFolder := ExpandFileName(ExtractFileDir(Paramstr(0)) +
    '\..\..\..\');

  NewTest('This Test EXE is in the list');
  lExeProject := lowercase(ChangeFileExt(ExtractFileName(Paramstr(0)), '.dpr'));
  checkisTrue(pos(lExeProject, lowercase(CaseBuilder.ProjectList)) > 0);

  NewTest('The Test Builder project is in the list');
  lExeProject := 'testbuilder.dpr';
  checkisTrue(pos(lExeProject, lowercase(CaseBuilder.ProjectList)) > 0);

  NewTest('The Lexer project is in the list');
  lExeProject := 'testlexer.dpr';
  checkisTrue(pos(lExeProject, lowercase(CaseBuilder.ProjectList)) > 0);


end;

initialization

NewSet('Test Test Builder Class in TestTTestBuilder.pas');

PrepareSet(Prepare);

AddTestCase('Get the Test Project Files', Check_The_List_of_Test_Projects);

FinaliseSet(Finalise);

end.
