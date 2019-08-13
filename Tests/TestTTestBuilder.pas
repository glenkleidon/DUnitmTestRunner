unit TestTTestBuilder;

interface

uses SysUtils, MiniTestFramework,
  uTestBuilder,
  uTestBuilder.ProjectInfo;

implementation

procedure Prepare;
begin
end;

procedure Finalise;
begin
end;

procedure Check_The_List_of_Test_Projects;
begin
  notImplemented;
end;


initialization

NewSet('Test Test Builder Class in TestTTestBuilder.pas');

PrepareSet(Prepare);

AddTestCase('Get the Test Project Files', Check_The_List_of_Test_Projects);

FinaliseSet(Finalise);


end.
