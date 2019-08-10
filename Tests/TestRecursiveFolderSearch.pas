unit TestRecursiveFolderSearch;

interface
 Uses SysUtils, RecursiveFolderSearch;

implementation
 uses MiniTestFramework, Classes;

procedure Prepare;
begin
end;

procedure Finalise;
begin
end;

Procedure Recursively_Check_For_DPR_Files;
var
  lStartDir: string;
  lResultList: TStringlist;
  lExeProject: string;
begin
  lStartDir := ExpandFileName(ExtractFileDir(Paramstr(0)) + '\..\..\..\');

  lResultList := TStringlist.create;
  try
    lResultList.Text:= SearchFolderForFiles('*.dpr',lStartDir);
    NewTest('Correct Number of Folder entries returned');
    CheckIsEqual(3,lresultList.count);

    NewTest('This Test EXE is in the list');
    lExeProject := lowercase(ChangeFileExt(ExtractFileName(Paramstr(0)),'.dpr'));
    checkisTrue(pos(lExeProject,lowercase(lResultList.Text))>0);

    NewTest('The Test Builder project is in the list');
    lExeProject := 'testbuilder.dpr';
    checkisTrue(pos(lExeProject,lowercase(lResultList.Text))>0);

    NewTest('The Lexer project is in the list');
    lExeProject := 'testlexer.dpr';
    checkisTrue(pos(lExeProject,lowercase(lResultList.Text))>0);

  finally
   freeandnil(lResultList);
  end;
end;

initialization
    NewSet('Recursive Directory Search in TestRecursiveFolderSearch.pas');

    PrepareSet(Prepare);

    AddTestCase('Recursively Check for DPR Files',
      Recursively_Check_for_DPR_Files
    );

    FinaliseSet(Finalise);

end.
