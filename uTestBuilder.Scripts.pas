unit uTestBuilder.Scripts;

interface

uses SysUtils, Classes;

const
  DEFAULT_DUNITM_BUILD_PREFIX = ':: PROJECT: <PROJECTNAME>'#13#10 +
    '@cd "<PROJECTDIR>"'#13#10 +
    '@ECHO ------------------------------------------------------------------'#13#10
    + '@ECHO PROJECT <PROJECTNAME> Building with <DELPHISHORTNAME>...'#13#10 +
    '@ECHO PROJECT <PROJECTNAME> Building with <DELPHISHORTNAME>...>>%TEST_RESULTS%'#13#10
    + 'IF NOT EXIST "<OUTPUTPATH>" (mkdir "<OUTPUTPATH>")'#13#10 +
    'IF NOT EXIST "<DCUOUTPUTPATH>" (mkdir "<DCUOUTPUTPATH>")'#13#10;

  DEFAULT_DUNITM_BUILD_COMMAND = '"<DCCPATH>\<DCCEXE>" ' +
    '-$O- -$W+ -$D- -$C+ ' + '<SWITCH_NOCONFIG> -B -Q -TX.exe ' +
    '-A<UNITALIASES> ' + '-D<CONDITIONALDEFINES> ' + '-E"<OUTPUTPATH>" ' +
    '-I<INCLUDESEARCHPATH> ' + '<SWITCH_DCUPATH>"<DCUOUTPUTPATH>" ' +
    '-NS<NAMESPACES> ' + '-O<OBJECTSEARCHPATH> ' + '-R<RESOURCESEARCHPATH> ' +
    '-U<UNITSEARCHPATH> ' + '-CC -VN -W- -H-' +
    '<CRITICALFLAGS> <PROJECTNAME> >>%TEST_RESULTS%'#13#10;

  DEFAULT_DUNITM_BUILD_SUFFIX = '@if %ERRORLEVEL% EQU 0 ('#13#10 +
    ' @ECHO BUILD OK.'#13#10 + ' @ECHO BUILD OK.>>%TEST_RESULTS%'#13#10 +
    ' @"<EXEOUTPUTPATH><PROJECTNAME>.exe" >> %TEST_RESULTS%'#13#10 +
    ' @if %ERRORLEVEL% EQU 0 (' +
    '  @ECHO ALL <PROJECTNAME> TESTS PASSED '#13#10 +
    '  @ECHO ALL <PROJECTNAME> TESTS PASSED>>%TEST_RESULTS%'#13#10 +
    ' ) else ('#13#10 + '  SET EXIT_CODE=2'#13#10 +
    '  @ECHO Tests for <PROJECTNAME> FAILED! '#13#10 +
    '  @ECHO Tests for <PROJECTNAME> FAILED!>>%TEST_RESULTS%'#13#10 + ')'#13#10
    + ') ELSE ('#13#10 + ' @ECHO BUILD FAILED!'#13#10 +
    ' @ECHO BUILD FAILED!>>%TEST_RESULTS%'#13#10 + ' SET EXIT_CODE=1'#13#10 +
    ')'#13#10;

  DEFAULT_DUNITM_SCRIPT_COMMAND = DEFAULT_DUNITM_BUILD_PREFIX +
    DEFAULT_DUNITM_BUILD_COMMAND + DEFAULT_DUNITM_BUILD_SUFFIX;

  DUNITM_TEST_RUNNER_SCRIPT = ':: -- DUNIT M - TEST SCRIPT RUNNER --'#13#10 +
    ':: SET ENVIRONMENT '#13#10 + '@Echo off'#13#10 + 'SET EXIT_CODE=0'#13#10 +
    'SET ROOT=%cd%'#13#10 +
    'SET TEST_RESULTS=<ROOTPATH>TestRunnerResults.txt'#13#10 +
    '@echo ==== DUNITm TEST RUN Time: %DATE:~-4%-%DATE:~4,2%-%DATE:~7,2% ==== >%TEST_RESULTS%'#13#10
    + ':: EXECUTE TESTS '#13#10 + '<TESTEXECUTION>'#13#10 +
    '@ECHO SUMMARY:>>%TEST_RESULTS%'#13#10 + 'IF %EXIT_CODE% EQU 2 ('#13#10 +
    ' @echo ONE OR MORE TEST CASES FAILED>>%TEST_RESULTS%'#13#10 + ')'#13#10 +
    '@ECHO =================================================================='#13#10
    + 'IF %EXIT_CODE% EQU 1 ('#13#10 +
    ' @echo ONE OR MORE BUILDS FAILED>>%TEST_RESULTS%'#13#10 + ')'#13#10 +
    'IF %EXIT_CODE% EQU 0 ('#13#10 +
    ' @echo ALL TESTS PASSED>>%TEST_RESULTS%'#13#10 + ')'#13#10 +
    'IF [%1] NEQ [] ('#13#10 + '  @echo Opening in %1'#13#10 +
    '  %1 %TEST_RESULTS%'#13#10 + ')'#13#10 + 'cd %ROOT%'#13#10 +
    '@exit /b %EXIT_CODE%';

  REG_ROOT_DIR = 'RootDir';
  REG_LIBRARY = '\Library';
  REG_VALUE_SEARCHPATH = 'Search Path';
  REG_WIN32 = '\Win32';
  REG_WIN64 = '\Win64';

  PLATFORM_ANYCPU = 'AnyCPU';
  PLATFORM_WIN32 = 'Win32';
  PLATFORM_WIN64 = 'Win64';
  PLATFORM_OSX32 = 'OSX32';

  DELPHI_BIN_PATH = '\bin';
  DELPHI_LIB_PATH = '\lib';
  DELPHI_ENV_OVERRIDES = 'Environment Variables';
  DELPHI_COMPILER_WIN32 = 'dcc32.exe';
  DELPHI_COMPILER_WIN64 = 'dcc64.exe';
  DELPHI_COMPILER_OSX32 = '?';

Type
  TDUnitMBuildData = Record
    Version: string;
    DelphiRootPath: string;
    DelphiBinPath: string;
    CommonPath: string;
    DCCPath: string;
    DCCExe: string;
    DCCLib: string;
    UnitAliases: string;
    ConditionalDefines: string;
    OutputPath: string;
    DCUOutputPath: string;
    IncludeSearchPath: string;
    ObjectSearchPath: string;
    ResourceSearchPath: string;
    UnitSearchPath: string;
    ProjectName: string;
    ProjectDir: string;
    Namespaces: string;
    TargetPlatform: string;
    CriticalFlags: string;
  End;

Function Commandline(AProjectPath: string; ADelphiVersion: string;
  ABuildText: string = ''): string;

implementation

uses Registry, Delphi.Versions, Delphi.DProj, windows;

function PathToDir(APath: string): string;
begin
  result := APath;
  if copy(result, length(result), 1) = '\' then
    result := copy(result, 1, length(result) - 1);
end;

function RemoveDuplicatePathbackslash(APath: string): string;
begin
  result := APath;
  while pos('\\', copy(result, 3, MaxInt)) > 0 do
    result := copy(result, 1, 2) + StringReplace(copy(result, 3, MaxInt), '\\',
      '\', [rfReplaceAll]);

end;

function QuoteDirectories(ADirectories: string): string;
var
  lList: TStringlist;
  i: integer;
  lValue: string;
  lDuplicateIndex: integer;
begin
  lList := TStringlist.Create;
  try
    lList.Text := StringReplace(ADirectories, ';', #13#10, [rfReplaceAll]);
    for i := lList.Count - 1 downto 0 do
    begin
      lValue := lList[i];
      lDuplicateIndex := lList.IndexOf(lValue);
      if lDuplicateIndex < i then
      begin
        lList.Delete(i);
        continue;
      end;
      if length(lValue) = 0 then
      begin
        lList.Delete(i);
      end
      else
      begin
        lValue := RemoveDuplicatePathbackslash(Trim(lValue));
        if (pos('"', lValue) = 0) and (pos(' ', lValue) > 0) then
          lValue := format('"%s"', [lValue]);
        lList[i] := lValue;
      end;
    end;
    result := StringReplace(lList.Text, #13#10, ';', [rfReplaceAll]);
    result := copy(result, 1, length(result) - 1); // remove the trailing ;
  finally
    freeandnil(lList);
  end;

end;

function CompilerFromPlatform(APlatform: string): string;
begin
  result := '';
  if APlatform = PLATFORM_WIN32 then
    result := DELPHI_COMPILER_WIN32
  else if APlatform = PLATFORM_WIN64 then
    result := DELPHI_COMPILER_WIN64
  else if APlatform = PLATFORM_OSX32 then
    result := DELPHI_COMPILER_OSX32;
end;

function GetFinalCommandLine(ACommand: string; AProject: TDUnitMBuildData;
  AProperties: IDelphiProjectProperties): string;
begin
  result := ACommand;

  // Check for critical folders

  if length(AProject.OutputPath) = 0 then
    AProject.OutputPath := AProject.ProjectDir + '\TestBin';
  if length(AProject.DCUOutputPath) = 0 then
    AProject.DCUOutputPath := AProject.ProjectDir + '\TestDCU';

  result := StringReplace(result, '<PROJECTDIR>', AProject.ProjectDir,
    [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '<PROJECTNAME>',
    ChangeFileExt(AProject.ProjectName, ''), [rfReplaceAll, rfIgnoreCase]);

  result := StringReplace(result, '<DELPHISHORTNAME>',
    AProperties.Properties.values[ENV_DELPHI_SHORTNAME],
    [rfReplaceAll, rfIgnoreCase]);

  result := StringReplace(result, '<DELPHIPRODUCTNAME>',
    AProperties.Properties.values[ENV_DELPHI_PRODUCTNAME],
    [rfReplaceAll, rfIgnoreCase]);

  result := StringReplace(result, '<OUTPUTPATH>', AProject.OutputPath,
    [rfReplaceAll, rfIgnoreCase]);

  result := StringReplace(result, '<DCUOUTPUTPATH>', AProject.DCUOutputPath,
    [rfReplaceAll, rfIgnoreCase]);

  result := StringReplace(result, '<DCCPATH>', AProject.DCCPath,
    [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '<DCCEXE>', AProject.DCCExe,
    [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '<SWITCH_NOCONFIG>',
    AProperties.Properties.values['SWITCH_NOCONFIG'],
    [rfReplaceAll, rfIgnoreCase]);

  if length(AProject.UnitAliases) = 0 then
    result := StringReplace(result, '-A<UNITALIASES> ', '', [rfIgnoreCase])
  else
    result := StringReplace(result, '<UNITALIASES>', AProject.UnitAliases,
      [rfIgnoreCase]);

  if length(AProject.ConditionalDefines) = 0 then
    result := StringReplace(result, '-D<CONDITIONALDEFINES> ', '',
      [rfIgnoreCase])
  else
    result := StringReplace(result, '<CONDITIONALDEFINES>',
      AProject.ConditionalDefines, [rfIgnoreCase]);

  result := StringReplace(result, '<OUTPUTPATH>',
    QuoteDirectories(AProject.OutputPath), [rfIgnoreCase]);

  result := StringReplace(result, '<EXEOUTPUTPATH>',
    includeTrailingPathDelimiter(AProject.OutputPath),
    [rfIgnoreCase, rfReplaceAll]); // For scripting usually

  result := StringReplace(result, '<SWITCH_DCUPATH>',
    AProperties.Properties.values['SWITCH_DCUPATH'],
    [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '<DCUOUTPUTPATH>',
    QuoteDirectories(AProject.OutputPath), [rfIgnoreCase]);

  if not DirectoryExists(AProject.OutputPath) then
    ForceDirectories(AProject.OutputPath);
  if not DirectoryExists(AProject.DCUOutputPath) then
    ForceDirectories(AProject.DCUOutputPath);

  AProperties.Properties.values['Platform'] :=
    AProperties.Handlers.PlatformName;

  // Expand the Search Paths
  AProject.UnitSearchPath := AProperties.Handlers.ExpandDelphiEnvVariables
    (AppendPath(AProject.UnitSearchPath, AProperties.Properties.values
    [ENV_BDS_DCUPATH]));
  // Early Versions of BDS had the Platform set to ANYCPU by default (Hacky fix below)
  // TODO : fix hacky ANYCPU replacement
  AProject.UnitSearchPath := StringReplace(AProject.UnitSearchPath,
    PLATFORM_ANYCPU, AProject.TargetPlatform, [rfReplaceAll, rfIgnoreCase]);

  AProject.IncludeSearchPath := AProperties.Handlers.ExpandDelphiEnvVariables
    (AProject.IncludeSearchPath);
  AProject.ResourceSearchPath := AProperties.Handlers.ExpandDelphiEnvVariables
    (AProject.ResourceSearchPath);
  AProject.ObjectSearchPath := AProperties.Handlers.ExpandDelphiEnvVariables
    (AProject.ObjectSearchPath);

  result := StringReplace(result, '<UNITSEARCHPATH>',
    QuoteDirectories(AProject.UnitSearchPath), [rfIgnoreCase]);

  result := StringReplace(result, '<INCLUDESEARCHPATH>',
    QuoteDirectories(AppendPath(AProject.UnitSearchPath,
    AProject.IncludeSearchPath)), [rfIgnoreCase]);

  result := StringReplace(result, '<OBJECTSEARCHPATH>',
    QuoteDirectories(AppendPath(AProject.UnitSearchPath,
    AProject.ObjectSearchPath)), [rfIgnoreCase]);

  result := StringReplace(result, '<RESOURCESEARCHPATH>',
    QuoteDirectories(AppendPath(AProject.UnitSearchPath,
    AProject.ResourceSearchPath)), [rfIgnoreCase]);

  if length(AProject.Namespaces) = 0 then
    result := StringReplace(result, '-NS<NAMESPACES> ', '', [rfIgnoreCase])
  else
    result := StringReplace(result, '<NAMESPACES>', AProject.Namespaces,
      [rfIgnoreCase]);

  result := StringReplace(result, '<PROJECTNAME>', AProject.ProjectName,
    [rfIgnoreCase]);

  result := StringReplace(result, '<CRITICALFLAGS>', AProject.CriticalFlags,
    [rfIgnoreCase]);

  result := AProperties.Handlers.ExpandDelphiEnvVariables(result);
end;

Function PropertiesFromRegistry(AProject: TDUnitMBuildData;
  AVersion: TDelphiVersion; AProperties: IDelphiProjectProperties)
  : TDUnitMBuildData;
var
  lRegKey, lAltRegKey: String;
  lRegKeys: TStringlist;
  lCurrentKey: string;
  lReg, lEnvReg: TRegistry;
  lRoot: string;
begin
  result := AProject;
  lRegKeys := TStringlist.Create;
  try
    lRegKeys.Text := DelphiRegKeyByDelphiVersion(AVersion.DelphiVersion);
    lRegKey := lRegKeys[0];
    if lRegKeys.Count > 1 then
      lAltRegKey := lRegKeys[1];
  finally
    freeandnil(lRegKeys);
  end;

  if length(lRegKey) = 0 then
    raise Exception.Create('Registry Path not found for this version');
  lReg := TRegistry.Create;
  lEnvReg := TRegistry.Create;
  try
    lReg.RootKey := HKEY_CURRENT_USER;
    lEnvReg.RootKey := HKEY_CURRENT_USER;
    if not(lReg.OpenKeyReadOnly(lRegKey)) then
      if lReg.OpenKeyReadOnly(lAltRegKey) then
        lRegKey := lAltRegKey;
    if not lReg.ValueExists(REG_ROOT_DIR) then
      raise Exception.CreateFmt('Delphi %s not installed correctly',
        [AVersion.ProductName]);

    lEnvReg.OpenKeyReadOnly(lRegKey + DELPHI_ENV_OVERRIDES);

    // Root Folder for Delphi
    if lEnvReg.ValueExists(REG_BDS) then
      lRoot := lEnvReg.ReadString(REG_BDS);
    if length(lRoot) = 0 then
      lRoot := lReg.ReadString(REG_ROOT_DIR);
    lRoot := PathToDir(lRoot);

    result.DelphiRootPath := lRoot;
    result.DelphiBinPath := lRoot + DELPHI_BIN_PATH;
    result.DCCPath := result.DelphiBinPath;
    result.DCCLib := lRoot + DELPHI_LIB_PATH;
    lReg.CloseKey;

    lCurrentKey := lRegKey + REG_LIBRARY;
    if result.TargetPlatform = PLATFORM_WIN64 then
    begin
      lCurrentKey := lCurrentKey + REG_WIN64;
      result.DCCExe := CompilerFromPlatform(result.TargetPlatform);
    end
    else if AVersion.DelphiVersion > 12 then
      lCurrentKey := lCurrentKey + REG_WIN32;

    // Library Search Path
    lReg.OpenKeyReadOnly(lCurrentKey);
    if lReg.ValueExists(REG_VALUE_SEARCHPATH) then
    begin
      result.UnitSearchPath :=
        QuoteDirectories(AppendPath(result.UnitSearchPath,
        lReg.ReadString(REG_VALUE_SEARCHPATH)));
    end;

    AProperties.Properties.values[ENV_STUDIONAME] :=
      DelphiStudioByDelphiVersion(AVersion.DelphiVersion);
    AProperties.Properties.values[REG_BDS] := result.DelphiRootPath;
    AProperties.Properties.values[REG_BDSBIN] := result.DelphiBinPath;
    AProperties.Properties.values[REG_BDSINCLUDE] := result.IncludeSearchPath;
    AProperties.Properties.values[REG_BDSLIB] := result.DCCLib;

  finally
    freeandnil(lEnvReg);
    freeandnil(lReg);
  end;
end;

Function PropertiesFromDProj(AProject: TDUnitMBuildData;
  AVersion: TDelphiVersion; AProperties: IDelphiProjectProperties)
  : TDUnitMBuildData;
var
  lProperty: string;
begin
  result := AProject;
  AProperties.Properties.Text := AProperties.Properties.Text +
    GetTargetProperties(AProject.DelphiBinPath);

  AProperties.ExtractPropertiesFromDProj(AProject.ProjectDir + '\' +
    ChangeFileExt(AProject.ProjectName, '.dproj'));

  result.ConditionalDefines := AProperties.Properties.values['DCC_Define'];
  result.TargetPlatform := AProperties.Properties.values['Platform'];
  if sameText(result.TargetPlatform, PLATFORM_ANYCPU) then
  begin
    result.TargetPlatform := PLATFORM_WIN32;
    result.UnitSearchPath := StringReplace(result.UnitSearchPath,
      PLATFORM_ANYCPU, result.TargetPlatform, [rfReplaceAll, rfIgnoreCase]);
  end;
  result.UnitAliases := AProperties.Properties.values['DCC_UnitAlias'];
  if length(result.UnitAliases) = 0 then
    result.UnitAliases := AProperties.Properties.values
      [DPROJ_COMMON_UNITALIASES];
  result.DCUOutputPath := AProperties.Properties.values['DCC_DcuOutput'];
  result.OutputPath := AProperties.Properties.values['DCC_ExeOutput'];
  result.Namespaces := AProperties.Properties.values['DCC_Namespace'];

  // Search Path
  lProperty := Trim(AProject.UnitSearchPath);
  if (length(lProperty) > 0) and (copy(lProperty, length(lProperty), 1) <> ';')
  then
    lProperty := ';' + lProperty;
  result.UnitSearchPath := AProperties.Properties.values['DCC_UnitSearchPath'] +
    lProperty;

  result.IncludeSearchPath := result.UnitSearchPath + ';' +
    AProperties.Properties.values['BRCC_IncludePath'];
  result.ObjectSearchPath := result.UnitSearchPath;

end;

Procedure AssignCompilerFlag(var AProject: TDUnitMBuildData; AFlag: string;
  AValue: string);
begin
  if AFlag = 'A' then
    AProject.UnitAliases := AValue
  else if AFlag = 'E' then
    AProject.OutputPath := AValue
  else if (AFlag = 'N') then
  begin
    if copy(AValue, 1, 1) = '0' then
      AValue := copy(AValue, 2, MaxInt);
    AProject.DCUOutputPath := AValue
  end
  else if AFlag = 'U' then
    AProject.UnitSearchPath := AValue
  else if AFlag = 'I' then
    AProject.IncludeSearchPath := AValue
  else if AFlag = 'O' then
    AProject.ObjectSearchPath := AValue
  else if AFlag = 'R' then
    AProject.ResourceSearchPath := AValue
  else if AFlag = 'D' then
    AProject.ConditionalDefines := AValue
  else if AFlag = '$M' then
  begin
    if (AValue <> '-') and (AValue <> '+') then
      AProject.CriticalFlags := Trim(AProject.CriticalFlags + '-$M' + AValue);
  end
  else if ((copy(AFlag, 1, 1) = '$') and (pos(' ' + AFlag + ' ', ' $C $D') < 1))
  then
    AProject.CriticalFlags := Trim(AProject.CriticalFlags + format(' -%s%s',
      [AFlag, AValue]));
end;

Function PropertiesFromCFG(AProject: TDUnitMBuildData; AVersion: TDelphiVersion;
  AProperties: IDelphiProjectProperties): TDUnitMBuildData;
var
  lFile: TStringlist;
  i, fl: integer;
  lValue, lPrefix, lFlag: string;
begin
  result := AProject;
  lFile := TStringlist.Create;
  try
    lFile.LoadFromFile(includeTrailingPathDelimiter(AProject.ProjectDir) +
      ChangeFileExt(AProject.ProjectName, '.cfg'));
    for i := 0 to lFile.Count - 1 do
    begin
      lValue := lFile[i];
      if length(lValue) = 0 then
        continue;
      fl := 2;
      lPrefix := copy(lValue, 2, 1);
      if pos(lPrefix, '$LNW') > 0 then
        fl := 3;
      lFlag := copy(lValue, 1, fl);
      lValue := copy(lValue, fl + 1, MaxInt);
      AssignCompilerFlag(result, lFlag, lValue);
    end;
  finally
    freeandnil(lFile);
  end;
end;

Function PropertiesFromDOF(AProject: TDUnitMBuildData; AVersion: TDelphiVersion;
  AProperties: IDelphiProjectProperties): TDUnitMBuildData;
var
  lFile: TStringlist;
  i, p, r: integer;
  lF: boolean;
  lValue, lPrefix, lFlag: string;
  lSection: string;

  function SetSection(AValue: string): boolean;
  begin
    result := false;
    if (pos('[', AValue) > 0) and (pos(']', AValue) < 0) then
    begin
      result := true;
      lSection := AValue;
    end;
  end;

begin
  result := AProject;
  lFile := TStringlist.Create;
  try
    lFile.LoadFromFile(includeTrailingPathDelimiter(AProject.ProjectDir) +
      ChangeFileExt(AProject.ProjectName, '.dof'));

    // Directories and Conditionals
    result.UnitAliases := lFile.values['UnitAliases'];
    result.UnitSearchPath := lFile.values['SearchPath'];
    result.OutputPath := lFile.values['OutputDir'];
    result.ConditionalDefines := lFile.values['Conditionals'];

    // Flags
    lFlag := '-K';
    lValue := lFile.values['ImageBase'];
    AssignCompilerFlag(result, lFlag, lValue);

    lFlag := '-$M';
    lValue := format('%s;%s', [lFile.values['MinStackSize'],
      lFile.values['MaxStackSize']]);
    AssignCompilerFlag(result, lFlag, lValue);

    r := lFile.IndexOf('[Compiler]');
    if r = -1 then
      exit;
    for i := 0 to lFile.Count - 1 do
    begin
      if SetSection(lValue) then
        continue;
      if not sameText(lSection, '[Compiler]') then
        exit;
      // set the flags
      lValue := lFile[i];
      if length(lValue) = 0 then
        continue;
      p := pos('=', lValue);
      if p = 2 then
      begin
        lFlag := copy(lValue, 1, 1);
        lValue := copy(lValue, 3, 1);
        case lFlag[1] of
          'A', 'Z':
            ;
        else
          if copy(lValue, 3, 1) = '0' then
            lValue := '-'
          else
            lValue := '+';
        end;
        AssignCompilerFlag(result, '-$' + lFlag, lValue);
      end;
    end;
  finally
    freeandnil(lFile);
  end;
end;

Function PropertiesFromProject(AProject: TDUnitMBuildData;
  AVersion: TDelphiVersion; AProperties: IDelphiProjectProperties)
  : TDUnitMBuildData;
begin
  if AVersion.DelphiVersion < 9 then
  begin
    if fileExists(format('%s\%s', [AProject.ProjectDir,
      ChangeFileExt(AProject.ProjectName, '.dof')])) then
      result := PropertiesFromDOF(AProject, AVersion, AProperties)
    else if fileExists(format('%s\%s', [AProject.ProjectDir,
      ChangeFileExt(AProject.ProjectName, '.cfg')])) then
      result := PropertiesFromCFG(AProject, AVersion, AProperties);
  end
  else
    result := PropertiesFromDProj(AProject, AVersion, AProperties);
end;

Function GetProjectProperties(AProject: TDUnitMBuildData;
  AVersion: TDelphiVersion; AProperties: IDelphiProjectProperties = nil)
  : TDUnitMBuildData;
begin
  if AProperties = nil then
    AProperties := TDelphiProjectProperties.Create;
  result := PropertiesFromRegistry(AProject, AVersion, AProperties);
  result := PropertiesFromProject(result, AVersion, AProperties);
end;

Function Commandline(AProjectPath: string; ADelphiVersion: string;
  ABuildText: string = ''): string;
var
  lProject: TDUnitMBuildData;
  lDelphi: TDelphiVersion;
  lProperties: IDelphiProjectProperties;
  lBuildText: string;
begin
  result := '';

  DelphiVersionInit(lDelphi);

  if (length(ABuildText) = 0) then
    lBuildText := DEFAULT_DUNITM_BUILD_COMMAND
  else
    lBuildText := ABuildText;

  lProperties := TDelphiProjectProperties.Create;
  lProject.ProjectDir := ExtractFileDir(AProjectPath);

  lProject.ProjectName := ChangeFileExt(extractFileName(AProjectPath), '.dpr');
  lProject.Version := ADelphiVersion;

  // Default values - these may get overridden;
  lProject.OutputPath := lProject.ProjectDir;
  lProject.DCUOutputPath := lProject.ProjectDir + '\DCU';
  lProject.TargetPlatform := PLATFORM_WIN32;
  lProject.DCCExe := DELPHI_COMPILER_WIN32;

  if length(ADelphiVersion) = 0 then
    lDelphi := ALL_DELPHI_VERSIONS[DelphiIndexByVersion(DefaultDelphiVersion)]
  else
    lDelphi := FindDelphi(ADelphiVersion);

  if lDelphi.DelphiVersion = -1 then
    raise Exception.CreateFmt('Delphi Version %s not found', [ADelphiVersion]);
  // Set Version Specific Properties
  lProperties.Properties.values[ENV_PRODUCTVERSION] :=
    DelphiProductVersion(lDelphi);
  lProperties.Properties.values[ENV_BDSCOMMONDIR] := DEFAULT_BDSCOMMON;
  lProperties.Properties.values[ENV_BDSUSERDIR] := DEFAULT_BDSUSERDIR;
  lProperties.Properties.values[ENV_BDSCOMPANY] :=
    DelphiCompanyByDelphiVersion(lDelphi.DelphiVersion);
  lProperties.Properties.values[ENV_DELPHI_SHORTNAME] := lDelphi.ShortName;
  lProperties.Properties.values[ENV_DELPHI_PRODUCTNAME] := lDelphi.ProductName;
  if lDelphi.DelphiVersion >= 15 then
    lProperties.Properties.values[ENV_BDS_DCUPATH] := DEFAULT_BDSDCUDIR_X64
  else
    lProperties.Properties.values[ENV_BDS_DCUPATH] := DEFAULT_BDSDCUDIR;

  lProperties.Properties.Text := lProperties.Properties.Text +
    VersionSpecificCompilerSwitches(lDelphi);

  // Set Up the properties from the Project File and Appropriate version registry
  lProject := GetProjectProperties(lProject, lDelphi, lProperties);
  result := GetFinalCommandLine(lBuildText, lProject, lProperties);
end;

end.
