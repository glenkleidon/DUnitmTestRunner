unit uTestBuilder.Scripts;

interface

uses SysUtils, Classes;

const
  DEFAULT_DUNITM_BUILD_PREFIX = ':: PROJECT- <PROJECTNAME>'#13#10 +
    '@cd <PROJECTDIR>'#13#10;

  DEFAULT_DUNITM_BUILD_COMMAND = '"<DCCPATH>\<DCCEXE>" ' +
    '-$O- -$W+ -$D- -$C+ ' + '--no-config -B -Q -TX.exe ' + '-A<UNITALIASES> ' +
    '-D<CONDITIONALDEFINES> ' + '-E<OUTPUTPATH> ' + '-I<INCLUDESEARCHPATH> ' +
    '-NU<DCUOUTPUTPATH> ' + '-NS<NAMESPACES> ' + '-O<OBJECTSEARCHPATH> ' +
    '-R<RESOURCESEARCHPATH> ' + '-U<UNITSEARCHPATH> ' + '-CC -VN -W- -H-' +
    '<CRITICALFLAGS> <PROJECTNAME>'#13#10;

  DEFAULT_DUNITM_BUILD_SUFFIX = '@if %ERRORLEVEL% EQU 0 ('#13#10 +
    ' @ECHO BUILD PASSED: <PROJECTNAME>'#13#10 +
    ' @"<EXEOUTPUTPATH><PROJECTNAME>.exe" >> %TEST_RESULTS%'#13#10 +
    ' @if %ERRORLEVEL% NEQ 0 ( SET EXIT_CODE=2 )'#13#10 + ') ELSE ('#13#10 +
    ' @ECHO BUILD FAILED: <PROJECTNAME>>>%TEST_RESULTS%'#13#10 +
    ' SET EXIT_CODE=1'#13#10 + ')'#13#10;

  DEFAULT_DUNITM_SCRIPT_COMMAND = DEFAULT_DUNITM_BUILD_PREFIX +
    DEFAULT_DUNITM_BUILD_COMMAND + DEFAULT_DUNITM_BUILD_SUFFIX;

  DUNITM_TEST_RUNNER_SCRIPT = ':: -- DUNIT M - TEST SCRIPT RUNNER --'#13#10 +
    ':: SET ENVIRONMENT '#13#10 + '@Echo off'#13#10 + 'SET EXIT_CODE=0'#13#10 +
    'SET TEST_RESULTS=<ROOTPATH>Test_results.txt'#13#10 +
    ':: EXECUTE TESTS '#13#10 + '<TESTEXECUTION>'#13#10 +
    '@ECHO SUMMARY:>>%TEST_RESULTS%'#13#10 + 'IF %EXIT_CODE% EQU 2 ('#13#10 +
    ' @echo ONE OR MORE TEST CASES FAILED>>%TEST_RESULTS%'#13#10 + ')'#13#10 +
    'IF %EXIT_CODE% EQU 1 ('#13#10 +
    ' @echo ONE OR MORE BUILDS FAILED>>%TEST_RESULTS%'#13#10 + ')'#13#10 +
    'IF %EXIT_CODE% EQU 0 ('#13#10 +
    ' @echo ALL TESTS PASSED>>%TEST_RESULTS%'#13#10 + ')'#13#10 +
    'cd %ROOT%'#13#10 + '@exit /b %EXIT_CODE%';

  REG_ROOT_DIR = 'RootDir';
  REG_LIBRARY = '\Library';
  REG_VALUE_SEARCHPATH = 'Search Path';
  REG_WIN32 = '\Win32';
  REG_WIN64 = '\Win64';

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

  {
    Compiler switches: -$<letter><state> (defaults are shown below)
    A8  Aligned record fields
    B-  Full boolean Evaluation
    C+  Evaluate assertions at runtime
    D+  Debug information
    G+  Use imported data references
    H+  Use long strings by default
    I+  I/O checking
    J-  Writeable structured consts
    L+  Local debug symbols
    M-  Runtime type info
    O+  Optimization
    P+  Open string params
    Q-  Integer overflow checking
    R-  Range checking
    T-  Typed @ operator
    U-  Pentium(tm)-safe divide
    V+  Strict var-strings
    W-  Generate stack frames
    X+  Extended syntax
    Y+  Symbol reference info
    Z1  Minimum size of enum types

  }
  (*

    -A<unit>=<alias> = Set unit alias
    -B = Build all units
    -CC = Console target
    -CG = GUI target
    -D<syms> = Define conditionals
    -E<path> = EXE/DLL output directory
    -F<offset> = Find error
    -GD = Detailed map file
    -GP = Map file with publics
    -GS = Map file with segments
    -H = Output hint messages
    -I<paths> = Include directories
    -J = Generate .obj file
    -JPHNE = Generate C++ .obj file, .hpp file, in namespace, expo
    -JL = Generate package .lib, .bpi, and all .hpp files for C++
    -K<addr> = Set image base addr
    -LE<path> = package .bpl output directory
    -LN<path> = package .dcp output directory
    -LU<package> = Use package
    -M = Make modified units
    -NU<path> = unit .dcu output directory
    -NH<path> = unit .hpp output directory
    -NO<path> = unit .obj output directory
    -NB<path> = unit .bpi output directory
    -NX<path> = unit .xml output directory
    -NS<namespaces> = Namespace search path
    -O<paths> = Object directories
    -P = look for 8.3 file names also
    -Q = Quiet compile
    -R<paths> = Resource directories
    -TX<ext> = Output name extension
    -U<paths> = Unit directories
    -V = Debug information in EXE
    -VR = Generate remote debug (RSM)
    -VT = Debug information in TDS
    -VN = TDS symbols in namespace
    -W[+|-|^][warn_id] = Output warning messages
    -Z = Output 'never build' DCPs
    -$<dir> = Compiler directive
    --help = Show this help screen
    --version = Show name and version
    --codepage:<cp> = specify source file encoding
    --default-namespace:<namespace> = set namespace
    --depends = output unit dependency information
    --doc = output XML documentation
    --drc = output resource string .drc file
    --no-config = do not load default dcc32.cfg file
    --description:<string> = set executable description
    --inline:{on|off|auto} = function inlining control
    --legacy-ifend = allow legacy $IFEND directive
    --zero-based-strings[+|-] = strings are indexed starting at 0
    --peflags:<flags> = set extra PE Header flags field
    --peoptflags:<flags> = set extra PE Header optional flags fiel
    --peosversion:<major>.<minor> = set OS Version fields in PE He
    --pesubsysversion:<major>.<minor> = set Subsystem Version fiel
    --peuserversion:<major>.<minor> = set User Version fields in P


  *)
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
  result := StringReplace(result, '<DCCPATH>', AProject.DCCPath,
    [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '<DCCEXE>', AProject.DCCExe,
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

  result := StringReplace(result, '<DCUOUTPUTPATH>',
    QuoteDirectories(AProject.OutputPath), [rfIgnoreCase]);

  if not DirectoryExists(AProject.OutputPath) then
    ForceDirectories(AProject.OutputPath);
  if not DirectoryExists(AProject.DCUOutputPath) then
    ForceDirectories(AProject.DCUOutputPath);

  // Expand the Search Paths
  AProject.UnitSearchPath := AProperties.Handlers.ExpandDelphiEnvVariables
    (AppendPath(AProject.UnitSearchPath, DEFAULT_BDSDCUDIR));
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

  result := AProperties.Handlers.ExpandDelphiEnvVariables(result);
end;

Function PropertiesFromRegistry(AProject: TDUnitMBuildData;
  AVersion: TDelphiVersion; AProperties: IDelphiProjectProperties)
  : TDUnitMBuildData;
var
  lRegKey: String;
  lCurrentKey: string;
  lReg, lEnvReg: TRegistry;
  lRoot: string;
begin
  result := AProject;
  lRegKey := DelphiRegKeyByDelphiVersion(AVersion.DelphiVersion);
  if length(lRegKey) = 0 then
    raise Exception.Create('Registry Path not found for this version');
  lReg := TRegistry.Create;
  lEnvReg := TRegistry.Create;
  try
    lReg.RootKey := HKEY_CURRENT_USER;
    lEnvReg.RootKey := HKEY_CURRENT_USER;
    lReg.OpenKeyReadOnly(lRegKey);
    lEnvReg.OpenKeyReadOnly(lRegKey + DELPHI_ENV_OVERRIDES);
    if not lReg.ValueExists(REG_ROOT_DIR) then
      raise Exception.CreateFmt('Delphi %s not installed correctly',
        [AVersion.ProductName]);

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

    AProperties.Properties.Values[ENV_STUDIONAME] :=
      DelphiStudioByDelphiVersion(AVersion.DelphiVersion);
    AProperties.Properties.Values[REG_BDS] := result.DelphiRootPath;
    AProperties.Properties.Values[REG_BDSBIN] := result.DelphiBinPath;
    AProperties.Properties.Values[REG_BDSINCLUDE] := result.IncludeSearchPath;
    AProperties.Properties.Values[REG_BDSLIB] := result.DCCLib;

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
  AProperties.ExtractPropertiesFromDProj(AProject.ProjectDir + '\' +
    ChangeFileExt(AProject.ProjectName, '.dproj'));

  result.ConditionalDefines := AProperties.Properties.Values['DCC_Define'];
  result.TargetPlatform := AProperties.Properties.Values['Platform'];
  result.UnitAliases := AProperties.Properties.Values['DCC_UnitAlias'];
  result.DCUOutputPath := AProperties.Properties.Values['DCC_DcuOutput'];
  result.OutputPath := AProperties.Properties.Values['DCC_ExeOutput'];
  result.Namespaces := AProperties.Properties.Values['DCC_Namespace'];

  // Search Path
  lProperty := Trim(AProject.UnitSearchPath);
  if (length(lProperty) > 0) and (copy(lProperty, length(lProperty), 1) <> ';')
  then
    lProperty := ';' + lProperty;
  result.UnitSearchPath := AProperties.Properties.Values['DCC_UnitSearchPath'] +
    lProperty;

  result.IncludeSearchPath := result.UnitSearchPath + ';' +
    AProperties.Properties.Values['BRCC_IncludePath'];
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
    Result.UnitAliases := lFile.Values['UnitAliases'];
    Result.UnitSearchPath := lFile.Values['SearchPath'];
    result.OutputPath := lFile.Values['OutputDir'];
    result.ConditionalDefines := lFile.Values['Conditionals'];

    // Flags
    lFlag := '-K';
    lValue := lFile.Values['ImageBase'];
    AssignCompilerFlag(result, lFlag, lValue);

    lFlag := '-$M';
    lvalue := format('%s;%s',[lFile.Values['MinStackSize'],
        lFile.Values['MaxStackSize']]);
    AssignCompilerFlag(result, lFlag, lValue);

    r := lFile.IndexOf('[Compiler]');
    if r=-1 then exit;
    for i := 0 to lFile.Count - 1 do
    begin
      if SetSection(lValue) then continue;
      if not SameText(lSection, '[Compiler]') then exit;
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
        AssignCompilerFlag(result, '-$'+lFlag, lValue);
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
    raise Exception.CreateFmt('Delphi Version % not found', [ADelphiVersion]);
  // Set Version Specific Properties
  lProperties.Properties.Values[ENV_PRODUCTVERSION] :=
    DelphiProductVersion(lDelphi);
  lProperties.Properties.Values[ENV_BDSCOMMONDIR] := DEFAULT_BDSCOMMON;
  lProperties.Properties.Values[ENV_BDSUSERDIR] := DEFAULT_BDSUSERDIR;
  lProperties.Properties.Values[ENV_BDSCOMPANY] :=
    DelphiCompanyByDelphiVersion(lDelphi.DelphiVersion);

  // Set Up the properties from the Project File and Appropriate version registry
  lProject := GetProjectProperties(lProject, lDelphi, lProperties);
  result := GetFinalCommandLine(lBuildText, lProject, lProperties);
end;

end.
