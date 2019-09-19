unit uTestBuilder.Scripts;

interface

uses SysUtils, Classes;

const
  DEFAULT_DUNITM_BUILD_COMMAND = '<DCCPATH><DCCEXE> ' + '-$O- ' + '-$W+ ' +
    '--no-config ' + '-B ' + '-TX.exe ' + '-A<UNITALIASES> ' +
    '-D<CONDITIONALDEFINES> ' + '-E<OUTPUTPATH> ' + '-I<INCLUDESEARCHPATH> ' +
    '-NU<DCUOUTPUTPATH> ' + '-NS<NAMESPACES> ' + '-O<OBJECTSEARCHPATH> ' +
    '-R<RESOURCESEARCHPATH> ' + '-U<UNITSEARCHPATH> ' + '-CC ' + '-V ' + '-VN '
    + '<PROJECTNAME>';
  REG_ROOT_DIR = 'RootDir';
  REG_LIBRARY = '\Library';
  REG_VALUE_SEARCHPATH = 'Search Path';
  REG_WIN32 = '\Win32';
  REG_WIN64 = '\Win64';

  PLATFORM_WIN32 = 'Win32';
  PLATFORM_WIN64 = 'Win64';
  PLATFORM_OSX32 = 'OSX32';

  DELPHI_BIN_PATH = 'bin\';
  DELPHI_COMPILER_WIN32 = 'dcc32.exe';
  DELPHI_COMPILER_WIN64 = 'dcc64.exe';
  DELPHI_COMPILER_OSX32 = '?';

Type
  TDUnitMBuildData = Record
    DelphiBinPath: string;
    DCCPath: string;
    DCCExe: string;
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
Function Commandline(AProjectPath: string; ADelphiVersion: string): string;

implementation

uses Registry, Delphi.Versions, Delphi.DProj;

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

function GetFinalCommandLine(ACommand: string;
  AProject: TDUnitMBuildData): string;
begin
  result := ACommand;
  result := Stringreplace(result, '<DCCPATH>', AProject.DCCPath,
    [rfReplaceAll, rfIgnoreCase]);
  result := Stringreplace(result, '<DCCEXE>', AProject.DCCExe,
    [rfReplaceAll, rfIgnoreCase]);

  if length(AProject.UnitAliases) = 0 then
    result := Stringreplace(result, '-A<UNITALIASES> ', '', [rfIgnoreCase])
  else
    result := Stringreplace(result, '<UNITALIASES>', AProject.UnitAliases,
      [rfIgnoreCase]);

  if length(AProject.ConditionalDefines) = 0 then
    result := Stringreplace(result, '-D<CONDITIONALDEFINES> ', '',
      [rfIgnoreCase])
  else
    result := Stringreplace(result, '<CONDITIONALDEFINES>',
      AProject.ConditionalDefines, [rfIgnoreCase]);

  result := Stringreplace(result, '<OUTPUTPATH>', AProject.OutputPath,
    [rfIgnoreCase]);
  result := Stringreplace(result, '<DCUOUTPUTPATH>', AProject.OutputPath,
    [rfIgnoreCase]);
  if not DirectoryExists(AProject.OutputPath) then
    ForceDirectories(AProject.OutputPath);
  if not DirectoryExists(AProject.DCUOutputPath) then
    ForceDirectories(AProject.DCUOutputPath);

  result := Stringreplace(result, '<UNITSEARCHPATH>', AProject.UnitSearchPath
    + ';' + AProject.IncludeSearchPath, [rfIgnoreCase]);

  result := Stringreplace(result, '<INCLUDESEARCHPATH>', AProject.UnitSearchPath
    + ';' + AProject.IncludeSearchPath, [rfIgnoreCase]);

  result := Stringreplace(result, '<OBJECTSEARCHPATH>', AProject.UnitSearchPath
    + ';' + AProject.ObjectSearchPath, [rfIgnoreCase]);

  result := Stringreplace(result, '<RESOURCESEARCHPATH>',
    AProject.UnitSearchPath + ';' + AProject.ResourceSearchPath,
    [rfIgnoreCase]);

  if length(AProject.Namespaces) = 0 then
    result := Stringreplace(result, '-NS<NAMESPACES> ', '', [rfIgnoreCase])
  else
    result := Stringreplace(result, '<NAMESPACES>', AProject.Namespaces,
      [rfIgnoreCase]);

  result := Stringreplace(result, '<PROJECTNAME>', AProject.ProjectName,
    [rfIgnoreCase]);

end;

Function PropertiesFromRegistry(AProject: TDUnitMBuildData;
  AVersion: TDelphiVersion): TDUnitMBuildData;
var
  lRegKey: String;
  lCurrentKey: string;
  lReg: TRegistry;
begin
  result := AProject;
  lRegKey := DelphiRegKeyByDelphiVersion(AVersion.DelphiVersion);
  if lRegKey.length = 0 then
    raise Exception.Create('Registry Path not found for this version');
  lReg := TRegistry.Create;
  try
    lReg.OpenKeyReadOnly(lRegKey);
    if not lReg.ValueExists(REG_ROOT_DIR) then
      raise Exception.CreateFmt('Delphi %s not installed correctly',
        [AVersion.ProductName]);
    result.DelphiBinPath := lReg.ReadString(REG_ROOT_DIR);
    result.DCCPath := result.DelphiBinPath + DELPHI_BIN_PATH;
    lReg.CloseKey;

    // Library Search Path
    lCurrentKey := lRegKey + REG_LIBRARY;
    if result.TargetPlatform = PLATFORM_WIN64 then
    begin
      lCurrentKey := lCurrentKey + REG_WIN64;
      result.DCCExe := CompilerFromPlatform(result.TargetPlatform);
    end
    else if AVersion.DelphiVersion > 12 then
      lCurrentKey := lCurrentKey + REG_WIN32;

    lReg.OpenKeyReadOnly(lCurrentKey);
    if lReg.ValueExists(REG_VALUE_SEARCHPATH) then
      result.UnitSearchPath := lReg.ReadString(REG_VALUE_SEARCHPATH);
  finally
    freeandnil(lReg);
  end;
end;

Function PropertiesFromDProj(AProject: TDUnitMBuildData;
  AVersion: TDelphiVersion): TDUnitMBuildData;
var
  lProject: IDelphiProjectProperties;
  lProperty: string;
begin
  result := AProject;
  lProject := TDelphiProjectProperties.Create;
  lProject.ExtractPropertiesFromDProj(changefileExt(AProject.ProjectName,
    '.dproj'));

  result.ConditionalDefines := lProject.Properties.Values['DCC_Define'];
  result.TargetPlatform := lProject.Properties.Values['Platform'];
  result.UnitAliases := lProject.Properties.values['DCC_UnitAlias'];
  result.DCUOutputPath := lProject.Properties.Values['DCC_DcuOutput'];
  result.OutputPath := lProject.Properties.Values['DCC_ExeOutput'];
  result.Namespaces := lProject.Properties.Values['DCC_Namespace'];


  // Search Path
  lProperty := trim(AProject.UnitSearchPath);
  if (length(lProperty)>0) and (copy(lProperty,length(lProperty),1)<>';') then
    lProperty := lProperty + ';';
  result.UnitSearchPath := lProperty +
    lProject.Properties.Values['DCC_UnitSearchPath'];

  result.IncludeSearchPath := result.UnitSearchPath + ';' + lProject.Properties.Values['BRCC_IncludePath'];
  result.ObjectSearchPath := result.UnitSearchPath;




end;

Function PropertiesFromCFG(AProject: TDUnitMBuildData; AVersion: TDelphiVersion)
  : TDUnitMBuildData;
begin
  result := AProject;
end;

Function PropertiesFromDOF(AProject: TDUnitMBuildData; AVersion: TDelphiVersion)
  : TDUnitMBuildData;
begin
  result := AProject;
end;

Function PropertiesFromProject(AProject: TDUnitMBuildData;
  AVersion: TDelphiVersion): TDUnitMBuildData;
begin
  if AVersion.DelphiVersion < 9 then
  begin
    if fileExists(format('%s%s', [AProject.ProjectDir,
      changefileExt(AProject.ProjectName, '.dof')])) then
      result := PropertiesFromDOF(AProject, AVersion)
    else if fileExists(format('%s%s', [AProject.ProjectDir,
      changefileExt(AProject.ProjectName, '.cfg')])) then
      result := PropertiesFromCFG(AProject, AVersion);
  end
  else
    result := PropertiesFromDProj(AProject, AVersion);
end;

Function GetProjectProperties(AProject: TDUnitMBuildData;
  AVersion: TDelphiVersion): TDUnitMBuildData;
begin
  result := PropertiesFromProject(AProject, AVersion);
  result := PropertiesFromRegistry(Result, AVersion);
end;

Function Commandline(AProjectPath: string; ADelphiVersion: string): string;
var
  lProject: TDUnitMBuildData;
  lDelphi: TDelphiVersion;
begin
  result := '';
  lProject.ProjectDir := extractFilepath(AProjectPath);
  lProject.ProjectName := changefileExt(AProjectPath, '.dpr');

  // Default values - these may get overridden;
  lProject.OutputPath := lProject.ProjectDir;
  lProject.DCUOutputPath := lProject.ProjectDir + 'DCU\';
  lProject.TargetPlatform := PLATFORM_WIN32;
  lProject.DCCExe := DELPHI_COMPILER_WIN32;

  if length(ADelphiVersion) = 0 then
    lDelphi := ALL_DELPHI_VERSIONS[DelphiIndexByVersion(DefaultDelphiVersion)]
  else
    lDelphi := FindDelphi(ADelphiVersion);

  if lDelphi.DelphiVersion = -1 then
    raise Exception.CreateFmt('Delphi Version % not found', [ADelphiVersion]);
  lProject := GetProjectProperties(lProject, lDelphi);
  result := GetFinalCommandLine(DEFAULT_DUNITM_BUILD_COMMAND, lProject);
end;

end.
