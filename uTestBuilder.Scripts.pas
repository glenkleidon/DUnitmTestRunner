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

  DELPHI_BIN_PATH = 'bin\';

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
    IsX64: boolean;
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

uses Registry, Delphi.Versions;

function GetFinalCommandLine(ACommand: string; AProject: TDUnitMBuildData): string;
begin
  result := Acommand;
  Result := Stringreplace(Result, '<DCCPATH>', AProject.DCCPath,[rfReplaceAll, rfIgnoreCase]);
  Result := Stringreplace(Result, '<DCCEXE>', AProject.DCCExe,[rfReplaceAll, rfIgnoreCase]);

  if length(AProject.UnitAliases)=0 then
    Result := Stringreplace(Result, '-A<UNITALIASES>', '',[rfIgnoreCase])
  else
    Result := Stringreplace(Result, '<UNITALIASES>', AProject.UnitAliases,[rfIgnoreCase]);

  if length(AProject.ConditionalDefines)=0 then
    Result := Stringreplace(Result, '-D<CONDITIONALDEFINES>', '',[rfIgnoreCase])
  else
    Result := Stringreplace(Result, '<CONDITIONALDEFINES>', AProject.ConditionalDefines,[rfIgnoreCase]);

  Result := Stringreplace(Result, '<OUTPUTPATH>', AProject.OutputPath,[rfIgnoreCase]);
  Result := Stringreplace(Result, '<DCUOUTPUTPATH>', AProject.OutputPath,[rfIgnoreCase]);
  if not DirectoryExists(AProject.OutputPath) then
    ForceDirectories(AProject.OutputPath);
  if not DirectoryExists(AProject.DCUOutputPath) then
    ForceDirectories(AProject.DCUOutputPath);

  Result := Stringreplace(Result, '<INCLUDESEARCHPATH>', AProject.UnitSearchPath+';' +
    AProject.IncludeSearchPath,[rfIgnoreCase]);

  Result := Stringreplace(Result, '<OBJECTSEARCHPATH>', AProject.UnitSearchPath+';' +
    AProject.ObjectSearchPath,[rfIgnoreCase]);

  Result := Stringreplace(Result, '<RESOURCESEARCHPATH>', AProject.UnitSearchPath+';' +
    AProject.ResourceSearchPath,[rfIgnoreCase]);

  if length(AProject.Namespaces)=0 then
    Result := Stringreplace(Result, '-NS<NAMESPACES>', '',[rfIgnoreCase])
  else
    Result := Stringreplace(Result, '<NAMESPACES>', AProject.Namespaces,[rfIgnoreCase]);

  Result := StringReplace(Result, '<PROJECTNAME>', AProject.ProjectName, [rfIgnoreCase]);

end;

Function ProperiesFromRegistry(AProject: TDUnitMBuildData;
  AVersion: TDelphiVersion): TDUnitMBuildData;
var
  lRegKey: String;
  lCurrentKey: string;
  lReg: TRegistry;
begin
  result := AProject;
  lRegKey := DelphiRegKeyByDelphiVersion(AVersion.DelphiVersion);
  if lRegKey.Length = 0 then
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
    if result.IsX64 then
      lCurrentKey := lCurrentKey + REG_WIN64
    else if AVersion.DelphiVersion > 12 then
      lCurrentKey := lCurrentKey + REG_WIN32;

    lReg.OpenKeyReadOnly(lCurrentKey);
    if lReg.ValueExists(REG_VALUE_SEARCHPATH) then
      result.UnitSearchPath := lReg.ReadString(REG_VALUE_SEARCHPATH);
  finally
    freeandnil(lReg);
  end;
end;

Function Commandline(AProjectPath: string; ADelphiVersion: string): string;
var
  lProject: TDUnitMBuildData;
  lDelphi: TDelphiVersion;
begin
  result := '';
  lProject.ProjectDir := extractFilepath(AProjectPath);
  lProject.ProjectName := ChangeFileExt(AProjectPath, '.dpr');
  //Default output paths - these may get overridden;
  lProject.OutputPath := lProject.ProjectDir;
  lProject.DCUOutputPath := lProject.ProjectDir + 'DCU\';
  if length(ADelphiVersion)=0 then
    lDelphi := ALL_DELPHI_VERSIONS[DelphiIndexByVersion(DefaultDelphiVersion)]
  else
    lDelphi := FindDelphi(ADelphiVersion);

  if lDelphi.DelphiVersion = -1 then
    raise Exception.Createfmt('Delphi Version % not found', [ADelphiVersion]);
  lProject := ProperiesFromRegistry(lProject, lDelphi);
  result := GetFinalCommandLine(DEFAULT_DUNITM_BUILD_COMMAND, lProject);
end;

end.
