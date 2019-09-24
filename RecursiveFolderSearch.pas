unit RecursiveFolderSearch;

interface

{$IFDEF VER120} {$DEFINE BEFOREVARIANTS} {$DEFINE BEFORE_INLINE} {$ENDIF}
{$IFDEF VER130} {$DEFINE BEFOREVARIANTS} {$DEFINE BEFORE_INLINE} {$ENDIF}
{$IFNDEF BEFOREVARIANTS}
 {$IFDEF VER140} {$DEFINE BEFORE_INLINE} {$ENDIF}
 {$IFDEF VER150} {$DEFINE BEFORE_INLINE} {$ENDIF}
 {$IFDEF VER160} {$DEFINE BEFORE_INLINE} {$ENDIF}
 {$IFDEF VER170} {$DEFINE BEFORE_INLINE} {$ENDIF}
 {$IF CompilerVersion >= 17.0}
      {$DEFINE HAS_INLINE}
 {$IFEND}
 {$IF CompilerVersion >= 20.0}
      {$DEFINE HAS_VARUSTRING}
      {$DEFINE HAS_STRINGBUILDER}
 {$IFEND}
 {$IF CompilerVersion >= 23.0}
      {$DEFINE HAS_VARUSTRING}
 {$IFEND}
{$ENDIF}

uses SysUtils, Classes;

{$IFNDEF HAS_STRINGBUILDER}
 Type
 TStringBuilder = Class(TStringStream)
 public
   constructor Create;
   function ToString: string;
   Procedure Append(AText: string);
 end;
{$ENDIF}

Function SearchFolderForFiles(const AFilter: string; const AStartFolder: String;
  AOptions: string = ''): string; overload;

Procedure SearchFolderForFiles(const AFilter: string;
  const AStartFolder: String; ABuilder: TStringBuilder;
  AOptions: string = ''); overload;

implementation

Function SearchFolderForFiles(const AFilter: string; const AStartFolder: String;
  AOptions: string = ''): string;
var
  lBuilder: TStringBuilder;
begin
  Result := '';
  lBuilder := TStringBuilder.create;
  try
    SearchFolderForFiles(AFilter, AStartFolder, lBuilder, AOptions);
    Result := lBuilder.ToString;
  finally
    freeandnil(lBuilder);
  end;
end;

Procedure SearchFolderForFiles(const AFilter: string;
  const AStartFolder: String; ABuilder: TStringBuilder; AOptions: string = '');
var
  lFolderSearchRec: TSearchRec;
  lSearchList: TStringlist;
  lSearchStr: String;
  lStartFolder, lSubFolder, lExtension: String;
  i: integer;
begin
  lSearchList := TStringlist.create;
  try
    lSearchList.Text := StringReplace(AFilter, ';', #13#10, [rfReplaceAll]);
    lStartFolder := IncludeTrailingPathDelimiter(AStartFolder);

    // Locate the Files in this folder
    for i := 0 to lSearchList.Count - 1 do
    begin
      lSearchStr := lSearchList[i];
      lExtension := extractFileExt(lSearchStr);
      if findfirst(lStartFolder + lSearchStr, 0, lFolderSearchRec) = 0 then
        try
          repeat
            if SameText(extractFileExt(lFolderSearchRec.name), lExtension) then
            begin
              ABuilder.Append(lStartFolder + lFolderSearchRec.name + #13#10);
            end;
          until findnext(lFolderSearchRec) <> 0;
        finally
          findclose(lFolderSearchRec);
        end;
    end;

    // Now do the Subfolders
    if findfirst(lStartFolder+'*', faDirectory, lFolderSearchRec) = 0 then
      try
        repeat
          if (lFolderSearchRec.name[1] = '.') or (lFolderSearchrec.Attr and faDirectory =0)  then
            continue;
          lSubFolder := lStartFolder + lFolderSearchRec.name;
          SearchFolderForFiles(lSearchList.Text, lSubFolder, ABuilder,
            AOptions);
        until findnext(lFolderSearchRec) <> 0;
      finally
        findclose(lFolderSearchRec);
      end;

  finally
    freeandnil(lSearchList);
  end;

end;

{ TStringBuilder }

procedure TStringBuilder.Append(AText: string);
begin
  self.WriteString(AText);
end;

constructor TStringBuilder.Create;
begin
  inherited Create('');
end;

function TStringBuilder.ToString: string;
begin
  result := Self.DataString;
end;

end.
