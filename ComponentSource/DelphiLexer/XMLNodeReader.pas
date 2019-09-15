unit XMLNodeReader;

interface

uses SysUtils, Classes;

const

  XML_WHITESPACE = ' '#9#13#10#11#12;

  XML_ATTRIBUTE_COMMENT_NAME = 'Comment';

Type
  TXMLAttribute = Record
    Name: string;
    Value: string;
  End;

  TXMLAttributes = array of TXMLAttribute;

  TXMLNode = record
    Path: string;
    Name: string;
    Attributes: TXMLAttributes;
    Value: string;
    HasValue: boolean;
  end;

  IXmlNodeReader = Interface
    function GetDone: boolean;
    function GetContent: string;
    function GetPath: string;
    function GetPosition: integer;
    property Path: string read GetPath;
    property Position: integer read GetPosition;
    property Content: string read GetContent;
    property Done: boolean read GetDone;
    function NextNode: TXMLNode;
  end;

  TXmlNodeReader = Class(TInterfacedObject, IXmlNodeReader)
  private
    fPosition: integer;
    fContent: string;
    fPath: string;
    function LoadFromFile(AFilename: string): boolean;
    function GetDone: boolean;
    function GetContent: string;
    function GetPath: string;
    function GetPosition: integer;
  public
    property Path: string read GetPath;
    property Position: integer read GetPosition;
    property Content: string read GetContent;
    property Done: boolean read GetDone;
    procedure Reset;
    constructor CreateFromFile(AFilename: string); overload;
    constructor Create(AText: string = ''); overload;
    destructor Destroy; override;
    function NextNode: TXMLNode;
  End;

function StripWhiteSpace(AText: string): string;
function ResetXMLNode: TXMLNode;
function GetXMLAttributes(AText: string): TXMLAttributes;
/// <summary>
/// Return the depth of an XML Path.
/// Eg '' = 0; '/' = 1; '/XML/Path' = 2;
/// </summary>
function PathDepth(AXMLPath: string): integer;

implementation

function ResetXMLNode: TXMLNode;
begin
  result.Path := '';
  result.Name := '';
  result.Value := '';
  result.HasValue := false;
  setlength(result.Attributes, 0);
end;

function StripWhiteSpace(AText: string): string;
var
  i, l, c: integer;
begin
  l := length(AText);
  setlength(result, l);
  c := 0;
  for i := 1 to l do
  begin
    if pos(AText[i], XML_WHITESPACE) = 0 then
    begin
      inc(c);
      result[c] := AText[i];
    end;
  end;
  setlength(result, c);
end;

function GetXMLAttributes(AText: string): TXMLAttributes;
var
  i, b, p, q: integer;
  pText: PChar;
  Done: boolean;
  lName: string;
begin
  setlength(result, 0);
  if length(AText) = 0 then
    exit;

  i := 0;
  b := 1;
  pText := @AText[b];
  Done := false;
  repeat
    p := pos('"', pText);
    if p = 0 then
    begin
      // must be a comment
      setlength(result, i + 1);
      result[i].Name := XML_ATTRIBUTE_COMMENT_NAME;
      result[i].Value := Trim(AText);
      exit;
    end;
    pText := @AText[b + p];
    q := pos('"', pText);
    if q = 0 then
      break;
    // ok we have it.
    setlength(result, i + 1);
    lName := StripWhiteSpace(copy(AText, b, p - 1));
    result[i].Name := copy(lName, 1, length(lName) - 1);
    result[i].Value := copy(AText, b + p, q - 1);
    inc(i);
    inc(b, p + q);
    pText := @AText[b];
    Done := b >= length(AText);
  until Done;
end;

function PathDepth(AXMLPath: string): integer;
var
  p,l,b: integer;
  pPath: PChar;
begin
  result := -1;
  l := length(AXMLPath);
  p := pos('/',AXMLPath);
  if p=0 then exit;
  Result := 0;
  b := 0;
  while (p>0) and (b<l) do
  begin
    if (b+p)<l then inc(result);
    inc(b,p);
    pPath := @AXMLPath[b+1];
    p := pos('/', pPath);
  end;

end;

{ TXmlNodeReader }

constructor TXmlNodeReader.Create(AText: string = '');
begin
  self.Reset;
  self.fContent := AText;
end;

/// <summary>
/// Ensure that this process is not locking as the file may be
/// open by Delphi or other editors.
/// </summary>
constructor TXmlNodeReader.CreateFromFile(AFilename: string);
begin
  if not fileexists(AFilename) then
    raise Exception.Createfmt('File %s does not exist', [AFilename]);
  self.Reset;
  self.LoadFromFile(AFilename);
end;

destructor TXmlNodeReader.Destroy;
begin
  // this destructor is actually unnecessary, but it helps to
  // prove that the destructor is being called when you expect.
  self.fPosition := 0;
  self.fContent := '';
  self.fPath := '';
  inherited;
end;

function TXmlNodeReader.GetContent: string;
begin
  result := self.fContent;
end;

function TXmlNodeReader.GetDone: boolean;
begin
  result := self.Position >= length(self.Content);
end;

function TXmlNodeReader.GetPath: string;
begin
  result := self.fPath;
end;

function TXmlNodeReader.GetPosition: integer;
begin
  result := self.fPosition;
end;

function TXmlNodeReader.LoadFromFile(AFilename: string): boolean;
var
  lStream: TFileStream;
  lStringStream: TStringStream;
begin
  lStringStream := nil;
  lStream := TFileStream.Create(AFilename, fmOpenRead, fmShareDenyNone);
  try
    lStringStream := TStringStream.Create('');
    lStringStream.CopyFrom(lStream, 0);
    self.fContent := lStringStream.DataString;
  finally
    freeandnil(lStringStream);
    freeandnil(lStream);
  end;
end;

function TXmlNodeReader.NextNode: TXMLNode;
var
  p, q, r, l: integer;
  pContent: PChar;
  lOpen, lAttributes: string;
  lFirstOrCloseNode: boolean;

  function NextWhiteSpace(AValue: string): integer;
  var
    i, p1: integer;
  begin
    result := 0;
    for i := 1 to length(XML_WHITESPACE) do
    begin
      p1 := pos(XML_WHITESPACE[i], AValue);
      if p1 > 0 then
      begin
        result := p1;
        exit;
      end;
    end;
  end;

  Procedure ShrinkPath;
  var
    lPath: string;
    p1: integer;
  begin
    p1 := LastDelimiter('/', self.Path);
    if p1 > 0 then
      self.fPath := copy(self.Path, 1, p1 - 1);
  end;

begin
  l := length(fContent);
  result := ResetXMLNode;
  lFirstOrCloseNode := true;
  while lFirstOrCloseNode do
  begin
    pContent := @self.fContent[Position];
    p := pos('<', pContent);
    if p = 0 then
    begin
      self.fPosition := length(self.fContent) + 1;
      exit;
    end;

    q := pos('>', pContent);
    lOpen := copy(pContent, p + 1, q - p - 1);
    self.fPosition := Position + q;

    // Is this a close node?
    if copy(lOpen, 1, 1) = '/' then
    begin
      ShrinkPath;
    end
    else
      lFirstOrCloseNode := false;
  end;
  // Get the Name
  lAttributes := '';
  r := NextWhiteSpace(lOpen);
  if r = 0 then
  begin
    result.Name := lOpen;
    // check for an empty node.
    if copy(result.Name, length(result.Name), 1) = '/' then
    begin
      // fix the name
      result.Name := copy(result.Name, 1, length(result.Name) - 1);
      result.Path := self.Path + '/' + result.Name;
      exit;
    end;
  end
  else
  begin
    result.Name := copy(lOpen, 1, r - 1);
    lAttributes := copy(lOpen, r + 1, MAXINT);
    // Check for Empty node.
    if StripWhiteSpace(lAttributes) = '/' then
      exit;
    result.Attributes := GetXMLAttributes(lAttributes);
  end;
  self.fPath := self.Path + '/' + result.Name;
  result.Path := self.Path;

  // Get The Value;
  pContent := @fContent[Position];
  p := pos('<', pContent);
  if p = 0 then
    p := l;
  result.Value := StripWhiteSpace(copy(pContent, 1, p - 1));
  result.HasValue := length(result.Value) > 0;
  if (p + Position < l) and (pContent[p] { note - pContent is 0 based } = '/')
  then
  begin
    ShrinkPath;
    q := pos('>', pContent);
    self.fPosition := Position + q;
  end;
end;

procedure TXmlNodeReader.Reset;
begin
  self.fPosition := 1;
  self.fContent := '';
  self.fPath := '';
end;

end.
