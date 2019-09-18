unit Delphi.DProj;

interface

uses SysUtils, Classes, XMLNodeReader;

const
  DPROJ_PROPERTY_GROUP = 'PropertyGroup';
  DPROJ_CONDITION_ATTRIBUTE = 'Condition';

type
  TConditionOperator = (coNone, coEquals, coNotEquals, coExists, coAnd, coOr);

  TProjectCondition = record
    Group: string;
    FirstArgument: string;
    ConditionalOperator: TConditionOperator;
    SecondArgument: string;
  end;

  TProjectConditions = array of TProjectCondition;

  IDelphiProjectProperties = Interface
    function GetProperties: TStrings;
    procedure DoExtraction(ANodeReader: IXMLNodeReader);
    function ConditionIndex(ANode: TXMLNode): Integer;
    function ConditionIsMet(ACondition: String): boolean;
    procedure ExtractPropertiesFromDProj(AFilename: string);
    procedure ExtractProperties(AText: string);
    property Properties: TStrings read GetProperties;
  End;

  TDelphiProjectProperties = Class(TInterfacedObject, IDelphiProjectProperties)
  private
    fProperties: TStringlist;
    function GetProperties: TStrings;
    procedure DoExtraction(ANodeReader: IXMLNodeReader);
    function ConditionIndex(ANode: TXMLNode): Integer;
    function ConditionIsMet(ACondition: String): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ExtractPropertiesFromDProj(AFilename: string);
    procedure ExtractProperties(AText: string);
    property Properties: TStrings read GetProperties;
  End;

function ParseCondition(ACondition: string): TProjectConditions;

implementation

function ParseCondition(ACondition: string): TProjectConditions;
var
  l, i, lGroupDepth, lGroupIndex, lConditionIndex: Integer;
  c: char;
  lInQuote: boolean;
  lValue: string;
  lGroupName: string;

  procedure AddArgument;
  var
    p, s: Integer;
    lOperator: TConditionOperator;
  begin
    s := length(result);
    case lConditionIndex of
      1:
        begin
          // Add A new condition
          setlength(result, s + 1);
          result[s].Group := lGroupName;
          result[s].FirstArgument := lValue;
        end;
      2:
        begin
          if trim(lValue) = '==' then
            result[s - 1].ConditionalOperator := coEquals
          else if trim(lValue) = '!=' then
            result[s - 1].ConditionalOperator := coNotEquals
        end;
      3:
        result[s - 1].SecondArgument := lValue;
      4:
        begin
          lOperator := coNone;
          if sameText(trim(lValue), 'or') then
            lOperator := coOr
          else if sameText(trim(lValue), 'and') then
            lOperator := coAnd;

          if lOperator <> coNone then
          begin
            setlength(result, s + 1);
            result[s].Group := lGroupName;
            if result[s].Group = result[s - 1].Group then
              result[s].FirstArgument := result[s].Group
            else
            begin
              p := LastDelimiter('.', result[s - 1].Group);
              result[s].FirstArgument := '.' + copy(result[s - 1].Group,
                1, p - 1);
            end;
            result[s].ConditionalOperator := lOperator;
            inc(lGroupIndex);
          end;
          lConditionIndex := 0;
        end;
    end;
    lValue := '';
    inc(lConditionIndex);
  end;

  procedure SetGroupName;
  begin
    lGroupName := inttoStr(lGroupIndex) + '.' + inttoStr(lGroupDepth);
  end;

begin
  setlength(result, 0);
  l := length(ACondition);
  lInQuote := false;
  lGroupDepth := 0;
  lGroupIndex := 0;
  lConditionIndex := 0;
  lValue := '';
  SetGroupName;
  for i := 1 to l do
  begin
    c := ACondition[i];
    case c of
      '(':
        begin
          if not lInQuote then
          begin
            if (lGroupIndex = 0) then
              inc(lGroupIndex);
            inc(lGroupDepth);
            SetGroupName;
            continue;
          end;
        end;
      ')':
        begin
          if not lInQuote then
          begin
            if (lGroupDepth > 0) then
              dec(lGroupDepth);
            SetGroupName;
            continue;
          end;
        end;
      '''':
        begin
          if lInQuote then
          begin
            lInQuote := false;
            AddArgument;
          end
          else
          begin
            lInQuote := true;
            AddArgument;
          end;
          continue;
        end;
    end;
    lValue := lValue + c;
  end;

end;

Function IsConditionMet(AConditions: TProjectConditions;
  AProperties: TStrings): boolean;
var
  i, lMaxCondition: Integer;
  lCondition: TProjectCondition;
  lArgumentOne, lArgumentTwo: string;
  lArgumentResult: boolean;
  lIgnoreGroup: string;
  lLastIngnoredOperator: TConditionOperator;

  Function GetArgument(AArgument: string): string;
  var
    lArgument: string;
  begin
    if copy(AArgument, 1, 2) = '$(' then
    begin
      lArgument := copy(AArgument, 3, length(AArgument) - 3);
      result := AProperties.Values[lArgument];
    end
    else if copy(AArgument, 1, 1) = '''' then
      result := copy(AArgument, 2, length(AArgument) - 1)
    else
    begin
      result := AArgument;
    end;
  end;

  Function isLastOperator(AIndex: Integer;
    AOperator: TConditionOperator): boolean;
  var
    i: Integer;
  begin
    result := true;
    for i := AIndex + 1 to lMaxCondition do
      if AConditions[i].ConditionalOperator = AOperator then
      begin
        result := false;
        exit;
      end;
  end;

  function isLastOr(AIndex: Integer): boolean;
  begin
    result := isLastOperator(AIndex, coOr);
  end;

  function isLastAnd(AIndex: Integer): boolean;
  begin
    result := isLastOperator(AIndex, coAnd);
  end;

begin
  lMaxCondition := length(AConditions) - 1;
  result := false;
  lIgnoreGroup := '';
  for i := 0 to lMaxCondition do
  begin
    lCondition := AConditions[i];

    // If at least one Argument has failed in this group, but there are more OR
    // conditions to be assessed.
    if (lCondition.Group = lIgnoreGroup) then
    begin
      case lCondition.ConditionalOperator of
        coAnd:
          begin
            if isLastOr(i) then
              exit;
            lLastIngnoredOperator := coAnd;
            continue;
          end;
        coOr:
          begin
            lLastIngnoredOperator := coOr;
            continue;
          end;
      else
        begin
          if (result) then
            continue
          else if lLastIngnoredOperator = coAnd then
            continue;
        end;
      end;
    end;
    lIgnoreGroup := '';
    lLastIngnoredOperator := coNone;

    // Check the arguments.
    lArgumentResult := true;
    lArgumentOne := GetArgument(lCondition.FirstArgument);
    lArgumentTwo := GetArgument(lCondition.SecondArgument);
    case lCondition.ConditionalOperator of
      coEquals:
        lArgumentResult := sameText(lArgumentOne, lArgumentTwo);
      coNotEquals:
        lArgumentResult := not sameText(lArgumentOne, lArgumentTwo);
      coExists:
        lArgumentResult := length(lArgumentOne) > 0;
      coAnd:
        // if we aren't true here, then the condition fails immediately
        if (not result) then
          exit;
      coOr:
        if (result) then
        begin
          if isLastAnd(i) then
            exit
          else
          begin
            // its ok to ignore this group because the previous is already
            // true
            lIgnoreGroup := lCondition.Group;
            continue;
          end;
        end;
    end;
    result := lArgumentResult;
    if (not result) then
    begin
      if isLastOr(i) then
        exit
      else
        lIgnoreGroup := lCondition.Group;
    end;
  end;
end;

{ TDelphiProjectProperties }

function TDelphiProjectProperties.ConditionIsMet(ACondition: String): boolean;
begin
  result := IsConditionMet(ParseCondition(ACondition), Self.fProperties);
end;

constructor TDelphiProjectProperties.Create;
begin
  Self.fProperties := TStringlist.Create;
end;

destructor TDelphiProjectProperties.Destroy;
begin
  freeandnil(Self.fProperties);
  inherited;
end;

procedure TDelphiProjectProperties.DoExtraction(ANodeReader: IXMLNodeReader);
var
  lNode: TXMLNode;
  lCondition: TXMLAttribute;
  lPropertyName: string;

  Function SkipToNextNode: boolean;
  var
    lDepth: Integer;
  begin
    lDepth := PathDepth(lNode.Path);
    repeat
      lNode := ANodeReader.NextNode;
    until (PathDepth(lNode.Path) <= lDepth);

  end;

  Function NodeIsRelevant: boolean;
  var
    lIndex: Integer;
  begin
    result := true;
    lIndex := ConditionIndex(lNode);
    if (lIndex >= 0) and (not ConditionIsMet(lNode.Attributes[lIndex].Value))
    then
    begin
      SkipToNextNode;
      result := NodeIsRelevant;
    end;
  end;

begin

  while not ANodeReader.Done do
  begin
    lNode := ANodeReader.NextNode;
    if not NodeIsRelevant then
      continue;
    case PathDepth(lNode.Path) of
      0 .. 2:
        ; // ignore
      3:
        Self.fProperties.Values[lNode.Name] := lNode.Value;
      4 .. 9999:
        begin
          Self.fProperties.Values[StringReplace(copy(lNode.Path, 2, MAXINT),
            '/', '.', [rfReplaceAll])] := lNode.Value;
        end;
    end;
  end;
end;

procedure TDelphiProjectProperties.ExtractProperties(AText: string);
begin
  DoExtraction(TXMLNodeReader.Create(AText));
end;

procedure TDelphiProjectProperties.ExtractPropertiesFromDProj
  (AFilename: string);
begin
  DoExtraction(TXMLNodeReader.CreateFromFile(AFilename));
end;

function TDelphiProjectProperties.GetProperties: TStrings;
begin
  result := TStrings(Self.fProperties);
end;

function TDelphiProjectProperties.ConditionIndex(ANode: TXMLNode): Integer;
var
  i, lMaxAttribute: Integer;
begin
  result := -1;
  lMaxAttribute := length(ANode.Attributes) - 1;
  for i := 0 to lMaxAttribute do
    if sameText(ANode.Attributes[i].Name, DPROJ_CONDITION_ATTRIBUTE) then
    begin
      result := i;
      exit;
    end;
end;

end.
