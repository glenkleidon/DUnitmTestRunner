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

  TDelphiProjectProperties = Class
  private
    fProperties: TStringlist;
    function GetProperties: TStrings;
    procedure DoExtraction(ANodeReader: IXMLNodeReader);
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
  l, i, lGroupDepth, lGroupIndex, lConditionIndex: integer;
  c: char;
  lInQuote: boolean;
  lValue: string;
  lGroupName: string;

  procedure AddArgument;
  var
    p, s: integer;
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

{ TDelphiProjectProperties }

constructor TDelphiProjectProperties.Create;
begin
  self.fProperties := TStringlist.Create;
end;

destructor TDelphiProjectProperties.Destroy;
begin
  freeandnil(self.fProperties);
  inherited;
end;

procedure TDelphiProjectProperties.DoExtraction(ANodeReader: IXMLNodeReader);
begin

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
  result := TStrings(self.fProperties);
end;

end.
