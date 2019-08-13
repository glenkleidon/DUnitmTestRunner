unit Delphi.Lexer;

interface

uses SysUtils;

const
  TOKEN_WHITE_SPACE = ' '#9#13#10;
  TOKEN_SEPARATORS = TOKEN_WHITE_SPACE + ';,.:()[]';
  TOKEN_OPERATORS = '=+-/*';
  TOKEN_QUOTE = '''';

  TOKEN_STOP_CHARS = TOKEN_WHITE_SPACE + TOKEN_SEPARATORS + TOKEN_OPERATORS;

  TOKEN_COMMENT_BLOCKS: Array [1 .. 4, 1 .. 2] of String = (('//', #13),
    ('(*', '*)'), ('{$', '}'), ('{', '}'));

  TOKEN_COMMENT_START = 1;
  TOKEN_COMMENT_STOP = 2;
  TOKEN_COMMENT_NAME_INDEX_EOL = 1;
  TOKEN_COMMENT_NAME_INDEX_BRACKET_STAR = 2;
  TOKEN_DIRECTIVE_NAME_INDEX = 3;
  TOKEN_COMMENT_NAME_INDEX_BRACE = 4;

Type
  TDelphiTokenType = (dtUnknown, dtComment, dtCompilerDirective, dtIdentifier,
    dtKeyword, dtSeparator, dtOperator, dtLiteral, dtEOT);

  TTokenInfo = Record
    isToken: boolean;
    TokenType: TDelphiTokenType;
    StartPos: integer;
    EndPos: integer;
    Token: string;
    Terminator: string;
    TerminatorType: TDelphiTokenType;
  End;

  TTokenInfoHelper = Record helper for TTokenInfo
    Procedure Init;
  End;

Function NextToken(AText: string; AStartPos: integer = 1): TTokenInfo;
Function CheckForComment(AText: string; AStartPos: integer = 1): TTokenInfo;
Function LocateToken(AToken: String; ATokenType: TDelphiTokenType;
  AText: string; AStartPos: integer = 1; AStopAt: String = '';
  ATerminator: string = ''): TTokenInfo;
Function TextBeweenTokens(AStartToken: String;
  AStartTokenType: TDelphiTokenType; AEndToken: String;
  AEndTokenType: TDelphiTokenType; AText: string;
  AEndTokenTerminator: String = ''; AStartPos: integer = 1): String;

implementation

const
  DELPHI_RESERVED_KEYWORDS =
    ',and,array,as,asm,begin,case,class,const,constructor,destructor,' +
    'dispinterface,div,do,downto,else,end,except,exports,file,' +
    'finalization,finally,for,function,goto,if,implementation,in,' +
    'inherited,initialization,inline,interface,is,label,library,mod' +
    ',nil,not,object,of,or,out,packed,procedure,program,property,' +
    'raise,record,repeat,resourcestring,set,shl,shr,string,then,' +
    'threadvar,to,try,type,unit,until,uses,var,while,with,xor,';

function IsKeyWord(AToken: TTokenInfo): boolean;
begin
  result := pos(','+AToken.Token+',', DELPHI_RESERVED_KEYWORDS)>0;
end;

Function SkipWhiteSpace(AText: String; AStartPos: integer = 1): integer;
// inline;
var
  c: Char;
  lSize: integer;
begin
  // locate the first non whitespace
  result := -1;

  lSize := length(AText);
  if (AStartPos) > lSize then
    exit;

  result := AStartPos;

  c := AText[result];
  while (result < lSize) and (pos(c, TOKEN_WHITE_SPACE) <> 0) do
  begin
    inc(result);
    c := AText[result];
  end;

end;

Function CheckForComment(AText: string; AStartPos: integer = 1): TTokenInfo;
var
  lSize: integer;
  lPos, lCommentIndex, i: integer;
  First2Chars: String;
begin
  result.Init;

  lPos := SkipWhiteSpace(AText, AStartPos);

  lSize := length(AText);
  if (lPos > lSize) or (lSize = 0) then
    exit;

  First2Chars := Copy(AText, lPos, 2);

  for i := 1 to 4 do
  begin
    if Copy(First2Chars, 1, length(TOKEN_COMMENT_BLOCKS[i, TOKEN_COMMENT_START])
      ) = TOKEN_COMMENT_BLOCKS[i, TOKEN_COMMENT_START] then
    begin
      if i = TOKEN_DIRECTIVE_NAME_INDEX then
      begin
        result.TokenType := dtCompilerDirective;
      end
      else
      begin
        result.TokenType := dtComment;
      end;
      result.isToken := true;
      result.StartPos := lPos;
      result.EndPos := pos(TOKEN_COMMENT_BLOCKS[i, TOKEN_COMMENT_STOP],
        AText, lPos);
      case i of
        TOKEN_COMMENT_NAME_INDEX_EOL:
          begin
            result.EndPos := result.EndPos - 1;
            if (result.EndPos < 1) then
              result.EndPos := lSize + 1;
          end;
        TOKEN_COMMENT_NAME_INDEX_BRACKET_STAR:
          begin
            result.EndPos := result.EndPos + 1;
          end;
        TOKEN_DIRECTIVE_NAME_INDEX, TOKEN_COMMENT_NAME_INDEX_BRACE:
          begin
            // no modification required
          end;
      end;
      result.Token := Copy(AText, result.StartPos,
        1 + result.EndPos - result.StartPos);
      exit;
    end;
  end;

end;

Function NextToken(AText: string; AStartPos: integer = 1): TTokenInfo;
var
  p, lStart: integer;
  lSize: integer;
  c: Char;
  lInQuote, lDone: boolean;
  lToken: string;
begin
  result.Init;

  // locate the first non whitespace
  lStart := SkipWhiteSpace(AText, AStartPos);
  if lStart = -1 then
    exit;

  result := CheckForComment(AText, lStart);
  if (result.TokenType in [dtCompilerDirective, dtComment]) then
    exit;

  lSize := length(AText);
  result.StartPos := lStart;
  result.EndPos := lStart;
  result.TokenType := dtUnknown;

  c := AText[result.EndPos];
  lInQuote := c = TOKEN_QUOTE;
  lDone := false;
  while true do
  begin
    if lInQuote then
    begin
      if c = TOKEN_QUOTE then
      begin
        if (result.EndPos < lSize) and (AText[result.EndPos + 1] <> TOKEN_QUOTE)
        then
        begin
          // The end of a text literal token
          result.Terminator := TOKEN_QUOTE;
          result.TokenType := dtLiteral;
          result.TerminatorType := dtSeparator;
          break;
        end
      end
      else
      begin
        // We were NOT in quotes, therefore we have just hit one.
        result.TokenType := dtUnknown;
        result.Terminator := TOKEN_QUOTE;
        result.TerminatorType := dtSeparator;
        break;
      end;
      continue;
    end;
    if pos(c, TOKEN_SEPARATORS) > 0 then
    begin
      if (result.EndPos = result.StartPos) then
      begin
        lToken := c;
        result.TokenType := dtSeparator;
      end
      else
        dec(result.EndPos);
      result.TerminatorType := dtSeparator;
      result.Terminator := c;
      // account for the := operator
      if (c = ':') and (result.EndPos < lSize) and
        (AText[result.EndPos + 1] = '=') then
      begin
        result.TerminatorType := dtOperator;
        result.Terminator := ':=';
        if (result.EndPos = result.StartPos) then
          result.TokenType := dtOperator;
      end;
      break;
    end
    else if pos(c, TOKEN_OPERATORS) > 0 then
    begin
      if (result.EndPos = result.StartPos) then
      begin
        lToken := c;
        result.TokenType := dtOperator;
      end
      else
        dec(result.EndPos);
      result.Terminator := c;
      result.TerminatorType := dtOperator;
      break;
    end;
    inc(result.EndPos);
    if (result.EndPos > lSize) then
    begin
      result.Terminator := '';
      result.TerminatorType := dtEOT;
      break;
    end;
    lToken := lToken + c;
    c := AText[result.EndPos];
  end;

  result.Token := lToken;
  if (result.TokenType=dtUnknown) then
    if IsKeyWord(result) then
      result.TokenType := dtKeyword;

end;

Function LocateToken(AToken: String; ATokenType: TDelphiTokenType;
  AText: string; AStartPos: integer = 1; AStopAt: String = '';
  ATerminator: string = ''): TTokenInfo;
var
  lSize, lPos: integer;
  lToken: TTokenInfo;
  lDone, lDoStop, lUseTerminator: boolean;
begin
  result.Init;
  lDoStop := length(AStopAt) > 0;
  lSize := length(AText);
  lPos := AStartPos;
  lUseTerminator := (length(AToken) = 0) and (length(ATerminator) > 0);

  repeat
    lToken := NextToken(AText, lPos);
    if ((sameText(AToken, lToken.Token)) and (ATokenType = lToken.TokenType)) or
      (lUseTerminator and (sameText(lToken.Terminator, ATerminator))) then
    begin
      result := lToken;
      exit;
    end
    else if (lDoStop) and (sameText(lToken.Token, AStopAt)) then
    begin
      result.EndPos := lToken.EndPos;
      exit;
    end;
    lPos := lToken.EndPos + 1;
  until (lPos > lSize);

end;

Function TextBeweenTokens(AStartToken: String;
  AStartTokenType: TDelphiTokenType; AEndToken: String;
  AEndTokenType: TDelphiTokenType; AText: string;
  AEndTokenTerminator: String = ''; AStartPos: integer = 1): String;
var
  lStartToken, lEndToken: TTokenInfo;
begin
  lStartToken := LocateToken(AStartToken, AStartTokenType, AText, AStartPos);
  if not sameText(AStartToken, lStartToken.Token) then
    raise Exception.Create('Start Token not found.');

  lEndToken := LocateToken(AEndToken, AEndTokenType, AText, lStartToken.EndPos,
    '', AEndTokenTerminator);
  if not sameText(AEndToken, lEndToken.Token) then
    raise Exception.Create('End Token not found.');

  result := Copy(AText, lStartToken.EndPos + 1,
    lEndToken.EndPos - lStartToken.EndPos - 1);
end;

{ TokeInfoHelper }

procedure TTokenInfoHelper.Init;
begin
  self.isToken := false;
  self.TokenType := dtUnknown;
  self.StartPos := 0;
  self.EndPos := 0;
  self.Token := '';
end;

end.
