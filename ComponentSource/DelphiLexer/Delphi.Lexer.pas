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

Function SkipWhiteSpace(AText: String; AStartPos: integer = 1): integer;
// inline;
var
  c: Char;
  lSize: integer;
begin
  // locate the first non whitespace
  Result := -1;

  lSize := length(AText);
  if (AStartPos) > lSize then
    exit;

  Result := AStartPos;

  c := AText[Result];
  while (Result < lSize) and (pos(c, TOKEN_WHITE_SPACE) <> 0) do
  begin
    inc(Result);
    c := AText[Result];
  end;

end;

Function CheckForComment(AText: string; AStartPos: integer = 1): TTokenInfo;
var
  lSize: integer;
  lPos, lCommentIndex, i: integer;
  First2Chars: String;
begin
  Result.Init;

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
        Result.TokenType := dtCompilerDirective;
      end
      else
      begin
        Result.TokenType := dtComment;
      end;
      Result.isToken := true;
      Result.StartPos := lPos;
      Result.EndPos := pos(TOKEN_COMMENT_BLOCKS[i, TOKEN_COMMENT_STOP],
        AText, lPos);
      case i of
        TOKEN_COMMENT_NAME_INDEX_EOL:
          begin
            Result.EndPos := Result.EndPos - 1;
            if (Result.EndPos < 1) then
              Result.EndPos := lSize + 1;
          end;
        TOKEN_COMMENT_NAME_INDEX_BRACKET_STAR:
          begin
            Result.EndPos := Result.EndPos + 1;
          end;
        TOKEN_DIRECTIVE_NAME_INDEX, TOKEN_COMMENT_NAME_INDEX_BRACE:
          begin
            // no modification required
          end;
      end;
      Result.Token := Copy(AText, Result.StartPos,
        1 + Result.EndPos - Result.StartPos);
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
  Result.Init;

  // locate the first non whitespace
  lStart := SkipWhiteSpace(AText, AStartPos);
  if lStart = -1 then
    exit;

  Result := CheckForComment(AText, lStart);
  if (Result.TokenType in [dtCompilerDirective, dtComment]) then
    exit;

  lSize := length(AText);
  Result.StartPos := lStart;
  Result.EndPos := lStart;
  Result.TokenType := dtUnknown;

  c := AText[Result.EndPos];
  lInQuote := c = TOKEN_QUOTE;
  lDone := false;
  while true do
  begin
    if lInQuote then
    begin
      if c = TOKEN_QUOTE then
      begin
        if (Result.EndPos < lSize) and (AText[Result.EndPos + 1] <> TOKEN_QUOTE)
        then
        begin
          // The end of a text literal token
          Result.Terminator := TOKEN_QUOTE;
          Result.TokenType := dtLiteral;
          Result.TerminatorType := dtSeparator;
          break;
        end
      end
      else
      begin
        // We were NOT in quotes, therefore we have just hit one.
        Result.TokenType := dtUnknown;
        Result.Terminator := TOKEN_QUOTE;
        Result.TerminatorType := dtSeparator;
        break;
      end;
      continue;
    end;
    if pos(c, TOKEN_SEPARATORS) > 0 then
    begin
      if (Result.EndPos=Result.StartPos) then 
      begin
        lToken := c;
        Result.TokenType := dtSeparator;
      end else dec(Result.EndPos); 
      Result.TerminatorType := dtSeparator;
      Result.Terminator := c;
      // account for the := operator
      if (c = ':') and (Result.EndPos < lSize) and
        (AText[Result.EndPos + 1] = '=') then
      begin
        Result.TerminatorType := dtOperator;
        Result.Terminator := ':=';
        if (Result.EndPos=Result.StartPos) then Result.TokenType := dtOperator; 
      end;
      break;
    end
    else if pos(c, TOKEN_OPERATORS) > 0 then
    begin
      if (Result.EndPos=Result.StartPos) then
      begin
       lToken := c;
       Result.TokenType := dtOperator;
      end else dec(Result.EndPos);
      Result.Terminator := c;
      Result.TerminatorType := dtOperator;
      break;
    end;
    inc(Result.EndPos);
    if (Result.EndPos > lSize) then
    begin
      Result.Terminator := '';
      Result.TerminatorType := dtEOT;
      break;
    end;
    lToken := lToken + c;
    c := AText[Result.EndPos];
  end;

  Result.Token := lToken;

end;

Function LocateToken(AToken: String; ATokenType: TDelphiTokenType;
  AText: string; AStartPos: integer = 1; AStopAt: String = '';
  ATerminator: string = ''): TTokenInfo;
var
  lSize, lPos: integer;
  lToken: TTokenInfo;
  lDone, lDoStop, lUseTerminator: boolean;
begin
  Result.Init;
  lDoStop := length(AStopAt) > 0;
  lSize := length(AText);
  lPos := AStartPos;
  lUseTerminator := (length(AToken) = 0) and (length(ATerminator) > 0);

  repeat
    lToken := NextToken(AText, lPos);
    if ((sameText(AToken, lToken.Token)) and (ATokenType = lToken.TokenType)) or
      (lUseTerminator and (sameText(lToken.Terminator, ATerminator))) then
    begin
      Result := lToken;
      exit;
    end
    else if (lDoStop) and (sameText(lToken.Token, AStopAt)) then
    begin
      Result.EndPos := lToken.EndPos;
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

  Result := Copy(AText, lStartToken.EndPos+1,
  lEndToken.EndPos - lStartToken.EndPos-1);
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
