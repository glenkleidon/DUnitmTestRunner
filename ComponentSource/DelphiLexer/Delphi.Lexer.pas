unit Delphi.Lexer;

interface

uses SysUtils;

const
  TOKEN_WHITE_SPACE = ' '#9#13#10;
  TOKEN_STOP_CHARS = TOKEN_WHITE_SPACE + ';,. :=()[]`+-/*';
  TOKEN_QUOTE = '''';
  TOKEN_COMMENT_BLOCKS: Array [1 .. 3, 1 .. 2] of String = (('//', #13),
    ('(*', '*)'), ('{', '}'));
  TOKEN_COMMENT_START = 1;
  TOKEN_COMMENT_STOP = 2;
  TOKEN_COMMENT_NAME_INDEX_EOL = 1;
  TOKEN_COMMENT_NAME_INDEX_BRACKET_STAR=2;
  TOKEN_COMMENT_NAME_INDEX_BRACE=3;


Type
  TokenInfo = Record
    isToken: boolean;
    isComment: boolean;
    StartPos: integer;
    EndPos: integer;
    Token: string;
  End;

  TokenInfoHelper = Record helper for TokenInfo
    Procedure Init;
  End;

Function NextToken(AText: string; AStartPos: integer = 1): TokenInfo;
Function CheckForComment(AText: string; AStartPos: integer = 1): TokenInfo;

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

Function CheckForComment(AText: string; AStartPos: integer = 1): TokenInfo;
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

  for i := 1 to 3 do
  begin
    if Copy(First2Chars, 1, length(TOKEN_COMMENT_BLOCKS[i, TOKEN_COMMENT_START])
      ) = TOKEN_COMMENT_BLOCKS[i, TOKEN_COMMENT_START] then
    begin
      Result.isComment := true;
      Result.StartPos := lPos;
      Result.EndPos := pos(AText, TOKEN_COMMENT_BLOCKS[i, TOKEN_COMMENT_STOP]);
      if (Result.EndPos=0) and (i=TOKEN_COMMENT_NAME_INDEX_EOL) then
      begin
       // EOL then assume that the Comment is to the end of the line.
       Result.EndPos := lSize;
      end;
      Result.Token := Copy(AText, Result.StartPos, 1+Result.EndPos-Result.StartPos);
      exit;
    end;
  end;

end;

Function NextToken(AText: string; AStartPos: integer = 1): TokenInfo;
var
  p, lStart: integer;
  lSize: integer;
  c: Char;
  lInQuote: boolean;
begin
  Result.Init;

  // locate the first non whitespace
  lStart := SkipWhiteSpace(AText, AStartPos);
  if lStart = -1 then
    exit;

  lSize := length(AText);

  c := AText[lStart];
  while (lStart < lSize) and (pos(c, TOKEN_WHITE_SPACE) = 0) do
  begin
    inc(lStart);
    c := AText[lStart];
  end;

  if lStart >= lSize then
    exit;

  Result := CheckForComment(AText, lStart);
  if Result.isComment then
    exit;

  Result.StartPos := lStart;
  Result.EndPos := lStart;

  c := AText[Result.EndPos];
  lInQuote := c = TOKEN_QUOTE;

  while (Result.EndPos < lSize) and (pos(c, TOKEN_STOP_CHARS) = 0) do
  begin
    if c = TOKEN_QUOTE then
    begin
      if lInQuote then
      begin
        if (Result.EndPos < lSize) and (AText[Result.EndPos + 1] <> TOKEN_QUOTE)
        then
        begin
          // End of a quote, that is the end of a text literal token
          break;
        end
      end
      else
      begin
        // ok we have reached a new single quote - that is a token
        break;
      end;
    end;
    inc(Result.EndPos);
    c := AText[Result.EndPos];
  end;

end;

{ TokeInfoHelper }

procedure TokenInfoHelper.Init;
begin
  self.isToken := false;
  self.isComment := false;
  self.StartPos := 0;
  self.EndPos := 0;
  self.Token := '';
end;

end.
