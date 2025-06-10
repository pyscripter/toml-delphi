{
    Copyright (c) 2020 by Ryan Joseph

    TOML Parser
    This unit implements the main parser class

    ********************************************************************

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

}


unit TOMLParser;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Scanner,
  TOMLTypes;

type

  TTOMLScanner = class(TScanner)
    private
      document: TTOMLDocument;
      TableStack: TTOMLContainerList;
    private
      function ParseArray: TTOMLArray;
      function ParseTable: TTOMLData;
      function ParseInlineTable: TTOMLTable;
      function ParseArrayOfTables: TTOMLData;
      function ParseString(AllowMultiline: Boolean = True): string;
      function ParseDate(continueFromNumber: boolean = false): TTOMLDate;
      function ParseTime(continueFromNumber: boolean = false;
        IsOffset: Boolean = False): TTOMLDate.TTime;
      procedure ParsePair;
      function ParseValue: TTOMLData;
      function ParseKey: TArray<string>;

      function ReadDigits(digits: integer; decimals: boolean = false): string;
    protected
      procedure ParseToken; override;
      procedure UnknownCharacter(out cont: boolean); override;
      function ReadWord: TScanner.TIdentifier; override;
      function ReadNumber: string; override;
      function GetException: EScannerClass; override;
    public
      destructor Destroy; override;
      procedure Parse; override;
  end;

function GetTOML(contents: TBytes): TTOMLDocument;

implementation

uses
  System.Character,
  System.StrUtils;

type
  ETOML = class(EScanner);

function GetTOML(contents: TBytes): TTOMLDocument;
var
  parser: TTOMLScanner;
begin
  parser := TTOMLScanner.Create(contents);

  // check whether the input is valid utf8
  if not TEncoding.UTF8.IsBufferValid(contents) then
    parser.ParserError('The input is not valid utf8');
  try
    parser.Parse;
    result := parser.document;
  finally
    parser.Free;
  end;
end;

{ TTOMLScanner }

function TTOMLScanner.GetException: EScannerClass;
begin
  result := ETOML;
end;

function TTOMLScanner.ParseString(AllowMultiline: Boolean = True): string;
var
  quote: AnsiChar;
  scalar: Cardinal;
  multiline,
  literal,
  firstPass: boolean;
  DoNotRead: Boolean;
begin
  pattern := [];
  quote := c;
  literal := quote = '''';
  multiline := false;
  firstPass := true;
  DoNotRead := False;
  while true do
    begin
      if DoNotRead then
        DoNotRead := False
      else
        ReadChar;

      // Control character validation
      if (c in [#$0..#$8,#$A..#$1F, #$7F]) and not (multiline and IsLineEnding) then
        ParserError(Format('Invalid character "#%d in string', [Ord(c)]));

      // EOL validation
      if c = #13 then
      begin
        AdvancePattern;
        if c <>  #10 then
          ParserError('Invalid line end: CR without LF');
        AdvancePattern;
        Continue;
      end;

      { Multi-line basic strings are surrounded by three quotation marks on each side
      and allow newlines. A newline immediately following the opening delimiter will be trimmed.
      All other whitespace and newline characters remain intact. }
      if firstPass and (c = quote) then
        begin
          ReadChar;
          firstPass := false;
          if c = quote then
            begin
              if not AllowMultiline then
                ParserError('Invalid use of multiline strings');
              multiline := true;
              // trim first new line
              if Peek(sLineBreak, 1) then
                Advance(Length(sLineBreak));
              continue;
            end
          else // the string is empty so return now
            begin
              result := TEncoding.UTF8.GetString(pattern);
              ReadToken;
              exit;
            end;
        end;

      { For convenience, some popular characters have a compact escape sequence.

        \b         - backspace       (U+0008)
        \t         - tab             (U+0009)
        \n         - linefeed        (U+000A)
        \f         - form feed       (U+000C)
        \r         - carriage return (U+000D)
        \"         - quote           (U+0022)
        \\         - backslash       (U+005C)
        \uXXXX     - unicode         (U+XXXX)
        \UXXXXXXXX - unicode         (U+XXXXXXXX) }

      if not literal and (c = '\') then
        begin
          ReadChar;

          { For writing long strings without introducing extraneous whitespace,
            use a "line ending backslash". When the last non-whitespace character
            on a line is a \, it will be trimmed along with all whitespace (including newlines)
            up to the next non-whitespace character or closing delimiter.
            All of the escape sequences that are valid for basic strings are
            also valid for multi-line basic strings. }

          if WhiteSpaceTillEOL then
            begin
              SkipSpace;
              if c <> quote then
                pattern := pattern + [Ord(c)]
              else
                // corner case: "line ending backslash" followed by a quote
                DoNotRead := True;
              continue;
            end;

          // escaped quotes
          if c = quote then
            pattern := pattern + [Ord(c)]
          else
            case c of
              'b':
                pattern := pattern + [8];
              't':
                pattern := pattern + [9];
              'n':
                pattern := pattern + [10];
              'f':
                pattern := pattern + [12];
              'r':
                pattern := pattern + [13];
              '\':
                pattern := pattern + [Ord(c)];
              'u':
                begin
                  ReadChar;
                  scalar := StrToInt(string('$' + PeekString(4)));
                  if Char(scalar).IsSurrogate then
                    ParserError('Unicode characters need to be unicode scalars');
                  pattern := pattern + TEncoding.Utf8.GetBytes(Char(scalar));
                  Advance(4 - 1);
                end;
              'U':
                begin
                  ReadChar;
                  scalar := StrToInt(string('$' + PeekString(8)));
                  pattern := pattern + TEncoding.Utf8.GetBytes(Char.ConvertFromUtf32(scalar));
                  Advance(8 - 1);
                end;
            else
              ParserError('Bad string escape char: "' + c + '"');
            end
        end
      // line breaks are not allowed
      else if not multiline and IsLineEnding then
        ParserError('Single line strings must not contain line endings (#'+IntToStr(ord(c))+')')
      // join any character that isn't a quote
      else if c <> quote then
        pattern := pattern + [Ord(c)]
      // terminate string
      else if c = quote then
        begin
          if multiline then
            // Up to 5 concequtive quotes are allowed
            begin
              if Peek(quote+quote+quote+quote+quote) then
              begin
                pattern := pattern + [Ord(c), Ord(c)];
                result := TEncoding.UTF8.GetString(pattern);
                ReadTo(5);
                exit;
              end
              else if Peek(quote+quote+quote+quote) then
              begin
                // interpret the first as a quote inside the string
                pattern := pattern + [Ord(c)];
                result := TEncoding.UTF8.GetString(pattern);
                ReadTo(4);
                exit;
              end
              // end of string
              else if Peek(quote+quote+quote) then
              begin
                result := TEncoding.UTF8.GetString(pattern);
                ReadTo(3);
                exit;
              end
              else
                pattern := pattern + [Ord(c)];
            end
          else
            begin
              result := TEncoding.UTF8.GetString(pattern);
              ReadTo(1);
              exit;
            end;
        end;
    end;
  Assert(false, 'string termination error');
end;

function TTOMLScanner.ParseArrayOfTables: TTOMLData;
var
  keys: TArray<string>;
  table: TTOMLData;
  parent, child: TTOMLTable;
  arr: TTOMLArray;
  i: integer;
  StartLine: Integer;
begin
  StartLine := fileInfo.line;

  Consume(TToken.SquareBracketOpen);
  Consume(TToken.SquareBracketOpen);
  keys := ParseKey;

  //writeln('parse array of tables: ',keys.CommaText);

  TableStack.Pop;
  parent := TableStack.Peek;

  for i := 0 to Length(keys) - 1 do
    begin
      table := parent.Find(keys[i]);
      if table = nil then
        begin
          // add array as value for key and then add empty table
          arr := TTOMLArray.Create;
          child := TTOMLTable.Create(keys[i]);
          child.parent := parent;
          arr.Add(child);

          parent.Add(child.Name, arr);
          parent := child;
        end
      else if table is TTOMLTable then
        parent := TTOMLTable(table)
      else if table is TTOMLArray then
        begin
          // the last key should add a new table to the array
          if i = length(keys) - 1 then
            begin
              child := TTOMLTable.Create(keys[i]);
              TTOMLArray(table).Add(child);
            end
          else
            child := TTOMLArray(table).Last as TTOMLTable;
          parent := child;
        end;
    end;

  // push table
  TableStack.Push(parent);
  result := parent;

  if c <>  ']' then
    ParserError('Table array headers must end with "]]"');

  Consume(TToken.SquareBracketClosed);

  if (fileInfo.Line > StartLine) then
    ParserError('Table array headers must be on a single line');

  Consume(TToken.SquareBracketClosed);

  if (token <> TToken.EOF) and (fileInfo.Line = StartLine) then
    ParserError('Table array headers must finish with EOL');
end;

function TTOMLScanner.ParseTable: TTOMLData;
var
  keys: TArray<string>;
  table: TTOMLData;
  parent, child: TTOMLTable;
  i: integer;
  StartLine: Integer;
begin
  StartLine := fileInfo.line;

  if c = '[' then
    exit(ParseArrayOfTables);

  Consume(TToken.SquareBracketOpen);
  // parse array of tables
  keys := ParseKey;

  //writeln('parse table: ',keys.CommaText);

  TableStack.Pop;
  parent := TableStack.Peek;

  for i := 0 to Length(keys) - 1 do
    begin
      table := parent.Find(keys[i]);
      if table = nil then
        begin
          child := TTOMLTable.Create(keys[i]);
          parent.Add(child.Name, child);
          parent := child;
        end
      else if table is TTOMLTable then
        begin
          // the final key defines a new table
          // which is illegal if
          if (i = Length(keys) - 1) and TTOMLTable(table).defined then
            ParserError('Table "'+keys[i]+'" is already defined')
          else
            parent := TTOMLTable(table);
        end
      else if table is TTOMLArray then
        begin
          child := TTOMLArray(table).Last as TTOMLTable;
          parent := child;
        end
      else
        ParserError('Key "'+keys[i]+'" is already defined as a value.');
    end;

  // the final table is now defined
  parent.defined := true;

  // push table
  TableStack.Push(parent);
  result := parent;

  if (fileInfo.Line > StartLine) then
    ParserError('Table headers must be on a single line');

  Consume(TToken.SquareBracketClosed);

  if (token <> TToken.EOF) and (fileInfo.Line = StartLine) then
    ParserError('Tables headers must finish with EOL');
end;

{ Parse inline tables
  https://toml.io/en/v1.0.0-rc.1#inline-table }

function TTOMLScanner.ParseInlineTable: TTOMLTable;
begin
  // inline tables don't allow newlines so we can override the newline behavior
  // of the scanner by enabling newlines as tokens
  readLineEndingsAsTokens := true;

  Consume(TToken.CurlyBracketOpen);

  // push new table to stack
  result := TTOMLTable.Create;
  TableStack.Push(result);

  try
    while not TryConsume(TToken.CurlyBracketClosed) do
    begin
      ParsePair;

      if TryConsume(TToken.Comma) then
      begin
        // curly bracket found for pair
        if TryConsume(TToken.CurlyBracketClosed) then
          ParserError('Inline tables do not allow trailing commas.');
        continue;
      end
      else
      begin
        Consume(TToken.CurlyBracketClosed);
        Break;
      end;
    end;
    // disable EOL tokens and clear the next one if it's found
    readLineEndingsAsTokens := false;
    TryConsume(TToken.EOL);
  except
    result.Free;
    raise;
  end;

  result.terminated := true;

  TableStack.Pop;
end;


function TTOMLScanner.ParseArray: TTOMLArray;
var
  value: TTOMLData;
  oldreadLineEndingsAsTokens: Boolean;
begin
  oldreadLineEndingsAsTokens := readLineEndingsAsTokens;
  readLineEndingsAsTokens := False;

  Consume(TToken.SquareBracketOpen);
  result := TTOMLArray.Create;
  result.terminated := true;


  while not TryConsume(TToken.SquareBracketClosed) do
  begin
    value := ParseValue;
    result.Add(value);
    if not TryConsume(TToken.Comma) then
    begin
      Consume(TToken.SquareBracketClosed);
      Break;
    end;
  end;
  readLineEndingsAsTokens := oldreadLineEndingsAsTokens;
end;

function TTOMLScanner.ParseValue: TTOMLData;

  function ParseNamedValue(negative: boolean = false): TTOMLData;
  var
    valueString: string;
  begin
    result := nil;
    valueString := TEncoding.Utf8.GetString(pattern);
    if (valueString = 'false') or (valueString = 'true') then
      begin
        if negative then
          ParserError('Negative booleans are invalid');
        result := TTOMLNumber.Create(StrToBool(valueString), TTOMLNumberType.Boolean);
        Consume;
      end
    else if valueString = 'inf' then
      begin
        if negative then
          result := TTOMLNumber.Create(-1/0, TTOMLNumberType.Float)
        else
          result := TTOMLNumber.Create(1/0, TTOMLNumberType.Float);
        Consume;
      end
    else if valueString = 'nan' then
      begin
        result := TTOMLNumber.Create(0/0, TTOMLNumberType.Float);
        Consume;
      end;
  end;

  function BinToInt(const S: string): Int64;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 1 to Length(S) do
    begin
      if S[i] = '1' then
        Result := (Result shl 1) or 1
      else if S[i] = '0' then
        Result := Result shl 1
      else
        raise EConvertError.Create('Invalid binary digit: ' + S[i]);
    end;
  end;

  function OctalToInt(const S: string): Int64;
  var
    i: Integer;
  begin
    Result := 0;
    // Process each digit
    for i := 1 to Length(S) do
    begin
      case S[i] of
        '0'..'7':
          begin
            Result := Result * 8 + Ord(S[i]) - Ord('0');
          end;
        else
          raise EConvertError.CreateFmt('Invalid octal digit "%s" in string "%s"', [S[i], S]);
      end;
    end;
  end;

var
  negative: boolean;
  str: string;
begin
  Result := nil;

  case token of
    TToken.DoubleQuote:
      result := TTOMLValue.Create(ParseString);
    TToken.SingleQuote:
      result := TTOMLValue.Create(ParseString);
    TToken.Integer:
      begin
        // the integer is a possible date so switch parsers
        if (Length(pattern) = 4) and (c = '-') then
          begin
            result := ParseDate(true);
            Consume;
          end
        else if (Length(pattern) = 2) and (c = ':') then
          begin
            result := TTOMLDate.Create(ParseTime(true));
            Consume;
          end
        else
          begin
            if ((Length(pattern) > 1)  and (pattern[0] = Ord('0'))) or
              ((Length(pattern) > 2)  and (pattern[0] in [Ord('+'), Ord('-')]) and
              (pattern[1] = Ord('0')))
            then
              ParserError('Numbers with leading zeros not allowed');

            str := TEncoding.UTF8.GetString(pattern);
            result := TTOMLNumber.Create(StrToInt64(str), TTOMLNumberType.Integer);
            Consume;
          end;
      end;
    TToken.HexadecimalNumber:
      begin
        if pattern[0] in [Ord('+'), Ord('-')] then
          ParserError('Hex numbers should not have a sign');
        str := TEncoding.UTF8.GetString(pattern);
        result := TTOMLNumber.Create(StrToInt64(str), TTOMLNumberType.Hexadecimal);
        Consume;
      end;
    TToken.OctalNumber:
      begin
        if pattern[0] in [Ord('+'), Ord('-')] then
          ParserError('Octal numbers should not have a sign');
        result := TTOMLNumber.Create(OctalToInt(TEncoding.UTF8.GetString(pattern)), TTOMLNumberType.Octal);
        Consume;
      end;
    TToken.BinaryNumber:
      begin
        if pattern[0] in [Ord('+'), Ord('-')] then
          ParserError('Binary numbers should not have a sign');
        result := TTOMLNumber.Create(BinToInt(TEncoding.UTF8.GetString(pattern)), TTOMLNumberType.Binary);
        Consume;
      end;
    TToken.RealNumber:
      begin
        if ((Length(pattern) > 1)  and (pattern[0] = Ord('0')) and (pattern[1] in [Ord('0')..Ord('9')])) or
          ((Length(pattern) > 2)  and (pattern[0] in [Ord('+'), Ord('-')]) and
          (pattern[1] = Ord('0')) and (pattern[2] in [Ord('0')..Ord('9')]))
        then
          ParserError('Numbers with leading zeros not allowed');

        str := TEncoding.UTF8.GetString(pattern);
        result := TTOMLNumber.Create(StrToFloat(str), TTOMLNumberType.Float);
        Consume;
      end;
    TToken.SquareBracketOpen:
      result := ParseArray;
    TToken.CurlyBracketOpen:
      result := ParseInlineTable;
    TToken.Plus,
    TToken.Dash:
      begin
        negative := token = TToken.Dash;
        Consume;
        result := ParseNamedValue(negative);
      end;
    TToken.ID:
      begin
        result := ParseNamedValue;
        if result = nil then
          ParserError('Invalid value "' + TEncoding.UTF8.GetString(pattern) +'"');
      end;
    else
      ParserError('Unexpected token "'+token.ToString+'"')
  end;
  Assert(result <> nil, 'Invalid TOML value from "'+token.ToString+'"');
end;

function TTOMLScanner.ParseKey: TArray<string>;
begin
  { *Bare* keys may only contain ASCII letters, ASCII digits, underscores, and dashes (A-Za-z0-9_-).
    Note that bare keys are allowed to be composed of only ASCII digits, e.g. 1234,
    but are always interpreted as strings.

    *Quoted* keys follow the exact same rules as either basic strings or literal strings and allow
    you to use a much broader set of key names. Best practice is to use bare keys except when absolutely necessary. }

  result := [];

  while true do
    begin
      if (token = TToken.DoubleQuote) or (token = TToken.SingleQuote) then
        begin
          Consume;
          result := result + [ParseString(False)];
        end
      else if token = TToken.Integer then
        begin
          if c in ['a'..'z','A'..'Z','_','-'] then  // e.g. 2004-abc
            Consume(['0'..'9', 'a'..'z','A'..'Z','_','-']);
          result := result + [TEncoding.UTF8.GetString(pattern)];
          Consume;
        end
      else if token = TToken.RealNumber then  // e.g. 1.2   = true
        begin
          if c in ['a'..'z','A'..'Z','_','-'] then
            Consume(['0'..'9', 'a'..'z','A'..'Z','_','-']);
          result := result + TEncoding.UTF8.GetString(pattern).Split(['.']);
          Consume;
        end
      else if token = TToken.Dash then
        begin
          if c in ['a'..'z','A'..'Z','_','-'] then
            Consume(['0'..'9', 'a'..'z','A'..'Z','_','-']);
          result := result + TEncoding.UTF8.GetString(pattern).Split(['.']);
          Consume;
        end
      else
        begin
          result := result + [TEncoding.UTF8.GetString(pattern)];
          Consume(TToken.ID);
        end;

      if not TryConsume(TToken.Dot) then
        break;
    end;
end;

{ Parse key/value pair
  https://toml.io/en/v1.0.0-rc.1#keyvalue-pair }

procedure TTOMLScanner.ParsePair;
var
  keys: TArray<string>;
  lastKey: TTOMLData;
  table, value: TTOMLData;
  child, parent: TTOMLTable;
  i: integer;
  StartLine: Integer;
begin
  StartLine := fileInfo.line;

  keys := ParseKey;
  Consume(TToken.Equals);

  if fileInfo.line > StartLine then
    ParserError('In key-value pairs, the key and value must be on the same line');

  //writeln('parse pair: ',keys.CommaText);

  parent := TableStack.Peek;

  // add dotted keys as tables
  if Length(keys) > 1 then
    begin
      for i := 0 to Length(keys) - 2 do
        begin
          table := parent.Find(keys[i]);
          if table = nil then
            begin
              child := TTOMLTable.Create;
              parent.Add(keys[i], child);
              parent := child;
            end
          else if table is TTOMLTable then
            parent := TTOMLTable(table)
          else if table is TTOMLArray then
            begin
              // the node at the current key is a fully defined array
              {
                list.items = [1,2,3]
                list.items.more = 1  # ERROR
              }
              if TTOMLArray(table).terminated then
                ParserError('Dotted key "'+keys[i]+'", can''t index into array');
              child := TTOMLArray(table).Last as TTOMLTable;
              parent := child;
            end;
        end;
    end;

  value := ParseValue;
  try
    if (length(keys) <= 1) and (parent.Find(keys[0]) <> nil) then
      begin
        if (value is TTOMLTable) and (TTOMLTable(value).terminated) then
          ParserError('Inline tables can not replace partially defined tables');
      end;

    if parent.terminated then
      ParserError('Additional keys can not be added to fully defined inline tables');

    // push the last key to the parent table
    if Length(keys) > 1 then
      begin
        lastKey := parent.Find(keys[Length(keys) - 2]);
        if (lastKey <> nil) and (lastKey is TTOMLValue) then
          ParserError('"'+keys[Length(keys) - 2]+'" is already defined as '+TTOMLValue(lastKey).TypeString);
      end;
  except
    value.Free;
    raise;
  end;

  parent.defined := true;

  parent.Add(keys[Length(keys) - 1], value);
end;

function TTOMLScanner.ReadDigits(digits: integer; decimals: boolean = false): string;
begin
  pattern := [];
  while Length(pattern) < digits do
    begin
      if not (c in ['0'..'9']) then
        ParserError('Expected '+ digits.ToString + ' digits but got "' +
          Length(pattern).ToString + '".');
      AdvancePattern;
    end;

  // decimal part at the end
  if decimals and (c = '.') then
    begin
      AdvancePattern;
      if not (c in ['0'..'9']) then
        ParserError('In floating numbers the dot needs to be surrounded by at least one digit on each side');
      while c in ['0'..'9'] do
        AdvancePattern;
    end;

  result := TEncoding.UTF8.GetString(pattern);
end;

function TTOMLScanner.ParseTime(continueFromNumber: boolean = false; IsOffset:
    Boolean = False): TTOMLDate.TTime;
begin
  // hours
  // the parsing is being continued from a number
  // so the hour is already in the pattern buffer
  if continueFromNumber then
    result.hours := StrToInt(TEncoding.UTF8.GetString(pattern))
  else
    result.hours := StrToInt(ReadDigits(2));
  Consume(':');

  // minutes
  result.minutes := StrToInt(ReadDigits(2));

  // seconds only if it is not an offset
  if not IsOffset then
    begin
      Consume(':');
      result.seconds := StrToFloat(ReadDigits(2, true));
    end
  else
    result.seconds := 0;

  if (result.hours >= HoursPerDay) or (result.minutes >= MinsPerHour) or
    (result.seconds >= SecsPerMin)
  then
    ParserError('Invalid time');
end;

function TTOMLScanner.ParseDate(continueFromNumber: boolean): TTOMLDate;
var
  date: TTOMLDate;
  HasTime: Boolean;
  PositiveOffset: Boolean;
  LDate: TDateTime;
begin
  date := TTOMLDate.Create;
  try
    // the parsing is being continued from a number
    // so the year is already in the pattern buffer
    if continueFromNumber then
      date.year := StrToInt(TEncoding.UTF8.GetString(pattern))
    else
      date.year := StrToInt(ReadDigits(4));
    Consume('-');

    // month
    date.month := StrToInt(ReadDigits(2));

    // day
    Consume('-');
    date.day := StrToInt(ReadDigits(2));

    if not TryEncodeDate(date.year, date.month, date.day, LDate) then
      ParserError('Invalid date');

    // the date is a solo year
    if IsLineEnding or IsEOF then
      begin
        result := date;
        exit;
      end;

    // Date must be separated by single space or "T" from time
    // but time may be missing

    HasTime := UpCase(c) = 'T';
    if HasTime or (c = ' ') then
    begin
      Advance(1);

      HasTime := HasTime or (c in ['0'..'9']);
      if HasTime then
      begin
        // hour
        date.time := ParseTime;

        // zulu
        if UpCase(c) = 'Z' then
        begin
          Advance(1);
          date.time.z := true;
        end;

        // offset time
        if c in ['+', '-'] then
        begin
          PositiveOffset := c = '+';
          Advance(1);;
          date.offset := ParseTime(False, True);
          if PositiveOffset then
            date.offset.hours := -date.offset.hours;
        end;
      end;
    end;
  except
    date.Free;
    raise;
  end;

  pattern := [];

  result := date;
end;

function TTOMLScanner.ReadNumber: string;

  function LastChar: AnsiChar;
  begin
    if Length(pattern) = 0 then
      result := #0
    else
      result := AnsiChar(pattern[high(pattern)]);
  end;

var
  negative: boolean;
  underscore: boolean;
  found: boolean;
label
  Finished;
begin
  negative := false;
  underscore := false;
  pattern := [];
  token := TToken.Integer;

  if c = '-' then
  begin
    negative := true;
    AdvancePattern;
  end
  else if c = '+' then
    AdvancePattern;

 if (negative and (c = '-')) or (c = '+') then
   ParserError('Invalid +/- sequence');


  while c in ['0'..'9', '.', 'e', 'E', '_'] do
    begin
      if c = '_' then
        begin
          if ((length(pattern) = 0) or not (pattern[length(pattern) - 1] in [Ord('0')..Ord('9')])) or
            not (Peek(1) in ['0'..'9']) or underscore
          then
            ParserError('In floating numbers the dot needs to be surrounded by at least one digit on each side');
          ReadChar;
          underscore := true;
          continue;
        end;

      if c in ['0'..'9'] then
        underscore := false;

      // parse octal
      if (c = '0') and (Peek(1) = 'o') then
        begin
          token := TToken.OctalNumber;
          Advance(2);
          if c = '_' then
            ParserError('Each underscore must be surrounded by at least one digit on each side');
          continue;
        end;

      // parse binary
      if (c = '0') and (Peek(1) = 'b') then
        begin
          token := TToken.BinaryNumber;
          Advance(2);
          if c = '_' then
            ParserError('Each underscore must be surrounded by at least one digit on each side');
          continue;
        end;

      // parse hexadecimal
      if (c = '0') and (Peek(1) = 'x') then
        begin
          token := TToken.HexadecimalNumber;
          AdvancePattern(2);
          while true do
            begin
              if c in ['0'..'9', 'A'..'F','a'..'f','_'] then
                begin
                  if c = '_' then
                    begin
                      if ((length(pattern) = 0) or
                        not (AnsiChar(pattern[length(pattern) - 1]) in ['0'..'9', 'A'..'F','a'..'f'])) or
                        not (Peek(1) in ['0'..'9', 'A'..'F','a'..'f']) or underscore
                      then
                        ParserError('Each underscore must be surrounded by at least one digit on each side');
                      ReadChar;
                      underscore := true;
                      continue;
                    end;
                  AdvancePattern;
                  underscore := false;
                end
              else
                begin
                  if IsWhiteSpace then
                    break
                  else
                    ParserError('Invalid hexadecimal number');
                end;
            end;
          goto Finished;
        end;

      if Char(c).ToLower = 'e' then
        begin
          if underscore then
            ParserError('Each underscore must be surrounded by at least one digit on each side');
          token := TToken.RealNumber;
          AdvancePattern;
          if (c = '-') or (c = '+') then
            AdvancePattern;
          found := false;
          while c in ['0'..'9', '_'] do
            begin
              found := true;
              if c = '_' then
                begin
                  if (length(pattern) = 0) or
                    not (pattern[length(pattern) - 1] in [Ord('0')..Ord('9')]) or underscore
                  then
                    ParserError('Each underscore must be surrounded by at least one digit on each side');
                  ReadChar;
                  underscore := true;
                  continue;
                end;
              AdvancePattern;
              underscore := false;
            end;
          if not found then
            ParserError('Exponent must be followed by an integer');
          break;
        end
      else if c = '.' then
      begin
        if ((length(pattern) = 0) or not (pattern[length(pattern) - 1] in [Ord('0')..Ord('9')]))  or
          not (Peek(1) in ['0'..'9'])
        then
          ParserError('In floating numbers the dot needs to be surrounded by at least one digit on each side');
        token := TToken.RealNumber;
      end;

      AdvancePattern;
    end;

  Finished:

  if underscore then
    ParserError('Each underscore must be surrounded by at least one digit on each side');

  // incomplete prefixed number
  if Length(pattern) = 0 then
    case token of
      TToken.OctalNumber:
        ParserError('Incomplete octal number');
      TToken.BinaryNumber:
        ParserError('Incomplete binary number');
    end
    else if (Length(pattern) = 2) and (token = TToken.HexadecimalNumber) then
      ParserError('Incomplete hexadecimal number');

  result := TEncoding.UTF8.GetString(pattern);
end;

function TTOMLScanner.ReadWord: TScanner.TIdentifier;
begin
  pattern := [];
  // TODO: words must start with alphas and quotes are allowed
  // dashes must have be adjacent to at least 1 alpha
  while c in ['a'..'z','A'..'Z','0'..'9','_','-'] do
    AdvancePattern;
  result := TEncoding.UTF8.GetString(pattern);
end;

procedure TTOMLScanner.UnknownCharacter(out cont: boolean);
begin
  case c of
    '"':
      token := TToken.DoubleQuote;
    '''':
      token := TToken.SingleQuote;
    '#':
      begin
        repeat
          ReadChar;
          if c in [#$1..#$8,#$B, #$C, #$E..#$1F, #$7F] then
            ParserError(Format('Invalid character "#%d in comment', [Ord(c)]));
        until IsLineEnding or IsEOF;
        cont := true;
      end;
    else
      inherited;
  end;
end;

procedure TTOMLScanner.ParseToken;
var
  OldLine: Integer;
begin
  case token of
    TToken.SquareBracketOpen:
      ParseTable;
    // key/value pairs can be ID, Integer (non-negative) or strings (see ParseKey)
    TToken.ID,
    TToken.Integer,
    TToken.RealNumber,
    TToken.DoubleQuote,
    TToken.SingleQuote,
    TToken.Dash:
      begin
        OldLine := fileInfo.line;
        ParsePair;
        if (token <> TToken.EOF) and (fileInfo.Line = OldLine) then
          ParserError('Key-value pairs must finish with EOL');
      end
    else
      if token <> TToken.EOF then
        ParserError('Unexpected "'+token.ToString+'" found.');
  end;
end;

procedure TTOMLScanner.Parse;
begin
  TableStack := TTOMLContainerList.Create;
  document := TTOMLDocument.Create('document');
  try
    // Push twice so that it stays at the top once the document
    // key/value pair are processed and the first subtable is found
    TableStack.Push(document);
    TableStack.Push(document);
    inherited;
  except
    FreeAndNil(document);
    raise;
  end;
end;

destructor TTOMLScanner.Destroy;
begin
  TableStack.Free;

  inherited;
end;


end.
