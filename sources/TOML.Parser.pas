{
    TOML Parser
    This unit implements the main parser class

    Copyright (c) 2020 by Ryan Joseph
    Extensively re-written by PyScripter

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


unit TOML.Parser;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  TOML.Scanner,
  TOML.Support;

type

  TTOMLParser = class(TScanner)
    private
      document: TJSONObject;
      FDefinedTables: TList<TJSONObject>;
      FCreatedInline: TList<TJSONObject>;
      FImmutableList: TList<TJSONValue>;
      TableStack: TStack<TJsonObject>;
    private
      // support functions
      function GetOrCreateTable(AParent: TJSONObject; Keys: TArray<string>; IsPairKey:
          Boolean = False): TJSONObject;
      function FindValue(Table: TJSONObject; Keys: TArray<string>): TJSONValue;
      procedure AddPair(Table: TJSONObject; Key: string; Value: TJsonValue);

      function ParseArray: TJSONArray;
      procedure ParseTable;
      function ParseInlineTable: TJSONObject;
      procedure ParseArrayOfTables;
      function ParseString(AllowMultiline: Boolean = True): string;
      function ParseDate(continueFromNumber: Boolean): TBytes;
      function ParseTime(continueFromNumber: Boolean = false;
        IsOffset: Boolean = False): TBytes;
      procedure ParsePair;
      function ParseValue: TJSONValue;
      function ParseKey: TArray<string>;

      function ReadDigits(digits: integer; decimals: Boolean = False): string;
    protected
      procedure ParseToken; override;
      procedure UnknownCharacter(out cont: Boolean); override;
      function ReadWord: TScanner.TIdentifier; override;
      function ReadNumber: string; override;
      function GetException: EScannerClass; override;
    public
      constructor Create(Bytes: TBytes); override;
      destructor Destroy; override;
      procedure Parse; override;
      class function TOMLToJson(Contents: TBytes): TJSONObject;
  end;


implementation

uses
  System.Character,
  System.StrUtils;

resourcestring
  rsCannotRedefineTable = 'Cannot redefine a table';
  rsCannotRedefineKey = 'Cannot redefine an existing key';
  rsInvalidUtf8 = 'Invalid utf8 input';
  rsCannotExtendImmutableValues = 'Cannot extend immutable values';
  rsInvalidStringCharacter = 'Invalid character "#%d in string';
  rsInvalidLineBreak = 'Invalid line break: CR without LF';
  rsInvalidUnicodeCharacter = 'Unicode characters need to be unicode scalars';
  rsInvalidEscapeChar = 'Invalid string escape char: "%s"';
  rsInvalidMultilineString = 'Invalid multiline string';
  rsSingleLineNoBreaks = 'Single line strings must not contain line breaks (' +
  '#%d)';
  rsInputTerminationError = 'Input termination error';
  rsCannotExtendStaticArrays = 'Cannot extend static table arrays';
  rsTableArrayHeaders = 'Table array headers must end with "]]"';
  rsTableArrayHeadersSingleLine = 'Table array headers must be on a single l' +
  'ine';
  rsTableArrayHeadersEOL = 'Table array headers must finish with EOL';
  rsInlineTablesNoTrailingComma = 'Inline tables do not allow trailing comma' +
  's.';
  rsNegativeBoolInvalid = 'Negative booleans are invalid';
  rsInvalidBinaryDigit = 'Invalid binary digit: "%s"';
  rsInvalidOctalDigit = 'Invalid octal digit "%s" in string "%s"';
  rsNumbersWithLeadingZeros = 'Numbers with leading zeros not allowed';
  rsSignedHexNumbers = 'Hex numbers should not have a sign';
  rsSignedOctalNumbers = 'Octal numbers should not have a sign';
  rsSignedBinaryNumbers = 'Binary numbers should not have a sign';
  rsInvalidValue = 'Invalid value "%s"';
  rsUnexpectedToken = 'Unexpected token "%s"';
  rsKeyValueSameLine = 'Key and Value must be on the same line in key-value ' +
  'pairs';
  rsDotSurroundedByDigits = 'In numbers the dot needs to be surrounded by at' +
  ' least one digit on each side';
  rsInvalidTime = 'Invalid time';
  rsInvalidDate = 'Invalid date';
  rsWrongNumberOfDigits = 'Expected %d digits but got %d';
  rsInvalidPlusMinus = 'Invalid +/- sequence';
  rsUnderscoreSurroundedByDigits = 'Each underscore must be surrounded by at' +
  ' least one digit on each side';
  rsInvalidHexNumber = 'Invalid hexadecimal number';
  rsIncompleteOctalNumber = 'Incomplete octal number';
  rsIncompleteBinaryNumber = 'Incomplete binary number';
  rsIncompleteHexNumber = 'Incomplete hexadecimal number';
  rsKeyValuePairsEOL = 'Key-value pairs must finish with EOL';
  rsExponentFolllowedByInteger = 'Exponent must be followed by an integer';
  rsInvalidCharacterInComment = 'Invalid character "#%d in comment';

type
  ETOML = class(EScanner);

class function TTOMLParser.TOMLToJson(Contents: TBytes): TJSONObject;
var
  Parser: TTOMLParser;
begin
  Parser := TTOMLParser.Create(Contents);
  try
    // check whether the input is valid utf8
    if not TEncoding.UTF8.IsBufferValid(Contents) then
      Parser.ParserError(rsInvalidUtf8);
    Parser.Parse;
    Result := Parser.document;
  finally
    Parser.Free;
  end;
end;

{ TTOMLParser }

function TTOMLParser.GetException: EScannerClass;
begin
  result := ETOML;
end;

function TTOMLParser.GetOrCreateTable(AParent: TJSONObject; Keys:
    TArray<string>; IsPairKey: Boolean = False): TJSONObject;
var
  Table: TJSONValue;
begin
  while Length(keys) > 0 do
  begin
    Table := AParent.Values[Keys[0]];

    if FImmutableList.Contains(Table) then
      ParserError(rsCannotExtendImmutableValues);

    if Table is TJSONArray then
    begin
      if TJSonArray(Table)[TJSonArray(Table).Count - 1] is TJSONObject then
        Table := TJSonArray(Table)[TJSonArray(Table).Count - 1]
      else
        ParserError(rsCannotRedefineKey);
    end
    else if Assigned(Table) then
    begin
      if not (Table is TJSONObject) or
        (IsPairKey and FDefinedTables.Contains(TJSONObject(Table)))
      then
        ParserError(rsCannotRedefineKey);
    end
    else if not Assigned(Table) then
    begin
      Table := TJSONObject.Create;
      if IsPairKey then
        FCreatedInline.Add(TJSONObject(Table));
      AddPair(AParent, keys[0], Table);
    end;
    AParent := TJSONObject(Table);
    Delete(Keys, 0, 1);
  end;
  Result := AParent;
end;

function TTOMLParser.ParseString(AllowMultiline: Boolean = True): string;
var
  quote: AnsiChar;
  scalar: Cardinal;
  multiline,
  literal,
  firstPass: Boolean;
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
      if (c in CharSetIllegalStr) and not (multiline and IsLineEnding) then
        ParserError(Format(rsInvalidStringCharacter, [Ord(c)]));

      // EOL validation
      if c = #13 then
      begin
        AdvancePattern;
        if c <>  #10 then
          ParserError(rsInvalidLineBreak);
        AdvancePattern;
        Continue;
      end;

      { Multi-line basic strings are surrounded by three quotation marks on each side
      and allow newlines. A newline immediately following the opening delimiter will be trimmed.
      All other whitespace and newline characters remain intact. }
      if firstPass and (c = quote) then
        begin
          ReadChar;
          firstPass := False;
          if c = quote then
            begin
              if not AllowMultiline then
                ParserError(rsInvalidMultilineString);
              multiline := true;
              // trim first new line
              if Peek(sLineBreak, 1) then
                Advance(Length(sLineBreak));
              Continue;
            end
          else // the string is empty so return now
            begin
              result := TEncoding.UTF8.GetString(pattern);
              ReadToken;
              Exit;
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
                    ParserError(rsInvalidUnicodeCharacter);
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
              ParserError(Format(rsInvalidEscapeChar, [c]));
            end
        end
      // line breaks are not allowed
      else if not multiline and IsLineEnding then
        ParserError(Format(rsSingleLineNoBreaks, [Ord(c).ToString]))
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
                Exit;
              end
              else if Peek(quote+quote+quote+quote) then
              begin
                // interpret the first as a quote inside the string
                pattern := pattern + [Ord(c)];
                result := TEncoding.UTF8.GetString(pattern);
                ReadTo(4);
                Exit;
              end
              // end of string
              else if Peek(quote+quote+quote) then
              begin
                result := TEncoding.UTF8.GetString(pattern);
                ReadTo(3);
                Exit;
              end
              else
                pattern := pattern + [Ord(c)];
            end
          else
            begin
              result := TEncoding.UTF8.GetString(pattern);
              ReadTo(1);
              Exit;
            end;
        end;
    end;
  Assert(False, rsInputTerminationError);
end;

procedure TTOMLParser.ParseArrayOfTables;
var
  ParentTable: TJSONObject;
  NewTable: TJSONObject;
  Keys: TArray<string>;
  Arr: TJSONArray;
  Value: TJSONValue;
  StartLine: Integer;
begin
  StartLine := fileInfo.line;

  Consume(TToken.SquareBracketOpen);
  Consume(TToken.SquareBracketOpen);
  Keys := ParseKey;

  FDefinedTables.Add(TableStack.Peek);
  FDefinedTables.AddRange(FCreatedInline.ToArray);
  FCreatedInline.Clear;

  TableStack.Pop;

  Value := FindValue(TableStack.Peek, Keys);
  if (Value <> nil) and not (Value is TJSONArray) then
    ParserError(rsCannotRedefineKey);
  if FImmutableList.Contains(Value) then
    ParserError(rsCannotExtendStaticArrays);

  if Value is TJSONArray then
    Arr := TJSONArray(Value)
  else
  begin
    // Create the parent table if needed and then create the array.
    ParentTable := GetOrCreateTable(TableStack.Peek,
      Copy(Keys, 0, Length(Keys) - 1));
    Arr :=  TJSONArray.Create;
    AddPair(ParentTable, Keys[Length(Keys) - 1], Arr);
  end;
  NewTable := TJSONObject.Create;
  Arr.Add(NewTable);

  // push the new table
  TableStack.Push(NewTable);

  if c <>  ']' then
    ParserError(rsTableArrayHeaders);

  Consume(TToken.SquareBracketClosed);

  if (fileInfo.Line > StartLine) then
    ParserError(rsTableArrayHeadersSingleLine);

  Consume(TToken.SquareBracketClosed);

  if (token <> TToken.EOF) and (fileInfo.Line = StartLine) then
    ParserError(rsTableArrayHeadersEOL);
end;

procedure TTOMLParser.ParseTable;
var
  Keys: TArray<string>;
  ParentTable: TJSONObject;
  NewTable: TJSONObject;
  Value: TJSONValue;
  StartLine: Integer;
begin
  StartLine := fileInfo.line;

  if c = '[' then
  begin
    ParseArrayOfTables;
    Exit;
  end;

  Consume(TToken.SquareBracketOpen);
  Keys := ParseKey;

  FDefinedTables.Add(TableStack.Peek);
  FDefinedTables.AddRange(FCreatedInline.ToArray);
  FCreatedInline.Clear;

  TableStack.Pop;

  Value := FindValue(TableStack.Peek, Keys);
  if Value <> nil then
  begin
    if Value is TJSONObject then
    begin
      // defining a super table
      NewTable := TJSONObject(Value);
      if FDefinedTables.Contains(NewTable) then
        ParserError(rsCannotRedefineTable);
    end
    else
    begin
      ParserError(rsCannotRedefineKey);
      Exit; // To avoid warning about NewTable not initialized
    end;
  end
  else
  begin
    // Create the parent table if needed and then create this table
    ParentTable := GetOrCreateTable(TableStack.Peek,
      Copy(Keys, 0, Length(Keys) - 1));
    if Assigned(ParentTable.Values[Keys[Length(Keys) - 1]]) then
      ParserError(rsCannotRedefineKey);
    NewTable :=  TJSONObject.Create;
    AddPair(ParentTable, Keys[Length(Keys) - 1], NewTable);
  end;

  // push table
  TableStack.Push(NewTable);

  if (fileInfo.Line > StartLine) then
    ParserError(rsTableArrayHeadersSingleLine);

  Consume(TToken.SquareBracketClosed);

  if (token <> TToken.EOF) and (fileInfo.Line = StartLine) then
    ParserError(rsTableArrayHeadersEOL);
end;

function TTOMLParser.ParseInlineTable: TJSONObject;
begin
  // inline tables don't allow newlines so we can override the newline behavior
  // of the scanner by enabling newlines as tokens
  readLineEndingsAsTokens := true;

  Consume(TToken.CurlyBracketOpen);

  // push new table to stack
  Result := TJSONObject.Create;

  TableStack.Push(Result);
  try
    while not TryConsume(TToken.CurlyBracketClosed) do
    begin
      ParsePair;

      if TryConsume(TToken.Comma) then
      begin
        // curly bracket found for pair
        if TryConsume(TToken.CurlyBracketClosed) then
          ParserError(rsInlineTablesNoTrailingComma);
        Continue;
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
    Result.Free;
    raise;
  end;

  FImmutableList.Add(Result);
  FDefinedTables.Add(Result);
  FDefinedTables.AddRange(FCreatedInline.ToArray);
  FCreatedInline.Clear;
  TableStack.Pop;
end;

function TTOMLParser.ParseArray: TJSONArray;
var
  Value: TJSONValue;
  oldreadLineEndingsAsTokens: Boolean;
begin
  oldreadLineEndingsAsTokens := readLineEndingsAsTokens;
  readLineEndingsAsTokens := False;

  Consume(TToken.SquareBracketOpen);
  Result := TJSONArray.Create;

  try
    while not TryConsume(TToken.SquareBracketClosed) do
    begin
      Value := ParseValue;
      Result.AddElement(Value);
      if not TryConsume(TToken.Comma) then
      begin
        Consume(TToken.SquareBracketClosed);
        Break;
      end;
    end;
    readLineEndingsAsTokens := oldreadLineEndingsAsTokens;
  except
    Result.Free;
    raise;
  end;

  FImmutableList.Add(Result);
end;

function TTOMLParser.ParseValue: TJSONValue;

  function ParseNamedValue(Negative: Boolean = False): TJSONValue;
  var
    valueString: string;
  begin
    Result := nil;
    valueString := TEncoding.Utf8.GetString(pattern);
    if (valueString = 'false') or (valueString = 'true') then
      begin
        if Negative then
          ParserError(rsNegativeBoolInvalid);
        Result := TJSONBool.Create(StrToBool(valueString));
        Consume;
      end
    else if valueString = 'inf' then
      begin
        if Negative then
          Result := TJSONNumber.Create('-inf')
        else
          Result := TJSONNumber.Create('inf');
        Consume;
      end
    else if valueString = 'nan' then
      begin
          Result := TJSONNumber.Create('nan');
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
        raise EConvertError.Create(Format(rsInvalidBinaryDigit, [S[i]]));
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
          raise EConvertError.CreateFmt(rsInvalidOctalDigit, [S[i], S]);
      end;
    end;
  end;

var
  negative: Boolean;
  str: string;
begin
  Result := nil;

  case token of
    TToken.DoubleQuote:
      result := TJSONString.Create(ParseString);
    TToken.SingleQuote:
      result := TJSONString.Create(ParseString);
    TToken.Integer:
      begin
        // the integer is a possible date so switch parsers
        if (Length(pattern) = 4) and (c = '-') then
          begin
            result := TJSONString.Create(TEncoding.UTF8.GetString(ParseDate(True)));
            Consume;
          end
        else if (Length(pattern) = 2) and (c = ':') then
          begin
            result := TJSONString.Create(TEncoding.UTF8.GetString(ParseTime(True)));
            Consume;
          end
        else
          begin
            if ((Length(pattern) > 1)  and (pattern[0] = Ord('0'))) or
              ((Length(pattern) > 2)  and (pattern[0] in [Ord('+'), Ord('-')]) and
              (pattern[1] = Ord('0')))
            then
              ParserError(rsNumbersWithLeadingZeros);

            str := TEncoding.UTF8.GetString(pattern);
            result := TJSONNumber.Create(str);
            try
              Consume;
            except
              result.Free;
              raise;
            end;
          end;
      end;
    TToken.HexadecimalNumber:
      begin
        if pattern[0] in [Ord('+'), Ord('-')] then
          ParserError(rsSignedHexNumbers);
        str := TEncoding.UTF8.GetString(pattern);
        result := TJSONNumber.Create(StrToInt64(str));
        Consume;
      end;
    TToken.OctalNumber:
      begin
        if pattern[0] in [Ord('+'), Ord('-')] then
          ParserError(rsSignedOctalNumbers);
        result := TJSONNumber.Create(OctalToInt(TEncoding.UTF8.GetString(pattern)));
        Consume;
      end;
    TToken.BinaryNumber:
      begin
        if pattern[0] in [Ord('+'), Ord('-')] then
          ParserError(rsSignedBinaryNumbers);
        result := TJSONNumber.Create(BinToInt(TEncoding.UTF8.GetString(pattern)));
        Consume;
      end;
    TToken.RealNumber:
      begin
        if ((Length(pattern) > 1)  and (pattern[0] = Ord('0')) and (pattern[1] in [Ord('0')..Ord('9')])) or
          ((Length(pattern) > 2)  and (pattern[0] in [Ord('+'), Ord('-')]) and
          (pattern[1] = Ord('0')) and (pattern[2] in [Ord('0')..Ord('9')]))
        then
          ParserError(rsNumbersWithLeadingZeros);

        str := TEncoding.UTF8.GetString(pattern);
        result := TJSONNumber.Create(str);
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
          ParserError(Format(rsInvalidValue, [TEncoding.UTF8.GetString(pattern)]));
      end;
    else
      ParserError(Format(rsUnexpectedToken, [token.ToString]))
  end;
  Assert(result <> nil, Format(rsUnexpectedToken, [token.ToString]));
end;

function TTOMLParser.ParseKey: TArray<string>;
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
            Consume(CharSetBareKey);
          result := result + [TEncoding.UTF8.GetString(pattern)];
          Consume;
        end
      else if token = TToken.RealNumber then  // e.g. 1.2   = true
        begin
          if c in ['a'..'z','A'..'Z','_','-'] then
            Consume(CharSetBareKey);
          result := result + TEncoding.UTF8.GetString(pattern).Split(['.']);
          Consume;
        end
      else if token = TToken.Dash then
        begin
          if c in ['a'..'z','A'..'Z','_','-'] then
            Consume(CharSetBareKey);
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

procedure TTOMLParser.ParsePair;
var
  Keys: TArray<string>;
  ParentTable: TJSONObject;
  Value: TJSONValue;
  StartLine: Integer;
begin
  StartLine := fileInfo.line;

  Keys := ParseKey;
  Consume(TToken.Equals);

  if fileInfo.line > StartLine then
    ParserError(rsKeyValueSameLine);

  Value := FindValue(TableStack.Peek, Keys);
  if Value <> nil then
    ParserError(rsCannotRedefineKey);

  // add dotted keys as tables
  ParentTable := GetOrCreateTable(TableStack.Peek,
    Copy(Keys, 0, Length(Keys) - 1), True);

  if FDefinedTables.Contains(ParentTable) then
    ParserError(rsCannotRedefineTable);

  Value := ParseValue;

  AddPair(ParentTable, keys[Length(keys) - 1], Value);
end;

function TTOMLParser.ReadDigits(digits: integer; decimals: Boolean = False): string;
var
  Len: Integer;
begin
  Len := Length(Pattern);
  while Length(pattern) < Len + digits do
    begin
      if not (c in ['0'..'9']) then
        ParserError(Format(rsWrongNumberOfDigits, [digits, Length(pattern)]));
      AdvancePattern;
    end;

  // decimal part at the end
  if decimals and (c = '.') then
    begin
      AdvancePattern;
      if not (c in ['0'..'9']) then
        ParserError(rsDotSurroundedByDigits);
      while c in ['0'..'9'] do
        AdvancePattern;
    end;

  result := TEncoding.UTF8.GetString(Copy(pattern, Len));
end;

function TTOMLParser.ParseTime(continueFromNumber: Boolean = false; IsOffset:
    Boolean = False): TBytes;
var
  Hours, Minutes: Integer;
  Seconds: double;
begin
  // hours
  // the parsing is being continued from a number
  // so the hour is already in the pattern buffer
  if continueFromNumber then
    Hours := StrToInt(TEncoding.UTF8.GetString(pattern))
  else
    Hours := StrToInt(ReadDigits(2));
  Consume(':');

  // minutes
  Minutes := StrToInt(ReadDigits(2));

  // seconds only if it is not an offset
  if not IsOffset then
    begin
      Consume(':');
      Seconds := StrToFloat(ReadDigits(2, true));
    end
  else
    Seconds := 0;

  if (Hours >= HoursPerDay) or (Minutes >= MinsPerHour) or
    (Seconds >= SecsPerMin)
  then
    ParserError(rsInvalidTime);

  Result := Copy(pattern);
end;

function TTOMLParser.ParseDate(continueFromNumber: Boolean): TBytes;
var
  Year, Month, Day: Integer;
  HasTime: Boolean;
  LDate: TDateTime;
begin
  // the parsing is being continued from a number
  // so the year is already in the pattern buffer
  if continueFromNumber then
    Year := StrToInt(TEncoding.UTF8.GetString(pattern))
  else
    Year := StrToInt(ReadDigits(4));
  Consume('-');

  // month
  Month := StrToInt(ReadDigits(2));

  // day
  Consume('-');
  Day := StrToInt(ReadDigits(2));

  if not TryEncodeDate(Year, Month, Day, LDate) then
    ParserError(rsInvalidDate);

  // the date is a solo year
  if IsLineEnding or IsEOF then
    begin
      Result := Copy(pattern);
      Exit;
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
      pattern := pattern + [Ord('T')];
      // hour
      ParseTime;

      // zulu
      if UpCase(c) = 'Z' then
        AdvancePattern(1);

      // offset time
      if c in ['+', '-'] then
      begin
        AdvancePattern(1);;
        ParseTime(False, True);
      end;
    end;
  end;

  result := Copy(pattern);
end;

function TTOMLParser.ReadNumber: string;

  function LastChar: AnsiChar;
  begin
    if Length(pattern) = 0 then
      result := #0
    else
      result := AnsiChar(pattern[high(pattern)]);
  end;

var
  negative: Boolean;
  underscore: Boolean;
  found: Boolean;
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
   ParserError(rsInvalidPlusMinus);


  while c in ['0'..'9', '.', 'e', 'E', '_'] do
    begin
      if c = '_' then
        begin
          if ((length(pattern) = 0) or not (pattern[length(pattern) - 1] in [Ord('0')..Ord('9')])) or
            not (Peek(1) in ['0'..'9']) or underscore
          then
            ParserError(rsDotSurroundedByDigits);
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
            ParserError(rsUnderscoreSurroundedByDigits);
          continue;
        end;

      // parse binary
      if (c = '0') and (Peek(1) = 'b') then
        begin
          token := TToken.BinaryNumber;
          Advance(2);
          if c = '_' then
            ParserError(rsUnderscoreSurroundedByDigits);
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
                        ParserError(rsUnderscoreSurroundedByDigits);
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
                    ParserError(rsInvalidHexNumber);
                end;
            end;
          goto Finished;
        end;

      if Char(c).ToLower = 'e' then
        begin
          if underscore then
            ParserError(rsUnderscoreSurroundedByDigits);
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
                    ParserError(rsUnderscoreSurroundedByDigits);
                  ReadChar;
                  underscore := true;
                  continue;
                end;
              AdvancePattern;
              underscore := false;
            end;
          if not found then
            ParserError(rsExponentFolllowedByInteger);
          break;
        end
      else if c = '.' then
      begin
        if ((length(pattern) = 0) or not (pattern[length(pattern) - 1] in [Ord('0')..Ord('9')]))  or
          not (Peek(1) in ['0'..'9'])
        then
          ParserError(rsDotSurroundedByDigits);
        token := TToken.RealNumber;
      end;

      AdvancePattern;
    end;

  Finished:

  if underscore then
    ParserError(rsUnderscoreSurroundedByDigits);

  // incomplete prefixed number
  if Length(pattern) = 0 then
    case token of
      TToken.OctalNumber:
        ParserError(rsIncompleteOctalNumber);
      TToken.BinaryNumber:
        ParserError(rsIncompleteBinaryNumber);
    end
    else if (Length(pattern) = 2) and (token = TToken.HexadecimalNumber) then
      ParserError(rsIncompleteHexNumber);

  result := TEncoding.UTF8.GetString(pattern);
end;

function TTOMLParser.ReadWord: TScanner.TIdentifier;
begin
  pattern := [];
  // TODO: words must start with alphas and quotes are allowed
  // dashes must have be adjacent to at least 1 alpha
  while c in CharSetBareKey do
    AdvancePattern;
  result := TEncoding.UTF8.GetString(pattern);
end;

procedure TTOMLParser.UnknownCharacter(out cont: Boolean);
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
            ParserError(Format(rsInvalidCharacterInComment, [Ord(c)]));
        until IsLineEnding or IsEOF;
        cont := true;
      end;
    else
      inherited;
  end;
end;

procedure TTOMLParser.ParseToken;
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
          ParserError(rsKeyValuePairsEOL);
      end
    else
      if token <> TToken.EOF then
        ParserError(Format(rsUnexpectedToken, [token.ToString]))
  end;
end;

procedure TTOMLParser.Parse;
begin
  document := TJSONObject.Create();
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

procedure TTOMLParser.AddPair(Table: TJSONObject; Key: string;
  Value: TJsonValue);
// To allow empty keys
begin
  Table.AddPair(TJSONPair.Create(Key, Value));
end;

constructor TTOMLParser.Create(Bytes: TBytes);
begin
  inherited;
  FDefinedTables := TList<TJSONObject>.Create;
  FCreatedInline := TList<TJSONObject>.Create;
  TableStack := TStack<TJsonObject>.Create;
  FImmutableList := TList<TJSONValue>.Create;
end;

destructor TTOMLParser.Destroy;
begin
  TableStack.Free;
  FDefinedTables.Free;
  FCreatedInline.Free;
  FImmutableList.Free;
  inherited;
end;

function TTOMLParser.FindValue(Table: TJSONObject;
  Keys: TArray<string>): TJSONValue;
// Deals with empty keys
var
  Key: string;
  Arr: TJsonArray;
begin
  Result := Table;
  for Key in Keys do
  begin
    if Result is TJSONObject  then
      Result := TJSONObject(Result).Values[Key]
    else if Result is TJSONArray  then
    begin
      Arr := TJSONArray(Result);
      if (Arr.Count > 0) and (Arr[Arr.Count - 1] is TJSONObject) then
        Result := TJSONObject(Arr[Arr.Count - 1]).Values[Key]
      else
        Result := nil;
    end
    else
      Result := nil;

    if Result = nil then
      Exit;
  end;
end;

end.
