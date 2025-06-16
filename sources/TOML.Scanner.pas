{
    Copyright (c) 2020 by Ryan Joseph

    This unit implements a basic tokenizer

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

unit TOML.Scanner;

interface

uses
  System.SysUtils;

type
  EScanner = class(Exception);

  EScannerClass = class of EScanner;

  TScanner = class
    protected type
      TFileInfo = record
        line: integer;
        column: integer;
      end;
    protected
      currentIndex: integer;
      fileInfo: TFileInfo;
      consumedLineEnding: boolean;
    protected type
      {$scopedenums on}
      TIdentifier = string;
      TToken = (Unknown,
                // patterns
                ID,
                Integer,
                RealNumber,
                HexadecimalNumber,
                OctalNumber,
                BinaryNumber,
                // symbols
                DoubleQuote,
                SingleQuote,
                SquareBracketOpen,
                SquareBracketClosed,
                CurlyBracketOpen,
                CurlyBracketClosed,
                Equals,
                Comma,
                BackSlash,
                Dash,
                Dot,
                Plus,
                EOL,
                EOF
                );
      {$scopedenums off}
    protected
      contents: TBytes;
      pattern: TBytes;
      token: TToken;
      c: AnsiChar;
      readLineEndingsAsTokens: boolean;
    protected
      procedure Consume; overload; inline;
      procedure Consume(t: TToken); overload;
      procedure Consume(inChar: AnsiChar); overload;
      procedure Consume(charset: TSysCharSet); overload;
      function TryConsume(t: TToken): boolean; overload;
      function TryConsume(t: TToken; out s: string): boolean; overload; inline;

      function ReadToken: TToken;
      function ReadTo(count: integer): TToken;
      function ReadChar: AnsiChar;
      function ReadWord: TIdentifier; virtual;
      function ReadNumber: string; virtual;

      procedure ReadUntilEOL;
      procedure SkipSpace;

      function IsLineEnding: boolean;
      function IsEOF: Boolean;
      function IsWhiteSpace: boolean;
      function WhiteSpaceTillEOL: boolean;

      procedure Advance(count: integer);
      procedure AdvancePattern(count: integer = 1);

       function Peek(offset: integer = 1): AnsiChar; overload;
      function Peek(str: AnsiString; offset: integer = 0): boolean; overload;
      function PeekString(count: integer): AnsiString;

      { Errors }
      procedure ParserError (messageString: string = '');
      function GetException: EScannerClass; virtual;

      { Handlers }
      procedure ParseToken; virtual; abstract;
      procedure UnknownCharacter(out cont: boolean); virtual;
    public
      constructor Create(Bytes: TBytes); virtual;
      procedure Parse; virtual;
  end;

type
  TTokenMethods = record helper for TScanner.TToken
    function ToString: string;
  end;

implementation

resourcestring
  rsParseError = 'Error at %d:%d: %s';
  rsUnexpectedCharacter = 'Unexpected character "%s"';
  rsInvalidToken = 'Invalid token';
  rsExpectedGot = 'Expected "%s", got "%s"';
  rsUnknownCharacter = 'Unknown character "%s"';
  rsInvalidLineBreak = 'Invalid line break: CR without LF';

const
  TCharSetLineEnding = [#10, #13];
  TCharSetWhiteSpace = [#32, #9];
  TCharSetWord =       ['a'..'z','A'..'Z','_','-'];
  TCharSetInteger =    ['0'..'9'];
  TCharSetQuotes =     ['"', ''''];

function TTokenMethods.ToString: string;
begin
  case self of
    TScanner.TToken.ID:
      result := 'ID';
    TScanner.TToken.Integer:
      result := 'Integer';
    TScanner.TToken.RealNumber:
      result := 'Real';
    TScanner.TToken.DoubleQuote:
      result := '"';
    TScanner.TToken.SingleQuote:
      result := '''';
    TScanner.TToken.SquareBracketOpen:
      result := '[';
    TScanner.TToken.SquareBracketClosed:
      result := ']';
    TScanner.TToken.CurlyBracketOpen:
      result := '{';
    TScanner.TToken.CurlyBracketClosed:
      result := '}';
    TScanner.TToken.Equals:
      result := '=';
    TScanner.TToken.Comma:
      result := ',';
    TScanner.TToken.BackSlash:
      result := '\';
    TScanner.TToken.Dash:
      result := '-';
    TScanner.TToken.Dot:
      result := '.';
    TScanner.TToken.Plus:
      result := '+';
    TScanner.TToken.EOF:
      result := 'End of File';
    TScanner.TToken.EOL:
      result := 'End of Line';
    else
      raise EScanner.Create(rsInvalidToken);
  end;
end;

procedure TScanner.Consume;
begin
  Consume(token);
end;

procedure TScanner.Consume(t: TToken);
begin
  if token = t then
    ReadToken
  else
    ParserError(Format(rsExpectedGot, [t.ToString, token.ToString]));
end;

procedure TScanner.Consume(inChar: AnsiChar);
begin
  if c = inChar then
    AdvancePattern
  else
    ParserError(Format(rsExpectedGot,  [Char(inChar), Char(c)]));
end;

procedure TScanner.Consume(charset: TSysCharSet);
begin
  if not (c in charset) then
    ParserError(Format(rsUnexpectedCharacter, [c]))
  else
    while c in charset do
      AdvancePattern;
end;

function TScanner.TryConsume(t: TToken; out s: string): boolean;
begin
  s := TEncoding.UTF8.GetString(pattern);
  result := TryConsume(t);
end;

function TScanner.TryConsume(t: TToken): boolean;
begin
  if token = t then
    begin
      ReadToken;
      result := true;
    end
  else
    result := false;
end;

procedure TScanner.ReadUntilEOL;
begin
  repeat
    ReadChar;
  until IsLineEnding or IsEOF;
end;

procedure TScanner.SkipSpace;
begin
  consumedLineEnding := false;
  while True do
  begin
    if c in TCharSetWhiteSpace then
      ReadChar
    else if c in TCharSetLineEnding then
    begin
      if c = #13 then
      begin
        ReadChar;
        if c <>  #10 then
          ParserError(rsInvalidLineBreak);
      end;
      consumedLineEnding := true;
      ReadChar;
    end
    else
      Break;
  end;
end;

function TScanner.IsEOF: Boolean;
begin
  Result := c = #0;
end;

function TScanner.IsLineEnding: boolean;
begin
  result := c in TCharSetLineEnding;
end;

function TScanner.IsWhiteSpace: boolean;
begin
  result := (c in TCharSetWhiteSpace) or IsLineEnding;
end;

function TScanner.Peek(offset: integer = 1): AnsiChar;
begin
  if currentIndex + offset < length(contents) then
    result := AnsiChar(contents[currentIndex + offset])
  else
    result := #0;
end;

function TScanner.Peek(str: AnsiString; offset: integer = 0): boolean;
var
  i, contentsOffset: integer;
begin
  result := false;
  for i := 0 to length(str) - 1 do
    begin
      contentsOffset := currentIndex + offset + i;
      if (contentsOffset < length(contents)) and (contents[contentsOffset] = Ord(str[i + 1])) then
        result := true
      else
        exit(false);
    end;
end;

function TScanner.PeekString(count: integer): AnsiString;
var
  i: integer;
begin
  result := '';
  for i := 0 to count - 1 do
    result := result + AnsiChar(contents[currentIndex + i]);
end;

function TScanner.ReadChar: AnsiChar;
begin
  if IsLineEnding then
    begin
      Inc(fileInfo.line);
      fileInfo.column := 0;
    end;
  Inc(currentIndex);
  c := AnsiChar(contents[currentIndex]);
  Inc(fileInfo.column);
  result := c;
end;

function TScanner.ReadWord: TIdentifier;
begin
  pattern := [];
  while c in TCharSetWord + TCharSetInteger do
    begin
      pattern := pattern + [Ord(c)];
      ReadChar;
    end;
  result := TEncoding.UTF8.GetString(pattern);
end;


{ Appends character the current pattern and reads next character }
procedure TScanner.AdvancePattern(count: integer = 1);
begin
  while count > 0 do
    begin
      pattern := pattern + [Ord(c)];
      ReadChar;
      Dec(count);
    end;
end;

{ Moves the scanner by 'count' characters }
procedure TScanner.Advance(count: integer);
begin
  while count > 0 do
    begin
      ReadChar;
      Dec(count);
    end;
end;

{ Advances by "count" and reads token at new position }
function TScanner.ReadTo(count: integer): TToken;
begin
  Advance(count);
  result := ReadToken;
end;


function TScanner.ReadNumber: string;
begin
  pattern := [];
  token := TToken.Integer;

  if c = '-' then
    AdvancePattern;

  if c = '+' then
    AdvancePattern;

  while c in TCharSetInteger + ['.', 'e'] do
    begin
      // TODO: must be followed by a number!
      if c = 'e' then
        begin
          AdvancePattern;
          if c = '-' then
            begin
              AdvancePattern;
              while c in TCharSetInteger do
                AdvancePattern;
               break;
            end;
        end
      else if c = '.' then
        token := TToken.RealNumber;
      AdvancePattern;
    end;

  result := TEncoding.UTF8.GetString(pattern);
end;

function TScanner.GetException: EScannerClass;
begin
  result := EScanner;
end;

procedure TScanner.ParserError(messageString: string = '');
begin
  // in case we pass in line ending from the current character "c"
  // replace these with something human readable
  messageString := StringReplace(messageString, #10, 'EOL', []);
  messageString := StringReplace(messageString, #12, 'EOL', []);
  messageString := StringReplace(messageString, #13, 'EOL', []);
  raise GetException.Create(Format(rsParseError,
    [fileInfo.line, fileInfo.column, messageString]));
end;

procedure TScanner.UnknownCharacter(out cont: boolean);
begin
  ParserError(Format(rsUnknownCharacter, [c]));
end;

function TScanner.WhiteSpaceTillEOL: boolean;
var
  ch: AnsiChar;
  Index: Integer;
begin
  Result := True;
  ch := c;
  Index := 1;

  while not (ch in TCharSetLineEnding + [#0])  do
  begin
    if not (ch in TCharSetWhiteSpace) then
      Exit(False);
    ch := Peek(Index);
    Inc(Index);
  end;
end;

function TScanner.ReadToken: TToken;
var
  cont: boolean;
begin
  token := TToken.EOF;

  while (currentIndex < length(contents) - 1) and (token = TToken.EOF) do
    begin
      //writeln('  ', currentIndex, ':', c);
      case c of
        '+':
          begin
            if Peek in TCharSetInteger then
              ReadNumber
            else
              begin
                token := TToken.Plus;
                ReadChar;
              end;
          end;
        '-':
          begin
            if Peek in TCharSetInteger then
              ReadNumber
            else
              begin
                token := TToken.Dash;
                ReadChar;
              end;
          end;
        '0'..'9':
          ReadNumber;
        'a'..'z','A'..'Z','_':
          begin
            token := TToken.ID;
            ReadWord;
          end;
        '[':
          begin
            token := TToken.SquareBracketOpen;
            ReadChar;
          end;
        ']':
          begin
            token := TToken.SquareBracketClosed;
            ReadChar;
          end;
        '{':
          begin
            token := TToken.CurlyBracketOpen;
            ReadChar;
          end;
        '}':
          begin
            token := TToken.CurlyBracketClosed;
            ReadChar;
          end;
        '=':
          begin
            token := TToken.Equals;
            ReadChar;
          end;
        ',':
          begin
            token := TToken.Comma;
            ReadChar;
          end;
        '.':
          begin
            token := TToken.Dot;
            ReadChar;
          end;
        '\':
          begin
            token := TToken.BackSlash;
            ReadChar;
          end;
        #10, #13, #32, #9:
          begin
            SkipSpace;
            if consumedLineEnding and readLineEndingsAsTokens then
              begin
                token := TToken.EOL;
                consumedLineEnding := false;
              end;
          end;
        else
          begin
            cont := false;
            UnknownCharacter(cont);
            if cont then
              continue;
          end;
      end;
    end;

    result := token;
end;

procedure TScanner.Parse;
begin
  ReadToken;
  while token <> TToken.EOF do
    ParseToken;
end;

constructor TScanner.Create(Bytes: TBytes);
begin
  contents := Bytes + [0];
  currentIndex := 0;
  fileInfo.line := 1;
  fileInfo.column := 1;
  c := AnsiChar(contents[currentIndex]);
end;

end.
