unit TOML.Writer;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.DateUtils,
  System.JSON,
  System.Math,
  TOML.Support;

const
  MAX_LINE_LENGTH = 100;

type
  TTOMLWriter = class
  public
    class function ToTOML(Json: TJsonObject;
      MultilineStrings: Boolean = False; Indent: Integer = 4): string;
    class procedure WriteToStream(Json: TJsonObject; Stream: TStream;
      MultilineStrings: Boolean = False; Indent: Integer = 4);
    class procedure WriteToFile(Json: TJsonObject; const FName: string;
      MultilineStrings: Boolean = False; Indent: Integer = 4);
  end;

implementation

uses
  System.StrUtils;

type
  TTOMLWriterImpl = class
  private type
    TContext = record
      AllowMultiline: Boolean;
      IndentStr: string;
      InlineTableCache: TDictionary<TJSONObject, string>;
      constructor Create(AllowMultiline: Boolean; Indent: Integer);
    end;

    TTableEntry = record
      Key: string;
      Value: TJSONValue;
      IsArrayOfTables: Boolean;
      constructor Create(AKey: string; AValue: TJSONValue; AIsArrayOfTables: Boolean);
    end;
  private
    FCxt: TContext;
    FMultiLineStrings: Boolean;
    FIndent: Integer;
    // Helper functions
    class procedure GenTableChunks(Table: TJSONObject; const Ctx: TContext; Name: string; InsideAot: Boolean;
      TextWriter: TTextWriter); static;
    class function FormatLiteral(Obj: TJSONValue; const Ctx: TContext; NestLevel: Integer = 0): string; static;
    class function FormatKeyPart(const Part: string): string; static;
    class function FormatString(const S: string; AllowMultiline: Boolean): string; static;
    class function FormatInlineTable(Obj: TJSONObject; const Ctx: TContext): string; static;
    class function FormatInlineArray(Obj: TJSONArray; const Ctx: TContext; NestLevel: Integer): string; static;
    class function FormatDecimal(const Value: Extended): string; static;
    class function IsArrayOfTables(Obj: TJSONValue): Boolean; static;
    class function IsSuitableInlineTable(Obj: TJSONObject; const Ctx: TContext): Boolean; static;
    class function AllCharsInSet(const Str: string; CharSet: TSysCharSet): Boolean; static;
    class function EscapedChar(Chr: Char): string; static;
  public
    // Main serialization functions
    procedure StreamTOML(Obj: TJSONObject; Stream: TStream);
    function ToTOML(Obj: TJSONObject): string;
    constructor Create(MultilineStrings: Boolean = False; Indent: Integer = 4);
    destructor Destroy; override;
  end;

{ TContext }

constructor TTOMLWriterImpl.TContext.Create(AllowMultiline: Boolean; Indent: Integer);
begin
  if Indent < 0 then
    raise Exception.Create('Indent width must be non-negative');
  Self.AllowMultiline := AllowMultiline;
  Self.IndentStr := StringOfChar(' ', Indent);
  Self.InlineTableCache := TDictionary<TJSONObject, string>.Create;
end;


{ TTableEntry }

constructor TTOMLWriterImpl.TTableEntry.Create(AKey: string; AValue: TJSONValue; AIsArrayOfTables: Boolean);
begin
  Key := AKey;
  Value := AValue;
  IsArrayOfTables := AIsArrayOfTables;
end;

{ Helper Functions }

class function TTOMLWriterImpl.IsArrayOfTables(Obj: TJSONValue): Boolean;
var
  Arr: TJSONArray;
  I: Integer;
begin
  Result := False;
  if Obj is TJSONArray then
  begin
    Arr := TJSONArray(Obj);
    if Arr.Count > 0 then
    begin
      Result := True;
      for I := 0 to Arr.Count - 1 do
        if not (Arr.Items[I] is TJSONObject) then
          Exit(False);
    end;
  end;
end;

class function TTOMLWriterImpl.IsSuitableInlineTable(Obj: TJSONObject; const Ctx: TContext): Boolean;
var
  Rendered: string;
begin
  Rendered := Ctx.IndentStr + FormatInlineTable(Obj, Ctx) + ',';
  Result := (Length(Rendered) <= MAX_LINE_LENGTH) and (Pos(#10, Rendered) = 0);
end;

class function TTOMLWriterImpl.FormatKeyPart(const Part: string): string;
begin
  if (Part <> '') and AllCharsInSet(Part, CharSetBareKey) then
    Result := Part
  else
    Result := FormatString(Part, False);
end;

class function TTOMLWriterImpl.FormatString(const S: string; AllowMultiline: Boolean): string;
const
  CharSetEscapedChars = [#8, #10, #12, #13, '"', '\'];
var
  DoMultiline: Boolean;
  ResultBuilder: TStringBuilder;
  Idx, SeqStart: Integer;
  C: Char;
begin
  DoMultiline := AllowMultiline and (Pos(#10, S) > 0);
  ResultBuilder := TStringBuilder.Create;
  try
    if DoMultiline then
      ResultBuilder.Append('"""'#10)
    else
      ResultBuilder.Append('"');

    Idx := 1;
    SeqStart := 1;
    while Idx <= Length(S) do
    begin
      C := S[Idx];
      if CharInSet(C, CharSetIllegalStr + ['\']) then
      begin
        ResultBuilder.Append(Copy(S, SeqStart, Idx - SeqStart));
        if CharInSet(C, CharSetEscapedChars) then
        begin
          if DoMultiline and ((C = #10) or (C = #13)) then
            ResultBuilder.Append(C)
          else
            ResultBuilder.Append(EscapedChar(C));
        end
        else
          ResultBuilder.Append('\u' + IntToHex(Ord(C), 4));
        SeqStart := Idx + 1;
      end;
      Inc(Idx);
    end;
    ResultBuilder.Append(Copy(S, SeqStart, Idx - SeqStart));
    if DoMultiline then
      ResultBuilder.Append('"""')
    else
      ResultBuilder.Append('"');
    Result := ResultBuilder.ToString;
  finally
    ResultBuilder.Free;
  end;
end;

class function TTOMLWriterImpl.FormatDecimal(const Value: Extended): string;
begin
  if IsNaN(Value) then
    Result := 'nan'
  else if IsInfinite(Value) then
    if Value < 0 then
      Result := '-inf'
    else
      Result := 'inf'
  else
    Result := FloatToStr(Value).ToLower;
end;

class function TTOMLWriterImpl.FormatLiteral(Obj: TJSONValue; const Ctx: TContext; NestLevel: Integer): string;
begin
  if Obj is TJSONBool then
    Result := IfThen(TJSONBool(Obj).AsBoolean, 'true', 'false')
  else if Obj is TJSONNumber then
    Result := FormatDecimal(TJSONNumber(Obj).AsDouble)
  else if Obj is TJSONString then
    Result := FormatString(TJSONString(Obj).Value, Ctx.AllowMultiline)
  else if Obj is TJSONArray then
    Result := FormatInlineArray(TJSONArray(Obj), Ctx, NestLevel)
  else if Obj is TJSONObject then
    Result := FormatInlineTable(TJSONObject(Obj), Ctx)
  else
    raise Exception.CreateFmt('Object of type %s is not TOML serializable', [Obj.ClassName]);
end;

class function TTOMLWriterImpl.FormatInlineTable(Obj: TJSONObject; const Ctx: TContext): string;
var
  Pair: TJSONPair;
  Items: TStringList;
  I: Integer;
begin
  if Ctx.InlineTableCache.ContainsKey(Obj) then
    Exit(Ctx.InlineTableCache[Obj]);

  if Obj.Count = 0 then
    Result := '{}'
  else
  begin
    Items := TStringList.Create;
    try
      for I := 0 to Obj.Count - 1 do
      begin
        Pair := Obj.Pairs[I];
        Items.Add(FormatKeyPart(Pair.JsonString.Value) + ' = ' + FormatLiteral(Pair.JsonValue, Ctx));
      end;
      Result := '{ ' + String.Join(', ', Items.ToStringArray) + ' }';
    finally
      Items.Free;
    end;
  end;
  Ctx.InlineTableCache.AddOrSetValue(Obj, Result);
end;

class function TTOMLWriterImpl.FormatInlineArray(Obj: TJSONArray; const Ctx: TContext; NestLevel: Integer): string;
var
  ItemIndent, ClosingBracketIndent: string;
  I: Integer;
  Builder: TStringBuilder;
begin
  if Obj.Count = 0 then
    Exit('[]');

  ItemIndent := StringOfChar(' ', Length(Ctx.IndentStr) * (1 + NestLevel));
  ClosingBracketIndent := StringOfChar(' ', Length(Ctx.IndentStr) * NestLevel);
  Builder := TStringBuilder.Create;
  try
    Builder.Append('[').Append(SLineBreak);
    for I := 0 to Obj.Count - 1 do
    begin
      Builder.Append(ItemIndent).Append(FormatLiteral(Obj.Items[I], Ctx, NestLevel + 1));
      if I < Obj.Count - 1 then
        Builder.Append(',');
      Builder.Append(SLineBreak);
    end;
    Builder.Append(ClosingBracketIndent).Append(']');
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

class procedure TTOMLWriterImpl.GenTableChunks(Table: TJSONObject;
  const Ctx: TContext; Name: string; InsideAot: Boolean;
  TextWriter: TTextWriter);
var
  Yielded: Boolean;
  JustWroteHeader: Boolean;
  Pair: TJSONPair;
  Arr: TJSONArray;
  KeyPart, DisplayName: string;
  I, J: Integer;
  HasLiterals, HasArrayOfTables: Boolean;
begin
  Yielded := False;
  JustWroteHeader := False;
  HasLiterals := False;
  HasArrayOfTables := False;

  // Check if the table has literals or arrays of tables
  for I := 0 to Table.Count - 1 do
  begin
    Pair := Table.Pairs[I];
    if not (Pair.JsonValue is TJSONObject) and not IsArrayOfTables(Pair.JsonValue) then
      HasLiterals := True
    else if IsArrayOfTables(Pair.JsonValue) then
      HasArrayOfTables := True;
  end;

  // Write header for non-root tables with literals, arrays of tables, or empty tables
  if (InsideAot) or ((Name <> '') and (HasLiterals or HasArrayOfTables or (Table.Count = 0))) then
  begin
    if Yielded then
      TextWriter.WriteLine;
    Yielded := True;
    JustWroteHeader := True;
    if InsideAot then
      TextWriter.WriteLine('[[' + Name + ']]')
    else
      TextWriter.WriteLine('[' + Name + ']');
  end;

  // Process each pair in order
  for I := 0 to Table.Count - 1 do
  begin
    Pair := Table.Pairs[I];
    if Pair.JsonValue is TJSONNull then
      Continue;
    KeyPart := FormatKeyPart(Pair.JsonString.Value);
    if Name <> '' then
      DisplayName := Name + '.' + KeyPart
    else
      DisplayName := KeyPart;

    if Pair.JsonValue is TJSONObject then
    begin
      if I > 0 then
        TextWriter.WriteLine;
      GenTableChunks(TJSONObject(Pair.JsonValue), Ctx, DisplayName, False, TextWriter);
    end
    else if IsArrayOfTables(Pair.JsonValue) then
    begin
      if I > 0 then
        TextWriter.WriteLine;
      Arr := TJSONArray(Pair.JsonValue);
      for J := 0 to Arr.Count - 1 do
      begin
        if IsSuitableInlineTable(TJSONObject(Arr.Items[J]), Ctx) then
        begin
          if Yielded and not JustWroteHeader then
            TextWriter.WriteLine;
          Yielded := True;
          JustWroteHeader := False;
          TextWriter.WriteLine(KeyPart + ' = ' + FormatLiteral(Pair.JsonValue, Ctx));
          Break;
        end
        else
        begin
          if Yielded and not JustWroteHeader then
            TextWriter.WriteLine;
          Yielded := True;
          JustWroteHeader := False;
          GenTableChunks(TJSONObject(Arr.Items[J]), Ctx, DisplayName, True, TextWriter);
        end;
      end;
    end
    else
    begin
      Yielded := True;
      JustWroteHeader := False;
      TextWriter.WriteLine(KeyPart + ' = ' + FormatLiteral(Pair.JsonValue, Ctx));
    end;
  end;
end;

{ Main Serialization Functions }

procedure TTOMLWriterImpl.StreamTOML(Obj: TJSONObject; Stream: TStream);
var
  Writer: TStreamWriter;
begin
  FCxt := TContext.Create(FMultilineStrings, FIndent);

  Writer := TStreamWriter.Create(Stream);
  try
    GenTableChunks(Obj, FCxt, '', False, Writer);
  finally
    Writer.Free;
  end;
end;

function TTOMLWriterImpl.ToTOML(Obj: TJSONObject): string;
var
  Writer: TStringWriter;
begin
  FCxt := TContext.Create(FMultilineStrings, FIndent);

  Writer := TStringWriter.Create;
  try
    GenTableChunks(Obj, FCxt, '', False, Writer);
    Result := Writer.ToString;
  finally
    Writer.Free;
  end;
end;

class function TTOMLWriterImpl.AllCharsInSet(const Str: string; CharSet: TSysCharSet): Boolean;
var
  Ch: Char;
begin
  Result := True;
  for Ch in Str do
    if not CharInSet(Ch, CharSet) then
      Exit(False);
end;

class function TTOMLWriterImpl.EscapedChar(Chr: Char): string;
begin
  case Chr of
    #8:  Result := '\b';
    #10: Result := '\n';
    #12: Result := '\f';
    #13: Result := '\r';
    '"': Result := '\"';
    '\': Result := '\\';
  end;
end;

constructor TTOMLWriterImpl.Create(MultilineStrings: Boolean; Indent: Integer);
begin
  FMultiLineStrings := MultilineStrings;
  FIndent := Indent;
end;

destructor TTOMLWriterImpl.Destroy;
begin
  FCxt.InlineTableCache.Free;
  inherited;
end;


{ TTOMLWriter }

class function TTOMLWriter.ToTOML(Json: TJsonObject; MultilineStrings: Boolean;
  Indent: Integer): string;
var
  Writer: TTOMLWriterImpl;
begin
  Writer := TTOMLWriterImpl.Create(MultilineStrings, Indent);
  try
    Result := Writer.ToTOML(Json);
  finally
    Writer.Free;
  end;
end;

class procedure TTOMLWriter.WriteToFile(Json: TJsonObject; const FName: string;
    MultilineStrings: Boolean = False; Indent: Integer = 4);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FName, fmOpenWrite);
  try
    WriteToStream(Json, FileStream);
  finally
    FileStream.Free;
  end;
end;

class procedure TTOMLWriter.WriteToStream(Json: TJsonObject; Stream: TStream;
    MultilineStrings: Boolean = False; Indent: Integer = 4);
var
  Writer: TTOMLWriterImpl;
begin
  Writer := TTOMLWriterImpl.Create(MultilineStrings, Indent);
  try
    Writer.StreamTOML(Json, Stream);
  finally
    Writer.Free;
  end;
end;

end.
