{
    Author: PyScripter (https://github.com/pyscripter)

    The main unit of the library.
		********************************************************************

    Permission is hereby granted, free of charge, to any person obtaining
    a copy of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense,
    and/or sell copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
    INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
    PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
    HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
    OR OTHER DEALINGS IN THE SOFTWARE.
}

unit TOML;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  TOML.Parser,
  TOML.Writer;

type
  TTOMLDocument = TJSONObject;
  TTOMLParser = TOML.Parser.TTOMLParser;
  TTOMLWriter = TOML.Writer.TTOMLWriter;

  TJSONObjectHelper = class helper for TJSONObject
    function ToTOML(MultilineStrings: Boolean = False; Indent: Integer = 4): string;
    procedure StreamTOML(Stream: TStream; MultilineStrings: Boolean = False; Indent: Integer = 4);
    procedure SaveTOMLtoFile(const FileName: string; MultilineStrings: Boolean = False; Indent: Integer = 4);
    class function FromTOML(const Contents: string): TJSONObject; overload;
    class function FromTOML(Contents: TBytes): TJSONObject; overload;
    class function FromTOML(Stream: TStream): TJSONObject; overload;
    class function FromTOMLFile(const FileName: string): TJSONObject;
  end;

  ETOMLSerializer = class(Exception);

  TTOMLSerializer = class
    class function Serialize<T>(const AValue: T): string; overload;
    class function Deserialize<T>(const ATOML: string): T; overload;
  end;

implementation

uses
  TOML.Support,
  System.Json.Serializers,
  System.Json.Readers,
  System.Json.Writers;

{ TJSONObjectHelper }

class function TJSONObjectHelper.FromTOML(const Contents: string): TJSONObject;
begin
  Result := FromTOML(TEncoding.UTF8.GetBytes(Contents));
end;

class function TJSONObjectHelper.FromTOML(Contents: TBytes): TJSONObject;
begin
  Result := TTOMLParser.TOMLToJson(Contents);
end;

class function TJSONObjectHelper.FromTOML(Stream: TStream): TJSONObject;
var
  Contents: TBytes;
begin
  SetLength(Contents, Stream.Size);
  Stream.Position := 0;
  Stream.Read(Contents, Stream.Size);
  Result := FromTOML(Contents);
end;

class function TJSONObjectHelper.FromTOMLFile(const FileName: string): TJSONObject;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := FromTOML(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJSONObjectHelper.SaveTOMLtoFile(const FileName: string;
  MultilineStrings: Boolean; Indent: Integer);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenWrite);
  try
    StreamTOML(Stream, MultilineStrings, Indent);
  finally
    Stream.Free;
  end;
end;

procedure TJSONObjectHelper.StreamTOML(Stream: TStream; MultilineStrings:
    Boolean = False; Indent: Integer = 4);
begin
  TTOMLWriter.WriteToStream(Self, Stream, MultilineStrings, Indent);
end;

function TJSONObjectHelper.ToTOML(MultilineStrings: Boolean;
  Indent: Integer): string;
begin
  Result := TTOMLWriter.ToTOML(Self, MultilineStrings, Indent);
end;

{ TTOMLSerializer }

class function TTOMLSerializer.Deserialize<T>(const ATOML: string): T;
var
  Serializer: TJsonSerializer;
  Reader: TJsonObjectReader;
  JsonObject: TJsonObject;
begin
  if not (GetTypeKind(T) in [tkClass, tkRecord]) then
    raise ETOMLSerializer.CreateRes(@rsTypeClassOrRecord);

  try
    JsonObject := TJSONObject.FromTOML(ATOML);
  except
    on E: Exception do
      raise ETOMLSerializer.Create(E.Message);
  end;

  Serializer := TJsonSerializer.Create;
  Reader := TJsonObjectReader.Create(JsonObject);
  try
    Result := Serializer.Deserialize<T>(Reader);
  finally
    Reader.Free;
    Serializer.Free;
    JsonObject.Free;
  end;
end;

class function TTOMLSerializer.Serialize<T>(const AValue: T): string;
var
  Serializer: TJsonSerializer;
  Writer: TJsonObjectWriter;
begin
  if not (GetTypeKind(T) in [tkClass, tkRecord]) then
    raise ETOMLSerializer.CreateRes(@rsTypeClassOrRecord);

  Serializer := TJsonSerializer.Create;
  Writer := TJsonObjectWriter.Create;
  try
    Serializer.Serialize(Writer, AValue);
    Result := (Writer.JSON as TJsonObject).ToTOML;
  finally
    Writer.Free;
    Serializer.Free;
  end;

end;

end.
