{
    Main unit

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
  TOML.Types,
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
