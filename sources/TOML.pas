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
  TOMLParser;

function GetTOML(Contents: TBytes): TJSONObject; overload;
function GetTOML(Stream: TStream): TJSONObject; overload;
function GetTOML(FileName: string): TJSONObject; overload;

implementation

function GetTOML(Contents: TBytes): TJSONObject;
begin
  Result := TOMLParser.GetTOML(Contents);
end;

function GetTOML(Stream: TStream): TJSONObject;
var
  Contents: TBytes;
begin
  SetLength(Contents, Stream.Size);
  Stream.Position := 0;
  Stream.Read(Contents, Stream.Size);
  Result := GetTOML(Contents);
end;

function GetTOML(FileName: string): TJSONObject;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Result := GetTOML(Stream);
end;


end.
