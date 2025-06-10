{
    Copyright (c) 2020 by Ryan Joseph

    TOML Parser
    This unit implements the TOML data types

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
{$scopedenums on}

unit TOMLTypes;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections;

type
  TTOMLFloat = Double;
  TTOMLValueType = Variant;
  TTOMLNumberType = (Integer,
                     Float,
                     Octal,
                     Boolean,
                     Binary,
                     Hexadecimal);

  { TTOMLData }

  ETOMLData = class(Exception);

  TTOMLData = class
    private type
      TEnumerator = record
        private
          container: TTOMLData;
          currentValue: TTOMLData;
          index: integer;
        public
          constructor Create(inContainer: TTOMLData);
          function MoveNext: Boolean;
          procedure Reset;
          property CurrentIndex: integer read index;
          property Current: TTOMLData read currentValue;
      end;
    private
      function GetItem(index: integer): TTOMLData; overload; virtual;
      function GetItem(key: string): TTOMLData; overload; virtual;
      procedure SetItem(index: integer; item: TTOMLData); overload; virtual;
      procedure SetItem(key: string; item: TTOMLData); overload; virtual;
    public
      parent: TTOMLData;
      function ToInteger: integer; virtual;
      function ToFloat: TTOMLFloat; virtual;
      function AsJSON: TJSONValue; virtual;
      function Count: integer; virtual;
      function GetEnumerator: TEnumerator;
      property Items[index: integer]: TTOMLData read GetItem write SetItem; default;
      property Items[key: string]: TTOMLData read GetItem write SetItem; default;
  end;

  TTOMLDataList = TObjectList<TTOMLData>;
  TTOMLDataMap = TObjectOrderedDictionary<string, TTOMLData>;
  TTOMLDataClass = class of TTOMLData;

  { TTOMLValue }

  TTOMLValue = class(TTOMLData)
    private
      m_value: TTOMLValueType;
    public
      constructor Create(const inValue: TTOMLValueType);
      function ToString: string; override;
      function ToInteger: integer; override;
      function ToFloat: TTOMLFloat; override;
      function AsJSON: TJSONValue; override;
      function TypeString: String;
      property Value: TTOMLValueType read m_value;
  end;

  { TTOMLNumber }

  TTOMLNumber = class(TTOMLValue)
    private
      m_type: TTOMLNumberType;
    public
      constructor Create(const inValue: TTOMLValueType; const inType: TTOMLNumberType);
      property &Type: TTOMLNumberType read m_type;
  end;

  { TTOMLDate }

  TTOMLDate = class(TTOMLData)
    public type
      TTime = record
        hours: integer;
        minutes: integer;
        seconds: double;
        { A suffix which, when applied to a time, denotes a UTC
        offset of 00:00; often spoken "Zulu" from the ICAO
        phonetic alphabet representation of the letter "Z". }
        z: boolean;
        function IsSet: boolean;
      end;
    public
      year: integer;
      month: integer;
      day: integer;
      time: TTime;

      { To unambiguously represent a specific instant in time,
        you may use an RFC 3339 formatted date-time with offset.
        https://tools.ietf.org/html/rfc3339}

      offset: TTime;
    public
      constructor Create(localTime: TTime); overload;

      function ToString: string; override;
      function AsJSON: TJSONValue; override;
      function ToISO8601String(roundSeconds: boolean = true): string;
      function AsDateTime: TDateTime;
  end;

  { TTOMLContainer }

  TTOMLContainer = class(TTOMLData);

  { TTOMLArray }

  TTOMLArray = class(TTOMLContainer)
    private
      list: TTOMLDataList;
      function GetItem(index: integer): TTOMLData; override;
    public
      terminated: boolean;
    public
      constructor Create;
      destructor Destroy; override;

      procedure Add(const value: TTOMLValueType); overload;
      procedure Add(const data: TTOMLData); overload;

      function Last: TTOMLData;
      function AsJSON: TJSONValue; override;
      function AsStrings: TStringList;
      function AsArray: TArray<string>;
      function Count: integer; override;
  end;

  { TTOMLTable }

  TTOMLTable = class(TTOMLContainer)
    private
      map: TTOMLDataMap;
      m_name: string;
      function GetItem(key: string): TTOMLData; override;
      function GetItem(index: integer): TTOMLData; override;
      procedure SetItem(key: string; item: TTOMLData); override;
      function GetKey(index: integer): string;
    public
      defined: boolean;
      terminated: boolean;
    public
      constructor Create(name: string = '');
      destructor Destroy; override;

      procedure Add(const key: string; const value: TTOMLValueType); overload;
      procedure Add(const key: string; const data: TTOMLData); overload;
      procedure Put(const key: string; const value: TTOMLValueType); overload;
      procedure Put(const key: string; const data: TTOMLData); overload;

      function Find(const key: string): TTOMLData;
      function Contains(const key: string; dataType: TTOMLDataClass = nil): boolean;
      function AsJSON: TJSONValue; override;
      function Count: integer; override;

      property Name: string read m_name;
      property Keys[Index: Integer]: string read GetKey;
      property Values[Index: Integer]: TTOMLData read GetItem;
  end;

  TTOMLContainerList = TStack<TTOMLTable>;

  { TTOMLDocument }

  TTOMLDocument = class(TTOMLTable);

implementation
uses
  Variants, Types, DateUtils;

{ TTOMLData }

function TTOMLData.GetEnumerator: TEnumerator;
begin
  result := TEnumerator.Create(self);
end;

constructor TTOMLData.TEnumerator.Create(inContainer: TTOMLData);
begin
  container := inContainer;
  index := 0;
end;

function TTOMLData.TEnumerator.MoveNext: Boolean;
var
  count: integer;
begin
  count := container.Count;
  if index = count then
    exit(false);
  while index < count do
    begin
      currentValue := container[index];
      Inc(index);
      if currentValue <> Default(TTOMLData) then
        break;
    end;
  result := index <= count;
end;

procedure TTOMLData.TEnumerator.Reset;
begin
  index := 0;
end;

function TTOMLData.GetItem(index: integer): TTOMLData;
begin
  Assert(false, ClassName+' doesn''t implement indexing');
  result := nil;
end;

function TTOMLData.GetItem(key: string): TTOMLData;
begin
  Assert(false, ClassName+' doesn''t implement keys');
  result := nil;
end;

procedure TTOMLData.SetItem(index: integer; item: TTOMLData);
begin
  Assert(false, ClassName+' doesn''t implement setting by index');
end;

procedure TTOMLData.SetItem(key: string; item: TTOMLData);
begin
  Assert(false, ClassName+' doesn''t implement setting by key');
end;

function TTOMLData.Count: integer;
begin
  Assert(false, ClassName+' doesn''t implement indexing');
  result := 0;
end;

function TTOMLData.AsJSON: TJSONValue;
begin
  Assert(false, 'TOML data can''t be converted to JSON');
  result := nil;
end;

function TTOMLData.ToInteger: integer;
begin
  Assert(false, 'TOML data can''t be converted to integer');
  result := 0;
end;

function TTOMLData.ToFloat: TTOMLFloat;
begin
  Assert(false, 'TOML data can''t be converted to float');
  result := 0;
end;

{ TTOMLValue }

function TTOMLValue.TypeString: String;
begin
  case VarType(value) of
    varEmpty: result := 'Empty';
    varNull: result := 'Null';
    varSingle: result := 'Single';
    varDouble: result := 'Double';
    //varDecimal: result := 'Decimal';
    varCurrency: result := 'Currency';
    varDate: result := 'Date';
    varOleStr: result := 'UnicodeString';
    varString: result := 'Dynamic string';
    varBoolean: result := 'Boolean';
    varVariant: result := 'Variant';
    varUnknown: result := 'unknown';
    varShortInt: result := 'ShortInt';
    varSmallint: result := 'Smallint';
    varInteger: result := 'Integer';
    varInt64: result := 'Int64';
    varByte: result := 'Byte';
    varWord: result := 'Word';
    varLongWord: result := 'LongWord';
    //varQWord: result := 'QWord';
    varError: result := 'ERROR';
  else
      result := 'unknown';
  end;
end;

function TTOMLValue.AsJSON: TJSONValue;
begin
  case VarType(value) of
    varSingle,
    varDouble,
    //varDecimal,
    varCurrency:
      result := TJSONNumber.Create(Double(value));
//    varDate:
//      // TODO;
    varOleStr,
    varStrArg,
    varString,
    varUString:
      result := TJSONString.Create(value);
    varBoolean:
      result := TJSONBool.Create(Boolean(value));
    varShortInt,
    varSmallint,
    varInteger,
    varInt64,
    varByte,
    varWord,
    varLongWord{,
    varQWord}:
      result := TJSONNumber.Create(LongInt(value));
  else
    raise ETOMLData('TOML value '+IntToStr(VarType(value))+' couldn''t be mapped to JSON value.');
  end;
end;

function TTOMLValue.ToInteger: integer;
begin
  result := integer(value);
end;

function TTOMLValue.ToFloat: TTOMLFloat;
begin
  result := TTOMLFloat(value);
end;

function TTOMLValue.ToString: string;
begin
  result := string(value);
end;

constructor TTOMLValue.Create(const inValue: TTOMLValueType);
begin
  m_value := inValue;
end;

{ TTOMLDate }

function TTOMLDate.TTime.IsSet: boolean;
begin
  result := (hours > 0) or (minutes > 0) or (seconds > 0);
end;

constructor TTOMLDate.Create(localTime: TTime);
begin
  time := localTime;
end;

function TTOMLDate.ToString: string;
begin
  result := ToISO8601String(false);
end;

function TTOMLDate.ToISO8601String(roundSeconds: boolean): string;
var
  s: string;
  date: TDateTime;
begin
  if TryEncodeDate(Year, Month, Day, date) then
  begin
    result := Format('%.4d-%.2d-%.2d', [year, month, date]);
    if time.IsSet then
      Result := Result + 'T';
  end
  else
    Result := '';

  if time.IsSet then
    begin
      result := result + Format('%.*d',[2, time.hours])+':'+
                Format('%.*d',[2, time.minutes])+':';

      if roundSeconds then
        result := result + Format('%.*d',[2, Trunc(time.seconds)])
      else
        begin
          s := FloatToStr(time.seconds);
          if length(s) = 1 then
            result := Result + '0';
          result := result + s;
        end;

      if time.Z then
        result := Result + 'Z';

      if offset.IsSet then
        begin
          result := Result + '-';
          result := Result + Format('%.*d',[2, time.hours])+':'+
                    Format('%.*d',[2, time.minutes]);
        end;
    end;
end;

function TTOMLDate.AsJSON: TJSONValue;
begin
  result := TJSONString.Create(ToString);
end;

function TTOMLDate.AsDateTime: TDateTime;
begin
  result := ISO8601ToDate(ToISO8601String);
end;

{ TTOMLNumber }

constructor TTOMLNumber.Create(const inValue: TTOMLValueType; const inType: TTOMLNumberType);
begin
  m_value := inValue;
  m_type := inType;
end;

{ TTOMLArray }

function TTOMLArray.GetItem(index: integer): TTOMLData;
begin
  result := list[index];
end;

function TTOMLArray.AsStrings: TStringList;
var
  data: TTOMLData;
begin
  result := TStringList.Create;
  for data in list do
    result.Add(data.ToString);
end;

function TTOMLArray.AsArray: TArray<string>;
var
  i: integer;
begin
  result := nil;
  SetLength(result, Count);
  for i := 0 to Count - 1 do
    result[i] := list[i].ToString;
end;

function TTOMLArray.AsJSON: TJSONValue;
var
  arr: TJSONArray;
  data: TTOMLData;
begin
  arr := TJSONArray.Create;
  for data in list do
    arr.AddElement(data.AsJSON);
  result := arr;
end;

function TTOMLArray.Last: TTOMLData;
begin
  result := list.Last;
end;

function TTOMLArray.Count: integer;
begin
  result := list.Count;
end;

procedure TTOMLArray.Add(const value: TTOMLValueType);
begin
  Add(TTOMLValue.Create(value));
end;

procedure TTOMLArray.Add(const data: TTOMLData);
begin
  data.parent := self;
  list.Add(data);
end;

constructor TTOMLArray.Create;
begin
  list := TTOMLDataList.Create(true);
end;

destructor TTOMLArray.Destroy;
begin
  list.Free;
  inherited;
end;

{ TTOMLTable }

function TTOMLTable.GetKey(index: integer): string;
begin
  result := map.KeyList[index];
end;

function TTOMLTable.GetItem(key: string): TTOMLData;
var
  data: TTOMLData;
begin
  if map.TryGetValue(key, data) then
    result := data
  else
    raise ETOMLData.Create('Key "'+key+'" doesn''t exist in table "'+name+'"');
end;

function TTOMLTable.GetItem(index: integer): TTOMLData;
begin
  result := map.ValueList[index];
end;

procedure TTOMLTable.SetItem(key: string; item: TTOMLData);
begin
  Put(key, item);
end;

function TTOMLTable.Count: integer;
begin
  result := map.Count;
end;

function TTOMLTable.AsJSON: TJSONValue;
var
  i: integer;
  obj: TJSONObject;
begin
  obj := TJSONObject.Create;
  for i := 0 to map.Count - 1 do
    obj.AddPair(map.KeyList[i], map.ValueList[i].AsJSON);
  result := obj;
end;

procedure TTOMLTable.Add(const key: string; const data: TTOMLData);
begin
  if Contains(key) then
    raise ETOMLData.Create('Key "'+key+'" already exists in table "'+name+'"');
  data.parent := self;
  map.Add(key, data);
end;

procedure TTOMLTable.Add(const key: string; const value: TTOMLValueType);
begin
  Add(key, TTOMLValue.Create(value));
end;

procedure TTOMLTable.Put(const key: String; const value: TTOMLValueType);
begin
  Put(key, TTOMLValue.Create(value));
end;

procedure TTOMLTable.Put(const key: String; const data: TTOMLData);
begin
  data.parent := self;

  map.AddOrSetValue(key, data);
end;

function TTOMLTable.Contains(const key: string; dataType: TTOMLDataClass = nil): boolean;
var
  data: TTOMLData;
begin
  result := map.TryGetValue(key, data) and
    (not Assigned(dataType) or data.InheritsFrom(dataType));
end;

function TTOMLTable.Find(const key: string): TTOMLData;
var
  data: TTOMLData;
begin
  if map.TryGetValue(key, data) then
    result := data
  else
    result := nil;
end;

constructor TTOMLTable.Create(name: string);
begin
  m_name := name;
  defined := false;
  map := TTOMLDataMap.Create([doOwnsValues]);
  //map.Sorted := true;
end;

destructor TTOMLTable.Destroy;
begin
  map.Free;
  inherited;
end;


end.
