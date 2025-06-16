{

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

unit TOMLTypes;

interface

uses
  System.SysUtils,
  System.Classes,
  System.TypInfo,
  System.Rtti,
  System.JSON;

type

  TJSONFloat = class(TJSONString)
  private
    function GetAsDouble: double;
  protected
    function AsTValue(ATypeInfo: PTypeInfo; var AValue: TValue): Boolean; override;
  public
    procedure ToChars(Builder: TStringBuilder; Options: TJSONAncestor.TJSONOutputOptions); override;
    property AsDouble: double read GetAsDouble;
  end;

implementation
uses
  System.Types,
  System.Math,
  System.JSONConsts;

{ TJSONFloat }

function TJSONFloat.AsTValue(ATypeInfo: PTypeInfo; var AValue: TValue): Boolean;
begin
  if ATypeInfo.Kind = tkFloat then
  begin
    if (FValue = 'inf') or (FValue = '+inf') then
    begin
      AValue := Infinity;
      Result := True;
    end
    else if FValue = '-inf' then
    begin
      AValue := NegInfinity;
      Result := True;
    end
    else if (FValue = 'nan') or (FValue = '+nan') or (FValue = '-nan') then
    begin
      AValue := Nan;
      Result := True;
    end else
      Result := inherited;
  end
  else
    Result := inherited;
end;

function TJSONFloat.GetAsDouble: double;
var
  Value: TValue;
begin
  if not AsTValue(TypeInfo(double), Value) then
    raise EJSONException.CreateResFmt(@SCannotConvertJSONValueToType,
      [ClassName, 'double']);
  Result := Value.AsType<double>;
end;

procedure TJSONFloat.ToChars(Builder: TStringBuilder;
  Options: TJSONAncestor.TJSONOutputOptions);
begin
  if (FValue = 'inf') or (FValue = '+inf') or (FValue = '-inf') or
     (FValue = 'nan') or (FValue = '+nan') or (FValue = '-nan')
  then
    Builder.Append(FValue)
  else
    inherited;
end;

end.
