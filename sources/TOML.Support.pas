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

unit TOML.Support;

interface

uses
  System.SysUtils,
  System.Classes,
  System.TypInfo,
  System.Rtti,
  System.JSON;

const
  CharSetBareKey = ['0'..'9', 'a'..'z','A'..'Z','_','-'];
  CharSetIllegalStr = [#$0..#$8,#$A..#$1F, #$7F];

resourcestring
  rsTypeClassOrRecord = 'Type must be either a class or a record';

implementation
uses
  System.Types,
  System.Math,
  System.JSONConsts;

end.
