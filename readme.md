# toml-delphi

TOML parser, writer and serializer for Delphi. Learn more about Tom's Obvious, Minimal Language at https://toml.io/.

###  Features:
 - [TOML v1.0.0](https://toml.io/en/v1.0.0) compliant.
 - Passes all 734 (valid/invalid) [official validation tests](https://github.com/toml-lang/toml-test).
 - Fast. Single stream tokenizer and lexer, that doesn't use regex.
 - Converts TOML documents to Delphi's TJSONObject, thus allowing for easy traversal, manipulation and query of the generated documents
 - Includes TTOMLWriter for converting TJSONObjects back to TOML.
 - Provides for easy (de)serialization of Delphi objects and records from/to TOML.
 
### Usage

There is nothing to install.  Just download or clone this repository and add the sources subdirectory directory to Delphi's libray search path.

The main unit you need to add to your uses clause is TOML.

This is the interface section of this unit:

```pascal
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
```
You can convert TOML source to `TJSONObject` using one of the FromTOML functions.  For example to parse a TOML file you use:

```pascal
var JsonObject := TJSONObject.FromTOMLFile(FileName);

//or for parsing a TOML string:

var JsonObject := TJSONObject.FromTOML(TOMLstring);
```

To convert a `TJSONObject` to TOML you use one of the methods `ToTOML`, `StreamTOML` or `SaveTOMLToFile`.  For example:

```pascal
TOMLString := JsonObject.ToTOML;

// or

JsonObject.SaveTOMLToFile(FileName);
```
#### Serialization

The fact that TOML documents are just JSON objects, makes it possible to serialize Delphi objects and records using the built-in serealization infrastructure of Delphi (`TJsonSerializer`). toml-delphi provides the TTOMLSerializer class (see above) makes the process very straight-forward.  

Example:

```pascal
type
  TTestRec = record
    IntValue: Integer;
    FloatValue: double;
    StringValue: string;
    DateValue: TDateTime;
    ArrayValue: TArray<string>;
 end;

 procedure TestSerializer;
 var
   Rec: TTestRec;
   TOMLString: string;
 begin
   Rec.IntValue := 123;
   Rec.FloatValue := 3.14;
   Rec.StringValue := 'abc';
   Rec.DateValue := Now;
   Rec.ArrayValue := ['A', 'B', 'C'];

   Writeln('Serialized record:');
   WriteLn('==================');
   TOMLString := TTOMLSerializer.Serialize(Rec);
   Writeln(TOMLString);
   Writeln('Record deserialized and serialized again:');
   Writeln('=========================================');
   Rec := TTOMLSerializer.Deserialize<TTestRec>(TOMLString);
   TOMLString := TTOMLSerializer.Serialize(Rec);
   Writeln(TOMLString);
 end;
 ```

Output:

```
Serialized record:
==================
IntValue = 123
FloatValue = 3.14
StringValue = "abc"
DateValue = "2025-06-18T05:37:02.110+03:00"
ArrayValue = [
    "A",
    "B",
    "C"
]

Record deserialized and serialized again:
=========================================
IntValue = 123
FloatValue = 3.14
StringValue = "abc"
DateValue = "2025-06-18T05:37:02.110+03:00"
ArrayValue = [
    "A",
    "B",
    "C"
]
```

### Credits:
- The scanner and parser are based on [fpTOML](https://github.com/genericptr/fpTOML) by [Ryan Joseph](https://github.com/genericptr).  The parser has been extensively modified and improved.
- The TOML Writer is based on the python package [tomli-w](https://github.com/hukkin/tomli-w).