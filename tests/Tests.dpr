{
    Copyright (c) 2020 by Ryan Joseph

    TOML Parser Tests
}

{$APPTYPE CONSOLE}

program Tests;
uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  Scanner in '..\sources\Scanner.pas',
  TOMLParser in '..\sources\TOMLParser.pas',
  TOMLTypes in '..\sources\TOMLTypes.pas',
  TOML in '..\sources\TOML.pas';

function JsonValuesEqual(Value1, Value2: TJsonValue): Boolean;
var
  I: Integer;
  Obj1, Obj2: TJsonObject;
  Arr1, Arr2: TJsonArray;
  Pair: TJsonPair;
begin
  // Different types can't be equal
  if Value1.ClassType <> Value2.ClassType then
    Exit(False);

  if Value1 is TJsonObject then
  begin
    Obj1 := TJsonObject(Value1);
    Obj2 := TJsonObject(Value2);

    // Compare property count
    if Obj1.Count <> Obj2.Count then
      Exit(False);

    // Compare each property
    for Pair in Obj1 do
    begin
      if not Obj2.TryGetValue(Pair.JsonString.Value, Value2) then
        Exit(False);
      if not JsonValuesEqual(Pair.JsonValue, Value2) then
        Exit(False);
    end;
    Result := True;
  end
  else if Value1 is TJsonArray then
  begin
    Arr1 := TJsonArray(Value1);
    Arr2 := TJsonArray(Value2);

    // Compare array lengths
    if Arr1.Count <> Arr2.Count then
      Exit(False);

    // Compare each element
    for I := 0 to Arr1.Count - 1 do
      if not JsonValuesEqual(Arr1.Items[I], Arr2.Items[I]) then
        Exit(False);
    Result := True;
  end
  else if Value1 is TJsonString then
    Result := TJsonString(Value1).Value = TJsonString(Value2).Value
  else if Value1 is TJsonNumber then
    Result := TJsonNumber(Value1).ToString = TJsonNumber(Value2).ToString
  else if Value1 is TJsonBool then
    Result := TJsonBool(Value1).AsBoolean = TJsonBool(Value2).AsBoolean
  else if Value1 is TJsonNull then
    Result := True  // All nulls are equal
  else
    Result := False; // Unknown type
end;


procedure RunTests(dir: string; expectedFail: boolean; showJSON: boolean = false);
var
  Files: Tarray<string>;
  name, ext, path: string;
  contents: TBytes;
  doc: TTOMLDocument;
  json: TJSONValue;
begin
  dir := ExpandFileName(dir);
  Files := TDirectory.GetFiles(dir, '*.toml', TSearchOption.soAllDirectories);

  for path in files do
    begin
      name := ExtractFileName(path);
      ext := ExtractFileExt(path);
      if ext = '.toml' then
        begin
          write(ExtractFileName(dir), '/', name);
          doc := nil;
          contents := TFile.ReadAllBytes(path);
          try
            doc := GetTOML(contents);
            if expectedFail then
              begin
                doc.Free;
                WriteLn(' '#$D7'  Failed!');
                ReadLn;
                Halt;
              end;
          except
            on E: Exception do
              begin
                if not expectedFail then
                  begin
                    doc.Free;
                    WriteLn(' '#$D7' ', E.Message);
                    ReadLn;
                    Halt;
                  end;
              end;
          end;
          writeln(' '#$2713);
          if showJSON then
            begin
              json := doc.AsJSON;
              writeln(json.Format);
              json.Free;
            end;
          doc.Free;
        end;
    end;
  writeln(#$2713' All tests passed!');
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  RunTests('./pass', false, false);
  RunTests('./fail', true);
  ReadLn;
end.