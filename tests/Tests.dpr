{$APPTYPE CONSOLE}

program Tests;
uses
  WinApi.Windows,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.Generics.Collections,
  TOML.Scanner in '..\sources\TOML.Scanner.pas',
  TOML.Parser in '..\sources\TOML.Parser.pas',
  TOML.Types in '..\sources\TOML.Types.pas',
  TOML in '..\sources\TOML.pas',
  TOML.Writer in '..\sources\TOML.Writer.pas';

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


var
  BasePath: string;
  CompatibleTests: TStringList;


procedure RunTests(Dir: string; ExpectedFail: Boolean; ShowJSON: Boolean = False);

  procedure OutputResult(IsSuccess: Boolean; FileInfo: string;  ExcMsg: string  = '');
  var
    Mark: string;
  begin
    if IsSuccess  then
      Mark := #$2714
    else
      Mark := #$2716;
   Write(Mark + '  ' + FileInfo);
   if ExcMsg <> '' then
     Write(': ' + ExcMsg);
   WriteLn;
  end;

var
  Files: Tarray<string>;
  Name, Parentdir, Path: string;
  Doc: TJSONObject;
  Succeeded, Failed, Completed: Integer;
  FileInfo: string;
begin
  Dir := ExpandFileName(Dir);
  Files := TDirectory.GetFiles(Dir, '*.toml', TSearchOption.soAllDirectories);

  Succeeded := 0;
  Failed := 0;
  Completed := 0;

  for Path in files do
    begin
      if not CompatibleTests.Contains(
        Copy(Path, Length(BasePath) + 2).Replace('\', '/', [rfReplaceAll]))
      then
        Continue;

      Name := TPath.GetFileName(Path);
      Parentdir := TPath.GetFileName(TPath.GetDirectoryName(Path));
      FileInfo := TPath.Combine(Parentdir, Name);

      Inc(Completed);
      Doc := nil;

      try
        Doc := TJSONObject.FromTOMLFile(Path);
        if ExpectedFail then
        begin
          OutputResult(False, FileInfo);
          Inc(Failed)
        end
        else
        begin
          Inc(Succeeded);
          //OutputResult(True, FileInfo);
        end;
      except
        on E: Exception do
          begin
            if not ExpectedFail then
            begin
              Inc(Failed);
              OutputResult(False, FileInfo, E.Message);
            end
            else
            begin
              Inc(Succeeded);
              //OutputResult(True, FileInfo);
            end;
          end;
      end;
      if ShowJSON and Assigned(Doc) then
      begin
        Writeln(Doc.Format);
        Writeln(TTOMLWriter.ToTOML(Doc));
      end;
      Doc.Free;
    end;

  WriteLn;
  WriteLn(Format('Completed: %d, Succeeded: %d, Failed: %d',
    [Completed, Succeeded, Failed]));
  if Failed = 0 then
    Writeln(#$2713' All tests passed!');
  WriteLn;
end;

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


begin
  ReportMemoryLeaksOnShutdown := True;
  SetConsoleOutputCP(CP_UTF8);

//  TestSerializer;
//  Readln;
//  Exit;

  BasePath := TPath.GetFullPath('.\toml-test\tests');

  CompatibleTests := TStringList.Create;
  CompatibleTests.LoadFromFile(TPath.Combine(BasePath, 'files-toml-1.0.0'));

  RunTests(TPath.Combine(BasePath, 'valid'), False, False);
  RunTests(TPath.Combine(BasePath, 'invalid'), True, False);

  CompatibleTests.Free;

  ReadLn;
end.