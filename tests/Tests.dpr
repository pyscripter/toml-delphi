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

procedure RunTests(dir: string; expectedFail: boolean; showJSON: boolean = false);
var
  Files: Tarray<string>;
  name, ext, path: string;
  contents: TBytes;
  doc: TTOMLDocument;
  json: TJSONValue;
begin
  dir := ExpandFileName(dir);
  Files := TDirectory.GetFiles(dir);

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
                WriteLn(' 🔴 Failed!');
                ReadLn;
                Halt;
              end;
          except
            on E: Exception do
              begin
                if not expectedFail then
                  begin
                    WriteLn(' 🔴 ', E.Message);
                    ReadLn;
                    Halt;
                  end;
              end;
          end;
          if showJSON then
            begin
              json := doc.AsJSON;
              writeln(json.Format);
              json.Free;
            end;
          doc.Free;
          writeln(' ✓');
        end;
    end;
  writeln('🟢 All tests passed!');
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  RunTests('./pass', false);
  RunTests('./fail', true);
  ReadLn;
end.