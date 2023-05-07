program ArchExtractor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, Arch;

const
  APP_VERSION = ' v0.010';

type

  { TApplication }

  TApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure AECallback(strMsg: string);
begin
  WriteLn(strMsg);
end;

{ TApplication }

procedure TApplication.DoRun;
var
  strFilename: String;
  FS: TFileStream;
  ArchExtractor: TArchExtractor;
begin
  strFilename := GetOptionValue('f', '');

  if strFilename = '' then
  begin
    WriteLn('Arch Extractor', APP_VERSION);
    WriteLn('Usage: ArchExtractor.exe -f InputFile.Arch01');
    WriteLn('Options:');
    WriteLn('   -f: Input Arch00-01 file');
  end
  else
  begin
    FS := TFileStream.Create(strFilename, fmOpenRead);
    ArchExtractor := TArchExtractor.Create(FS, @AECallback);

    ArchExtractor.Load;
    ArchExtractor.ExtractFiles;

    ArchExtractor.Free;
    FS.Free;
  end;

  Terminate;
end;

constructor TApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TApplication.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TApplication;
begin
  Application:=TApplication.Create(nil);
  Application.Title:='ArchExtractor';
  Application.Run;
  Application.Free;
end.

