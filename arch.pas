unit Arch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PasZLib, FileUtil;

const
  C_STRING_BUFFER_LEN = 1024;

type

  TArchExtractorCallback = procedure(strMsg: string);

  TArchHeader = packed record
    szTag: array[0..3] of Char;
    dwVersion: Cardinal;
    dwNametableLength: Cardinal;
    dwDirCount: Cardinal;
    dwFileCount: Cardinal;
    dwUnk1: Cardinal;
    dwUnk2: Cardinal;
    dwUnk3: Cardinal;
    aHash: array[0..15] of Byte;
  end;

  TArchFileInfo = packed record
    dwFilenameOffset: Cardinal;
    qwFileOffset: UInt64;
    qwCompFileLen: UInt64;
    qwDecompFileLen: UInt64;
    dwCompression: Cardinal;
  end;

  TArchFileInfoTable = array of TArchFileInfo;

  TArchDirInfo = packed record
    dwFilenameOffset: Cardinal;
    dwFirstSubIndex: Cardinal;
    dwNextIndex: Cardinal;
    dwFileCount: Cardinal;
  end;

  TArchDirInfoTable = array of TArchDirInfo;
  TCardinalArray = array of Cardinal;

  { TArchExtractor }

  TArchExtractor = class(TObject)
  private
    m_Stream: TFileStream;
    m_Header: TArchHeader;
    m_Nametable: String;
    m_FileInfoTable: TArchFileInfoTable;
    m_DirInfoTable: TArchDirInfoTable;
    m_FileAndDir: TCardinalArray;
    m_CallbackFunc: TArchExtractorCallback;
    m_nCreatedDirs: Cardinal;
    procedure ReadHeader;
    procedure ReadNameTable;
    procedure ReadFileInfoTable;
    procedure ReadDirInfoTable;
    procedure CreateDirStructure(strRootDir: string);
    procedure ExtractFiles(strRootDir: string);
    procedure FillFileAndDir;
    procedure InflateBytes(aCompBuffer: TBytes; dwCompLen: Cardinal; aDecompBuffer: TBytes; dwDecompLen: Cardinal);
    procedure InflateLargeBytes(aCompBuffer: TBytes; dwCompLen: Cardinal; aDecompBuffer: TBytes; dwDecompLen: Cardinal);
  public
    property Header: TArchHeader read m_Header;
    property FileInfoTable: TArchFileInfoTable read m_FileInfoTable;
    property DirInfoTable: TArchDirInfoTable read m_DirInfoTable;
    procedure Load;
    procedure ExtractFiles;
    constructor Create(FS: TFileStream; Callback: TArchExtractorCallback);
    destructor Destroy; override;
  end;

implementation

{ TArchExtractor }

procedure TArchExtractor.ReadHeader;
begin
  m_Stream.Read(m_Header, sizeof(TArchHeader));
end;

procedure TArchExtractor.ReadNameTable;
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  MS.CopyFrom(m_Stream, m_Header.dwNametableLength);

  MS.Position := 0;
  SetLength(m_Nametable, m_Header.dwNametableLength);
  MS.ReadBuffer(m_Nametable[1], m_Header.dwNametableLength);

  MS.Free;
end;

procedure TArchExtractor.ReadFileInfoTable;
var i: Cardinal;
begin
  SetLength(m_FileInfoTable, m_Header.dwFileCount);
  for i := 0 to m_Header.dwFileCount - 1 do
  begin
    m_Stream.ReadBuffer(m_FileInfoTable[i], sizeof(TArchFileInfo));
  end;
end;

procedure TArchExtractor.ReadDirInfoTable;
var
  i: Cardinal;
begin
  SetLength(m_DirInfoTable, m_Header.dwDirCount);
  for i := 0 to m_Header.dwDirCount - 1 do
  begin
    m_Stream.ReadBuffer(m_DirInfoTable[i], sizeof(TArchDirInfo));
  end;
end;

procedure TArchExtractor.CreateDirStructure(strRootDir: string);
var i: Cardinal;
    szDir: PChar;
begin
  for i := 0 to m_Header.dwDirCount - 1 do
  begin
    szDir := PChar(m_Nametable) + m_DirInfoTable[i].dwFilenameOffset;
    if CreateDir(strRootDir + '\' + szDir) then
      Inc(m_nCreatedDirs, 1);
  end;
end;

procedure TArchExtractor.ExtractFiles;
var strRootDir: string;
    slDirs, slFiles: TStringList;
begin
  strRootDir := ExtractFileName(m_Stream.FileName) + '_RootFolder';
  m_nCreatedDirs := 0;

  FillFileAndDir;
  CreateDirStructure(strRootDir);
  ExtractFiles(strRootDir);

  m_CallbackFunc('');

  slDirs := FindAllDirectories(strRootDir);
  slFiles := FindAllFiles(strRootDir);

  m_CallbackFunc(Format('Directories: %d out of %d', [slDirs.Count + 1, m_Header.dwDirCount]));
  m_CallbackFunc(Format('Files: %d out of %d', [slFiles.Count, m_Header.dwFileCount]));

  slDirs.Free;
  slFiles.Free;
end;

procedure TArchExtractor.ExtractFiles(strRootDir: string);
var i: Cardinal;
    nPad: Cardinal;
    aChunkSizes: array[0..1] of Cardinal = (0, 0);
    qwTempLen: UInt64;
    aCompBuffer: TBytes = nil;
    aDecompBuffer: TBytes = nil;
    szFilename, szDir: PChar;
    FS: TFileStream;
begin
  for i := 0 to m_Header.dwFileCount - 1 do
  begin
    qwTempLen := 0;
    szFilename := PChar(m_Nametable) + m_FileInfoTable[i].dwFilenameOffset;
    szDir :=  PChar(m_Nametable) + m_DirInfoTable[m_FileAndDir[i]].dwFilenameOffset;

    FS := TFileStream.Create(strRootDir + '\' + szDir + '\' + szFilename, fmOpenWrite + fmCreate);
    m_Stream.Position := m_FileInfoTable[i].qwFileOffset;

    if m_FileInfoTable[i].dwCompression > 0 then
    begin
      m_CallbackFunc(Format('[%d] %s\%s (%d -> %d)', [i, szDir, szFilename, m_FileInfoTable[i].qwCompFileLen, m_FileInfoTable[i].qwDecompFileLen]));
      while qwTempLen < m_FileInfoTable[i].qwDecompFileLen do
      begin
        m_Stream.ReadBuffer(aChunkSizes[0], 8);
        SetLength(aCompBuffer, aChunkSizes[0]);
        m_Stream.ReadBuffer(aCompBuffer[0], aChunkSizes[0]);

        if aChunkSizes[0] < aChunkSizes[1] then
        begin
          SetLength(aDecompBuffer, aChunkSizes[1]);
          InflateLargeBytes(aCompBuffer, aChunkSizes[0], aDecompBuffer, aChunkSizes[1]);
          FS.WriteBuffer(aDecompBuffer[0], aChunkSizes[1]);
        end
        else
        begin
          FS.WriteBuffer(aCompBuffer[0], aChunkSizes[1]);
        end;

        nPad := m_Stream.Position mod 4;
        if nPad > 0 then nPad := 4 - nPad;

        m_Stream.Seek(nPad, soCurrent);
        qwTempLen := qwTempLen + aChunkSizes[1] + nPad + 4;
      end;
    end
    else
    begin
      m_CallbackFunc(Format('[%d] %s\%s (%d)', [i, szDir, szFilename, m_FileInfoTable[i].qwDecompFileLen]));
      if m_FileInfoTable[i].qwDecompFileLen > 0 then
        FS.CopyFrom(m_Stream, m_FileInfoTable[i].qwDecompFileLen);
    end;

    FS.Free;
  end;

  SetLength(aCompBuffer, 0);
  SetLength(aDecompBuffer, 0);
end;

procedure TArchExtractor.FillFileAndDir;
var i, j: Cardinal;
    nFile: Integer;
begin
  nFile := 0;
  SetLength(m_FileAndDir, m_Header.dwFileCount);
  for i := 0 to m_Header.dwDirCount - 1 do
  begin
    if m_DirInfoTable[i].dwFileCount > 0 then
    for j := 0 to m_DirInfoTable[i].dwFileCount - 1 do
    begin
      m_FileAndDir[nFile] := i;
      Inc(nFile, 1);
    end;
  end;
end;

procedure TArchExtractor.InflateBytes(aCompBuffer: TBytes; dwCompLen: Cardinal;
  aDecompBuffer: TBytes; dwDecompLen: Cardinal);
var ZS: TZStream;
begin
  ZS.next_in := @aCompBuffer[0];
  ZS.avail_in := 0;
  ZS.next_out := @aDecompBuffer[0];

  {%H-}inflateInit(ZS);
  while (ZS.total_out < dwDecompLen) and (ZS.total_in < dwCompLen) do
  begin
    ZS.avail_out := 1;
    ZS.avail_in := 1;
    if ({%H-}inflate(ZS, Z_NO_FLUSH) = Z_STREAM_END) then
      break;
  end;
  {%H-}inflateEnd(ZS);
end;

procedure TArchExtractor.InflateLargeBytes(aCompBuffer: TBytes;
  dwCompLen: Cardinal; aDecompBuffer: TBytes; dwDecompLen: Cardinal);
var ZS: TZStream;
begin
  ZS.next_in := @aCompBuffer[0];
  ZS.avail_in := dwCompLen;

  {%H-}inflateInit(ZS);
  while True do
  begin
    ZS.next_out := @aDecompBuffer[0];
    ZS.avail_out := dwDecompLen;
    if ({%H-}inflate(ZS, Z_NO_FLUSH) = Z_STREAM_END) then
      break;
  end;
  {%H-}inflateEnd(ZS);
end;

procedure TArchExtractor.Load;
begin
  ReadHeader;
  ReadNameTable;
  ReadFileInfoTable;
  ReadDirInfoTable;
end;

constructor TArchExtractor.Create(FS: TFileStream; Callback: TArchExtractorCallback);
begin
  m_Stream := FS;
  m_CallbackFunc := Callback;
end;

destructor TArchExtractor.Destroy;
begin
  inherited Destroy;
  SetLength(m_FileInfoTable, 0);
  SetLength(m_DirInfoTable, 0);
  SetLength(m_FileAndDir, 0);
end;

end.

