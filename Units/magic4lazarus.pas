(****************************************************************************
 *
 * Magic4Lazarus:
 * Retrive the mime of content from a file or a buffer.
 *
 * Copyright (c) Francisco Ernesto Teixeira 2010.
 * All Rights Reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice immediately at the beginning of the file, without modification,
 *    this list of conditions, and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ''AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 ****************************************************************************)

(* in linux install packages: libmagic, libmagic-dev *)

unit magic4lazarus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TMagic }
  { is a wrapper around the libmagic C library }

  TMagic = class
  private
    FHANDLE: integer;
    FOnlyMimeEncoding: boolean;
    FOnlyMimeType: boolean;
    FOnlyMimeEncodig: boolean;
    FFileMagic: string;
    FFlags: integer;
    procedure SetOnlyMimeType(const AValue: boolean);
    procedure SetOnlyMimeEncoding(const AValue: boolean);
    procedure BeforeDetect;
  public
    property HANDLE: integer read FHANDLE;
    property OnlyMimeType: boolean read FOnlyMimeType write SetOnlyMimeType;
    property OnlyMimeEncoding: boolean read FOnlyMimeEncoding write SetOnlyMimeEncoding;
    property FileMagic: string read FFileMagic write FFileMagic;
    constructor Create();
    destructor Destroy; override;
    function FromFile(filename: string): string;
    function FromBuffer(var buffer; count: longint): string;
  end;

// Detect the mime of file's content and return it
function DetectMimeFromFile(filename: string; filemagic: string): string;
// Detect the mime type of file's content and return it
function DetectMimeTypeFromFile(filename: string; filemagic: string): string;
// Detect the mime encoding of file's content and return it
function DetectMimeEncodingFromFile(filename: string; filemagic: string): string;

implementation

const
  {$IFDEF WINDOWS}
  MAGIC_LIBRARY = 'magic1.dll';
  {$ENDIF}
  {$IFDEF LINUX}
  MAGIC_LIBRARY = 'libmagic.so.1.0.0';
  {$ENDIF}
  {$IFDEF DARWIN}
  MAGIC_LIBRARY = 'libmagic.dylib';
  {$linklib libmagic}
  {$ENDIF}

  MAGIC_NONE = $000000; // No flags
  {$EXTERNALSYM MAGIC_NONE}
  MAGIC_DEBUG = $000001; // Turn on debugging
  {$EXTERNALSYM MAGIC_DEBUG}
  MAGIC_SYMLINK = $000002; // Follow symlinks
  {$EXTERNALSYM MAGIC_SYMLINK}
  MAGIC_COMPRESS = $000004; // Check inside compressed files
  {$EXTERNALSYM MAGIC_COMPRESS}
  MAGIC_DEVICES = $000008; // Look at the contents of devices
  {$EXTERNALSYM MAGIC_DEVICES}
  MAGIC_MIME_TYPE = $000010; // Return the MIME type
  {$EXTERNALSYM MAGIC_MIME_TYPE}
  MAGIC_CONTINUE = $000020; // Return all matches
  {$EXTERNALSYM MAGIC_CONTINUE}
  MAGIC_CHECK = $000040; // Print warnings to stderr
  {$EXTERNALSYM MAGIC_CHECK}
  MAGIC_PRESERVE_ATIME = $000080; // Restore access time on exit
  {$EXTERNALSYM MAGIC_PRESERVE_ATIME}
  MAGIC_RAW = $000100; // Don't translate unprintable chars
  {$EXTERNALSYM MAGIC_RAW}
  MAGIC_ERROR = $000200; // Handle ENOENT etc as real errors
  {$EXTERNALSYM MAGIC_ERROR}
  MAGIC_MIME_ENCODING = $000400; // Return the MIME encoding
  {$EXTERNALSYM MAGIC_MIME_ENCODING}
  MAGIC_MIME = (MAGIC_MIME_TYPE or MAGIC_MIME_ENCODING);
  {$EXTERNALSYM MAGIC_MIME}
  MAGIC_APPLE = $000800; // Return the Apple creator and type
  {$EXTERNALSYM MAGIC_APPLE}
  MAGIC_NO_CHECK_COMPRESS = $001000; // Don't check for compressed files
  {$EXTERNALSYM MAGIC_NO_CHECK_COMPRESS}
  MAGIC_NO_CHECK_TAR = $002000; // Don't check for tar files
  {$EXTERNALSYM MAGIC_NO_CHECK_TAR}
  MAGIC_NO_CHECK_SOFT = $004000; // Don't check magic entries
  {$EXTERNALSYM MAGIC_NO_CHECK_SOFT}
  MAGIC_NO_CHECK_APPTYPE = $008000; // Don't check application type
  {$EXTERNALSYM MAGIC_NO_CHECK_APPTYPE}
  MAGIC_NO_CHECK_ELF = $010000; // Don't check for elf details
  {$EXTERNALSYM MAGIC_NO_CHECK_ELF}
  MAGIC_NO_CHECK_TEXT = $020000; // Don't check for text files
  {$EXTERNALSYM MAGIC_NO_CHECK_TEXT}
  MAGIC_NO_CHECK_CDF = $040000; // Don't check for cdf files
  {$EXTERNALSYM MAGIC_NO_CHECK_CDF}
  MAGIC_NO_CHECK_TOKENS = $100000; // Don't check tokens
  {$EXTERNALSYM MAGIC_NO_CHECK_TOKENS}
  MAGIC_NO_CHECK_ENCODING = $200000; // Don't check text encodings
  {$EXTERNALSYM MAGIC_NO_CHECK_ENCODING}

  // Defined for backwards compatibility (renamed)
  MAGIC_NO_CHECK_ASCII = MAGIC_NO_CHECK_TEXT;
  {$EXTERNALSYM MAGIC_NO_CHECK_ASCII}

  // Defined for backwards compatibility; do nothing
  MAGIC_NO_CHECK_FORTRAN = $000000; // Don't check ascii/fortran
  {$EXTERNALSYM MAGIC_NO_CHECK_FORTRAN}
  MAGIC_NO_CHECK_TROFF = $000000; // Don't check ascii/troff
  {$EXTERNALSYM MAGIC_NO_CHECK_TROFF}

function magic_open(flags: integer): integer; cdecl; external MAGIC_LIBRARY;
procedure magic_close(cookie: integer); cdecl; external MAGIC_LIBRARY;

function magic_file(cookie: integer; filename: PChar): PChar; cdecl; external MAGIC_LIBRARY;
function magic_descriptor(cookie: integer): PChar; cdecl; external MAGIC_LIBRARY;
function magic_buffer(cookie: integer; var buffer; Count: longint): PChar; cdecl; external MAGIC_LIBRARY;

//function magic_error(cookie: integer): PChar; cdecl; external MAGIC_LIBRARY;
function magic_setflags(coockie: integer; flags: integer): integer; cdecl; external MAGIC_LIBRARY;

function magic_load(cookie: integer; magic_file: PChar): integer; cdecl; external MAGIC_LIBRARY;
function magic_compile(cookie: integer; magic_file: PChar): integer; cdecl; external MAGIC_LIBRARY;
//function magic_check(cookie: integer; magic_file: PChar): Integer; cdecl; external MAGIC_LIBRARY;
function magic_errno(cookie: integer): integer; cdecl; external MAGIC_LIBRARY;

// Detect the mime of file's content and return it
function DetectMimeFromFile(filename: string; filemagic: string): string;
var
  magic: TMagic;
begin
  magic := TMagic.Create();
  try
    magic.FileMagic := filemagic;
    Result := magic.FromFile(filename);
  finally
    FreeAndNil(magic);
  end;
end;

// Detect the mime type of file's content and return it
function DetectMimeTypeFromFile(filename: string; filemagic: string): string;
var
  magic: TMagic;
begin
  magic := TMagic.Create();
  try
    magic.FileMagic := filemagic;
    magic.OnlyMimeType := True;
    Result := magic.FromFile(filename);
  finally
    FreeAndNil(magic);
  end;
end;

// Detect the mime encoding of file's content and return it
function DetectMimeEncodingFromFile(filename: string; filemagic: string): string;
var
  magic: TMagic;
begin
  magic := TMagic.Create();
  try
    magic.FileMagic := filemagic;
    magic.OnlyMimeEncoding := True;
    Result := magic.FromFile(filename);
  finally
    FreeAndNil(magic);
  end;
end;

{ TMagic }

// Create a new libmagic wrapper.
constructor TMagic.Create();
begin
  Self.FHANDLE := 0;
  FOnlyMimeType := False;
  FOnlyMimeEncodig := False;
  FFileMagic := '';
  FFlags := MAGIC_MIME;

  Self.FHANDLE := magic_open(Self.FFlags);
end;

// On destroy this class close the library.
destructor TMagic.Destroy;
begin
  magic_close(Self.FHANDLE);
  inherited Destroy;
end;

// When is defined the kind of detect is only mime type, or no.
procedure TMagic.SetOnlyMimeType(const AValue: boolean);
begin
  if (Self.FOnlyMimeType = AValue) then
  begin
    exit;
  end;

  Self.FOnlyMimeType := AValue;
  if (Self.FOnlyMimeType) then
  begin
    Self.FOnlyMimeEncoding := False;
    Self.FFlags := MAGIC_MIME_TYPE;
  end
  else if (not Self.FOnlyMimeEncoding) then
  begin
    Self.FFlags := MAGIC_MIME;
  end;
end;

// When is defined the kind of detect is only mime encoding, or no.
procedure TMagic.SetOnlyMimeEncoding(const AValue: boolean);
begin
  if (Self.FOnlyMimeEncoding = AValue) then
  begin
    exit;
  end;

  Self.FOnlyMimeEncoding := AValue;
  if (Self.FOnlyMimeEncoding) then
  begin
    Self.FOnlyMimeType := False;
    Self.FFlags := MAGIC_MIME_ENCODING;
  end
  else if (not Self.FOnlyMimeType) then
  begin
    Self.FFlags := MAGIC_MIME;
  end;
end;

// Before detect a mime of content.
// raises Exception if the file magic does not exist
procedure TMagic.BeforeDetect;
begin
  if (not FileExists(Self.FFileMagic)) then
  begin
    raise Exception.Create('File magic does not exist: ' + Self.FFileMagic);
  end;
  magic_load(Self.FHANDLE, PChar(Self.FFileMagic));
  magic_setflags(Self.FHANDLE, Self.FFlags);
end;

// Identify the contents of file 'filename'.
// raises Exception if the file does not exist
function TMagic.FromFile(filename: string): string;
begin
  Self.BeforeDetect();
  if (not FileExists(filename)) then
  begin
    raise Exception.Create('File does not exist: ' + filename);
  end;
  Result := magic_file(Self.FHANDLE, PChar(filename));
end;

// Identify the contents of buffer.
function TMagic.FromBuffer(var buffer; count: longint): string;
begin
  Self.BeforeDetect();
  Result := magic_buffer(Self.FHANDLE, buffer, count);
end;

end.

