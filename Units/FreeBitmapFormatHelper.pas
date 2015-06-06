{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2015-2015, Conversion to FPC/Lazarus by Mark Malakanov.                      }
{                                                                                             }
{    This program is free software; you can redistribute it and/or modify it under            }
{    the terms of the GNU General Public License as published by the                          }
{    Free Software Foundation; either version 2 of the License, or (at your option)           }
{    any later version.                                                                       }
{                                                                                             }
{    This program is distributed in the hope that it will be useful, but WITHOUT ANY          }
{    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A          }
{    PARTICULAR PURPOSE. See the GNU General Public License for more details.                 }
{                                                                                             }
{    You should have received a copy of the GNU General Public License along with             }
{    this program; if not, write to the Free Software Foundation, Inc.,                       }
{    59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                                    }
{                                                                                             }
{#############################################################################################}
unit FreeBitmapFormatHelper;

{$mode delphi}

interface

uses
  SysUtils, FreeTypes, Graphics, GraphType;

type
TBitMapDataFormat = (
 //bmdf_BPP1, // Black and White
 bmdf_BPP16_R5G6B5, // 16-bit
 bmdf_BPP16_B5G6R5, // 16-bit
 // Formats in RGB order
 bmdf_BPP24_R8G8B8,
 bmdf_BPP32_R8G8B8,
 bmdf_BPP32_A8R8G8B8,
 bmdf_BPP32_R8G8B8A8,
 // Formats in Windows pixels order: BGR
 bmdf_BPP24_B8G8R8,
 bmdf_BPP32_B8G8R8,
 bmdf_BPP32_B8G8R8A8,
 // Qt Standard format (see lcl/interfaces/qt/qtproc.pp)
 bmdf_BPP32_B8G8R8A8_r
 );

TRGB = packed record
   rgbRed  : BYTE;
   rgbGreen: BYTE;
   rgbBlue : BYTE;
end;
PRGB = ^TRGB;

TRGBA = packed record
   rgbaRed  : BYTE;
   rgbaGreen: BYTE;
   rgbaBlue : BYTE;
   rgbaAlpha : BYTE;
end;
PRGBA = ^TRGBA;

TARGB = packed record
   argbAlpha : BYTE;
   argbRed  : BYTE;
   argbGreen: BYTE;
   argbBlue : BYTE;
end;
PARGB = ^TARGB;

TBGR = packed record
   bgrBlue : BYTE;
   bgrGreen: BYTE;
   bgrRed  : BYTE;
end;
PBGR = ^TBGR;

TBGRA = packed record
   bgraBlue : BYTE;
   bgraGreen: BYTE;
   bgraRed  : BYTE;
   bgraAlpha : BYTE;
end;
PBGRA = ^TBGRA;

TFreeBitmapFormatHelper = class
  private
    FBytesPerPixel : integer;
    procedure TRGBTriple_to_BPP16_R5G6B5(C:TRGBTriple; var p:TColor);
    procedure TRGBTriple_to_BPP16_B5G6R5(C:TRGBTriple; var p:TColor);
    // Formats in RGB order
    procedure TRGBTriple_to_BPP24_R8G8B8(C:TRGBTriple; var p:TRGB);
    procedure TRGBTriple_to_BPP32_R8G8B8(C:TRGBTriple; var p:TRGBA);
    procedure TRGBTriple_to_BPP32_A8R8G8B8(C:TRGBTriple; var p:TARGB);
    procedure TRGBTriple_to_BPP32_R8G8B8A8(C:TRGBTriple; var p:TRGBA);
    // Formats in Windows pixels order: BGR
    procedure TRGBTriple_to_BPP24_B8G8R8(C:TRGBTriple; var p:TBGR); // yes, it is opposite in WIndows, RGB is BGR :)
    procedure TRGBTriple_to_BPP32_B8G8R8(C:TRGBTriple; var p:TBGRA);
    procedure TRGBTriple_to_BPP32_B8G8R8A8(C:TRGBTriple; var p:TBGRA);
    // Qt Standard format (see lcl/interfaces/qt/qtproc.pp)
    procedure TRGBTriple_to_BPP32_B8G8R8A8_r(C:TRGBTriple; var p:TBGRA);

{    // 16-bits formats
    function TRGBTriple_to_BPP16_R5G6B5(C:TRGBTriple):TColor;
     // Formats in RGB order
    function TRGBTriple_to_BPP24_R8G8B8(C:TRGBTriple):TRGB;
    function TRGBTriple_to_BPP32_R8G8B8(C:TRGBTriple):TRGBA;
    function TRGBTriple_to_BPP32_A8R8G8B8(C:TRGBTriple):TARGB;
    function TRGBTriple_to_BPP32_R8G8B8A8(C:TRGBTriple):TRGBA;
     // Formats in Windows pixels order: BGR
    function TRGBTriple_to_BPP24_B8G8R8(C:TRGBTriple):TBGR; // yes, it is opposite in WIndows, RGB is BGR :)
    function TRGBTriple_to_BPP32_B8G8R8A8(C:TRGBTriple):TBGRA;
}
    // 16-bits formats
    function BPP16_R5G6B5_to_TRGBTriple(C:TColor):TRGBTriple;
    function BPP16_B5G6R5_to_TRGBTriple(C:TColor):TRGBTriple;
    // Formats in RGB order
    function BPP24_R8G8B8_to_TRGBTriple(C:TRGB):TRGBTriple;
    function BPP32_R8G8B8_to_TRGBTriple(C:TRGBA):TRGBTriple;
    function BPP32_A8R8G8B8_to_TRGBTriple(C:TARGB):TRGBTriple;
    function BPP32_R8G8B8A8_to_TRGBTriple(C:TRGBA):TRGBTriple;
    // Formats in Windows pixels order: BGR
    function BPP24_B8G8R8_to_TRGBTriple(C:TBGR):TRGBTriple; // yes, it is opposite in WIndows, RGB is BGR :)
    function BPP32_B8G8R8_to_TRGBTriple(C:TBGRA):TRGBTriple;
    function BPP32_B8G8R8A8_to_TRGBTriple(C:TBGRA):TRGBTriple;
  public
    FBitMapDataFormat:TBitMapDataFormat;
    constructor Create(aBitmap:TBitmap);
    procedure DetectBitMapDataFormat(ABitmap:TBitmap);
    function ToTRGBTriple(p:pointer):TRGBTriple;
    procedure FromTRGBTriple(c:TRGBTriple; p:pointer);
    property BytesPerPixel : integer read FBytesPerPixel;
end;

implementation

constructor TFreeBitmapFormatHelper.Create(aBitmap:TBitmap);
begin
  inherited Create;
  if Assigned(aBitmap) then
    DetectBitMapDataFormat(aBitmap);
end;

procedure TFreeBitmapFormatHelper.DetectBitMapDataFormat(ABitmap:TBitmap);
var Desc : TRawImageDescription;
begin
  FBitMapDataFormat := bmdf_BPP24_B8G8R8; //default Windows
  Desc := ABitmap.RawImage.Description;
  FBytesPerPixel := Desc.BitsPerPixel div 8;

  With Desc do
  begin
    if
      (Format = ricfRGBA) and (PaletteColorCount = 0)
      and (Depth = 16) // used bits per pixel
      and (BitOrder = riboBitsInOrder)
      and (ByteOrder = riboLSBFirst)
      and (BitsPerPixel = 16) // bits per pixel. can be greater than Depth.
      and (RedPrec = 5) // red precision. bits for red
      and (RedShift = 0)
      and (GreenPrec = 6)
      and (GreenShift = 5) // bitshift. Direction from least to most significant
      and (BluePrec = 5)
      and (BlueShift=11)
      and (AlphaPrec=0)
      and (MaskBitsPerPixel=0)
    then FBitMapDataFormat := bmdf_BPP16_R5G6B5
    else if
      (Format = ricfRGBA) and (PaletteColorCount = 0)
      and (Depth = 16) // used bits per pixel
      and (BitOrder = riboBitsInOrder)
      and (ByteOrder = riboLSBFirst)
      and (BitsPerPixel = 16) // bits per pixel. can be greater than Depth.
      and (RedPrec = 5) // red precision. bits for red
      and (RedShift = 11)
      and (GreenPrec = 6)
      and (GreenShift = 5) // bitshift. Direction from least to most significant
      and (BluePrec = 5)
      and (BlueShift=0)
      and (AlphaPrec=0)
      and (MaskBitsPerPixel=0)
    then FBitMapDataFormat := bmdf_BPP16_B5G6R5
    else if
      (Format = ricfRGBA) and (PaletteColorCount = 0)
      and (Depth = 24) // used bits per pixel
      and (BitOrder = riboBitsInOrder)
      and (ByteOrder = riboLSBFirst)
      and (BitsPerPixel = 24) // bits per pixel. can be greater than Depth.
      and (RedPrec = 8) // red precision. bits for red
      and (RedShift = 0)
      and (GreenPrec = 8)
      and (GreenShift = 8) // bitshift. Direction from least to most significant
      and (BluePrec = 8)
      and (BlueShift=16)
      and (AlphaPrec=0)
      and (MaskBitsPerPixel=0)
    then FBitMapDataFormat := bmdf_BPP24_R8G8B8
    else if
      (Format = ricfRGBA) and (PaletteColorCount = 0)
      and (Depth = 24) // used bits per pixel
      and (BitOrder = riboBitsInOrder)
      and (ByteOrder = riboLSBFirst)
      and (BitsPerPixel = 32) // bits per pixel. can be greater than Depth.
      and (RedPrec = 8) // red precision. bits for red
      and (RedShift = 0)
      and (GreenPrec = 8)
      and (GreenShift = 8) // bitshift. Direction from least to most significant
      and (BluePrec = 8)
      and (BlueShift=16)
      and (AlphaPrec=0)
      //and (MaskBitsPerPixel=0)
    then FBitMapDataFormat := bmdf_BPP32_R8G8B8
    else if
      (Format = ricfRGBA) and (PaletteColorCount = 0)
      and (Depth = 32) // used bits per pixel
      and (BitOrder = riboBitsInOrder)
      and (ByteOrder = riboLSBFirst)
      and (BitsPerPixel = 32) // bits per pixel. can be greater than Depth.
      and (RedPrec = 8) // red precision. bits for red
      and (RedShift = 8)
      and (GreenPrec = 8)
      and (GreenShift = 16) // bitshift. Direction from least to most significant
      and (BluePrec = 8)
      and (BlueShift=24)
      and (AlphaPrec=8)
      and (AlphaShift=0)
      and (MaskBitsPerPixel=0)
    then FBitMapDataFormat := bmdf_BPP32_A8R8G8B8
    else if
      (Format = ricfRGBA) and (PaletteColorCount = 0)
      and (Depth = 32) // used bits per pixel
      and (BitOrder = riboBitsInOrder)
      and (ByteOrder = riboLSBFirst)
      and (BitsPerPixel = 32) // bits per pixel. can be greater than Depth.
      and (RedPrec = 8) // red precision. bits for red
      and (RedShift = 0)
      and (GreenPrec = 8)
      and (GreenShift = 8) // bitshift. Direction from least to most significant
      and (BluePrec = 8)
      and (BlueShift=16)
      and (AlphaPrec=8)
      and (AlphaShift=24)
      and (MaskBitsPerPixel=0)
    then FBitMapDataFormat := bmdf_BPP32_R8G8B8A8
    else if
      (Format = ricfRGBA) and (PaletteColorCount = 0)
      and (Depth = 24) // used bits per pixel
      and (BitOrder = riboBitsInOrder)
      and (ByteOrder = DefaultByteOrder)
      and (BitsPerPixel = 24) // bits per pixel. can be greater than Depth.
      and (RedPrec = 8) // red precision. bits for red
      and (RedShift = 16)
      and (GreenPrec = 8)
      and (GreenShift = 8) // bitshift. Direction from least to most significant
      and (BluePrec = 8)
      and (BlueShift=0)
      and (AlphaPrec=0)
      and (AlphaShift=0)
      and (MaskBitsPerPixel=0)
    then FBitMapDataFormat := bmdf_BPP24_B8G8R8
    else if
      (Format = ricfRGBA) and (PaletteColorCount = 0)
      and (Depth = 24) // used bits per pixel
      and (BitOrder = riboBitsInOrder)
      and (ByteOrder = DefaultByteOrder)
      and (BitsPerPixel = 32) // bits per pixel. can be greater than Depth.
      and (RedPrec = 8) // red precision. bits for red
      and (RedShift = 16)
      and (GreenPrec = 8)
      and (GreenShift = 8) // bitshift. Direction from least to most significant
      and (BluePrec = 8)
      and (BlueShift=0)
      and (AlphaPrec=0)
      and (AlphaShift=0)
      //and (MaskBitsPerPixel=0) // Ignore mask, we do not need it for our purposes
    then FBitMapDataFormat := bmdf_BPP32_B8G8R8
    else if
      (Format = ricfRGBA) and (PaletteColorCount = 0)
      and (Depth = 32) // used bits per pixel
      and (ByteOrder = DefaultByteOrder)
      and (BitsPerPixel = 32) // bits per pixel. can be greater than Depth.
      and (RedPrec = 8) // red precision. bits for red
      and (RedShift = 16)
      and (GreenPrec = 8)
      and (GreenShift = 8) // bitshift. Direction from least to most significant
      and (BluePrec = 8)
      and (BlueShift=0)
      and (AlphaPrec=8)
      and (AlphaShift=24)
      and (MaskBitsPerPixel=0)
    then
      begin;
      if (BitOrder = riboBitsInOrder) then
        FBitMapDataFormat := bmdf_BPP32_B8G8R8A8
        else
          FBitMapDataFormat := bmdf_BPP32_B8G8R8A8_r;
      end
    else
      raise Exception.Create('Unsupported bitmap format:'+Desc.AsString);
  end;
end;

function TFreeBitmapFormatHelper.ToTRGBTriple(p:pointer):TRGBTriple;
begin
  case FBitMapDataFormat of
    bmdf_BPP16_R5G6B5:   result := BPP16_R5G6B5_to_TRGBTriple(TColor(p^));
    bmdf_BPP16_B5G6R5:   result := BPP16_B5G6R5_to_TRGBTriple(TColor(p^));
    bmdf_BPP24_R8G8B8:   result := BPP24_R8G8B8_to_TRGBTriple(TRGB(p^));
    bmdf_BPP32_A8R8G8B8: result := BPP32_A8R8G8B8_to_TRGBTriple(TARGB(p^));
    bmdf_BPP32_R8G8B8A8: result := BPP32_R8G8B8A8_to_TRGBTriple(TRGBA(p^));
    bmdf_BPP24_B8G8R8:   result := BPP24_B8G8R8_to_TRGBTriple(TBGR(p^));
    bmdf_BPP32_B8G8R8:   result := BPP32_B8G8R8_to_TRGBTriple(TBGRA(p^));
    bmdf_BPP32_B8G8R8A8: result := BPP32_B8G8R8A8_to_TRGBTriple(TBGRA(p^));
    bmdf_BPP32_B8G8R8A8_r: result := BPP32_B8G8R8A8_to_TRGBTriple(TBGRA(p^))
  end;
end;

procedure TFreeBitmapFormatHelper.FromTRGBTriple(c:TRGBTriple; p:pointer);
begin
  case FBitMapDataFormat of
    bmdf_BPP16_R5G6B5:   TRGBTriple_to_BPP16_R5G6B5(C,TColor(p^));
    bmdf_BPP24_R8G8B8:   TRGBTriple_to_BPP24_R8G8B8(C,TRGB(p^));
    bmdf_BPP32_A8R8G8B8: TRGBTriple_to_BPP32_A8R8G8B8(C,TARGB(p^));
    bmdf_BPP32_R8G8B8A8: TRGBTriple_to_BPP32_R8G8B8A8(C,TRGBA(p^));
    bmdf_BPP24_B8G8R8:   TRGBTriple_to_BPP24_B8G8R8(C,TBGR(p^));
    bmdf_BPP32_B8G8R8:   TRGBTriple_to_BPP32_B8G8R8(C,TBGRA(p^));
    bmdf_BPP32_B8G8R8A8: TRGBTriple_to_BPP32_B8G8R8A8(C,TBGRA(p^));
    bmdf_BPP32_B8G8R8A8_r: TRGBTriple_to_BPP32_B8G8R8A8_r(C,TBGRA(p^));
  end;
end;


procedure TFreeBitmapFormatHelper.TRGBTriple_to_BPP16_R5G6B5(C:TRGBTriple; var p:TColor);
begin
  p := RGBToColor(C.rgbtRed, C.rgbtGreen, C.rgbtBlue);
end;

procedure TFreeBitmapFormatHelper.TRGBTriple_to_BPP16_B5G6R5(C:TRGBTriple; var p:TColor);
begin
  p := RGBToColor(C.rgbtBlue, C.rgbtGreen, C.rgbtRed);
end;

// Formats in RGB order
procedure TFreeBitmapFormatHelper.TRGBTriple_to_BPP24_R8G8B8(C:TRGBTriple; var p:TRGB);
begin
  p.rgbRed   := C.rgbtRed;
  p.rgbGreen := C.rgbtGreen;
  p.rgbBlue  := C.rgbtBlue;
end;

procedure TFreeBitmapFormatHelper.TRGBTriple_to_BPP32_R8G8B8(C:TRGBTriple; var p:TRGBA);
begin
  p.rgbaRed   := C.rgbtRed;
  p.rgbaGreen := C.rgbtGreen;
  p.rgbaBlue  := C.rgbtBlue;
  p.rgbaAlpha:= 0;
end;

procedure TFreeBitmapFormatHelper.TRGBTriple_to_BPP32_A8R8G8B8(C:TRGBTriple; var p:TARGB);
begin
  p.argbRed   := C.rgbtRed;
  p.argbGreen := C.rgbtGreen;
  p.argbBlue  := C.rgbtBlue;
  p.argbAlpha:= 0;
end;

procedure TFreeBitmapFormatHelper.TRGBTriple_to_BPP32_R8G8B8A8(C:TRGBTriple; var p:TRGBA);
begin
  p.rgbaRed   := C.rgbtRed;
  p.rgbaGreen := C.rgbtGreen;
  p.rgbaBlue  := C.rgbtBlue;
  p.rgbaAlpha:= 0;
end;

// Formats in Windows pixels order: BGR
procedure TFreeBitmapFormatHelper.TRGBTriple_to_BPP24_B8G8R8(C:TRGBTriple; var p:TBGR); // yes, it is opposite in WIndows, RGB is BGR :)
begin
  p.bgrBlue  := C.rgbtBlue;
  p.bgrGreen := C.rgbtGreen;
  p.bgrRed   := C.rgbtRed;
end;

procedure TFreeBitmapFormatHelper.TRGBTriple_to_BPP32_B8G8R8(C:TRGBTriple; var p:TBGRA);
begin
  p.bgraBlue  := C.rgbtBlue;
  p.bgraGreen := C.rgbtGreen;
  p.bgraRed   := C.rgbtRed;
  p.bgraAlpha := 0;
end;

procedure TFreeBitmapFormatHelper.TRGBTriple_to_BPP32_B8G8R8A8(C:TRGBTriple; var p:TBGRA);
begin
  p.bgraBlue  := C.rgbtBlue;
  p.bgraGreen := C.rgbtGreen;
  p.bgraRed   := C.rgbtRed;
  p.bgraAlpha := 0;
end;

procedure TFreeBitmapFormatHelper.TRGBTriple_to_BPP32_B8G8R8A8_r(C:TRGBTriple; var p:TBGRA);
begin
  p.bgraBlue  := C.rgbtBlue;
  p.bgraGreen := C.rgbtGreen;
  p.bgraRed   := C.rgbtRed;
  p.bgraAlpha := 255;
end;


{
function TFreeBitmapFormatHelper.TRGBTriple_to_BPP16_R5G6B5(C:TRGBTriple):TColor;
begin
  result := RGBToColor(C.rgbtRed, C.rgbtGreen, C.rgbtBlue);
end;
 // Formats in RGB order
function TFreeBitmapFormatHelper.TRGBTriple_to_BPP24_R8G8B8(C:TRGBTriple):TRGB;
begin
  result.rgbRed   := C.rgbtRed;
  result.rgbGreen := C.rgbtGreen;
  result.rgbBlue  := C.rgbtBlue;
end;

function TFreeBitmapFormatHelper.TRGBTriple_to_BPP32_R8G8B8(C:TRGBTriple):TRGBA;
begin
  result.rgbaRed   := C.rgbtRed;
  result.rgbaGreen := C.rgbtGreen;
  result.rgbaBlue  := C.rgbtBlue;
  result.rgbaAlpha:= 0;
end;

function TFreeBitmapFormatHelper.TRGBTriple_to_BPP32_A8R8G8B8(C:TRGBTriple):TARGB;
begin
  result.argbRed   := C.rgbtRed;
  result.argbGreen := C.rgbtGreen;
  result.argbBlue  := C.rgbtBlue;
  result.argbAlpha:= 0;
end;

function TFreeBitmapFormatHelper.TRGBTriple_to_BPP32_R8G8B8A8(C:TRGBTriple):TRGBA;
begin
  result.rgbaRed   := C.rgbtRed;
  result.rgbaGreen := C.rgbtGreen;
  result.rgbaBlue  := C.rgbtBlue;
  result.rgbaAlpha:= 0;
end;
 // Formats in Windows pixels order: BGR
function TFreeBitmapFormatHelper.TRGBTriple_to_BPP24_B8G8R8(C:TRGBTriple):TBGR; // yes, it is opposite in WIndows, RGB is BGR :)
begin
  result.bgrBlue  := C.rgbtBlue;
  result.bgrGreen := C.rgbtGreen;
  result.bgrRed   := C.rgbtRed;
end;

function TFreeBitmapFormatHelper.TRGBTriple_to_BPP32_B8G8R8A8(C:TRGBTriple):TBGRA;
begin
  result.bgraRed   := C.rgbtRed;
  result.bgraGreen := C.rgbtGreen;
  result.bgraBlue  := C.rgbtBlue;
end;
}

// 16-bits formats
function TFreeBitmapFormatHelper.BPP16_R5G6B5_to_TRGBTriple(C:TColor):TRGBTriple;
begin
  result.rgbtRed:=C and $00001F;
  result.rgbtGreen:=(C shr 5) and $00003F;
  result.rgbtBlue:=(C shr 11) and $00001F;
end;
function TFreeBitmapFormatHelper.BPP16_B5G6R5_to_TRGBTriple(C:TColor):TRGBTriple;
begin
  result.rgbtBlue:=C and $00001F;
  result.rgbtGreen:=(C shr 5) and $00003F;
  result.rgbtRed:=(C shr 11) and $00001F;
end;

// Formats in RGB order
function TFreeBitmapFormatHelper.BPP24_R8G8B8_to_TRGBTriple(C:TRGB):TRGBTriple;
begin
  result.rgbtBlue:=C.rgbBlue;
  result.rgbtGreen:=C.rgbGreen;
  result.rgbtRed:=C.rgbRed;
end;

function TFreeBitmapFormatHelper.BPP32_R8G8B8_to_TRGBTriple(C:TRGBA):TRGBTriple;
begin
  result.rgbtBlue:=C.rgbaBlue;
  result.rgbtGreen:=C.rgbaGreen;
  result.rgbtRed:=C.rgbaRed;
end;

function TFreeBitmapFormatHelper.BPP32_A8R8G8B8_to_TRGBTriple(C:TARGB):TRGBTriple;
begin
  result.rgbtBlue:=C.argbBlue;
  result.rgbtGreen:=C.argbGreen;
  result.rgbtRed:=C.argbRed;
end;

function TFreeBitmapFormatHelper.BPP32_R8G8B8A8_to_TRGBTriple(C:TRGBA):TRGBTriple;
begin
  result.rgbtBlue:=C.rgbaBlue;
  result.rgbtGreen:=C.rgbaGreen;
  result.rgbtRed:=C.rgbaRed;
end;

// Formats in Windows pixels order: BGR
function TFreeBitmapFormatHelper.BPP24_B8G8R8_to_TRGBTriple(C:TBGR):TRGBTriple; // yes, it is opposite in WIndows, RGB is BGR :)
begin
  result.rgbtBlue := C.bgrBlue;
  result.rgbtGreen:= C.bgrGreen;
  result.rgbtRed  := C.bgrRed;
end;

function TFreeBitmapFormatHelper.BPP32_B8G8R8_to_TRGBTriple(C:TBGRA):TRGBTriple;
begin
  result.rgbtBlue:=C.bgraBlue;
  result.rgbtGreen:=C.bgraGreen;
  result.rgbtRed:=C.bgraRed;
end;

function TFreeBitmapFormatHelper.BPP32_B8G8R8A8_to_TRGBTriple(C:TBGRA):TRGBTriple;
begin
  result.rgbtBlue:=C.bgraBlue;
  result.rgbtGreen:=C.bgraGreen;
  result.rgbtRed:=C.bgraRed;
end;


end.

