unit FileIconWin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Windows, ShellApi,
  FileIcon;

type TFileIconWin = class(TFileIcon)
public
  function getIconByName(iconName:string; size:integer):TIcon; override;
  function getIconNameForFile(filename:string; size:integer):string; override;
  function getIconForFile(filename:string; size:integer):TIcon;override;
end;

implementation

function TFileIconWin.getIconNameForFile(filename:string; size:integer):string;
var
  mIcon: TIcon;
  myIconInfo: TIconInfo;
  FileInfo: SHFILEINFOW;
  pwc: array [0..1024] of WideChar;
  Flags: dword;
  r,i,iconIndex : integer;
  winBitmap: Windows.TBitmap;
  aBmp : Graphics.TBitmap;
  fTypeName: string;
begin
  StringToWideChar(fileName, pwc, length(fileName)*2);

  Flags := SHGFI_ICON or SHGFI_TYPENAME;
  if size > 16
     then Flags := Flags or SHGFI_LARGEICON
     else Flags := Flags or SHGFI_SMALLICON;

  r := SHGetFileInfo(@pwc, FILE_ATTRIBUTE_NORMAL, FileInfo, SizeOf(FileInfo), Flags);
  fTypeName := String(WideCharToString( FileInfo.szTypeName ));
  result := fTypeName;
end

function getIconByName(iconName:string; size:integer):TIcon; override;
begin
  //NO
end;

function TFileIconWin.getIconForFile(filename:string; size:integer;):TIcon; override;
var
  mIcon: TIcon;
  myIconInfo: TIconInfo;
  FileInfo: SHFILEINFOW;
  pwc: array [0..1024] of WideChar;
  Flags: dword;
  r,i,iconIndex : integer;
  winBitmap: Windows.TBitmap;
  aBmp : Graphics.TBitmap;
  fTypeName: string;
begin
  StringToWideChar(fileName, pwc, length(fileName)*2);

  Flags := SHGFI_ICON or SHGFI_TYPENAME;
  if size > 16
     then Flags := Flags or SHGFI_LARGEICON
     else Flags := Flags or SHGFI_SMALLICON;

  r := SHGetFileInfo(@pwc, FILE_ATTRIBUTE_NORMAL, FileInfo, SizeOf(FileInfo), Flags);
  fTypeName := String(WideCharToString( FileInfo.szTypeName ));

  mIcon := TIcon.Create;
  if (assigned(mIcon)) and (FileInfo.hIcon <> 0)
     and (GetIconInfo(FileInfo.hIcon, myIconInfo)) then
    begin
        aBmp := Graphics.TBitmap.Create;
        aBmp.LoadFromBitmapHandles(myIconInfo.hbmColor, myIconInfo.hbmMask, nil); // @r
        mIcon.Assign(aBmp); // there is no other way for TIcons
                            // - you MUST use a Bitmap, as it has implemented "Handles"-handling (setHandles)
                            // TIcons themselves cannot cope with handles
        aBmp.Free;
        iconIndex :=imageList.AddIcon(mIcon);
        FIconNamesMap.AddObject(fTypeName, TObject(PtrUint(iconIndex)));
        result := iconindex;
    end;
  end;

end;




end.

