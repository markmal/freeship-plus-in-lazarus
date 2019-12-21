unit FileIconWin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls,
  Windows, ShellApi,
  FileIcon;

type TFileIconWin = class(TFileIconAdapter)
protected
  function getIconNameForFile(filename:string; size:integer):string; override;
  function getIconForFile(filename:string; size:integer):TIcon;override;

  function getFileInfo(filename:string; size:integer):SHFILEINFOW;
  function getIconName(FileInfo : SHFILEINFOW):string;
  function getIcon(FileInfo : SHFILEINFOW):TIcon;

public
  function addIconsForFile(filename:string):integer; override;

end;

implementation

function TFileIconWin.getFileInfo(filename:string; size:integer):SHFILEINFOW;
var
  pwc: array [0..1024] of WideChar;
  Flags: dword;
  r : integer;
  FileInfo : SHFILEINFOW;
begin
  StringToWideChar(fileName, pwc, length(fileName)*2);

  Flags := SHGFI_ICON or SHGFI_TYPENAME;
  if size > 16
     then Flags := Flags or SHGFI_LARGEICON
     else Flags := Flags or SHGFI_SMALLICON;

  r := SHGetFileInfo(@pwc, FILE_ATTRIBUTE_NORMAL, FileInfo, SizeOf(FileInfo), Flags);
  result := FileInfo;
end;

function TFileIconWin.getIconName(FileInfo : SHFILEINFOW):string;
begin
  result := String(WideCharToString( FileInfo.szTypeName ));
end;

function TFileIconWin.getIcon(FileInfo : SHFILEINFOW):TIcon;
var
  mIcon: TIcon;
  myIconInfo: TIconInfo;
  r,i,iconIndex : integer;
  winBitmap: Windows.TBitmap;
  aBmp : Graphics.TBitmap;
  iconName: string;
begin
  mIcon := TIcon.Create;
  if (assigned(mIcon)) and (FileInfo.hIcon <> 0)
     and (GetIconInfo(FileInfo.hIcon, myIconInfo)) then
    begin
        aBmp := Graphics.TBitmap.Create;
        aBmp.LoadFromBitmapHandles(myIconInfo.hbmColor, myIconInfo.hbmMask, nil); // @r
        mIcon.Assign(aBmp); // there is no other way for TIcons
                            // - you MUST use a Bitmap, as it has implemented "Handles"
                            // - handling (setHandles)
                            // TIcons themselves cannot cope with handles
        aBmp.Free;
        result := mIcon;
    end;
end;


function TFileIconWin.getIconNameForFile(filename:string; size:integer):string;
var
  FileInfo : SHFILEINFOW;
begin
  FileInfo := getFileInfo(filename, size);
  result := getIconName(FileInfo);
end;

function TFileIconWin.getIconForFile(filename:string; size:integer):TIcon;
var
  FileInfo: SHFILEINFOW;
begin
  FileInfo := getFileInfo(filename, size);
  result := getIcon(FileInfo);
end;

// checks if icon is already cached and adds an icon if it is not cached,
// returns icon index in LargeIconList and SmallIconList
function TFileIconWin.addIconsForFile(filename:string):integer;
var iconName:string; i,i1,i2,iconIndex:integer;
    smallIcon, largeIcon: TIcon;
    FileInfoSmall, FileInfoLarge : SHFILEINFOW;
begin
  result:=-1;
  FileInfoSmall := getFileInfo(filename, 16);
  iconName := getIconName(FileInfoSmall);

  // check if icon is registered in iconName to IconIndex map
  i := FIconNamesMap.IndexOf(iconName);
  if (i>-1) then
    begin
    iconIndex := PtrUInt(FIconNamesMap.Objects[i]);
    result := iconIndex;
    end
  else
    begin
    iconIndex := -1;
    smallIcon := getIcon(FileInfoSmall);
    FileInfoLarge := getFileInfo(filename, 32);
    largeIcon := getIcon(FileInfoLarge);
    if assigned(smallIcon) and assigned(largeIcon) then
      begin
      i1 := FSmallImageList.AddIcon(smallIcon);
      i2 := FLargeImageList.AddIcon(largeIcon);
      iconIndex := i1;
      FIconNamesMap.AddObject(iconName, TObject(PtrUint(iconIndex)));
      result := iconIndex;
      end;
    end;
end;


end.

