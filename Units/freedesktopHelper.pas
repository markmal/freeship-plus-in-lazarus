unit freedesktopHelper;

{$mode fpc}{$H+}

interface

uses
  Classes, SysUtils, Gtk;

type

  FreedesktopHelper = class

  public     { Public declarations }
     Home:string;
     XDGTheme:string;
     XDGDataDirs:TStringList;

     constructor Create; override;
     destructor Destroy; override;

     function FindIconHelper(icon, size, scale, theme:string):TIcon;
     function FindIcon(icon, size, scale:string):TIcon;
  end

implementation

constructor FreedesktopHelper.Create();
var XDGDataDirsS: string;
begin
  // https://specifications.freedesktop.org/icon-theme-spec/icon-theme-spec-latest.html
  // Icons and themes are looked for in a set of directories.
  // By default, apps should look in $HOME/.icons (for backwards compatibility),
  // in $XDG_DATA_DIRS/icons and in /usr/share/pixmaps (in that order).
  Home := GetEnvironmentVariable('HOME');
  XDGTheme := 'highcolor';
  XDGDataDirsS := GetEnvironmentVariable('XDG_DATA_DIRS');
  XDGDataDirs := XDGDataDirsS.split(':');
  XDGDataDirs.Insert(Home+'/.icons',0);
  XDGDataDirs.Append(Home+'/usr/share/pixmaps',0);
end

destructor FreedesktopHelper.Destroy;
begin
end

function FreedesktopHelper.FindIcon(icon, size, scale:string):TIcon;
begin
  FindIconHelper(icon, size, scale, user selected theme);
  if filename != none
    return filename

  filename = FindIconHelper(icon, size, scale, "hicolor");
  if filename != none
    return filename

  return LookupFallbackIcon (icon)
end

function FreedesktopHelper.FindIcon(icon, size, scale:string):TIcon;
begin
end


end.

