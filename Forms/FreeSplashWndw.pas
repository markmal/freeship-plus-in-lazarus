{#############################################################################################}
{    This code is distributed as part of the FREE!ship Plus project. FREE!ship Plus is an     }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }

{    Copyright © 2005,      by Martijn van Engeland                                           }
{    Copyright © 2007-2012, by Timoshenko Victor F.                                           }
{    e-mail                  : vftim@rambler.ru, tvf@pisem.net                                }
{    FREE!ship project page  : http://freeship-plus.land.ru                                   }
{    FREE!ship homepage      : http://freeship-plus.pisem.su                                  }

{    This program is free software; you can redistribute it and/or modify it under            }
{    the terms of the GNU General Public License as published by the                          }
{    Free Software Foundation; either version 2 of the License, or (at your option)           }
{    any later version.                                                                       }

{    This program is distributed in the hope that it will be useful, but WITHOUT ANY          }
{    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A          }
{    PARTICULAR PURPOSE. See the GNU General Public License for more details.                 }

{    You should have received a copy of the GNU General Public License along with             }
{    this program; if not, write to the Free Software Foundation, Inc.,                       }
{    59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                                    }

{#############################################################################################}

unit FreeSplashWndw;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFDEF Windows}
  ShellAPI, {$ifndef LCL} jpeg, {$endif} Windows,
{$ENDIF}
{$IFDEF LCL}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  FreeGeometry,
  FreeVersionUnit,
  StdCtrls,
  FreeLogger,
  FreeShipUnit,
  FreeTYpes;

const
  SPLASH_TIME = 5000;

type

  { TFreeSplashWindow }

  TFreeSplashWindow = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Image1: TImage;
    Image2: TImage;
    Label2: TLabel;
    LabelCondition: TLabel;
    LabelBuildInfo: TLabel;
    LabelWarranty: TLabel;
    LabelWarranty1: TLabel;
    ButtonPanel: TPanel;
    Panel1: TPanel;
    PanelWarranty: TPanel;
    PanelCopyrights: TPanel;
    VersionPanel: TPanel;
    TopLeftPanel: TPanel;
    TopPanel: TPanel;
    Timer: TTimer;
    LabelVersion: TLabel;
    _Label2: TLabel;
    LabelRelease: TLabel;
    LabelAppName: TLabel;
    _Label4: TLabel;
    _Label5: TLabel;
    _label8: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState;
      X, Y: integer);
    procedure Image1MouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState;
      X, Y: integer);
    procedure PanelCopyrightsClick(
      Sender: TObject);
    procedure TopPanelClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject;
      var Action: TCloseAction);
    procedure Image1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure _Label2Click(Sender: TObject);
    procedure _Label5Click(Sender: TObject);
    procedure _Label4Click(Sender: TObject);
  private   { Private declarations }
    //                                  FOwner         : TFreeship;
    FCounter: integer;
  public    { Public declarations }
    FreeShip: TFreeShip;
    procedure ShowGPL;
    procedure ExpandToFit;

    //                                  FExeDirectory :string;
  end;

var
  FreeSplashWindow: TFreeSplashWindow;

procedure SetTransparentForm(form: TForm; AValue: byte = 0);

implementation

uses FreeLanguageSupport;

{$IFnDEF FPC}
  {$R *.dfm}

{$ELSE}
  {$R *.lfm}
{$ENDIF}

const
  WS_EX_LAYERED = $80000;
  LWA_COLORKEY = 1;
  LWA_ALPHA = 2;

type
  TSetLayeredWindowAttributes = function(hwnd: HWND;
    // handle to the layered window
    crKey: TColor;      // specifies the color key
    bAlpha: byte;       // value for the blend function
    dwFlags: DWORD      // action
    ): BOOL; stdcall;

{$IFNDEF LCL}
procedure SetTransparentForm(form: TForm; AValue: byte = 0);
var
  Info: TOSVersionInfo;
  SetLayeredWindowAttributes: TSetLayeredWindowAttributes;
begin
  //Check Windows version
  Info.dwOSVersionInfoSize := SizeOf(Info);
  GetVersionEx(Info);
  if (Info.dwPlatformId = VER_PLATFORM_WIN32_NT) and (Info.dwMajorVersion >= 5) then
  begin
    SetLayeredWindowAttributes :=
      GetProcAddress(GetModulehandle(user32), 'SetLayeredWindowAttributes');
    if Assigned(SetLayeredWindowAttributes) then
    begin
      SetWindowLong(AHandle, GWL_EXSTYLE, GetWindowLong(form.Handle, GWL_EXSTYLE) or
        WS_EX_LAYERED);
      //Make form transparent
      SetLayeredWindowAttributes(AHandle, 0, AValue, LWA_ALPHA);
    end;
  end;
end;

{$ELSE}
procedure SetTransparentForm(form: TForm; AValue: byte = 0);
begin
  form.AlphaBlendValue := AValue;
end;

{$ENDIF}

procedure TFreeSplashWindow.TimerTimer(Sender: TObject);
var
  TI: integer;
  ABV: integer;
  ABD: integer;
begin
  if not Timer.Enabled then exit;
  TI := Timer.Interval;
  Inc(FCounter, TI);
  ABD := 255 div TI;

  if AlphaBlend then
  begin
    ABV := AlphaBlendValue;

    if (FCounter < 1000) then
      Inc(ABV, ABD);
    if (ABV > 255) then
      ABV := 255;

    if (FCounter >= (SPLASH_TIME - 1000)) then
      Dec(ABV, ABD);
    if (ABV < 1) then
      ABV := 1;

    //LabelCondition.Caption:=IntToStr(ABV);
    //Label2.Update;
    AlphaBlendValue := ABV;
    //Update;
    //Application.ProcessMessages;
  end;
  //Self.SetFocus;
  //Self.SetZOrder(true);
  //Update;
  Application.ProcessMessages; // there maight be no message loop yet
  //Logger.Debug('in TFreeSplashWindow.TimerTimer: ABV='+IntToStr(ABV));

  if FCounter > SPLASH_TIME then
  begin
    //Visible:=false;
    ////Close;
    Application.ProcessMessages; // there maight be no message loop yet
  end;

  if not Timer.Enabled then exit;
  ExpandToFit;

end;{TFreeSplashWindow.TimerTimer}

procedure TFreeSplashWindow.ExpandToFit;
var
  diff: integer;
begin
  diff := LabelCondition.BoundsRect.Bottom - self.Height;
  diff := TopPanel.Height + Image1.Picture.Bitmap.Height
    + PanelCopyrights.Height + PanelWarranty .Height
    + 30 - self.Height;
  if diff > 0 then
  begin
    Width := Width + diff;
    Height := Height + diff;
    Position:=poScreenCenter;
    Position:=poMainFormCenter;
  end;
end;

procedure TFreeSplashWindow.Image1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  // This should hold splash window until mouse button released
  AlphaBlendValue := 255;
  Update;
  Timer.Enabled := False;
  Timer.OnTimer:=nil;
end;

procedure TFreeSplashWindow.CheckBox1Change(Sender: TObject);
begin
  ModalResult := mrOk;
  AlphaBlendValue := 255;
  Update;
  Timer.Enabled := False;
  Timer.OnTimer:=nil;
end;

procedure TFreeSplashWindow.Button1Click(Sender: TObject);
begin
  ShowGPL;
end;

procedure TFreeSplashWindow.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFreeSplashWindow.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  // close splash window when mouse button released
  Close;
end;

procedure TFreeSplashWindow.PanelCopyrightsClick(Sender: TObject);
begin

end;

procedure TFreeSplashWindow.TopPanelClick(Sender: TObject);
begin

end;

procedure TFreeSplashWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer.Enabled := False;
  Release;
end;{TFreeSplashWindow.FormClose}

procedure TFreeSplashWindow.Image1Click(Sender: TObject);
begin
  //Timer.Enabled:=False;
  //Close;
end;{TFreeSplashWindow.Image1Click}

resourcestring
  rsVersion = 'Version';
  rsRelease = 'Release';
  rsBuild = 'Build';

procedure TFreeSplashWindow.FormShow(Sender: TObject);
var
  Str: string;
begin
  LabelVersion.Caption := rsVersion + ': ' + FREESHIP_MAJOR_VERSION;
  LabelRelease.Caption := rsRelease + ': ' + ReleasedDate;
  LabelBuildInfo.Caption := rsBuild + ': ' + ResourceVersionInfo + ' ' +
    COMPILE_DATE + ' ' + COMPILE_TIME + ' ' + TARGET_CPU + ' ' + TARGET_OS;
  Str := '';
  if CurrentLanguage <> nil then
  begin
    Str := CurrentLanguage.ReadString('Translation', 'Author', '');
    if Uppercase(Str) = Uppercase('Translation: <Your name>') then
      str := '';
    _Label8.Caption := Str;
  end;
  _Label8.Visible := Str <> '';
  FCounter := 0;
  Timer.Enabled := True;
  Caption := '';

  if AlphaBlend then
    AlphaBlendValue := 1
  else
    AlphaBlendValue := 255;
end;{TFreeSplashWindow.FormShow}

procedure TFreeSplashWindow._Label2Click(Sender: TObject);
begin

end;

procedure TFreeSplashWindow._Label5Click(Sender: TObject);
begin
  // Open FREE!ship homepage in webbrowser
  // Skip translation
  OpenURL('http://hydronship.net'); { *Converted from ShellExecute* }
  // End Skip translation
end;{TFreeSplashWindow.Label5Click}

procedure TFreeSplashWindow._Label4Click(Sender: TObject);
begin
  // Send an email
  // Skip translation
  OpenDocument('mailto:vftim@rambler.ru?subject=FREE!ship+');
  { *Converted from ShellExecute* }
  // End Skip translation
end;{TFreeSplashWindow.Label4Click}

procedure TFreeSplashWindow.ShowGPL;
var FLang, FManDirectory, man, FileToFind:string;
begin
  FLang := Freeship.Preferences.Language;
  FManDirectory := Freeship.Preferences.ManualsDirectory;
  man := 'GPL-'+FLang;
  FileToFind := FileSearch(FManDirectory+DirectorySeparator+man+'.html',FManDirectory);
  if (FileToFind='') then
     FileToFind := FileSearch(FManDirectory+DirectorySeparator+man+'.pdf',FManDirectory);
  if (FileToFind='') then
     FileToFind := FileSearch(FManDirectory+DirectorySeparator+man+'.txt',FManDirectory);

  if (FileToFind='') and (FLang<>'English') then begin
     MessageDlg('Manual file "'+man+'" not found in "'+FManDirectory+'" directory'+EOL
               +'You can visit https://www.gnu.org/licenses/translations.html'+EOL
               +'English GPL will be opened.',mtInformation,[mbOk],0);
     man := 'GPL-English.html';
     FileToFind := FileSearch(FManDirectory+DirectorySeparator+man,FManDirectory);
  end;
  if FileToFind='' then begin
     MessageDlg('Manual file "'+man+'" not found in "'+FManDirectory+'" directory',mtInformation,[mbOk],0);
     exit;
  end;

  OpenDocument(FileToFind); { *Converted from ShellExecute* }
end;

end.
