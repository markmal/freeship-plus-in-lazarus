{#############################################################################################}
{    This code is distributed as part of the FREE!ship Plus project. FREE!ship Plus is an     }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2005,      by Martijn van Engeland                                           }
{    Copyright © 2007-2012, by Timoshenko Victor F.                                           }
{    e-mail                  : vftim@rambler.ru, tvf@pisem.net                                }
{    FREE!ship project page  : http://freeship-plus.land.ru                                   }
{    FREE!ship homepage      : http://freeship-plus.pisem.su                                  }
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
     FreeLogger;

const
 SPLASH_TIME = 5000;

type

{ TFreeSplashWindow }

 TFreeSplashWindow    = class(TForm)
                                  Image1     : TImage;
                                  Label2: TLabel;
                                  Timer      : TTimer;
                                  _Label1: TLabel;
                                  _Label2: TLabel;
                                  Label3     : TLabel;
                                  _Label4: TLabel;
                                  _Label5: TLabel;
                                  _Label6: TLabel;
                                  _Label7: TLabel;
                                  _label8: TLabel;
                                  LabelBuildInfo: TLabel;
                                  procedure Image1MouseDown(Sender: TObject;
                                    Button: TMouseButton; Shift: TShiftState;
                                    X, Y: Integer);
                                  procedure Image1MouseUp(Sender: TObject;
                                    Button: TMouseButton; Shift: TShiftState;
                                    X, Y: Integer);
                                  procedure TimerTimer(Sender: TObject);
                                  procedure FormClose(Sender: TObject; var Action: TCloseAction);
                                  procedure Image1Click(Sender: TObject);
                                  procedure FormShow(Sender: TObject);
                                  procedure _Label5Click(Sender: TObject);
                                  procedure _Label4Click(Sender: TObject);
                               private   { Private declarations }
//                                  FOwner         : TFreeship;							   
                                  FCounter : integer;
                               public    { Public declarations }
//                                  FExeDirectory :string;
                            end;

var FreeSplashWindow: TFreeSplashWindow;

procedure SetTransparentForm(form: TForm; AValue : byte = 0);
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
 LWA_ALPHA    = 2;

type
 TSetLayeredWindowAttributes = function (
     hwnd : HWND;         // handle to the layered window
     crKey : TColor;      // specifies the color key
     bAlpha : byte;       // value for the blend function
     dwFlags : DWORD      // action
     ): BOOL; stdcall;

{$IFNDEF LCL}
procedure SetTransparentForm(form: TForm; AValue : byte = 0);
var
 Info: TOSVersionInfo;
 SetLayeredWindowAttributes: TSetLayeredWindowAttributes;
begin
 //Check Windows version
 Info.dwOSVersionInfoSize := SizeOf(Info);
 GetVersionEx(Info);
 if (Info.dwPlatformId = VER_PLATFORM_WIN32_NT) and
 (Info.dwMajorVersion >= 5) then
   begin
     SetLayeredWindowAttributes := GetProcAddress(GetModulehandle(user32), 'SetLayeredWindowAttributes');
      if Assigned(SetLayeredWindowAttributes) then
       begin
        SetWindowLong(AHandle, GWL_EXSTYLE, GetWindowLong(form.Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
        //Make form transparent
        SetLayeredWindowAttributes(AHandle, 0, AValue, LWA_ALPHA);
      end;
   end;
end;
{$ELSE}
procedure SetTransparentForm(form: TForm; AValue : byte = 0);
begin
 form.AlphaBlendValue := AValue;
end;
{$ENDIF}

procedure TFreeSplashWindow.TimerTimer(Sender: TObject);
var TI : integer; ABV: integer; ABD:integer;
begin
  TI:=Timer.Interval;
  inc(FCounter,TI);
  ABD := 255 div TI;

 if AlphaBlend then
    begin
     ABV:=AlphaBlendValue;

     if (FCounter < 1000) then inc(ABV,ABD);
     if (ABV>255) then ABV:=255;

     if (FCounter >= (SPLASH_TIME-1000)) then dec(ABV,ABD);
     if (ABV<1) then ABV:=1;

     //Label3.Caption:=IntToStr(ABV);
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

  if FCounter>SPLASH_TIME
    then begin
       //Visible:=false;
       Close;
       Application.ProcessMessages; // there maight be no message loop yet
    end;
end;{TFreeSplashWindow.TimerTimer}

procedure TFreeSplashWindow.Image1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // This should hold splash window until mouse button released
  AlphaBlendValue := 255;
  Update;
  Timer.Enabled:=False;
end;

procedure TFreeSplashWindow.Image1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // close splash window when mouse button released
  Close;
end;

procedure TFreeSplashWindow.FormClose(Sender: TObject;var Action: TCloseAction);
begin
   Timer.Enabled:=False;
   Release;
end;{TFreeSplashWindow.FormClose}

procedure TFreeSplashWindow.Image1Click(Sender: TObject);
begin
   //Timer.Enabled:=False;
   //Close;
end;{TFreeSplashWindow.Image1Click}

procedure TFreeSplashWindow.FormShow(Sender: TObject);
var Str:string;
begin
   _Label1.Caption:=Userstring(279)+#32+VersionString(CurrentVersion);
   _Label6.Caption:='Release: '+ReleasedDate;
   LabelBuildInfo.Caption := 'Build: '+ResourceVersionInfo+' '+COMPILE_DATE+' '+COMPILE_TIME+' '+TARGET_CPU+' '+TARGET_OS;
   Str:='';
   if CurrentLanguage<>nil then
   begin
      Str:=CurrentLanguage.ReadString('Translation','Author','');
      if Uppercase(Str)=Uppercase('Translation: <Your name>') then str:='';
      _Label8.Caption:=Str;
   end;
   _Label8.Visible:=Str<>'';
   FCounter:=0;
   Timer.Enabled:=True;
   Caption:='';
   //Parent := NIL;

   //AlphaBlend := false;

   if AlphaBlend then AlphaBlendValue:=1 else AlphaBlendValue:=255;
end;{TFreeSplashWindow.FormShow}

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
    OpenDocument('mailto:vftim@rambler.ru?subject=FREE!ship+'); { *Converted from ShellExecute* }
   // End Skip translation
end;{TFreeSplashWindow.Label4Click}

end.
