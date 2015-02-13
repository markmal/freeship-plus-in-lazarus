{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2006, by Martijn van Engeland                                                }
{    e-mail                  : Info@FREEship.org                                              }
{    FREE!ship project page  : https://sourceforge.net/projects/freeship                      }
{    FREE!ship homepage      : www.FREEship.org                                               }
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

unit FreeBackgroundBlendingDlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
    {$IFDEF Windows}
    Windows,
    {$ELSE}
    FileUtil,
    {$ENDIF}
     Messages,
     SysUtils,
     Classes,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     StdCtrls,
     Buttons,
     ExtCtrls,
     FreeGeometry,
     ComCtrls;

type TFreeBackgroundBlendDialog = class(TForm)
                              Panel2: TPanel;
                              Panel1: TPanel;
                              Panel3: TPanel;
                              BitBtn1: TSpeedButton;
                              BitBtn2: TSpeedButton;
                              TrackBar1: TTrackBar;
    Label1: TLabel;
    _Label2: TLabel;
                              procedure BitBtn1Click(Sender: TObject);
                              procedure BitBtn2Click(Sender: TObject);
                              procedure TrackBar1Change(Sender: TObject);
                           private   { Private declarations }
                              FViewport: TFreeViewport;
                           public    { Public declarations }
                              function Execute(Viewport:TFreeViewport):Boolean;
                        end;
var FreeBackgroundBlendDialog:TFreeBackgroundBlendDialog;

implementation
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TFreeBackgroundBlendDialog.Execute(Viewport:TFreeViewport):Boolean;
begin
   FViewport:=Viewport;
   Trackbar1.Position:=Viewport.BackgroundImage.Alpha;
   TrackBar1Change(self);
   Showmodal;
   Result:=ModalResult=mrOk;
end;{TFreeBackgroundBlendDialog.Execute}

procedure TFreeBackgroundBlendDialog.BitBtn1Click(Sender: TObject);
begin
   ModalResult:=mrOk;
end;{TFreeBackgroundBlendDialog.BitBtn1Click}

procedure TFreeBackgroundBlendDialog.BitBtn2Click(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreeBackgroundBlendDialog.BitBtn2Click}

procedure TFreeBackgroundBlendDialog.TrackBar1Change(Sender: TObject);
begin
   FViewport.BackgroundImage.Alpha:=Trackbar1.Position;
   _Label2.Caption:=IntToStr(Round(100*(Trackbar1.Position)/Trackbar1.Max))+'%';
end;{TFreeBackgroundBlendDialog.TrackBar1Change}

end.