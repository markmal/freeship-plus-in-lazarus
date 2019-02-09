{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2005, by Martijn van Engeland                                                }
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

unit FreeMirrorPlaneDlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
 {$IFDEF Windows}
  Windows,
 {$ELSE}
  LCLIntf, LCLType, LMessages,
 {$ENDIF}
     Messages,
     SysUtils,
     Variants,
     Classes,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     Buttons,
     ExtCtrls,
     StdCtrls,
     FreeGeometry,
     FreeTypes,
     Spin;

type

{ TFreeMirrorPlaneDialog }

 TFreeMirrorPlaneDialog  = class(TForm)
                                BitBtn1: TSpeedButton;
                                BitBtn2: TSpeedButton;
                                 GroupBox1: TGroupBox;
                                 Panel1: TPanel;
                                 Panel2: TPanel;
                                 RadioButton1: TRadioButton;
                                 RadioButton2: TRadioButton;
                                 RadioButton3: TRadioButton;
                                 Edit1: TFloatSpinEdit;
                                 Label1: TLabel;
                                 CheckBox1: TCheckBox;
                                 Panel3: TPanel;
                                 procedure BitBtn1Click(Sender: TObject);
                                 procedure BitBtn2Click(Sender: TObject);
                              private   { Private declarations }
                                 function FGetPlane:T3DPlane;
                              public    { Public declarations }
                                 function Execute:Boolean;
                                 property Plane : T3DPlane read FGetPlane;
                           end;

var FreeMirrorPlaneDialog: TFreeMirrorPlaneDialog;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TFreeMirrorPlaneDialog.FGetPlane:T3DPlane;
begin
   Fillchar(Result,SizeOf(Result),0);
   if RadioButton1.Checked then Result.a:=1.0;
   if RadioButton2.Checked then Result.c:=1.0;
   if RadioButton3.Checked then Result.b:=1.0;
   Result.d:=-StrToFloat(Edit1.Text);
end;{TFreeMirrorPlaneDialog.FGetPlane}

function TFreeMirrorPlaneDialog.Execute:Boolean;
begin
   ShowModal;
   Result:=ModalResult=mrOK;
end;{TFreeMirrorPlaneDialog.Execute}

procedure TFreeMirrorPlaneDialog.BitBtn1Click(Sender: TObject);
begin
   Modalresult:=mrOK;
end;{TFreeMirrorPlaneDialog.BitBtn1Click}

procedure TFreeMirrorPlaneDialog.BitBtn2Click(Sender: TObject);
begin
   Modalresult:=mrCancel;
end;{TFreeMirrorPlaneDialog.BitBtn2Click}

end.
