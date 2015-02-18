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

unit FreeInsertPlaneDlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
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
     FreeTypes,
     FreeGeometry;

type

{ TFreeInsertPlaneDialog }

 TFreeInsertPlaneDialog  = class(TForm)
                                 GroupBox1: TGroupBox;
                                 Panel2: TPanel;
                                 RadioButton1: TRadioButton;
                                 RadioButton2: TRadioButton;
                                 RadioButton3: TRadioButton;
                                 Edit1: TEdit;
                                 Label1: TLabel;
                                 Label2: TLabel;
                                 Label4: TLabel;
                                 Label5: TLabel;
                                 Label3: TLabel;
                                 CheckBox1: TCheckBox;
                                 Panel1: TPanel;
                                 Panel3: TPanel;
    BitBtn1: TSpeedButton;
    BitBtn2: TSpeedButton;
                                 procedure RadioButton1Click(Sender: TObject);
                                 procedure RadioButton2Click(Sender: TObject);
                                 procedure RadioButton3Click(Sender: TObject);
                                 procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
                              private   { Private declarations }
                                 FMin  : T3DCoordinate;
                                 FMax  : T3DCoordinate;
                                 function FGetCreateControlcurve:Boolean;
                                 function FGetPlane:T3DPlane;
                                 procedure FUpdate;
                              public    { Public declarations }
                                 function Execute:Boolean;
                                 property CreateControlcurve : Boolean read FGetCreateControlcurve;
                                 property Max               : T3DCoordinate read FMax write FMax;
                                 property Min               : T3DCoordinate read FMin write FMin;
                                 property Plane             : T3DPlane read FGetPlane;
                           end;

var FreeInsertPlaneDialog: TFreeInsertPlaneDialog;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TFreeInsertPlaneDialog.FGetCreateControlcurve:Boolean;
begin
   Result:=Checkbox1.Checked;
end;{TFreeInsertPlaneDialog.FGetCreateControlcurve}

function TFreeInsertPlaneDialog.FGetPlane:T3DPlane;
begin
   Fillchar(Result,SizeOf(Result),0);
   if RadioButton1.Checked then Result.a:=1.0;
   if RadioButton2.Checked then Result.c:=1.0;
   if RadioButton3.Checked then Result.b:=1.0;
   Result.d:=-StrToFloat(Edit1.Text);
end;{TFreeInsertPlaneDialog.FGetPlane}

procedure TFreeInsertPlaneDialog.FUpdate;
begin
   if RadioButton1.Checked then label3.Caption:=FloatToStrF(Min.X+1e-4,ffFixed,7,4) else
      if RadioButton2.Checked then label3.Caption:=FloatToStrF(Min.Z+1e-4,ffFixed,7,4) else
         if RadioButton3.Checked then label3.Caption:=FloatToStrF(Min.Y+1e-4,ffFixed,7,4) else
            Label3.Caption:='';
   if RadioButton1.Checked then Label4.Caption:=FloatToStrF(Max.X+1e-4,ffFixed,7,4) else
      if RadioButton2.Checked then Label4.Caption:=FloatToStrF(Max.Z+1e-4,ffFixed,7,4) else
         if RadioButton3.Checked then Label4.Caption:=FloatToStrF(Max.Y+1e-4,ffFixed,7,4) else
            Label4.Caption:='';
end;{TFreeInsertPlaneDialog.FUpdate}

function TFreeInsertPlaneDialog.Execute:Boolean;
begin
   FUpdate;
   ShowModal;
   Result:=ModalResult=mrOK;
end;{TFreeInsertPlaneDialog.Execute}

procedure TFreeInsertPlaneDialog.RadioButton1Click(Sender: TObject);
begin
   FUpdate;
end;{TFreeInsertPlaneDialog.RadioButton1Click}

procedure TFreeInsertPlaneDialog.RadioButton2Click(Sender: TObject);
begin
   FUpdate;
end;{TFreeInsertPlaneDialog.RadioButton2Click}

procedure TFreeInsertPlaneDialog.RadioButton3Click(Sender: TObject);
begin
   FUpdate;
end;{TFreeInsertPlaneDialog.RadioButton3Click}

procedure TFreeInsertPlaneDialog.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
   // only valid numerical values
   if (Key in [#8,'1'..'9','0','-',#13]) or (Key=FormatSettings.DecimalSeparator) then else key:=#0;
end;{TFreeInsertPlaneDialog.Edit1KeyPress}

procedure TFreeInsertPlaneDialog.BitBtn1Click(Sender: TObject);
begin
   Modalresult:=mrOK;
end;{TFreeInsertPlaneDialog.BitBtn1Click}

procedure TFreeInsertPlaneDialog.BitBtn2Click(Sender: TObject);
begin
   Modalresult:=mrCancel;
end;{TFreeInsertPlaneDialog.BitBtn2Click}

end.