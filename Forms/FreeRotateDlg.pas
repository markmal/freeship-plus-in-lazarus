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

unit FreeRotateDlg;

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
     Classes,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     StdCtrls,
     Buttons,
     ExtCtrls;

type TFreeRotateDialog = class(TForm)
                              Panel2: TPanel;
                              Label3: TLabel;
                              Label6: TLabel;
                              Label9: TLabel;
                              Edit1: TEdit;
                              Edit2: TEdit;
                              Edit3: TEdit;
                              Panel1: TPanel;
                              OKButton: TSpeedButton;
                              CancelButton: TSpeedButton;
                              Panel3: TPanel;
                              Label1: TLabel;
                              Label2: TLabel;
                              Label4: TLabel;
                              procedure Edit1KeyPress(Sender: TObject; var Key: Char);
                              procedure Edit1Exit(Sender: TObject);
                              procedure Edit1KeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
                              procedure Edit2Exit(Sender: TObject);
                              procedure Edit3Exit(Sender: TObject);
                              procedure OKButtonClick(Sender: TObject);
                              procedure CancelButtonClick(Sender: TObject);
                           private   { Private declarations }
                              function FGetXValue:extended;
                              procedure FSetXValue(val:extended);
                              function FGetYValue:extended;
                              procedure FSetYValue(val:extended);
                              function FGetZValue:extended;
                              procedure FSetZValue(val:extended);
                           public    { Public declarations }
                              function Execute(Caption,Units:String):Boolean;
                              property XValue:Extended read FGetXValue write FSetXValue;
                              property YValue:Extended read FGetYValue write FSetYValue;
                              property ZValue:Extended read FGetZValue write FSetZValue;
                        end;

var FreeRotateDialog:TFreeRotateDialog;
implementation
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TFreeRotateDialog.FGetXValue:extended;
begin
   if Edit1.Text='' then result:=0.0
                    else Result:=StrToFloat(Edit1.Text);
end;{TFreeRotateDialog.FGetXValue}

procedure TFreeRotateDialog.FSetXValue(val:extended);
begin
   Edit1.Text:=FloatToStrF(Val,ffFixed,7,4);
end;{TFreeRotateDialog.FSetXValue}

function TFreeRotateDialog.FGetYValue:extended;
begin
   if Edit2.Text='' then result:=0.0
                    else Result:=StrToFloat(Edit2.Text);
end;{TFreeRotateDialog.FGetYValue}

procedure TFreeRotateDialog.FSetYValue(val:extended);
begin
   Edit2.Text:=FloatToStrF(Val,ffFixed,7,4);
end;{TFreeRotateDialog.FSetYValue}

function TFreeRotateDialog.FGetZValue:extended;
begin
   if Edit3.Text='' then result:=0.0
                    else Result:=StrToFloat(Edit3.Text);
end;{TFreeRotateDialog.FGetZValue}

procedure TFreeRotateDialog.FSetZValue(val:extended);
begin
   Edit3.Text:=FloatToStrF(Val,ffFixed,7,4);
end;{TFreeRotateDialog.FSetZValue}

function TFreeRotateDialog.Execute(Caption,Units:String):Boolean;
begin
   Self.Caption:=Caption;
   Label3.Caption:=Units;
   Label6.Caption:=Units;
   Label9.Caption:=Units;
   Showmodal;
   Result:=ModalResult=mrOk;
end;{TFreeRotateDialog.Execute}

procedure TFreeRotateDialog.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
   if (Key in [#8,'1'..'9','0','-',#13]) or (Key=FormatSettings.DecimalSeparator) then else key:=#0;
end;{TFreeRotateDialog.Edit1KeyPress}

procedure TFreeRotateDialog.Edit1Exit(Sender: TObject);
begin
   XValue:=self.XValue;
end;{TFreeRotateDialog.Edit1Exit}

procedure TFreeRotateDialog.Edit1KeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
   if Key=13 then SelectNext(Activecontrol,True,true);
end;{TFreeRotateDialog.Edit1KeyDown}

procedure TFreeRotateDialog.Edit2Exit(Sender: TObject);
begin
  inherited;
   YValue:=self.YValue;
end;{TFreeRotateDialog.Edit2Exit}

procedure TFreeRotateDialog.Edit3Exit(Sender: TObject);
begin
   ZValue:=self.ZValue;
end;{TFreeRotateDialog.Edit3Exit}

procedure TFreeRotateDialog.OKButtonClick(Sender: TObject);
begin
   ModalResult:=mrOk;
end;{TFreeRotateDialog.OKButtonClick}

procedure TFreeRotateDialog.CancelButtonClick(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreeRotateDialog.CancelButtonClick}

end.