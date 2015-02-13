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

unit FreeExtrudeDlg;

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

type TFreeExtrudeDialog = class(TForm)
                              Panel2: TPanel;
    _Label3: TLabel;
    _Label6: TLabel;
    _Label9: TLabel;
                              Edit1: TEdit;
                              Edit2: TEdit;
                              Edit3: TEdit;
                              Panel1: TPanel;
                              Panel3: TPanel;
                              Label1: TLabel;
                              Label2: TLabel;
                              Label4: TLabel;
    BitBtn1: TSpeedButton;
    BitBtn2: TSpeedButton;
                              procedure Edit1KeyPress(Sender: TObject; var Key: Char);
                              procedure Edit1Exit(Sender: TObject);
                              procedure Edit1KeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
                              procedure Edit2Exit(Sender: TObject);
                              procedure Edit3Exit(Sender: TObject);
                              procedure OKButtonClick(Sender: TObject);
                              procedure CancelButtonClick(Sender: TObject);
    procedure Edit2KeyPress(Sender: TObject; var Key: Char);
    procedure Edit3KeyPress(Sender: TObject; var Key: Char);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
                           private   { Private declarations }
                              function FGetXValue:extended;
                              procedure FSetXValue(val:extended);
                              function FGetYValue:extended;
                              procedure FSetYValue(val:extended);
                              function FGetZValue:extended;
                              procedure FSetZValue(val:extended);
                           public    { Public declarations }
                              function Execute(Str:string):Boolean;
                              property XValue:Extended read FGetXValue write FSetXValue;
                              property YValue:Extended read FGetYValue write FSetYValue;
                              property ZValue:Extended read FGetZValue write FSetZValue;
                        end;

var FreeExtrudeDialog : TFreeExtrudeDialog;

implementation
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TFreeExtrudeDialog.FGetXValue:extended;
begin
   if Edit1.Text='' then result:=0.0
                    else Result:=StrToFloat(Edit1.Text);
end;{TFreeExtrudeDialog.FGetXValue}

procedure TFreeExtrudeDialog.FSetXValue(val:extended);
begin
   Edit1.Text:=FloatToStrF(Val,ffFixed,7,4);
end;{TFreeExtrudeDialog.FSetXValue}

function TFreeExtrudeDialog.FGetYValue:extended;
begin
   if Edit2.Text='' then result:=0.0
                    else Result:=StrToFloat(Edit2.Text);
end;{TFreeExtrudeDialog.FGetYValue}

procedure TFreeExtrudeDialog.FSetYValue(val:extended);
begin
   Edit2.Text:=FloatToStrF(Val,ffFixed,7,4);
end;{TFreeExtrudeDialog.FSetYValue}

function TFreeExtrudeDialog.FGetZValue:extended;
begin
   if Edit3.Text='' then result:=0.0
                    else Result:=StrToFloat(Edit3.Text);
end;{TFreeExtrudeDialog.FGetZValue}

procedure TFreeExtrudeDialog.FSetZValue(val:extended);
begin
   Edit3.Text:=FloatToStrF(Val,ffFixed,7,4);
end;{TFreeExtrudeDialog.FSetZValue}

function TFreeExtrudeDialog.Execute(Str:string):Boolean;
begin
   _label3.Caption:=Str;
   _label6.Caption:=Str;
   _label9.Caption:=Str;
   Showmodal;
   Result:=ModalResult=mrOk;
end;{TFreeExtrudeDialog.Execute}

procedure TFreeExtrudeDialog.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
   if (Key in [#8,'1'..'9','0','-',#13]) or (Key=FormatSettings.DecimalSeparator) then else key:=#0;
   if Key=#13 then Edit1Exit(self);
end;{TFreeExtrudeDialog.Edit1KeyPress}

procedure TFreeExtrudeDialog.Edit1Exit(Sender: TObject);
begin
   XValue:=self.XValue;
end;{TFreeExtrudeDialog.Edit1Exit}

procedure TFreeExtrudeDialog.Edit1KeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
   if Key=13 then SelectNext(Activecontrol,True,true);
end;{TFreeExtrudeDialog.Edit1KeyDown}

procedure TFreeExtrudeDialog.Edit2Exit(Sender: TObject);
begin
  inherited;
   YValue:=self.YValue;
end;{TFreeExtrudeDialog.Edit2Exit}

procedure TFreeExtrudeDialog.Edit3Exit(Sender: TObject);
begin
   ZValue:=self.ZValue;
end;{TFreeExtrudeDialog.Edit3Exit}

procedure TFreeExtrudeDialog.OKButtonClick(Sender: TObject);
begin
   ModalResult:=mrOk;
end;{TFreeExtrudeDialog.OKButtonClick}

procedure TFreeExtrudeDialog.CancelButtonClick(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreeExtrudeDialog.CancelButtonClick}

procedure TFreeExtrudeDialog.Edit2KeyPress(Sender: TObject; var Key: Char);
begin
   if (Key in [#8,'1'..'9','0','-',#13]) or (Key=FormatSettings.DecimalSeparator) then else key:=#0;
   if Key=#13 then Edit2Exit(self);
end;{TFreeExtrudeDialog.Edit2KeyPress}

procedure TFreeExtrudeDialog.Edit3KeyPress(Sender: TObject; var Key: Char);
begin
   if (Key in [#8,'1'..'9','0','-',#13]) or (Key=FormatSettings.DecimalSeparator) then else key:=#0;
   if Key=#13 then Edit3Exit(self);
end;{TFreeExtrudeDialog.Edit3KeyPress}

procedure TFreeExtrudeDialog.BitBtn1Click(Sender: TObject);
begin
   ModalResult:=mrOK;
end;{TFreeExtrudeDialog.BitBtn1Click}

procedure TFreeExtrudeDialog.BitBtn2Click(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreeExtrudeDialog.BitBtn2Click}

end.