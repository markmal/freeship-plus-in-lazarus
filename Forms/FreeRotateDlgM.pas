{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2010, by Timoshenko V.F.                                                     }
{    e-mail                  : Info@FREEship.org                                              }
{    FREE!ship project page  : http://freeship-plus.land.ru                                   }
{    FREE!ship homepage      : www.Freeship-plus.pisem.su                                     }
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

{$mode objfpc}{$H+}

unit FreeRotateDlgM;

interface

uses
{$IFnDEF FPC}
  Windows,
  RichEdit,
{$ELSE}
  LCLIntf, LCLType, LMessages,
  PrintersDlgs, Printer4Lazarus, FreePrinter,
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
     FreeShipUnit,
     FreeLanguageSupport;

type

{ TFreeRotateMDialog }

 TFreeRotateMDialog = class(TForm)
                             CancelButton: TSpeedButton;
                             OKButton: TSpeedButton;
                             Panel1: TPanel;
                              Panel2: TPanel;
                              Label3: TLabel;
                              Label6: TLabel;
                              Label9: TLabel;
                              Edit1: TEdit;
                              Edit2: TEdit;
                              Edit3: TEdit;
                              Panel3: TPanel;
                              Label1: TLabel;
                              Label2: TLabel;
                              Label4: TLabel;
                              Label11: TLabel;
                              Label12: TLabel;
                              Label13: TLabel;
                              Label14: TLabel;
                              Label15: TLabel;
                              Label16: TLabel;
//                              Label17: TLabel;
                              GroupBox1: TGroupBox;
                              GroupBox2: TGroupBox;
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
                              function Execute(aCaption,Units:String):Boolean;
                              property XValue:Extended read FGetXValue write FSetXValue;
                              property YValue:Extended read FGetYValue write FSetYValue;
                              property ZValue:Extended read FGetZValue write FSetZValue;
                        end;

var FreeRotateMDialog:TFreeRotateMDialog;
implementation
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TFreeRotateMDialog.FGetXValue:extended;
begin
   if Edit1.Text='' then result:=0.0
                    else Result:=StrToFloat(Edit1.Text);
end;{TFreeRotateMDialog.FGetXValue}

procedure TFreeRotateMDialog.FSetXValue(val:extended);
begin
   Edit1.Text:=FloatToStrF(Val,ffFixed,7,4);
end;{TFreeRotateMDialog.FSetXValue}

function TFreeRotateMDialog.FGetYValue:extended;
begin
   if Edit2.Text='' then result:=0.0
                    else Result:=StrToFloat(Edit2.Text);
end;{TFreeRotateMDialog.FGetYValue}

procedure TFreeRotateMDialog.FSetYValue(val:extended);
begin
   Edit2.Text:=FloatToStrF(Val,ffFixed,7,4);
end;{TFreeRotateMDialog.FSetYValue}

function TFreeRotateMDialog.FGetZValue:extended;
begin
   if Edit3.Text='' then result:=0.0
                    else Result:=StrToFloat(Edit3.Text);
end;{TFreeRotateMDialog.FGetZValue}

procedure TFreeRotateMDialog.FSetZValue(val:extended);
begin
   Edit3.Text:=FloatToStrF(Val,ffFixed,7,4);
end;{TFreeRotateMDialog.FSetZValue}

function TFreeRotateMDialog.Execute(aCaption,Units:String):Boolean;
begin
   Self.Caption:=Caption;
   Label3.Caption:=Units;
   Label6.Caption:=Units;
   Label9.Caption:=Units;

   GlobalFreeship.Preferences.LoadImageIntoBitmap(OkButton.Glyph,'Ok');
   GlobalFreeship.Preferences.LoadImageIntoBitmap(CancelButton.Glyph,'Cancel');
   ShowTranslatedValues(Self);

   Showmodal;
   Result:=ModalResult=mrOk;
end;{TFreeRotateMDialog.Execute}

procedure TFreeRotateMDialog.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
   if (Key in [#8,'1'..'9','0','-',#13]) or (Key=FormatSettings.DecimalSeparator) then else key:=#0;
end;{TFreeRotateMDialog.Edit1KeyPress}

procedure TFreeRotateMDialog.Edit1Exit(Sender: TObject);
begin
   XValue:=self.XValue;
end;{TFreeRotateMDialog.Edit1Exit}

procedure TFreeRotateMDialog.Edit1KeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
   if Key=13 then SelectNext(Activecontrol,True,true);
end;{TFreeRotateMDialog.Edit1KeyDown}

procedure TFreeRotateMDialog.Edit2Exit(Sender: TObject);
begin
  inherited;
   YValue:=self.YValue;
end;{TFreeRotateMDialog.Edit2Exit}

procedure TFreeRotateMDialog.Edit3Exit(Sender: TObject);
begin
   ZValue:=self.ZValue;
end;{TFreeRotateMDialog.Edit3Exit}

procedure TFreeRotateMDialog.OKButtonClick(Sender: TObject);
begin
   ModalResult:=mrOk;
end;{TFreeRotateMDialog.OKButtonClick}

procedure TFreeRotateMDialog.CancelButtonClick(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreeRotateMDialog.CancelButtonClick}

end.
