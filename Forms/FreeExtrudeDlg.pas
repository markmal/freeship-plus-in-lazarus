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
  ExtCtrls, Spin,
  FreeShipUnit,
  FreeLanguageSupport;
type

  { TFreeExtrudeDialog }

  TFreeExtrudeDialog = class(TForm)
    BitBtn1: TSpeedButton;
    BitBtn2: TSpeedButton;
    FloatSpinEdit1: TFloatSpinEdit;
    FloatSpinEdit2: TFloatSpinEdit;
    FloatSpinEdit3: TFloatSpinEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    _Label3: TLabel;
    _Label6: TLabel;
    _Label9: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    procedure Edit1KeyPress(Sender: TObject; var Key: char);
    procedure Edit1Exit(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject;
      var Key: word; Shift: TShiftState);
    procedure Edit2Exit(Sender: TObject);
    procedure Edit3Exit(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure Edit2KeyPress(Sender: TObject; var Key: char);
    procedure Edit3KeyPress(Sender: TObject; var Key: char);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private   { Private declarations }
    function FGetXValue: extended;
    procedure FSetXValue(val: extended);
    function FGetYValue: extended;
    procedure FSetYValue(val: extended);
    function FGetZValue: extended;
    procedure FSetZValue(val: extended);
  public    { Public declarations }
    function Execute(Str: string): boolean;
    property XValue: extended read FGetXValue write FSetXValue;
    property YValue: extended read FGetYValue write FSetYValue;
    property ZValue: extended read FGetZValue write FSetZValue;
  end;

var FreeExtrudeDialog: TFreeExtrudeDialog;

implementation
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TFreeExtrudeDialog.FGetXValue: extended;
begin
  Result := FloatSpinEdit1.Value;
end;{TFreeExtrudeDialog.FGetXValue}

procedure TFreeExtrudeDialog.FSetXValue(val: extended);
begin
  FloatSpinEdit1.Value := Val;
end;{TFreeExtrudeDialog.FSetXValue}

function TFreeExtrudeDialog.FGetYValue: extended;
begin
  Result := FloatSpinEdit2.Value;
end;{TFreeExtrudeDialog.FGetYValue}

procedure TFreeExtrudeDialog.FSetYValue(val: extended);
begin
  FloatSpinEdit2.Value := Val;
end;{TFreeExtrudeDialog.FSetYValue}

function TFreeExtrudeDialog.FGetZValue: extended;
begin
  Result := FloatSpinEdit3.Value;
end;{TFreeExtrudeDialog.FGetZValue}

procedure TFreeExtrudeDialog.FSetZValue(val: extended);
begin
  FloatSpinEdit3.Value := Val;
end;{TFreeExtrudeDialog.FSetZValue}

function TFreeExtrudeDialog.Execute(Str: string): boolean;
begin
  _label3.Caption := Str;
  _label6.Caption := Str;
  _label9.Caption := Str;
  ShowTranslatedValues(Self);
  GlobalFreeShip.Preferences.LoadImageIntoBitmap(BitBtn1.Glyph, 'Ok');
  GlobalFreeShip.Preferences.LoadImageIntoBitmap(BitBtn2.Glyph, 'Cancel');
  Showmodal;
  Result := ModalResult = mrOk;
end;{TFreeExtrudeDialog.Execute}

procedure TFreeExtrudeDialog.Edit1KeyPress(Sender: TObject; var Key: char);
begin
  if (Key in [#8, '1'..'9', '0', '-', #13]) or (Key = FormatSettings.DecimalSeparator) then
  else key := #0;
  if Key = #13 then Edit1Exit(self);
end;{TFreeExtrudeDialog.Edit1KeyPress}

procedure TFreeExtrudeDialog.Edit1Exit(Sender: TObject);
begin
  XValue := self.XValue;
end;{TFreeExtrudeDialog.Edit1Exit}

procedure TFreeExtrudeDialog.Edit1KeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  if Key = 13 then SelectNext(Activecontrol, True, True);
end;{TFreeExtrudeDialog.Edit1KeyDown}

procedure TFreeExtrudeDialog.Edit2Exit(Sender: TObject);
begin
  inherited;
  YValue := self.YValue;
end;{TFreeExtrudeDialog.Edit2Exit}

procedure TFreeExtrudeDialog.Edit3Exit(Sender: TObject);
begin
  ZValue := self.ZValue;
end;{TFreeExtrudeDialog.Edit3Exit}

procedure TFreeExtrudeDialog.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;{TFreeExtrudeDialog.OKButtonClick}

procedure TFreeExtrudeDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;{TFreeExtrudeDialog.CancelButtonClick}

procedure TFreeExtrudeDialog.Edit2KeyPress(Sender: TObject; var Key: char);
begin
  if (Key in [#8, '1'..'9', '0', '-', #13]) or (Key = FormatSettings.DecimalSeparator) then
  else key := #0;
  if Key = #13 then Edit2Exit(self);
end;{TFreeExtrudeDialog.Edit2KeyPress}

procedure TFreeExtrudeDialog.Edit3KeyPress(Sender: TObject; var Key: char);
begin
  if (Key in [#8, '1'..'9', '0', '-', #13]) or (Key = FormatSettings.DecimalSeparator) then
  else key := #0;
  if Key = #13 then Edit3Exit(self);
end;{TFreeExtrudeDialog.Edit3KeyPress}

procedure TFreeExtrudeDialog.BitBtn1Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;{TFreeExtrudeDialog.BitBtn1Click}

procedure TFreeExtrudeDialog.BitBtn2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;{TFreeExtrudeDialog.BitBtn2Click}

end.
