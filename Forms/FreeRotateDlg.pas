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
  LCLIntf, LCLType, //LMessages,
{$ENDIF}
  //Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  FreeShipUnit,
  FreeLanguageSupport,
  ExtCtrls, Spin;

type

  { TFreeRotateDialog }

  TFreeRotateDialog = class(TForm)
    FloatSpinEdit1: TFloatSpinEdit;
    FloatSpinEdit2: TFloatSpinEdit;
    FloatSpinEdit3: TFloatSpinEdit;
    Panel2: TPanel;
    Label3: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    OKButton: TSpeedButton;
    CancelButton: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Panel3: TPanel;
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private   { Private declarations }
    function FGetXValue: extended;
    procedure FSetXValue(val: extended);
    function FGetYValue: extended;
    procedure FSetYValue(val: extended);
    function FGetZValue: extended;
    procedure FSetZValue(val: extended);
  public    { Public declarations }
    function Execute(Caption, Units: string): boolean;
    property XValue: extended read FGetXValue write FSetXValue;
    property YValue: extended read FGetYValue write FSetYValue;
    property ZValue: extended read FGetZValue write FSetZValue;
  end;

var
  FreeRotateDialog: TFreeRotateDialog;

implementation

{$IFnDEF FPC}
  {$R *.dfm}

{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TFreeRotateDialog.FGetXValue: extended;
begin
  Result := FloatSpinEdit1.Value;
end;{TFreeRotateDialog.FGetXValue}

procedure TFreeRotateDialog.FSetXValue(val: extended);
begin
  FloatSpinEdit1.Value := val;
end;{TFreeRotateDialog.FSetXValue}

function TFreeRotateDialog.FGetYValue: extended;
begin
  Result := FloatSpinEdit2.Value;
end;{TFreeRotateDialog.FGetYValue}

procedure TFreeRotateDialog.FSetYValue(val: extended);
begin
  FloatSpinEdit2.Value := val;
end;{TFreeRotateDialog.FSetYValue}

function TFreeRotateDialog.FGetZValue: extended;
begin
  Result := FloatSpinEdit3.Value;
end;{TFreeRotateDialog.FGetZValue}

procedure TFreeRotateDialog.FSetZValue(val: extended);
begin
  FloatSpinEdit3.Value := val;
end;{TFreeRotateDialog.FSetZValue}

function TFreeRotateDialog.Execute(Caption, Units: string): boolean;
begin
  GlobalFreeship.Preferences.LoadImageIntoBitmap(OkButton.Glyph, 'Ok');
  GlobalFreeship.Preferences.LoadImageIntoBitmap(CancelButton.Glyph, 'Cancel');
  ShowTranslatedValues(Self);
  Self.Caption := Caption;
  Label3.Caption := Units;
  Label6.Caption := Units;
  Label9.Caption := Units;

  Showmodal;
  Result := ModalResult = mrOk;
end;{TFreeRotateDialog.Execute}

procedure TFreeRotateDialog.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;{TFreeRotateDialog.OKButtonClick}

procedure TFreeRotateDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;{TFreeRotateDialog.CancelButtonClick}

end.
