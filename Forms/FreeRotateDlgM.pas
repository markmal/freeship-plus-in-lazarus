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
  LCLIntf, LCLType,
  // PrintersDlgs,
  Printer4Lazarus,
  //FreePrinter,
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
  ExtCtrls, Spin,
  FreeShipUnit,
  FreeLanguageSupport;

type

  { TFreeRotateMDialog }

  TFreeRotateMDialog = class(TForm)
    CancelButton: TSpeedButton;
    FloatSpinEdit1: TFloatSpinEdit;
    FloatSpinEdit2: TFloatSpinEdit;
    FloatSpinEdit3: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    OKButton: TSpeedButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    procedure FloatSpinEdit1Resize(Sender: TObject);
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
    function Execute(aCaption, Units: string): boolean;
    property XValue: extended read FGetXValue write FSetXValue;
    property YValue: extended read FGetYValue write FSetYValue;
    property ZValue: extended read FGetZValue write FSetZValue;
  end;

var
  FreeRotateMDialog: TFreeRotateMDialog;

implementation

{$IFnDEF FPC}
  {$R *.dfm}

{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TFreeRotateMDialog.FGetXValue: extended;
begin
  Result:=FloatSpinEdit1.Value;
end;{TFreeRotateMDialog.FGetXValue}

procedure TFreeRotateMDialog.FSetXValue(val: extended);
begin
  FloatSpinEdit1.Value := val;
end;{TFreeRotateMDialog.FSetXValue}

function TFreeRotateMDialog.FGetYValue: extended;
begin
  Result:=FloatSpinEdit2.Value;
end;{TFreeRotateMDialog.FGetYValue}

procedure TFreeRotateMDialog.FSetYValue(val: extended);
begin
  FloatSpinEdit2.Value := val;
end;{TFreeRotateMDialog.FSetYValue}

function TFreeRotateMDialog.FGetZValue: extended;
begin
  Result:=FloatSpinEdit3.Value;
end;{TFreeRotateMDialog.FGetZValue}

procedure TFreeRotateMDialog.FSetZValue(val: extended);
begin
  FloatSpinEdit3.Value := val;
end;{TFreeRotateMDialog.FSetZValue}

function TFreeRotateMDialog.Execute(aCaption, Units: string): boolean;
begin
  Self.Caption := Caption;
  Label3.Caption := Units;
  Label6.Caption := Units;
  Label9.Caption := Units;

  GlobalFreeship.Preferences.LoadImageIntoBitmap(OkButton.Glyph, 'Ok');
  GlobalFreeship.Preferences.LoadImageIntoBitmap(CancelButton.Glyph, 'Cancel');
  ShowTranslatedValues(Self);

  Showmodal;
  Result := ModalResult = mrOk;
end;{TFreeRotateMDialog.Execute}

procedure TFreeRotateMDialog.FloatSpinEdit1Resize(Sender: TObject);
begin
  Label14.Constraints.MinHeight:=FloatSpinEdit1.Height;
  Label15.Constraints.MinHeight:=FloatSpinEdit1.Height;
  Label16.Constraints.MinHeight:=FloatSpinEdit1.Height;
end;

procedure TFreeRotateMDialog.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;{TFreeRotateMDialog.OKButtonClick}

procedure TFreeRotateMDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;{TFreeRotateMDialog.CancelButtonClick}

end.
