{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }

{    Copyright © 2005, by Martijn van Engeland                                                }
{    e-mail                  : Info@FREEship.org                                              }
{    FREE!ship project page  : https://sourceforge.net/projects/freeship                      }
{    FREE!ship homepage      : www.FREEship.org                                               }

{    This program is free software; you can redistribute it and/or modify it under            }
{    the terms of the GNU General Public License as published by the                          }
{    Free Software Foundation; either version 2 of the License, or (at your option)           }
{    any later version.                                                                       }

{    This program is distributed in the hope that it will be useful, but WITHOUT ANY          }
{    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A          }
{    PARTICULAR PURPOSE. See the GNU General Public License for more details.                 }

{    You should have received a copy of the GNU General Public License along with             }
{    this program; if not, write to the Free Software Foundation, Inc.,                       }
{    59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                                    }

{#############################################################################################}

unit FreeNewModelDlg;

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
  SysUtils,
  Classes,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  Buttons,
  ExtCtrls, Spin,
  FreeShipUnit,
  FreeLanguageSupport;

type

  { TFreeNewModelDialog }

  TFreeNewModelDialog = class(TForm)
    FloatSpinEdit1: TFloatSpinEdit;
    FloatSpinEdit2: TFloatSpinEdit;
    FloatSpinEdit3: TFloatSpinEdit;
    Panel2: TPanel;
    Panel3: TPanel;
    BitBtn1: TSpeedButton;
    BitBtn2: TSpeedButton;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ComboBox1: TComboBox;
    Panel4: TPanel;
    Panel5: TPanel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private   { Private declarations }
    function FGetBreadth: single;
    function FGetDraft: single;
    function FGetLength: single;
    function FGetNCols: integer;
    function FGetNRows: integer;
    procedure FSetBreadth(Val: single);
    procedure FSetDraft(Val: single);
    procedure FSetLength(Val: single);
    procedure FSetNCols(Val: integer);
    procedure FSetNRows(Val: integer);
  public    { Public declarations }
    function Execute: boolean;
    property Breadth: single
      read FGetBreadth write FSetBreadth;
    property Draft: single
      read FGetDraft write FSetDraft;
    property Length: single
      read FGetLength write FSetLength;
    property NCols: integer
      read FGetNCols write FSetNCols;
    property NRows: integer
      read FGetNRows write FSetNRows;
  end;

var
  FreeNewModelDialog: TFreeNewModelDialog;

implementation

{$IFnDEF FPC}
  {$R *.dfm}

{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TFreeNewModelDialog.FGetLength: single;
begin
  Result := FloatSpinEdit1.Value;
end;{TFreeNewModelDialog.FGetLength}

procedure TFreeNewModelDialog.FSetLength(Val: single);
begin
  FloatSpinEdit1.Value := Val;
end;{TFreeNewModelDialog.FSetLength}

function TFreeNewModelDialog.FGetBreadth: single;
begin
  Result := FloatSpinEdit2.Value;
end;{TFreeNewModelDialog.FGetBreadth}

procedure TFreeNewModelDialog.FSetBreadth(Val: single);
begin
  FloatSpinEdit2.Value := Val;
end;{TFreeNewModelDialog.FSetBreadth}

function TFreeNewModelDialog.FGetDraft: single;
begin
  result:=FloatSpinEdit3.Value;
end;{TFreeNewModelDialog.FGetDraft}

procedure TFreeNewModelDialog.FSetDraft(Val: single);
begin
  FloatSpinEdit3.Value := Val;
end;{TFreeNewModelDialog.FSetDraft}

function TFreeNewModelDialog.FGetNCols: integer;
begin
  Result := SpinEdit1.Value;
end;{TFreeNewModelDialog.FGetNCols}

procedure TFreeNewModelDialog.FSetNCols(Val: integer);
begin
  if Val < 3 then
    Val := 3;
  SpinEdit1.Value := Val;
end;{TFreeNewModelDialog.FSetNCols}

function TFreeNewModelDialog.FGetNRows: integer;
begin
  Result := SpinEdit2.Value;
end;{TFreeNewModelDialog.FGetNRows}

procedure TFreeNewModelDialog.FSetNRows(Val: integer);
begin
  if Val < 3 then
    Val := 3;
  SpinEdit2.Value := Val;
end;{TFreeNewModelDialog.FSetNRows}

function TFreeNewModelDialog.Execute: boolean;
begin
  NCols := NCols;
  NRows := NRows;
  Length := Length;
  Breadth := Breadth;
  Draft := Draft;

  ShowTranslatedValues(Self);
  GlobalFreeShip.Preferences.LoadImageIntoBitmap(BitBtn1.Glyph, 'Ok');
  GlobalFreeShip.Preferences.LoadImageIntoBitmap(BitBtn2.Glyph, 'Cancel');

  Showmodal;
  Result := ModalResult = mrOk;
end;{TFreeNewModelDialog.Execute}

procedure TFreeNewModelDialog.BitBtn1Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;{TFreeNewModelDialog.BitBtn1Click}

procedure TFreeNewModelDialog.BitBtn2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;{TFreeNewModelDialog.BitBtn2Click}

end.
