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

unit FreeGridDlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
    {$IFDEF Windows}
  Windows,
  shlobj,
    {$ELSE}
  LCLIntf, LCLType, 
          {$IFDEF VER3}
  LazUTF8,
  LazFileUtils,
     {$ELSE}
  FileUtil, //deprecated
     {$ENDIF}

    {$ENDIF}
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  FreeTypes,
  FreeGeometry,
  ExtCtrls, Spin,
  FreeShipUnit,
  FreeLanguageSupport;
type

  { TFreeGridDialog }

  TFreeGridDialog = class(TForm)
    Plane: TComboBox;
    SizeA: TFloatSpinEdit;
    SizeB: TFloatSpinEdit;
    Label5: TLabel;
    lbDimensions: TLabel;
    lbPlane: TLabel;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    lbUnit1: TLabel;
    Panel1: TPanel;
    Panel3: TPanel;
    lbStartpoint: TLabel;
    lbNoPoints: TLabel;
    BitBtn1: TSpeedButton;
    BitBtn2: TSpeedButton;
    StartPointX: TFloatSpinEdit;
    StartPointY: TFloatSpinEdit;
    StartPointZ: TFloatSpinEdit;
    ColumnPoints: TSpinEdit;
    RowPoints: TSpinEdit;
    lbUnit2: TLabel;
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private   { Private declarations }
    function GetStartPoint: T3DCoordinate;
    procedure SetStartPoint(val: T3DCoordinate);
  public    { Public declarations }
    function Execute(Str: string): boolean;
    property StartPoint: T3DCoordinate
      read GetStartPoint write SetStartPoint;
  end;

var FreeGridDialog: TFreeGridDialog;

implementation
{$IFnDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TFreeGridDialog.GetStartPoint: T3DCoordinate;
begin
  Result := SetPoint(StartPointX.Value, StartPointY.Value, StartPointZ.Value);
end;{TFreeGridDialog.GetStartPoint}

procedure TFreeGridDialog.SetStartPoint(val: T3DCoordinate);
begin
  StartPointX.Value := Val.X;
  StartPointY.Value := Val.Y;
  StartPointZ.Value := Val.Z;
end;{TFreeGridDialog.SetStartPoint}

function TFreeGridDialog.Execute(Str: string): boolean;
begin
  lbUnit1.Caption := Str;
  lbUnit2.Caption := Str;
  GlobalFreeShip.Preferences.LoadImageIntoBitmap(BitBtn1.Glyph, 'Ok');
  GlobalFreeShip.Preferences.LoadImageIntoBitmap(BitBtn2.Glyph, 'Cancel');
  ShowTranslatedValues(Self);
  Showmodal;
  Result := ModalResult = mrOk;
end;{TFreeGridDialog.Execute}

procedure TFreeGridDialog.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;{TFreeGridDialog.OKButtonClick}

procedure TFreeGridDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;{TFreeGridDialog.CancelButtonClick}

procedure TFreeGridDialog.BitBtn1Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;{TFreeGridDialog.BitBtn1Click}

procedure TFreeGridDialog.BitBtn2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;{TFreeGridDialog.BitBtn2Click}

end.
