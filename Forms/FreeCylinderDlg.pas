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

unit FreeCylinderDlg;

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

  { TFreeCylinderDialog }

  TFreeCylinderDialog = class(TForm)
    CloseEndDisk: TCheckBox;
    CloseStartDisk: TCheckBox;
    Panel2: TPanel;
    Panel4: TPanel;
    _Label3: TLabel;
    _Label6: TLabel;
    _Label9: TLabel;
    Panel1: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    BitBtn1: TSpeedButton;
    BitBtn2: TSpeedButton;
    StartPointX: TFloatSpinEdit;
    StartPointY: TFloatSpinEdit;
    StartPointZ: TFloatSpinEdit;
    EndPointX: TFloatSpinEdit;
    EndPointY: TFloatSpinEdit;
    EndPointZ: TFloatSpinEdit;
    Radius: TFloatSpinEdit;
    NoOfPoints: TSpinEdit;
    Label3: TLabel;
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private   { Private declarations }
    function FGetStartPoint: T3DCoordinate;
    procedure FSetStartPoint(val: T3DCoordinate);
    function FGetEndPoint: T3DCoordinate;
    procedure FSetEndPoint(val: T3DCoordinate);
  public    { Public declarations }
    function Execute(Str: string): boolean;
    property StartPoint: T3DCoordinate
      read FGetStartPoint write FSetStartPoint;
    property EndPoint: T3DCoordinate
      read FGetEndPoint write FSetEndPoint;
  end;

var FreeCylinderDialog: TFreeCylinderDialog;

implementation
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TFreeCylinderDialog.FGetStartPoint: T3DCoordinate;
begin
  Result := SetPoint(StartPointX.Value, StartPointY.Value, StartPointZ.Value);
end;{TFreeCylinderDialog.FGetStartPoint}

procedure TFreeCylinderDialog.FSetStartPoint(val: T3DCoordinate);
begin
  StartPointX.Value := Val.X;
  StartPointY.Value := Val.Y;
  StartPointZ.Value := Val.Z;
end;{TFreeCylinderDialog.FSetStartPoint}

function TFreeCylinderDialog.FGetEndPoint: T3DCoordinate;
begin
  Result := SetPoint(EndPointX.Value, EndPointY.Value, EndPointZ.Value);
end;{TFreeCylinderDialog.FGetEndPoint}

procedure TFreeCylinderDialog.FSetEndPoint(val: T3DCoordinate);
begin
  EndPointX.Value := Val.X;
  EndPointY.Value := Val.Y;
  EndPointZ.Value := Val.Z;
end;{TFreeCylinderDialog.FSetEndPoint}

function TFreeCylinderDialog.Execute(Str: string): boolean;
begin
  _label3.Caption := Str;
  _label6.Caption := Str;
  _label9.Caption := Str;
  GlobalFreeShip.Preferences.LoadImageIntoBitmap(BitBtn1.Glyph, 'Ok');
  GlobalFreeShip.Preferences.LoadImageIntoBitmap(BitBtn2.Glyph, 'Cancel');
  ShowTranslatedValues(Self);
  Showmodal;
  Result := ModalResult = mrOk;
end;{TFreeCylinderDialog.Execute}

procedure TFreeCylinderDialog.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;{TFreeCylinderDialog.OKButtonClick}

procedure TFreeCylinderDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;{TFreeCylinderDialog.CancelButtonClick}

procedure TFreeCylinderDialog.BitBtn1Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;{TFreeCylinderDialog.BitBtn1Click}

procedure TFreeCylinderDialog.BitBtn2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;{TFreeCylinderDialog.BitBtn2Click}

end.
