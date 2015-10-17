{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2006, by Mark Malakanov                                                      }
{    FREE!ship project page  : https://github.com/markmal/freeship-plus-in-lazarus            }
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

unit FreeEmptyModelChooserDlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
    {$IFDEF Windows}
    Windows,
    {$ELSE}
    FileUtil,
    {$ENDIF}
     Messages,
     SysUtils,
     Classes,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     StdCtrls,
    ComCtrls,
    Buttons,
     ExtCtrls,
     FreeGeometry,
    FreeShipUnit,
    FreeLanguageSupport
     ;

type

{ TFreeEmptyModelChooserDialog }

 TFreeEmptyModelChooserDialog = class(TForm)
    LabelFileName: TLabel;
    Panel2: TPanel;
    Panel1: TPanel;
    Panel3: TPanel;
    OKbutton: TSpeedButton;
    CancelButton: TSpeedButton;
    Label1: TLabel;
    RadioGroup1: TRadioGroup;
    RbCreateNew: TRadioButton;
    RbLoadFile: TRadioButton;
    RbEmptyModel: TRadioButton;
    procedure OKbuttonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
 private   { Private declarations }
    FViewport: TFreeViewport;
 public    { Public declarations }
    function Execute(FileName:String):Boolean;
end;

var FreeEmptyModelChooserDialog:TFreeEmptyModelChooserDialog;

implementation
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}


function TFreeEmptyModelChooserDialog.Execute(FileName:String):Boolean;
begin
   LabelFileName.Caption := FileName;
   GlobalFreeship.Preferences.LoadImageIntoBitmap(OKbutton.Glyph,'Ok');
   GlobalFreeship.Preferences.LoadImageIntoBitmap(CancelButton.Glyph,'Cancel');
   ShowTranslatedValues(Self);
   Showmodal;
   Result:=ModalResult=mrOk;
end;{TFreeEmptyModelChooserDialog.Execute}

procedure TFreeEmptyModelChooserDialog.OKbuttonClick(Sender: TObject);
begin
   ModalResult:=mrOk;
end;{TFreeEmptyModelChooserDialog.BitBtn1Click}

procedure TFreeEmptyModelChooserDialog.CancelButtonClick(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreeEmptyModelChooserDialog.BitBtn2Click}


end.
