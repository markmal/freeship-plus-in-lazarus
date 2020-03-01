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

unit FreeHydrostaticsDlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
  RichEdit,
{$ELSE}
  LCLIntf, LCLType, //LMessages,
  PrintersDlgs, Printer4Lazarus, FreePrinter,
{$ENDIF}
  //Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
  Buttons,
  Printers,
  ExtCtrls,
  FreeShipUnit,
  FreeLanguageSupport;

type

  { TFreeHydrostaticsDialog }

  TFreeHydrostaticsDialog = class(TForm)
    Panel1: TPanel;
    Edit: TMemo;
    Panel22: TPanel;
    ButtonClose: TSpeedButton;
    ButtonPrint: TSpeedButton;
    PrintDialog: TPrintDialog;
    ButtonSave: TSpeedButton;
    SaveDialog: TSaveDialog;
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonPrintClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private   { Private declarations }
  public    { Public declarations }
  end;

var
  FreeHydrostaticsDialog: TFreeHydrostaticsDialog;

implementation

{$IFnDEF FPC}
  {$R *.dfm}

{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TFreeHydrostaticsDialog.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;{TFreeHydrostaticsDialog.Button1Click}

procedure TFreeHydrostaticsDialog.ButtonPrintClick(Sender: TObject);
var
  PrintText: TextFile;
  Str: ansistring;
  I: integer;
begin
  if PrintDialog.Execute then
  begin

    AssignPrn(PrintText);
    Rewrite(PrintText);
    Printer.Canvas.Font.Assign(Edit.Font);
    for I := 1 to Edit.Lines.Count do
    begin
      Str := Edit.Lines[I - 1];
      Writeln(PrintText, Str);
    end;
    CloseFile(PrintText);
  end;
end;{TFreeHydrostaticsDialog.SpeedButton1Click}

procedure TFreeHydrostaticsDialog.ButtonSaveClick(Sender: TObject);
begin
  // Skip translation
  if SaveDialog.Execute then
    case SaveDialog.FilterIndex of
      1: Edit.Lines.SaveToFile(ChangeFileExt(SaveDialog.FileName, '.txt'));
        // save as plain text

    end;
  // End Skip translation
end;{TFreeHydrostaticsDialog.SpeedButton2Click}

procedure TFreeHydrostaticsDialog.FormShow(Sender: TObject);
var
  I: integer;
  S: string;
begin
  I := Edit.Lines.Count;
  S := Edit.Lines.CommaText;
  // Place cursor at beginning
  Edit.CaretPos := TPoint(Point(0, 0));
  I := Edit.Lines.Count;
  S := Edit.Lines.CommaText;

  GlobalFreeship.Preferences.LoadImageIntoBitmap(ButtonPrint.Glyph, 'Print');
  GlobalFreeship.Preferences.LoadImageIntoBitmap(ButtonSave.Glyph, 'Save');
  GlobalFreeship.Preferences.LoadImageIntoBitmap(ButtonClose.Glyph, 'Ok');
  ShowTranslatedValues(Self);

end;{TFreeHydrostaticsDialog.FormShow}

end.
