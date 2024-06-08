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

unit FreeUndoHistoryDlg;

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
     Variants,
     Classes,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     StdCtrls,
     ExtCtrls,
     LCLTranslator,
     FreeShipUnit, FreeVersionUnit,
    FreeStringsUnit,
     ComCtrls,
    FreeTypes,
     FreeGeometry,
     Buttons, DBGrids;

type

{ TFreeUndoHistoryDialog }

  TFreeUndoHistoryDialog   = class(TForm)
    Panel: TPanel;
    Panel1: TPanel;
    UndoBox: TListBox;
    FreeShip1: TFreeShip;
    Viewport: TFreeViewport;
    Splitter1: TSplitter;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure UndoBoxClick(Sender: TObject);
    procedure ViewportRequestExtents(Sender: TObject; var Min,
    Max: T3DCoordinate);
    procedure ViewportRedraw(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    private { Private declarations }
    public { Public declarations }
    procedure CreateFreeShip;
    procedure CreateViewPort;
    function Execute(Freeship:TFreeShip):Boolean;
  end;

var FreeUndoHistoryDialog: TFreeUndoHistoryDialog;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TFreeUndoHistoryDialog.CreateFreeShip;
begin
  FreeShip1:= TFreeShip.Create(Self);
  with FreeShip1 do
  begin
    FileChanged := True;
    Filename := 'New model.fbm';
    FileVersion := fv230;
    Precision := fpLow;
  end;
end;

procedure TFreeUndoHistoryDialog.CreateViewPort;
begin
  Viewport := TFreeViewport.Create(Self);
  Viewport.Parent:=Panel1;
  with Viewport do
  begin
    Left := 264;
    Height := 392;
    Top := 9;
    Width := 411;
    Angle := 20;
    Align := alClient;
    BackgroundImage.Alpha := 255;
    BackgroundImage.Owner := Viewport;
    BackgroundImage.Quality := 100;
    BackgroundImage.Scale := 1;
    BackgroundImage.ShowInView := fvBodyplan;
    BackgroundImage.Tolerance := 5;
    BackgroundImage.Transparent := False;
    BackgroundImage.TransparentColor := clBlack;
    BackgroundImage.Visible := True;
    BevelOuter := bvLowered;
    BorderStyle := bsSingle;
    CameraType := ftStandard;
    DoubleBuffer := True;
    Elevation := 20;
    Margin := 0;
    ViewType := fvPerspective;
    ViewportMode := vmWireFrame;
    OnRedraw := ViewportRedraw;
    OnRequestExtents := ViewportRequestExtents;
  end;
end;

function TFreeUndoHistoryDialog.Execute(Freeship:TFreeShip):Boolean;
var I,Max : Integer;
    Undo : TFreeUndoObject;
    Str  : string;
begin
   CreateFreeShip;
   CreateViewPort;
   UndoBox.Clear;
   try
      UndoBox.Items.BeginUpdate;
      Max:=0;
      for I:=1 to Freeship.UndoCount do
      begin
         Undo:=Freeship.UndoObject[I-1];
         if length(Undo.UndoText)>Max then Max:=Length(Undo.UndoText);
      end;
      inc(Max,2);
      for I:=1 to Freeship.UndoCount do
      begin
         Undo:=Freeship.UndoObject[I-1];
         Str:=Freeship.UndoObject[I-1].UndoText;
         if Length(Str)>0 then Str[1]:=Upcase(Str[1]);
         While length(Str)<Max do Str:=Str+#32;
         Str:=Str+' ('+Undo.Time+')';
         UndoBox.Items.AddObject(Str,Undo);
      end;
   finally
      UndoBox.Items.EndUpdate;
      Freeship1.AddViewport(Viewport);
      UndoBox.ItemIndex:=Freeship.UndoPosition-1;
      Viewport.Color:=Freeship.Preferences.ViewportColor;

      Freeship.Preferences.LoadImageIntoBitmap(Speedbutton1.Glyph,'Ok');
      Freeship.Preferences.LoadImageIntoBitmap(Speedbutton2.Glyph,'Cancel');
      //ShowTranslatedValues(Self);

      ShowModal;
   end;
   Result:=ModalResult=mrOK;
end;{TFreeUndoHistoryDialog.Execute}

procedure TFreeUndoHistoryDialog.UndoBoxClick(Sender: TObject);
var Undo:TFreeUndoObject;
begin
   if UndoBox.ItemIndex<>-1 then
   begin
      Undo:=UndoBox.Items.Objects[UndoBox.ItemIndex] as TFreeUndoObject;;
      Freeship1.LoadProject(Undo.Undodata);
      Viewport.ZoomExtents;
   end;
end;{TFreeUndoHistoryDialog.UndoBoxClick}

procedure TFreeUndoHistoryDialog.ViewportRequestExtents(Sender: TObject;var Min, Max: T3DCoordinate);
begin
   Freeship1.Extents(Min,Max);
end;{TFreeUndoHistoryDialog.ViewportRequestExtents}

procedure TFreeUndoHistoryDialog.ViewportRedraw(Sender: TObject);
begin
   Freeship1.DrawToViewport(Viewport);
end;{TFreeUndoHistoryDialog.ViewportRedraw}

procedure TFreeUndoHistoryDialog.SpeedButton1Click(Sender: TObject);
begin
   ModalResult:=mrOK;
end;{TFreeUndoHistoryDialog.SpeedButton1Click}

procedure TFreeUndoHistoryDialog.SpeedButton2Click(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreeUndoHistoryDialog.BitBtn2Click}

procedure TFreeUndoHistoryDialog.FormResize(Sender: TObject);
begin
   Speedbutton1.Left:=Panel.Width-132;
   Speedbutton2.Left:=Speedbutton1.Left+SpeedButton1.Width+2;
end;



end.
