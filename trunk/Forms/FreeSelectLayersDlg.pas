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

unit FreeSelectLayersDlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
    {$IFDEF Windows}
     Windows,
    {$ELSE}
     LCLIntf, LCLType, LMessages,
    {$ENDIF}
     Messages,
     SysUtils,
     Variants,
     Classes,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     Buttons,
     ExtCtrls,
     StdCtrls,
     FreeshipUnit,
     FreeGeometry,
     FasterList,
     CheckLst;

type TFreeSelectMode          = (fsFaces,fsPoints);
     TFreeSelectLayersDialog  = class(TForm)
                                 Panel1: TPanel;
                                 Panel3: TPanel;
                                 BitBtn1: TSpeedButton;
                                 BitBtn2: TSpeedButton;
                                 LayerBox: TCheckListBox;
                                 Panel4: TPanel;
                                 CheckBox: TCheckBox;
                                 procedure BitBtn1Click(Sender: TObject);
                                 procedure BitBtn2Click(Sender: TObject);
                                 procedure LayerBoxClickCheck(Sender: TObject);
                                 procedure CheckBoxClick(Sender: TObject);
                              private   { Private declarations }
                                 FFreeship   : TFreeShip;
                                 FSelectMode : TFreeSelectMode;
                                 function FGetLayer(Index:Integer):TFreeSubdivisionLayer;
                                 function FGetNumberOfLayers:Integer;
                                 function FGetSelected(Index:Integer):boolean;
                                 procedure FFillBox;
                                 procedure FUpdateSelection(Destination:TFasterList);
                              public    { Public declarations }
                                 function Execute(Freeship:TFreeShip;SelectMode:TFreeSelectMode):Boolean;
                                 procedure ExtractSelectedFaces(var Destination:TFasterList);
                                 procedure ExtractSelectedPoints(var Destination:TFasterList);
                                 property Layer[index:integer]    : TFreeSubdivisionLayer read FGetLayer;
                                 property NumberOfLayers          : Integer read FGetNumberOfLayers;
                                 property Selected[index:integer] : Boolean read FGetSelected;
                           end;

var FreeSelectLayersDialog: TFreeSelectLayersDialog;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TFreeSelectLayersDialog.FGetNumberOfLayers:Integer;
begin
   Result:=Layerbox.Count;
end;{TFreeSelectLayersDialog.FGetNumberOfLayers}

function TFreeSelectLayersDialog.FGetSelected(Index:Integer):boolean;
begin
   Result:=Layerbox.Checked[index];
end;{TFreeSelectLayersDialog.FGetSelected}

function TFreeSelectLayersDialog.FGetLayer(Index:Integer):TFreeSubdivisionLayer;
begin
   Result:=Layerbox.Items.Objects[index] as TFreeSubdivisionLayer;
end;{TFreeSelectLayersDialog.FGetLayer}

procedure TFreeSelectLayersDialog.FFillBox;
var I,Index : Integer;
    Layer   : TFreeSubdivisionLayer;
begin
   LayerBox.Items.BeginUpdate;
   Layerbox.Clear;
   for I:=1 to FFreeship.NumberOfLayers do
   begin
      Layer:=FFreeship.Layer[I-1];
      if Layer.Count>0 then
      begin
         Index:=Layerbox.Items.AddObject(Layer.Name,Layer);
         Layerbox.Checked[index]:=Layer.Visible;
      end;
   end;
   LayerBox.Items.EndUpdate;
   FUpdateSelection(nil);
end;{TFreeSelectLayersDialog.FFillBox}

procedure TFreeSelectLayersDialog.FUpdateSelection(Destination:TFasterList);
var I,J    : Integer;
    Ind    : Integer;
    Layer  : TFreeSubdivisionLayer;
    Select : Boolean;
    Point  : TFreeSubdivisionControlPoint;
    Face   : TFreeSubdivisionControlFace;
begin
   if FSelectMode=fsFaces then
   begin
      for I:=1 to LayerBox.Count do
      begin
         Layer:=self.Layer[I-1];
         Select:=Layerbox.Checked[I-1];
         for J:=1 to Layer.Count do if Layer.Items[J-1].Selected<>Select then Layer.Items[J-1].Selected:=Select;
      end;
      if LayerBox.Count>0 then FFreeship.Redraw;
   end else
   begin
      for I:=1 to FFreeship.Surface.NumberOfControlPoints do
      begin
         Point:=FFreeship.Surface.ControlPoint[I-1];
         Select:=False;
         if Point.NumberOfFaces>0 then
         begin
            if Checkbox.Checked then
            begin
               // Point must be included in the selection if AT LEAST 1 attached
               // face belongs to a selected layer
               for J:=1 to Point.NumberOfFaces do
               begin
                  Face:=Point.Face[J-1] as TFreeSubdivisionControlFace;
                  Layer:=Face.Layer;
                  Ind:=LayerBox.Items.IndexOfObject(Layer);
                  if Layerbox.Checked[ind] then
                  begin
                     Select:=True;
                     break;
                  end;
               end;
            end else
            begin
               // Point must be included in the selection only if ALL attached
               // faces belong to selected layers
               Select:=True;
               for J:=1 to Point.NumberOfFaces do
               begin
                  Face:=Point.Face[J-1] as TFreeSubdivisionControlFace;
                  Layer:=Face.Layer;
                  Ind:=LayerBox.Items.IndexOfObject(Layer);
                  if not Layerbox.Checked[ind] then
                  begin
                     Select:=false;
                     break;
                  end;
               end;
            end;
            if Destination=nil then Point.Selected:=Select
                               else if Select then Destination.Add(Point);
         end;
      end;
      FFreeship.Redraw;
   end;
end;{TFreeSelectLayersDialog.FUpdateSelection}

function TFreeSelectLayersDialog.Execute(Freeship:TFreeShip;SelectMode:TFreeSelectMode):Boolean;
var OldEdges      : Boolean;
    OldNormals    : Boolean;
    OldControlNet : boolean;
begin
   FFreeship:=Freeship;
   FSelectMode:=SelectMode;
   FFillBox;
   OldNormals:=Freeship.Visibility.ShowNormals;
   OldEdges:=Freeship.Visibility.ShowInteriorEdges;
   OldControlNet:=Freeship.Visibility.ShowControlNet;
   Checkbox.Visible:=SelectMode=fsPoints;
   if SelectMode=fsFaces then
   begin
      Freeship.Visibility.ShowNormals:=False;
      Freeship.Visibility.ShowInteriorEdges:=True;
   end else
   begin
      Freeship.Visibility.ShowNormals:=False;
      Freeship.Visibility.ShowControlNet:=True;
      Freeship.Visibility.ShowInteriorEdges:=False;
   end;
   try
      ShowModal;
   finally
      Result:=ModalResult=mrOK;
      Freeship.Visibility.ShowInteriorEdges:=OldEdges;
      Freeship.Visibility.ShowNormals:=OldNormals;
      Freeship.Visibility.ShowControlNet:=OldControlNet;
   end;
end;{TFreeSelectLayersDialog.Execute}

procedure TFreeSelectLayersDialog.ExtractSelectedFaces(var Destination:TFasterList);
var I,J  : Integer;
    Layer: TFreeSubdivisionLayer;
begin
   for I:=1 to NumberOfLayers do if Selected[I-1] then
   begin
      Layer:=self.Layer[I-1];
      Destination.Capacity:=Destination.Count+Layer.Count;
      for J:=1 to Layer.Count do Destination.Add(Layer.Items[J-1]);
   end;
end;{TFreeSelectLayersDialog.ExtractSelectedFaces}

procedure TFreeSelectLayersDialog.ExtractSelectedPoints(var Destination:TFasterList);
begin
   FUpdateSelection(Destination);
end;{TFreeSelectLayersDialog.ExtractSelectedPoints}

procedure TFreeSelectLayersDialog.BitBtn1Click(Sender: TObject);
begin
   Modalresult:=mrOK;
end;{TFreeSelectLayersDialog.BitBtn1Click}

procedure TFreeSelectLayersDialog.BitBtn2Click(Sender: TObject);
begin
   Modalresult:=mrCancel;
end;{TFreeSelectLayersDialog.BitBtn2Click}

procedure TFreeSelectLayersDialog.LayerBoxClickCheck(Sender: TObject);
begin
   if Layerbox.ItemIndex<>-1 then FUpdateSelection(nil);
end;{TFreeSelectLayersDialog.LayerBoxClickCheck}

procedure TFreeSelectLayersDialog.CheckBoxClick(Sender: TObject);
begin
   if FSelectMode=fsPoints then FUpdateSelection(nil);
end;{TFreeSelectLayersDialog.CheckBoxClick}

end.