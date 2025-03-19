{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2005, by Martijn van Engeland                                                }
{    Copyright © 2007,2008 by Timoshenko V.F.                                                 }
{    e-mail                  : vftim@rambler.ru                                               }
{    FREE!ship project page  : www.FREEship-plus.pisem.su                                     }
{    FREE!ship homepage      : www.FREEship-plus.land.ru                                      }
{                                                                                        }
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

unit FreeLayerDlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, 
{$ENDIF}
  SysUtils,
     Classes,
     Graphics,
     Controls,
     StdCtrls,
     ExtCtrls,
     Forms,
     FreeShipUnit,
     FreeTypes,
     FreeGeometry,
     CheckLst,
     Dialogs,
     ComCtrls,
     LCLTranslator,
     ImgList,
     Spin, Buttons;

type

{ TFreeLayerDialog }

  TFreeLayerDialog  = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    cbControlNetVisible: TCheckBox;
    Edit1: TEdit;
    Edit2: TFloatSpinEdit;
    Edit3: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6_: TLabel;
    Label6_0: TLabel;
    Label6_1: TLabel;
    Label6_1_: TLabel;
    Label6_2: TLabel;
    Label6_3: TLabel;
    Label6_4: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    SpeedButtonTexture: TSpeedButton;
    ToolBar1: TToolBar;
    ToolButton20: TToolButton;
    MenuImages: TImageList;
    ColorDialog: TColorDialog;
    ToolButton1: TToolButton;
    AlphaBar: TTrackBar;
    WeightBox: TFloatSpinEdit;
    XgBox: TFloatSpinEdit;
    YgBox: TFloatSpinEdit;
    ZgBox: TFloatSpinEdit;
    _Label10: TLabel;
    _Label11: TLabel;
    _Label12: TLabel;
    _Label4: TLabel;
    _Label6: TLabel;
    _ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    LayerBox: TCheckListBox;
    MoveUp: TToolButton;
    MoveDown: TToolButton;
    _ToolButton4: TToolButton;
    procedure cbControlNetVisibleClick(Sender: TObject);
    procedure LayerBoxClick(Sender: TObject);
    procedure LayerBoxClickCheck(Sender: TObject);
    procedure LayerBoxDblClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure LayerBoxItemClick(Sender: TObject;
    Index: integer);
    procedure LayerBoxSelectionChange(Sender: TObject;
    User: boolean);
    procedure MenuImagesChange(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure SpeedButtonTextureClick(Sender: TObject);
    procedure ToolButton20Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure Edit2KeyPress(Sender: TObject; var Key: Char);
    procedure Edit2Exit(Sender: TObject);
    procedure Edit3KeyPress(Sender: TObject; var Key: Char);
    procedure Edit3Exit(Sender: TObject);
    procedure MoveUpClick(Sender: TObject);
    procedure MoveDownClick(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure AlphaBarChange(Sender: TObject);
  private
    FFreeShip : TFreeShip;
    FProgrammaticalChange: Boolean;
    procedure SelectLayer(Index: integer);
    procedure FFillBox;
    function FGetSelectedLayer:TFreeSubdivisionLayer;
    function FGetWeight:single;
    procedure FSetWeight(val:single);
    function FGetXg:single;
    procedure FSetXg(val:single);
    function FGetYg:single;
    procedure FSetYg(val:single);
    function FGetZg:single;
    procedure FSetZg(val:single);
  public
    WeightS,XgS,YgS,ZgS : single;
    function Execute(FreeShip:TFreeShip):Boolean;
    procedure UpdateMenu;
    property SelectedLayer:TFreeSubdivisionLayer read FGetSelectedLayer;
    property Weight  : single read FGetWeight write FSetWeight;
    property Xg  : single read FGetXg write FSetXg;
    property Yg  : single read FGetYg write FSetYg;
    property Zg  : single read FGetZg write FSetZg;
  end;

var FreeLayerDialog : TFreeLayerDialog;

implementation

uses FreeStringsUnit,
     FreeLinesplanFrme,
     FreeTextureDlg;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TFreeLayerDialog.UpdateMenu;
var I,N : INteger;
begin
   N:=0;
   for I:=1 to FFreeship.NumberOfLayers do if FFreeship.Layer[I-1].Count=0 then inc(N);
   Toolbutton3.Enabled:=(N>0) and (N<FFreeship.NumberOfLayers);
   MoveUp.Enabled:=False;
   MoveDown.Enabled:=False;
   if (FFreeship.NumberOfLayers>1) and (SelectedLayer<>nil) then
   begin
      MoveUp.Enabled:=SelectedLayer.LayerIndex>0;
      MoveDown.Enabled:=SelectedLayer.LayerIndex<FFreeship.NumberOfLayers-1;
   end;
end;{TFreeLayerDialog.UpdateMenu}

function TFreeLayerDialog.Execute(FreeShip:TFreeShip):Boolean;
var Undo : TFreeUndoObject;
begin
   FFreeShip:=FreeShip;
   Undo:=Freeship.Edit.CreateUndoObject(rs_layer_properties {UserString[244]},False);
   FFillBox;
   _Label4.Caption:=DensityStr(FFreeship.ProjectSettings.ProjectUnits);
   // Skip translation
   if FFreeship.ProjectSettings.ProjectUnits=fuImperial then _Label6.Caption:=LenMMStr(FFreeship.ProjectSettings.ProjectUnits)
                                                        else _Label6.Caption:=LenMMStr(FFreeship.ProjectSettings.ProjectUnits);
   // End skip translation

   ToolBar1.ButtonWidth :=Freeship.Preferences.ToolIconSize;
   ToolBar1.ButtonHeight:=Freeship.Preferences.ToolIconSize;

   Freeship.Preferences.LoadImageIntoList(MenuImages, 0, 'ExitProgram');
   Freeship.Preferences.LoadImageIntoList(MenuImages, 1, 'NewLayer');
   Freeship.Preferences.LoadImageIntoList(MenuImages, 2, 'DeleteEmptyLayers');
   Freeship.Preferences.LoadImageIntoList(MenuImages, 3, 'MoveUp');
   Freeship.Preferences.LoadImageIntoList(MenuImages, 4, 'MoveDown');

   UpdateMenu;
   ShowModal;
   Result:=ModalResult=mrOk;
   if Result then Undo.Accept else
   begin
      Undo.Restore;
      FreeAndNil(Undo);
   end;
end;{TFreeLayerDialog.Execute}

procedure TFreeLayerDialog.FFillBox;
var I,N     : Integer;
    Layer   : TFreeSubdivisionLayer;
begin
   LayerBox.Items.BeginUpdate;
   LayerBox.Clear;
   try
      for I:=1 to FFreeShip.NumberOfLayers do
      begin
         Layer:=FFreeShip.Layer[I-1];
         N:=Layerbox.Items.AddObject(Layer.Name,Layer);
         LayerBox.Checked[N]:=Layer.SurfaceVisible;
      end;
   finally
      LayerBox.Items.EndUpdate;
      if LayerBox.Count>0 then
      begin
         LayerBox.ItemIndex:=0;
         LayerBoxItemClick(self,0);
      end;
   end;
end;{TFreeLayerDialog.FFillBox}

function TFreeLayerDialog.FGetSelectedLayer:TFreeSubdivisionLayer;
begin
   if Layerbox.ItemIndex<>-1 then Result:=Layerbox.Items.Objects[Layerbox.ItemIndex] as TFreeSubdivisionLayer
                             else Result:=nil;
end;{TFreeLayerDialog.FGetSelectedLayer}


procedure TFreeLayerDialog.LayerBoxClickCheck(Sender: TObject);
var index : integer; chk : boolean;
    Layer   : TFreeSubdivisionLayer;
begin
   index := Layerbox.ItemIndex;
   chk := Layerbox.Checked[index];
   Layer:=Layerbox.Items.Objects[index] as TFreeSubdivisionLayer;
   if Layer=nil then exit;
   if Layer.SurfaceVisible <> chk then
     begin
     Layer.SurfaceVisible:=chk;
     FFreeShip.FileChanged:=true;
     FFreeShip.Redraw;
     end;
   UpdateMenu;
end;

procedure TFreeLayerDialog.LayerBoxClick(Sender: TObject);
var Index: integer;
begin
  index := Layerbox.ItemIndex;
  LayerBoxItemClick(Sender, Index);
end;

procedure TFreeLayerDialog.cbControlNetVisibleClick(Sender: TObject);
var index : integer; chk : boolean;
    Layer   : TFreeSubdivisionLayer;
begin
   index := Layerbox.ItemIndex;
   chk := cbControlNetVisible.Checked;
   Layer:=Layerbox.Items.Objects[index] as TFreeSubdivisionLayer;
   if Layer=nil then exit;
   if Layer.ControlNetVisible <> chk then
     begin
     Layer.ControlNetVisible:=chk;
     FFreeShip.FileChanged:=true;
     FFreeShip.Redraw;
     end;
   UpdateMenu;
end;

procedure TFreeLayerDialog.SelectLayer(Index: integer);
var Layer   : TFreeSubdivisionLayer;
    Prop    : TLayerProperties;
begin
  FProgrammaticalChange := true;
  index := Layerbox.ItemIndex;
  if index = -1 then exit;

  Layer:=Layerbox.Items.Objects[index] as TFreeSubdivisionLayer;
  if Layer=nil then exit;

  Edit1.Text:=Layer.Name;
  Panel3.Color:=Layer.Color;
  Checkbox1.Checked:=Layer.Developable;
  Checkbox2.Checked:=Layer.UseForIntersections;
  Checkbox3.Checked:=Layer.UseInHydrostatics;
  Checkbox4.Checked:=Layer.ShowInLinesplan;
  Checkbox5.Checked:=Layer.Symmetric;
  cbControlNetVisible.Checked := Layer.ControlNetVisible;
  ////////         Checkbox5.Enabled:=not Layer.UseInHydrostatics;
  Checkbox6.Enabled:=false;
  if Checkbox6.Checked then
    begin
    //		 Weightbox.Enabled:=True;
    //		 Xgbox.Enabled:=True;
    //		 Ygbox.Enabled:=True;
    //		 Zgbox.Enabled:=True;
    Edit2.Text:=FloatToDec(Layer.MaterialDensity,4);
    Edit3.Text:=FloatToDec(Layer.Thickness,4);
    Prop:=Layer.SurfaceProperties;
    WeightS:=Prop.Weight+Weight*1000.;
    XgS:=Prop.SurfaceCenterOfGravity.X*Prop.Weight+Xg*Weight*1000.;
    YgS:=Prop.SurfaceCenterOfGravity.Y*Prop.Weight+Yg*Weight*1000.;
    ZgS:=Prop.SurfaceCenterOfGravity.Z*Prop.Weight+Zg*Weight*1000.;
    Prop.SurfaceCenterOfGravity.X:=XgS/WeightS;
    Prop.SurfaceCenterOfGravity.Z:=YgS/WeightS;
    Prop.SurfaceCenterOfGravity.Z:=ZgS/WeightS;
    Prop.Weight:=WeightS;
    end
  else
    begin
    Weightbox.Enabled:=False;
    Xgbox.Enabled:=False;
    Ygbox.Enabled:=False;
    Zgbox.Enabled:=False;
    Edit2.Text:=FloatToDec(Layer.MaterialDensity,4);
    Edit3.Text:=FloatToDec(Layer.Thickness,4);
    Prop:=Layer.SurfaceProperties;
    end;

  if FFreeship.ProjectSettings.ProjectUnits=fuImperial
    then Prop.Weight:=Prop.Weight/(12*2240)
    else Prop.Weight:=Prop.Weight/1000;

  _Label10.Caption:=FloatToStrF(Prop.SurfaceArea,ffFixed,7,3)+#32+AreaStr(FFreeship.ProjectSettings.ProjectUnits);
  _Label11.Caption:=FloatToStrF(Prop.Weight,ffFixed,7,3)+#32+WeightStr(FFreeship.ProjectSettings.ProjectUnits);
  _Label12.Caption:=Makelength(Prop.SurfaceCenterOfGravity.X,2,7)+','+
                  Makelength(Prop.SurfaceCenterOfGravity.Y,2,7)+', '+
                  Makelength(Prop.SurfaceCenterOfGravity.Z,2,7)+#32+LengthStr(FFreeship.ProjectSettings.ProjectUnits);

  AlphaBar.Position:=round((255-Layer.AlphaBlend) * 100 / 255);
  //_label1.Caption:=FloatToStrF(100*(255-Layer.AlphaBlend)/255,ffFixed,7,1)+'%';
  UpdateMenu;
  FProgrammaticalChange := false;
end;{TFreeLayerDialog.LayerBoxClick}


procedure TFreeLayerDialog.LayerBoxItemClick(Sender: TObject; Index: integer);
begin
  index := Layerbox.ItemIndex;
  if index = -1 then exit;
  SelectLayer(Index);
end;{TFreeLayerDialog.LayerBoxClick}

procedure TFreeLayerDialog.LayerBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  SelectLayer(LayerBox.ItemIndex);
end;

procedure TFreeLayerDialog.LayerBoxDblClick(Sender: TObject);
var Layer : TFreeSubdivisionLayer;
begin
   if Layerbox.ItemIndex<>-1 then
   begin
      Layer:=Layerbox.Items.Objects[Layerbox.ItemIndex] as TFreeSubdivisionLayer;
      FFreeShip.ActiveLayer:=Layer;
   end;
   UpdateMenu;
end;{TFreeLayerDialog.LayerBoxDblClick}

procedure TFreeLayerDialog.Edit1Change(Sender: TObject);
begin
   if SelectedLayer<>nil then if SelectedLayer.Name<>Edit1.Text then
   begin
      SelectedLayer.Name:=Edit1.Text;
      Layerbox.Items[Layerbox.ItemIndex]:=SelectedLayer.Name;
   end;
end;{TFreeLayerDialog.Edit1Change}

procedure TFreeLayerDialog.MenuImagesChange(Sender: TObject);
begin
   if Assigned(Sender)
     then;
end;

procedure TFreeLayerDialog.Panel3Click(Sender: TObject);
begin
   if SelectedLayer<>nil then
   begin
      ColorDialog.Color:=SelectedLayer.Color;
      if ColorDialog.Execute then
      begin
         SelectedLayer.Color:=ColorDialog.Color;
         Panel3.Color:=ColorDialog.Color;
         FFreeShip.Redraw;
      end;
   end;
end;{TFreeLayerDialog.Panel3Click}

procedure TFreeLayerDialog.CheckBox1Click(Sender: TObject);
begin
   if SelectedLayer<>nil then if SelectedLayer.Developable<>Checkbox1.Checked then
   begin
      SelectedLayer.Developable:=CheckBox1.Checked;
      FFreeShip.FileChanged:=true;
      FFreeShip.Redraw;
   end;
end;{TFreeLayerDialog.CheckBox1Click}

procedure TFreeLayerDialog.SpeedButtonTextureClick(Sender: TObject);
var Dlg: TFreeTextureForm;
begin
   Dlg := TFreeTextureForm.Create(FFreeShip);
   Dlg.Execute(FFreeShip, SelectedLayer);
   Dlg.Free;
end;

procedure TFreeLayerDialog.ToolButton20Click(Sender: TObject);
var N:         Integer;
    NewLayer:  TFreeSubdivisionLayer;
    LayVis:    Boolean;
begin
// create the new layer, set the colour, set the file to "changed", create undo objcect:
   NewLayer:=FFreeShip.Edit.Layer_New;
// now update the dialog box. The new layer is the active layer (not the selected layer)
   if FFreeShip.ActiveLayer<>NewLayer then FFreeShip.ActiveLayer:=NewLayer;
   LayVis:=NewLayer.SurfaceVisible;
   N:=Layerbox.Items.AddObject(NewLayer.Name,NewLayer);
   Layerbox.Checked[N] := LayVis;
   Layerbox.ItemIndex:=N;
   LayerBoxItemClick(self,N);
   UpdateMenu;
end;{TFreeLayerDialog.ToolButton20Click}

procedure TFreeLayerDialog.ToolButton1Click(Sender: TObject);
begin
   Modalresult:=mrOK;
end;{TFreeLayerDialog.ToolButton1Click}

procedure TFreeLayerDialog.ToolButton3Click(Sender: TObject);
var noFeedback: Boolean;
begin
// delete the emtpy layers, set a new active layer:
   noFeedback := False; // if quiet, then no undo object, no feedback about number of deleted layers
   FFreeShip.Edit.Layer_DeleteEmpty(noFeedback);
// now update the dialog box: clear it and build it new
   FFillBox;
   UpdateMenu
end;{TFreeLayerDialog.ToolButton3Click}

procedure TFreeLayerDialog.CheckBox2Click(Sender: TObject);
begin
   if SelectedLayer<>nil then if SelectedLayer.UseForIntersections<>Checkbox2.Checked then
   begin
      SelectedLayer.UseForIntersections:=CheckBox2.Checked;
      FFreeship.Built:=False;// forces to rebuild all intersection lines
      FFreeShip.FileChanged:=true;
      FFreeShip.Redraw;
   end;
end;{TFreeLayerDialog.CheckBox2Click}

procedure TFreeLayerDialog.CheckBox3Click(Sender: TObject);
begin
   if SelectedLayer<>nil then if SelectedLayer.UseInHydrostatics<>Checkbox3.Checked then
   begin
      SelectedLayer.UseInHydrostatics:=CheckBox3.Checked;
/////      Checkbox5.Enabled:=not SelectedLayer.UseInHydrostatics;
/////      Checkbox5.Checked:=SelectedLayer.Symmetric;
      FFreeship.Built:=False;// forces to rebuild all hydrostatic calculations
      FFreeShip.FileChanged:=true;
      FFreeShip.Redraw;
   end;
end;{TFreeLayerDialog.CheckBox3Click}

procedure TFreeLayerDialog.Edit2KeyPress(Sender: TObject; var Key: Char);
begin
   if (Key in [#8,'1'..'9','0',#13]) or (Key=FormatSettings.DecimalSeparator) then else key:=#0;
   if Key=#13 then Edit2Exit(self);
end;{TFreeLayerDialog.Edit2KeyPress}

procedure TFreeLayerDialog.Edit2Exit(Sender: TObject);
var Value:TFloatType;
begin
   if SelectedLayer<>nil then
   begin
      Value:=Edit2.Value;
      if Value<>SelectedLayer.MaterialDensity then
      begin
         SelectedLayer.MaterialDensity:=Value;
      end;
      LayerBoxItemClick(self,Layerbox.ItemIndex);
   end;
end;{TFreeLayerDialog.Edit2Exit}

procedure TFreeLayerDialog.Edit3KeyPress(Sender: TObject; var Key: Char);
begin
   if (Key in [#8,'1'..'9','0',#13]) or (Key=FormatSettings.DecimalSeparator) then else key:=#0;
   if Key=#13 then Edit3Exit(self);
end;{TFreeLayerDialog.Edit3KeyPress}

procedure TFreeLayerDialog.Edit3Exit(Sender: TObject);
var Value:TFloatType;
begin
   if SelectedLayer<>nil then
   begin
      Value:=Edit3.Value;
      if Value<>SelectedLayer.Thickness then
      begin
         SelectedLayer.Thickness:=Value;
      end;
      LayerBoxItemClick(self,Layerbox.ItemIndex);
   end;
end;{TFreeLayerDialog.Edit3Exit}

procedure TFreeLayerDialog.MoveUpClick(Sender: TObject);
var Index : Integer;
    Layer:TFreeSubdivisionLayer;
begin
   if SelectedLayer<>nil then
   begin
      Layer:=SelectedLayer;
      Layer.MoveUp;
      FFillBox;
      Index:=Layerbox.Items.IndexOfObject(Layer);
      Layerbox.ItemIndex:=Index;
      FFreeship.FileChanged:=True;
      UpdateMenu;
   end;
end;{TFreeLayerDialog.MoveUpClick}

procedure TFreeLayerDialog.MoveDownClick(Sender: TObject);
var Index : Integer;
    Layer:TFreeSubdivisionLayer;
begin
   if SelectedLayer<>nil then
   begin
      Layer:=SelectedLayer;
      Layer.MoveDown;
      FFillBox;
      Index:=Layerbox.Items.IndexOfObject(Layer);
      Layerbox.ItemIndex:=Index;
      FFreeship.FileChanged:=True;
      UpdateMenu;
   end;
end;{TFreeLayerDialog.MoveDownClick}

procedure TFreeLayerDialog.CheckBox4Click(Sender: TObject);
begin
   if SelectedLayer<>nil then if SelectedLayer.ShowInLinesplan<>Checkbox4.Checked then
   begin
      SelectedLayer.ShowInLinesplan:=CheckBox4.Checked;
      FFreeShip.FileChanged:=true;
      if FFreeship.LinesplanFrame<>nil then TFreeLinesplanFrame(FFreeship.LinesplanFrame).Viewport.ZoomExtents;
      FFreeShip.Redraw;
   end;
end;{TFreeLayerDialog.CheckBox4Click}

procedure TFreeLayerDialog.CheckBox5Click(Sender: TObject);
begin
   if SelectedLayer<>nil then if SelectedLayer.Symmetric<>Checkbox5.Checked then
   begin
      SelectedLayer.Symmetric:=CheckBox5.Checked;
      FFreeship.Built:=False;// forces to rebuild all hydrostatic calculations
      FFreeShip.FileChanged:=true;
      FFreeShip.Redraw;
   end;
end;{TFreeLayerDialog.CheckBox5Click}

procedure TFreeLayerDialog.CheckBox6Click(Sender: TObject);
begin
   Weightbox.Enabled:=not Checkbox6.Checked;
   Xgbox.Enabled:=not Checkbox6.Checked;
   Ygbox.Enabled:=not Checkbox6.Checked;
   Zgbox.Enabled:=not Checkbox6.Checked;
   if CheckBox6.Checked then
   begin
		 Weightbox.Enabled:=True;
		 Xgbox.Enabled:=True;
		 Ygbox.Enabled:=True;
		 Zgbox.Enabled:=True;
   end;
end;{TFreeLayerDialog.CheckBox6Click}

procedure TFreeLayerDialog.AlphaBarChange(Sender: TObject);
var val:byte;
begin
   if FProgrammaticalChange then exit;
   Val:=round(255 - Alphabar.Position * 255 / 100);
   if SelectedLayer<>nil then if SelectedLayer.AlphaBlend<>val then
   begin
      SelectedLayer.AlphaBlend:=val;
      //_label1.Caption:=FloatToStrF(100*(255-SelectedLayer.AlphaBlend)/255,ffFixed,7,1)+'%';
      FFreeShip.FileChanged:=true;
      FFreeShip.Redraw;
   end;
end;{TFreeLayerDialog.AlphaBarChange}

function TFreeLayerDialog.FGetWeight:single;
begin
   Result:=Weightbox.Value;
end;{TFreeLayerDialog.FGetWeight}

procedure TFreeLayerDialog.FSetWeight(val:single);
begin
   Weightbox.Value:=val;
end;{TFreeLayerDialog.FSetWeight}

function TFreeLayerDialog.FGetXg:single;
begin
   Result:=Xgbox.Value;
end;{TFreeLayerDialog.FGetXg}

procedure TFreeLayerDialog.FSetXg(val:single);
begin
   Xgbox.Value:=val;
end;{TFreeLayerDialog.FSetXg}

function TFreeLayerDialog.FGetYg:single;
begin
   Result:=Ygbox.Value;
end;{TFreeLayerDialog.FGetYg}

procedure TFreeLayerDialog.FSetYg(val:single);
begin
   Ygbox.Value:=val;
end;{TFreeLayerDialog.FSetYg}

function TFreeLayerDialog.FGetZg:single;
begin
   Result:=Zgbox.Value;
end;{TFreeLayerDialog.FGetZg}

procedure TFreeLayerDialog.FSetZg(val:single);
begin
   Zgbox.Value:=val;
end;{TFreeLayerDialog.FSetZg}

end.

