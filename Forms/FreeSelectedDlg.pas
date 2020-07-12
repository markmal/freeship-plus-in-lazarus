unit FreeSelectedDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, StdCtrls, Spin, ActnList, Menus,
  FreeShipUnit,
  FreeGeometry
;

type

  { TFormSelected }

  TFormSelected = class(TForm)
    aClose: TAction;
    ActionList1: TActionList;
    bbClose: TBitBtn;
    cbEdgeIsCrease: TCheckBox;
    CheckBoxAnchorHard: TCheckBox;
    EditAnchorPoint: TEdit;
    EditLinearConstraintA: TEdit;
    EditLinearConstraintB: TEdit;
    eFaceLayer: TEdit;
    eFaceName: TEdit;
    eEdgePoinA: TEdit;
    eEdgeCurve: TEdit;
    eEdgePoinB: TEdit;
    eCurveName: TEdit;
    ePointName: TEdit;
    eEdgeName: TEdit;
    ePointType: TEdit;
    eEdgeType: TEdit;
    fsePointX: TFloatSpinEdit;
    fsePointY: TFloatSpinEdit;
    fsePointZ: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    gbEdgePoints: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lvFacePoints: TListView;
    lvCurvePoints: TListView;
    lvFaces: TListView;
    lvCurves: TListView;
    lvPointFaces: TListView;
    lvPointEdges: TListView;
    lvEdgeFaces: TListView;
    lvPointGroups: TListView;
    lvPoints: TListView;
    lvEdges: TListView;
    miUnselect: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel19: TPanel;
    Panel2: TPanel;
    Panel20: TPanel;
    Panel21: TPanel;
    Panel22: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    PopupMenu1: TPopupMenu;
    pPointsProperties: TPanel;
    pPointsProperties1: TPanel;
    pPointsProperties2: TPanel;
    pPointsProperties3: TPanel;
    pTop: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    tsEdges: TTabSheet;
    tsCurves: TTabSheet;
    tsFaces: TTabSheet;
    tsPoints: TTabSheet;
    procedure aCloseExecute(Sender: TObject);
    procedure lvCurvesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvEdgesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvFacesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvPointsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure miUnselectClick(Sender: TObject);
  private
    FFreeShip:TFreeShip;
    procedure Reload;
  public
    procedure onSelectionUpdate (Sender: TObject);
    property FreeShip:TFreeShip read FFreeShip write FFreeShip;
  end;

var
  FormSelected: TFormSelected;

implementation

{$R *.lfm}

{ TFormSelected }

procedure TFormSelected.lvPointsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var i:integer;
  CP: TFreeSubdivisionControlPoint;
  cpG: TFreeSubdivisionControlPointGroup;
  cpF: TFreeSubdivisionFace;
  cpE: TFreeSubdivisionEdge;
begin
  if not Selected then exit;

  CP := TFreeSubdivisionControlPoint(Item.Data);

  fsePointX.Value := CP.Coordinate.X;
  fsePointY.Value := CP.Coordinate.Y;
  fsePointZ.Value := CP.Coordinate.Z;
  ePointName.Caption := CP.Name;
  case CP.VertexType of
    svRegular: ePointType.Text := 'Regular';
    svCrease:  ePointType.Text := 'Crease';
    svDart:    ePointType.Text := 'Dart';
    svCorner:  ePointType.Text := 'Corner';
  end;

  EditLinearConstraintA.Text := '';
  if (CP.LinearConstraintPointA<>nil) then
    if (CP.LinearConstraintPointA.Name > '') then
     EditLinearConstraintA.Text := CP.LinearConstraintPointA.Name
     else EditLinearConstraintA.Text := 'Point['+IntToStr(CP.LinearConstraintPointA.Id)+']';
  EditLinearConstraintB.Text := '';
  if (CP.LinearConstraintPointB<>nil) then
    if (CP.LinearConstraintPointB.Name > '') then
     EditLinearConstraintB.Text := CP.LinearConstraintPointB.Name
     else EditLinearConstraintB.Text := 'Point['+IntToStr(CP.LinearConstraintPointB.Id)+']';

  CheckBoxAnchorHard.Checked := false;
  EditAnchorPoint.Text := '';
  if (CP.AnchorPoint<>nil) then
  begin
    CheckBoxAnchorHard.Checked := CP.IsAnchorHard;
    if (CP.AnchorPoint.Name > '') then
     EditAnchorPoint.Text := CP.AnchorPoint.Name
     else EditAnchorPoint.Text := 'Point['+IntToStr(CP.AnchorPoint.Id)+']';
  end;

  lvPointGroups.Clear;
  for i:=0 to FreeShip.Surface.ControlPointGroups.Count-1 do
  begin
    cpG:=FreeShip.Surface.ControlPointGroups[i];
    if cpG.ControlPoints.IndexOf(CP) >=0 then
    begin
       item := lvPointGroups.Items.Add;
       item.Caption:=IntToStr(cpG.Id);
       item.Subitems.Add(cpG.Name);
       item.Data := cpG;
    end;
  end;

  lvPointFaces.Clear;
  for i:=0 to CP.Faces.Count-1 do
  begin
    cpF:=CP.Faces[i];
    item := lvPointFaces.Items.Add;
    item.Caption:=IntToStr(cpF.Id);
    item.Subitems.Add(cpF.Name);
    item.Data := cpF;
  end;

  lvPointEdges.Clear;
  for i:=0 to CP.Edges.Count-1 do
  begin
    cpE:=CP.Edges[i];
    item := lvPointEdges.Items.Add;
    item.Caption:=IntToStr(cpE.Id);
    item.Subitems.Add(cpE.Name);
    item.Data := cpE;
  end;

end;

procedure TFormSelected.miUnselectClick(Sender: TObject);
var
  item: TListItem;
  CP: TFreeSubdivisionControlPoint;
  CE: TFreesubdivisionControlEdge;
  CF: TFreeSubdivisionControlFace;
  CC: TFreeSubdivisionControlCurve;
begin
  if (PageControl1.ActivePage = tsPoints)
    and lvPoints.Focused and (lvPoints.Selected <> nil)
  then
    begin
    item := lvPoints.Selected;
    CP := TFreeSubdivisionControlPoint(item.Data);
    CP.Selected := false;
    end;

  if (PageControl1.ActivePage = tsEdges)
    and lvEdges.Focused and (lvEdges.Selected <> nil)
  then
    begin
    item := lvEdges.Selected;
    CE := TFreeSubdivisionControlEdge(item.Data);
    CE.Selected := false;
    end;

  if (PageControl1.ActivePage = tsFaces)
    and lvFaces.Focused and (lvFaces.Selected <> nil)
  then
    begin
    item := lvFaces.Selected;
    CF := TFreeSubdivisionControlFace(item.Data);
    CF.Selected := false;
    end;

  if (PageControl1.ActivePage = tsCurves)
    and lvCurves.Focused and (lvCurves.Selected <> nil)
  then
    begin
    item := lvCurves.Selected;
    CC := TFreeSubdivisionControlCurve(item.Data);
    CC.Selected := false;
    end;

end;

procedure TFormSelected.lvEdgesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var i:integer;
  CE: TFreeSubdivisionControlEdge;
  cpF: TFreeSubdivisionFace;
begin
  if not Selected then exit;

  CE := TFreeSubdivisionControlEdge(Item.Data);

  eEdgeName.Text := CE.Name;
  cbEdgeIsCrease.Checked := CE.Crease;

  lvEdgeFaces.Clear;
  for i:=0 to CE.Faces.Count-1 do
  begin
    cpF:=CE.Faces[i];
    item := lvEdgeFaces.Items.Add;
    item.Caption:=IntToStr(cpF.Id);
    item.Subitems.Add(cpF.Name);
    item.Data := cpF;
  end;

  eEdgeCurve.Text:='';
  if CE.Curve <> nil then
    if CE.Curve.Name<>'' then
      eEdgeCurve.Text:=CE.Curve.Name
    else
      eEdgeCurve.Text:=format('Curve[%d]',[CE.Curve.Id]);

end;

procedure TFormSelected.lvFacesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var i:integer;
  CF: TFreeSubdivisionControlFace;
  cpF: TFreeSubdivisionPoint;
begin
  if not Selected then exit;

  CF := TFreeSubdivisionControlFace(Item.Data);

  eFaceName.Text := CF.Name;

  lvFacePoints.Clear;
  for i:=0 to CF.Points.Count-1 do
  begin
    cpF:=CF.Points[i];
    item := lvFacePoints.Items.Add;
    item.Caption:=IntToStr(cpF.Id);
    item.Subitems.Add(cpF.Name);
    item.Data := cpF;
  end;

  eFaceLayer.Text:='';
  if CF.Layer <> nil then
    eFaceLayer.Text:=CF.Layer.Name;

end;

procedure TFormSelected.aCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormSelected.lvCurvesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var i:integer;
  CC: TFreeSubdivisionControlCurve;
  cpF: TFreeSubdivisionPoint;
begin
  if not Selected then exit;

  CC := TFreeSubdivisionControlCurve(Item.Data);

  eCurveName.Text := CC.Name;

  lvFacePoints.Clear;
  for i:=0 to CC.ControlPoints.Count-1 do
  begin
    cpF:=CC.ControlPoints[i];
    item := lvCurvePoints.Items.Add;
    item.Caption:=IntToStr(cpF.Id);
    item.Subitems.Add(cpF.Name);
    item.Data := cpF;
  end;

end;

procedure TFormSelected.Reload;
var N,i:integer;
  item: TListItem;
  CP:TFreeSubdivisionControlPoint;
  CE: TFreesubdivisionControlEdge;
  CF: TFreeSubdivisionControlFace;
  CC: TFreeSubdivisionControlCurve;
begin
  N := FreeShip.NumberOfSelectedControlPoints
     + FreeShip.NumberOfSelectedControlEdges
     + FreeShip.NumberOfSelectedControlFaces
     + FreeShip.NumberOfSelectedControlCurves
     + FreeShip.NumberOfselectedMarkers
     + FreeShip.NumberOfselectedFlowlines;

  lvPoints.Clear;
  for i:=0 to FreeShip.Surface.NumberOfSelectedControlPoints-1 do
  begin
    CP := FFreeShip.Surface.SelectedControlPoint[i];
    item := lvPoints.Items.Add;
    item.Caption:=IntToStr(CP.Id);
    item.Subitems.Add(CP.Name);
    item.Data := CP;
    if CP = FreeShip.Surface.ActiveControlPoint then
      item.Selected:=true;
  end;

  lvEdges.Clear;
  for i:=0 to FreeShip.Surface.NumberOfSelectedControlEdges-1 do
  begin
    CE := FFreeShip.Surface.SelectedControlEdge[i];
    item := lvEdges.Items.Add;
    item.Caption:=IntToStr(CE.Id);
    item.Subitems.Add(CE.Name);
    item.Data := CE;
    if CE = FreeShip.Surface.ActiveControlEdge then
      item.Selected:=true;
  end;

  lvFaces.Clear;
  for i:=0 to FreeShip.Surface.NumberOfSelectedControlFaces-1 do
  begin
    CF := FFreeShip.Surface.SelectedControlFace[i];
    item := lvFaces.Items.Add;
    item.Caption:=IntToStr(CF.Id);
    item.Subitems.Add(CF.Name);
    item.Data := CF;
    if CF = FreeShip.Surface.ActiveControlFace then
      item.Selected:=true;
  end;

  lvCurves.Clear;
  for i:=0 to FreeShip.Surface.NumberOfSelectedControlCurves-1 do
  begin
    CC := FFreeShip.Surface.SelectedControlCurve[i];
    item := lvCurves.Items.Add;
    item.Caption:=IntToStr(CC.Id);
    item.Subitems.Add(CC.Name);
    item.Data := CC;
    if CC = FreeShip.Surface.ActiveControlCurve then
      item.Selected:=true;
  end;

  (* TODO
  + FreeShip.NumberOfselectedMarkers
  + FreeShip.NumberOfselectedFlowlines;
  *)
end;

procedure TFormSelected.onSelectionUpdate(Sender: TObject);
begin
  Reload;
  if Sender is TFreeSubdivisionControlPoint then
     PageControl1.ActivePage := tsPoints;
  if Sender is TFreesubdivisionControlEdge then
     PageControl1.ActivePage := tsEdges;
  if Sender is TFreesubdivisionControlFace then
     PageControl1.ActivePage := tsFaces;
  if Sender is TFreesubdivisionControlCurve then
     PageControl1.ActivePage := tsCurves;
end;

end.

