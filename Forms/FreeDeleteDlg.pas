unit FreeDeleteDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Grids,
  FreeShipUnit,
  FreeGeometry
  ;

type

  { TFreeDeleteDialog }

  TFreeDeleteDialog = class(TForm)
    bbDelete: TBitBtn;
    bbCancel: TBitBtn;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    lbTotalNumber: TLabel;
    Panel1: TPanel;
    pTools: TPanel;
    sgObjects: TStringGrid;
    procedure CheckBox1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sgObjectsCheckboxToggled(sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
    procedure sgObjectsGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
  private
    FFreeShip: TFreeShip;
    procedure UpdateMenu;
  public
    procedure Reload;
    property FreeShip:TFreeShip read FFreeShip write FFreeShip;
  end;

var
  FreeDeleteDialog: TFreeDeleteDialog;

implementation

{$R *.lfm}

{ TFreeDeleteDialog }

procedure TFreeDeleteDialog.CheckBox1Click(Sender: TObject);
var i:integer; C: char; S: TCheckboxState;
begin
  if CheckBox1.Checked then C := '1' else C := '0';
  if CheckBox1.Checked then S := cbChecked else S := cbUnChecked;
  for i:=1 to sgObjects.RowCount-1 do
    begin
    sgObjects.Cells[0,i] := C;
    sgObjectsCheckboxToggled(sgObjects,0,i,S);
    end;
end;

procedure TFreeDeleteDialog.FormResize(Sender: TObject);
begin
  with sgObjects do
    ColWidths[3] := ClientWidth - ColWidths[0] - ColWidths[1] - ColWidths[2]
                    - 2 * GridLineWidth;
end;

procedure TFreeDeleteDialog.sgObjectsCheckboxToggled(sender: TObject; aCol, aRow: Integer;
  aState: TCheckboxState);
var index : integer; chk : boolean;
    Layer   : TFreeSubdivisionLayer;
    O: TFreeNamedObject;
begin
  index := aRow-1;
  chk := (aState = cbChecked);
  //Layer := FFreeShip.Surface.Layer[index];
  //Layer := FFreeShip.Surface.FindLayer(sgLayers.Cells[2,aRow]);
  O := sgObjects.Objects[2,aRow] as TFreeNamedObject;
  if O=nil then exit;

  if (aCol=0)and(O.Selected <> chk) then
   begin
   O.Selected:=chk;
   FFreeShip.FileChanged:=true;
   //if FOnChange <> nil then
   //  FOnChange(Self);
   FFreeShip.Redraw;
   end;

  UpdateMenu;
end;

procedure TFreeDeleteDialog.sgObjectsGetCellHint(Sender: TObject; ACol, ARow: Integer;
  var HintText: String);
begin
  if ARow = 0 then
    begin
    HintText := sgObjects.Columns[aCol].Title.Caption;
    end
  else
    HintText := '';
end;

procedure TFreeDeleteDialog.UpdateMenu;
var i,N:integer; C: string; S: TCheckboxState;
begin
  N:=0;
  for i:=1 to sgObjects.RowCount-1 do
    begin
    C := sgObjects.Cells[0,i];
    if C = '0' then inc(N);
    end;
  lbTotalNumber.Caption := format('Total: %d',[N]);
  bbDelete.Enabled := N > 0;
end;

procedure TFreeDeleteDialog.Reload;
var i,r,N:integer;
  CP:TFreeSubdivisionControlPoint;
  CPG:TFreeSubdivisionControlPointGroup;
  CE: TFreesubdivisionControlEdge;
  CF: TFreeSubdivisionControlFace;
  CC: TFreeSubdivisionControlCurve;
  M:TFreeMarker; FL:TFreeFlowline;
begin
  N := FreeShip.NumberOfSelectedControlPoints
     + FreeShip.NumberOfSelectedControlPointGroups
     + FreeShip.NumberOfSelectedControlEdges
     + FreeShip.NumberOfSelectedControlFaces
     + FreeShip.NumberOfSelectedControlCurves
     + FreeShip.NumberOfselectedMarkers
     + FreeShip.NumberOfselectedFlowlines;

  sgObjects.Clear;
  sgObjects.BeginUpdate;
  sgObjects.RowCount := N + 1;

  lbTotalNumber.Caption := format('Total: %d',[N]);
  r:=1;
  for i:=0 to FreeShip.Surface.NumberOfSelectedControlPoints-1 do
  begin
    CP := FFreeShip.Surface.SelectedControlPoint[i];
    sgObjects.Cells[0,r] := '1';
    sgObjects.Cells[1,r] := 'Point';
    sgObjects.Cells[2,r] := format('[%d]',[CP.Id]);
    sgObjects.Objects[2,r] := CP;
    sgObjects.Cells[3,r] := CP.Name;
    inc(r);
  end;

  for i:=0 to FreeShip.Surface.NumberOfSelectedControlPointGroups-1 do
  begin
    CPG := FFreeShip.Surface.SelectedControlPointGroup[i];
    r:=i+1;
    sgObjects.Cells[0,r] := '1';
    sgObjects.Cells[1,r] := 'Group';
    sgObjects.Cells[2,r] := format('[%d]',[CPG.Id]);
    sgObjects.Objects[2,r] := CPG;
    sgObjects.Cells[3,r] := CPG.Name;
    inc(r);
  end;

  for i:=0 to FreeShip.Surface.NumberOfSelectedControlEdges-1 do
  begin
    CE := FFreeShip.Surface.SelectedControlEdge[i];
    sgObjects.Cells[0,r] := '1';
    sgObjects.Cells[1,r] := 'Edge';
    sgObjects.Cells[2,r] := format('[%d]',[CE.Id]);
    sgObjects.Objects[2,r] := CE;
    sgObjects.Cells[3,r] := CE.Name;
    inc(r);
  end;

  for i:=0 to FreeShip.Surface.NumberOfSelectedControlFaces-1 do
  begin
    CF := FFreeShip.Surface.SelectedControlFace[i];
    sgObjects.Cells[0,r] := '1';
    sgObjects.Cells[1,r] := 'Face';
    sgObjects.Cells[2,r] := format('[%d]',[CF.Id]);
    sgObjects.Objects[2,r] := CF;
    sgObjects.Cells[3,r] := CF.Name;
    inc(r);
  end;

  for i:=0 to FreeShip.Surface.NumberOfSelectedControlCurves-1 do
  begin
    CC := FFreeShip.Surface.SelectedControlCurve[i];
    sgObjects.Cells[0,r] := '1';
    sgObjects.Cells[1,r] := 'Curve';
    sgObjects.Cells[2,r] := format('[%d]',[CC.Id]);
    sgObjects.Objects[2,r] := CC;
    sgObjects.Cells[3,r] := CC.Name;
    inc(r);
  end;

  for i:=0 to FreeShip.NumberOfSelectedMarkers-1 do
  begin
    M := FFreeShip.SelectedMarker[i];
    sgObjects.Cells[0,r] := '1';
    sgObjects.Cells[1,r] := 'Marker';
    sgObjects.Cells[2,r] := format('[%d]',[M.Id]);
    sgObjects.Objects[2,r] := M;
    sgObjects.Cells[3,r] := M.Name;
    inc(r);
  end;

  for i:=0 to FreeShip.NumberOfSelectedFlowlines-1 do
  begin
    FL := FFreeShip.SelectedFlowline[i];
    sgObjects.Cells[0,r] := '1';
    sgObjects.Cells[1,r] := 'Flowline';
    sgObjects.Cells[2,r] := format('[%d]',[FL.Id]);
    sgObjects.Objects[2,r] := FL;
    sgObjects.Cells[3,r] := FL.Name;
    inc(r);
  end;

  sgObjects.EndUpdate;

end;


end.

