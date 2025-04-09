unit FreeLayerVisibilityDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, ComCtrls, Grids,
  LCLTranslator,
     FreeShipUnit,
     //FreeTypes,
     FreeGeometry,
     FasterList;

type

  { TFreeLayerVisibilityDialog }

  TFreeLayerVisibilityDialog = class(TForm)
    bbClose: TBitBtn;
    cbFreeStanding: TCheckBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label1: TLabel;
    Panel1: TPanel;
    pTools: TPanel;
    sgLayers: TStringGrid;
    procedure bbCloseClick(Sender: TObject);
    procedure cbFreeStandingChange(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sgLayersCheckboxToggled(sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
    procedure sgLayersGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
  private
    FFreeShip : TFreeShip;
    FOnChange: TNotifyEvent;
    //Layers   : specialize TFasterList<TFreeSubdivisionLayer>;
    procedure UpdateMenu;

  public
    procedure FillLayers;
    property FreeShip:TFreeShip read FFreeShip write FFreeShip;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

var
  FreeLayerVisibilityDialog: TFreeLayerVisibilityDialog;

implementation

{$R *.lfm}

{ TFreeLayerVisibilityDialog }

procedure TFreeLayerVisibilityDialog.bbCloseClick(Sender: TObject);
begin
  ModalResult := mrClose;
  Close;
end;

procedure TFreeLayerVisibilityDialog.cbFreeStandingChange(Sender: TObject);
begin
  if FFreeShip.Visibility.ShowFreeObjects <> cbFreeStanding.Checked then
  begin
    FFreeShip.Visibility.ShowFreeObjects := cbFreeStanding.Checked;
    FFreeShip.FileChanged:=true;
    if FOnChange <> nil then
     FOnChange(Self);
    FFreeShip.Redraw;
  end;
end;

procedure TFreeLayerVisibilityDialog.CheckBox1Click(Sender: TObject);
var i:integer; C: char; S: TCheckboxState;
begin
  if CheckBox1.Checked then C := '1' else C := '0';
  if CheckBox1.Checked then S := cbChecked else S := cbUnChecked;
  for i:=1 to sgLayers.RowCount-1 do
    begin
    sgLayers.Cells[0,i] := C;
    sgLayersCheckboxToggled(Sender, 0, i, S);
    end;
end;

procedure TFreeLayerVisibilityDialog.CheckBox2Click(Sender: TObject);
var i:integer; C: char; S: TCheckboxState;
begin
  if CheckBox2.Checked then C := '1' else C := '0';
  if CheckBox2.Checked then S := cbChecked else S := cbUnChecked;
  for i:=1 to sgLayers.RowCount-1 do
    begin
    sgLayers.Cells[1,i] := C;
    sgLayersCheckboxToggled(Sender, 1, i, S);
    end;
end;

procedure TFreeLayerVisibilityDialog.FormResize(Sender: TObject);
begin
  with sgLayers do
    ColWidths[2] := ClientWidth - ColWidths[0] - ColWidths[1] - 2 * GridLineWidth;
end;

procedure TFreeLayerVisibilityDialog.FormShow(Sender: TObject);
var Undo : TFreeUndoObject;
begin
  FillLayers;
  UpdateMenu;
end;

procedure TFreeLayerVisibilityDialog.sgLayersCheckboxToggled(sender: TObject;
  aCol, aRow: Integer; aState: TCheckboxState);
var index : integer; chk : boolean;
    Layer   : TFreeSubdivisionLayer;
begin
  index := aRow-1;
  chk := (aState = cbChecked);
  //Layer := FFreeShip.Surface.Layer[index];
  //Layer := FFreeShip.Surface.FindLayer(sgLayers.Cells[2,aRow]);
  Layer := sgLayers.Objects[2,aRow] as TFreeSubdivisionLayer;
  if Layer=nil then exit;

  if (aCol=0)and(Layer.SurfaceVisible <> chk) then
   begin
   Layer.SurfaceVisible:=chk;
   FFreeShip.FileChanged:=true;
   if FOnChange <> nil then
     FOnChange(Self);
   FFreeShip.Redraw;
   end;

  if (aCol=1)and(Layer.ControlNetVisible <> chk) then
   begin
   Layer.ControlNetVisible:=chk;
   FFreeShip.FileChanged:=true;
   if FOnChange <> nil then
     FOnChange(Self);
   FFreeShip.Redraw;
   end;

  UpdateMenu;
end;

procedure TFreeLayerVisibilityDialog.sgLayersGetCellHint(Sender: TObject; ACol,
  ARow: Integer; var HintText: String);
begin
  if ARow = 0 then
    begin
    HintText := sgLayers.Columns[aCol].Title.Caption;
    end
  else
    HintText := sgLayers.Cells[2,aRow];
end;

procedure TFreeLayerVisibilityDialog.FillLayers;
var I,N,r     , sc: Integer;
    Layer   : TFreeSubdivisionLayer;
    S,C : char;
    so: TSortOrder;
begin
  cbFreeStanding.Checked := FreeShip.Visibility.ShowFreeObjects;

  sc := sgLayers.SortColumn;
  so := sgLayers.SortOrder;

  sgLayers.Clear;
  sgLayers.BeginUpdate;
  sgLayers.RowCount := FFreeShip.NumberOfLayers + 1;
  try
    for I:=0 to FFreeShip.NumberOfLayers - 1 do
    begin
      Layer:=FFreeShip.Layer[I];
      if Layer.SurfaceVisible then S := '1' else S := '0';
      if Layer.ControlNetVisible then C := '1' else C := '0';
      r := i + 1;
      sgLayers.Cells[0,r] := S;
      sgLayers.Cells[1,r] := C;
      sgLayers.Cells[2,r] := Layer.Name;
      sgLayers.Objects[2,r] := Layer;
    end;
  finally
  sgLayers.EndUpdate;
  if sc > -1 then
    begin
    sgLayers.SortOrder := so;
    sgLayers.SortColRow(true,sc);
    end;
  end;
end;

procedure TFreeLayerVisibilityDialog.UpdateMenu;
begin
  cbFreeStanding.Checked := FreeShip.Visibility.ShowFreeObjects;
end;

end.

