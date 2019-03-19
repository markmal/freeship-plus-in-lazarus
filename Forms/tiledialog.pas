unit TileDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, ActnList;

type

  TTile = class(TPanel)
  private
    FImage: TImage;
    FLabel: TLabel;
    FFileName:string;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property FileName:string read FFileName write FFileName;
    property Image: TImage read FImage write FImage;
    property CaptionLabel: TLabel read FLabel write FLabel;
  end;

  { TTileDialog }

  TTileDialog = class(TForm)
    ActionOpenAnother: TAction;
    ActionList1: TActionList;
    ButtonOpenFile: TBitBtn;
    FlowPanel: TFlowPanel;
    TopPanel: TPanel;
    ScrollBox: TScrollBox;
    procedure ActionOpenAnotherExecute(Sender: TObject);
    procedure ButtonOpenFileClick(Sender: TObject);
    procedure TopPanelClick(Sender: TObject);
    procedure TileOnClick(Sender: TObject);
  private
    FTileWidth: integer;
    FTileHeight: integer;
    FSelectedFileName: string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure setTileSize(aWidth, aHeight: integer);
    procedure addTile(aPicture:TPicture; aCaption:string; aFileName:string);
    property FileName:string read FSelectedFileName;
    //procedure add(aFileName:string);
  end;

var
  Form2: TTileDialog;

implementation

uses LazFileUtils;

{$R *.lfm}

 constructor TTile.Create(TheOwner: TComponent);
 begin
   inherited Create(TheOwner);

   BorderSpacing.Around := 8;
   BevelOuter := bvNone;
   //Name := 'Tile';
   Caption := 'Tile';

   FImage:= TImage.Create(Self);
   with FImage do
     begin
       Parent := Self;
       Left := 0;
       Top := 0;
       Align := alClient;
       Center := True;
       Proportional := True;
       Stretch := True;
     end;

   FLabel:= TLabel.Create(Self);
   with FLabel do
     begin
       Parent := Self;
       Align := alBottom;
       Alignment := taCenter;
       BorderSpacing.Around := 6;
       ParentColor := False;
       WordWrap := True;
     end;

 end;

constructor TTileDialog.Create(AOwner: TComponent);
 begin
   inherited Create(AOwner);
   FTileWidth:=400;
   FTileHeight:=300;
   Width := Screen.Width * 3 div 4;
   Height := Screen.Height * 3 div 4;
 end;

procedure TTileDialog.ButtonOpenFileClick(Sender: TObject);
var p:TPicture;
begin
  //FSelectedFileName:=
end;

procedure TTileDialog.ActionOpenAnotherExecute(Sender: TObject);
begin
  FSelectedFileName := '*';
  Close;
end;

procedure TTileDialog.TileOnClick(Sender: TObject);
begin
  FSelectedFileName := TTile(TImage(Sender).Parent).FileName;
  Close;
end;

procedure TTileDialog.TopPanelClick(Sender: TObject);
begin

end;

{
procedure TTileDialog.add(aFileName:string);
begin
  p:=TPicture.Create;
  p. (aFileName);
  sTime:=FormatDateTime('YYYY-DD-MM hh:mm:ss',FileDateToDateTime(FileAgeUTF8(aFileName)));
  addTile(p, sTime + ' - ' + aFileName);
end;
}

procedure TTileDialog.setTileSize(aWidth, aHeight: integer);
 begin
   FTileWidth:=aWidth;
   FTileHeight:=aHeight;
 end;

 procedure TTileDialog.addTile(aPicture:TPicture; aCaption:string; aFileName:string);
 var vtile:TTile; fpc:TFlowPanelControl; i:integer;
 begin
   vtile:=TTile.Create(Self);
   vtile.FileName:=aFileName;
   vtile.Width := FTileWidth;
   vtile.Height := FTileHeight;
   //vtile.Color:=clLime;
   vtile.Image.Picture := aPicture;
   vtile.CaptionLabel.Caption := aCaption;
   vtile.Visible:=true;
   fpc:=TFlowPanelControl(FlowPanel.ControlList.Add);
   fpc.Control := vtile;
   vtile.Parent:=FlowPanel;
   i:=FlowPanel.GetControlIndex(vtile);
   vtile.Image.OnClick:=@TileOnClick;
 end;


end.

