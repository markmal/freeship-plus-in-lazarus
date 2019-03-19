unit FreeOpenDialog;

//{$mode delphi}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
       {$IFDEF VER3}
  LazUTF8,
  LazFileUtils,
     {$ELSE}
  FileUtil, //deprecated
     {$ENDIF}
  Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtDlgs, ExtCtrls, ComCtrls;

type

  TFreeOpenDialog = class(TOpenPictureDialog)
  //TFreeOpenDialog = class(TPreviewFileDialog)
  private
    FOnPreview: TNotifyEvent;
  protected
    procedure UpdatePreview; override;
    function GetPreviewImage: TImage;
  public
    constructor Create(TheOwner: TComponent); override;
    function GetPlaces: TListItems;
    property PreviewImage: TImage read GetPreviewImage;
    property OnPreview: TNotifyEvent read FOnPreview write FOnPreview;
  end;

implementation

{ TFreeOpenDialog}

procedure TFreeOpenDialog.UpdatePreview;
var
  FPreviewFilename: string;
  FileIsValid: boolean;
begin
  //if Assigned(OnFolderChange) then OnFolderChange(Self);
  if Assigned(FOnPreview) then
    FOnPreview(Self);
end;

function TFreeOpenDialog.GetPreviewImage: TImage;
begin
  Result := ImageCtrl;
end;

constructor TFreeOpenDialog.Create(TheOwner: TComponent);
var
  H, W: integer;
  P: TComponent;
begin
  inherited Create(TheOwner);
  Filter := 'FREE!Ship model|*.ftm;*.fbm|Images|*.jpg;*.png';
  OnPreview := nil;
end;


function TFreeOpenDialog.GetPlaces: TListItems;
var
  e, gbe: TComponentEnumerator;
  gbi, c: TComponent;
begin
  Result := nil;
end;


end.
