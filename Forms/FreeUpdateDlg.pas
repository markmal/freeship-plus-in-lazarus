unit FreeUpdateDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, Buttons, ActnList, fpjsonrtti;

type TGitHubAsset = class(TCollectionItem)
  private
    f_name: string;
    f_content_type: string;
    f_browser_download_url: string;
    f_size: integer;
    f_download_count: integer;
    f_updated_at: string;
  published
    property name: String read f_name write f_name;
    property content_type: String read f_content_type write f_content_type;
    property browser_download_url: String read f_browser_download_url write f_browser_download_url;
    property size: integer read f_size write f_size;
    property download_count: integer read f_download_count write f_download_count;
    property updated_at: String read f_updated_at write f_updated_at;
end;

type TGitHubRelease = class(TPersistent)
  private
    f_name:string;
    f_tag_name:string;
    f_url:string;
    f_assets_url:string;
    f_html_url:string;
    f_body:string;
    f_assets: TCollection;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property name:string read f_name write f_name;
    property tag_name:string read f_tag_name write f_tag_name;
    property url:string read f_url write f_url;
    property assets_url:string read f_assets_url write f_assets_url;
    property html_url:string read f_html_url write f_html_url;
    property body:string read f_body write f_body;
    property assets: TCollection read f_assets write f_assets;
end;

type

  { TFreeUpdateForm }

  TFreeUpdateForm = class(TForm)
    ActionCheckUpdate: TAction;
    ActionList1: TActionList;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabelDownloadLink: TLabel;
    Label6: TLabel;
    LabelDownloadPage: TLabel;
    LabelNewVersionDate: TLabel;
    LabelNewVersion: TLabel;
    LabelCurrentVersion: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    procedure ActionCheckUpdateExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LabelDownloadLinkClick(Sender: TObject);
    procedure LabelDownloadPageClick(Sender: TObject);
  private
    FCurrentVersion:string;
  public
    procedure GetGitHubReleases;
  end;

var
  FreeUpdateForm: TFreeUpdateForm;

implementation

{$R *.lfm}

uses
  fpjson, jsonparser, fphttpclient,
  LazFileUtils, strutils, lclintf,
  FreeVersionUnit;

constructor TGitHubRelease.Create;
begin
  f_assets := TCollection.Create(TGitHubAsset);
end;

destructor TGitHubRelease.Destroy;
begin
  f_assets.Free;
end;

procedure TFreeUpdateForm.LabelDownloadLinkClick(Sender: TObject);
begin
  OpenURL(LabelDownloadLink.Hint);
end;

procedure TFreeUpdateForm.LabelDownloadPageClick(Sender: TObject);
begin
  OpenURL(LabelDownloadPage.Hint);
end;

procedure TFreeUpdateForm.ActionCheckUpdateExecute(Sender: TObject);
begin
  GetGitHubReleases;
end;

procedure TFreeUpdateForm.FormActivate(Sender: TObject);
begin
end;

procedure TFreeUpdateForm.FormCreate(Sender: TObject);
begin
  FCurrentVersion := ResourceVersionInfo;
  LabelCurrentVersion.Caption:=FCurrentVersion;
end;

procedure TFreeUpdateForm.FormShow(Sender: TObject);
begin
  GetGitHubReleases;
end;

// versions must be in format of <int>.<int>.<int>.<int>
function compareVersions(v1,v2:string):integer;
var slv1,slv2:TStringArray; i:integer;
begin
  slv1 := v1.split('.');
  slv2 := v2.split('.');
  for i:=0 to length(slv1)-1 do
    begin
    result:= StrToInt(slv1[i]) - StrToInt(slv2[i]);
    if result <> 0 then exit;
    end;
end;

procedure TFreeUpdateForm.GetGitHubReleases;
var
  Http: TFPHttpClient;
  Content : string;
  jData, jghReleaseData: TJSONData;
  s: String;
  i: integer;
  DeStreamer: TJSONDeStreamer;
  GitHubRelease: TGitHubRelease;
  GitHubAsset : TGitHubAsset;
  havedpkg : boolean;
  fext: string;
  vers:string;
  download_asset:integer;

  function extractVersion(fn: string):string;
  var s:string;
  begin
    s:=ExtractFileNameWithoutExt(fn);
    s:=lowercase(s);
    s:=ReplaceStr(s,'freeship','');
    s:=ReplaceStr(s,'windows','');
    s:=ReplaceStr(s,'linux','');
    s:=ReplaceStr(s,'all','');
    s:=ReplaceStr(s,'gtk2','');
    s:=ReplaceStr(s,'gtk3','');
    s:=ReplaceStr(s,'win32','');
    s:=ReplaceStr(s,'amd64','');
    s:=ReplaceStr(s,'x86-64','');
    s:=ReplaceStr(s,'x64','');
    s:=ReplaceStr(s,'-','');
    s:=ReplaceStr(s,'_','');
    result:=s;
  end;

begin
  LabelNewVersion.Caption := '';
  LabelNewVersionDate.Caption := '';
  Memo1.Clear;

  LabelDownloadLink.Caption := '';
  LabelDownloadLink.Hint := '';
  LabelDownloadLink.Enabled:=false;
  LabelDownloadPage.Cursor:=crDefault;

  LabelDownloadPage.Caption := 'https://github.com/markmal/freeship-plus-in-lazarus/releases';
  LabelDownloadPage.Hint := LabelDownloadPage.Caption;
  LabelDownloadPage.Enabled:=true;
  LabelDownloadPage.Cursor:=crHandPoint;


  havedpkg := false;
  {$ifdef linux}
  havedpkg := FileExists('/bin/dpkg') or FileExists('/usr/bin/dpkg');
  {$endif}

  Http:=TFPHttpClient.Create(Nil);
  Http.AllowRedirect := true;
  Http.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
  try
    Content:=Http.Get('https://api.github.com/repos/markmal/freeship-plus-in-lazarus/releases');
   jData:=GetJSON(Content);
   try
     jghReleaseData := jData.Items[0]; //0 - get last release
     DeStreamer := TJSONDeStreamer.Create(nil);
     GitHubRelease:=TGitHubRelease.Create;
     DeStreamer.JSONToObject(TJSONObject(jghReleaseData), GitHubRelease);

     download_asset := -1;
     for i:=0 to GitHubRelease.assets.Count-1 do
     begin
       GitHubAsset:= GitHubRelease.assets.Items[i] as TGitHubAsset;
       fext:=ExtractFileExt(GitHubAsset.name);
       {$ifdef Windows}
       if //(GitHubAsset.content_type = 'chemical/x-msi-msi')
          lowercase(fext) = '.msi'
          then downlad_asset := i;
       {$else}
       if not havedpkg //and (GitHubAsset.content_type = 'application/zip')
          and (lowercase(fext) = '.zip')
          then download_asset := i;
       if havedpkg //and (GitHubAsset.content_type = 'application/vnd.debian.binary-package')
          and (lowercase(fext) = '.deb')
          then download_asset := i;
       {$endif}
       vers := extractVersion(GitHubAsset.name);
       if compareVersions(vers,FCurrentVersion) <= 0 then
       begin
         Memo1.Text := 'FreeShip is up to date';
         download_asset := -1;
       end;
     end;
     if download_asset > -1 then
     begin
       GitHubAsset:= GitHubRelease.assets.Items[download_asset] as TGitHubAsset;

       LabelNewVersion.Caption := vers;
       LabelNewVersionDate.Caption := GitHubAsset.updated_at;

       LabelDownloadLink.Caption := GitHubAsset.name;
       LabelDownloadLink.Hint := GitHubAsset.browser_download_url;
       LabelDownloadLink.Enabled:=true;
       LabelDownloadLink.Cursor:=crHandPoint;

       LabelDownloadPage.Caption := GitHubRelease.name;
       LabelDownloadPage.Hint := GitHubRelease.html_url;
       LabelDownloadPage.Enabled:=true;
       Memo1.Text := GitHubRelease.body;
     end;
   finally
     jData.Free;
     if assigned(DeStreamer) then DeStreamer.Free;
     if assigned(GitHubRelease) then GitHubRelease.Free;
   end;
  finally
    Http.Free;
  end;
end;

end.

