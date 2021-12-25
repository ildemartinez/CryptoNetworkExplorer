unit Ut_Images;

interface

uses
  controls, classes;

type
  TImageResourceList = class(TImageList)
  private
    FLoadedBitmaps : TStringList;

  public
    constructor Create(Onwer : TComponent); override;
    destructor Destroy; override;

    function GetIndexByName(const aImageName : String) : integer;
  end;

  // Contenedor de imagenes cargadas de disco
  TGlobalImageList = class(TImageList)
  private
    FLoadedBitmaps : TStringList;

  public
    constructor Create(Onwer : TComponent); override;
    destructor Destroy; override;

    function GetImageIndexByName(const aImageName : String) : integer;
  end;
    
  function GetGlobalImagesList : TGlobalImageList;

{.$R solar_components_comp_res.dcr}
{.$R global_res16x16.dcr}

implementation

uses
  //dcgen,
  dialogs, forms, ut_resources, ImgList;

type
  TMLMEASURESTATICTEXT = class(TObject)
  end;

var
  _GetGlobalImagesList : TGlobalImageList;

function GetGlobalImagesList : TGlobalImageList;
begin
  if _GetGlobalImagesList = nil then
    _GetGlobalImagesList := TGlobalImageList.Create(application);

  result := _GetGlobalImagesList;
end;

{ TImageResourceList }

constructor TImageResourceList.Create(Onwer: TComponent);
begin
  inherited;

  FLoadedBitmaps := TStringList.Create;
  Self.Height := 16;
  Width := 16;

end;

destructor TImageResourceList.Destroy;
begin

  FLoadedBitmaps.Free;

  inherited;
end;

function TImageResourceList.GetIndexByName(
  const aImageName: String): integer;
var
  H:THandle;
begin
  result := FLoadedBitmaps.IndexOf(aImageName);
  if result >= 0 then
    exit
  else
  begin
    h:=FindNameBitmapRes(aImageName);
    if h > 0 then
    begin
    showmessage('ut_images');
//      result := AddBitmapFromResource(self,aImageName,H);
      FLoadedBitmaps.Add(aImageName);
    end;
  end;
end;

{ TGlobalImageList }

constructor TGlobalImageList.Create(Onwer: TComponent);
begin
  inherited;

  FLoadedBitmaps := TStringList.Create;
  Height := 16;
  Width := 16;

end;

destructor TGlobalImageList.Destroy;
begin
  FLoadedBitmaps.Free;

  inherited;
end;

function TGlobalImageList.GetImageIndexByName(
  const aImageName: String): integer;
var
  H:THandle;
begin
   result := FLoadedBitmaps.IndexOf(aImageName);
  if result >= 0 then
    exit
  else
  begin
    h:=FindNameBitmapRes(aImageName);
    if h > 0 then              
    begin
//    showmessage('ut_images');
//      result := AddBitmapFromResource(self,aImageName,H);
      FLoadedBitmaps.Add(aImageName);
    end;
  end;
end;

initialization
  _GetGlobalImagesList := nil;
  
end.
