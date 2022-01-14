unit CryptoLabMainFormUnit;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.AppEvnts,
  BTCPeerDiscoveryUnit,
  BTCTypes,
  //VirtualTrees,
  btcnetworkunit,
  CryptoNetworkTreeViewUnit,
  Data.DB,
  Vcl.DBCtrls,
  Vcl.Menus,
  System.Actions,
  Vcl.ActnList,
  Vcl.StdActns,
  Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnMan,
  Vcl.ToolWin,
  Vcl.ActnCtrls,
  Vcl.ActnMenus,
  System.ImageList,
  Vcl.ImgList;

type
  TForm1 = class(TForm)
    ActionMainMenuBar1: TActionMainMenuBar;
    ActionManager1: TActionManager;
    WindowArrange1: TWindowArrange;
    WindowMinimizeAll1: TWindowMinimizeAll;
    Action1: TAction;
    Action2: TAction;
    ImageList1: TImageList;
    procedure Action2Execute(Sender: TObject);

  private
    { Private declarations }
    fCryptoNetworkTreeView1: TCryptoNetworkTreeView;
    fCryptoNetwork: TBTCNetwork;
  strict protected
    procedure OnVersionMessge(Sender: TObject; versionMessage: TVersionMessage);

  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Action2Execute(Sender: TObject);
begin
  self.Close;
end;

constructor TForm1.Create(Owner: TComponent);
begin
  inherited;

  with TSplitter.Create(self) do
  begin
    parent := self;
    Align := alLeft;
  end;

  fCryptoNetwork := TBTCNetwork.Create(self);
  fCryptoNetwork.MaxPeers := 0; // all possible
  fCryptoNetwork.OnVersionMessage := OnVersionMessge;

  fCryptoNetworkTreeView1 := TCryptoNetworkTreeView.Create(self);
  fCryptoNetworkTreeView1.parent := self;
  fCryptoNetworkTreeView1.Align := alLeft;
  fCryptoNetworkTreeView1.AsTree := true;
  fCryptoNetworkTreeView1.CryptoNetwork := self.fCryptoNetwork;
end;

destructor TForm1.Destroy;
begin

  inherited;
end;

procedure TForm1.OnVersionMessge(Sender: TObject; versionMessage: TVersionMessage);
begin
  { *Memo1.Lines.add('version :' + versionMessage.protocol_version.ToString);
    Memo1.Lines.add('NodeServices :' + versionMessage.node_services.ToString);
    Memo1.Lines.add('TimeStamp :' + DateTimeToStr(versionMessage.node_timestamp));
    Memo1.Lines.add('Receiving ip: ' + versionMessage.receiving_node_ip);
    Memo1.Lines.add('Port: ' + versionMessage.receiving_node_port.ToString);

    Memo1.Lines.add('Emmiting ip: ' + versionMessage.emmiting_node_ip);
    Memo1.Lines.add('Port: ' + versionMessage.emmiting_node_port.ToString);

    Memo1.Lines.add('User agent: ' + versionMessage.user_agent);* }
end;

end.
