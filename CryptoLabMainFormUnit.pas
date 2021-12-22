unit CryptoLabMainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.AppEvnts,
  BTCPeerDiscoveryUnit, BTCTypes, VirtualTrees, btcnetworkunit,
  CryptoNetworkTreeViewUnit, Data.DB, Vcl.DBCtrls, Vcl.Menus;

type
  TForm1 = class(TForm)
    TrayIcon1: TTrayIcon;
    ApplicationEvents1: TApplicationEvents;
    Button1: TButton;
    PopupMenu1: TPopupMenu;

    procedure TrayIcon1DblClick(Sender: TObject);
    procedure ApplicationEvents1Minimize(Sender: TObject);
    procedure Button1Click(Sender: TObject);

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

procedure TForm1.ApplicationEvents1Minimize(Sender: TObject);
begin
  { Hide the window and set its state variable to wsMinimized. }
  hide();
  WindowState := wsMinimized;
  { Show the animated tray icon and also a hint balloon. }
  TrayIcon1.Visible := True;
  TrayIcon1.Animate := True;
  TrayIcon1.ShowBalloonHint;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  fCryptoNetwork.connect;

  fCryptoNetworkTreeView1.CryptoNetwork := fCryptoNetwork;

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
  fCryptoNetworkTreeView1.CryptoNetwork := self.fCryptoNetwork;

  TrayIcon1.Hint := 'BTC Agent';
  // hide();
  // WindowState := wsMinimized;

  { Set up a hint balloon. }
  TrayIcon1.BalloonTitle := 'Restoring the window.';
  TrayIcon1.BalloonHint :=
    'Double click the system tray icon to restore the window.';
  TrayIcon1.BalloonFlags := bfInfo;
  TrayIcon1.ShowBalloonHint();

  { Show the animated tray icon and also a hint balloon. }
  TrayIcon1.Visible := True;
  TrayIcon1.Animate := True;
  TrayIcon1.ShowBalloonHint;

end;

destructor TForm1.Destroy;
begin

  inherited;
end;

procedure TForm1.OnVersionMessge(Sender: TObject;
  versionMessage: TVersionMessage);
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

procedure TForm1.TrayIcon1DblClick(Sender: TObject);
begin
  { Hide the tray icon and show the window,
    setting its state property to wsNormal. }
  TrayIcon1.Visible := False;
  Show();
  WindowState := wsNormal;
  Application.BringToFront();
end;

end.
