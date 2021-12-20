unit CryptoLabMainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.AppEvnts,
  BTCPeerDiscoveryUnit, BTCAgentUnit, BTCTypes;

type
  TForm1 = class(TForm)

    Memo1: TMemo;
    TrayIcon1: TTrayIcon;
    ApplicationEvents1: TApplicationEvents;
    Button1: TButton;
    BTCPeerDiscovery1: TBTCPeerDiscovery;
    BTCAgent1: TBTCAgent;
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure ApplicationEvents1Minimize(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BTCPeerDiscovery1Response(Sender: TObject; Peer: string);
    procedure BTCAgent1Message(Sender: TObject; const aMessage: string);
    procedure BTCAgent1VerackMessage(Sender: TObject);
    procedure BTCAgent1VersionMessage(Sender: TObject;
      versionMessage: TVersion);


  private
    { Private declarations }
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


procedure TForm1.BTCAgent1Message(Sender: TObject; const aMessage: string);
begin
   Memo1.Lines.add('Left to implement ->'+aMessage);
end;

procedure TForm1.BTCAgent1VerackMessage(Sender: TObject);
begin
  Memo1.lines.add('verack');
end;

procedure TForm1.BTCAgent1VersionMessage(Sender: TObject;
  versionMessage: TVersion);
begin

  memo1.Lines.add('version :'+versionMessage.protocol_version.ToString);
end;

procedure TForm1.BTCPeerDiscovery1Response(Sender: TObject; Peer: string);
begin
  Memo1.lines.add(Peer);

  // solamente pasamos el último
  self.BTCAgent1.disconnect;
  BTCAgent1.PeerIp := Peer;
  self.BTCAgent1.connect;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  BTCPeerDiscovery1.Discover;
end;

constructor TForm1.Create(Owner: TComponent);
begin
  inherited;

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
