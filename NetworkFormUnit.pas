unit NetworkFormUnit;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  BTCNetworkUnit,
  ISubjectUnit,
  Vcl.Controls,
  Vcl.Forms,
  cryptonetworktreeviewunit,
  Vcl.Dialogs,
  BTCPeerNodeUnit,
  VirtualTrees;

type
  TNetworkForm = class(TForm, INetworkObserver)
  private
    ftree: tcryptonetworktreeview;
    fNetwork: TBTCNetwork;
    procedure SetNetwork(const Value: TBTCNetwork);
  public
    { Public declarations }
    constructor Create(Owner: Tcomponent); override;
    procedure NewBTCAgentAdded(aBTCAgent: TBTCPeerNode);
    procedure NodeConnected(aBTCAgent: TBTCPeerNode);

    property Network: TBTCNetwork read fNetwork write SetNetwork;
  end;

implementation

{$R *.dfm}
{ TNetworkForm }

constructor TNetworkForm.Create(Owner: Tcomponent);
var
  acolumn: TVirtualTreeColumn;
begin
  inherited;

  ftree := tcryptonetworktreeview.Create(self);
  ftree.BeginUpdate;
  ftree.parent := self;
  ftree.Align := alclient;
  ftree.AsTree := false;

  with ftree.Header do
  begin
    Columns.clear;
    acolumn := Columns.Add;
    acolumn.Width := 150;
    acolumn.Text := 'Server';

    acolumn := Columns.Add;
    acolumn.Width := 150;
    acolumn.Text := 'Agent';

    Options := Options + [hovisible];
  end;

  ftree.EndUpdate;

end;

procedure TNetworkForm.NewBTCAgentAdded(aBTCAgent: TBTCPeerNode);
begin

end;

procedure TNetworkForm.NodeConnected(aBTCAgent: TBTCPeerNode);
begin

end;

procedure TNetworkForm.SetNetwork(const Value: TBTCNetwork);
begin
  fNetwork := Value;
  // Value.RegisterObserver(self);

  ftree.CryptoNetwork := Value;
end;

end.
