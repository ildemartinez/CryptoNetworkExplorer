unit BTCPeerDiscoveryUnit;

interface

uses
  System.SysUtils, System.Classes, ipwcore, ipwtypes,
  ipwdns;

type
  TSeedsNotifyEvent = procedure(Sender: TObject; Peer: string) of object;

  TBTCPeerDiscovery = class(TComponent)
  strict private
    fDNS: TipwDNS;
    fMaxPeers : cardinal;

    FOnResponse: TSeedsNotifyEvent;

    procedure DoResponse(Sender: TObject; RequestId: Integer;
      const Domain: String; StatusCode: Integer; const Description: String;
      Authoritative: Boolean);
  public
    constructor Create(Owner: TComponent); override;
    procedure Discover;
  published
    property MaxPeers : cardinal read fMaxPeers write fMaxPeers;
    property OnResponse: TSeedsNotifyEvent read FOnResponse write FOnResponse;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CryptoCurrency', [TBTCPeerDiscovery]);
end;

{ TPeerDiscoveryBTC }

constructor TBTCPeerDiscovery.Create(Owner: TComponent);
begin
  inherited;

  fDNS := TipwDNS.Create(self);
  fDNS.OnResponse := DoResponse;
end;

procedure TBTCPeerDiscovery.Discover;
begin
  fDNS.Query('seed.bitcoin.sipa.be');
end;

procedure TBTCPeerDiscovery.DoResponse(Sender: TObject; RequestId: Integer;
  const Domain: String; StatusCode: Integer; const Description: String;
  Authoritative: Boolean);
var
  k: Integer;
begin
  for k := 0 to fDNS.RecordCount - 1 do
  begin
    if Assigned(FOnResponse) and (k<fMaxPeers) then
      FOnResponse(self, fDNS.RecordFieldValue[k]);
  end;

end;

end.
