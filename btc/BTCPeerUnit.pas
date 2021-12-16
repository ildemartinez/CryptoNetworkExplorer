unit BTCPeerUnit;

interface

uses
  classes, ipwcore, ipwtypes,
  ipwdns;

type
  TSeedsNotifyEvent = procedure(Sender: TObject; Peer: string) of object;

  TBTCPeerComponent = class(TComponent)
  private
    fDNS: TipwDNS;

    FOnResponse: TSeedsNotifyEvent;

  protected
    procedure DoResponse(Sender: TObject; RequestId: Integer;
      const Domain: String; StatusCode: Integer; const Description: String;
      Authoritative: Boolean);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure GetSeeds;

    property OnResponse: TSeedsNotifyEvent read FOnResponse write FOnResponse;
  end;

implementation

{ TBTCSeeder }

constructor TBTCPeerComponent.Create;
begin
  fDNS := TipwDNS.Create(self);
  fDNS.OnResponse := DoResponse;
end;

destructor TBTCPeerComponent.Destroy;
begin
  fDNS.free;

  inherited;
end;

procedure TBTCPeerComponent.GetSeeds;
begin
  self.fDNS.Query('seed.bitcoin.sipa.be');
end;

procedure TBTCPeerComponent.DoResponse(Sender: TObject; RequestId: Integer;
  const Domain: String; StatusCode: Integer; const Description: String;
  Authoritative: Boolean);
var
  k: Integer;
begin
  for k := 0 to fDNS.RecordCount - 1 do
  begin
    if Assigned(FOnResponse) then
      FOnResponse(self, fDNS.RecordFieldValue[k]);
  end;
end;

end.
