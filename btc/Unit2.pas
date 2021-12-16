unit Unit2;

interface

uses
  classes, ipwcore, ipwtypes,
  ipwdns;

type
  TSeedsNotifyEvent = procedure(Sender: TObject; Peer : string) of object;

  TBTCSeeder = class(TComponent)
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

constructor TBTCSeeder.Create;
begin
  fDNS := TipwDNS.Create(self);
  fDNS.OnResponse := DoResponse;
end;

destructor TBTCSeeder.Destroy;
begin
  fDNS.free;

  inherited;
end;

procedure TBTCSeeder.GetSeeds;
begin
  self.fDNS.Query('seed.bitcoin.sipa.be');
end;

procedure TBTCSeeder.DoResponse(Sender: TObject; RequestId: Integer;
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
