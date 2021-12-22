unit BTCNetworkUnit;

interface

uses
  classes, Generics.Collections,
  BTCPeerDiscoveryUnit, BTCAgentUnit, BTCTypes, ISubjectUnit;

type
  TBTCNetwork = class(TComponent, ISubject)
  strict private
    fObserverList: Tlist<iobserver>;

    fBTCPeerDiscovery: TBTCPeerDiscovery;
    fBTCAgents: Tlist<TBTCAgent>;
    fMaxPeers: cardinal;

    function GetMaxPeers: cardinal;
    procedure SetMaxPeers(const Value: cardinal);

    procedure OnDiscovered(Sender: TObject; Peer: string);
    procedure DoVersionMessage(Sender: TObject;
      versionMessage: TVersionMessage);
  private
    fMessageVersionEvent: TMessageEventVersion;
    function getcount: cardinal;
    function GetNodes(index: integer): TBTCAgent;

  public

    procedure RegisterObserver(aObserver: iobserver);
    procedure NotifyNewBTCAgent(const aBTCAgent: TBTCAgent);

    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure connect;
    property Nodes[index: integer]: TBTCAgent read GetNodes;
  published
    property MaxPeers: cardinal read GetMaxPeers write SetMaxPeers;
    property count: cardinal read getcount;

    property OnVersionMessage: TMessageEventVersion read fMessageVersionEvent
      write fMessageVersionEvent;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CryptoCurrency', [TBTCNetwork]);
end;

{ TBTCNetwork }

procedure TBTCNetwork.connect;
begin
  fBTCPeerDiscovery.Discover;
end;

constructor TBTCNetwork.Create(Owner: TComponent);
begin
  inherited;

  fObserverList := Tlist<iobserver>.Create;

  fBTCPeerDiscovery := TBTCPeerDiscovery.Create(self);
  fBTCPeerDiscovery.OnResponse := OnDiscovered;

  fBTCAgents := Tlist<TBTCAgent>.Create;
end;

destructor TBTCNetwork.Destroy;
var
  I: integer;
begin
  for I := 0 to fBTCAgents.count - 1 do
  begin
    fBTCAgents.Items[I].free;
  end;

  inherited;
end;

procedure TBTCNetwork.DoVersionMessage(Sender: TObject;
  versionMessage: TVersionMessage);
begin
  if assigned(fMessageVersionEvent) then
    fMessageVersionEvent(self, versionMessage);
end;

function TBTCNetwork.getcount: cardinal;
begin
  result := fBTCAgents.count;
end;

function TBTCNetwork.GetMaxPeers: cardinal;
begin
  result := fBTCPeerDiscovery.MaxPeers;
end;

function TBTCNetwork.GetNodes(index: integer): TBTCAgent;
begin
  result := self.fBTCAgents[index];
end;

procedure TBTCNetwork.NotifyNewBTCAgent(const aBTCAgent: TBTCAgent);
var
  aObserver: iobserver;
begin
  for aObserver in fObserverList do
  begin
    aObserver.NewBTCAgentAdded(aBTCAgent);
  end;
end;

procedure TBTCNetwork.OnDiscovered(Sender: TObject; Peer: string);
var
  aBTCAgent: TBTCAgent;
  k: integer;
  exists: boolean;
begin

  // TODO Refactor

  exists := false;
  // Search if exists in the list
  for k := 0 to fBTCAgents.count - 1 do
  begin
    if fBTCAgents[k].PeerIp = Peer then
      exists := true;
  end;

  if not exists then
  begin
    aBTCAgent := TBTCAgent.Create(self);
    aBTCAgent.OnVersionMessage := DoVersionMessage;
    aBTCAgent.PeerIp := Peer;

    fBTCAgents.Add(aBTCAgent);

    NotifyNewBTCAgent(aBTCAgent);

    // aBTCAgent.connect;
  end;
end;

procedure TBTCNetwork.RegisterObserver(aObserver: iobserver);
begin
  fObserverList.Add(aObserver);
end;

procedure TBTCNetwork.SetMaxPeers(const Value: cardinal);
begin
  fBTCPeerDiscovery.MaxPeers := Value;
end;

end.
