unit BTCNetworkUnit;

interface

uses
  classes, Generics.Collections,
  IPeerNodeunit,
  BTCPeerDiscoveryUnit, BTCTypes, ISubjectUnit, BTCPeerNodeUnit,
  NodeObserverPattern;

type
  TBTCNetwork = class(TComponent, ISubject, INodeObserver)
  strict private
    fObserverList: Tlist<INetworkObserver>;

    fBTCPeerDiscovery: TBTCPeerDiscovery;
    fBTCAgents: Tlist<TBTCPeerNode>;

    fDirectNode: TBTCRPCNode;

    function GetMaxPeers: cardinal;
    procedure SetMaxPeers(const Value: cardinal);

    procedure OnDiscovered(Sender: TObject; Peer: string);
    procedure DoVersionMessage(Sender: TObject;
      versionMessage: TVersionMessage);
  private
    fMessageVersionEvent: TMessageEventVersion;
    function getcount: cardinal;
    function GetNodes(index: integer): TBTCPeerNode;
    function GetDirectNode: TBTCPeerNode;
    // procedure NodeConnected;

  public
    procedure AttachToSubject(aINodeSubject: INodeObservable);
    procedure DoNotify(const msgtype: TMSGType; const aNode: INode);

    procedure RegisterObserver(aObserver: INetworkObserver);
    procedure NotifyNewBTCAgent(const aBTCAgent: TBTCPeerNode);
    procedure NotifyNodeConnected(const aBTCAgent: TBTCPeerNode);

    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure connect;
    property Nodes[index: integer]: TBTCPeerNode read GetNodes;
    property DirectNode: TBTCPeerNode read GetDirectNode;
  published
    property MaxPeers: cardinal read GetMaxPeers write SetMaxPeers;
    property count: cardinal read getcount;

    property OnVersionMessage: TMessageEventVersion read fMessageVersionEvent
      write fMessageVersionEvent;

  end;

procedure Register;

implementation

uses
  sysutils, forms,
  inifiles;

procedure Register;
begin
  RegisterComponents('CryptoCurrency', [TBTCNetwork]);
end;

{ TBTCNetwork }

procedure TBTCNetwork.AttachToSubject(aINodeSubject: INodeObservable);
begin

end;

procedure TBTCNetwork.connect;
begin
  fBTCPeerDiscovery.Discover;
end;

constructor TBTCNetwork.Create(Owner: TComponent);
var
  aIniFile: TIniFile;
  aServer, aUser, aPass: string;
  aPort: integer;
  aini: string;
begin
  inherited;

  fObserverList := Tlist<INetworkObserver>.Create;

  fBTCPeerDiscovery := TBTCPeerDiscovery.Create(self);
  fBTCPeerDiscovery.OnResponse := OnDiscovered;

  fBTCAgents := Tlist<TBTCPeerNode>.Create;

  // Lets check if exists RPC Node in the ini file
  aini := ExtractFilePath(Application.ExeName) + 'config.ini';
  aIniFile := TIniFile.Create(aini);
  aServer := aIniFile.ReadString('RPCBTCNODE', 'rpcbind', '');
  aUser := aIniFile.ReadString('RPCBTCNODE', 'rpcuser', '');
  aPass := aIniFile.ReadString('RPCBTCNODE', 'rpcpassword', '');
  aPort := aIniFile.ReadInteger('RPCBTCNODE', 'rpcport', 8332);

  // If exists create a default connection node
  fDirectNode := TBTCRPCNode.Create(self);

  fDirectNode.PeerIp := aServer;
  fDirectNode.rpcuser := aUser;
  fDirectNode.rpcpassword := aPass;
  fDirectNode.rpcport := aPort;

  fBTCAgents.add(fDirectNode);
  NotifyNewBTCAgent(fDirectNode);
  // fDirectNode.Connect;
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

procedure TBTCNetwork.DoNotify(const msgtype: TMSGType; const aNode: INode);
begin

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

function TBTCNetwork.GetDirectNode: TBTCPeerNode;
begin
  result := fDirectNode;
end;

function TBTCNetwork.GetMaxPeers: cardinal;
begin
  result := fBTCPeerDiscovery.MaxPeers;
end;

function TBTCNetwork.GetNodes(index: integer): TBTCPeerNode;
begin
  result := self.fBTCAgents[index];
end;

procedure TBTCNetwork.NotifyNewBTCAgent(const aBTCAgent: TBTCPeerNode);
var
  aObserver: INetworkObserver;
begin
  for aObserver in fObserverList do
  begin
    aObserver.NewBTCAgentAdded(aBTCAgent);
  end;
end;

procedure TBTCNetwork.NotifyNodeConnected(const aBTCAgent: TBTCPeerNode);
var
  aObserver: INetworkObserver;
begin
  for aObserver in fObserverList do
  begin
    aObserver.NodeConnected(aBTCAgent);
  end;
end;

procedure TBTCNetwork.OnDiscovered(Sender: TObject; Peer: string);
var
  aBTCAgent: TBTCPeerNode;
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
    aBTCAgent := TBTCPeerNode.Create(self);
    aBTCAgent.OnVersionMessage := DoVersionMessage;
    aBTCAgent.PeerIp := Peer;

    AttachObserverToSubject(self, aBTCAgent);
    // aBTCAgent.NodeSubject.RegisterObserver(self);

    fBTCAgents.add(aBTCAgent);

    NotifyNewBTCAgent(aBTCAgent);

    // aBTCAgent.connect;
  end;
end;

procedure TBTCNetwork.RegisterObserver(aObserver: INetworkObserver);
begin
  if fObserverList.IndexOf(aObserver) < 0 then
    fObserverList.add(aObserver);
end;

procedure TBTCNetwork.SetMaxPeers(const Value: cardinal);
begin
  fBTCPeerDiscovery.MaxPeers := Value;
end;

end.
