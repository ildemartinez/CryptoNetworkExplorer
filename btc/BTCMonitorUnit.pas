unit BTCMonitorUnit;

interface

uses
  classes, generics.Collections, BTCPeerUnit, BTCThreadMonitorUnit;

type

  TBTCMonitorComponent = class(TComponent)
  private
    fBTCPeerComponent: TBTCPeerComponent;

    fMaxConnections: integer;
    fConnectionsPool: TObjectList<TBTCThreadMonitor>;

    procedure Response(Sender: TObject; Peer: String);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    property MaxConnections: integer read fMaxConnections write fMaxConnections;
  end;

implementation

uses
  dialogs;

constructor TBTCMonitorComponent.Create(Owner: TComponent);
begin
  inherited;

  fMaxConnections := 1;

  fConnectionsPool := TObjectList<TBTCThreadMonitor>.Create;
  fConnectionsPool.OwnsObjects := true;

  fBTCPeerComponent := TBTCPeerComponent.Create(self);
  fBTCPeerComponent.OnResponse := Response;
  fBTCPeerComponent.GetSeeds();

end;

destructor TBTCMonitorComponent.Destroy;
begin
  // Destroy ConnectionsPool
  fConnectionsPool.free;

  // Destroy BTCPeer
  fBTCPeerComponent.free;

  inherited;
end;

procedure TBTCMonitorComponent.Response(Sender: TObject; Peer: String);
begin
  // Create just MaxConnections

  if fConnectionsPool.Count < MaxConnections then
  begin
    fConnectionsPool.Add(TBTCThreadMonitor.Create(Peer,self));
  end;
end;

end.
