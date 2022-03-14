unit PeerNodeUnit;

interface

uses
  classes,
  IPeerNodeUnit,
  NodeObserverPattern;

type
  TPeerNode = class(TComponent, INode, INodeObservable)
  strict private
    fImplNodeObsevable: TNodeObservable;
    fIP: string;
    function GetAgent: string;
    function GetIP: string;
    procedure SetPeerIP(const Value: string);
  protected
    fConnected: boolean;
    fServerConnected: boolean;
    fUserAgent: string;
    procedure GetPeers(); virtual;
    property ImplNodeObsevable: TNodeObservable
      read fImplNodeObsevable write fImplNodeObsevable
      implements INodeObservable;
  public
    constructor Create(Owner: TComponent); override;
    function Connected: boolean;
    function ServerConnected: boolean;
  published
    property Agent: string read GetAgent;
    property PeerIp: string read GetIP write SetPeerIP;
  end;

implementation

constructor TPeerNode.Create(Owner: TComponent);
begin
  inherited;

  fServerConnected := false;
  fConnected := false;
  fUserAgent := '';

  fImplNodeObsevable := TNodeObservable.Create;
end;

function TPeerNode.Connected: boolean;
begin
  result := fConnected;
end;

function TPeerNode.GetAgent: string;
begin
  result := fUserAgent;
end;

function TPeerNode.GetIP: string;
begin
  result := fIP;
end;

procedure TPeerNode.GetPeers;
begin

end;

function TPeerNode.ServerConnected: boolean;
begin
  result := fServerConnected;
end;

procedure TPeerNode.SetPeerIP(const Value: string);
begin
  fIP := Value;
end;

end.
