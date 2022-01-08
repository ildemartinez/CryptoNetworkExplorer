unit PeerNodeUnit;

interface

uses
  classes,
  IPeerNodeUnit,
  NodeObserverPattern;

type
  TPeerNode = class(TComponent, INode, INodeObservable)
  strict private
    fIP: string;

    fImplNodeObsevable: TNodeObservable;

    function GetIP: string;
    procedure SetPeerIP(const Value: string);
  strict private
    function GetAgent: string;
  protected
    fServerConnected: boolean;
    fConnected: boolean;

    property ImplNodeObsevable: TNodeObservable read fImplNodeObsevable write fImplNodeObsevable
      implements INodeObservable;
  protected
    fUserAgent: string;

    procedure GetPeers(); virtual;
  public
    constructor Create(Owner: TComponent); override;

    function ServerConnected: boolean;
    function Connected: boolean;

  published
    property PeerIp: string read GetIP write SetPeerIP;
    property Agent: string read GetAgent;
  end;

implementation

function TPeerNode.Connected: boolean;
begin
  result := fConnected;
end;

constructor TPeerNode.Create(Owner: TComponent);
begin
  inherited;

  fServerConnected := false;
  fConnected := false;
  fUserAgent := '';

  fImplNodeObsevable := TNodeObservable.Create;
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
