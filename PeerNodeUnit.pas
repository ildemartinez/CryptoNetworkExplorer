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
  strict protected
    fServerConnected: boolean;
    fConnected: boolean;

    property ImplNodeObsevable: TNodeObservable read fImplNodeObsevable
      write fImplNodeObsevable implements INodeObservable;
  public
    constructor Create(Owner: TComponent); override;

    function ServerConnected: boolean;
    function Connected: boolean;
  published
    property PeerIp: string read GetIP write SetPeerIP;
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

  fImplNodeObsevable := TNodeObservable.Create;
end;

function TPeerNode.GetIP: string;
begin
  result := fIP;
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
