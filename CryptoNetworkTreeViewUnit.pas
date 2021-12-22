unit CryptoNetworkTreeViewUnit;

interface

uses
  System.SysUtils, System.Classes,
  BTCNetworkUnit, isubjectunit, BTCAgentUnit,
  VirtualTrees;

type

  TNodeTypes = (ntroot, ntnetwork, ntnode);

  TTreeData = record
    node_type: TNodeTypes;
    nodedata : TBTCAgent;
    Text: String;
  end;

  PTreeData = ^TTreeData; // This is a node example.

  TCryptoNetworkTreeView = class(TCustomVirtualStringTree, IObserver)
  private
    fCryptonetwork: TBTCNetwork;
    procedure setCryptoNetwork(const Value: TBTCNetwork);

    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure upddate;
  protected
    procedure DoInitNode(Parent, Node: PVirtualNode;
      var InitStates: TVirtualNodeInitStates); override;

    procedure DoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure DoInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
  public
    constructor Create(Owner: TComponent); override;

    procedure NewBTCAgentAdded(aBTCAgent: TBTCAgent);
  published
    property CryptoNetwork: TBTCNetwork read fCryptonetwork
      write setCryptoNetwork;
  end;

procedure Register;

implementation

uses
dialogs;

procedure Register;
begin
  RegisterComponents('CryptoCurrency', [TCryptoNetworkTreeView]);
end;

{ TCryptoNetworkTreeView }

constructor TCryptoNetworkTreeView.Create(Owner: TComponent);
begin
  inherited;

  NodeDataSize := SizeOf(TTreeData);

  OnGetText := DoGetText;
  OnInitChildren := DoInitChildren;

  RootNodeCount := 5;
end;

procedure TCryptoNetworkTreeView.DoGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  data, parentdata: PTreeData;
begin
  data := GetNodeData(Node);
  parentdata := GetNodeData(Node.Parent);

  case data^.node_type of
    ntroot:
      CellText := 'Networks';
    ntnetwork:
      CellText := 'BTC Network';
    ntnode:
     // if data^.nodedata <> nil then
        CellText := data^.nodedata.PeerIp;
  end;

end;

procedure TCryptoNetworkTreeView.DoInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  data, parentdata: PTreeData;
begin
  data := GetNodeData(Node);
  parentdata := GetNodeData(Node.Parent);

  if data <> nil then
  begin
    case data^.node_type of
      ntroot:
        ChildCount := 1;
      ntnetwork:
        ChildCount := CryptoNetwork.count;
      ntnode :
        ChildCount := 0;
    end;
  end;
end;

procedure TCryptoNetworkTreeView.DoInitNode(Parent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  data, parentdata: PTreeData;
begin
  data := GetNodeData(Node);
  parentdata := GetNodeData(Parent);

  if parentdata = nil then
  begin
    data^.node_type := ntroot;
    Node.States := Node.States + [vsHasChildren];
  end
  else
    case parentdata^.node_type of
      ntroot:
        begin
          data^.node_type := ntnetwork;
          Node.States := Node.States + [vsHasChildren];
        end;
      ntnetwork:
        begin
          data^.node_type := ntnode;
          data^.nodedata := CryptoNetwork.nodes[node.Index];
        end;
    end;

end;

procedure TCryptoNetworkTreeView.NewBTCAgentAdded(aBTCAgent: TBTCAgent);
var
  aEnumerator : TVTVirtualNodeEnumerator;
begin

  //InitNode(GetFirst(false).FirstChild);
  ReinitNode(GetFirst(),true);


end;

procedure TCryptoNetworkTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
    if AComponent = CryptoNetwork then
      CryptoNetwork := nil;
end;

procedure TCryptoNetworkTreeView.setCryptoNetwork(const Value: TBTCNetwork);
begin
  fCryptonetwork := Value;
  if Value <> nil then
  begin
    RootNodeCount := 1;

    Value.FreeNotification(self);
    Value.RegisterObserver(self);
  end;
end;

procedure TCryptoNetworkTreeView.upddate;
begin

end;

end.
