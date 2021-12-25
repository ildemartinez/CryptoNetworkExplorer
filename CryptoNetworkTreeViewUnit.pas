unit CryptoNetworkTreeViewUnit;

interface

uses
  System.SysUtils, System.Classes, System.Types, vcl.Menus,
  BTCNetworkUnit, isubjectunit, BTCPeerNodeUnit, CryptoNetworkPopupMenuUnit,
  NodeObserverPattern, ipeernodeunit,
  VirtualTrees;

type

  TNodeTypes = (ntroot, ntnetwork, ntnode);

  TTreeData = record
    node_type: TNodeTypes;
    networkdata: TBTCNetwork;
    nodedata: TBTCPeerNode;
    Text: String;
  end;

  PTreeData = ^TTreeData; // This is a node example.

  TCryptoNetworkTreeView = class(TCustomVirtualStringTree, INetworkObserver,
    INodeObserver)
  private
    fCryptonetwork: TBTCNetwork;
    // fPopupMenu: TCryptoNetworkPopupMenu;
    procedure setCryptoNetwork(const Value: TBTCNetwork);

    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure upddate;
  protected
    procedure DoInitNode(Parent, Node: PVirtualNode;
      var InitStates: TVirtualNodeInitStates); override;
    procedure MenuItemClick(Sender: TObject);
    procedure DoGetPopupmenu(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const P: TPoint; var AskParent: Boolean;
      var PopupMenu: TPopupMenu);

    procedure NodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure DoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure DoInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure DoPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);

  public
    constructor Create(Owner: TComponent); override;

    // I
    procedure NewBTCAgentAdded(aBTCAgent: TBTCPeerNode);
    procedure NodeConnected(aBTCAgent: TBTCPeerNode);
    // I
    procedure DoNotify(const msgtype: TMSGType; const aNode: INode);
  published
    property CryptoNetwork: TBTCNetwork read fCryptonetwork
      write setCryptoNetwork;
  end;

procedure Register;

implementation

uses
  vcl.Graphics,
  dialogs,
  NodeFormUnit;

procedure Register;
begin
  RegisterComponents('CryptoCurrency', [TCryptoNetworkTreeView]);
end;

constructor TCryptoNetworkTreeView.Create(Owner: TComponent);
var
  aMenuItem: TMenuItem;
begin
  inherited;

  // por el momento ponemos aquí las acciones
  PopupMenu := TCryptoNetworkPopupMenu.Create(self);
  aMenuItem := TMenuItem.Create(self);
  aMenuItem.caption := 'Connect';
  aMenuItem.OnClick := MenuItemClick;

  PopupMenu.items.add(aMenuItem);
  //

  NodeDataSize := SizeOf(TTreeData);

  self.TreeOptions.SelectionOptions := TreeOptions.SelectionOptions +
    [toRightClickSelect];
  OnGetText := DoGetText;
  OnInitChildren := DoInitChildren;
  OnGetPopupMenu := DoGetPopupmenu;
  OnNodeDblClick := NodeDblClick;
  OnPaintText := DoPaintText;

  // RootNodeCount := 5;
end;

procedure TCryptoNetworkTreeView.DoGetPopupmenu(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const P: TPoint;
  var AskParent: Boolean; var PopupMenu: TPopupMenu);
var
  data, parentdata: PTreeData;
begin
  data := GetNodeData(Node);

  case data^.node_type of
    ntroot:
      ;
    ntnetwork:
      ;
    ntnode:
      ;
  end;
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
        if CryptoNetwork <> nil then
          ChildCount := CryptoNetwork.count;
      ntnode:
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
    Node.States := Node.States + [vsHasChildren, vsExpanded];
  end
  else
    case parentdata^.node_type of
      ntroot:
        begin
          data^.node_type := ntnetwork;
          data^.networkdata := self.fCryptonetwork;
          Node.States := Node.States + [vsHasChildren, vsExpanded];
        end;
      ntnetwork:
        begin
          data^.node_type := ntnode;
          data^.nodedata := CryptoNetwork.nodes[Node.Index];
          AttachObserverToSubject(self, CryptoNetwork.nodes[Node.Index]);
        end;
    end;

end;

procedure TCryptoNetworkTreeView.DoNotify(const msgtype: TMSGType;
  const aNode: INode);
var
  aVirtualNodeEnumerator: TVTVirtualNodeEnumerator;
  data: PTreeData;
begin
  // todo optimizar la salida
  aVirtualNodeEnumerator := nodes.GetEnumerator;

  while aVirtualNodeEnumerator.MoveNext do
  begin
    data := GetNodeData(aVirtualNodeEnumerator.Current);
    if data^.node_type = ntnode then
    begin
      if data^.nodedata.PeerIp = aNode.GetIP then
        InvalidateNode(aVirtualNodeEnumerator.Current)
    end;
  end;

end;

procedure TCryptoNetworkTreeView.DoPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  data, parentdata: PTreeData;
begin
  data := GetNodeData(Node);
  parentdata := GetNodeData(Node.Parent);

  case data^.node_type of
    ntroot:
      ;
    ntnetwork:
      ;
    ntnode:
      if data^.nodedata.Connected then
        TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold]
      else
        TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsBold]

  end;

end;

procedure TCryptoNetworkTreeView.MenuItemClick(Sender: TObject);
var
  aVirtualNodeEnumerator: TVTVirtualNodeEnumerator;
  data: PTreeData;
begin
  aVirtualNodeEnumerator := SelectedNodes.GetEnumerator;

  while aVirtualNodeEnumerator.MoveNext do
  begin
    data := GetNodeData(aVirtualNodeEnumerator.Current);
    if data^.node_type = ntnode then
    begin
      data^.nodedata.Connect();
    end
    else if data^.node_type = ntnetwork then
    begin
      data^.networkdata.Connect;
    end
  end;
end;

procedure TCryptoNetworkTreeView.NewBTCAgentAdded(aBTCAgent: TBTCPeerNode);
begin

  // InitNode(GetFirst(false).FirstChild);
  ReinitNode(GetFirst(), true);
end;

procedure TCryptoNetworkTreeView.NodeConnected(aBTCAgent: TBTCPeerNode);
begin

end;

procedure TCryptoNetworkTreeView.NodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  aVirtualNodeEnumerator: TVTVirtualNodeEnumerator;
  data: PTreeData;
begin
  aVirtualNodeEnumerator := SelectedNodes.GetEnumerator;

  while aVirtualNodeEnumerator.MoveNext do
  begin
    data := GetNodeData(aVirtualNodeEnumerator.Current);
    if data^.node_type = ntnode then
    begin
      with TNodeForm.Create(self) do
      begin
        Node := data^.nodedata;
        show();
      end;
    end;
  end;

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
