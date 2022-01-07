unit CryptoNetworkTreeViewUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  vcl.Menus,
  BTCNetworkUnit,
  isubjectunit,
  BTCPeerNodeUnit,
  CryptoNetworkPopupMenuUnit,
  NodeObserverPattern,
  ipeernodeunit,
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

  TCryptoNetworkTreeView = class(TCustomVirtualStringTree, INetworkObserver, INodeObserver)
  private
    fCryptonetwork: TBTCNetwork;
    fAsTree: boolean;

    // fPopupMenu: TCryptoNetworkPopupMenu;
    procedure setCryptoNetwork(const Value: TBTCNetwork);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure upddate;
    procedure SetOptions(const Value: boolean);
    procedure SetAsTree(const Value: boolean);
  protected
    procedure DoInitNode(Parent, Node: PVirtualNode;
      var InitStates: TVirtualNodeInitStates); override;
    procedure MenuItemClick(Sender: TObject);
    procedure MenuItemClickGetPeers(Sender: TObject);
    procedure DoGetPopupmenu(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      const P: TPoint; var AskParent: boolean; var PopupMenu: TPopupMenu);

    procedure NodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure DoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure DoInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure DoPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure GetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: boolean; var ImageIndex: TImageIndex);
  public
    constructor Create(Owner: TComponent); override;

    // I
    procedure NewBTCAgentAdded(aBTCAgent: TBTCPeerNode);
    procedure NodeConnected(aBTCAgent: TBTCPeerNode);
    // I
    procedure DoNotify(const msgtype: TMSGType; const aNode: INode);
  published
    property CryptoNetwork: TBTCNetwork read fCryptonetwork write setCryptoNetwork;
    property AsTree: boolean write SetAsTree;
  end;

procedure Register;

{$R resources.res}

implementation

uses
  vcl.Graphics,
  vcl.ImgList,
  dialogs,
  NodeFormUnit,
  networkformunit,
  Ut_images;

procedure Register;
begin
  RegisterComponents('CryptoCurrency', [TCryptoNetworkTreeView]);
end;

constructor TCryptoNetworkTreeView.Create(Owner: TComponent);
var
  aMenuItem: TMenuItem;
begin
  inherited;

  PopupMenu := TCryptoNetworkPopupMenu.Create(self);
  // por el momento ponemos aqu� las acciones
  aMenuItem := TMenuItem.Create(self);
  aMenuItem.caption := 'Connect';
  aMenuItem.OnClick := MenuItemClick;
  PopupMenu.items.add(aMenuItem);
  //

  // por el momento ponemos aqu� las acciones
  aMenuItem := TMenuItem.Create(self);
  aMenuItem.caption := 'Get Peers';
  aMenuItem.OnClick := MenuItemClickGetPeers;
  PopupMenu.items.add(aMenuItem);
  //

  NodeDataSize := SizeOf(TTreeData);

  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toRightClickSelect,
  // toLevelSelectConstraint,
  tomultiselect, toSiblingSelectConstraint];

  OnGetText := DoGetText;
  OnInitChildren := DoInitChildren;
  OnGetPopupMenu := DoGetPopupmenu;
  OnNodeDblClick := NodeDblClick;
  OnPaintText := DoPaintText;
  OnGetImageIndex := GetImageIndex;

  Images := GetGlobalImagesList();
  // RootNodeCount := 5;
end;

procedure TCryptoNetworkTreeView.DoGetPopupmenu(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; const P: TPoint; var AskParent: boolean; var PopupMenu: TPopupMenu);
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

procedure TCryptoNetworkTreeView.DoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  data, parentdata: PTreeData;

begin
  data := GetNodeData(Node);
  parentdata := GetNodeData(Node.Parent);

  if fAsTree then
  begin
    case data^.node_type of
      ntroot:
        CellText := 'Networks';
      ntnetwork:
        CellText := 'BTC Network';
      ntnode:
        // if data^.nodedata <> nil then
        CellText := data^.nodedata.PeerIp;
    end;
  end
  else
    case Column of
      0:
        begin
          case data^.node_type of
            ntnetwork:
              CellText := '';
            ntnode:
              CellText := data^.nodedata.PeerIp;
          end;
        end;
      1:
        begin
          case data^.node_type of
            ntnetwork:
              CellText := '';
            ntnode:
              CellText := data^.nodedata.Agent;
          end;
        end;
    end;
end;

procedure TCryptoNetworkTreeView.DoInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var ChildCount: Cardinal);
var
  data, parentdata: PTreeData;
begin
  data := GetNodeData(Node);
  parentdata := GetNodeData(Node.Parent);

  if data <> nil then
  begin

    if fAsTree then
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
    end
    else
    begin
      case data^.node_type of
        ntnetwork:
          if CryptoNetwork <> nil then
            ChildCount := CryptoNetwork.count;
        ntnode:
          ChildCount := 0;
      end;
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

  if fAsTree then
  begin
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
  end
  else
  begin
    if parentdata = nil then
    begin
      data^.node_type := ntnetwork;
      data^.networkdata := self.fCryptonetwork;
      Node.States := Node.States + [vsHasChildren, vsExpanded];
    end
    else
      case parentdata^.node_type of
        ntnetwork:
          begin
            data^.node_type := ntnode;
            data^.nodedata := CryptoNetwork.nodes[Node.Index];
            AttachObserverToSubject(self, CryptoNetwork.nodes[Node.Index]);
          end;
      end;
  end;

end;

procedure TCryptoNetworkTreeView.DoNotify(const msgtype: TMSGType; const aNode: INode);
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

procedure TCryptoNetworkTreeView.DoPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
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
      begin
        if data^.nodedata.Connected then
        begin
          TargetCanvas.Font.Color := clBlack;
          TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold] - [fsItalic];

        end
        else if data^.nodedata.serverconnected then
        begin

          TargetCanvas.Font.Color := clBlack;
          TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsItalic] - [fsBold];
        end
        else
        begin
          TargetCanvas.Font.Color := clGray;
          TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsItalic] - [fsBold];
        end;
      end;

  end;

end;

procedure TCryptoNetworkTreeView.GetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: boolean; var ImageIndex: TImageIndex);
Var
  ResName: String;
  H: THandle;
begin
  ImageIndex := -1; // GetGlobalImagesList.GetImageIndexByName('');
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

procedure TCryptoNetworkTreeView.MenuItemClickGetPeers(Sender: TObject);
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
      data^.nodedata.GetPeers();
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

procedure TCryptoNetworkTreeView.NodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
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
    end
    else if data^.node_type = ntnetwork then
    begin
      with TNetworkForm.Create(self) do
      begin
        Network := data^.networkdata;
        show();
      end;
    end;

  end;

end;

procedure TCryptoNetworkTreeView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
    if AComponent = CryptoNetwork then
      CryptoNetwork := nil;
end;

procedure TCryptoNetworkTreeView.SetAsTree(const Value: boolean);
begin
  fAsTree := Value;

  // As Tree
  if Value = true then
  begin
    TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toShowRoot, toShowTreeLines];
    TreeOptions.SelectionOptions := TreeOptions.SelectionOptions - [toextendedfocus];
    TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toToggleondblclick];

  end
  // as grid
  else
  begin
    TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowRoot, toShowTreeLines] + [toHotTrack , tohidefocusrect, toshowhorzgridlines, toshowvertgridlines] ;
    TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toFullRowSelect ];
    Header.Options := Header.Options - [hocolumnresize];
    TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toGridExtensions];
  end;
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

procedure TCryptoNetworkTreeView.SetOptions(const Value: boolean);
begin

end;

procedure TCryptoNetworkTreeView.upddate;
begin

end;

end.
