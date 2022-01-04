unit NodeFormUnit;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  BTCPeerNodeUnit,
  NodeObserverPattern,
  Vcl.StdCtrls,
  PeerNodeUnit,
  IPeerNodeUnit;

type
  TNodeForm = class(TForm, INodeObserver)
    Label1: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    fPeerNode: TPeerNode;

    procedure SetNode(const Value: TPeerNode);
    { Private declarations }

    procedure DoNotify(const msgtype: TMSGType; const aNode: INode);
  public
    { Public declarations }
    destructor Destroy; override;

    property Node: TPeerNode write SetNode;
  end;

implementation

{$R *.dfm}
{ TNodeForm }

destructor TNodeForm.Destroy;
begin

  // nos unregistramos
  DeattachObserverFromSubject(self, fPeerNode);
  inherited;
end;

procedure TNodeForm.DoNotify(const msgtype: TMSGType; const aNode: INode);
begin
  caption := 'connected' + aNode.GetIP;
  Label1.Caption := aNode.getagent;
end;

procedure TNodeForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TNodeForm.SetNode(const Value: TPeerNode);
begin
  if fPeerNode <> Value then
  begin
    if fPeerNode <> nil then
      DeattachObserverFromSubject(self, fPeerNode);
  end;

  fPeerNode := Value;

  if Value <> nil then
  begin

    AttachObserverToSubject(self, Value);
    caption := Value.PeerIp;
    Label1.Caption := value.agent;
  end;

end;

end.
