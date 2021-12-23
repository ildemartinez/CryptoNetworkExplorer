unit NodeFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BTCPeerNodeUnit, NodeObserverPattern,
  Vcl.StdCtrls;

type
  TNodeForm = class(TForm, INodeObserver)
    Label1: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    fPeerNode: TBTCPeerNode;

    procedure SetNode(const Value: TBTCPeerNode);
    { Private declarations }

    procedure NodeConnected(aNode: string);
  public
    { Public declarations }
    destructor Destroy; override;

    property Node: TBTCPeerNode write SetNode;
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

procedure TNodeForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TNodeForm.NodeConnected(aNode: string);
begin
  caption := 'connected' + aNode;

end;

procedure TNodeForm.SetNode(const Value: TBTCPeerNode);
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
  end;

end;

end.