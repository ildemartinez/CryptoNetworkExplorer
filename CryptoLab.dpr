program CryptoLab;

uses
  Vcl.Forms,
  CryptoLabMainFormUnit in 'CryptoLabMainFormUnit.pas' {Form1},
  ISubjectUnit in 'ISubjectUnit.pas',
  CryptoNetworkTreeViewUnit in 'CryptoNetworkTreeViewUnit.pas',
  CryptoNetworkPopupMenuUnit in 'CryptoNetworkPopupMenuUnit.pas',
  NodeFormUnit in 'NodeFormUnit.pas' {NodeForm},
  NodeObserverPattern in 'NodeObserverPattern.pas',
  IPeerNodeUnit in 'IPeerNodeUnit.pas',
  PeerNodeUnit in 'PeerNodeUnit.pas',
  NetworkFormUnit in 'NetworkFormUnit.pas' {NetworkForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
