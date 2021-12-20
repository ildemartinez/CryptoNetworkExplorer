program CryptoLab;

uses
  Vcl.Forms,
  CryptoLabMainFormUnit in 'btc\CryptoLabMainFormUnit.pas' {Form1},
  BTCPeerUnit in 'btc\BTCPeerUnit.pas',
  BTCMonitorUnit in 'btc\BTCMonitorUnit.pas',
  BTCThreadMonitorUnit in 'btc\BTCThreadMonitorUnit.pas',
  BTCTypes in 'btc\BTCTypes.pas',
  BTCAgentUnit in 'BTCAgentUnit.pas',
  BTCPeerDiscoveryUnit in 'BTCPeerDiscoveryUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
