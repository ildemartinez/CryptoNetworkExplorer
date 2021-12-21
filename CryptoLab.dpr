program CryptoLab;

uses
  Vcl.Forms,
  CryptoLabMainFormUnit in 'CryptoLabMainFormUnit.pas' {Form1},
  ISubjectUnit in 'ISubjectUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
