unit Component1;

interface

uses
  System.SysUtils, System.Classes;

type
  TAgentBTC = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CryptoCurrencies', [TAgentBTC]);
end;

end.
