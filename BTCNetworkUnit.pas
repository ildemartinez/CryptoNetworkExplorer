unit BTCNetworkUnit;

interface

uses
  classes;

type
  TBTCNetwork = class(TComponent)
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CryptoCurrency', [TBTCNetwork]);
end;

end.
