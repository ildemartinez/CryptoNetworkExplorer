unit PeerDiscoveryBTCUnit;

interface

uses
  System.SysUtils, System.Classes;

type
  TPeerDiscoveryBTC = class(TComponent)

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CryptoCurrency', [TPeerDiscoveryBTC]);
end;

end.
