unit CryptoNetworkPopupMenuUnit;

interface

uses
  Vcl.Menus,
  classes;

type
  TCryptoNetworkPopupMenu = class(TPopupMenu)
  strict protected
    procedure DoPopup(Sender: TObject); override;
    procedure MenuItemClick(Sender: TObject);
  end;

implementation

{ TCryptoNetworkPopupMenu }

procedure TCryptoNetworkPopupMenu.DoPopup(Sender: TObject);
begin
  inherited;

end;

procedure TCryptoNetworkPopupMenu.MenuItemClick(Sender: TObject);
begin

end;

end.
