unit IPeerNodeUnit;

interface

type
  INode = interface
    ['{90648BB9-6CEB-46A7-AD53-12775A7577DF}']
    // just to test server connectivity
    function ServerConnected: boolean;
    // test protocol conected
    function Connected: boolean;
    // Get the server node IP
    function GetIP: string;

    procedure GetPeers();

    function GetAgent : string;
  end;

  IBTCPeerNode = interface(INode)
    ['{6F43154D-FCC0-4021-96E0-3B5AB9D3142B}']
  end;

implementation

end.
