unit IPeerNodeUnit;

interface

type
  INode = interface
    ['{90648BB9-6CEB-46A7-AD53-12775A7577DF}']
    function Connected: boolean;
    function GetIP: string;
  end;

  IBTCPeerNode = interface(INode)
    ['{6F43154D-FCC0-4021-96E0-3B5AB9D3142B}']
  end;

implementation

end.
