unit BTCTypes;

interface

const
  MESSAGE_TYPE_VERSION = 'version';
  MESSAGE_TYPE_VERACK = 'verack';

type
  THeader = record
    start_string: uint32;
    command_name: array [1 .. 12] of ansichar;
    // char is two bytes, ansichar is a byte long...!
    payload_size: uint32;
    checksum: uint32;
  end;

  TVersion = record
    protocol_version: uint32;
    body: array [1 .. 16] of byte;
    body2: array [1 .. 26] of byte;
    body3: array [1 .. 26] of byte;
    body4: array [1 .. 8] of byte;
    body5: array [1 .. 16] of byte;
    body6: array [1 .. 4] of byte;
  end;

function BuildHeader(const command: string): THeader;

implementation

function BuildHeader(const command: string): THeader;
var
  aHeader: THeader;
  k: byte;
begin
  // TODO refector y controlar que length < 13
  aHeader.start_string := $D9B4BEF9;

  // Clear
  for k := 1 to 12 do
    aHeader.command_name[k] := char(0);

  for k := 1 to Length(command) do
    aHeader.command_name[k] := ansichar(command[k]);

  (*
    aHeader.command_name[1] := 'v';
    aHeader.command_name[2] := 'e';
    aHeader.command_name[3] := 'r';
    aHeader.command_name[4] := 's';
    aHeader.command_name[5] := 'i';
    aHeader.command_name[6] := 'o';
    aHeader.command_name[7] := 'n';
    aHeader.command_name[8] := char(0);
    aHeader.command_name[9] := char(0);
    aHeader.command_name[10] := char(0);
    aHeader.command_name[11] := char(0);
    aHeader.command_name[12] := char(0); *)

  aHeader.payload_size := $66;
  aHeader.checksum := $9B3DBA94;
  result := aHeader;
end;

end.
