unit Ut_Resources;

interface

function FindNameBitmapRes(const BitmapName : string): THandle;

implementation

uses
  windows, classes, //dccommon,
  dialogs; //, mlcustomtreeviewsource;

function FindNameBitmapRes(const BitmapName : string): THandle;
var
  PackList: TList;
  i: Integer;
begin
  if BitmapName = '' then
    exit;
{  PackList := TList.Create;
  ResModulesToList(PackList);
  try
    for i := 0 to PackList.Count - 1 do
    begin
      result := Integer(PackList[i]);
      if (result > 0) and (FindResource(result, PChar(BitmapName), RT_BITMAP) <> 0) then
        exit;
    end;
    result := 0;
  finally
    PackList.Free;
  end;       }
end;

end.
 