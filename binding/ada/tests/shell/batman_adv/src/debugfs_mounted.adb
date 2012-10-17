with Ada.Text_IO;
with MeshAware.Linux;

procedure debugfs_mounted is
begin
   Ada.Text_IO.Put(Boolean'Image (MeshAware.Linux.Debug_Filesystem_Mounted));
exception
   when others =>
      Ada.Text_IO.Put(Boolean'Image(False));
end debugfs_mounted;
