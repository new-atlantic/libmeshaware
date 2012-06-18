with Ada.Text_IO;
with MeshAware.BATMAN_Advanced;

procedure debugfs_mounted is
begin
   Ada.Text_IO.Put(Boolean'Image (MeshAware.BATMAN_Advanced.Debug_FS_Mounted));
exception
   when others =>
      Ada.Text_IO.Put(Boolean'Image(False));
end debugfs_mounted;
