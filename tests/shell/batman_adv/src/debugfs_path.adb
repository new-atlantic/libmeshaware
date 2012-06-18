with Ada.Text_IO;
with MeshAware.BATMAN_Advanced;

procedure debugfs_path is
begin
   Ada.Text_IO.Put(MeshAware.BATMAN_Advanced.Debug_FS_Path);
exception
   when others =>
      Ada.Text_IO.Put("NONE");
end debugfs_path;
