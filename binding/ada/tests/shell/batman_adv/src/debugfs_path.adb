with Ada.Text_IO;
with MeshAware.Linux;

procedure debugfs_path is
begin
   Ada.Text_IO.Put(MeshAware.Linux.Debug_Filesystem_Path);
exception
   when others =>
      Ada.Text_IO.Put("NONE");
end debugfs_path;
