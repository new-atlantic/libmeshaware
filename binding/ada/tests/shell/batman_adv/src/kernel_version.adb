with Ada.Text_IO;
with MeshAware.Linux;

procedure kernel_version is
begin
   Ada.Text_IO.Put(MeshAware.Linux.Kernel_Version);
exception
   when others =>
      Ada.Text_IO.Put("NONE");
end kernel_version;
