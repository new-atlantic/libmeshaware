with Ada.Text_IO;
with MeshAware.BATMAN_Advanced;

procedure kmod_version is
begin
   Ada.Text_IO.Put(MeshAware.BATMAN_Advanced.Kernel_Module_Version);
exception
   when others =>
      Ada.Text_IO.Put("NONE");
end kmod_version;
