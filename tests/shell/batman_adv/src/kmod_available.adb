with Ada.Text_IO;
with MeshAware.BATMAN_Advanced;

procedure kmod_available is
begin
   Ada.Text_IO.Put(Boolean'Image(MeshAware.BATMAN_Advanced.Kernel_Module_Available));
exception
   when others =>
      Ada.Text_IO.Put(Boolean'Image(False));
end kmod_available;
