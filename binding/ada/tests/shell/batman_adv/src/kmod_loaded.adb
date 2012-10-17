with Ada.Text_IO;
with MeshAware.BATMAN_Advanced;

procedure kmod_loaded is
begin
   Ada.Text_IO.Put(Boolean'Image(MeshAware.BATMAN_Advanced.Kernel_Module_Loaded));
exception
   when others =>
      Ada.Text_IO.Put(Boolean'Image(False));
end kmod_loaded;
