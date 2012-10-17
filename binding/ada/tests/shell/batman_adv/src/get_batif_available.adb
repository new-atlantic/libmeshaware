with Ada.Command_Line;
with Ada.Text_IO;
with MeshAware.BATMAN_Advanced.Bat_Interfaces;
with MeshAware.Exceptions;

procedure get_batif_available is
   Interface_Name : String := Ada.Command_Line.Argument (1);
   My_IF : MeshAware.BATMAN_Advanced.Bat_Interfaces.Bat_Interface := (Name_Length => Interface_Name'Length, IF_Name => Interface_Name);
begin
   Ada.Text_IO.Put(Boolean'Image(My_IF.Available));
exception
   when MeshAware.Exceptions.Network_Interface_Error =>
      Ada.Text_IO.Put(Boolean'Image(False));
end get_batif_available;
