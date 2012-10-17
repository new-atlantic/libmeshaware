with Ada.Command_Line;
with Ada.Text_IO;
with MeshAware.BATMAN_Advanced.Bat_Interfaces;
with MeshAware.Exceptions;

procedure batif_name is
   Interface_Name : String := Ada.Command_Line.Argument (1);
   My_IF : MeshAware.BATMAN_Advanced.Bat_Interfaces.Bat_Interface := (Name_Length => Interface_Name'Length, IF_Name => Interface_Name);
begin
   if My_IF.Available then
      Ada.Text_IO.Put(My_IF.Name);
   else
      raise MeshAware.Exceptions.Network_Interface_Error;
   end if;
exception
   when others =>
      Ada.Text_IO.Put("NONE");
end batif_name;
