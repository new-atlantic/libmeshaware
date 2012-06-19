--  Copyright 2012 Torsti Schulz
--
--  This file is part of the meshaware library for Ada.
--
--  meshaware is free software: you can redistribute it and/or modify
--  it under the terms of the GNU Lesser General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  meshaware is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Lesser General Public License for more details.
--
--  You should have received a copy of the GNU Lesser General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ada.IO_Exceptions;
with Ada.Text_IO;

with MeshAware.Exceptions; use MeshAware.Exceptions;
with MeshAware.Linux;      use MeshAware.Linux;

package body MeshAware.BATMAN_Advanced.Bat_Meshes is

   ----------------------------------------------------------------------------

   function Get_Bat_Mesh (Interface_Object : in Bat_Interface)
                         return Bat_Mesh is
      The_Mesh : Bat_Mesh := (IF_Name_Length => Interface_Object.Name'Length,
                              Mesh_Interface => Interface_Object);
   begin
      return The_Mesh;
   end Get_Bat_Mesh;

   ----------------------------------------------------------------------------

   function Available (Mesh_Object : in Bat_Mesh) return Boolean is
   begin
      if Kernel_Module_Available
        and Kernel_Module_Loaded
        and Mesh_Object.Mesh_Interface.Available
        and Mesh_Object.Mesh_Interface.Up then
         return True;
      else
         return False;
      end if;
   exception
      when others =>
         return False;
   end Available;

   ----------------------------------------------------------------------------

   function Number_Of_Nodes (Mesh_Object : in Bat_Mesh)
                            return Node_Count is
      Originators_File_Name : constant String := Debug_Filesystem_Path
        & "/batman_adv/" & Mesh_Object.Mesh_Interface.Name & "/originators";
      Originators_File      : Ada.Text_IO.File_Type;
      N_Nodes               : Node_Count      := 0;
   begin
      if not Kernel_Module_Loaded then
         raise BATMAN_Adv_Module_Not_Loaded;
      elsif not Mesh_Object.Mesh_Interface.Available then
         raise Network_Interface_Not_Available;
      end if;

      Ada.Text_IO.Open (File => Originators_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => Originators_File_Name);
      while not Ada.Text_IO.End_Of_File (Originators_File) loop
         N_Nodes := N_Nodes + 1;
         declare
            Line : constant String := Ada.Text_IO.Get_Line (Originators_File);
         begin
            if Line = "No batman nodes in range ..." then
               Ada.Text_IO.Close (File => Originators_File);
               return 0;
            end if;
         end;
      end loop;

      N_Nodes := N_Nodes - 2;
      Ada.Text_IO.Close (File => Originators_File);
      return N_Nodes;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         --  TODO: Is this correct? When would this occur?
         raise Linux_Error;
   end Number_Of_Nodes;

   ----------------------------------------------------------------------------

end MeshAware.BATMAN_Advanced.Bat_Meshes;
