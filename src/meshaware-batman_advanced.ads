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

with MeshAware.Addresses;
use  MeshAware.Addresses;
with MeshAware.Interfaces;
use MeshAware.Interfaces;
with MeshAware.Meshes;
use MeshAware.Meshes;
with MeshAware.Nodes;
use MeshAware.Nodes;

package MeshAware.BATMAN_Advanced is

   ---------------------
   --  Bat_Interface  --
   ---------------------

   type Bat_Interface is new Network_Interface with record
      IF_Name : String (1 .. 4) := "bat0";
      --  Physical_Interface : Network_Interface'Class;
   end record;

   overriding function Available (Interface_Object : in Bat_Interface)
                                 return Boolean;

   overriding function Up (Interface_Object : in Bat_Interface) return Boolean;

   overriding function Name (Interface_Object : in Bat_Interface)
                            return String;
   ----------------
   --  Bat_Mesh  --
   ----------------

   type Bat_Mesh is new Mesh with record
      Default_Interface : Bat_Interface;
   end record;

   overriding function Get_Mesh return Bat_Mesh;

   overriding function Available (Mesh_Object : in Bat_Mesh) return Boolean;

   overriding function Number_Of_Nodes (Mesh_Object : in Bat_Mesh)
                                       return Node_Count;

   --  procedure Set_Default_Interface;

   ----------------
   --  Bat_Node  --
   ----------------

   type Bat_Node is new Node with record
      Bat_Address : MAC_Address;
      Mesh_Object : Bat_Mesh;
   end record;

   overriding function Address (Node_Object : in Bat_Node;
                                Address_Type : in Network_Address_Type)
                               return Network_Address'Class;

   ---------------------------
   --  Auxiliary functions  --
   ---------------------------

   function Kernel_Version return String;

   function Kernel_Module_Available return Boolean;

   function Kernel_Module_Loaded return Boolean;

   function Kernel_Module_Version return String;

   function Debug_FS_Mounted return Boolean;

   function Debug_FS_Path return String;

end MeshAware.BATMAN_Advanced;

