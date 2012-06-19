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

with MeshAware.BATMAN_Advanced.Bat_Interfaces;
with MeshAware.Networks;
with MeshAware.Nodes;

use MeshAware.BATMAN_Advanced.Bat_Interfaces;
use MeshAware.Networks;
use MeshAware.Nodes;

package MeshAware.BATMAN_Advanced.Bat_Meshes is

   type Bat_Mesh (IF_Name_Length : Positive) is new Mesh with record
      Mesh_Interface : Bat_Interface (Name_Length => IF_Name_Length);
   end record;

   not overriding function Get_Bat_Mesh
     (Interface_Object : in Bat_Interface) return Bat_Mesh;

   overriding function Available (Mesh_Object : in Bat_Mesh) return Boolean;

   overriding function Number_Of_Nodes (Mesh_Object : in Bat_Mesh)
                                       return Node_Count;

end MeshAware.BATMAN_Advanced.Bat_Meshes;
