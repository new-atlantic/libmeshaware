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
with MeshAware.BATMAN_Advanced.Bat_Meshes;
with MeshAware.Nodes;

use MeshAware.Addresses;
use  MeshAware.BATMAN_Advanced.Bat_Meshes;
use MeshAware.Nodes;

package MeshAware.BATMAN_Advanced.Bat_Nodes is

   type Bat_Node is new Node with record
      Bat_Address : MAC_Address;
      Mesh_Object : not null access Bat_Mesh;
   end record;

   overriding function Address (Node_Object : in Bat_Node;
                                Address_Type : in Network_Address_Type)
                               return Network_Address'Class;

end MeshAware.BATMAN_Advanced.Bat_Nodes;
