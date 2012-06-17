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

--  with MeshAware.Addresses;
--  use MeshAware.Addresses;
with MeshAware.Nodes;
use MeshAware.Nodes;

package MeshAware.Meshes is

   type Mesh_Protocol_State is (Unavailable,
                                Disabled,
                                Enabled,
                                Unknown);

   type Mesh_State is (Unavailable,
                       Offline,
                       Online,
                       Connected,
                       Unknown);

   type Mesh is limited interface;

   function Get_Mesh return Mesh is abstract;

   function Available (Mesh_Object : in Mesh) return Boolean is abstract;

   function Number_Of_Nodes (Mesh_Object : in Mesh)
                            return Node_Count is abstract;

   --  function Number_Of_Nodes (Mesh_Object : in Mesh)
   --                           return Node_Count is abstract;

   --  function Number_Of_Next_Hops (Mesh_Object : in Mesh)
   --                               return Node_Count is abstract;

   --  function Get_Own_Node return Node is abstract;

   --  function Get_Node_By_Address (Mesh_Object : in Mesh;
   --                                Address : Network_Address'Class)
   --                               return Node is abstract;

   --  function Get_Node_By_Address (Mesh_Object : in Mesh;
   --                                Address_Type : in Network_Address_Type;
   --                                Address : in String)
   --                               return Node is abstract;

   --  procedure Start_Gathering_Statistics (Mesh_Object : in Mesh)
   --                                       is abstract;

   --  procedure Stop_Gathering_Statistics (Mesh_Object : in Mesh) is abstract;
end MeshAware.Meshes;
