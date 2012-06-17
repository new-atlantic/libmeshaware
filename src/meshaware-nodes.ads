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
use MeshAware.Addresses;
--  with MeshAware.Networks;
--  use MeshAware.Networks;

package MeshAware.Nodes is
   type Hop_Count is new Natural;
   type Node_Count is new Natural;
   type Meter is digits 4;
   type Bytes_Per_Second is digits 4;

   type Node is interface;

   type Node_Array is array (Natural range <>) of access Node'Class;

   function Address (Node_Object : in Node;
                     Address_Type : in Network_Address_Type)
                    return Network_Address'Class is abstract;

   --  function Is_Self (Node_Object : in Node) return Boolean is abstract;

   --  function Subnet_Mask (Node_Object : in Node) return ??? is abstract;

   --  function Network_Context (Node_Object : in Node)
   --                           return Network is abstract;

   --  function Network_Hostname (Node_Object : in Node)
   --                            return Hostname is abstract;

   --  function Number_Of_Neighbours (Node_Object : in Node)
   --                              return Node_Count is abstract;

   --  function Neighbours (Node_Object : in Node)
   --                      return Node_Array is abstract;

   --  function Next_Hops (Node_Object : in Node)
   --                     return Node_Array is abstract;

   --  function Number_of_Nth_Hops (Node_Object : in Node;
   --                               Hop : in Hop_Count)
   --                              return Node_Count is abstract;
   --  function Nth_Hops (Node_Object : in Node; Hop : in Hop_Count)
   --                    return Node_Array is abstract;

   --  function Is_Neighbour_To_Self (Node_Object : in Node)
   --                                return Boolean is abstract;

   --  function Is_Neighbour_To_Node (Node_Object : in Node;
   --                                 Target_Node : in Node)
   --                                return Boolean is abstract;

   --  function Is_MeshAware (Node_Object : in Node)
   --                        return Boolean is abstract;

   --  function Is_Gateway (Node_Object : in Node) return Boolean is abstract;

   --  function Is_Connected_To_Self (Node_Object : in Node)
   --                                return Boolean is abstract;

   --  function Is_Connected_To_Node (Node_Object : in Node;
   --                                 Target_Node : in Node)
   --                                return Boolean is abstract;

   --  function Last_Seen_By_Self (Node_Object : in Node)
   --                             return Duration is abstract;

   --  function Last_Seen_By_Node (Node_Object : in Node;
   --                              Target_Node : in Node)
   --                             return Duration is abstract;

   --  function Hops_To_Self (Node_Object : in Node)
   --                        return Hop_Count is abstract;

   --  function Hops_To_Node (Node_Object : in Node; Target_Node : in Node)
   --                        return Hop_Count is abstract;

   --  function Distance_To_Self (Node_Object : in Node)
   --                            return Meter is abstract;

   --  function Distance_To_Node (Node_Object : in Node; Target_Node : in Node)
   --                            return Meter is abstract;

   --  function Time_To_Self (Node_Object : in Node)
   --                        return Duration is abstract;

   --  function Time_To_Node (Node_Object : in Node; Target_Node : in Node)
   --                        return Duration is abstract;

   --  function Average_Latency_To_Self (Node_Object : in Node)
   --                                   return Duration is abstract;

   --  function Average_Latency_To_Node (Node_Object : in Node;
   --                                    Target_Node : in Node)
   --                                   return Duration is abstract;

   --  function Average_Bandwith_To_Self (Node_Object : in Node)
   --                                    return Bytes_Per_Second is abstract;

   --  function Average_Bandwith_To_Self (Node_Object : in Node;
   --                                     Target_Node : in Node)
   --                                    return Bytes_Per_Second is abstract;

   -- Interfaces and interface types --
   -- Sockets and other communication --
   -- Abstracted HTML requests? --
   -- Service Discovery --
end MeshAware.Nodes;
