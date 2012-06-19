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

package body MeshAware.BATMAN_Advanced.Bat_Nodes is

   ----------------------------------------------------------------------------

   function Address (Node_Object : in Bat_Node;
                     Address_Type : in Network_Address_Type)
                    return Network_Address'Class is
   begin
      case Address_Type is
         when MAC =>
            return Node_Object.Bat_Address;
         when IPv4 =>
            --  TODO: replace with:
            --        return MAC_To_IPv4 (Node_Object.Bat_Address)
            return Node_Object.Bat_Address;
         when IPv6 =>
            --  TODO: replace with:
            --        return MAC_To_IPv6 (Node_Object.Bat_Address)
            return Node_Object.Bat_Address;
      end case;
   end Address;

   ----------------------------------------------------------------------------

end MeshAware.BATMAN_Advanced.Bat_Nodes;
