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

package MeshAware.Addresses is
   type Network_Address is interface;

   --  function Get_Address_String (Address_Object : in Network_Address)
   --                              return String is abstract;

   type Hostname is private;

   type Network_Address_Type is (IPv4, IPv6, MAC);

   type MAC_Address_Type is (Ethernet,
                             Wireless_802_11,
                             Bluetooth);

   type MAC_Address is new Network_Address with record
      Address_Type : MAC_Address_Type;
      Address : String (1 .. 12);  --  TODO: don't store as string
   end record;

   --  type IPv4_Address is new Network_Address with record
   --  type IPv6_Address is new Network_Address with record

private
   type Hostname is null record;
end MeshAware.Addresses;
