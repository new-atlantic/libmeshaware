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

package MeshAware.Networks is

   type Context_Type is (Bluetooth,
                         Local_Area_Network,
                         Internet,
                         MANET_Network);

   type Routing_Type is (Managed,
                         Mesh,
                         Bluetooth);

   type Routing_Protocol is (BATMAN_Advanced,
                             olsrd,
                             Babel,
                             Ethernet,
                             Bluetooth);

   type Network is record
      Context : Context_Type;
      Routing : Routing_Type;
      Protocol : Routing_Protocol;
   end record;

end MeshAware.Networks;
