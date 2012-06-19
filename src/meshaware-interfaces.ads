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

package MeshAware.Interfaces is

   -------------------------
   --  Network_Interface  --
   -------------------------

   type Network_Interface is interface;

   function Get_Interface (Name : String) return Network_Interface is abstract;

   function Available (Interface_Object : in Network_Interface)
                      return Boolean is abstract;

   function Up (Interface_Object : in Network_Interface)
               return Boolean is abstract;

   function Name (Interface_Object : in Network_Interface)
                 return String is abstract;

   --  TODO: return MAC_Address from MA.Addresses not string
   function HW_Address (Interface_Object : in Network_Interface)
                        return String is abstract;


   type Physical_Interface (Name_Length : Positive) is
     abstract new Network_Interface with record
        IF_Name : String (1 .. Name_Length);
   end record;

   type Physical_Interface_Access is access Physical_Interface;

end MeshAware.Interfaces;
