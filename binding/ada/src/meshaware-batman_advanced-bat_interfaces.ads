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

with MeshAware.Interfaces; use MeshAware.Interfaces;

package MeshAware.BATMAN_Advanced.Bat_Interfaces is

   type Bat_Interface (Name_Length : Positive) is
     new Network_Interface with record
        IF_Name : String (1 .. Name_Length) := "bat0";
   end record;

   --  TODO: one Bat_Interface can be assigned to multiple physical interfaces.
   --        Represent as method returning array of access or similar?

   overriding function Get_Interface (Name : String) return Bat_Interface;

   overriding function Available (Interface_Object : in Bat_Interface)
                                 return Boolean;

   overriding function Up (Interface_Object : in Bat_Interface) return Boolean;

   overriding function Name (Interface_Object : in Bat_Interface)
                            return String;

   overriding function HW_Address (Interface_Object : in Bat_Interface)
                                  return String;

end MeshAware.BATMAN_Advanced.Bat_Interfaces;
