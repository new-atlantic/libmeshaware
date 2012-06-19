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
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Text_IO;
with MeshAware.Exceptions; use MeshAware.Exceptions;

package body MeshAware.Interfaces.Ethernet is

   function Get_Interface (Name : String) return Ethernet_Interface is
      --  TODO: instead of a sensible_default, if no name is given search
      --        for the first available ethernet interface.
      Filename      : constant String := "/sys/class/net" & Name;
      The_Interface : Ethernet_Interface (Name_Length => Name'Length);
   begin
      if Ada.Directories.Exists (Name => Filename) then
         The_Interface.IF_Name := Name;
         --  FIXME: Validate interface type!!!!
         --  raise Interface_Type_Error!!!!
         return The_Interface;
      else
         raise Network_Interface_Error;
      end if;
   end Get_Interface;

   function Available (Interface_Object : in Ethernet_Interface)
                      return Boolean is
      Filename : constant String := "/sys/class/net"
        & Interface_Object.Name;
   begin
      if Ada.Directories.Exists (Name => Filename) then
         return True;
      else
         return False;
      end if;
      --  TODO: Handle exceptions!
   end Available;

   function Up (Interface_Object : in Ethernet_Interface)
               return Boolean is
      --  Check operstate and carrier to find out the network interface state
      Operstate_Filename : constant String := "/sys/class/net/"
        & Interface_Object.Name & "/operstate";
      Carrier_Filename   : constant String := "/sys/class/net/"
        & Interface_Object.Name & "/carrier";
      Operstate_File     : Ada.Text_IO.File_Type;
      Carrier_File       : Ada.Text_IO.File_Type;
      State              : Boolean;
   begin
      if not Interface_Object.Available then
         raise Network_Interface_Error;
      end if;

      Ada.Text_IO.Open (File => Operstate_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => Operstate_Filename);

      declare
         Line : constant String := Ada.Text_IO.Get_Line (Operstate_File);
      begin
         Ada.Text_IO.Close (File => Operstate_File);
         if Line = "down" then
            State := False;
            return State;
         elsif Line = "up" then
            State := True;
         elsif Line = "unknown" then
            State := True;
         else
            State := False;
            return State;
         end if;
      end;

      Ada.Text_IO.Open (File => Carrier_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => Carrier_Filename);

      declare
         Line : constant String := Ada.Text_IO.Get_Line (Carrier_File);
      begin
         Ada.Text_IO.Close (File => Carrier_File);
         if Line = "1" then
            State := True;
         elsif Line = "0" then
            State := False;
         else
            State := False;
         end if;
      end;

      return State;

   exception
      when Ada.IO_Exceptions.Device_Error =>
         return False;
   end Up;

   function Name (Interface_Object : in Ethernet_Interface)
                 return String is
   begin
      return Interface_Object.IF_Name;
   end Name;

   function HW_Address (Interface_Object : in Ethernet_Interface)
                        return String is
      Filename   : constant String := "/sys/class/net/"
        & Interface_Object.Name & "/address";
      File     : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => Filename);

      declare
         Line : constant String := Ada.Text_IO.Get_Line (File);
      begin
         Ada.Text_IO.Close (File => File);
         return Line;
      end;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         if not Interface_Object.Available then
            raise Network_Interface_Error;
         else
            --  TODO: What if anything could cause this situtation?
            raise Network_Interface_Error;
         end if;
   end HW_Address;

end MeshAware.Interfaces.Ethernet;
