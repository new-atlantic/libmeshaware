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

with MeshAware.Linux;      use MeshAware.Linux;
with MeshAware.Exceptions; use MeshAware.Exceptions;

package body MeshAware.BATMAN_Advanced.Bat_Interfaces is

   ----------------------------------------------------------------------------

   function Get_Interface (Name : String) return Bat_Interface is
      The_Interface : Bat_Interface (Name_Length => Name'Length);
   begin
      if not The_Interface.Available then
         raise Network_Interface_Not_Available;
      else
         --  TODO: validate that it is a bat_interface!!!!
         The_Interface.IF_Name := Name;
         return The_Interface;
      end if;
   end Get_Interface;

   ----------------------------------------------------------------------------

   function Available (Interface_Object : Bat_Interface)
                      return Boolean is
      Filename       : constant String := "/sys/devices/virtual/net/"
        & Interface_Object.Name;
   begin
      if Ada.Directories.Exists (Name => Filename) then
         return True;
      elsif Kernel_Module_Loaded then
         return False;
      else
         raise BATMAN_Adv_Module_Not_Loaded;
      end if;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         --  Cause: Ada.Text_IO.Open
         raise ProcFS_File_Not_Found;
   end Available;

   ----------------------------------------------------------------------------

   function Up (Interface_Object : Bat_Interface)
               return Boolean is
      --  Check operstate and carrier to find out the network interface state
      --  TODO: Check that the physical interface is up too!
      Operstate_File_Name : constant String := "/sys/devices/virtual/net/"
        & Interface_Object.Name & "/operstate";
      Carrier_File_Name   : constant String := "/sys/devices/virtual/net/"
        & Interface_Object.Name & "/carrier";
      Operstate_File      : Ada.Text_IO.File_Type;
      Carrier_File        : Ada.Text_IO.File_Type;
   begin
      if not Kernel_Module_Loaded then
         raise BATMAN_Adv_Module_Not_Loaded;
      end if;

      if not Available (Interface_Object => Interface_Object) then
         raise Network_Interface_Not_Available;
      end if;

      Ada.Text_IO.Open (File => Operstate_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => Operstate_File_Name);

      declare
         Line : constant String := Ada.Text_IO.Get_Line (Operstate_File);
      begin
         Ada.Text_IO.Close (File => Operstate_File);
         if Line = "down" then
            return False;
         elsif Line = "up" then
            null;
         elsif Line = "unknown" then
            null;
         else
            return False;
         end if;

      end;

      Ada.Text_IO.Open (File => Carrier_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => Carrier_File_Name);

      declare
         Line : constant String := Ada.Text_IO.Get_Line (Carrier_File);
      begin
         Ada.Text_IO.Close (File => Carrier_File);
         if Line = "1" then
            return True;
         else
            return False;
         end if;
      end;

   exception
      when Ada.IO_Exceptions.Device_Error =>
         return False;
   end Up;

   ----------------------------------------------------------------------------

   function Name (Interface_Object : in Bat_Interface) return String is
   begin
      return Interface_Object.IF_Name;
   end Name;

   ----------------------------------------------------------------------------

   function HW_Address (Interface_Object : in Bat_Interface)
                        return String is
      MAC_Address_File_Name : constant String := "/sys/devices/virtual/net/"
        & Interface_Object.Name & "/address";
      MAC_Address_File      : Ada.Text_IO.File_Type;
   begin
      if not Kernel_Module_Loaded then
         raise BATMAN_Adv_Module_Not_Loaded;
      elsif not Interface_Object.Available then
         raise Network_Interface_Not_Available;
      end if;

      Ada.Text_IO.Open (File => MAC_Address_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => MAC_Address_File_Name);

      declare
         Line : constant String := Ada.Text_IO.Get_Line (MAC_Address_File);
      begin
         Ada.Text_IO.Close (File => MAC_Address_File);
         return Line;
      end;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         --  TODO: What if anything could cause this situtation?
         raise Network_Interface_Error;
   end HW_Address;

   ----------------------------------------------------------------------------

end MeshAware.BATMAN_Advanced.Bat_Interfaces;
