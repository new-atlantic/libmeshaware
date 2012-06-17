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
with Ada.Strings.Fixed;
with Ada.Text_IO;

with MeshAware.Exceptions;
use MeshAware.Exceptions;

package body MeshAware.BATMAN_Advanced is

   ----------------------------------------------------------------------------
   ----------------
   --  Bat_Mesh  --
   ----------------

   function Get_Mesh return Bat_Mesh is
      The_Mesh : Bat_Mesh;
   begin
      return The_Mesh;
   end Get_Mesh;

   ----------------------------------------------------------------------------

   function Available (Mesh_Object : in Bat_Mesh) return Boolean is
   begin
      if Kernel_Module_Available
         and Kernel_Module_Loaded
         and Mesh_Object.Bat_Interface_Available
         and Mesh_Object.Bat_Interface_Up then
         return True;
      else
         return False;
      end if;
   exception
      when BATMAN_Adv_Module_Error | Linux_Error | Network_Interface_Error =>
         return False;
   end Available;

   ----------------------------------------------------------------------------

   function Bat_Interface_Available (Bat_Mesh_Object : Bat_Mesh)
                                    return Boolean is
      --  Tests whether the interface directory exists.
      Interface_Path : constant String := "/sys/devices/virtual/net/"
                                        & Bat_Mesh_Object.Default_Interface;
      Availability   : Boolean;
   begin
      if Ada.Directories.Exists (Name => Interface_Path) then
         Availability := True;
      elsif Kernel_Module_Loaded then
         Availability := False;
      else
         raise BATMAN_Adv_Module_Error;
      end if;

      return Availability;

   exception
      when BATMAN_Adv_Module_Error =>
         raise BATMAN_Adv_Module_Error;
      when Linux_Error =>
         raise Linux_Error;
   end Bat_Interface_Available;

   ----------------------------------------------------------------------------

   function Bat_Interface_Up (Bat_Mesh_Object : Bat_Mesh)
                             return Boolean is
      --  Check operstate and carrier to find out the network interface state
      --  TODO: check that tests are correct for all types of network
      --        interfaces (wifi, ethernet, ?bluetooth?, ppp)
      Operstate_Filename : constant String := "/sys/devices/virtual/net/"
        & Bat_Mesh_Object.Default_Interface & "/operstate";
      Carrier_Filename : constant String := "/sys/devices/virtual/net/"
        & Bat_Mesh_Object.Default_Interface & "/carrier";
      Operstate_File     : Ada.Text_IO.File_Type;
      Carrier_File     : Ada.Text_IO.File_Type;
      State              : Boolean;
   begin
      if not Kernel_Module_Loaded then
         raise BATMAN_Adv_Module_Error;
      end if;

      if not Bat_Interface_Available (Bat_Mesh_Object => Bat_Mesh_Object) then
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
         else
            State := False;
         end if;
      end;

      return State;

   exception
      when Ada.IO_Exceptions.Device_Error =>
         --  This exception should never occur as the function returns if
         --  operstate is "down" and so we never try to open carrier if the
         --  interface is down.
         return False;
   end Bat_Interface_Up;

   ----------------------------------------------------------------------------


   function Number_Of_Nodes (Mesh_Object : in Bat_Mesh)
                            return Node_Count is
      Filename        : constant String := Debug_FS_Path & "/batman_adv/"
        & Mesh_Object.Default_Interface & "/originators";
      File : Ada.Text_IO.File_Type;
      N_Nodes : Node_Count             := 0;
   begin
      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => Filename);
      while not Ada.Text_IO.End_Of_File (File) loop
         N_Nodes := N_Nodes + 1;
         declare
            Line : constant String := Ada.Text_IO.Get_Line (File);
         begin
            if Line = "No batman nodes in range ..." then
               Ada.Text_IO.Close (File => File);
               return 0;
            end if;
         end;
      end loop;

      N_Nodes := N_Nodes - 2;
      Ada.Text_IO.Close (File => File);
      return N_Nodes;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         if not Kernel_Module_Loaded then
            raise BATMAN_Adv_Module_Error;
         elsif not Bat_Interface_Available (Mesh_Object) then
            raise Network_Interface_Error;
         else
            --  TODO: Is this correct? When would 'else' occur?
            raise Linux_Error;
         end if;
      when Debug_FS_Error =>
         raise Debug_FS_Error;
   end Number_Of_Nodes;

   ----------------------------------------------------------------------------
   ----------------
   --  Bat_Node  --
   ----------------

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
   ---------------------------
   --  Auxiliary functions  --
   ---------------------------

   function Kernel_Version return String is
      Filename : constant String := "/proc/version";
      File     : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (File => File,
                 Mode => Ada.Text_IO.In_File,
                 Name => Filename);
      declare
         Start_Position : Natural;
         End_Position   : Natural;
         Prefix         : constant String := "Linux version";
         Line           : constant String := Ada.Text_IO.Get_Line (File);
      begin
         Start_Position := Line'First + Prefix'Length + 1;
         End_Position   := Ada.Strings.Fixed.Index (Source => Line,
                                                    Pattern => " ",
                                                    From => Start_Position
                                                   ) - 1;
         Ada.Text_IO.Close (File => File);
         return Line (Start_Position .. End_Position);
      end;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         raise Linux_Error;
   end Kernel_Version;

   ----------------------------------------------------------------------------

   function Kernel_Module_Available return Boolean is
      Filename     : constant String := "/lib/modules/" & Kernel_Version
        & "/kernel/net/batman-adv/batman-adv.ko";
      Availability : Boolean         := False;
   begin
      if Ada.Directories.Exists (Name => Filename) then
         Availability := True;
      end if;

      return Availability;

   exception
      when Linux_Error =>
         raise Linux_Error;
   end Kernel_Module_Available;

   ----------------------------------------------------------------------------

   function Kernel_Module_Loaded return Boolean is
      --  Tests that batman_adv is listed in /proc/modules.
      Filename : constant String := "/proc/modules";
      File : Ada.Text_IO.File_Type;
      Module_Name : constant String := "batman_adv";
      Module_Availability  : Boolean := False;
   begin
      Ada.Text_IO.Open (File => File,
                 Mode => Ada.Text_IO.In_File,
                 Name => Filename);

      while not Ada.Text_IO.End_Of_File (File) loop
         declare
            Line : constant String := Ada.Text_IO.Get_Line (File);
         begin
            if Line (Line'First .. Line'First + Module_Name'Length - 1)
              = Module_Name then
               Module_Availability := True;
            end if;
         end;
      end loop;

      Ada.Text_IO.Close (File => File);
      return Module_Availability;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         raise Linux_Error;
   end Kernel_Module_Loaded;

   ----------------------------------------------------------------------------

   function Kernel_Module_Version return String is
      Filename  : constant String := "/sys/module/batman_adv/version";
      File : Ada.Text_IO.File_Type;
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
         if not Kernel_Module_Loaded then
            raise BATMAN_Adv_Module_Error;
         else
            raise Linux_Error;
         end if;
   end Kernel_Module_Version;

   ----------------------------------------------------------------------------

   function Debug_FS_Mounted return Boolean is
      --  Tests that debugfs is listed in /proc/mounts.
      Filename                 : constant String := "/proc/mounts";
      File                     : Ada.Text_IO.File_Type;
      Filesystem_Name          : constant String := "debugfs";
      Filesystem_Availability  : Boolean         := False;
   begin
      Ada.Text_IO.Open (File => File,
                 Mode => Ada.Text_IO.In_File,
                 Name => Filename);

      while not Ada.Text_IO.End_Of_File (File) loop
         declare
            Line : constant String := Ada.Text_IO.Get_Line (File);
         begin
            if  Ada.Strings.Fixed.Index
              (Source => Line, Pattern => Filesystem_Name) > 0 then
               Filesystem_Availability := True;
            end if;
         end;
      end loop;

      Ada.Text_IO.Close (File => File);
      return Filesystem_Availability;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         raise Linux_Error;
   end Debug_FS_Mounted;

   ----------------------------------------------------------------------------

   function Debug_FS_Path return String is
      --  Gets the debugfs mount point from /proc/mounts.
      Filename                 : constant String := "/proc/mounts";
      File                     : Ada.Text_IO.File_Type;
      Filesystem_Name          : constant String := "debugfs";
   begin

      if not Debug_FS_Mounted then
         raise Debug_FS_Error;
      end if;

      Ada.Text_IO.Open (File => File,
                 Mode => Ada.Text_IO.In_File,
                 Name => Filename);

      while not Ada.Text_IO.End_Of_File (File) loop
         declare
            Start_Position : Natural;
            End_Position : Natural;
            Line : constant String := Ada.Text_IO.Get_Line (File);
         begin
            --  In the line containing debugfs search the position of the
            --  whitespace preceding and succeeding the path.
            if  Ada.Strings.Fixed.Index
              (Source => Line, Pattern => Filesystem_Name) > 0 then
               Start_Position := Ada.Strings.Fixed.Index (Source => Line,
                                                          Pattern => " ");
               End_Position := Ada.Strings.Fixed.Index (Source => Line,
                                                        Pattern => " ",
                                                        From => Start_Position
                                                          + 1);
               Ada.Text_IO.Close (File => File);
               return Line (Start_Position + 1 .. End_Position - 1);
            end if;
         end;
      end loop;

      Ada.Text_IO.Close (File => File);
      raise Debug_FS_Error;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         raise Linux_Error;
   end Debug_FS_Path;

   ----------------------------------------------------------------------------

end MeshAware.BATMAN_Advanced;
