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
with MeshAware.Linux;      use MeshAware.Linux;

package body MeshAware.BATMAN_Advanced is

   ----------------------------------------------------------------------------

   function Kernel_Module_Available return Boolean is
      Version_File_Name : constant String := "/lib/modules/" & Kernel_Version
        & "/kernel/net/batman-adv/batman-adv.ko";
   begin
      if Ada.Directories.Exists (Name => Version_File_Name) then
         return True;
      else
         return False;
      end if;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         --  Cause: Ada.Directories.Exists
         raise Linux_Error;
         --  TODO: Name_Error
         --  AARM: "The exception Name_Error is propagated if the string given
         --  as Name does not allow the identification of an external file
         --  (including directories and special files)."
         --
         --  Test if this occurs e.g. if using UNIX pathnames on Windows.
         --  Rename exception to reflect the exact cause of name error.
   end Kernel_Module_Available;

   ----------------------------------------------------------------------------

   function Kernel_Module_Loaded return Boolean is
      Modules_File_Name : constant String        := "/proc/modules";
      Modules_File      : Ada.Text_IO.File_Type;
      Module_Name       : constant String        := "batman_adv";
   begin
      if not Kernel_Module_Available then
         raise Kernel_Module_Not_Available;
      end if;

      Ada.Text_IO.Open (File => Modules_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => Modules_File_Name);

      while not Ada.Text_IO.End_Of_File (Modules_File) loop
         declare
            Line : constant String := Ada.Text_IO.Get_Line (Modules_File);
         begin
            if Line (Line'First .. Line'First + Module_Name'Length - 1)
              = Module_Name then
               Ada.Text_IO.Close (File => Modules_File);
               return True;
            end if;
         end;
      end loop;

      Ada.Text_IO.Close (File => Modules_File);
      return False;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         --  Cause: Ada.Text_IO.Open
         raise ProcFS_File_Not_Found;
   end Kernel_Module_Loaded;

   ----------------------------------------------------------------------------

   function Kernel_Module_Version return String is
      Version_File_Name  : constant String := "/sys/module/batman_adv/version";
      Version_File       : Ada.Text_IO.File_Type;
   begin
      if not Kernel_Module_Loaded then
         raise BATMAN_Adv_Module_Not_Loaded;
      end if;

      Ada.Text_IO.Open (File => Version_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => Version_File_Name);

      declare
         Line : constant String := Ada.Text_IO.Get_Line (Version_File);
      begin
         Ada.Text_IO.Close (File => Version_File);
         return Line;
      end;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         --  Cause: Ada.Text_IO.Open
            raise SysFS_File_Not_Found;
   end Kernel_Module_Version;

   ----------------------------------------------------------------------------

end MeshAware.BATMAN_Advanced;
