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

with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with MeshAware.Exceptions; use MeshAware.Exceptions;

package body MeshAware.Linux is

   ----------------------------------------------------------------------------

   function Kernel_Version return String is
      Version_File_Name : constant String        := "/proc/version";
      Version_File      : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (File => Version_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => Version_File_Name);

      declare
         Start_Position : Natural;
         End_Position   : Natural;
         Prefix         : constant String := "Linux version ";
         Line           : constant String
           := Ada.Text_IO.Get_Line (Version_File);
      begin
         Start_Position := Line'First + Prefix'Length;
         End_Position   := Ada.Strings.Fixed.Index (Source  => Line,
                                                    Pattern => " ",
                                                    From    => Start_Position
                                                   ) - 1;

         Ada.Text_IO.Close (File => Version_File);
         return Line (Start_Position .. End_Position);
      end;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         --  Cause: Ada.Text_IO.Open
         raise ProcFS_File_Not_Found;
   end Kernel_Version;

   ----------------------------------------------------------------------------

   function Debug_Filesystem_Mounted return Boolean is
      Mounts_File_Name     : constant String        := "/proc/mounts";
      Mounts_File          : Ada.Text_IO.File_Type;
      Filesystem_Type_Name : constant String        := "debugfs";
   begin
      Ada.Text_IO.Open (File => Mounts_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => Mounts_File_Name);

      while not Ada.Text_IO.End_Of_File (Mounts_File) loop
         declare
            Line : constant String := Ada.Text_IO.Get_Line (Mounts_File);
         begin
            if  Ada.Strings.Fixed.Index (Source  => Line,
                                         Pattern => Filesystem_Type_Name) > 0
            then
               Ada.Text_IO.Close (File => Mounts_File);
               return True;
            end if;
         end;
      end loop;

      Ada.Text_IO.Close (File => Mounts_File);
      return False;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         --  Cause: Ada.Text_IO.Open
         raise ProcFS_File_Not_Found;
   end Debug_Filesystem_Mounted;

   ----------------------------------------------------------------------------

   function Debug_Filesystem_Path return String is
      Mounts_File_Name     : constant String        := "/proc/mounts";
      Mounts_File          : Ada.Text_IO.File_Type;
      Filesystem_Type_Name : constant String        := "debugfs";
   begin

      if not Debug_Filesystem_Mounted then
         raise Debug_Filesystem_Not_Mounted;
      end if;

      Ada.Text_IO.Open (File => Mounts_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => Mounts_File_Name);

      while not Ada.Text_IO.End_Of_File (Mounts_File) loop
         declare
            Start_Position : Natural;
            End_Position   : Natural;
            Line           : constant String
              := Ada.Text_IO.Get_Line (Mounts_File);
         begin
            if  Ada.Strings.Fixed.Index (Source  => Line,
                                         Pattern => Filesystem_Type_Name) > 0
            then
               Start_Position := Ada.Strings.Fixed.Index (Source  => Line,
                                                          Pattern => " ");
               End_Position := Ada.Strings.Fixed.Index
                 (Source  => Line,
                  Pattern => " ",
                  From => Start_Position + 1);

               Ada.Text_IO.Close (File => Mounts_File);
               return Line (Start_Position + 1 .. End_Position - 1);
            end if;
         end;
      end loop;

      Ada.Text_IO.Close (File => Mounts_File);
      raise Debug_Filesystem_Not_Mounted;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         --  Cause: Ada.Text_IO.Open
         raise ProcFS_File_Not_Found;
   end Debug_Filesystem_Path;

   ----------------------------------------------------------------------------

end MeshAware.Linux;
