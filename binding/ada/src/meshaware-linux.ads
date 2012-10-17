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

-------------------------------------------------------------------------------
--  MeshAware.Linux
--
--  Purpose:
--    This package provides functions for accessing information that is only
--    relevant for Linux-based systems.
-------------------------------------------------------------------------------

package MeshAware.Linux is

   ----------------------------------------------------------------------------
   --  Exceptions
   ----------------------------------------------------------------------------

   DebugFS_Not_Mounted   : exception;  --  Raised when attempting to access
                                       --  debugfs or retrieve its mount path
                                       --  while it is not mounted.

   ProcFS_File_Not_Found : exception;  --  Raised when failing to access a file
                                       --  under /proc.

   function Kernel_Version return String;
   ----------------------------------------------------------------------------
   --  Purpose:
   --    This function returns the Linux kernel version.
   --    (Equivalent to uname -r).
   --  Exceptions:
   --    ProcFS_File_Not_Found
   ----------------------------------------------------------------------------

   function Debug_Filesystem_Mounted return Boolean;
   ----------------------------------------------------------------------------
   --  Purpose:
   --    This function checks if the Linux Debug Filesystem (debugfs) is
   --    mounted.
   --
   --  Exceptions:
   --    ProcFS_File_Not_Found
   ----------------------------------------------------------------------------

   function Debug_Filesystem_Path return String;
   ----------------------------------------------------------------------------
   --  Purpose:
   --    This function returns the path to the mount point of debugfs. If
   --    debugfs is mounted at multiple paths, returns the first one listed
   --    in /proc/mounts.
   --  Exceptions:
   --    DebugFS_Not_Mounted
   --    ProcFS_File_Not_Found
   ----------------------------------------------------------------------------

end MeshAware.Linux;
