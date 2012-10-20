/*
 * Copyright 2012 Torsti Schulz
 *
 * This file is part of the meshaware library.
 *
 * libmeshaware is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * libmeshaware is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/// @file

#include <stdlib.h>
#include <string.h>

#include "meshaware.h"
#include "meshutil/batman_adv.h"

struct ma_mesh_network *ma_get_available_mesh_networks (void)
{
   struct ma_mesh_network *first_mesh   = NULL;
   struct ma_mesh_network *current_mesh = NULL;

#ifdef __linux

   /// FIXME: Only looks for "bat0". Ignores errors.

   int batman_adv_ver_str_len;

   if (mu_badv_kmod_available (NULL)
       && mu_badv_kmod_loaded (NULL)
       && mu_badv_if_available (NULL, NULL)
       && mu_badv_if_up (NULL, NULL)) {

      first_mesh = malloc (sizeof (struct ma_mesh_network));

      if (!first_mesh) { return NULL; } // on malloc() error

      batman_adv_ver_str_len = strlen (mu_badv_kmod_version (NULL));

      first_mesh->protocol = batman_adv;
      first_mesh->version  = calloc (batman_adv_ver_str_len + 1, sizeof (char));

      strncpy (first_mesh->version, 
               mu_badv_kmod_version (NULL),
                batman_adv_ver_str_len); /// TODO: calloc ensures that last the
                                         ///       byte is '\0'?

      current_mesh = first_mesh;

   }

#endif                          /* __linux */

   return first_mesh;
}
