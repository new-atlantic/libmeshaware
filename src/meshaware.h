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

#ifndef MESHAWARE_H
#define MESHAWARE_H 1

typedef enum {
   batman_adv,  /// B.A.T.M.A.N. advanced
} ma_mesh_protocol;

struct ma_mesh_network {
   ma_mesh_protocol         protocol;
   char                     *version;
   struct ma_mesh_network   *next;
};

struct ma_mesh_network *ma_get_available_mesh_networks (void);

#endif                          /* MESHAWARE_H */
