/*
 * Copyright 2012 Timo Sand, Torsti Schulz
 *
 * This file is part of the meshaware library.
 *
 * meshaware is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * meshaware is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/// @file meshaware.c

#include <stdlib.h>

#include "meshaware.h"
#include "batman_adv.h"

int maw_determine_mesh_protocol(maw_mesh_protocol *protocol) {
	if (! batman_adv_kernel_mod_loaded()) {
		protocol->name = batman_adv;
		// TODO: Call batman_adv_module_version to determine version.
		protocol->version = (char*) '1';
		return 0;
	} else {
		protocol = NULL;
		return NO_MESH_PROTOCOL_AVAILABLE;
	}
}

