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

#include <stdio.h>
#include <stdlib.h>

#include "meshaware.h"
#include "batman_adv.h"

int maw_determine_mesh_protocol(maw_mesh_protocol *protocol) {
	if (! batman_adv_kernel_mod_loaded()) {
		// TODO: Need to allocate with malloc, not automagically.
		protocol->name = batman_adv;
		if (! batman_adv_module_version_string (&(protocol->version))) {
			// TODO: Docs must remind to free() protocol.version
			return 0;
		} else {
			// TODO: return defined ERR_CODE
			return 1;
		}
	} else {
		protocol = NULL;
		return NO_MESH_PROTOCOL_AVAILABLE;
	}
}

