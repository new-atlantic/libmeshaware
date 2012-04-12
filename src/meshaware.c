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
		protocol->name = batman_adv;
		batman_adv_version version;
		if (! batman_adv_module_version (&version)) {
			int release_len = 0;
			int bugfix_len = 0;
			int i = version.release_number;
			int j = version.bugfix_counter;
			if (version.release_number) {
				while (i) {
					i /= 10;
					release_len++;
				}
			} else {
				release_len = 1;
			}
			if (version.bugfix_counter) {
				while (j) {
					j /= 10;
					bugfix_len++;
				}
			} else {
				bugfix_len = 1;
			}
			int version_len = 4 + 1 + release_len + 1 + bugfix_len;
			// TODO: Docs must remind to free() protocol.version
			protocol->version = malloc (version_len + 1);
			// TODO: Check return values for snprintf.
			snprintf (protocol->version, 5, "%d", version.year);
			snprintf (protocol->version + 4,
			          release_len + 2,
			          ".%d",
			          version.release_number);
			snprintf (protocol->version + 4 + 1 + release_len,
			          bugfix_len + 2,
			          ".%d",
			          version.bugfix_counter);
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

