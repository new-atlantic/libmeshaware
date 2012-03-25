/*
 * Copyright 2012 Torsti Schulz
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

#include <errno.h>
#include <unistd.h>

#include "batman_adv.h"

static int batman_adv_kernel_mod_loaded (void)
{
	if (access ("/sys/module/batman_adv/version", F_OK) != -1) {
		return 1;
	} else {
		if (errno == ENOENT)
			return 0;
		else
			return -1;
	}
}

// TODO: batman_adv_version: fp = fopen ("/sys/module/batman_adv/version");

