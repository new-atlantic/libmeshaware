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

/* We only support the "new" version numbering introduced with 2010.0.0. We
 * don't see much reason to even trying to support older versions than that.
 */

typedef struct
{
	unsigned int year;
	unsigned int release_number;
	unsigned int bugfix_counter;
} batman_adv_version;

/* Releases to date. Last updated 2012-03-25.
 *
 * RELEASE  | DATE       | STATUS
 * ---------|------------|-------
 * 2010.0.0 | 2010-06-19 | ?
 * 2010.1.0 | 2010-09-04 | ?
 * 2010.2.0 | 2010-11-22 | ?
 * 2011.1.0 | 2011-01-29 | ?
 * 2011.2.0 | 2011-04-19 | ?
 * 2011.3.0 | 2011-06-19 | ?
 * 2011.3.1 | 2011-10-19 | ?
 * 2011.4.0 | 2011-11-14 | partial
 * 2012.0.0 | 2012-02-09 | ?
 */

static batman_adv_version available_batman_adv_versions[] = {
	{0,    0, 0}, // dummy version: something is wrong or not implemented
	{2011, 4, 0}
};

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

static batman_adv_version batman_adv_module_version (void)
{
	batman_adv_version version = {0, 0, 0};
	return version;
}

