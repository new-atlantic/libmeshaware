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

#ifdef __linux

#ifndef _ISOC99_SOURCE
#define _ISOC99_SOURCE 1
#endif

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 700
#endif

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
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

/* batman-adv releases to date. Last updated 2012-03-25.
 * Versioning scheme description:
 * http://www.open-mesh.org/wiki/open-mesh/2010-06-19-batman-adv-2010-0-0-release
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

/* Array of versions with at least nominal support */
static batman_adv_version available_batman_adv_versions[] = {
	{2011, 4, 0}
};

int batman_adv_kernel_mod_loaded (void)
{
	// Checking the sys filesystem should work since 2010.0.0, not before.
	if (access ("/sys/module/batman_adv/version", F_OK) != -1) {
		return 1;
	} else {
		if (errno == ENOENT)
			return 0;
		else
			// TODO: Do the other errors apply to F_OK?
			return -1;
	}
}

// TODO: Return int an pass batman_adv_version as pointer? -> No need for dummy.
static batman_adv_version batman_adv_module_version (void)
{
	batman_adv_version version;
	batman_adv_version dummy_version = {0, 0, 0};

	FILE *fp;
	char *line = NULL;
	size_t len = 0;
	ssize_t read;

	fp = fopen ("/sys/module/batman_adv/version", "r");

	if (!fp) return dummy_version;

	if ((read = getdelim (&line, &len, '.', fp)) == -1) {
		return dummy_version;
	} else if (read == 5) {
		for (int i = 0; i < 4; i++) {
			if (!isdigit (line[i])) return dummy_version;
		}
		line[4] = '\0';
		version.year = atoi (line);
	} else {
		return dummy_version;
	}

	if ((read = getdelim (&line, &len, '.', fp)) == -1) {
		return dummy_version;
	} else if (read == 2) {
		if (!isdigit (line[0])) return dummy_version;
		line[1] = '\0';
		version.release_number = atoi (line);
	} else {
		return dummy_version;
	}

	if ((read = getdelim (&line, &len, '.', fp)) == -1) {
		return dummy_version;
	} else if (read >= 1) {
		for (int i = 0; i < read - 1; i++) {
			if (!isdigit (line[i])) return dummy_version;
		}
		version.bugfix_counter = atoi (line);
	} else {
		return dummy_version;
	}

	// clean up;
	if (line) free (line);
	if (fclose (fp) == EOF) return dummy_version;

	return version;
}

#endif

