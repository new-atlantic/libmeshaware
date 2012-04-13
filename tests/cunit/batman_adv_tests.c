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

#undef _ISOC99_SOURCE
#define _ISOC99_SOURCE 1

#undef _XOPEN_SOURCE
#define _XOPEN_SOURCE 700

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <CUnit/CUnit.h>
#include <CUnit/Basic.h>

#include "../../src/batman_adv.c"

#define KERNEL_MODULE_NAME "batman_adv"

void check_module_loaded (void)
{
	FILE *fp;
	char *line = NULL;
	size_t len = 0;
	ssize_t read;

	ssize_t module_name_len = strlen (KERNEL_MODULE_NAME);
	bool module_loaded = false;

	fp = fopen ("/proc/modules", "r");
	if (!fp) CU_FAIL ("opening '/proc/modules' failed");

	while ((read = getline (&line, &len, fp)) != -1) {
		if (read > module_name_len) {
			if (!strncmp (line,
			              KERNEL_MODULE_NAME,
			              module_name_len)) {
				module_loaded = true;
				break;
			}
		}
	}

	if (!line) free (line);
	line = NULL;

	if (!module_loaded) {
		CU_ASSERT (batman_adv_kernel_mod_loaded() == 1);
	} else {
		CU_ASSERT (batman_adv_kernel_mod_loaded() == 0);
	}

	if (fclose (fp) == EOF) CU_FAIL ("opening '/proc/modules' failed");
}

void check_module_version (void) {
	batman_adv_version *version = malloc (sizeof (batman_adv_version));
	int version_ret_val = batman_adv_module_version (version);

	if (batman_adv_kernel_mod_loaded()) {
		CU_ASSERT (version_ret_val == -1);
	} else {
		FILE *fp;
		char *line = NULL;
		size_t len = 0;
		ssize_t read;

		char *year = malloc (5);
		char *release = malloc (2);

		fp = fopen (BATMAN_ADV_VERSION_PATH, "r");
		if (!fp) CU_FAIL ("opening 'batman_adv/version' failed");

		if ((read = getline (&line, &len, fp)) == -1)
			CU_FAIL ("reading 'batman_adv/version' failed");

		char *bug = malloc (read - 5);

		memcpy (year, line, 4);
		year[4] = '\0';
		memcpy (release, line + 5, 1);
		release[1] = '\0';
		memcpy (bug, line + 7, read - 6);
		bug[read - 6] = '\0';

		CU_ASSERT (version->year == atoi (year));
		CU_ASSERT (version->release_number == atoi (release));
		CU_ASSERT (version->bugfix_counter == atoi (bug));

		free (year);
		free (release);
		free (bug);
		free (version);

		len = 0;
		char *version_string = NULL;
		
		if (batman_adv_module_version_string (&version_string)) {
			CU_FAIL ("getting the version string failed");
		}

		fp = fopen (BATMAN_ADV_VERSION_PATH, "r");
		if (!fp) CU_FAIL ("opening 'batman_adv/version' failed");

		if ((read = getline (&line, &len, fp)) == -1)
			CU_FAIL ("reading 'batman_adv/version' failed");

		CU_ASSERT (strncmp(version_string, line, read - 1) == 0);
		
		free (line);
	}
}

int main (void)
{
	unsigned int failures;
	CU_pSuite pSuite = NULL;

	if (CU_initialize_registry() != CUE_SUCCESS)
		return CU_get_error();

	pSuite = CU_add_suite ("meshaware batman_adv suite", NULL, NULL);

	if (!pSuite) {
		CU_cleanup_registry();
		return CU_get_error();
	}

	if (!CU_add_test (pSuite,
	                  "Test whether checking for mod batman_adv works",
	                  check_module_loaded)) {
		CU_cleanup_registry();
		return CU_get_error();
	}

	if (!CU_add_test (pSuite,
	                  "Test whether checking of batman_adv version works",
	                  check_module_version)) {
		CU_cleanup_registry();
		return CU_get_error();
	}

	CU_basic_set_mode (CU_BRM_VERBOSE);
	CU_basic_run_tests();
	failures = CU_get_number_of_failures();
	CU_cleanup_registry();

	if (failures > 0) {
		return EXIT_FAILURE;
	} else {
		return EXIT_SUCCESS;
	}
}

#endif /* __linux */

