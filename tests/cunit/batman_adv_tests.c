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

#define _ISOC99_SOURCE
#define _XOPEN_SOURCE 700

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
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
		CU_ASSERT (batman_adv_kernel_mod_loaded() == 0);
	} else {
		CU_ASSERT (batman_adv_kernel_mod_loaded() == 1);
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

