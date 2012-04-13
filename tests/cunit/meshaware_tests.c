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

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 700
#endif

#include <stdlib.h>
#include <string.h>

#include <CUnit/CUnit.h>
#include <CUnit/Basic.h>

#include "../../src/meshaware.h"
#include "../../src/batman_adv.c"

void check_that_batman_adv_is_loaded(void) {
	maw_mesh_protocol *returned_protocol = malloc(sizeof(maw_mesh_protocol));
	if (!batman_adv_kernel_mod_loaded()
	    && !maw_determine_mesh_protocol(returned_protocol)) {
		FILE *fp;
		char *line = NULL;
		size_t len = 0;
		ssize_t read;

		fp = fopen ("/sys/module/batman_adv/version", "r");
		if (!fp) CU_FAIL ("opening 'batman_adv/version' failed");

		if ((read = getline (&line, &len, fp)) == -1)
			CU_FAIL ("reading 'batman_adv/version' failed");

		CU_ASSERT (returned_protocol->name == batman_adv);
		CU_ASSERT (strncmp(returned_protocol->version, line, read - 1) == 0);

	}
	free(returned_protocol->version);
	free(returned_protocol);
}

int main (void) {
	unsigned int failures;
	CU_pSuite pSuite = NULL;

	if (CU_initialize_registry() != CUE_SUCCESS)
		return CU_get_error();

	pSuite = CU_add_suite ("meshaware suite", NULL, NULL);

	if (!pSuite) {
		CU_cleanup_registry();
		return CU_get_error();
	}

	if (!CU_add_test (pSuite,
	                  "Test whether checking for mesh protocol works",
	                  check_that_batman_adv_is_loaded)) {
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

