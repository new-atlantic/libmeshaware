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

#ifndef MESHAWARE_BATMAN_ADV_H
#define MESHAWARE_BATMAN_ADV_H 1
#ifdef __linux

/* We only support the "new" version numbering introduced with 2010.0.0. We
 * don't see much reason to even trying to support older versions than that.
 */

typedef struct
{
	unsigned int year;
	unsigned int release_number;
	unsigned int bugfix_counter;
} batman_adv_version;

/**
 * @brief Test whether the batman_adv kernel module is loaded.
 *
 * Should not work for releases preceding 2010.0.0.
 *
 * @retval  0 The module is loaded.
 * @retval  1 The module is not loaded.
 * @retval -1 Error trying to determine if the module is loaded.
 **/

extern int __attribute__ ((visibility ("internal")))
batman_adv_kernel_mod_loaded (void);

/**
 * @brief Test whether the batman_adv kernel module version is supported.
 *
 * @retval  0 The version is (at least partly) supported.
 * @retval  1 The module is (likely) not supported.
 * @retval -1 Module not loaded or version not recognized.
 **/

extern int __attribute__ ((visibility ("internal")))
batman_adv_kernel_mod_version_supported (void);

/**
 * @brief Get potential next hops in bat mesh.
 *
 * @param[in,out] addresses Pointer to an array of addresses. The funtion will
 *                          call malloc for a NULL-pointer or reallocate the
 *                          memory to fit the number of addresses.
 *
 * @returns Number of neighbours and thus length of array.
 * @retval  0 No neighbours. Will not change addresses.
 * @retval -1 Error.
 *
 **/

extern int __attribute__ ((visibility ("internal")))
batman_adv_mesh_potential_next_hops (/* pointer to array of (MAC?)addresses */);

/**
 * @brief Check the version of batman_adv.
 *
 * @param[in,out] version Pointer to struct to hold version information.
 *
 * @retval -1 Version could not be determined.
 **/

extern __attribute__ ((visibility ("internal")))
int batman_adv_module_version (batman_adv_version *version);

#endif /* __linux */
#endif /* MESHAWARE_BATMAN_ADV_H */

