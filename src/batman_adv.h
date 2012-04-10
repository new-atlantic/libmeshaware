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
#define MESHAWARE_BATMAN_ADV_H
#ifdef __linux

/**
 * @brief Test whether the module is loaded.
 *
 * Should not work for releases preceding 2010.0.0
 *
 * @retval  1 The module is loaded.
 * @retval  0 The module is not loaded.
 * @retval -1 Error trying to determine if the module is loaded.
 **/

int __attribute__ ((visibility ("internal")))
batman_adv_kernel_mod_loaded (void);

/**
 * @brief Test whether the module version is supported.
 *
 * @retval  1 The version is (at least partly) supported.
 * @retval  0 The module is (likely) not supported.
 * @retval -1 Module not loaded or version not recognized.
 **/

int __attribute__ ((visibility ("internal")))
batman_adv_kernel_mod_version_supported (void);

/**
 * @brief Get potential next hops from local node.
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

int __attribute__ ((visibility ("internal")))
batman_adv_mesh_potential_next_hops (/* pointer to array of (MAC?)addresses */);

#endif
#endif

