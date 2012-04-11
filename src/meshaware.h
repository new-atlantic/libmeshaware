/*
 * Copyright 2012 Torsti Schulz, Timo Sand
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

#ifndef MESHAWARE_H
#define MESHAWARE_H 1

#include <stdint.h>

// Error codes.
#define NO_MESH_PROTOCOL_AVAILABLE 101



typedef enum maw_protocol_name {
	batman_adv,
} maw_protocol_name;

typedef struct maw_mesh_protocol
{
	maw_protocol_name name;
	char              *version;
} maw_mesh_protocol;

typedef enum maw_address_type {
	IPv4,
	IPv6,
	MAC48,
} maw_address_type;

typedef union maw_address {
	//TODO: Use POSIX or X/OPEN types for IP-addresses.
	uint32_t IPv4_addr;
	//type IPv6_addr;
	//type MAC48_addr;
} maw_address;

/**
 * @brief Sets the protocol to the used.
 *
 * @returns Success or failure.
 **/

int maw_determine_mesh_protocol(maw_mesh_protocol*);

/**
 * @brief Returns a the number and addresses of neighbouring nodes.
 *
 *
 * @param[in]     protocol  Mesh protool type & version.
 * @param[out]    addr_type Type of netwotk address in addresses.
 * @param[in,out] addresses Pointer to an array of addresses. The funtion will
 *                          call malloc for a NULL-pointer or reallocate the
 *                          memory to fit the number of addresses.
 *
 * @returns Number of neighbouring nodes.
 *
 * @retval  0 No neighbouring nodes available.
 * @retval -1 An error occurred.
 *
 **/

extern int __attribute__ ((visibility ("default")))
maw_potential_next_hops (maw_mesh_protocol const *protocol,
                         maw_address_type        *addr_type,
                         maw_address             *addresses);

#endif /* MESHAWARE_H */

