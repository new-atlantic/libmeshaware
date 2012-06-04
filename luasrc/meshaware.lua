-- Copyright 2012 Torsti Schulz
--
-- This file is part of the meshaware library.
--
-- meshaware is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- meshaware is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

require 'batman_adv'

local M = {}
meshaware = M

--- Check whether a mesh network is available.
-- @return nil If no mesh network is available return nil.
-- @return protocol Return a protocol "object".
function M.mesh_available ()
	if batman_adv.batmesh_available () then
		return 'batman_adv'
	else
		return nil
	end
end

--- Get the number of nodes in the network.
-- @param protocol A mesh protocol object from mesh_available ()
-- @return number The number of nodes in the network.
-- @return 0 No nodes other than self in network.
-- @return nil Protocol or mesh not available.
function M.n_nodes_in_mesh (protocol)
	if protocol == 'batman_adv' then
		return batman_adv.n_nodes_in_batmesh ()
	else
		return nil
	end
end

--- Get the number of potential next hops.
-- @param protocol A mesh protocol object from mesh_available ()
-- @return number The number of potential next hops.
-- @return 0 No nodes other than self in network.
-- @return nil Protocol or mesh not available.
function M.n_neighbours (protocol)
	return nil
end

--- Get the number of actual next hops.
-- @param protocol A mesh protocol object from mesh_available ()
-- @return number The number of next hops.
-- @return 0 No nodes other than self in network.
-- @return nil Protocol or mesh not available.
function M.n_next_hops (protocol)
	return nil
end


return meshaware

