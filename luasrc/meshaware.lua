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

M.mesh = {}

--- Check whether a mesh network is available and initialize the mesh object.
-- TODO: Support for prefering a specific mesh protocol if multiple available.
-- @return true Mesh network is available.
-- @return false No mesh network available.
function M.mesh.mesh_available (self)
	self.protocol = {}
	if batman_adv.batmesh_available (self.protocol) then
		self.n_nodes_in_mesh = batman_adv.n_nodes_in_batmesh
		self.neighbours = batman_adv.batmesh_neighbours
		self.next_hops = batman_adv.batmesh_next_hops
		return true
	else
		self = nil
		return false
	end
end

return meshaware

