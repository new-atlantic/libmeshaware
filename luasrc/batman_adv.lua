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

-- batman-adv releases to date. Last updated 2012-03-25.
-- Version scheme description:
-- http://www.open-mesh.org/wiki/open-mesh/2010-06-19-batman-adv-2010-0-0-release
--
-- RELEASE  | DATE       | STATUS
-- ---------|------------|-------
-- 2010.0.0 | 2010-06-19 | unknown
-- 2010.1.0 | 2010-09-04 | unknown
-- 2010.2.0 | 2010-11-22 | unknown
-- 2011.1.0 | 2011-01-29 | unknown
-- 2011.2.0 | 2011-04-19 | unknown
-- 2011.3.0 | 2011-06-19 | unknown
-- 2011.3.1 | 2011-10-19 | unknown
-- 2011.4.0 | 2011-11-14 | partially tested
-- 2012.0.0 | 2012-02-09 | unknown

local M = {}
batman_adv = M

--- LOCAL: Check whether the batman_adv kernel module is loaded.
-- @return true The module is loaded.
-- @return false The module is not loaded.
-- @return nil Could not open /proc/modules. Is this linux?
local function batman_adv_kmod_loaded ()
	local f = io.open('/proc/modules', 'r')
	if not f then
		return nil
	end

	local s = f:read ('*all')
	f:close ()

	if s:match ('batman_adv') then
		return true
	else
		return false
	end
end

--- LOCAL: Get the batman_adv kernel module version.
-- @return version Version string.
-- @return nil Could not open /sys/module/batman_adv/version. Module not loaded?
local function batman_adv_kmod_version ()
	local f = io.open ('/sys/module/batman_adv/version', 'r')
	if f then
		local s = f:read ('*all')
		f:close ()
		return s:sub(1, -2) -- Return string without \n.
	else
		return nil
	end
end

--- LOCAL: Test whether a version of batman_adv is supported.
-- TODO: The supported versions list should be based on tests.
-- @param version batman_adv version string.
-- @return true Version is supported.
-- @return false Version is not supported.
local function batman_adv_kmod_version_supported (version)
	local supported_versions = 
	{
		['2011.4.0'] = true,
	}

	if supported_versions[version] then
		return true
	else
		return false
	end
end

--- LOCAL: Check whether a bat0 interface is available.
-- TODO: The interface does not have to be called bat0 and there could
-- be multiple bat-interfaces.
-- @return true Interface is available.
-- @return false Interface is not available.
local function bat_interface_available ()
	local f = io.open ('/sys/devices/virtual/net/bat0/type', 'r')
	if not f then
		return false
	else
		f:close ()
		return true
	end
end

--- LOCAL: Check that the bat0 interface is up.
-- TODO: Check that the tests are correct for all kinds of network interfaces.
-- @return true The interface is up.
-- @return false The interface is down.
-- @return nil The interface is not available/does not exist.
local function bat_interface_up ()
	local o = io.open ('/sys/devices/virtual/net/bat0/operstate', 'r')
	local c = io.open ('/sys/devices/virtual/net/bat0/carrier', 'r')

	if not o then
		return nil
	end

	if not c then
		o:close ()
		return false
	end

	local os = o:read ('*all')
	local cs = c:read ('*all')

	if os:match('down\n') then
		return false
	elseif (os:match('up\n') or os:match ('unknown\n'))
	       and cs:match ('1\n') then
		return true
	else
		return false
	end

	o:close ()
	c:close ()
end

--- LOCAL: Check that that the bat0 interface is connected to a network.
-- TODO: Not implemented yet. Different test for wired and wifi?
-- e.g. eth0 cable and speaking to other device
-- wifi, connected to a adhoc network
-- @return true The interface is connected.
-- @return false The interface is not connected.
local function bat_interface_connected ()
	return false
end

--- Check that a working batman_adv mesh network is available.
-- @return true A batmesh is available.
-- @return false A batmesh is not available.
function M.batmesh_available (protocol)
	if not batman_adv_kmod_loaded () then
		return false
	elseif not batman_adv_kmod_version_supported ( batman_adv_kmod_version ()) then
		return false
	elseif not bat_interface_available () then
		return false
	elseif not bat_interface_up () then
		return false
--	elseif not bat_interface_connected () then
--		return false
	else
		protocol.name = 'batman_adv'
		protocol.version = batman_adv_kmod_version ()
		return true
	end
end


--- Get the number of nodes in the batmesh.
-- @return number Number of nodes in mesh. TODO: Includes self?
-- @return 0 No other nodes in mesh network.
-- @return nil Could not query bat interface, does it exist?
function M.n_nodes_in_batmesh ()
	local counter = 0
	local f = io.open ('/sys/kernel/debug/batman_adv/bat0/originators', 'r')
	if not f then
		return nil
	end

	f:read ('*line')
	f:read ('*line')
	local s = f:read ('*line')
	if not s:match ('No batman nodes in range ...') then
		while s do
			counter = counter + 1
			s = f:read ('*line')
		end
		f:close ()
		return counter
	else
		f:close ()
		return 0
	end
end

--- Get the number of potential next hops in the batmesh.
function M.batmesh_n_neighbours ()
	local f = io.open ('/sys/kernel/debug/batman_adv/bat0/originators', 'r')
	if not f then
		return nil
	end

	f:read ('*line')
	f:read ('*line')
	local s = f:read ('*line')
	if not s:match ('No batman nodes in range ...') then
		local neighbours = {}
		neighbours.addr_type = "mac"
		local neighbour_mac_addresses = {}
		neighbours.addresses = {}
		while s do
			local counter = 0
			for mac_address in s:gmatch('%x%x:%x%x:%x%x:%x%x:%x%x:%x%x') do
				counter = counter + 1
				if counter > 2 then
					neighbour_mac_addresses[mac_address] = true
				end
			end
			counter = 0
			s = f:read ('*line')
		end
		f:close ()
		for key, value in pairs(neighbour_mac_addresses) do
			neighbours.addresses[#neighbours.addresses + 1] = key
		end
		return neighbours
	else
		f:close ()
		return 0
	end
end

--- Get the number of actual next hops in the batmesh.
function M.batmesh_n_next_hops ()
	local f = io.open ('/sys/kernel/debug/batman_adv/bat0/originators', 'r')
	if not f then
		return nil
	end

	f:read ('*line')
	f:read ('*line')
	local s = f:read ('*line')
	if not s:match ('No batman nodes in range ...') then
		local next_hops = {}
		next_hops.addr_type = "mac"
		local next_hop_mac_addresses = {}
		next_hops.addresses = {}
		while s do
			local counter = 0
			for mac_address in s:gmatch('%x%x:%x%x:%x%x:%x%x:%x%x:%x%x') do
				counter = counter + 1
				if counter == 2 then
					next_hop_mac_addresses[mac_address] = true
					break
				end
			end
			counter = 0
			s = f:read ('*line')
		end
		f:close ()
		for key, value in pairs(next_hop_mac_addresses) do
			next_hops.addresses[#next_hops.addresses + 1] = key
		end
		return next_hops
	else
		f:close ()
		return 0
	end
end

return batman_adv
