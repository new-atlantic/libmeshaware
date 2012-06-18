#!/bin/bash
#  Copyright 2012 Torsti Schulz
#
#  This file is part of the meshaware library for Ada.
#
#  meshaware is free software: you can redistribute it and/or modify
#  it under the terms of the GNU Lesser General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  meshaware is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

PASS="$(echo -n '['; tput setaf 2; echo -n 'pass'; tput sgr0; echo -n ']')"
FAIL="$(echo -n '['; tput setaf 1; echo -n 'fail'; tput sgr0; echo -n ']')"

echo -e "\t\t\t\t\t TEST\t\tEXPECTED\t\tRESULT"
echo -e "\t\t\t\t\t ----\t\t--------\t\t------"
#########
echo -n "1 Test: Kernel_Version"
EXPECTED="$(uname -r)"
RESULT="$(bin/kernel_version)"
if [ "$EXPECTED" == "$RESULT" ]; then
    echo -e "\t\t\t$PASS\t\t$EXPECTED\t\t$RESULT"
else
    echo -e "\t\t$FAIL\t\t$EXPECTED\t\t$RESULT"
fi

#########
echo ""
echo -n "2 Test: Kernel_Module_Available: "

if [ -f "/lib/modules/$(uname -r)/kernel/net/batman-adv/batman-adv.ko" ]; then
    EXPECTED="TRUE"
else
    EXPECTED="FALSE"
fi;

RESULT=$(bin/kmod_available)
if [ "$EXPECTED" == "$RESULT" ]; then
    echo -e "\t$PASS\t\t$EXPECTED\t\t\t$RESULT"
else
    echo -e "\t$FAIL\t\t$EXPECTED\t\t\t$RESULT"
fi

if [ "$EXPECTED" == "FALSE" ]; then
    echo "batman_adv kernel module not available. Aborting..."
fi

########

echo ""
echo "3 Test group: Kernel_Module_Loaded"

modprobe -r batman_adv

if [ "$(grep -o "^batman_adv" /proc/modules)" == "batman_adv" ]; then
    echo "Removing batman_adv failed. Aborting..."
    exit
else
    echo -n "3.1 Test: When not loaded: "
    EXPECTED="FALSE"
    RESULT=$(bin/kmod_loaded)
    if [ "$EXPECTED" == "$RESULT" ]; then
        echo -e "\t\t$PASS\t\t$EXPECTED\t\t\t$RESULT"
    else
        echo -e "\t\t$FAIL\t\t$EXPECTED\t\t\t$RESULT"
    fi
fi;

####

modprobe batman_adv

if [ -z $(grep -o "^batman_adv" /proc/modules) ]; then
    echo "Inserting kernel module failed. Aborting..."
    exit
else
    echo -n "3.2 Test: When loaded: "
        EXPECTED="TRUE"
        RESULT=$(bin/kmod_loaded)
        if [ "$EXPECTED" == "$RESULT" ]; then
            echo -e "\t\t\t$PASS\t\t$EXPECTED\t\t\t$RESULT"
        else
            echo -e "\t\t\t$FAIL\t\t$EXPECTED\t\t\t$RESULT"
        fi
fi;

########

echo ""
echo "4 Test group: Kernel_Module_Version"

modprobe -r batman_adv

if [ "$(grep -o "^batman_adv" /proc/modules)" == "batman_adv" ]; then
    echo "Removing batman_adv failed. Aborting..."
    exit
else
    echo -n "4.1 Test: When kmod not loaded: "
    EXPECTED="NONE"
    RESULT=$(bin/kmod_version)
    if [ "$EXPECTED" == "$RESULT" ]; then
        echo -e "\t$PASS\t\t$EXPECTED\t\t\t$RESULT"
    else
        echo -e "\t$FAIL\t\t$EXPECTED\t\t\t$RESULT"
    fi
fi;

####

modprobe batman_adv

if [ ! $(grep -o "^batman_adv" /proc/modules) == "batman_adv" ]; then
    echo "Inserting kernel module failed. Aborting..."
    exit
else
    echo -n "4.2 Test: When kmod loaded: "
    RESULT=$(bin/kmod_version)
    if [ -f /sys/module/batman_adv/version ]; then
        EXPECTED=$(cat "/sys/module/batman_adv/version")
    else
        EXPECTED="$FAIL: kmod missing"
    fi
    if [ "$EXPECTED" == "$RESULT" ]; then
        echo -e "\t\t$PASS\t\t$EXPECTED\t\t$RESULT"
    else
        echo -e "\t\t$FAIL\t\t$EXPECTED\t$RESULT"
    fi
fi;

########

echo ""
echo "5 Test group: Debug_FS_Mounted"

DEBUG_FS_PATH=$(cat /proc/mounts  | grep debugfs | grep -o "/[^[:space:]]*")
if [ -n "$DEBUG_FS_PATH" ]; then
    umount "$DEBUG_FS_PATH"
fi

DEBUG_FS_PATH=$(cat /proc/mounts  | grep debugfs | grep -o "/[^[:space:]]*")
if [ -n "$DEBUG_FS_PATH" ]; then
    echo "Unmounting debugfs failed. Aborting..."
    exit
else
    echo -n "5.1 Test: debugfs not mounted: "
    RESULT=$(bin/debugfs_mounted)
    EXPECTED="FALSE"
    if [ "$EXPECTED" == "$RESULT" ]; then
        echo -e "\t\t$PASS\t\t$EXPECTED\t\t\t$RESULT"
    else
        echo -e "\t\t$FAIL\t\t$EXPECTED\t\t\t$RESULT"
    fi
fi

####

mount -t debugfs none /sys/kernel/debug/

DEBUG_FS_PATH=$(cat /proc/mounts  | grep debugfs | grep -o "/[^[:space:]]*")

if [ -z "$DEBUG_FS_PATH" ]; then
    echo "Mounting debugfs failed. Aborting..."
    exit
else
    echo -n "5.2 Test: debugfs mounted: "
    RESULT="$(bin/debugfs_mounted)"
    EXPECTED="TRUE"
    if [ "$EXPECTED" == "$RESULT" ]; then
        echo -e "\t\t$PASS\t\t$EXPECTED\t\t\t$RESULT"
    else
        echo -e "\t\t$FAIL\t\t$EXPECTED\t\t\t$RESULT"
    fi
fi

########

echo ""
echo "6 Test group: Debug_FS_Path"

DEBUG_FS_PATH=$(cat /proc/mounts  | grep debugfs | grep -o "/[^[:space:]]*")
if [ -n "$DEBUG_FS_PATH" ]; then
    umount "$DEBUG_FS_PATH"
fi

DEBUG_FS_PATH=$(cat /proc/mounts  | grep debugfs | grep -o "/[^[:space:]]*")
if [ -n "$DEBUG_FS_PATH" ]; then
    echo "Unmounting debugfs failed. Aborting..."
    exit
else
    echo -n "6.1 Test: debugfs not mounted: "
    RESULT=$(bin/debugfs_path)
    EXPECTED="NONE"
    if [ "$EXPECTED" == "$RESULT" ]; then
        echo -e "\t\t$PASS\t\t$EXPECTED\t\t\t$RESULT"
    else
        echo -e "\t\t$FAIL\t\t$EXPECTED\t\t\t$RESULT"
    fi
fi

####

mount -t debugfs none /sys/kernel/debug/

DEBUG_FS_PATH=$(cat /proc/mounts  | grep debugfs | grep -o "/[^[:space:]]*")

if [ -z "$DEBUG_FS_PATH" ]; then
    echo "Mounting debugfs failed. Aborting..."
    exit
else
    echo -n "6.2 Test: debugfs mounted: "
    RESULT=$(bin/debugfs_path)
    EXPECTED="$DEBUG_FS_PATH"
    if [ "$EXPECTED" == "$RESULT" ]; then
        echo -e "\t\t$PASS\t\t$(echo -n "$EXPECTED" | cut -c -20 )\t$(echo -n "$RESULT" | cut -c -20 )"
    else
        echo -e "\t\t$FAIL\t\t$(echo -n "$EXPECTED" | cut -c -20 )\t$(echo -n "$RESULT" | cut -c -20 )"
    fi
fi

########
