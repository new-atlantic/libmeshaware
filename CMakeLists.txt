cmake_minimum_required (VERSION 2.8)
project (meshaware C)
set (meshaware_VERSION_MAJOR 0)
set (meshaware_VERSION_MINOR 0)
set (meshaware_PATCH_VERSION 1)
set (meshaware_VERSION
   "${meshaware_VERSION_MAJOR}.${meshaware_VERSION_MINOR}.${meshaware_PATCH_VERSION}")

find_library (MESHUTIL_LIBRARY meshutil)

add_library (meshaware SHARED src/meshaware.c)
add_library (meshaware_static STATIC src/meshaware.c)

target_link_libraries (meshaware meshutil)

set (HARDENING_FLAGS "-Wformat -Wformat-security -Werror=format-security -D_FORTIFY_SOURCE=2 -fstack-protector --param ssp-buffer-size=4 -Wl,-z,now -Wl,-z,relro")

set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O2 -Wall -Wextra -fpic ${HARDENING_FLAGS}")
if (CMAKE_COMPILER_IS_GNUCC)
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c99")
endif (CMAKE_COMPILER_IS_GNUCC)

install (TARGETS meshaware
         LIBRARY DESTINATION lib)
install(DIRECTORY src/ DESTINATION include/meshaware
          FILES_MATCHING PATTERN "*.h")
          
