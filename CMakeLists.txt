cmake_minimum_required (VERSION 2.8)
project (meshaware C)
set (meshaware_VERSION_MAJOR 0)
set (meshaware_VERSION_MINOR 0)
set (meshaware_PATCH_VERSION 0)
set (meshaware_VERSION
   ${meshaware_MAJOR_VERSION}.${meshaware_MINOR_VERSION}.${meshaware_PATCH_VERSION})

add_library (meshaware SHARED src/batman_adv.c)

set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O2 -Wall -Wextra -fpic")
if (CMAKE_COMPILER_IS_GNUCC)
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c99")
endif (CMAKE_COMPILER_IS_GNUCC)

install (TARGETS meshaware
         LIBRARY DESTINATION lib)
install(DIRECTORY src/ DESTINATION include/meshaware
          FILES_MATCHING PATTERN "*.h")

enable_testing ()
find_library (CUNIT_LIBRARY cunit)
add_executable (cunit_batman_adv tests/cunit/batman_adv_tests.c)
target_link_libraries (cunit_batman_adv ${CUNIT_LIBRARY})
add_test (cunit_batman_adv_test cunit_batman_adv)
