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
install(DIRECTORY src/ DESTINATION include/
          FILES_MATCHING PATTERN "meshaware.h")

enable_testing ()
find_library (CUNIT_LIBRARY cunit)
set (test_BIN_DIR "tests/bin/")
IF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
	add_executable (${test_BIN_DIR}cunit_batman_adv tests/cunit/batman_adv_tests.c)
	target_link_libraries (${test_BIN_DIR}cunit_batman_adv ${CUNIT_LIBRARY})
	add_test (cunit_batman_adv_test ${test_BIN_DIR}cunit_batman_adv)
ENDIF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")

