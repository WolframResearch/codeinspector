
cmake_minimum_required(VERSION 3.0)

project(codeinspector
	LANGUAGES
		NONE
)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})
include(WolframKernel)
include(PacletInfo)

#
# Used for quickly reporting syntax errors in WL source files
#
find_program(CODEPARSER_EXE
	NAMES
		codeparser codeparser.exe
	HINTS
		${CODEPARSER_EXE_DIR}
)

set(PACLET "CodeInspector")
set(WOLFRAMKERNEL ${WOLFRAMKERNEL_DEFAULT} CACHE FILEPATH "Path to WolframKernel")
set(LOCAL_BUILD OFF CACHE BOOL "Local build")
# Work-around for bug 349779 is to pause ~1 second
set(BUG349779_PAUSE 1 CACHE STRING "Bug 349779 pause")
#
# Evidence suggests that when bug 349779 strikes, the kernel does exit after 30 minutes
# So double that and cross fingers.
#
# Related bugs: 349779
# Related issues: RE-514227
#
set(BUG349779_TIMEOUT 3600 CACHE STRING "Bug 349779 timeout")

message(STATUS "CMAKE_VERSION: ${CMAKE_VERSION}")
message(STATUS "PACLET: ${PACLET}")
message(STATUS "WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
message(STATUS "LOCAL_BUILD: ${LOCAL_BUILD}")
message(STATUS "CMAKE_SIZEOF_VOID_P: ${CMAKE_SIZEOF_VOID_P}")
message(STATUS "BUG349779_PAUSE: ${BUG349779_PAUSE}")
message(STATUS "BUG349779_TIMEOUT: ${BUG349779_TIMEOUT}")
message(STATUS "CODEPARSER_EXE: ${CODEPARSER_EXE}")

set(WL_PACLET_SOURCES
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/AbstractRules.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/AggregateRules.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/Boxes.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/BracketMismatches.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/CodeInspector.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/ConcreteRules.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/External.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/Format.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/ImplicitTokens.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/Summarize.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/Utils.wl
)

set(WL_PACLET_EXAMPLES
	${PROJECT_SOURCE_DIR}/CodeInspector/Resources/Examples/Collatz.m
)

set(PACLETINFO_SOURCE
	${PROJECT_SOURCE_DIR}/${PACLET}/PacletInfo.m
)



#
# Set VERSION_NUMBER, SYSTEMID, and PACLET_VERSION
#
CheckWolframKernel()
CheckPacletInfo()


file(MAKE_DIRECTORY
	${PROJECT_BINARY_DIR}/paclet/${PACLET}
)


#
# Copy WL source files
#

set(COPIED_PACLETINFO ${PROJECT_BINARY_DIR}/paclet/${PACLET}/PacletInfo.m)

add_custom_command(
	OUTPUT
		${COPIED_PACLETINFO}
	COMMAND
		${CMAKE_COMMAND} -DSRC=${PACLETINFO_SOURCE} -DCODEPARSER_EXE=${CODEPARSER_EXE} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	COMMAND
		${CMAKE_COMMAND} -E copy ${PACLETINFO_SOURCE} ${COPIED_PACLETINFO}
	COMMAND
		${CMAKE_COMMAND} -DLOCAL_BUILD=${LOCAL_BUILD} -DLOCAL_BUILD_VERSION=${LOCAL_BUILD_VERSION} -DCOPIED_PACLETINFO=${COPIED_PACLETINFO} -P ${PROJECT_SOURCE_DIR}/cmake/ReplacePacletInfo.cmake
	DEPENDS
		${PACLETINFO_SOURCE}
		${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
		${PROJECT_SOURCE_DIR}/cmake/ReplacePacletInfo.cmake
)
list(APPEND COPIED_WL_PACLET_SOURCES ${COPIED_PACLETINFO})

foreach(SRC ${WL_PACLET_SOURCES})
	get_filename_component(BARE_SRC ${SRC} NAME)
	add_custom_command(
		OUTPUT
			${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC}
		COMMAND
			${CMAKE_COMMAND} -DSRC=${SRC} -DCODEPARSER_EXE=${CODEPARSER_EXE} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
		COMMAND
			${CMAKE_COMMAND} -E copy ${SRC} ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC}
		DEPENDS
			${SRC}
			${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	)
	list(APPEND COPIED_WL_PACLET_SOURCES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC})
endforeach()

foreach(SRC ${WL_PACLET_EXAMPLES})
	get_filename_component(BARE_SRC ${SRC} NAME)
	add_custom_command(
		OUTPUT
			${PROJECT_BINARY_DIR}/paclet/${PACLET}/Resources/Examples/${BARE_SRC}
		#
		# Do not inspect examples
		#
		COMMAND
			${CMAKE_COMMAND} -E copy ${SRC} ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Resources/Examples/${BARE_SRC}
		DEPENDS
			${SRC}
	)
	list(APPEND COPIED_WL_PACLET_EXAMPLES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Resources/Examples/${BARE_SRC})
endforeach()



#
# paclet target
#

set(PACLET_SOURCES
	${COPIED_WL_PACLET_SOURCES}
	${COPIED_WL_PACLET_EXAMPLES}
)

if(LOCAL_BUILD)
set(PACLET_OUTPUT ${PROJECT_BINARY_DIR}/paclet/${PACLET}-${LOCAL_BUILD_VERSION}.paclet)
else(LOCAL_BUILD)
set(PACLET_OUTPUT ${PROJECT_BINARY_DIR}/paclet/${PACLET}-${PACLET_VERSION}.paclet)
endif(LOCAL_BUILD)

add_custom_target(paclet
	DEPENDS
		${PACLET_OUTPUT}
)

#
# Create the paclet archive
#
add_custom_command(
	OUTPUT
		${PACLET_OUTPUT}
	DEPENDS
		${PACLET_SOURCES}
		${PROJECT_SOURCE_DIR}/${PACLET}/Generate/CreatePacletArchive.wl
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	COMMAND
		${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/${PACLET}/Generate/CreatePacletArchive.wl -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -DPACLET=${PACLET} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	VERBATIM
	WORKING_DIRECTORY
		${PROJECT_SOURCE_DIR}
)



