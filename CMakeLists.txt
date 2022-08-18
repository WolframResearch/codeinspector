
#
# https://cmake.org/cmake/help/latest/release/3.15.html
# The cmake(1) command gained a new --install option. This may be used after building a project to run installation without using the generated build system or the native build tool.
#
cmake_minimum_required(VERSION 3.15)

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
set(PACLET_LAYOUT_DIR "paclet" CACHE FILEPATH "Path to complete, built paclet layout (relative to build directory)")
set(WOLFRAMKERNEL ${WOLFRAMKERNEL_DEFAULT} CACHE FILEPATH "Path to WolframKernel")
set(LOCAL_BUILD OFF CACHE BOOL "Local build")
#
# Time to pause when starting kernel
#
# Work-around for bug 349779 is to pause ~1 second
# bug 349779 was fixed in version 12.0
# 
# Related bugs: 349779
#
set(KERNEL_PAUSE 0 CACHE STRING "Kernel startup pause")
#
# Timeout for starting a kernel and getting a result
#
# RE machines can be very slow when starting a kernel, so we need to be very generous with this timeout
#
# Should be at least 10 minutes = 600 seconds
#
# Evidence suggests that when bug 349779 strikes, the kernel does exit after 30 minutes = 1800 seconds
# bug 349779 was fixed in version 12.0
#
# Related bugs: 349779
# Related issues: RE-514227
#
set(KERNEL_TIMEOUT 600 CACHE STRING "Kernel startup timeout")

if(NOT DEFINED BUILDNUMBER)
set(BUILDNUMBER 0)
endif()
message(STATUS "BUILDNUMBER: ${BUILDNUMBER}")
message(STATUS "CMAKE_VERSION: ${CMAKE_VERSION}")
message(STATUS "PACLET: ${PACLET}")
message(STATUS "PACLET_LAYOUT_DIR: ${PACLET_LAYOUT_DIR}")
message(STATUS "WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
message(STATUS "LOCAL_BUILD: ${LOCAL_BUILD}")
if(LOCAL_BUILD)
message(STATUS "Configuring for local build")
endif()
# message(STATUS "CMAKE_SIZEOF_VOID_P: ${CMAKE_SIZEOF_VOID_P}")
message(STATUS "KERNEL_PAUSE: ${KERNEL_PAUSE}")
message(STATUS "KERNEL_TIMEOUT: ${KERNEL_TIMEOUT}")
message(STATUS "CODEPARSER_EXE: ${CODEPARSER_EXE}")
if(NOT CODEPARSER_EXE)
message(STATUS "Optional tool CODEPARSER_EXE was not found; skipping")
endif()

set(STATIC_WL_PACLET_KERNEL_SOURCES
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/AbstractRules.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/AggregateRules.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/Boxes.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/BracketMismatches.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/CodeInspector.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/ConcreteRules.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/External.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/Format.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/ImplicitTokens.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/LinterUI.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/Summarize.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/SuppressedRegions.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/TokenRules.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/Utils.wl
)

set(STATIC_WL_PACLET_FRONTEND_SOURCES
	${PROJECT_SOURCE_DIR}/CodeInspector/FrontEnd/TextResources/CodeInspector.tr
)

set(PROCESSED_SYMBOLS_DATA_FILES
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/BadSymbols.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/BuiltinFunctions.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/Constants.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/ExperimentalSymbols.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/FreeCharacters.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/FreeLongNames.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/ObsoleteSymbols.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/SessionSymbols.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/SpecialCharacters.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/SpecialLongNames.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/SystemCharacters.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/SystemLongNames.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/UndocumentedCharacters.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/UndocumentedLongNames.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/UndocumentedSymbols.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/UnsupportedCharacters.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/UnsupportedLongNames.wl
)

set(STATIC_WL_PACLET_RESOURCES_SOURCES
	${PROJECT_SOURCE_DIR}/CodeInspector/Resources/Examples/Collatz.m
)

set(PACLETINFO_IN_SOURCE
	${PROJECT_SOURCE_DIR}/${PACLET}/PacletInfo.wl.in
)

set(GENERATED_WL_PACLET_FRONTEND_SOURCES
	${PROJECT_BINARY_DIR}/generated/wl/FrontEnd/Palettes/CodeAnalysisOptions.nb
)



#
# Set VERSION_NUMBER, SYSTEMID, and PACLET_VERSION
#
CheckWolframKernel()
CheckPacletInfo()

#
# Force re-configure if PacletInfo.wl.in changes, e.g. paclet version is changed and name of .paclet has changed
#
set_property(
	DIRECTORY
	APPEND
	PROPERTY
		CMAKE_CONFIGURE_DEPENDS
	${PACLETINFO_IN_SOURCE}
)


if(NOT ${VERSION_NUMBER} GREATER_EQUAL 1210)
message(WARNING "VERSION_NUMBER is below 1210; ForceVersionInstall was added in 12.1. Installing paclets via CMake may not work. (VERSION_NUMBER is ${VERSION_NUMBER})")
endif()


file(MAKE_DIRECTORY
	${PROJECT_BINARY_DIR}/paclet/${PACLET}
	${PROJECT_BINARY_DIR}/generated/wl/FrontEnd/Palettes/
)


#
# Copy WL source files
#

set(REPLACED_PACLETINFO ${PROJECT_BINARY_DIR}/paclet/${PACLET}/PacletInfo.wl)

add_custom_command(
	OUTPUT
		${REPLACED_PACLETINFO}
	COMMAND
		${CMAKE_COMMAND} -DSRC=${PACLETINFO_IN_SOURCE} -DCODEPARSER_EXE=${CODEPARSER_EXE} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	COMMAND
		${CMAKE_COMMAND} -E copy ${PACLETINFO_IN_SOURCE} ${REPLACED_PACLETINFO}
	COMMAND
		${CMAKE_COMMAND} -DTRANSPORT=${TRANSPORT} -DBUILDNUMBER=${BUILDNUMBER} -DVERSION_NUMBER=${VERSION_NUMBER} -DWOLFRAMLIBRARY_VERSION=${WOLFRAMLIBRARY_VERSION} -DLOCAL_BUILD=${LOCAL_BUILD} -DLOCAL_BUILD_VERSION=${LOCAL_BUILD_VERSION} -DREPLACED_PACLETINFO=${REPLACED_PACLETINFO} -P ${PROJECT_SOURCE_DIR}/cmake/ReplacePacletInfo.cmake
	DEPENDS
		${PACLETINFO_IN_SOURCE}
		${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
		${PROJECT_SOURCE_DIR}/cmake/ReplacePacletInfo.cmake
)

add_custom_target(replaced-pacletinfo
	ALL
	DEPENDS
		${REPLACED_PACLETINFO}
)


#
# static Kernel sources
#
foreach(SRC ${STATIC_WL_PACLET_KERNEL_SOURCES})
	file(RELATIVE_PATH REL ${PROJECT_SOURCE_DIR}/${PACLET}/Kernel/ ${SRC})
	add_custom_command(
		OUTPUT
			${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${REL}
		COMMAND
			${CMAKE_COMMAND} -DSRC=${SRC} -DCODEPARSER_EXE=${CODEPARSER_EXE} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
		COMMAND
			${CMAKE_COMMAND} -E copy ${SRC} ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${REL}
		DEPENDS
			${SRC}
			${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	)
	list(APPEND COPIED_WL_PACLET_SOURCES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${REL})
endforeach()

#
# static FrontEnd sources
#
foreach(SRC ${STATIC_WL_PACLET_FRONTEND_SOURCES})
	file(RELATIVE_PATH REL ${PROJECT_SOURCE_DIR}/${PACLET}/FrontEnd/ ${SRC})
	add_custom_command(
		OUTPUT
			${PROJECT_BINARY_DIR}/paclet/${PACLET}/FrontEnd/${REL}
		#
		# Do not inspect FrontEnd sources
		#
		COMMAND
			${CMAKE_COMMAND} -E copy ${SRC} ${PROJECT_BINARY_DIR}/paclet/${PACLET}/FrontEnd/${REL}
		DEPENDS
			${SRC}
	)
	list(APPEND COPIED_WL_PACLET_SOURCES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/FrontEnd/${REL})
endforeach()

#
# processed Data sources
#
foreach(SRC ${PROCESSED_SYMBOLS_DATA_FILES})
	file(RELATIVE_PATH REL ${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/ ${SRC})
	add_custom_command(
		OUTPUT
			${PROJECT_BINARY_DIR}/paclet/${PACLET}/Resources/Data/${REL}
		COMMAND
			${CMAKE_COMMAND} -E copy ${SRC} ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Resources/Data/${REL}
		DEPENDS
			${SRC}
	)
	list(APPEND COPIED_WL_PACLET_SOURCES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Resources/Data/${REL})
endforeach()

#
# static Resources sources
#
foreach(SRC ${STATIC_WL_PACLET_RESOURCES_SOURCES})
	file(RELATIVE_PATH REL ${PROJECT_SOURCE_DIR}/${PACLET}/Resources/ ${SRC})
	add_custom_command(
		OUTPUT
			${PROJECT_BINARY_DIR}/paclet/${PACLET}/Resources/${REL}
		#
		# Do not inspect Resources sources
		#
		COMMAND
			${CMAKE_COMMAND} -E copy ${SRC} ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Resources/${REL}
		DEPENDS
			${SRC}
	)
	list(APPEND COPIED_WL_PACLET_SOURCES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Resources/${REL})
endforeach()

#
# generated FrontEnd sources
#
foreach(SRC ${GENERATED_WL_PACLET_FRONTEND_SOURCES})
	file(RELATIVE_PATH REL ${PROJECT_BINARY_DIR}/generated/wl/FrontEnd/ ${SRC})
	add_custom_command(
		OUTPUT
			${PROJECT_BINARY_DIR}/paclet/${PACLET}/FrontEnd/${REL}
		COMMAND
			${CMAKE_COMMAND} -E copy ${SRC} ${PROJECT_BINARY_DIR}/paclet/${PACLET}/FrontEnd/${REL}
		DEPENDS
			${SRC}
	)
	list(APPEND COPIED_WL_PACLET_SOURCES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/FrontEnd/${REL})
endforeach()



#
# generated srcs
#


#
# CodeAnalysisOptions.nb
#
add_custom_command(
	OUTPUT
		${PROJECT_BINARY_DIR}/generated/wl/FrontEnd/Palettes/CodeAnalysisOptions.nb
	COMMAND
		${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/CodeInspector/Generate/MakeCodeAnalysisOptionsPalette.wl -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	DEPENDS
		${PROJECT_SOURCE_DIR}/CodeInspector/Generate/MakeCodeAnalysisOptionsPalette.wl
		${PROJECT_SOURCE_DIR}/CodeTools/Generate/GenerateSources.wl
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	VERBATIM
	WORKING_DIRECTORY
		${PROJECT_SOURCE_DIR}
)


#
# paclet target
#

set(PACLET_SOURCES
	replaced-pacletinfo
	${COPIED_WL_PACLET_SOURCES}
)

if(LOCAL_BUILD)
set(PACLET_OUTPUT ${PROJECT_BINARY_DIR}/paclet/${PACLET}-${LOCAL_BUILD_VERSION}.paclet)
else(LOCAL_BUILD)
set(PACLET_OUTPUT ${PROJECT_BINARY_DIR}/paclet/${PACLET}-${PACLET_VERSION}.paclet)
endif(LOCAL_BUILD)

add_custom_target(paclet
	ALL
	DEPENDS
		${PACLET_OUTPUT}
)

#
# Create the paclet archive
#
add_custom_command(
	OUTPUT
		${PACLET_OUTPUT}
	COMMAND
		#
		# CreatePacletArchive may be slow on RE machines, so allow re-trying if JLink connection timeout is hit
		#
		# see: RE-515885
		#
		${CMAKE_COMMAND} -DRETRY_ON_FAILURE=ON -DSCRIPT=${PROJECT_SOURCE_DIR}/CodeTools/Generate/CreatePacletArchive.wl -DBUILDDIR=${PROJECT_BINARY_DIR} -DPACLET_LAYOUT_DIR=${PACLET_LAYOUT_DIR} -DPACLET=${PACLET} -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	DEPENDS
		${PACLET_SOURCES}
		${PROJECT_SOURCE_DIR}/CodeTools/Generate/CreatePacletArchive.wl
		${PROJECT_SOURCE_DIR}/CodeTools/Generate/GenerateSources.wl
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	VERBATIM
	WORKING_DIRECTORY
		${PROJECT_BINARY_DIR}
)

install(
	CODE
		"execute_process(COMMAND ${CMAKE_COMMAND} -DPACLET_OUTPUT=${PACLET_OUTPUT} -DPACLET_WOLFRAMVERSION=${PACLET_WOLFRAMVERSION} \"-DWOLFRAMKERNEL=${WOLFRAMKERNEL}\" -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InstallPaclet.cmake)"
	COMPONENT
		paclet
)
