
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

if(NOT DEFINED BUILDNUMBER)
set(BUILDNUMBER 0)
endif()
message(STATUS "BUILDNUMBER: ${BUILDNUMBER}")
message(STATUS "CMAKE_VERSION: ${CMAKE_VERSION}")
message(STATUS "PACLET: ${PACLET}")
message(STATUS "WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
message(STATUS "LOCAL_BUILD: ${LOCAL_BUILD}")
if(LOCAL_BUILD)
message(STATUS "Configuring for local build")
endif()
# message(STATUS "CMAKE_SIZEOF_VOID_P: ${CMAKE_SIZEOF_VOID_P}")
message(STATUS "BUG349779_PAUSE: ${BUG349779_PAUSE}")
message(STATUS "BUG349779_TIMEOUT: ${BUG349779_TIMEOUT}")
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
	${PROJECT_SOURCE_DIR}/CodeInspector/FrontEnd/Palettes/CodeAnalysisOptions.nb
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


#
# ForceVersionInstall was added in 12.1
#
if(NOT ${VERSION_NUMBER} GREATER_EQUAL 1210)
message(WARNING "VERSION_NUMBER is below 1210; installing paclets via CMake may not work.")
endif()


file(MAKE_DIRECTORY
	${PROJECT_BINARY_DIR}/paclet/${PACLET}
)


#
# Copy WL source files
#

set(COPIED_PACLETINFO ${PROJECT_BINARY_DIR}/paclet/${PACLET}/PacletInfo.wl)

add_custom_command(
	OUTPUT
		${COPIED_PACLETINFO}
	COMMAND
		${CMAKE_COMMAND} -DSRC=${PACLETINFO_IN_SOURCE} -DCODEPARSER_EXE=${CODEPARSER_EXE} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	COMMAND
		${CMAKE_COMMAND} -E copy ${PACLETINFO_IN_SOURCE} ${COPIED_PACLETINFO}
	DEPENDS
		${PACLETINFO_IN_SOURCE}
		${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
)
list(APPEND COPIED_WL_PACLET_SOURCES ${COPIED_PACLETINFO})

#
# static Kernel sources
#
foreach(SRC ${STATIC_WL_PACLET_KERNEL_SOURCES})
	file(RELATIVE_PATH REL ${PROJECT_SOURCE_DIR}/${PACLET}/Kernel/ ${SRC})
	add_custom_command(
		OUTPUT
			${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${REL}
		COMMAND
			${CMAKE_COMMAND} -DSRC=${SRC} -DCODEPARSER_EXE=${CODEPARSER_EXE} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
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
# paclet target
#

set(PACLET_SOURCES
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
		${CMAKE_COMMAND} -DBUILDNUMBER=${BUILDNUMBER} -DLOCAL_BUILD=${LOCAL_BUILD} -DLOCAL_BUILD_VERSION=${LOCAL_BUILD_VERSION} -DCOPIED_PACLETINFO=${COPIED_PACLETINFO} -P ${PROJECT_SOURCE_DIR}/cmake/ReplacePacletInfo.cmake
	COMMAND
		${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/CodeTools/Generate/CreatePacletArchive.wl -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -DPACLET=${PACLET} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	DEPENDS
		${PACLET_SOURCES}
		${PROJECT_SOURCE_DIR}/CodeTools/Generate/CreatePacletArchive.wl
		${PROJECT_SOURCE_DIR}/CodeTools/Generate/GenerateSources.wl
		${PROJECT_SOURCE_DIR}/cmake/ReplacePacletInfo.cmake
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	VERBATIM
	WORKING_DIRECTORY
		${PROJECT_SOURCE_DIR}
)

install(
	CODE
		"execute_process(COMMAND ${CMAKE_COMMAND} -DPACLET_OUTPUT=${PACLET_OUTPUT} \"-DWOLFRAMKERNEL=${WOLFRAMKERNEL}\" -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InstallPaclet.cmake)"
	COMPONENT
		paclet
)
