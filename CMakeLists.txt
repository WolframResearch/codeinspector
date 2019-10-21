
cmake_minimum_required(VERSION 3.0)

project(lint)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})

set(PACLET "Lint")
set(WOLFRAMKERNEL /Applications/Mathematica.app/Contents/MacOS/WolframKernel CACHE FILEPATH "Path to WolframKernel")
set(BUILD_DOCS OFF CACHE BOOL "Build documentation")

message(STATUS "PACLET: ${PACLET}")
message(STATUS "WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
message(STATUS "BUILD_DOCS: ${BUILD_DOCS}")

set(WL_PACLET_SOURCES
	${CMAKE_SOURCE_DIR}/Lint/Kernel/AbstractRules.wl
	${CMAKE_SOURCE_DIR}/Lint/Kernel/AggregateRules.wl
	${CMAKE_SOURCE_DIR}/Lint/Kernel/External.wl
	${CMAKE_SOURCE_DIR}/Lint/Kernel/Folds.wl
	${CMAKE_SOURCE_DIR}/Lint/Kernel/Format.wl
	${CMAKE_SOURCE_DIR}/Lint/Kernel/ImplicitTimes.wl
	${CMAKE_SOURCE_DIR}/Lint/Kernel/Lint.wl
	${CMAKE_SOURCE_DIR}/Lint/Kernel/Report.wl
	${CMAKE_SOURCE_DIR}/Lint/Kernel/Utils.wl
)

set(PACLETINFO_SOURCE
	${CMAKE_SOURCE_DIR}/Lint/PacletInfo.m
)

set(DOCUMENTATION_NOTEBOOK_PATHS
	English/ReferencePages/Symbols/LintFile.nb
	English/ReferencePages/Symbols/LintString.nb
	English/ReferencePages/Symbols/LintFileReport.nb
	English/ReferencePages/Symbols/LintStringReport.nb
	English/Tutorials/LintTutorial.nb
)

#
# Setup documentation notebook paths
#
foreach(NB_PATH ${DOCUMENTATION_NOTEBOOK_PATHS})
	set(SRC ${CMAKE_SOURCE_DIR}/${PACLET}/Documentation/${NB_PATH})
	set(BUILT ${CMAKE_BINARY_DIR}/paclet/${PACLET}/Documentation/${NB_PATH})
	list(APPEND DOCUMENTATION_SOURCE ${SRC})
	list(APPEND BUILT_DOCUMENTATION ${BUILT})
endforeach()



#
# Set VERSION_NUMBER, SYSTEMID, and PACLET_VERSION
#
include(WolframKernel)
CheckWolframKernel()



file(MAKE_DIRECTORY
	${CMAKE_BINARY_DIR}/paclet/${PACLET}
)


#
# Copy WL source files
#
add_custom_command(
    OUTPUT ${CMAKE_BINARY_DIR}/paclet/${PACLET}/PacletInfo.m
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/${PACLET}/PacletInfo.m ${CMAKE_BINARY_DIR}/paclet/${PACLET}/PacletInfo.m
    MAIN_DEPENDENCY ${CMAKE_SOURCE_DIR}/${PACLET}/PacletInfo.m)
list(APPEND COPIED_WL_PACLET_SOURCES ${CMAKE_BINARY_DIR}/paclet/${PACLET}/PacletInfo.m)

foreach(SRC ${WL_PACLET_SOURCES})
	get_filename_component(BARE_SRC ${SRC} NAME)
	add_custom_command(
    OUTPUT ${CMAKE_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC}
    COMMAND ${CMAKE_COMMAND} -E copy ${SRC} ${CMAKE_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC}
    MAIN_DEPENDENCY ${SRC})
	list(APPEND COPIED_WL_PACLET_SOURCES ${CMAKE_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC})
endforeach()

set(PACLET_SOURCES
	${COPIED_WL_PACLET_SOURCES}
)




if(BUILD_DOCS)

list(APPEND PACLET_SOURCES
	${BUILT_DOCUMENTATION}
)

endif(BUILD_DOCS)





#
# docs target
#

if(BUILD_DOCS)

add_custom_target(docs
	DEPENDS
		${BUILT_DOCUMENTATION}
)

set(DOCS_APPPATH /Applications/Eclipse.app/Contents/Eclipse/configuration/org.eclipse.osgi/525/0/.cp/MathematicaSource/ CACHE PATH "Path of MathematicaSource inside Workbench installation")

message(STATUS "DOCS_APPPATH: ${DOCS_APPPATH}")

if (NOT IS_DIRECTORY ${DOCS_APPPATH})
message(FATAL_ERROR "DOCS_APPPATH (${DOCS_APPPATH}) does not exist")
endif(NOT IS_DIRECTORY ${DOCS_APPPATH})

set(DOCS_JLINKPATH /Applications/Mathematica.app/Contents/SystemFiles/Links/JLink/ CACHE PATH "Path of JLink")

message(STATUS "DOCS_JLINKPATH: ${DOCS_JLINKPATH}")

if (NOT IS_DIRECTORY ${DOCS_JLINKPATH})
message(FATAL_ERROR "DOCS_JLINKPATH (${DOCS_JLINKPATH}) does not exist")
endif(NOT IS_DIRECTORY ${DOCS_JLINKPATH})


#
# build the docs
#
add_custom_command(
	OUTPUT
		${BUILT_DOCUMENTATION}
	DEPENDS
		${DOCUMENTATION_SOURCE}
	COMMAND
		ant -DmathExe=${WOLFRAMKERNEL} -DappPath=${DOCS_APPPATH} -Djlinkpath=${DOCS_JLINKPATH} -DinputDir=${CMAKE_SOURCE_DIR}/${PACLET}/Documentation/ -DoutputDir=${CMAKE_BINARY_DIR}/paclet/${PACLET}/Documentation/ -file ${DOCS_APPPATH}/DocumentationBuild/SystemFiles/ant/Build/notebook.xml
	WORKING_DIRECTORY
		${CMAKE_SOURCE_DIR}
)

endif(BUILD_DOCS)






#
# paclet target
#
add_custom_target(paclet
	DEPENDS ${CMAKE_BINARY_DIR}/paclet/${PACLET}-${PACLET_VERSION}.paclet
)

#
# Pack the paclet
#
add_custom_command(
	OUTPUT
		${CMAKE_BINARY_DIR}/paclet/${PACLET}-${PACLET_VERSION}.paclet
	DEPENDS
		${PACLET_SOURCES}
	COMMAND
		${WOLFRAMKERNEL} -noinit -noprompt -run PackPaclet["${CMAKE_BINARY_DIR}/paclet/${PACLET}"]\;Exit[]
	VERBATIM
	WORKING_DIRECTORY
		${CMAKE_SOURCE_DIR}
)



