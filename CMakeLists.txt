
cmake_minimum_required(VERSION 3.0)

project(codeinspector)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})
include(WolframKernel)
include(PacletInfo)

set(PACLET "CodeInspector")
set(WOLFRAMKERNEL ${WOLFRAMKERNEL_DEFAULT} CACHE FILEPATH "Path to WolframKernel")
set(BUILD_DOCS OFF CACHE BOOL "Build documentation")
set(LOCAL_BUILD OFF CACHE BOOL "Local build")

message(STATUS "PACLET: ${PACLET}")
message(STATUS "WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
message(STATUS "BUILD_DOCS: ${BUILD_DOCS}")
message(STATUS "LOCAL_BUILD: ${LOCAL_BUILD}")

set(WL_PACLET_SOURCES
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/AbstractRules.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/AggregateRules.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/Boxes.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/CodeInspector.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/ConcreteRules.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/External.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/Format.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/ImplicitTokens.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/Summarize.wl
	${PROJECT_SOURCE_DIR}/CodeInspector/Kernel/Utils.wl
)

set(WL_GENERATE_CREATEPACLETARCHIVE_SOURCES
	${PROJECT_SOURCE_DIR}/${PACLET}/Generate/CreatePacletArchive.wl
)

set(PACLETINFO_SOURCE
	${PROJECT_SOURCE_DIR}/${PACLET}/PacletInfo.m
)

set(DOCUMENTATION_NOTEBOOK_PATHS
	English/Guides/CodeInspector.nb
	English/ReferencePages/Symbols/CodeInspect.nb
	English/ReferencePages/Symbols/CodeInspectSummarize.nb
	English/ReferencePages/Symbols/InspectedFileObject.nb
	English/ReferencePages/Symbols/InspectedLineObject.nb
	English/ReferencePages/Symbols/InspectedStringObject.nb
	English/ReferencePages/Symbols/InspectionObject.nb
	English/Tutorials/CodeInspectorTutorial.nb
	English/Tutorials/ReleaseNotes.nb
	English/Tutorials/WritingAdditionalRules.nb
)

#
# Setup documentation notebook paths
#
foreach(NB_PATH ${DOCUMENTATION_NOTEBOOK_PATHS})
	set(SRC ${PROJECT_SOURCE_DIR}/${PACLET}/Documentation/${NB_PATH})
	set(BUILT ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Documentation/${NB_PATH})
	list(APPEND DOCUMENTATION_SOURCE ${SRC})
	list(APPEND BUILT_DOCUMENTATION ${BUILT})
endforeach()



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
		${CMAKE_COMMAND} -E copy ${PROJECT_SOURCE_DIR}/${PACLET}/PacletInfo.m ${COPIED_PACLETINFO}
	COMMAND
		${CMAKE_COMMAND} -DLOCAL_BUILD=${LOCAL_BUILD} -DCOPIED_PACLETINFO=${COPIED_PACLETINFO} -P ${PROJECT_SOURCE_DIR}/cmake/ReplacePacletInfo.cmake
	DEPENDS
		${PROJECT_SOURCE_DIR}/${PACLET}/PacletInfo.m
		${PROJECT_SOURCE_DIR}/cmake/ReplacePacletInfo.cmake
)
list(APPEND COPIED_WL_PACLET_SOURCES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/PacletInfo.m)

foreach(SRC ${WL_PACLET_SOURCES})
	get_filename_component(BARE_SRC ${SRC} NAME)
	add_custom_command(
    OUTPUT ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC}
    COMMAND ${CMAKE_COMMAND} -E copy ${SRC} ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC}
    DEPENDS ${SRC})
	list(APPEND COPIED_WL_PACLET_SOURCES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC})
endforeach()

set(PACLET_SOURCES
	${COPIED_WL_PACLET_SOURCES}
	${WL_GENERATE_CREATEPACLETARCHIVE_SOURCES}
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
		ant -DmathExe=${WOLFRAMKERNEL} -DappPath=${DOCS_APPPATH} -Djlinkpath=${DOCS_JLINKPATH} -DinputDir=${PROJECT_SOURCE_DIR}/${PACLET}/Documentation/ -DoutputDir=${PROJECT_BINARY_DIR}/paclet/${PACLET}/Documentation/ -file ${DOCS_APPPATH}/DocumentationBuild/SystemFiles/ant/Build/notebook.xml
	WORKING_DIRECTORY
		${PROJECT_SOURCE_DIR}
)

endif(BUILD_DOCS)






#
# paclet target
#
add_custom_target(paclet
	DEPENDS ${PROJECT_BINARY_DIR}/paclet/${PACLET}-${PACLET_VERSION}.paclet
)

#
# Create the paclet archive
#
add_custom_command(
	OUTPUT
		${PROJECT_BINARY_DIR}/paclet/${PACLET}-${PACLET_VERSION}.paclet
	DEPENDS
		${PACLET_SOURCES}
		${PROJECT_SOURCE_DIR}/${PACLET}/Generate/CreatePacletArchive.wl
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	COMMAND
		${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/${PACLET}/Generate/CreatePacletArchive.wl -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -DPACLET=${PACLET} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	VERBATIM
	WORKING_DIRECTORY
		${PROJECT_SOURCE_DIR}
)



