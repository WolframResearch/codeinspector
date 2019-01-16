
cmake_minimum_required(VERSION 3.0)

project(lint)

set(PACLET "Lint")

message(STATUS "PACLET: ${PACLET}")

set(PACLET_VERSION "0.8" CACHE STRING "Paclet Version")

message(STATUS "PACLET_VERSION: ${PACLET_VERSION}")

set(WOLFRAMKERNEL /Applications/Mathematica.app/Contents/MacOS/WolframKernel CACHE FILEPATH "Path to WolframKernel")

message(STATUS "WOLFRAMKERNEL: ${WOLFRAMKERNEL}")

set(BUILD_DOCS OFF CACHE BOOL "Build documentation")

message(STATUS "BUILD_DOCS: ${BUILD_DOCS}")

set(WL_SOURCES
	${CMAKE_SOURCE_DIR}/Lint/Format.wl
	${CMAKE_SOURCE_DIR}/Lint/ImplicitTimes.wl
	${CMAKE_SOURCE_DIR}/Lint/Lint.wl
	${CMAKE_SOURCE_DIR}/Lint/Report.wl
	${CMAKE_SOURCE_DIR}/Lint/Rules.wl
	${CMAKE_SOURCE_DIR}/Lint/Utils.wl
)

set(PACLETINFO_SOURCE
	${CMAKE_SOURCE_DIR}/Lint/PacletInfo.m
)

set(DOCUMENTATION_NOTEBOOKS
	${CMAKE_SOURCE_DIR}/Lint/Documentation/English/ReferencePages/Symbols/LintFile.nb
	${CMAKE_SOURCE_DIR}/Lint/Documentation/English/ReferencePages/Symbols/LintString.nb
	${CMAKE_SOURCE_DIR}/Lint/Documentation/English/ReferencePages/Symbols/LintFileReport.nb
	${CMAKE_SOURCE_DIR}/Lint/Documentation/English/ReferencePages/Symbols/LintStringReport.nb
	${CMAKE_SOURCE_DIR}/Lint/Documentation/English/Tutorials/LintTutorial.nb
)

set(BUILT_PACLETINFO
	${CMAKE_BINARY_DIR}/paclet/Lint/PacletInfo.m
)

set(BUILT_DOCUMENTATION
	${CMAKE_BINARY_DIR}/paclet/Lint/Documentation/English/Index/_0.cfs
	${CMAKE_BINARY_DIR}/paclet/Lint/Documentation/English/Index/segments.gen
	${CMAKE_BINARY_DIR}/paclet/Lint/Documentation/English/Index/segments_3
	${CMAKE_BINARY_DIR}/paclet/Lint/Documentation/English/ReferencePages/Symbols/LintFile.nb
	${CMAKE_BINARY_DIR}/paclet/Lint/Documentation/English/ReferencePages/Symbols/LintString.nb
	${CMAKE_BINARY_DIR}/paclet/Lint/Documentation/English/ReferencePages/Symbols/LintFileReport.nb
	${CMAKE_BINARY_DIR}/paclet/Lint/Documentation/English/ReferencePages/Symbols/LintStringReport.nb
	${CMAKE_BINARY_DIR}/paclet/Lint/Documentation/English/SpellIndex/_0.cfs
	${CMAKE_BINARY_DIR}/paclet/Lint/Documentation/English/SpellIndex/segments.gen
	${CMAKE_BINARY_DIR}/paclet/Lint/Documentation/English/SpellIndex/segments_3
	${CMAKE_BINARY_DIR}/paclet/Lint/Documentation/English/Tutorials/LintTutorial.nb
)








file(MAKE_DIRECTORY
	${CMAKE_BINARY_DIR}/paclet/${PACLET}
)


#
# Configure WL source files
#
# Use configure_file to create a file-level dependency between input and output
#
foreach(SRC ${WL_SOURCES})
	get_filename_component(BARE_SRC ${SRC} NAME)
	set(CONFIGURED_SRC ${CMAKE_BINARY_DIR}/paclet/${PACLET}/${BARE_SRC})
	configure_file(${SRC}
		${CONFIGURED_SRC}
	)
	list(APPEND CONFIGURED_WL_SOURCES ${CONFIGURED_SRC})
endforeach()

set(PACLET_SOURCES
	# Pack script
	${CMAKE_SOURCE_DIR}/scripts/Pack.wl
	${CONFIGURED_WL_SOURCES}
	${BUILT_PACLETINFO}
)

if(BUILD_DOCS)

list(APPEND PACLET_SOURCES
	${BUILT_DOCUMENTATION}
)

endif(BUILD_DOCS)





#
# Build PacletInfo.m
#
# Run the BuildPacletInfo.wl script to build a PacletInfo.m appropriate for including in the paclet
#
add_custom_command(
	OUTPUT
		${BUILT_PACLETINFO}
	DEPENDS
		${CMAKE_SOURCE_DIR}/scripts/BuildPacletInfo.wl
		${PACLETINFO_SOURCE}
	COMMAND ${WOLFRAMKERNEL} -noinit -script ${CMAKE_SOURCE_DIR}/scripts/BuildPacletInfo.wl -source ${PACLETINFO_SOURCE} -built ${BUILT_PACLETINFO} -pacletVersion "${PACLET_VERSION}"
	WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)





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

set(DOCS_JLINKPATH /Applications/Mathematica.app/Contents/SystemFiles/Links/JLink/ CACHE PATH "Path of JLink")

message(STATUS "DOCS_JLINKPATH: ${DOCS_JLINKPATH}")


#
# build the docs
#
add_custom_command(
	OUTPUT
		${BUILT_DOCUMENTATION}
	DEPENDS
		${DOCUMENTATION_NOTEBOOKS}
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
		${WOLFRAMKERNEL} -noinit -script ${CMAKE_SOURCE_DIR}/scripts/Pack.wl -pacletDir ${CMAKE_BINARY_DIR}/paclet/${PACLET}
	WORKING_DIRECTORY
		${CMAKE_SOURCE_DIR}
)



