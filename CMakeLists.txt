
cmake_minimum_required(VERSION 3.0)

project(lint)

set(PACLET "Lint")

set(PACLET_VERSION "0.5" CACHE STRING "Paclet Version")

set(WOLFRAMKERNEL /Applications/Mathematica.app/Contents/MacOS/WolframKernel CACHE FILEPATH "Path to WolframKernel")

set(BUILD_DOCS OFF CACHE BOOL "Build documentation")

set(WL_SOURCES
	${CMAKE_SOURCE_DIR}/PacletInfo.m
	${CMAKE_SOURCE_DIR}/Lint/ImplicitMultiplication.wl
	${CMAKE_SOURCE_DIR}/Lint/Lint.wl
	${CMAKE_SOURCE_DIR}/Lint/Run.wl
)

set(DOCUMENTATION_NOTEBOOKS
	${CMAKE_SOURCE_DIR}/Lint/Documentation/English/Tutorials/LintExamples.nb
)

set(BUILT_DOCUMENTATION_NOTEBOOKS
	${CMAKE_BINARY_DIR}/paclet/${PACLET}/Documentation/English/Tutorials/LintExamples.nb
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
)

if(BUILD_DOCS)

list(APPEND PACLET_SOURCES
	${BUILT_DOCUMENTATION_NOTEBOOKS}
)

endif(BUILD_DOCS)








#
# docs target
#

add_custom_target(docs
	DEPENDS
		${BUILT_DOCUMENTATION_NOTEBOOKS}
)

set(DOCS_APPPATH /Applications/Eclipse.app/Contents/Eclipse/configuration/org.eclipse.osgi/525/0/.cp/MathematicaSource/ CACHE PATH "Path of MathematicaSource inside Workbench installation")
set(DOCS_JLINKPATH /Applications/Mathematica.app/Contents/SystemFiles/Links/JLink/ CACHE PATH "Path of JLink")

#
# build the docs
#
add_custom_command(
	OUTPUT
		${BUILT_DOCUMENTATION_NOTEBOOKS}
	DEPENDS
		${DOCUMENTATION_NOTEBOOKS}
	COMMAND
		ant -DmathExe=${WOLFRAMKERNEL} -DappPath=${DOCS_APPPATH} -Djlinkpath=${DOCS_JLINKPATH} -DinputDir=${CMAKE_SOURCE_DIR}/Lint/Documentation/ -DoutputDir=${CMAKE_BINARY_DIR}/paclet/${PACLET}/Documentation/ -file ${DOCS_APPPATH}/DocumentationBuild/SystemFiles/ant/Build/notebook.xml
	WORKING_DIRECTORY
		${CMAKE_SOURCE_DIR}
)






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



