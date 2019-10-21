BeginPackage["Lint`External`"]

OpenInEditor


$Editor

Begin["`Private`"]


(*

For "Visual Studio Code", the better command would be just "code"

But I cannot figure out how to have the command run in the shell, so that "code" is found.


Running without the shell is bad also, because it means that other commands that depend on the shell are not run

e.g., specifying "python3" in the settings for Visual Studio Code may not work, and the full path to python3 may be
needed


*)


OpenInEditor[file_String, line_Integer, col_Integer] :=
Module[{},
	Switch[$InterfaceEnvironment,
		"Macintosh",
			Switch[$Editor,
				"Sublime Text",
					Run["/Applications/Sublime\\ Text.app/Contents/SharedSupport/bin/subl " <> file <> ":" <> ToString[line] <> ":" <> ToString[col] <> ""]
				,
				"Visual Studio Code",
					Run["/usr/local/bin/code -g " <> file <> ":" <> ToString[line] <> ":" <> ToString[col] <> ""]
			]
	]
]



End[]

EndPackage[]
