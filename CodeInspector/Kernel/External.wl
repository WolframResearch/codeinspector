BeginPackage["CodeInspector`External`"]

OpenInEditor


$Editor


Begin["`Private`"]



OpenInEditor::fail = "Cannot open file in editor: `1` (`2`)"


(*

For "Visual Studio Code":

As a 1-time setup, you must run "Shell Command: Install 'code' command in PATH" in the Command Palette of VSCode

https://code.visualstudio.com/docs/setup/mac#_launching-from-the-command-line


For "Visual Studio Code", the better command would be just "code"

But if Mathematica is launched from Finder/LaunchServices, then PATH does not contain /usr/local/bin

NOTE: launching Mathematica from Terminal would allow /usr/local/bin to be added to PATH

Related links: https://mathematica.stackexchange.com/a/99708/63281


Running without the shell is bad also, because it means that other commands that depend on the shell are not run

e.g., specifying "python3" in the settings for Visual Studio Code may not work, and the full path to python3 may be
needed


*)


Options[OpenInEditor] = {
  "Editor" -> Automatic
}


OpenInEditor[file_String, OptionsPattern[]] :=
Module[{editor, res},

  editor = OptionValue["Editor"];
  If[editor === Automatic,
    editor = $Editor
  ];

  Switch[{$InterfaceEnvironment, editor},
    {"Macintosh", "Sublime Text"},
      (*
      Run needs spaces escaped
      *)
      Run[escapeSpaces["/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl"] <> " " <> file]
    ,
    {"Macintosh", "Visual Studio Code"},
      (*
      Run needs spaces escaped
      *)
      Run[escapeSpaces["/usr/local/bin/code"] <> " -g " <> file]
    ,
    {_, "SystemOpen"},
      (*
      SystemOpen does NOT need spaces escaped
      *)
      res = SystemOpen[file];
      If[FailureQ[res],
        Message[OpenInEditor::fail, file, "SystemOpen"]
      ];
    ,
    _,
      (*
      Editor "FrontEnd" is supported in all environments.

      NotebookOpen does NOT need spaces escaped
      *)
      res = NotebookOpen[file];
      If[FailureQ[res],
        Message[OpenInEditor::fail, file, "FrontEnd"]
      ];
  ]
]


OpenInEditor[file_String, line_Integer, OptionsPattern[]] :=
Module[{editor, res},

  editor = OptionValue["Editor"];
  If[editor === Automatic,
    editor = $Editor
  ];

  Switch[{$InterfaceEnvironment, editor},
    {"Macintosh", "Sublime Text"},
      (*
      Run needs spaces escaped
      *)
      Run[escapeSpaces["/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl"] <> " " <> file <> ":" <> ToString[line]]
    ,
    {"Macintosh", "Visual Studio Code"},
      (*
      Run needs spaces escaped
      *)
      Run[escapeSpaces["/usr/local/bin/code"] <> " -g " <> file <> ":" <> ToString[line]]
    ,
    {_, "SystemOpen"},
      (*
      SystemOpen does NOT need spaces escaped
      *)
      res = SystemOpen[file];
      If[FailureQ[res],
        Message[OpenInEditor::fail, file, "SystemOpen"]
      ];
    ,
    _,
      (*
      Editor "FrontEnd" is supported in all environments.

      NotebookOpen does NOT need spaces escaped
      *)
      res = NotebookOpen[file];
      If[FailureQ[res],
        Message[OpenInEditor::fail, file, "FrontEnd"]
      ];
  ]
]


OpenInEditor[file_String, line_Integer, col_Integer, OptionsPattern[]] :=
Module[{editor, res},

  editor = OptionValue["Editor"];
  If[editor === Automatic,
    editor = $Editor
  ];

  Switch[{$InterfaceEnvironment, editor},
    {"Macintosh", "Sublime Text"},
      (*
      Run needs spaces escaped
      *)
      Run[escapeSpaces["/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl"] <> " " <> file <> ":" <> ToString[line] <> ":" <> ToString[col] <> ""]
    ,
    {"Macintosh", "Visual Studio Code"},
      (*
      Run needs spaces escaped
      *)
      Run[escapeSpaces["/usr/local/bin/code"] <> " -g " <> file <> ":" <> ToString[line] <> ":" <> ToString[col] <> ""]
    ,
    {_, "SystemOpen"},
      (*
      SystemOpen does NOT need spaces escaped
      *)
      res = SystemOpen[file];
      If[FailureQ[res],
        Message[OpenInEditor::fail, file, "SystemOpen"]
      ];
    ,
    _,
      (*
      Editor "FrontEnd" is supported in all environments.

      NotebookOpen does NOT need spaces escaped
      *)
      res = NotebookOpen[file];
      If[FailureQ[res],
        Message[OpenInEditor::fail, file, "FrontEnd"]
      ];
  ]
]


(*
assume that path does not already have escaped spaces
*)
escapeSpaces[path_] := StringReplace[path, " " -> "\\ "]


End[]

EndPackage[]
