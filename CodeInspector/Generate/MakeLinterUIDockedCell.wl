(* ::Section::Closed:: *)
(*Package Header*)


BeginPackage["LinterUIDockedCell`"]


Begin["`Private`"]


Needs["CodeInspector`"]
Needs["WLUtilities`Resources`"]


(* ::Section::Closed:: *)
(*Lou's addDefinitions tools*)

(* ::Text:: *)
(*constructInit[syms] constructs an Initialization setting with the Definition of each of the given symbols. Think of it as an opt-in, non-recursive SaveDefinitions.*)

SetAttributes[constructInit, HoldAllComplete]
constructInit[syms__] := 
	Module[{init},
		init = HoldComplete /@ Unevaluated[{syms}];
		init = Replace[init, HoldComplete[sym_] :>
			ToExpression[ToString[Definition[sym], InputForm], InputForm, HoldComplete], {1}];
		init = Flatten[HoldComplete @@ init, Infinity, HoldComplete];
		Replace[init, HoldComplete[exprs__] :> (Initialization :> {exprs})]
	]


SetAttributes[addDefinitions, HoldAllComplete]


(*  just in case you want to prepend to an existing Initialization *)
addDefinitions[DynamicModule[args__, Initialization :> init_, opts___], {syms__}(*, firstAction:Hold[_]:Hold[Null]*)] :=
	Replace[constructInit[syms], {
		(Initialization :> newinit_) :> DynamicModule[args, Initialization :> ((*First[firstAction]; *)newinit; init), opts],
		else_ :> DynamicModule[args, Initialization :> init, opts]
	}]


(* it's easier if there's not another init setting already *)
addDefinitions[DynamicModule[args__], {syms__Symbol}] :=
	With[{newinit = constructInit[syms]}, DynamicModule[args, newinit]]


(* ::Section::Closed:: *)
(*Docked Cell*)


$previewLength = 25;


dockedCellMenuItem[cell_CellObject] :=
With[{},
	Button[
		(* Display a preview of the cell contents ($previewLength characters), and the severity count icons from the cell bracket button. *)
		DynamicModule[{hoverQ = False},
			DynamicWrapper[
				Graphics[
					{
						{
							Dynamic[If[TrueQ[hoverQ], CodeInspector`LinterUI`Private`colorData["RaftBackHover"], CodeInspector`LinterUI`Private`colorData["PopupBack"]]],
							Rectangle[{-1, -1}, {1, 1}]},
						
						(* Display $previewStringLength characters of the cell contents on the left of the menu item. *)
						Inset[
							With[
								(* Convert the cell box contents into a string, so that it may be clipped to display the preview. *)
								{rawString = ToString[First@Flatten@List@MakeExpression[
										CodeInspector`LinterUI`Private`varValue[cell, "CellContents"][[1, 1]],
										StandardForm]]},
								(* rawString is of the form "HoldComplete[XXXX]". Extract "XXXX". *)
								{expressionString = StringTake[rawString, {14, -2}]},
								(* Clip expressionString to the preview length. *)
								{previewString = StringTake[expressionString, {1, UpTo[$previewLength]}]},
								(* Add an elipsis to the end of the string if it was clipped, and make sure it fits within $previewLength. *)
								CodeInspector`LinterUI`Private`styleData["FixedWidth"][
									If[StringLength[expressionString] > $previewLength,
										StringDrop[previewString, -1] <> "\[Ellipsis]",
										previewString]]],
							
							Offset[{8, 0}, {-1, 0}], {-1, 0}],

						(* On the right of the menu item, display the same issue count icons as used in the cell bracket button. *)
						Inset[
							CodeInspector`LinterUI`Private`lintSeverityCountsIconRow[cell],
							Offset[{-8, 0}, {1, 0}], {1, 0}]
					},

					ImageSize -> {300, 25}, AspectRatio -> Full, PlotRange -> {{-1, 1}, {-1, 1}}, ImagePadding -> None],
				
				hoverQ = CurrentValue["MouseOver"]]],
		
		(* When the menu item is clicked, delete the menu popup cell and scroll the notebook view to the given cell. *)
		NotebookDelete[EvaluationCell[]];
		SelectionMove[cell, After, Cell],

		Appearance -> None]]


dockedCellPopupMenuCell[notebook_NotebookObject, Dynamic[popupPresentQ_]] :=
	With[{paneWidth = Automatic, maxPaneHeight = 200},
		Cell[BoxData @ ToBoxes @
			DynamicModule[{},
			Highlighted[
				Pane[
					Column[
						dockedCellMenuItem /@ CodeInspector`LinterUI`Private`varValue[notebook, All, "Cell"],
						ItemSize -> {0, 0}, Spacings -> 0],
					{paneWidth, UpTo[maxPaneHeight]},
					(* The pane should scroll if its contents exceeds maxPaneHeight. *)
					ImageSizeAction -> "Scrollable", Scrollbars -> {False, Automatic}, AppearanceElements -> None],

				Background -> CodeInspector`LinterUI`Private`colorData["UIBack"], RoundingRadius -> 3,
				Frame -> True, FrameStyle -> Directive[AbsoluteThickness[1], CodeInspector`LinterUI`Private`colorData["UIEdge"]],
				(* Attached cells appear *underneath* docked cells, so align the top of the highlighted pane with the bottom of the docked cell. *)
				FrameMargins -> {{4, 4}, {4, 4}}],

				Initialization :> (popupPresentQ = True),
				Deinitialization :> (popupPresentQ = False)],
			
			CellFrameMargins -> 0]]


dockedCellSeverityCountsButton[notebook_NotebookObject] :=
	DynamicModule[{popupPresentQ = False},
		CodeInspector`LinterUI`Private`button[
			Pane[
				CodeInspector`LinterUI`Private`lintSeverityCountsIconRow[notebook],
				BaselinePosition -> Scaled[-.06]],
			If[!popupPresentQ,
				AttachCell[
					EvaluationCell[],
					dockedCellPopupMenuCell[notebook, Dynamic[popupPresentQ]],
					{Left, Bottom},
					Offset[{0, 1}, Automatic],
					{Left, Top},
					RemovalConditions -> {"MouseExit"}]],
			ImageSize -> {Automatic, 14},
			FrameMargins -> {3{1, 1}, {1, 1}},
			Alignment -> {Center, Baseline}]]


dockedCell =
	Cell[BoxData @ ToBoxes @
		addDefinitions[
			DynamicModule[{notebook, notebookID},
				Graphics[
					{
						(* The left-aligned pod title and analysis-in-progress indicator. *)
						Inset[
							Row[
								{
									Pane[CodeInspector`LinterUI`Private`styleData["SectionHeader"]["Code Analysis"], BaselinePosition -> (Baseline -> Scaled[.65])],
									Spacer[8],
									Pane[
										PaneSelector[
											{
												(* If dockedCellPresentQ isn't True, then the docked cell is left over from a previous session. In which case, display neither a loading indicator nor lint counts. *)
												{False, False} -> Spacer[0],
												{True, False} -> Spacer[0],
												(* Display an activity indicator while analysis is in progress. *)
												{True, True} -> ProgressIndicator[Appearance -> "Percolate"],
												(* Display the agregate severity counts for all cells in the notebook. *)
												{False, True} -> Pane[
													CodeInspector`LinterUI`Private`isolatedDynamic[
														Dynamic[CodeInspector`LinterUI`Private`DynamicTriggers`dockedCellLintCounts],
														dockedCellSeverityCountsButton[notebook]],
													BaselinePosition->Scaled[.5]]
											},
											Dynamic[{CodeInspector`LinterUI`Private`varValue[notebook, "AnalysisInProgressQ"], TrueQ[CodeInspector`LinterUI`Private`varValue[notebook, "DockedCellPresentQ"]]}],
											ImageSize -> Automatic],

										BaselinePosition -> Scaled[.15]]},
								Alignment -> Baseline],

							Offset[{8, 0}, {-1, 0}], {-1, 0}],
						
						Inset[
							PaneSelector[
								{
									True -> CodeInspector`LinterUI`Private`button[
										"Reanalyze Notebook",
										CodeInspector`LinterUI`Private`attachAnalysisAction[EvaluationNotebook[]],
										Method -> "Queued"],

									False -> Button[

										Highlighted[
											Style["Analyze Notebook", FontColor -> GrayLevel[0.2], FontFamily -> "Source Sans Pro", FontWeight -> Plain, FontSize -> 14],
											ImageSize -> {Automatic, 19},
											FrameMargins -> {9{1, 1}, 0{1, 1}},
											BaselinePosition -> Baseline,
											Alignment -> {Center, Center},
											Background -> White,
											Frame -> True,
											FrameStyle -> Dynamic[If[CurrentValue["MouseOver"], Hue[0.55,0.82,0.87], GrayLevel[.8]]]],

										Needs["CodeInspector`"];
										CodeInspector`LinterUI`Private`varSet[{notebook, "DockedCellPresentQ"}, True];
										CodeInspector`AttachAnalysis[notebook],
										Appearance -> False,
										Method -> "Queued"]},

									Dynamic[TrueQ[CodeInspector`LinterUI`Private`varValue[notebook, "DockedCellPresentQ"]]],
									ImageSize -> Automatic],

							Offset[{-26, 0}, {1, 0}], {1, 0}],
						
						(* Draw a "Close" button that clears the entire linter interface from a notebook. *)
						Button[
							Tooltip[
								(* This is the resolved expression from evaluating CodeInspector`LinterUI`Private`closeIcon[{-11, 0}, {1, 0}] *)
								{GrayLevel[0.4], Disk[Offset[{-11, 0}, {1, 0}], Offset[7]], GrayLevel[0.97], AbsoluteThickness[1.5], CapForm["Round"], Line[{{Offset[{-13.5, 2.5}, {1, 0}], Offset[{-8.5, -2.5}, {1, 0}]}, {Offset[{-13.5, -2.5}, {1, 0}], Offset[{-8.5, 2.5}, {1, 0}]}}]},
								"Close analysis", TooltipDelay -> 0],

							(* Delete the lint pods. *)
							NotebookDelete /@ Flatten[CodeInspector`LinterUI`Private`varValue[notebook, All, "UIAttachedCells"]];

							(* Delete docked cells with CellTags -> "AttachedAnalysisDockedCell" *)
							CurrentValue[EvaluationNotebook[], DockedCells] = 
								With[{dockedCells = CurrentValue[EvaluationNotebook[], DockedCells]},
									Pick[
										dockedCells,
										Map[
											Quiet[Options[#, CellTags]] =!= {CellTags -> "CodeAnalysisDockedCell"}&,
											dockedCells]]];

							CodeInspector`LinterUI`Private`applyToVar[Remove, {EvaluationNotebook[], All}];
							CodeInspector`LinterUI`Private`varSet[{notebook, "DockedCellPresentQ"}, True]]
						
					},

					ImageSize -> {Full, 23}, AspectRatio -> Full, PlotRange -> {{-1, 1}, {-1, 1}}],

				Initialization :> (
					notebook = EvaluationNotebook[];
					notebookID = Last[notebook]),

				Deinitialization :> CodeInspector`LinterUI`Private`varSet[{notebook, "DockedCellPresentQ"}, False]
			],
			
			(* Save the following definitions in the DynamicModule's Initialization option. *)
			{dockedCellSeverityCountsButton, dockedCellPopupMenuCell, dockedCellMenuItem, $previewLength,
				CodeInspector`LinterUI`Private`applyToVar,
				CodeInspector`LinterUI`Private`varValue,
				CodeInspector`LinterUI`Private`varSet,
				CodeInspector`LinterUI`Private`varNameString}],

		Background -> GrayLevel[.97],
		(* Draw frame lines at the top and bottom of the cell. *)
		CellFrame -> {{0, 0}, {1, 1}}, CellFrameColor -> GrayLevel[.85],
		CellFrameMargins -> {{0, 0}, {0, 0}},
		CellTags -> "CodeAnalysisDockedCell"];


(* ::Section::Closed:: *)
(*Write the docked cell into CodeInspector.tr*)


trPath = FileNameJoin[{ParentDirectory[NotebookDirectory[]], "FrontEnd", "TextResources", "CodeInspector.tr"}];
WLUtilities`Resources`SetTextResource[trPath, "@@resource CodeInspectorExpressions", "DockedCell" -> dockedCell]


(* ::Section::Closed:: *)
(*Package Footer*)


End[]
EndPackage[]