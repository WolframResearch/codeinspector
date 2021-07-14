(* ::Package:: *)

(* ::Section::Closed:: *)
(*Package Header*)


BeginPackage["LinterUISuppressionPalette`"]


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
(*Tag Suppression Dialog*)


getDisabledTags[scope_?(MatchQ[$FrontEnd | _NotebookObject | _CellObject]), inheritance_:CurrentValue] /; Or[inheritance === CurrentValue, inheritance === AbsoluteCurrentValue] :=
	If[
		(* We want to *only* interrogate tags in the given scope, but if the CodeToolsOptions Association hasn't been defined, then CurrentValue
			resolves the queried option from a higher scope, rather than just returning Inherited. Therefore, we have to check if CodeToolsOptions
			has been defined at the given scope. If not, then we're safe to just return {} for the suppressed/enabled tags. *)
		!MemberQ[Options[scope], CodeAssistOptions -> l1_ /; MemberQ[l1, "CodeToolsOptions" -> _]],
		{},

		With[
			{tagsPath = {CodeAssistOptions, "CodeToolsOptions", "CodeInspect", "Tags"}},
			{rawTagsAssoc = inheritance[scope, tagsPath]},
			
			(* Note on variable names:
				The "Tags" options are structured as either (e.g.)
				<|"ImplicitTimesAcrossLines" -> <|Enabled -> False|>|>
				or (e.g.)
				<|"DuplicateClauses" -> <|"If" -> <|Enabled -> False|>|>|>
				if there are sub-tags to the tags.
				The functions in the following KeyValueMap use variables "tag", "tagOptions", "tagOption" and "value" which correspond to this structure, such that:
				<|tag -> tagOptions|>
				and
				<|tag -> <|tagOption -> value|>|> *)
			
			(* From the raw association of tags and options, we want to find all tags for which Enabled is True or False
				(i.e. explicitly-specced rather than Inherited) and return an list of all such tags, where sub-tags are given by {tag, sub-tag}. *)
			
			{unflattenedTags = Catch @ KeyValueMap[
			
				Function[{tag, tagOptions},
					Catch @ Lookup[
						(* If tagOptions isn't an Association, then Enabled hasn't been specced, so we can throw an empty list. *)
						Replace[tagOptions, Except[_Association] :> Throw[{}]],
						
						(* Look for Enabled in the tag options. *)
						Enabled,
						
						(* If Enabled isn't present, then we can look for option values of the form "SubTag" -> <|___, Enabled -> True/False, ___|> *)
						(* Return {tag, subtag} for subtags. *)
						{tag, #}& /@ Catch @ Keys @ Select[
							tagOptions,
							(* Select options for which Enabled -> True/False (and are therefore subtags). *)
							Catch @ BooleanQ[Lookup[Replace[#, Except[_Association] :> Throw[{}]], Enabled, None]]&],
						
						(* If Enabled is present and if its value is boolean, return the tag. Otherwise discard the tag. *)
						Replace[#, {_?BooleanQ -> tag, _ -> Nothing}]&]],
				
				(* If the "Tags" option value isn't an association, then no tags have been enabled/disabled. So return an empty list. *)
				Replace[rawTagsAssoc, Except[_Association] :> Throw[{}]]]},
			
			(* Flatten out and sort the list of tags by tag then subtag. *)
			SortBy[Flatten[unflattenedTags, 1], Replace[#, l:{__String} :> StringJoin[l]]&]]]


(* Construct the Enabled option path for a tag or subtag. *)
constructTagEnabledPath[tag_?(MatchQ[_String | {_String, _String}])] := {
	CodeAssistOptions, "CodeToolsOptions", "CodeInspect", "Tags",
	Switch[Head[tag], String, tag, List, Splice[tag]],
	Enabled}


disabledTags[notebook_NotebookObject, cell_:None] :=
	Union[
		Function[tag, {tag, $FrontEnd}] /@ getDisabledTags[$FrontEnd],
		Function[tag, {tag, notebook}] /@ getDisabledTags[notebook],
		If[cell === None,
			{},
			Function[tag, {tag, cell}] /@ Flatten[getDisabledTags /@ Flatten[{cell}], 1]]]


togglerPane[] :=
	With[
		{notebook = InputNotebook[]},
		{cell = With[{cells = SelectedCells[notebook]}, Replace[cells, Except[{__CellObject}] -> None]]},
		{tags = disabledTags[notebook, cell]},

		Pane[
			If[tags === {},

				Column[
					{
						Spacer[{1, 100}],
						CodeInspector`LinterUI`Private`styleData["TogglerPaletteHeadings"][
							"No suppressions affecting the selection",
							FontSlant -> Italic, FontSize -> 14]},
						ItemSize -> {Full, 20},
						Spacings -> 0,
						BaseStyle -> {FontSize -> 1}],

				Column[
					{
						Spacer[{1, 10}],
						(* Column headings. *)
						Grid[
							{{
								Spacer[(* 18 *)33],
								Pane[CodeInspector`LinterUI`Private`styleData["TogglerPaletteHeadings"]["Issue", FontColor -> GrayLevel[.45]], ImageSize -> {(* 150 *)151, 17}],
								Pane[CodeInspector`LinterUI`Private`styleData["TogglerPaletteHeadings"]["Scope", FontColor -> GrayLevel[.45]], ImageSize -> {110, 17}]}},
							
							Dividers -> {Center, False},
							FrameStyle -> Directive[AbsoluteThickness[1], CodeInspector`LinterUI`Private`colorData[(* "TogglerDelim" *)"TogglerBack"]],
							Spacings -> {10, 0},
							ItemSize -> Full,
							Alignment -> Left,
							BaseStyle -> {FontSize -> 1}],
						
						(* Rows of suppression controls. *)
						Pane[
							Row[clearSuppressionControl @@@ tags],
							ImageSizeAction -> "Scrollable",
							Scrollbars -> {False, Automatic},
							AppearanceElements -> None,
							Alignment -> {Center, Top},
							ImageSize -> {333, 185},
							FrameMargins -> {{0, 0}, {1, 1}},
							BaseStyle -> {LineIndent -> 0}],
						
						Spacer[{1, 4}],

						DynamicModule[{hoverQ},
							DynamicWrapper[
								Pane[
									CodeInspector`LinterUI`Private`button[
										Row[{togglerClearAllButton[Dynamic[hoverQ]], Spacer[3], Style["Clear All", FontSize -> 13]}],

										CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeInspect", "Tags"}] = Inherited;
										CurrentValue[InputNotebook[], {CodeAssistOptions, "CodeToolsOptions", "CodeInspect", "Tags"}] = Inherited;
										CurrentValue[SelectedCells[InputNotebook[]], {CodeAssistOptions, "CodeToolsOptions", "CodeInspect", "Tags"}] = Inherited;
										CodeInspector`LinterUI`Private`togglerTickle = RandomReal[],

										FrameMargins -> {{5.5, 8}, {2, 0}}],
									FrameMargins -> {{6, 0}, {3, 3}}],
								
								hoverQ = CurrentValue["MouseOver"]]]},

					BaseStyle -> {FontSize -> 1},
					Spacings -> {0, {0, 0, 4, {0}}},
					ItemSize -> {0, 0}, Spacings -> 0,
					Dividers -> {None, {3 -> GrayLevel[.8]}}]],

			ImageSize -> {335, 250},
			Alignment -> {Center, Top}]]


togglerClearAllButton[Dynamic[hoverQ_]] :=
	Graphics[
		{
			CapForm["Round"], AbsoluteThickness[1.5],
			Dynamic[If[hoverQ, CodeInspector`LinterUI`Private`colorData["TogglerEdgeHover"], CodeInspector`LinterUI`Private`colorData["TogglerCross"]]],
			Line[{{{-1, -1}, {1, 1}}, {{-1, 1}, {1, -1}}}]},
		ImageSize -> 15{1, 1}, PlotRangePadding -> 1.75, PlotRange -> 1, BaselinePosition -> Scaled[.2]]


clearSuppressionControl[tag_?(MatchQ[_String | {_String, _String}]), scope_?(MatchQ[_FrontEndObject | _NotebookObject | _CellObject | {___CellObject}])] :=
	With[
		{
			(* The tag name. For subtags, insert a triangle between the tag and subtag. *)
			tagText = Replace[tag, l_List :> StringJoin[Insert[l, "\[VeryThinSpace]\:25bb\[VeryThinSpace]", 2]]],

			(* The scope. *)
			scopeText = Switch[scope,
				_FrontEndObject,
				"All Notebooks",
				_NotebookObject,
				FE`Evaluate[
					FEPrivate`TruncateStringToWidth[
						AbsoluteCurrentValue[scope, WindowTitle],
						"DialogStyle", 105]],
				_CellObject,
				"Selected Cell",
				_,
				"Selected Cells"]
		},
		
		DynamicModule[
			{clearedQ = False, hoverQ = False},
			PaneSelector[
				{
					False -> Button[
						DynamicWrapper[
							Highlighted[
								Grid[
									{{
										(* Display the remove icon (a cross, which changes colour on mouseover to match the control's frame). *)
										togglerClearAllButton[Dynamic[hoverQ]],

										(* Display the tag name. *)
										Pane[CodeInspector`LinterUI`Private`styleData["TogglerTagText"][tagText], ImageSize -> {150, 20}, BaselinePosition -> Baseline, Alignment -> {Left, Center}],

										(* Display the scope. *)
										Pane[scopeText, ImageSize -> {110, 20}, BaselinePosition -> Baseline, Alignment -> {Left, Center}]
									}},
								
									ItemSize -> Full,
									Alignment -> {Left, Baseline},
									Dividers -> {Center, False},
									FrameStyle -> Directive[AbsoluteThickness[1], CodeInspector`LinterUI`Private`colorData["TogglerDelim"]],
									BaseStyle -> {FontFamily -> "Source Sans Pro", FontSize -> 13, FontColor -> CodeInspector`LinterUI`Private`colorData["TogglerText"]}],
								
								Background -> Dynamic[If[hoverQ, CodeInspector`LinterUI`Private`colorData["TogglerBackHover"], CodeInspector`LinterUI`Private`colorData["TogglerBack"]]],
								RoundingRadius -> Dynamic[If[hoverQ, 3, 0]],
								Frame -> True, FrameMargins -> {{2, 2}, {0, 0}},
								FrameStyle -> Dynamic[If[hoverQ, CodeInspector`LinterUI`Private`colorData["TogglerEdgeHover"], CodeInspector`LinterUI`Private`colorData["TogglerEdge"]]]],

							hoverQ = CurrentValue["MouseOver"]],
						
						(* Button action. Clear the suppression, and hide the control by setting clearedQ to True. *)
						CurrentValue[scope, constructTagEnabledPath[tag]] = Inherited;
						clearedQ = True;
						CodeInspector`LinterUI`Private`togglerTickle = RandomReal[],
						
						Appearance -> None,
						Tooltip -> "Clear suppression",
						TooltipDelay -> 0],


					True -> Pane[Spacer[{0,0}], ImageSize -> {0, 0}, ImageMargins -> {{0, 0}, {-1, 0}}]},

				Dynamic[clearedQ]]]]


togglerPalette =
	CreatePalette[
		addDefinitions[
			DynamicModule[{},
				Dynamic[
					CodeInspector`LinterUI`Private`togglerTickle;
					With[{nb = InputNotebook[]}, AbsoluteCurrentValue[nb, "SelectionHasUpdatedStyles"]];
					Dynamic[togglerPane[], SingleEvaluation -> True]]],
			{
				getDisabledTags,
				constructTagEnabledPath,
				disabledTags,
				togglerPane,
				togglerClearAllButton,
				clearSuppressionControl,
				CodeInspector`LinterUI`Private`colorData,
				CodeInspector`LinterUI`Private`styleData,
				CodeInspector`LinterUI`Private`button
			}],

		Background -> GrayLevel[.975],
		WindowTitle -> "Code Analysis Suppressions",
		Saveable -> False,
		Evaluator -> "System"];

 


(* ::Section::Closed:: *)
(*Write*)


NotebookSave[togglerPalette,
	FileNameJoin[{ParentDirectory[NotebookDirectory[]], "FrontEnd", "Palettes", "CodeAnalysisSuppressions.nb"}]]


(* ::Section::Closed:: *)
(*Package Footer*)


End[]
EndPackage[]
