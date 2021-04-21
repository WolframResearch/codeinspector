(* ::Package:: *)

(* ::Section::Closed:: *)
(*PackageHeader*)


BeginPackage["CodeInspector`LinterUI`"]


AttachAnalysis::usage = "AttachAnalysis[] attaches code analysis pods to the \"Input\" and \"Code\" cells in the evaluation notebook which contain issues.
AttachAnalysis[notebook] attaches code analysis pods to the \"Input\" and \"Code\" cells in notebook which contain issues.
AttachAnalysis[{cell1, cell2, ...}] attaches code analysis pods to the \"Input\" and \"Code\" cells in the list of cells.";


Begin["`Private`"]


Needs["CodeParser`"]
Needs["CodeInspector`"]


(* ::Section::Closed:: *)
(*Appearance Elements*)


$UIRoundingRadius = 4;


colour = With[
	{
		errorSev3 = RGBColor[0.9400000000000001, 0.64, 0],
		errorSev2 = RGBColor[1, 0.45, 0],
		errorSev1 = RGBColor[0.827451, 0.00392157, 0.00392157]
	},
	
	<|
		(* General. *)
		"UIBack" -> GrayLevel[.97],
		"UIEdge" -> GrayLevel[.85],
		"CodeBack" -> RGBColor[0.99, 1, 1],
		"UIDark" -> (*RGBColor[0.53, 0.34, 0]*)GrayLevel[.4],
		"WarningText" -> RGBColor[0.89, 0.14, 0.05],
		"PopupEdge" -> GrayLevel[0.75],
		"PopupBack" -> GrayLevel[0.97],
		"Delimiter" -> GrayLevel[.85],
		"CellBracketMarker" -> RGBColor[0.968627, 0.619608, 0.117647],
		
		(* Button colours. *)
		"ButtonBack" -> White,
		"ButtonBackHover" -> White,
		"ButtonBackMouseDown" -> Hue[0.55, 0.33, 1],
		"ButtonBackInactive" -> White,
		"ButtonEdge" -> GrayLevel[.8],
		"ButtonEdgeHover" -> Hue[0.55,0.82,0.87],
		"ButtonEdgeInactive" -> GrayLevel[.85],
		"ButtonText" -> GrayLevel[0.2],
		"ButtonTextHover" -> GrayLevel[0.2],
		"ButtonTextInactive" -> GrayLevel[0.7],
		"ApplyButtonText" -> White,
		"ApplyButtonBack" -> Red,
		"ApplyButtonBackHover" -> RGBColor[0.854902, 0, 0],
		"HashButtonBack" -> White,
		"HashButtonBackHover" -> Hue[0.1, 0.26, 1],
		"HashButtonEdge" -> RGBColor[Rational[81, 85], 0.79, 0.37],
		
		(* Raft colours. *)
		"RaftBack" -> GrayLevel[1],
		"RaftItemHighlight" -> RGBColor[0.96, 0.97, 0.97],
		"RaftBackHover" -> RGBColor[0.94, 0.95, 0.96],
		"RaftBackOpen" -> RGBColor[0.96, 0.97, 0.97],
		"RaftFrame" -> RGBColor["#C1D3E1"],
		"RaftLabel" -> GrayLevel[0.2],
		"RaftMenuItem" -> GrayLevel[0.2],
		"RaftDelimiter" -> GrayLevel[0.9],
		"CodeHighlight" -> RGBColor[1, 0.67, 0.73],
		
		(* Error types. *)
		"Formatting" -> errorSev3,
		"Remark" -> errorSev3,
		"ImplicitTimes" -> errorSev3,
		"Scoping" -> errorSev3,
		"Warning" -> errorSev2,
		"Error" -> errorSev1,
		"Fatal" -> errorSev1
	|>];


style = <|
	
	"SectionHeader" -> Function[
		Style[#1, ##2, FontColor -> colour["UIDark"], FontFamily -> "Source Sans Pro", FontWeight -> Plain, FontSize -> 13]],
		
	"RaftLabel" -> Function[
		Style[Row[CodeInspector`Utils`boldify[#1]], ##2, FontColor -> colour["RaftLabel"], FontFamily -> "Source Sans Pro", FontWeight -> Plain, FontSize -> 13]],
	
	"RaftMenuItem" -> Function[
		Style[#1, ##2, FontColor -> colour["RaftMenuItem"], FontFamily -> "Source Sans Pro", FontWeight -> Plain, FontSize -> 13]],
	
	"Button" -> Function[
		Style[#1, ##2, FontColor -> colour["ButtonText"], FontFamily -> "Source Sans Pro", FontWeight -> Plain, FontSize -> 14]],
	
	"ApplyButton" -> Function[
		Style[#1, ##2, FontColor -> colour["ApplyButtonText"], FontFamily -> "Source Sans Pro", FontWeight -> Plain, FontSize -> 12]],
	
	"FooterText" -> Function[
		Style[#1, ##2, FontColor -> colour["UIDark"], FontFamily -> "Source Sans Pro", FontWeight -> Plain, FontSize -> 12]]
|>;


inputStyle[boxes_, opts___] := StyleBox[boxes, "Input", opts, ShowStringCharacters -> True]


icon = <|
	
	(* Exclamation mark. *)
	"Exclam" -> Function[colour,
		Graphics[
			{EdgeForm[], FaceForm[colour],
				FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}, {{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}, 
					{{0, 2, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}}, {{{9.015, 17.369999999999997}, {4.4558892299999995, 17.369999999999997}, {0.76, 13.64500928}, {0.76, 
					9.049999999999999}, {0.76, 4.454992000000001}, {4.4558892299999995, 0.7300000000000004}, {9.015, 0.7300000000000004}, {13.574109499999999, 0.7300000000000004}, {17.270000000000003, 4.454992000000001}, {17.270000000000003, 
					9.049999999999999}, {17.270000000000003, 13.64500928}, {13.574109499999999, 17.369999999999997}, {9.015, 17.369999999999997}}, {{10.196100000000001, 3.4052000000000007}, {9.87255988, 3.1110688000000017}, {9.450673499999999, 
					2.9510432000000026}, {9.015, 2.9572000000000003}, {8.57976338, 2.9535392000000016}, {8.15874695, 3.1132320000000036}, {7.833899999999999, 3.4052000000000007}, {7.4720592199999984, 3.726876800000001}, {7.279557699999998, 
					4.200092800000004}, {7.313199999999999, 4.6852}, {7.310677779999997, 5.125935999999998}, {7.49009957, 5.547884160000004}, {7.808499999999999, 5.850000000000001}, {8.11909247, 6.178172800000002}, {8.552426630000001, 6.359376000000001}, 
					{9.002299999999998, 6.3492000000000015}, {9.451585360000001, 6.356242560000002}, {9.883723179999999, 6.175539840000003}, {10.196100000000001, 5.850000000000001}, {10.52990934, 5.547096960000005}, {10.715518569999999, 5.112158079999995}, 
					{10.704099999999999, 4.659600000000001}, {10.72568619, 4.1865760000000005}, {10.53993091, 3.7279008000000005}, {10.196100000000001, 3.4052000000000007}}, {{11.046999999999997, 12.249999999999998}, {10.437399999999998, 9.0756}, 
					{10.39444606, 8.741139839999999}, {10.242574379999997, 8.430503040000001}, {10.0056, 8.192400000000001}, {9.700927, 7.958544000000002}, {9.320065430000001, 7.8488697599999995}, {8.9388, 7.885200000000001}, {8.56093055, 
					7.842825600000001}, {8.18181904, 7.953311360000001}, {7.8847, 8.192400000000001}, {7.65662578, 8.44187456}, {7.5064508199999995, 8.75349056}, {7.4529000000000005, 9.0884}, {7.0084, 12.249999999999998}, {6.939052919999998, 
					12.673503360000002}, {6.89663238, 13.10104256}, {6.881399999999999, 13.530000000000001}, {6.89486708, 13.91046976}, {7.136015949999997, 14.24466112}, {7.491, 14.374799999999999}, {8.000101090000001, 14.59409472}, {8.55050639, 
					14.698928000000002}, {9.1039, 14.681999999999999}, {9.67359533, 14.748591999999999}, {10.248492579999999, 14.612789119999999}, {10.729499999999998, 14.298}, {11.021667309999996, 14.000360319999999}, {11.170132849999996, 13.58886464}, 
					{11.135899999999998, 13.1716}, {11.140814899999997, 12.86200512}, {11.110992759999997, 12.55284288}, {11.046999999999997, 12.249999999999998}}}]},
			AspectRatio -> Automatic, ImageSize -> 14{18./18, 19./18}, PlotRange -> {{0., 18.}, {0., 18.14}},
			BaselinePosition -> Scaled[.2]]],
	
	(* Cell Bracket Icon. *)
	"GoToPod" -> Function[colour,
		Graphics[{FaceForm[colour],
  FilledCurve[{{{0, 2, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, 
      {0, 1, 0}, {1, 3, 3}}}, {{{4., 12.}, {21., 12.}, {23.2112, 12.}, {25., 10.2112}, 
      {25., 8.}, {25., 4.}, {25., 1.788800000000002}, {23.2112, 0.}, {21., 0.}, {4., 
      0.}, {1.7888, 0.}, {0., 1.788800000000002}, {0., 4.}, {0., 8.}, {0., 10.2112}, 
      {1.7888, 12.}, {4., 12.}}}],
	  FaceForm[White],
	  FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, 
      {1, 3, 3}}}, {{{6.54, 2.6499999999999986}, {6.54, 3.0072659999999978}, 
      {6.730599000000001, 3.3373929999999987}, {7.04, 3.516025000000001}, {7.349401, 
      3.6946579999999987}, {7.730599000000001, 3.6946579999999987}, {8.04, 
      3.516025000000001}, {8.349401, 3.3373929999999987}, {8.54, 3.0072659999999978}, 
      {8.54, 2.6499999999999986}, {8.557882000000001, 2.379697000000002}, {8.458281, 
      2.114819999999998}, {8.266731, 1.9232699999999987}, {8.075180000000001, 
      1.731720000000001}, {7.810303, 1.6321199999999987}, {7.54, 1.6499999999999986}, 
      {6.987715, 1.6499999999999986}, {6.54, 2.097714999999999}, {6.54, 
      2.6499999999999986}}}], 
  FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}}}, 
     {{{6.91, 5.09}, {6.659999999999999, 10.07}, {8.4, 10.07}, {8.16, 5.09}, {6.91, 
      5.09}}}],
  FilledCurve[{{{0, 2, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, 
      {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}}, 
     {{{16.21, 3.9499999999999993}, {13.129999999999999, 7.}, {13.03534, 7.093883}, 
      {12.9821, 7.221680999999999}, {12.9821, 7.355}, {12.9821, 7.488319}, {13.03534, 
      7.616117}, {13.129999999999999, 7.71}, {13.22388, 7.8046560000000005}, 
      {13.351680000000002, 7.857899000000001}, {13.485000000000001, 7.857899000000001}, 
      {13.618319999999997, 7.857899000000001}, {13.746119999999998, 7.8046560000000005}, 
      {13.84, 7.71}, {16.21, 5.37}, {18.59, 7.7}, {18.683880000000002, 7.794656}, 
      {18.811680000000003, 7.847899000000001}, {18.944999999999997, 7.847899000000001}, 
      {19.078319999999998, 7.847899000000001}, {19.20612, 7.794656}, {19.3, 7.7}, 
      {19.394660000000002, 7.606117}, {19.4479, 7.478319}, {19.4479, 7.345000000000001}, 
      {19.4479, 7.211681}, {19.394660000000002, 7.083883}, {19.3, 6.99}}}]
   }, ImageSize -> 31{25./25, 12./25}, 
 PlotRange -> {{0., 25.}, {0., 12.}}, AspectRatio -> Automatic, ImagePadding -> 1]
	],

	(* Take-action chevron. *)
	"TakeAction" -> Function[colour,
		(*With[
			{chevron = Function[xPos, Line[{{-.5+xPos, 1}, {.5+xPos, 0}, {-.5+xPos, -1}}]]},
			Graphics[
				{
					colour, AbsoluteThickness[1.8], CapForm["Round"], JoinForm["Miter"],
					chevron[0], chevron[1.5]},
				AspectRatio \[Rule] Full, ImageSize \[Rule] .7{13, 11}, BaselinePosition \[Rule] Bottom]]*)
		With[
			{chevron = Function[xPos, Line[{{-.5+xPos, 1}, {.5+xPos, 0}, {-.5+xPos, -1}}]]},
			Graphics[
				{
					colour, AbsoluteThickness[1.8], CapForm["Round"], JoinForm["Miter"],
					chevron[0]},
				AspectRatio -> Full, ImageSize -> .7{8, 11}, BaselinePosition -> Bottom, ImageMargins -> {{0, 4}, {0, 0}}]]],
	
	(* Bin. *)
	"Bin" -> Function[colour,
		Graphics[{EdgeForm[], FaceForm[colour], {FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
			{{{7.81, 12.35}, {7.81, 13.36}, {5.81, 13.36}, {5.81, 12.35}, {2.8099999999999996, 12.35}, {2.8099999999999996, 11.34}, {10.81, 11.34}, {10.81, 12.35}, {7.81, 12.35}}}]}, 
			{FilledCurve[{{{0, 2, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}}, {{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}, {{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
			{{{2.8099999999999996, 10.34}, {2.8099999999999996, 1.8099999999999987}, {2.8153789999999996, 0.9600799999999996}, {3.5001360000000004, 0.27088}, {4.35, 0.259999999999998}, {9.27, 0.259999999999998},
			{10.12213, 0.2654999999999976}, {10.81002, 0.9578499999999988}, {10.81, 1.8099999999999987}, {10.81, 10.34}}, {{5.81, 2.269999999999998}, {4.81, 2.269999999999998}, {4.81, 8.27}, 
			{5.81, 8.27}}, {{8.81, 2.269999999999998}, {7.81, 2.269999999999998}, {7.81, 8.27}, {8.81, 8.27}}}]}},
			AspectRatio -> Automatic, ImageSize -> {14., 14.}, PlotRange -> {{0., 13.62}, {0., 13.62}}, ImageMargins -> {{0, 0}, {0, 2}}]],
	
	(* Insert/Replace. *)
	"Caret" -> Function[colour,
		Graphics[{EdgeForm[], FaceForm[colour], {FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}}}, 
			{{{9.53, 1.1599999999999984}, {6.81, 2.99}, {4.09, 1.1599999999999984}, {3.53, 1.9900000000000002}, {6.53, 3.99}, {6.7007270000000005, 4.099095999999999}, {6.919273000000001, 4.099095999999999}, {7.09, 3.99},
			{10.09, 1.9900000000000002}}}]}, {FilledCurve[{{{0, 2, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, 
			{0, 1, 0}, {1, 3, 3}}}, {{{4.819999999999999, 12.29}, {8.8, 12.29}, {9.358327999999998, 12.29}, {9.81, 11.838327999999999}, {9.81, 11.28}, {9.81, 6.409999999999999}, {9.81, 5.851671999999999}, 
			{9.358327999999998, 5.4}, {8.8, 5.4}, {4.819999999999999, 5.4}, {4.261672000000001, 5.4}, {3.8099999999999996, 5.851671999999999}, {3.8099999999999996, 6.409999999999999}, {3.8099999999999996, 11.28}, 
			{3.8099999999999996, 11.838327999999999}, {4.261672000000001, 12.29}, {4.819999999999999, 12.29}}}]}},
			AspectRatio -> Automatic, ImageSize -> {14., 14.}, PlotRange -> {{0., 13.62}+{1, -1}, {0., 13.62}+{1, -1}}, ImageMargins -> {{0, 0}, {0, 2}}]],
	
	(* Ignore error in cell. *)
	"IgnoreInCell" -> Function[colour,
		Graphics[{EdgeForm[], FaceForm[colour], { 
			FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}}, {{0, 2, 0}, {0, 1, 0}}}, 
				{{{11.69, 13.37}, {7.57, 13.37}, {7.57, 12.37}, {7.76, 12.37}, {11.19, 8.93}, {11.19, 1.2499999999999982}, {7.57, 1.2499999999999982}, {7.57, 0.24999999999999822}, {12.19, 0.24999999999999822}, {12.19, 13.37}},
				{{9.17, 12.37}, {11.17, 12.37}, {11.17, 10.37}}}]}, { 
				FilledCurve[{{{0, 2, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, 
				{0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}}, {{{5.319999999999999, 6.81}, {7.659999999999999, 9.139999999999999}, {7.851783, 9.337814}, {7.851783, 9.652185999999999}, {7.659999999999999, 9.85}, 
				{7.566116999999999, 9.944655999999998}, {7.438319000000001, 9.997898999999999}, {7.305, 9.997898999999999}, {7.171681, 9.997898999999999}, {7.043883, 9.944655999999998}, {6.95, 9.85}, 
				{4.619999999999999, 7.52}, {2.2800000000000002, 9.85}, {2.189267, 9.946677}, {2.062586, 10.001518999999998}, {1.93, 10.001518999999998}, {1.7974139999999998, 10.001518999999998}, {1.670733, 9.946677}, 
				{1.58, 9.85}, {1.4853439999999998, 9.756117}, {1.4321009999999998, 9.628319}, {1.4321009999999998, 9.495000000000001}, {1.4321009999999998, 9.361680999999999}, {1.4853439999999998, 9.233882999999999}, 
				{1.58, 9.139999999999999}, {3.9099999999999997, 6.81}, {1.58, 4.469999999999999}, {1.4833229999999997, 4.3792670000000005}, {1.428481, 4.252585999999997}, {1.428481, 4.119999999999999}, {1.428481, 3.9874139999999976},
				{1.4833229999999997, 3.8607329999999997}, {1.58, 3.769999999999998}, {1.6702800000000002, 3.6728620000000003}, {1.7973959999999998, 3.61838}, {1.93, 3.619999999999999}, {2.062118, 3.6207689999999992},
				{2.1883270000000006, 3.6748589999999997}, {2.2800000000000002, 3.769999999999998}, {4.619999999999999, 6.1}, {7., 3.769999999999998}, {7.079746000000001, 3.6869499999999995}, 
				{7.185712, 3.6339669999999984}, {7.3, 3.619999999999999}, {7.435134, 3.6203309999999984}, {7.5646119999999994, 3.6742799999999995}, {7.659999999999999, 3.769999999999998}, {7.8505709999999995, 3.964421999999999},
				{7.8505709999999995, 4.275577999999999}, {7.659999999999999, 4.469999999999999}}}]}},
				AspectRatio -> Automatic, ImageSize -> {14., 14.}, PlotRange -> {{-.5, 13.62}, {-.5, 13.62}}, ImageMargins -> {{0, 0}, {0, 2}}]],
	
	(* Ignore error in notebook. *)
	"IgnoreInNotebook" -> Function[colour,
		Graphics[{ 
  {FaceForm[colour], 
   FilledCurve[{{{0, 2, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {0, 
    1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {0, 1, 0}, {1, 3, 3}, 
    {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 
    3}}}, {{{4.08, 5.81}, {6.41, 8.139999999999999}, {6.504656, 8.233882999999999}, 
    {6.557899, 8.361680999999999}, {6.557899, 8.495000000000001}, {6.557899, 
    8.628318999999998}, {6.504656, 8.756117}, {6.41, 8.849999999999998}, 
    {6.319267000000001, 8.946676999999998}, {6.192585999999999, 9.001519}, {6.06, 
    9.001519}, {5.927413999999999, 9.001519}, {5.800733, 8.946676999999998}, {5.71, 
    8.849999999999998}, {3.3699999999999997, 6.52}, {1., 8.849999999999998}, {0.906117, 
    8.944655999999998}, {0.778319, 8.997899}, {0.645, 8.997899}, {0.511681, 8.997899}, 
    {0.38388300000000003, 8.944655999999998}, {0.29000000000000004, 8.849999999999998}, 
    {0.19534400000000002, 8.756117}, {0.142101, 8.628318999999998}, {0.142101, 
    8.495000000000001}, {0.142101, 8.361680999999999}, {0.19534400000000002, 
    8.233882999999999}, {0.29000000000000004, 8.139999999999999}, {2.66, 5.81}, 
    {0.32999999999999996, 3.469999999999999}, {0.233323, 3.37927}, {0.17848100000000003, 
    3.252589999999998}, {0.17848100000000003, 3.119999999999999}, {0.17848100000000003, 
    2.987409999999999}, {0.233323, 2.86073}, {0.32999999999999996, 2.769999999999998}, 
    {0.42231500000000005, 2.675790000000001}, {0.548115, 2.621880000000001}, 
    {0.6799999999999999, 2.619999999999999}, {0.801135, 2.630259999999998}, 
    {0.9146200000000001, 2.6834599999999984}, {1., 2.769999999999998}, 
    {3.3699999999999997, 5.1}, {5.71, 2.769999999999998}, {5.80028, 2.67286}, {5.927396, 
    2.61838}, {6.06, 2.619999999999999}, {6.260038, 2.6228099999999994}, 
    {6.439144000000001, 2.744589999999997}, {6.515315999999999, 2.9295799999999996}, 
    {6.591488, 3.1145599999999973}, {6.55006, 3.3271499999999996}, {6.41, 
    3.469999999999999}}}]}, 
  {FaceForm[colour], 
   FilledCurve[{{{0, 2, 0}, {1, 3, 3}, {0, 1, 0}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 
     3, 3}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {0, 1, 0}, 
     {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 
     0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, 
     {1, 3, 3}, {0, 1, 0}, {1, 3, 3}}, {{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, 
    {{{11.7, 13.37}, {5.26, 13.37}, {4.330546, 13.3645}, {3.579984, 12.60947}, {3.58, 
     11.68}, {3.58, 8.84}, {4.64, 9.91}, {5.015796, 10.288039}, {5.526954999999999, 
     10.500422}, {6.06, 10.5}, {6.589577, 10.497767}, {7.09665, 10.285588}, 
     {7.470000000000001, 9.91}, {7.625710999999999, 9.752296999999999}, 
     {7.7541269999999995, 9.569811}, {7.85, 9.37}, {10.44, 9.37}, {10.44, 8.62}, {8.05, 
     8.62}, {8.05, 8.62}, {8.05, 8.54}, {8.05, 8.5}, {8.050422000000001, 7.966955}, 
     {7.838039, 7.455796}, {7.46, 7.079999999999999}, {7.2700000000000005, 
     6.889999999999999}, {11.48, 6.889999999999999}, {11.48, 6.139999999999999}, {6.48, 
     6.139999999999999}, {6.1499999999999995, 5.81}, {7.470000000000001, 
     4.529999999999999}, {7.58, 4.4}, {9.58, 4.4}, {9.58, 3.619999999999999}, {8., 
     3.619999999999999}, {8.05171, 3.448030000000001}, {8.078647, 3.2695699999999963}, 
     {8.08, 3.09}, {8.08, 1.985430000000001}, {7.184569, 1.0899999999999999}, {6.08, 
     1.0899999999999999}, {5.546955, 1.0895799999999998}, {5.035796, 
     1.3019599999999993}, {4.659999999999999, 1.6799999999999997}, {3.58, 
     2.769999999999998}, {3.58, 1.9299999999999997}, {3.58, 1.00216}, {4.332162, 
     0.24999999999999822}, {5.26, 0.24999999999999822}, {11.7, 0.24999999999999822}, 
     {12.62947, 0.24997999999999898}, {13.384500000000001, 1.0005499999999987}, {13.39, 
     1.9299999999999997}, {13.39, 11.68}, {13.39, 12.613361}, {12.633359999999998, 
     13.37}, {11.7, 13.37}}, {{9.27, 11.11}, {5.09, 11.11}, {5.09, 11.86}, {9.27, 
     11.86}}}]}}, AspectRatio -> Automatic, ImageSize -> {14., 14.}, 
 PlotRange -> {{0., 13.62}, {-.5, 13.62}}]],
	
	(* Ignore error always. *)
	"IgnoreAlways" -> Function[colour,
		Graphics[{EdgeForm[], FaceForm[colour], FilledCurve[{{{0, 2, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {0, 
			1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}}, {{{7.52, 6.81}, 
			{10.739999999999998, 10.03}, {10.83466, 10.123883}, {10.8879, 10.251681}, {10.8879, 10.385}, {10.8879, 10.518318999999998}, {10.83466, 10.646117}, {10.739999999999998, 10.739999999999998}, 
			{10.649269999999998, 10.836676999999998}, {10.522590000000001, 10.891518999999999}, {10.39, 10.891518999999999}, {10.25741, 10.891518999999999}, {10.13073, 10.836676999999998}, {10.04, 
			10.739999999999998}, {6.81, 7.52}, {3.58, 10.739999999999998}, {3.489267, 10.836676999999998}, {3.362586, 10.891518999999999}, {3.23, 10.891518999999999}, {3.097414, 10.891518999999999}, {2.970733, 
			10.836676999999998}, {2.88, 10.739999999999998}, {2.7853440000000003, 10.646117}, {2.732101, 10.518318999999998}, {2.732101, 10.385}, {2.732101, 10.251681}, {2.7853440000000003, 10.123883}, {2.88, 
			10.03}, {6.1, 6.81}, {2.88, 3.619999999999999}, {2.73994, 3.47715}, {2.698512, 3.264559999999996}, {2.774684, 3.07958}, {2.8508559999999994, 2.894589999999999}, {3.029962, 2.7728099999999998}, {3.23, 
			2.769999999999998}, {3.3626039999999997, 2.7683799999999987}, {3.48972, 2.8228600000000004}, {3.58, 2.92}, {6.81, 6.1}, {10., 2.880000000000001}, {10.091669999999999, 2.7848599999999983}, {10.21788, 
			2.730769999999998}, {10.350000000000001, 2.7299999999999986}, {10.482600000000001, 2.7283799999999996}, {10.60972, 2.7828599999999994}, {10.7, 2.880000000000001}, {10.79668, 2.9707300000000014}, 
			{10.85152, 3.097409999999998}, {10.85152, 3.2299999999999986}, {10.85152, 3.362589999999999}, {10.79668, 3.4892699999999994}, {10.7, 3.58}}}]},
			AspectRatio -> Automatic, ImageSize -> {14., 14.}, PlotRange -> {{-.5, 13.62}, {-.5, 13.62}}, ImageMargins -> {{0, 0}, {0, 2}}]],
	
	(* Info (used for documentation links). *)
	"Info" -> Function[colour,
		Graphics[{FaceForm[colour], 
			FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}, {{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 
			3}}, {{1, 4, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}}}, {{{6.81, 13.}, {3.3913569999999997, 13.}, {0.62, 10.228643}, {0.62, 6.81}, 
			{0.62, 3.3913600000000006}, {3.3913569999999997, 0.6199999999999992}, {6.81, 0.6199999999999992}, {10.228639999999999, 0.6199999999999992}, {13., 
			3.3913600000000006}, {13., 6.81}, {13., 10.228643}, {10.228639999999999, 13.}, {6.81, 13.}}, {{6.81, 3.6899999999999995}, {6.92, 3.5299999999999994}, {7.94, 
			4.4399999999999995}, {8., 4.0699999999999985}, {8.06, 3.6999999999999993}, {7.44, 3.1899999999999995}, {7.13, 2.969999999999999}, {6.621312, 2.67488}, {6.037101, 
			2.535780000000001}, {5.45, 2.5699999999999985}, {4.83, 2.629999999999999}, {4.67, 3.259999999999998}, {4.67, 3.6599999999999984}, {4.851589, 4.494925}, {5.09903, 
			5.314153999999997}, {5.41, 6.109999999999999}, {5.54, 6.449999999999999}, {6.159999999999999, 8.}, {6.159999999999999, 8.}, {6.359999999999999, 8.51}, {7.46, 
			8.309999999999999}, {7.8, 8.25}, {8.139999999999999, 8.189999999999998}, {8.05, 7.659999999999999}, {7.859999999999999, 7.249999999999999}, {7.67, 
			6.839999999999999}, {7.24, 5.919999999999999}, {7.06, 5.34}, {6.878061000000001, 4.809523999999998}, {6.79337, 4.250561999999997}, {6.81, 3.6899999999999995}}, 
			{{8.81, 9.79}, {8.810053, 9.512194000000001}, {8.587752, 9.285447}, {8.31, 9.28}, {7.51, 9.28}, {7.2283349999999995, 9.28}, {7., 9.508334999999999}, {7., 9.79}, 
			{7., 10.62}, {7.005447, 10.897752}, {7.232194, 11.120052999999999}, {7.51, 11.12}, {8.350000000000001, 11.12}, {8.623907999999998, 11.11468}, {8.84468, 10.893908}, 
			{8.850000000000001, 10.62}}}]},
			AspectRatio -> Automatic, ImageSize -> {14., 14.}, PlotRange -> {{0., 13.62}, {0., 13.62}}, ImageMargins -> {{0, 0}, {0, 2}}]],
	
	(* Link open arrow. *)
	"OpenLinkArrow" -> Function[colour,
		Graphics[{FaceForm[colour], 
			FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {0, 1, 
			0}, {1, 3, 3}}}, {{{10.360000000000001, 10.09}, {10.311209999999999, 10.213465}, {10.21347, 10.311209999999999}, {10.09, 10.36}, {10.02754, 10.374876999999998}, 
			{9.962460000000002, 10.374876999999998}, {9.9, 10.36}, {6.08, 10.36}, {5.803858, 10.36}, {5.58, 10.136142}, {5.58, 9.86}, {5.58, 9.583858}, {5.803858, 9.36}, {6.08, 
			9.36}, {8.7, 9.36}, {3.3699999999999997, 4.0699999999999985}, {3.198604, 3.8728380000000016}, {3.2078830000000003, 3.5769599999999997}, {3.391297, 
			3.390930000000001}, {3.574711, 3.204889999999999}, {3.870427, 3.191419999999999}, {4.07, 3.3599999999999994}, {9.4, 8.689999999999998}, {9.4, 6.079999999999999}, 
			{9.4, 5.803857999999999}, {9.623857999999998, 5.58}, {9.9, 5.58}, {10.17614, 5.58}, {10.4, 5.803857999999999}, {10.4, 6.079999999999999}, {10.4, 9.899999999999999}, 
			{10.40207, 9.965622}, {10.388349999999999, 10.030783}, {10.360000000000001, 10.09}}}]},
			AspectRatio -> Automatic, ImageSize -> {14., 14.}, PlotRange -> {{0., 13.62}, {0., 13.62}}(*, ImageMargins \[Rule] {{0, 0}, {0, 2}}*)]],
	
	(* Pop out icon. *)
	"PopOut" -> (*Function[colour,
		Graphics[{
			colour, Disk[{0, 0}, 1],
			CapForm["Round"], White,
			AbsoluteThickness[1.5], Line[{{.5{-1, 0}, .5{1, 0}}, {.5{0, 1}, .5{0, -1}}}]},
			PlotRange -> 1, ImagePadding -> 1, ImageSize -> 15{1, 1}]]*)
	Function[colour,
		Graphics[{FaceForm[colour],
		FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}, {{1, 4, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {0, 1, 0}, 
      {0, 1, 0}, {0, 1, 0}, {1, 3, 3}}, {{1, 4, 3}, {1, 3, 3}, {0, 1, 0}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}}, 
     {{{7., 14.}, {3.134007, 14.}, {0., 10.865993}, {0., 7.}, {0., 3.134009999999998}, {3.134007, 0.}, {7., 0.}, {10.865990000000002, 0.}, {14., 3.134009999999998}, {14., 7.}, {14., 8.856515000000002}, 
      {13.262500000000001, 10.636993}, {11.94975, 11.949747}, {10.636989999999999, 13.262502}, {8.856515000000002, 14.}, {7., 14.}}, {{10.41, 3.6499999999999986}, {10.41, 3.3738600000000005}, {10.18614, 
      3.1499999999999986}, {9.91, 3.1499999999999986}, {3.8499999999999996, 3.1499999999999986}, {3.5738579999999995, 3.1499999999999986}, {3.3499999999999996, 3.3738600000000005}, {3.3499999999999996, 
      3.6499999999999986}, {3.3499999999999996, 9.71}, {3.35532, 9.983908}, {3.5760919999999996, 10.20468}, {3.8499999999999996, 10.21}, {5.51, 10.21}, {5.544165, 9.823222}, {5.726979999999999, 9.464761}, {6.02, 9.21}, 
      {4.35, 9.21}, {4.35, 4.149999999999999}, {9.41, 4.149999999999999}, {9.41, 5.8100000000000005}, {9.667296, 5.520240999999999}, {10.024369999999998, 5.338131000000001}, {10.41, 5.300000000000001}}, {{11., 7.}, {11., 
      6.723857999999999}, {10.77614, 6.5}, {10.5, 6.5}, {10.223859999999998, 6.5}, {10., 6.723857999999999}, {10., 7.}, {10., 9.33}, {6.71, 6.}, {6.614611999999999, 5.90428}, {6.4851339999999995, 5.850331000000001}, 
      {6.35, 5.85}, {6.218114999999999, 5.851879000000004}, {6.092314999999999, 5.905793000000001}, {6., 6.}, {5.905344, 6.093883}, {5.852100999999999, 6.221680999999999}, {5.852100999999999, 6.355}, {5.852100999999999, 
      6.488319}, {5.905344, 6.616117}, {6., 6.71}, {9.33, 10.}, {7., 10.}, {6.723858, 10.}, {6.5, 10.223858}, {6.5, 10.5}, {6.5, 10.776142}, {6.723858, 11.}, {7., 11.}, {10.54, 11.}, {10.60299, 11.009306}, {10.66701, 
      11.009306}, {10.729999999999999, 11.}, {10.85157, 10.948358}, {10.948359999999997, 10.851567}, {11., 10.73}, {11.00931, 10.667009}, {11.00931, 10.602991}, {11., 10.54}}}]},
		ImageSize -> 15{1, 1}, PlotRange -> {{0., 14.}, {0., 14.}}, ImagePadding -> {{1, 1}, {1, 1}}, AspectRatio -> Automatic]
		]
		(*Function[colour,
		Graphics[{ 
  EdgeForm[], FaceForm[colour], 
  FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}, {{0, 2, 0}, {1, 3, 3}, {0, 
    1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}}, {{1, 4, 
    3}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}}}, 
   {{{7., 13.5}, {3.4101489999999997, 13.5}, {0.5, 10.589851}, {0.5, 7.}, {0.5, 
    3.41015}, {3.4101489999999997, 0.5}, {7., 0.5}, {10.58985, 0.5}, {13.5, 3.41015}, 
    {13.5, 7.}, {13.5, 10.589851}, {10.58985, 13.5}, {7., 13.5}}, {{4.609999999999999, 
    7.22}, {4.609999999999999, 5.460000000000001}, {4.082803999999999, 
    5.687240999999998}, {3.7409510000000004, 6.205916000000001}, {3.74, 6.78}, {3.74, 
    9.25}, {3.74, 10.04529}, {4.384709999999999, 10.690000000000001}, {5.18, 
    10.690000000000001}, {6.94, 10.690000000000001}, {7.570081, 10.697118}, 
    {8.130562999999999, 10.290972}, {8.32, 9.690000000000001}, {7.06, 
    9.690000000000001}, {6.406739000000001, 9.690021999999999}, {5.780522, 9.429154}, 
    {5.320479, 8.965354999999999}, {4.860434999999999, 8.501556}, {4.604667, 7.873239}, 
    {4.609999999999999, 7.22}}, {{10.26, 4.75}, {10.26, 3.9547100000000004}, {9.61529, 
    3.3100000000000005}, {8.82, 3.3100000000000005}, {7.06, 3.3100000000000005}, 
    {6.676359000000001, 3.30734}, {6.307514999999999, 3.4578699999999998}, 
    {6.035296000000001, 3.7282100000000007}, {5.763077, 3.9985599999999994}, {5.609991, 
    4.366349000000001}, {5.609999999999999, 4.75}, {5.609999999999999, 7.22}, 
    {5.615495999999999, 8.016905}, {6.263076, 8.660019}, {7.06, 8.66}, {8.82, 8.66}, 
    {9.613020999999998, 8.654549}, {10.25455, 8.013020999999998}, {10.26, 7.22}}}]}, 
 AspectRatio -> Automatic, ImageSize -> 15{1, 1}, PlotRange -> {{0., 14.}, {0., 14.}}]]*),
	
	(* Opener chevrons. *)
	"DownChevron" -> Function[colour, Graphics[
		{colour, AbsoluteThickness[2], CapForm["Round"],
			Line[{{-1, 0}, {0, -1}, {1, 0}}]},
		ImagePadding -> {3{1, 1}, 3{1, 1}}, ImageSize -> {18, 11}, AspectRatio -> Full, BaselinePosition -> Bottom]],
	"UpChevron" -> Function[colour, Graphics[
		{colour, AbsoluteThickness[2], CapForm["Round"],
			Line[{{-1, 0}, {0, 1}, {1, 0}}]},
		ImagePadding -> {3{1, 1}, 3{1, 1}}, ImageSize -> {18, 11}, AspectRatio -> Full, BaselinePosition -> Bottom]]
|>;


closeIcon[offset_, pos_] := With[{crossSize = 2.5, diskRad = 7},
	Tooltip[
		{
			colour["UIDark"],
			Disk[Offset[offset, pos], Offset[diskRad]],
			colour["UIBack"], AbsoluteThickness[1.5], CapForm["Round"],
			Line[{{Offset[offset + crossSize {-1, 1}, pos], Offset[offset + crossSize {1, -1}, pos]},
				{Offset[offset + crossSize {-1, -1}, pos], Offset[offset + crossSize {1, 1}, pos]}}]},
				
		"Close analysis pod."]]


(* ::Text:: *)
(*bezierCirclePts returns the control points of an approximate Bezier curve for each quadrant of the unit circle.*)


bezierCirclePts = With[{c = .551915024494},
	<|
		"Q1" -> {{1, 0}, {1, 0}, {1, c}, {c, 1}, {0, 1}, {0, 1}},
		"Q2" -> {{0, 1}, {0, 1}, {-c, 1}, {-1, c}, {-1, 0}, {-1, 0}},
		"Q3" -> {{-1, 0}, {-1, 0}, {-1, -c}, {-c, -1}, {0, -1}, {0, -1}},
		"Q4" -> {{0, -1}, {0, -1}, {c, -1}, {1, -c}, {1, 0}, {1, 0}}
	|>
];


SetAttributes[button, HoldRest]


button[
	disp_, action_,
	OptionsPattern[{
		ImageSize -> {Automatic, 19},
		FrameMargins -> {9{1, 1}, 0{1, 1}},
		BaselinePosition -> Baseline,
		Alignment -> {Center, Center},
		"ActiveQ" -> True,
		(* Hover colours for the text, background and frame. *)
		"TextColour" -> colour["ButtonText"],
		"TextHoverColour" -> colour["ButtonTextHover"],
		"TextInactiveColour" -> colour["ButtonTextInactive"],
		"BackColour" -> colour["ButtonBack"],
		"BackHoverColour" -> colour["ButtonBackHover"],
		"BackMouseDownColour" -> colour["ButtonBackMouseDown"],
		"BackInactiveColour" -> colour["ButtonBackInactive"],
		"EdgeColour" -> colour["ButtonEdge"],
		"EdgeHoverColour" -> colour["ButtonEdgeHover"],
		"EdgeInactiveColour" -> colour["ButtonEdgeInactive"]
	}]
] :=
	Pane[ 
		DynamicModule[{hoverQ = False, mouseDownQ = False},
			DynamicWrapper[
				EventHandler[
					Highlighted[
						style["Button"][
							disp,
							FontColor -> Dynamic @ Which[
								TrueQ[!OptionValue["ActiveQ"]], OptionValue["TextInactiveColour"],
								hoverQ, OptionValue["TextHoverColour"],
								True, OptionValue["TextColour"]]],
						
						FrameMargins -> OptionValue[FrameMargins], Alignment -> OptionValue[Alignment],
						Frame -> True, ImageSize -> OptionValue[ImageSize], RoundingRadius -> 3,
						
						FrameStyle -> Dynamic @ Directive[AbsoluteThickness[.5], Which[
							TrueQ[!OptionValue["ActiveQ"]], OptionValue["EdgeInactiveColour"],
							hoverQ, OptionValue["EdgeHoverColour"],
							True, OptionValue["EdgeColour"]]],
						
						Background -> Dynamic @ Which[
							TrueQ[!OptionValue["ActiveQ"]], OptionValue["BackInactiveColour"],
							hoverQ && mouseDownQ, OptionValue["BackMouseDownColour"],
							hoverQ, OptionValue["BackHoverColour"],
							True, OptionValue["BackColour"]]],
					
					{
						"MouseDown" :> (mouseDownQ = True),
						(* "ActiveQ" must be False and only False to inactivate the button. *)
						(* Note that "MouseClicked" (or indeed Button) wasn't used for the button action becuase
							there seems to be some bug that doesn't evaluate the "MouseClicked" action in some cases. Yet to characterise. *)
						"MouseUp" :> (
								mouseDownQ = False;
								If[hoverQ && OptionValue["ActiveQ"] =!= False, action])
					},
					PassEventsDown -> True],
				
				hoverQ = CurrentValue["MouseOver"]]],

		FrameMargins -> None, BaselinePosition -> OptionValue[BaselinePosition], Alignment -> {Center, Center}]


popupPane[
	contents_, {width_, height_},
	caretPos_ /; Between[caretPos, {-1, 1}],
	OptionsPattern[{
		Alignment -> {Left, Top},
		Background -> colour["PopupBack"],
		FrameStyle -> Directive[{AbsoluteThickness[1], colour["PopupEdge"]}]
	}]
] :=
	(* safetyPadding is added to each edge to ensure that no lines are clipped when rendered by the FE. *)
	With[{roundingRad = 4, caretH = 6, caretW = 12, safetyPadding = 3},
		Overlay[
			{
				Pane[
					(* The background of the popup is a pane with a caret at position caretPos along the top edge. *)
					Graphics[
						{
							EdgeForm[OptionValue[FrameStyle]], FaceForm[OptionValue[Background]],
							(* We want to be able to specify caretPos to be between -1 and 1 along the top edge. Therefore, the centre
								of each rounded corner needs to lie a distance of caretW/2 outside the region {{-1, 1}, {-1, 1}}.
								This means that at caretPos -1 or 1, the rounding of the corner will start at the very edge of the caret.
								Hence, each corner coord needs to be:
									scaled by the rounding radius: roundingRad(#),
									shifted a distance {1, 1} away from the {{-1, 1}, {-1, 1}} region: + (caretW/2){1, 1},
									shifted down by the caret height for the top two corners: + {0, -caretH}. *)
							FilledCurve[BezierCurve[Join[
								(* Top right corner. *)
								Offset[roundingRad(#) + (caretW/2){1, 1} + {0, -caretH}, {1, 1}]& /@ bezierCirclePts["Q1"][[2;;]],
								(* Caret. *)
								(* Seeing as the frame lies a distance (roundingRad + caretW/2) from the region {{-1, 1}, {-1, 1}}, so does the caret itself. *)
								Splice[Table[Offset[#, {caretPos, 1}], 3(* Verticies of an order 3 Bezier curve are three coincident points. *)]]& /@{
									{caretW/2, -caretH + caretW/2 + roundingRad},
									{0, caretW/2 + roundingRad},
									{-caretW/2, -caretH + caretW/2 + roundingRad}},
								(* Top left corner. *)
								Offset[roundingRad(#) + (caretW/2){-1, 1} + {0, -caretH}, {-1, 1}]& /@ bezierCirclePts["Q2"],
								(* Bottom left corner. *)
								Offset[roundingRad(#) + (caretW/2){-1, -1}, {-1, -1}]& /@ bezierCirclePts["Q3"],
								(* Bottom right corner *)
								Offset[roundingRad(#) + (caretW/2){1, -1}, {1, -1}]& /@ bezierCirclePts["Q4"][[;;-2]]
							]]]
						}, PlotRange -> 1, ImagePadding -> (roundingRad + caretW/2 + safetyPadding), AspectRatio -> Full],
					{width, height}],
			
				With[{contentsPadding = safetyPadding + roundingRad},
					(* Keep the contents within the area set by the frame's rounding radius and caret height. *)
					Pane[
						Style[contents, LineIndent -> 0],
						{width - 2contentsPadding, height - (2contentsPadding + caretH)},
						ImageMargins -> {
							{contentsPadding, contentsPadding},
							{contentsPadding, contentsPadding + caretH}},
					Alignment -> OptionValue[Alignment]]]},
					
			(* Allow user interactions in the contents pane. *)
			{1, 2}, 2,
			
			Alignment -> {Center, Center}]]


(* ::Section::Closed:: *)
(*Support Functions*)


(* Make sure that attached cells are evaluating in the same kernel that loaded the CodeInspector`LinterUI` context. *)
$evaluator = CurrentValue[Evaluator];


(* ::Text:: *)
(*Construct a unique variable for a lint, of the form CodeInspector`LinterUI`LintStates`NbXXXXCellXXXXLintXXXXName.*)
(*These will be used to store Raft cells, and to track when the user interacts with a lint.*)
(*Note that we're using these uniquely-named variables here rather than nested associations because if an element of the association changes, then all dynamic objects tracking the association will update needlessly. So for example if the lint state of one raft changes, then all rafts (in all cells) would be redrawn.*)


constructLintVar[cell_CellObject, lint_CodeInspector`InspectionObject, name_String] :=
	ToHeldExpression[StringJoin[
		"CodeInspector`LinterUI`LintVars`",
		
		(* The notebook identifier. *)
		"Nb",
		With[{nbID = Last[ParentNotebook[cell]]},
			(* For UUID nb objects, replace dashes with "x"s so they can be used in variable names.
				Otherwise, the nb object index is just a number, so convert it to a string. *)
			If[StringQ[nbID], StringReplace[nbID, "-" -> "x"], ToString[nbID]]],
			
		(* The cell identifier. *)
		"Cell",
		With[{cellID = First[cell]},
			(* As with nb objects, check if the cell object contains a UUID or a number. *)
			If[StringQ[cellID], StringReplace[cellID, "-" -> "x"], ToString[cellID]]],
		
		(* The lint source part spec. *)
		"Lint",
		Sequence @@ Riffle[ToString /@ extractFirstList[Last[lint][CodeParser`Source]], "x"],
		
		(* The name of the variable. *)
		name
	]]


applyChanges[cell_CellObject] :=
	With[{notebookID = Last[ParentNotebook[cell]]}, 
	
	NotebookWrite[
		cell,
		CodeInspector`LinterUI`lintedCells[notebookID][cell]["CellContents"],
		After];
		
	NotebookDelete[
		CodeInspector`LinterUI`lintedCells[notebookID][cell]["UIAttachedCells"]]]


(* ::Text:: *)
(*There are several large constructs that need to dynamically redraw when some value changes.*)
(*These values are stored in an association, so if we simply track a value from that association, then the construct will needlessly redraw when other values of that association change.*)
(*Therefore we wrap the construct in a DynamicWrapper that tracks the specific value of the association that we're interested in. The Dynamic construct then only updates with the tracker, and is thus prevented from needlessly refreshing when other values of the association change.*)


SetAttributes[isolatedDynamic, HoldRest]


isolatedDynamic[Dynamic[var_], expr_] :=
	DynamicModule[{tracker = var},
		DynamicWrapper[
			Dynamic[tracker; expr, TrackedSymbols :> {tracker}],
			If[tracker =!= var, tracker = var]]]


constrainWidth[expr_, width_:354] := Pane[Style[expr, LineIndent -> 0], width, FrameMargins -> 0, ImageMargins -> 0, BaselinePosition -> Baseline]


extractFirstList[expr_] := FirstCase[expr, _List, {}, {0, Infinity}]


(* ::Section::Closed:: *)
(*Lint Rafts*)


constructRaftMenuItemLabel[raftType_, icon_, label_] :=
	With[{},
		Highlighted[
			Grid[
				{{icon, Spacer[7], constrainWidth[label, Full]}},
				ItemSize -> Automatic, Spacings -> 0, Alignment -> {Left, Top}],
			
			Frame -> None, RoundingRadius -> 0, FrameMargins -> {{5, 2}, {2, 2}},
			
			ImageSize -> Switch[raftType,
				"inPlace", 384,
				"mooring", Full],
				
			ImageMargins -> {{0, 0}, {0, 0}},
			
			(* Highlight the menu item on mouseover. *)
			Background -> Dynamic[If[CurrentValue["MouseOver"], colour["RaftItemHighlight"], colour["RaftBack"]]]
		]
	]


raftMenuItemClickAction[
	Dynamic[itemClicked_],
	raftType_, Dynamic[raftCell_], Dynamic[raftMenu_]
] := (
	(* itemClicked tells the parent raft whether this menu was deleted by a) clicking on a menu item (=True), or b) by mousing
		out or clicking the raft closer (=False). This is important for correctly setting the lint state variable to either "inactive"
		or "hoverXXXX" on menu dismissal. *)
	itemClicked = True;
			
	(* If the raft is in-place, then clicking a menu item should delete the entire raft. However, mooring rafts need to
		stay in the mooring when an action is taken, so only delete the raft menu. *)
	Switch[raftType,
		"inPlace", NotebookDelete[raftCell],
		"mooring", NotebookDelete[raftMenu]]
)


(* ::Subsection::Closed:: *)
(*"Ignore" Menu Item*)


makeRaftMenuIgnoreItem[
	(* The scope at which you're suppressing the lint tag (CellObj, NbObj, or $FrontEnd). *)
	scope_,
	(* The menu item icon. *)
	icon_,
	(* The menu item label, of the form {"Text", #, "more text."}&, where # is the lint tag that gets filled in from... *)
	label_Function,
	(* ...the lint, which is also used to set the suppression option value. *)
	lint_CodeInspector`InspectionObject,
	(* itemClicked is a state variable. *)
	Dynamic[itemClicked_],
	(* These raftXXXX vars are used to delete raft components when a menu item is clicked. *)
	raftType_, Dynamic[raftCell_], Dynamic[raftMenu_]
] := 
	With[{argument = Last[lint]["Argument"]},
		Button[
		
			(* ----- The menu item label ----- *)
			
			constructRaftMenuItemLabel[raftType, icon,
				(* The lint tag is drawn in a different colour to the rest of the item.
					If it has an "Argument", then display LintTag\:25b9Argument. *)
				style["RaftMenuItem"][Row[label[
					Style[If[argument === Missing["KeyAbsent", "Argument"],
						lint["Tag"],
						Row[{lint["Tag"], "\:25BB", argument}, "\[VeryThinSpace]"]], FontColor -> colour["UIDark"]]]]]],
			
			
			(* ----- The menu item action ----- *)
			
			(* Append the supression tag to the option value. *)
			(* CurrentValue is used to set the option, but AbsoluteCurrentValue is used to query the option to ensure correct
				scope inheritance. See https://stash.wolfram.com/projects/FE/repos/frontend/pull-requests/5783/overview *)
			CurrentValue[scope, {CodeAssistOptions, "CodeToolsOptions", "CodeInspect", "Disableds"}] =
				DeleteDuplicates @ Append[
					Replace[
						AbsoluteCurrentValue[scope, {CodeAssistOptions, "CodeToolsOptions", "CodeInspect", "Disableds"}],
						Inherited -> {}],
					If[argument === Missing["KeyAbsent", "Argument"], lint["Tag"], {lint["Tag"], argument}]];
			
			(* Set state variables and delete raft components. *)
			raftMenuItemClickAction[Dynamic[itemClicked], raftType, Dynamic[raftCell], Dynamic[raftMenu]],
				
			Appearance -> None]]


(* ::Subsection::Closed:: *)
(*CodeAction Menu Item*)


makeRaftMenuCodeActionItem[
	cell_CellObject,
	codeAction_CodeParser`CodeAction,
	(* The box contents of the cell. *)
	Dynamic[cellContents_],
	(* itemClicked is a state variable. *)
	Dynamic[itemClicked_],
	(* These raftXXXX vars are used to delete raft components when a menu item is clicked. *)
	raftType_, Dynamic[raftCell_], Dynamic[raftMenu_]
] := 
	With[{notebookID = Last[ParentNotebook[cell]]},
		Button[
		
			(* ----- The menu item label ----- *)
			
			constructRaftMenuItemLabel[raftType, 
			
				(* For deletion actions, use the trash icon. For replacement and insertion actions, use the caret icon. *)
				Switch[codeAction[[2]],
					Alternatives[CodeParser`DeleteNode, CodeParser`DeleteTriviaNode, CodeParser`DeleteText, CodeParser`DeleteTrivia],
					icon["Bin"][colour["UIDark"]],
					Alternatives[CodeParser`InsertNode, CodeParser`ReplaceNode, CodeParser`InsertNodeAfter, CodeParser`InsertText, CodeParser`InsertTextAfter, CodeParser`ReplaceText],
					icon["Caret"][colour["UIDark"]],
					(* Failsafe. *)
					_, icon["Caret"][colour["UIDark"]]],
				
				(* The code action label. Format pieces of code within the label with "Input" style, and enforce "StandardForm"'s font. *)
					style["RaftMenuItem"][Row[
						(* I'm using Brenton's boldify function to do the string-parsing, and then replacing his formatting wrappers with StyleBoxes. *)
						Replace[CodeInspector`Utils`boldify[codeAction["Label"]],
							CodeInspector`Format`LintMarkup[s_, ___] :>
								RawBoxes[inputStyle[s, FontFamily ->
									CurrentValue[{StyleDefinitions, "StandardForm", FontFamily}]]],
							1]]]],
			
			
			(* ----- The menu item action ----- *)
			
			(* Perform the codeAction transformation on the cell contents. This involves converting into, and back out of, Brenton's concrete syntax tree. *)
			cellContents = ReplacePart[
				cellContents,
				(* cellContents is of the form Cell[BoxData[_], ___], so we want to apply the code actions on the contents of BoxData. *)
				{1, 1} -> CodeParser`ToStandardFormBoxes @ CodeParser`CodeAction`ApplyCodeAction[
					codeAction,
					CodeParser`CodeConcreteParseBox[First[First[cellContents]]]]];
			
			(* Now re-lint, and re-markup the code. *)
			Module[
				(* Regenerate the lints. *)
				{lints = CodeInspector`CodeInspectBox[First[First[cellContents]]]},
				
				(* Re-markup. *)
				CodeInspector`LinterUI`lintedCells[notebookID][cell]["MarkedUpCode"] = 
					Fold[Function[{codeBoxes, lint}, markupCode[cell, lint, codeBoxes]],
						First[First[cellContents]],
						ReverseSortBy[lints, Last[#][CodeParser`Source]&]];
				
				(* Regenerate the rafts. *)
				CodeInspector`LinterUI`lintedCells[notebookID][cell]["LintRafts"] =
					makeRaftCell[cell, #]& /@ lints];
			
			(* Inform the rest of the UI that an edit has been made. *)
			CodeInspector`LinterUI`lintedCells[notebookID][cell]["EditsMadeQ"] = True;
			
			(* Set state variables and delete raft components. *)
			raftMenuItemClickAction[Dynamic[itemClicked], raftType, Dynamic[raftCell], Dynamic[raftMenu]],
				
			Appearance -> None]]


(* ::Subsection::Closed:: *)
(*Docs Menu Item*)


makeRaftMenuDocsItem[
	symbol_String,
	(* itemClicked is a state variable. *)
	Dynamic[itemClicked_],
	(* These raftXXXX vars are used to delete raft components when a menu item is clicked. *)
	raftType_, Dynamic[raftCell_], Dynamic[raftMenu_]
] := 
	With[{},
		Button[
			(* ----- The menu item label ----- *)
			
			(* The label is the docs info icon, followed by the name of the symbol, and an open-link arrow icon. *)
			constructRaftMenuItemLabel[raftType,
				icon["Info"][colour["UIDark"]],
				Row[{
					style["RaftMenuItem"][Row[CodeInspector`Utils`boldify["``" <> symbol <> "``"],
						(* Fudge factor to get the icons and text to line up nicely. *)
						BaselinePosition -> (Baseline -> Scaled[.135])]],
					icon["OpenLinkArrow"][colour["UIDark"]]}]],
			
			
			(* ----- The menu item action ----- *)
			
			(* Open the documentation page for the symbol. *)
			SystemOpen[URLBuild[{"paclet:ref", symbol}]];
			
			(* Set state variables and delete raft components. *)
			raftMenuItemClickAction[Dynamic[itemClicked], raftType, Dynamic[raftCell], Dynamic[raftMenu]],
				
			Appearance -> None]]


(* ::Subsection::Closed:: *)
(*Construct Raft*)


makeRaftMenu[cell_CellObject, lint_CodeInspector`InspectionObject, raftCell_CellObject, Dynamic[itemClicked_], raftType_, Dynamic[raftMenu_]] :=
	With[
		{
			(* Don't provide documentation links for these symbols. *)
			excludedDocsSymbols = {"List"},
			
			(* Last will pull out both the notebook index for legacy notebook objects, and the UUID for new notebook objects. *)
			notebookID = Last[ParentNotebook[cell]]
		},
		
		{
			(* Get any system symbols that have been marked up as "``XXXX``" in the description. We'll provide docs refs for these. *)
			symbols = DeleteDuplicates @ DeleteCases[
				StringCases[lint["Description"],
					"``" ~~ s__ ~~ "``" /; StringFreeQ[s, " "] && Quiet[Context[s]] === "System`" :> s],
				Alternatives @@ excludedDocsSymbols],
			
			(* Get the list of code actions from the lint, if they exist. *)
			actionItems = Replace[Last[lint][CodeParser`CodeActions], _Missing -> {}],
			
			(* Define the "Ignore" item buttons. *)
			ignoreItems = {
			
				(* Cell *)
				makeRaftMenuIgnoreItem[cell, icon["IgnoreInCell"][colour["UIDark"]],
					{"Ignore ", #, " errors in this cell"}&,
					lint, Dynamic[itemClicked], raftType, Dynamic[raftCell], Dynamic[raftMenu]],
					
				(* Notebook *)
				makeRaftMenuIgnoreItem[ParentNotebook[cell], icon["IgnoreInNotebook"][colour["UIDark"]],
					{"Ignore ", #, " errors in this notebook"}&,
					lint, Dynamic[itemClicked], raftType, Dynamic[raftCell], Dynamic[raftMenu]],
					
				(* init.m *)
				makeRaftMenuIgnoreItem[$FrontEnd, icon["IgnoreAlways"][colour["UIDark"]],
					{"Ignore all ", #, " errors"}&,
					lint, Dynamic[itemClicked], raftType, Dynamic[raftCell], Dynamic[raftMenu]]
			},
			
			delimiter = Graphics[
				{CapForm["Round"], colour["RaftDelimiter"], AbsoluteThickness[1],
					Line[{{-1, 0}, {1, 0}}]},
				AspectRatio -> Full, PlotRange -> {{-1, 1}, {-1, 1}}, ImageMargins -> {{0, 0}, 2{1, 1}},
				ImagePadding -> {5{1, 1}, {0, 0}}, ImageSize -> {Full, 2}],
			
			(* For a mooring lint pod:
				The drop down menu's size is affected by the input/code cell's margins. The menu's ImageSize is Full so that it resizes
				with the window width, but this means that it spills over the window edge seeing as it's drawn inside the lint pod and
				therefore doesn't start at the very LHS of the window. Therefore we must subract the LHS and RHS cell margins from its
				width so that it fits in the pod properly. *)
			sumOfCellHMargins = Replace[Total[CodeInspector`LinterUI`lintedCells[notebookID][cell]["HMargins"]], Except[_?NumberQ] -> 70],
			
			hMarginsFudgeFactor = -2.5
		},
		
		
		Highlighted[
			Column[
				Flatten @ Riffle[
					Replace[{
						(* Code actions. *)
						Function[codeAction,
							makeRaftMenuCodeActionItem[
								cell,
								codeAction,
								Dynamic[CodeInspector`LinterUI`lintedCells[notebookID][cell]["CellContents"]],
								Dynamic[itemClicked],
								raftType, Dynamic[raftCell], Dynamic[raftMenu]]] /@ actionItems,
						
						(* Documentation links. *)
						Function[symbol,
							makeRaftMenuDocsItem[symbol, Dynamic[itemClicked],
								raftType, Dynamic[raftCell], Dynamic[raftMenu]]] /@ symbols,
						
						(* Ingore-lint actions. *)
						ignoreItems
					
					(* Replace {} with Nothing so that you don't get delimiters between empty sections. *)
					}, {} -> Nothing, 1],
					
					(* Include delimiters between the action, docu, and ignore sections. *)
					constrainWidth[delimiter,
						Switch[raftType,
							"inPlace", 383,
							"mooring", Full]]
				],
				ItemSize -> {Full, 0}, Spacings -> 0, Alignment -> Left],
			
			Frame -> True, FrameMargins -> 3, Background -> White, RoundingRadius -> 0,
			FrameStyle -> Directive[AbsoluteThickness[1], colour["RaftFrame"]],
			ImageMargins -> {
				Switch[raftType,
					"inPlace", {0, 0},
					"mooring", {0, sumOfCellHMargins + hMarginsFudgeFactor}],
				{0, 0}}
		]
	]


makeRaftCell[cell_CellObject, lint_CodeInspector`InspectionObject] :=
	With[
		{
			(* Construct the lint raft and state symbols. *)
			head = constructLintVar[cell, lint, "Raft"],
			lintState = constructLintVar[cell, lint, "State"],
			
			lintDescription = lint["Description"],
			notebookID = Last[ParentNotebook[cell]]
		},
		
		(* Store the raft in a down value of the raft symbol, with an argument to specify whether
			the raft is in-place or in the mooring, and an optional Deinitialization argument. *)
		Block[{deinit, raftType},
			SetDelayed @@ Join[
				
				Replace[head, head_ :>
					head[raftType_?(MatchQ["mooring" | "inPlace"]), deinit:(Deinitialization :> _):{None}],
					1],
				
				(* --------------- The raft cell. --------------- *)
				Hold[
					(* The raftType argument is necessary for specifying if the
						lint state variable is set to "hoverMooring" or "hoverInPlace". *)
					With[{hoverValue = Switch[raftType, "mooring", "hoverMooring", "inPlace", "hoverInPlace"]},
						Cell[BoxData @ ToBoxes @
							DynamicModule[{raftOpenQ = False, mouseOver = False, raftMenu, itemClicked = False},
								DynamicWrapper[
									Button[
										Highlighted[
											Grid[
												(* The raft label contains: *)
												{{
													(* an exclamation icon, coloured according to the lint severity, *)
													icon["Exclam"][colour[lint["Severity"]]], Spacer[6],
													(* a description of the lint. This underlines on mouseover, and has a double chevron icon at the end of it. *)
													constrainWidth[
														Style[
															Row[
																{
																	(* Incase the lint description contains any newlines, or it has line-wrapped, we need to add its words
																		individually to the Row, otherwise the "TakeAction" icon will appear floating to the right. Rather, we
																		want it to appear directly after the last word, as if it were itself a character. *)
																	Splice[style["RaftLabel"] /@ StringSplit[lintDescription]],
																	icon["TakeAction"][colour["RaftLabel"]]},
																" ",
																BaselinePosition -> Baseline],
															FontVariations -> {"Underline" -> Dynamic[mouseOver]}],
															
														Switch[raftType,
															"inPlace", UpTo[340],
															"mooring", Full,
															_, UpTo[340]]]}},
															
												ItemSize -> Automatic, Spacings -> {0, 0}, Alignment -> {Left, Baseline},
												BaselinePosition -> Scaled[.05]],
											
											(* Raft appearance options. *)
											Alignment -> {Center, Baseline},
											FrameMargins -> {{5, 5}, {3, 3}}, Frame -> True, RoundingRadius -> 1,
											FrameStyle -> Directive[colour["RaftFrame"], AbsoluteThickness[1]],
											(* Change the background according to the lint state. *)
											Background -> Dynamic[Switch[ReleaseHold[lintState],
												"inactive", colour["RaftBack"],
												"active", colour["RaftBackOpen"],
												"hoverMooring", colour["RaftBackHover"],
												(* If the raft type is in-place, then the "hoverInPlace" value could be due to the mouse hovering over the linted boxes, but
													not the raft itself \[LongDash] so we must check to see if the raft is actually hovered over before highlighting its background. *)
												"hoverInPlace",
												Which[
													raftType === "mooring", colour["RaftBackHover"],
													raftType === "inPlace" && mouseOver, colour["RaftBackHover"],
													True, colour["RaftBack"]],
												(* Failsafe. *)
												_, colour["RaftBack"]]]],
										
										
										(* ----- Button action. ----- *)
										
										(* If there isn't already a raft menu, attach one. *)
										If[!raftOpenQ,
											raftMenu = AttachCell[EvaluationBox[],
											
												ExpressionCell[
													DynamicModule[{},
														makeRaftMenu[cell, lint, ParentCell[EvaluationBox[]], Dynamic[itemClicked], raftType, Dynamic[raftMenu]],
														Initialization :> (1),
														(* Upon closure of the menu, set the lint state to "inactive". This is correct if the mouse has moved out of the menu/raft.
															However, if the menu has been closed by clicking the raft itself, then the state *should* be set to "hoverXXXX" \[LongDash] but this
															happens by the raft's button action after the menu's Deinitialization fires. (In other words, it's fine to potentially
															incorrectly set the lint state to "inactive" here, becuase it will immediately corrected by the raft's button action.) *)
														Deinitialization :> (raftOpenQ = False; Set @@ Append[lintState, "inactive"])],
													Evaluator -> $evaluator],
												
												{Left, Bottom}, Offset[{0, 2}, 0], {Left, Top},
												RemovalConditions -> {"MouseExit"}]];
										
										(* If the raft is already open, and its label is clicked, then close the existing raft menu.
											If not, set the state variables to the open/active states. *)
										If[raftOpenQ,
											NotebookDelete[raftMenu]; Set @@ Append[lintState, hoverValue],
											Set @@ Append[lintState, "active"]; raftOpenQ = True],
												
										Appearance -> None],
									
									
									(* ----- DynamicWrapper action ----- *)
									
									(* Set the lint state to "hoverMooring" or "hoverInPlace" on mouseover (if the lint state is not already "active"). *)
									mouseOver = CurrentValue["MouseOver"];
									If[!MatchQ[ReleaseHold[lintState], "active" | "hoverInPlace"],
										Set @@ Append[lintState, If[mouseOver, hoverValue, "inactive"]]],
									
									(* This only needs to update with CurrentValue["MouseOver"]. *)
									TrackedSymbols :> {}],
								
								Initialization :> (1),		
								Deinitialization :> Last[deinit]],

							Evaluator -> $evaluator]]]]];
		
		(* Return the raft symbol. *)
		ReleaseHold[head]
	]


(* ::Section::Closed:: *)
(*Lint Pod*)


$linterPodCellHMargins = {5, 5};


(* ::Subsection::Closed:: *)
(*Title Bar*)


confirmClosurePopup[cell_CellObject, uiAttachedCells_, Dynamic[popupPresentQ_]] :=
	Cell[BoxData @ ToBoxes @
		DynamicModule[{},
			popupPane[
				(* Draw a button to discard the changes, and a button to apply them. Style the "discard" button with red text. *)
				Row[{
					button[Style["Discard Edits", colour["WarningText"]], NotebookDelete[ParentCell[EvaluationCell[]]]; NotebookDelete[uiAttachedCells]],
					button[Style["Apply Edits"], applyChanges[cell]; NotebookDelete[ParentCell[EvaluationCell[]]]; NotebookDelete[uiAttachedCells]]}, Spacer[5]],
				{225, 57},
				1,
				Alignment -> {Center, Center}],
				
			(* popupPresentQ is used to stop multiple confirmation popup cells being
				attached if the user clicks on the "close" button multiple times. *)
			Initialization :> (1),
			Deinitialization :> (popupPresentQ = False)],
		
		Evaluator -> $evaluator]


popOutButton[] :=
	Button[icon["PopOut"][colour["UIDark"]], Null, Appearance -> None, Tooltip -> "Open in code analysis palette."]


applyChangesButton[cell_CellObject] :=
With[{notebookID = Last[ParentNotebook[cell]]},
	button[
		Style["Apply Edits", 13],
			
		If[CodeInspector`LinterUI`lintedCells[notebookID][cell]["EditsMadeQ"],
			applyChanges[cell]],
		
		"ActiveQ" :> TrueQ[CodeInspector`LinterUI`lintedCells[notebookID][cell]["EditsMadeQ"]],
		"TextColour" -> colour["ApplyButtonText"],
		"TextHoverColour" -> colour["ApplyButtonText"],
		"BackColour" -> colour["ApplyButtonBack"],
		"BackHoverColour" -> colour["ApplyButtonBackHover"],
		"EdgeColour" -> colour["ApplyButtonBack"],
		"EdgeHoverColour" -> colour["ApplyButtonBackHover"],
		ImageSize -> {Automatic, 17}, Alignment -> {Center, Scaled[-.05]}
		]]


titleBar[cell_CellObject] := 
	With[
		{
			roundingRad = $UIRoundingRadius - 1,
			hMargins = CodeInspector`LinterUI`lintedCells[Last[ParentNotebook[cell]]][cell]["HMargins"],
			hMarginsFudgeFactor = {0, 0}
		},
			
			DynamicModule[{popupPresentQ = False},
				Graphics[
					{
						(* Draw the title bar background \[LongDash] a rectangle with its top corners rounded. *)
						colour["UIBack"],
						FilledCurve[BezierCurve[Join[
							(* Top right corner. *)
							Offset[roundingRad(# + {-1, -1}), {1, 1}]& /@ bezierCirclePts["Q1"][[2;;]],
							(* Top left corner. *)
							Offset[roundingRad(# + {1, -1}), {-1, 1}]& /@ bezierCirclePts["Q2"],
							(* Bottom left corner. *)
							Table[{-1, -1}, 3],
							(* Bottom right corner *)
							Table[{1, -1}, 2]]]],
						
						(* The left-aligned pod title. *)
						Text[style["SectionHeader"]["Code Analysis", FontColor->colour["UIDark"]], Offset[{8, 0}, {-1, 0}], {-1, 0}],
						
						(* The right-aligned pop-out button. *)
						(*Inset[popOutButton[], Offset[{-33, 0}, {1, 0}], {Center, Center}],*)
						
						(* The right-aligned Apply button. *)
						(*Inset[applyChangesButton[cell], Offset[{-47, 0}, {1, 0}], {Right, Center}],*)
						
						(* Draw a "close" button at the right of the title bar. *)
						Button[closeIcon[{-13, -10}, {1, 1}],
							
							With[{notebookID = Last[ParentNotebook[cell]]},
							
								(* If edits have been made, attach the closure confirmation popup. Otherwise, just delete the lint pod cells. *)
								If[
									TrueQ[CodeInspector`LinterUI`lintedCells[notebookID][cell]["EditsMadeQ"]],
									
									(* Only attach the popup if there isn't one already present. *)
									If[!popupPresentQ,
										popupPresentQ = True;
										AttachCell[
											EvaluationBox[],
											ExpressionCell[
												confirmClosurePopup[
													cell,
													CodeInspector`LinterUI`lintedCells[notebookID][cell]["UIAttachedCells"],
													Dynamic[popupPresentQ]],
												Evaluator -> $evaluator],
											{Right, Bottom}, Offset[{6, 2}, Automatic], {Right, Top},
											RemovalConditions -> {"MouseClickOutside"}]],
										
									NotebookDelete[
										CodeInspector`LinterUI`lintedCells[notebookID][cell]["UIAttachedCells"]]]]]},
										
					AspectRatio -> Full, ImageSize -> {Full, 20}, PlotRange -> {{-1, 1}, {-1, 1}}, ImageMargins -> {{0, 0}, {0, 0}},
					ImagePadding -> {hMarginsFudgeFactor, {0, 0}}]]]


(* ::Subsection::Closed:: *)
(*Code Pane*)


codePane[cell_CellObject, cellType_] :=
	With[
		{
			notebookID = Last[ParentNotebook[cell]],
			hMargins = CodeInspector`LinterUI`lintedCells[Last[ParentNotebook[cell]]][cell]["HMargins"],
			hMarginsFudgeFactor = {0, 0},
			vContentPadding = {5, 4},
			hContentPadding = {0, 0}
		},
		
		Highlighted[
			Pane[
				Pane[
					isolatedDynamic[
						Dynamic[CodeInspector`LinterUI`lintedCells[notebookID][cell]["MarkedUpCode"]],
						(* Due to bug 407314, we can't just wrap the marked-up boxes in Cell[BoxData[]] seeing as the rafts prematurely disappear,
							so we instead ensure that the marked-up code is a list, and then wrap it in RowBox. *)
						With[{boxes = Flatten@List[CodeInspector`LinterUI`lintedCells[notebookID][cell]["MarkedUpCode"]]},
							(* Style the boxes as input code. *)
							RawBoxes[inputStyle[RowBox[boxes]]]]],
					
					(* The inner Pane width is Full for "Input" cells, and the width of the cell for "Code" cells. *)
					ImageSize -> Switch[cellType,
						{"Input"}, {Full, Full},
						(* Note that a code cell is only going to change width if its contents has changed, and in that case the
							whole lint pod needs to be recalculated. So it is not necessary to dynamically track the cell width. *)
						{"Code"}, {CodeInspector`LinterUI`lintedCells[notebookID][cell]["Width"], Full},
						(* Failsafe *)
						_, {Full, Full}],
					Alignment -> {Left, Top}, FrameMargins -> None, ImageMargins -> {{0, 0}, {0, 0}}, ContentPadding -> False, ImageSizeAction -> "Clip"],
				
				(* The outer Pane width is Full, and will scroll if the inner pane is wider than the notebook window (i.e. in the case of a "Code" cell). *)
				Full,
				
				FrameMargins -> {{0, 0}, {0, 0}},
				ImageMargins -> {{0, 0}, {0, 0}},
				Alignment -> {Left, Top}, ImageSizeAction -> "Scrollable", AppearanceElements -> None, Scrollbars -> {Automatic, False}, ContentPadding -> False],
				
			Background -> colour["CodeBack"], RoundingRadius -> 0, ContentPadding -> False,
			ImageMargins -> {hMarginsFudgeFactor, {0, 0}},
			FrameMargins -> {$linterPodCellHMargins + hContentPadding, vContentPadding},
			Frame -> True, FrameStyle -> Directive[colour["UIBack"], AbsoluteThickness[1]]]]


(* ::Subsection::Closed:: *)
(*Mooring*)


mooring[cell_CellObject, Dynamic[showAllQ_], minRafts_] :=
	With[
		{
			notebookID = Last[ParentNotebook[cell]],
			hMargins = CodeInspector`LinterUI`lintedCells[Last[ParentNotebook[cell]]][cell]["HMargins"],
			hMarginsFudgeFactor = {0, 0},
			hContentPadding = {10, 10}, vContentPadding = {0, 0},
			
			raftColumn = Function[list,
				Column[list, ItemSize -> {0, 0}, Spacings -> 0, Alignment -> Left]]
		},
		
		Highlighted[
			isolatedDynamic[
				Dynamic[CodeInspector`LinterUI`lintedCells[notebookID][cell]["LintRafts"]],
				
				PaneSelector[
					{
						(* Show a column of only minRafts in the condensed (default) view. *)
						False -> raftColumn[
							Riffle[
								RawBoxes /@ Part[
									Through[CodeInspector`LinterUI`lintedCells[notebookID][cell]["LintRafts"]["mooring"]],
									;; UpTo[minRafts], 1, 1],
								Spacer[{1, 5}]]],
					
						(* Show a column of all the rafts in the expanded view. *)
						True -> raftColumn[
							Riffle[
								RawBoxes /@ Part[
									Through[CodeInspector`LinterUI`lintedCells[notebookID][cell]["LintRafts"]["mooring"]],
									All, 1, 1],
								Spacer[{1, 5}]]]},
					
					(* Show the expanded view if showAllQ has been set to True by the "Show All" button, or if the total
						number of rafts is less than or equal to minRafts. *)
					Dynamic[Or[
						showAllQ,
						TrueQ[Length[CodeInspector`LinterUI`lintedCells[notebookID][cell]["LintRafts"]] <= minRafts]]],
					
					ImageSize -> Automatic]],
			
			
			Background -> colour["UIBack"], RoundingRadius -> 0, ImageSize -> Full,
			FrameMargins -> {$linterPodCellHMargins, vContentPadding},
			ImageMargins -> {hMarginsFudgeFactor, {0, 0}}]]


(* ::Subsection::Closed:: *)
(*Footer Bar*)


showAllButton[Dynamic[showAllQ_], cell_CellObject] :=
With[{formatIcon = Function[Show[#, ImageSize -> {13, 9}, BaselinePosition -> Scaled[.1]]]},
	button[
		(* Switch between "Show All v" and "Show Fewer ^" depending on showAllQ. *)
		PaneSelector[
			{
				False -> Row[{Style["Show All", 12], formatIcon[icon["DownChevron"][colour["UIDark"]]]}, " "],
				True -> Row[{Style["Show Fewer", 12], formatIcon[icon["UpChevron"][colour["UIDark"]]]}, " "]
			},
			Dynamic[showAllQ],
			ImageSize -> Automatic, ImageMargins -> 0, FrameMargins -> None, BaselinePosition -> (*(Scaled[0] -> Baseline)*)Baseline],

		(* The action of this button is just to toggle showAllQ. *)
		showAllQ = !TrueQ[showAllQ],

		ImageSize -> {Automatic, 16}, BaselinePosition -> (*(Scaled[.25] -> Baseline)*)Baseline, Alignment -> {Center, Scaled[-.05]}]]


footerBar[cell_CellObject, Dynamic[showAllQ_], minRafts_] :=
	With[
		{
			roundingRad = $UIRoundingRadius - 1,
			hMargins = CodeInspector`LinterUI`lintedCells[Last[ParentNotebook[cell]]][cell]["HMargins"],
			hMarginsFudgeFactor = {0, 0},
			footerHeight = 21,
			notebookID = Last[ParentNotebook[cell]]
		},
		
		DynamicModule[{raftCount = 0},
			DynamicWrapper[
				Graphics[
					{
						colour["UIBack"],
						FilledCurve[BezierCurve[Join[
							(* Top left corner. *)
							Table[{-1, 1}, 2],
							(* Bottom left corner. *)
							Offset[roundingRad(# + {1, 1}), {-1, -1}]& /@ bezierCirclePts["Q3"],
							(* Bottom right corner. *)
							Offset[roundingRad(# + {-1, 1}), {1, -1}]& /@ bezierCirclePts["Q4"],
							(* Top right corner *)
							Table[{1, 1}, 2]]]],
						
						(* The "Apply Edits" button. *)
						Inset[applyChangesButton[cell], Offset[{-6, 1}, {1, 0}], {Right, Center}],
						
						(* The lint count and "Show All" button. *)
						Inset[
							Row[style["FooterText"] /@ {

								Spacer[3],
								
								(* Lint count. *)
								"Showing", " ",
								Dynamic[If[showAllQ, raftCount, Clip[raftCount, {0, minRafts}]]], " ",
								"of", " ",
								Dynamic[raftCount], " ",
								Dynamic[If[raftCount === 1, "Issue", "Issues"], FontSize -> 12],
								".",
								
								Spacer[5],
								
								(* Only display the "Show All" button if the raft count is more than minRafts. *)
								PaneSelector[
									{True -> "",
										False -> showAllButton[Dynamic[showAllQ], cell]},
											
									Dynamic[TrueQ[raftCount <= minRafts]],
									
									ImageSize -> Automatic, ImageMargins -> 0, FrameMargins -> None]
							}],
								
							Offset[{5, 2.5}, {-1, -1}], {-1, -1}]
					},
									
					AspectRatio -> Full, ImageSize -> {Full, footerHeight}, PlotRange -> {{-1, 1}, {-1, 1}},
					ImageMargins -> {{0, 0}, {0, 0}},
					ImagePadding -> {hMarginsFudgeFactor, {0, 0}}],
				
				raftCount = Length[CodeInspector`LinterUI`lintedCells[notebookID][cell]["LintRafts"]]]]]


(* ::Subsection::Closed:: *)
(*Hash-Changed Overlay*)


SetAttributes[hashChangedOverlayButton, HoldRest]


hashChangedOverlayReanalyseButton[cell_CellObject] :=
	button["Reanalyze", AttachAnalysis[{cell}], ImageSize -> {98, 19}]


hashChangedOverlayClosePodButton[cell_CellObject] :=
	button["Close",
		NotebookDelete[EvaluationCell[]]; NotebookDelete[CodeInspector`LinterUI`lintedCells[Last[ParentNotebook[cell]]][cell]["UIAttachedCells"]],
		ImageSize -> {98, 19}]


hashChangedOverlay[cell_] :=
	Pane[
		Column[{
			style["SectionHeader"]["The cell contents have changed."],
			Row[
				{
					hashChangedOverlayClosePodButton[cell],
					hashChangedOverlayReanalyseButton[cell]},
				Spacer[10]]
		}, Spacings -> 1.1, Alignment -> Center],
		ImageMargins -> {{0, 0}, {0, 16}}]


(* ::Subsection::Closed:: *)
(*Assembled Pod*)


lintPod[cell_CellObject, cellType_] :=
	With[
		{
			hMargins = CodeInspector`LinterUI`lintedCells[Last[ParentNotebook[cell]]][cell]["HMargins"],
			delimiter = Function[vMargins, Graphics[
				{AbsoluteThickness[1], colour["Delimiter"], CapForm["Round"],
					Line[{{-1, 0}, {1, 0}}]},
				AspectRatio -> Full, PlotRange -> {{-1, 1}, {-1, 1}}, ImagePadding -> {(*10{1, 1}*){0, 0}, {0, 0}}, ImageSize -> {Full, 2},
				BaselinePosition -> Scaled[.1], ImageMargins -> {{0, 0}, vMargins}]],
			minRafts = 2,
			notebookID = Last[ParentNotebook[cell]]
		},
		
		DynamicModule[{showAllQ = False, cellHashChangedQ = False},
		Module[
			{contents =
				(* Stack all the lint pod components *)
				Column[
									{
										titleBar[cell],
										delimiter[{4, 0}],
										codePane[cell, cellType],
										delimiter[{5, 4}],
										mooring[cell, Dynamic[showAllQ], minRafts],
										delimiter[{4, 5}],
										footerBar[cell, Dynamic[showAllQ], minRafts]},
									ItemSize -> {Full, 0}, Spacings -> 0]},
									
			DynamicWrapper[
				Overlay[
					{
						(* The main linting pod body. *)
						Highlighted[
							(*(* Stack all the lint pod components *)
							Style[
								Column[
									{
										titleBar[cell],
										delimiter[{4, 0}],
										codePane[cell, cellType],
										delimiter[{5, 4}],
										mooring[cell, Dynamic[showAllQ], minRafts],
										delimiter[{4, 5}],
										footerBar[cell, Dynamic[showAllQ], minRafts]},
									ItemSize -> {Full, 0}, Spacings -> 0],
									
								Opacity[Dynamic @ If[cellHashChangedQ, .5, 1]]]*)contents,
								
							(* Add a border around the lint pod. *)
							RoundingRadius -> $UIRoundingRadius, Background -> colour["UIBack"], FrameMargins -> 0,
							Frame -> True, FrameStyle -> Directive[AbsoluteThickness[1], colour["UIEdge"]], 
							ImageMargins -> {$linterPodCellHMargins, {0, 0}}],
						
						(* The "inactive" overlay colour. *)
						Highlighted[Invisible[contents],
							(* Add a border around the lint pod. *)
							RoundingRadius -> $UIRoundingRadius, Background -> Opacity[.8, colour["UIBack"]], FrameMargins -> 0,
							Frame -> True, FrameStyle -> Directive[AbsoluteThickness[1], Lighter[colour["UIEdge"], .5]], 
							ImageMargins -> {$linterPodCellHMargins, {0, 0}}],
						
						(* The controls for choosing whether to close the pod, or reanalyse the cell if the cell hash has changed. *)
						hashChangedOverlay[cell]},
					
					(* If the cell hash has changed, display the linter pod body and the "Cell Has Changed" overlay.
						Otherwise, just display the body. *)
					(*Dynamic[If[cellHashChangedQ, {1, 2}, {1}]]*)Dynamic[If[cellHashChangedQ, {1, 2, 3}, {1}]],
					(* Ensure that the correct layer is interactable. *)
					(*Dynamic[If[cellHashChangedQ, 2, 1]]*)Dynamic[If[cellHashChangedQ, 3, 1]],
					Alignment -> {Center, Top}],
				
				
				(* Check if the original input/output cell has been modified. *)
				cellHashChangedQ =
					(FrontEndExecute[FrontEnd`CryptoHash[cell]][[2, -1]] =!= CodeInspector`LinterUI`lintedCells[notebookID][cell]["Hash"]),
				UpdateInterval -> .5]]]]


(* ::Section::Closed:: *)
(*UI Generation Actions*)


(* ::Text:: *)
(*Most lint sources are going to be a part spec that points to a single token. However, for errors that involve implicit tokens, such as implicit times and implicit Null, the InspectionObject provides the source as After[{XXXX}] or Before[{XXXX}], where XXXX points to the entire expression before or after the implicit token. Seeing as we always just want to underlight one token, and we can't underlight the implicit token itself, we need to find the token that's directly adjacent to the implicit token. This is done using findToken, which achieves this by recursively taking the first or last part of the code boxes pointed to by the XXXX in After[{XXXX}] or Before[{XXXX}].*)


findToken[codeBoxes_, sourceRaw_] :=
	With[
		{source = Replace[sourceRaw, {} -> {0}]},
		{
			maxRecursions = 1000,
			(* recursivePart:
				* 0 means we already have an appropriate source,
				* -1 means we want to recursively find the last of the last of the last etc part, seeing as the implicit token comes after the provided source,
				* 1 means we want to recursively find the first of the first of the first etc part, seeing as the implicit token comes before the provided source. *)
			partToRecursivelyTake = Switch[source, _List, None, _After, -1, _Before, 1]
		},
		
		If[partToRecursivelyTake === None,
			(* source is already a single token. *)
			source,
			
			(* source is adjacent to an implicit token. *)
			NestWhile[
				Function[newSource, Append[newSource, partToRecursivelyTake]],
				First[source],
				(* When the expression becomes atomic, we've reached the last token. *)
				Function[newSource, !AtomQ[codeBoxes[[Sequence @@ newSource]]]],
				1, maxRecursions]]]


markupCode[cell_CellObject, lint_CodeInspector`InspectionObject, codeBoxes_] :=
	Module[
		(* Construct the list of sources at which linting markup needs to be done. *)
		(* ReverseSort to markup the deepest lints first, although Sources and AdditionalSources should be at the same depth. *)
		{sources = ReverseSort[
			(* findToken ensures that each source points to only one token. *)
			findToken[codeBoxes, #]& /@ Prepend[
				Replace[Last[lint]["AdditionalSources"], _Missing -> {}],
				Last[lint][CodeParser`Source]]]},
		
		(* Markup each lint source. *)
		Block[{raftAttachedQ, mouseOver}, Fold[
			MapAt[Function[source,
				With[
					(* Construct a unique state variable for the lint. *)
					{lintState = constructLintVar[cell, lint, "State"],
						(* Construct the unique variable in which the lint raft will be stored. *)
						raft = constructLintVar[cell, lint, "Raft"]},
				
					(* Set the initial state of the lint as "inactive", meaning that the linted boxes are neither being hovered
						over ("hoverInPlace" or "hoverMooring"), nor is their raft open ("active"). Note that rafts appear both in-place in the marked-up
						code, and in the raft mooring at the bottom, so it is necessary for them to share state variables. *)
					Set @@ Append[lintState, "inactive"];
					
					(* Wrap the linted boxes in a DynamicWrapper that will manage hover effects and attachment of raft cells. *)
					DynamicModuleBox[{raftAttachedQ = False, mouseOver = False},
						Evaluate @ DynamicWrapperBox[
							(* Underlight the linted boxes, and tie their background to the unique state variable for that lint. *)
							StyleBox[source,
								FontVariations -> {"Underlight" -> colour[lint["Severity"]]},
								(* Highlight the linted boxes if the lint state is "hoverXXXX" or "active". *)
								Background -> Dynamic[Switch[ReleaseHold[lintState],
									(* The severity 1 colour is a bit dark for highlighting compared to the others, so replace it with Pink. *)
									"inactive", Opacity[.2, Replace[colour[lint["Severity"]], RGBColor[0.827451, 0.00392157, 0.00392157] -> Pink]],
									"hoverInPlace" | "hoverMooring" | "active", colour["CodeHighlight"],
									(* Failsafe. *)
									_, None]]],
							
							mouseOver = CurrentValue["MouseOver"];
							(* Update the lint state variable so that other instances of this lint (other sources, and rafts in
								the mooring) know whether this lint is being hovered over. "active" takes presedence over "hoverXXXX". *)
							If[mouseOver && !MatchQ[ReleaseHold[lintState], "active" | "hoverMooring"], Set @@ Append[lintState, "hoverInPlace"]];
							(* Attach the lint raft on mouseover of the linted boxes (given the absence of an existing raft). *)
							If[mouseOver && !raftAttachedQ,
								raftAttachedQ = True;
								AttachCell[EvaluationBox[],
									ReleaseHold[raft]["inPlace", Deinitialization :> (raftAttachedQ = False; Set @@ Append[lintState, "inactive"])],
									
									(* Check the mouse position and anchor the raft such that it isn't clipped by a window edge. *)
									Sequence @@ With[{mousePos = First[Replace[MousePosition["WindowScaled"], None -> {.5, 0}]]},
										(* If the spawn point is: *)
										Which[
											(* In the left-third of the window, then left-align the lint token and raft. *)
											Between[mousePos, {0, .33}],
											{{Left, Bottom}, {0, 0}, {Left, Top}},
											(* In the middle-third of the window, then center-align the lint token and raft. *)
											Between[mousePos, {.33, .67}],
											{{Center, Bottom}, {0, 0}, {Center, Top}},
											(* In the right-third of the window, then right-align the lint token and raft. *)
											Between[mousePos, {.67, 1}],
											{{Right, Bottom}, {0, 0}, {Right, Top}},
											(* Failsafe. *)
											True,
											{{Center, Bottom}, {0, 0}, {Center, Top}}]],
									
									RemovalConditions -> {"MouseExit"}]],
									
							(* This only needs to update with CurrentValue["MouseOver"]. *)
							TrackedSymbols :> {}],
						
						DynamicModuleValues :> {}]]], (*Folded Expression*)#1, (*Part Spec*)#2]&,
			
			(* Wrap the styling function around the boxes of the cell... *)
			codeBoxes,
			(* ...at each of the lint sources. *)
			sources]]]


(* ::Text:: *)
(*getCellInfo returns ``CellObject -> Association``, where the association contains of all the information about a cell needed for the linter UI. It returns Nothing if a cell produces no lints.*)


getCellInfo[cell_] :=
	Module[{lints, contents = NotebookRead[cell]},
		(* Check that the cell is of the expected form, and that its BoxData produces lints. *)
		If[!MatchQ[contents, Cell[BoxData[_], ___]] || (lints = CodeInspector`CodeInspectBox[First[First[contents]]]) === {},
			Nothing,
			cell -> <|
				(* The entire cell expression, upon which code actions will be applied. The value of this key will change when
					an edit is made via a raft menu item (i.e. "Replace this thing", "Delete this thing", etc.). *)
				"CellContents" -> contents,
				(* "MarkedUpCode" is the box contents of the cell, marked up (underlights, highlighting, etc.) according to its lints. *)
				"MarkedUpCode" -> (
					Fold[Function[{codeBoxes, lint}, markupCode[cell, lint, codeBoxes]],
						(* Start with the BoxData contents of the cell... *)
						First[First[contents]],
						(* ...and mark it up according to a lint, carrying the markup forwards to the next markup application. *)
						(* We reverse sort by source so that the deepest lints are marked up first, and thus avoid invalidating the part specs for other lints. *)
						ReverseSortBy[lints, Last[#][CodeParser`Source]&]]),
				(* "LintRafts" are raft cells that will appear in place under marked-up boxes, and in the mooring.
					There are two species, raftCell["inPlace"] and raftCell["mooring"]. *)
				"LintRafts" -> (makeRaftCell[cell, #]& /@ lints),
				(* "Hash" is the cell's "ShiftEnterHash" and will be used to check if a cell has been modified after
					analysis, and thus if the analysis needs to be refreshed. *)
				"Hash" -> FrontEndExecute[FrontEnd`CryptoHash[cell]][[2, -1]],
				(* "EditsMadeQ" switches to True if a CodeAction has been applied to the code copy. *)
				"EditsMadeQ" -> False,
				(* The linting pods for Code cells and Input cells will be rendered slightly
					differently, so we need to know what the cell type is. *)
				"Type" -> CurrentValue[cell, CellStyle],
				(* "Width" and "H(orizontal)Margins" are used to set the size of the linting pod. *)
				"Width" -> AbsoluteCurrentValue[cell, CellSize],
				"HMargins" -> First[AbsoluteCurrentValue[cell, CellMargins]]|>]]


analyseAction[
	HoldPattern[notebookOrCells_:EvaluationNotebook[]]
] /; MatchQ[notebookOrCells, _NotebookObject | {__CellObject}] :=
	Module[
		(* If the arg is a list of cells, then just assign it to cellsToLint.
			Otherwise, the arg is a notebook object, so retrieve the cells in that notebook and assign to cellsToLint. *)
		{cellsToLint = If[ListQ[notebookOrCells], notebookOrCells, Cells[notebookOrCells]]},
		
		(* We only want to analyse "Input" and "Code" cells, so generate a list of all those cells in the notebook / given list of cells. *)
		cellsToLint = Select[cellsToLint, MatchQ[CurrentValue[#, CellStyle], {"Code"} | {"Input"}]&];
		
		(* Create an association in which the keys are ("Input" and "Code") cell objects, and the values are
			associations of data for each cell, needed for the linter UI. *)
		cellsToLint = Association[getCellInfo /@ cellsToLint];
		
		(* CodeInspector`LinterUI`lintedCells[notebookID] is where the associations of info for linted cells are stored for a
			given notebook. Note that when a code action is applied, the original "Input" or "Code" cell remains untouched,
			and changes are stored in CodeInspector`LinterUI`lintedCells[notebookID][cell]["CellContents"]. This happens in
			the makeRaftMenuCodeActionItem function, and you can see that
			CodeInspector`LinterUI`lintedCells[notebookID][cell]["CellContents"] is passed to this function in makeRaftMenu.
			This continues to get updated as more code actions are applied, and finally it gets written into the original "Input"
			or "Code" cell when the user clicks the Apply button. *)
		(* Note that CodeInspector`LinterUI`lintedCells is itself an Association, where the keys are notebook IDs. *)
		With[{notebookID = If[ListQ[notebookOrCells], Last[ParentNotebook[First[notebookOrCells]]], Last[notebookOrCells]]},
			(* If the lintedCells var isn't already an Association, make it one. *)
			If[!AssociationQ[CodeInspector`LinterUI`lintedCells],
				Clear[CodeInspector`LinterUI`lintedCells];
				CodeInspector`LinterUI`lintedCells = <||>];
			
			If[!ListQ[notebookOrCells],
			
				(* If the arg is a notebook, then we can be sure that cellsToLint is the complete Association of lintable
					cells for that notebook (and not a subset). Therefore, just do a simple assignment. *)
				CodeInspector`LinterUI`lintedCells[notebookID] = cellsToLint,
				
				(* However, if analysisAction has been given a list of cells, then there's no guarantee that this represents the complete set
					of linted cells for the parent notebook. Therefore, merge cellsToLint with the existing lintedCells Association. *)
				(* ...But first ensure that CodeInspector`LinterUI`lintedCells[notebookID] is an Association, so that it can be merged. *)
				(* Check. *)
				If[!AssociationQ[CodeInspector`LinterUI`lintedCells[notebookID]],
					CodeInspector`LinterUI`lintedCells[notebookID] = <||>];
				(* Merge. *)
				CodeInspector`LinterUI`lintedCells[notebookID] =
					Merge[{CodeInspector`LinterUI`lintedCells[notebookID], cellsToLint}, Last]];
			
			(* Return only the newly-linted cells Association. *)
			cellsToLint]
	]


AttachAnalysis[
	HoldPattern[notebookOrCells_:EvaluationNotebook[]]
] /; MatchQ[notebookOrCells, _NotebookObject | {__CellObject}] :=
	Module[
		{cells, notebookID, hMargins, hMarginsFudgeFactor = {-13, (*-13*)4}, vMargins = {5, 5}, podCell, bracketCell},

		(* These should already be loaded, but just make sure. *)
		Needs["CodeParser`"];
		Needs["CodeInspector`"];
		
		If[ListQ[notebookOrCells],
			(* If the arg is a list of cells, then assign it to cells and get the notebook ID.
				This assumes that all cells are from the same notebook. *)
			cells = notebookOrCells;
			notebookID = Last[ParentNotebook[First[cells]]],
			(* If the arg is a notebook, then calculate the notebook ID and assign the existing
				linted cells to ``cells``, returning {} if the notebook has not yet been analysed. *)
			notebookID = Last[notebookOrCells];
			cells = Quiet @ Replace[Keys[CodeInspector`LinterUI`lintedCells[notebookID]], Except[_List] -> {}]];
		
		(* Delete any existing attached lint pod cells. *)
		Quiet @ NotebookDelete[
			Flatten @ Through[
				Map[
					CodeInspector`LinterUI`lintedCells[notebookID],
					cells
				]["UIAttachedCells"]]];
		
		(* Analyse the notebook / cells. *)
		cells = Keys[analyseAction[notebookOrCells]];
		
		(* Attach the lint pods. *)
		Map[
			Function[cell,
				hMargins = CodeInspector`LinterUI`lintedCells[Last[ParentNotebook[cell]]][cell]["HMargins"];
				(* The lint pod cell. *)
				podCell = 
					AttachCell[
						cell,
						ExpressionCell[
							lintPod[cell, CodeInspector`LinterUI`lintedCells[notebookID][cell]["Type"]],
							LineBreakWithin -> Automatic,
							CellMargins -> {hMargins + hMarginsFudgeFactor, vMargins},
							Evaluator -> $evaluator],
						"Inline"];
				
				(* Also attach a marker on the input/code cell bracket that takes you to the lint pod when clicked. *)
				bracketCell = 
					With[{podCellBurnt = podCell},
						AttachCell[
							cell,
							ExpressionCell[
								Button[icon["GoToPod"][colour["CellBracketMarker"]],
									SelectionMove[podCellBurnt, All, Cell],
									Appearance -> None],
								Evaluator -> $evaluator],
							{"CellBracket", Top}]];

				CodeInspector`LinterUI`lintedCells[notebookID][cell]["UIAttachedCells"] = {podCell, bracketCell};
				
				(* Return an Association in which keys are the linted input/code cells, and values are the attached UI cells. *)
				cell -> {podCell, bracketCell}],

			cells] // Association]


(* ::Section::Closed:: *)
(*Package Footer*)


End[]
EndPackage[]


(* ::Section:: *)
(*Testing*)


(* ::Input:: *)
(*CreatePalette[Pane[Column[{*)
(*Button["Analyze NB", AttachAnalysis[InputNotebook[]]],*)
(*Button["Analyze Cell(s)",AttachAnalysis[SelectedCells[InputNotebook[]]]]*)
(*}], FrameMargins -> 30], WindowTitle->"Analyze Code"]*)
