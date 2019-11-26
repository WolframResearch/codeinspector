BeginPackage["Lint`AbstractRules`"]

$DefaultAbstractRules


Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]
Needs["Lint`"]
Needs["Lint`Format`"]
Needs["Lint`Utils`"]



(*

Rules are of the form: pat -> func where pat is the node pattern to match on and func is the processing function for the node.

Functions are of the form: function[pos_, ast_] where pos is the position of the node in the AST, and ast is the AST itself.
  And function must return a list of Lints. 


A rule of thumb is to make patterns as specific as possible, to offload work of calling the function.

*)

$DefaultAbstractRules = <|


CallNode[LeafNode[Symbol, "String" | "Integer" | "Real" | "True", _], _, _] -> scanBadCalls,

(*

not a good scan

CallNode[SymbolNode["Failure", _, _], _, _] -> scanFailureCalls,
*)

(*
Tags: Control
*)
LeafNode[Symbol, "Return" | "Break" | "Continue", _] -> scanControls,


CallNode[LeafNode[Symbol, "Pattern", _], _, _] -> scanPatterns,

(*
Tags: WhichArguments SwitchWhichConfusion
*)
CallNode[LeafNode[Symbol, "Which", _], _, _] -> scanWhichs,

(*
Tags: SwitchArguments SwitchWhichConfusion OperatingSystemLinux
*)
CallNode[LeafNode[Symbol, "Switch", _], _, _] -> scanSwitchs,

(*
Tags: IfArguments
*)
CallNode[LeafNode[Symbol, "If", _], _, _] -> scanIfs,

(*
Tags: DuplicateKeys
*)
CallNode[LeafNode[Symbol, "Association", _], _, _] -> scanAssocs,

CallNode[LeafNode[Symbol, "List", _], { CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _], _, _]... }, _] -> scanRules,

(*
Tags: 
*)
CallNode[LeafNode[Symbol, "Module", _], _, _] -> scanModules,

(*
Tags: 
*)
CallNode[LeafNode[Symbol, "DynamicModule", _], _, _] -> scanDynamicModules,

(*
Tags: 
*)
CallNode[LeafNode[Symbol, "With", _], _, _] -> scanWiths,

(*
Tags: 
*)
CallNode[LeafNode[Symbol, "Block" | "Internal`InheritedBlock", _], _, _] -> scanBlocks,

(*
Tags: 
*)
(*
1-arg Optional[] is ok to have named patterns
Only scan 2-arg Optionals
*)
CallNode[LeafNode[Symbol, "Optional", _], {_, _}, _] -> scanOptionals,

(*
Tags: 
*)
(*

experimental

must handle Condition

CallNode[LeafNode[Symbol, "Replace" | "ReplaceAll" | "ReplaceRepeated", _], _, _] -> scanReplaces,
*)

(*
Scan some symbols that are intuitive, yet do not exist
*)
LeafNode[Symbol, "AnyFalse" | "AllFalse" | "Failed" | "Boolean" | 
                  "RealQ" | "FalseQ" | "RationalQ" | "ComplexQ" | 
                  "SymbolQ" | "Match", _] -> scanBadSymbols,

(*
Scan symbols that are in System` but are undocumented
*)
LeafNode[Symbol, "Absolute" | "ActionDelay" | "ActionMenuBox" | "ActionMenuBoxOptions" | 
                  "ActiveItem" | "AlgebraicRulesData" | "AlignmentMarker" |
                  "AllowAdultContent" | "AllowIncomplete" | "AllowKernelInitialization" | 
                  "Analytic" | "AnimatorBox" | "AnimatorBoxOptions" | "AnimatorElements" | 
                  "Annotate" | "AnnotationNames" | "AnnotationValue" | "AppendCheck" | 
                  "ArgumentCountQ" | "Arrow3DBox" | "ArrowBox" | "Authenticate" | 
                  "AutoEvaluateEvents" | "AutoIndentSpacings" | "AutoMatch" | 
                  "AutomaticImageSize" | "AutoNumberFormatting" | "AutoQuoteCharacters" | 
                  "AutoScaling" | "AutoStyleOptions" | "AutoStyleWords" | 
                  "BackgroundAppearance" | "BackgroundTasksSettings" | 
                  "Backsubstitution" | "BeginFrontEndInteractionPacket" | 
                  "BezierCurve3DBox" | "BezierCurve3DBoxOptions" | "BezierCurveBox" | 
                  "BezierCurveBoxOptions" | "BlankForm" | "Bounds" | "Box" | 
                  "BoxDimensions" | "Boxes" | "BoxForm" | "BoxID" | "BoxRotation" | 
                  "BoxRotationPoint" | "Bra" | "BraKet" | "BrowserCategory" | 
                  "BSplineCurve3DBox" | "BSplineCurve3DBoxOptions" | "BSplineCurveBox" | 
                  "BSplineCurveBoxOptions" | "BSplineSurface3DBox" | 
                  "BSplineSurface3DBoxOptions" | "ButtonCell" | "ButtonContents" | 
                  "ButtonStyleMenuListing" | "CachedValue" | "CacheGraphics" | 
                  "CardinalBSplineBasis" | "CellBoundingBox" | "CellContents" | 
                  "CellElementsBoundingBox" | "CellElementSpacings" | 
                  "CellEvaluationLanguage" | "CellLabelTemplate" | "ChangeOptions" | 
                  "ChannelHistoryLength" | "ChannelListenerWait" | "ChartElementData" | 
                  "ChartElementDataFunction" | "CheckAll" | "CheckboxBox" | 
                  "CheckboxBoxOptions" | "CircleBox" | "ClipboardNotebook" | 
                  "ClockwiseContourIntegral" | "Closed" | "ClosingEvent" | 
                  "CloudObjectInformation" | "CloudObjectInformationData" | "Coarse" | 
                  "CoefficientDomain" | "ColonForm" | "ColorSetterBox" | 
                  "ColorSetterBoxOptions" | "ColumnBackgrounds" | 
                  "CompletionsListPacket" | "ComponentwiseContextMenu" | 
                  "CompressedData" | "ConeBox" | "ConicHullRegion3DBox" | 
                  "ConicHullRegionBox" | "Connect" | "ConsoleMessage" | 
                  "ConsoleMessagePacket" | "ConsolePrint" | "ContentsBoundingBox" | 
                  "ContextMenu" | "Continuation" | "ContourIntegral" | "ContourSmoothing" | 
                  "ControlAlignment" | "ControlGroupContentsBox" | "ControllerDuration" | 
                  "ControllerInformationData" | "ConvertToBitmapPacket" | 
                  "ConvertToPostScript" | "ConvertToPostScriptPacket" | "Cookies" | 
                  "CopyTag" | "CounterBox" | "CounterBoxOptions" | 
                  "CounterClockwiseContourIntegral" | "CounterEvaluator" | 
                  "CounterStyle" | "CreateDataStructure" | "CreatePalettePacket" | 
                  "CuboidBox" | "CurlyDoubleQuote" | "CurlyQuote" | 
                  "CurrentlySpeakingPacket" | "CylinderBox" | "DampingFactor" | 
                  "DataCompression" | "DataStructure" | "DataStructureQ" | 
                  "DateDelimiters" | "DebugTag" | "Decimal" | "DeclareKnownSymbols" | 
                  "DefaultControlPlacement" | "DefaultFormatTypeForStyle" | 
                  "DefaultInputFormatType" | "DefaultOutputFormatType" | "DefaultStyle" | 
                  "DefaultTextFormatType" | "DefaultTextInlineFormatType" | 
                  "DefaultValue" | "DefaultValues" | "DefineExternal" | 
                  "DegreeLexicographic" | "DegreeReverseLexicographic" | 
                  "DeleteWithContents" | "DelimitedArray" | "DestroyAfterEvaluation" | 
                  "DeviceOpenQ" | "DialogIndent" | "DialogLevel" | "DifferenceOrder" | 
                  "DigitBlockMinimum" | "DisableConsolePrintPacket" | "DiskBox" | 
                  "DispatchQ" | "DisplayFlushImagePacket" | "DisplayRules" | 
                  "DisplaySetSizePacket" | "DisplayTemporary" | "DisplayWith" | 
                  "DisplayWithRef" | "DisplayWithVariable" | "DistributionDomain" | 
                  "Divergence" | "DocumentGeneratorInformationData" | 
                  "DomainRegistrationInformation" | "DOSTextFormat" | 
                  "DoubleContourIntegral" | "DoublyInfinite" | "Down" | "DrawEdges" | 
                  "DrawFrontFaces" | "DrawHighlighted" | "DualLinearProgramming" | 
                  "DumpGet" | "DynamicBox" | "DynamicBoxOptions" | "DynamicLocation" | 
                  "DynamicModuleBox" | "DynamicModuleBoxOptions" | "DynamicModuleParent" | 
                  "DynamicName" | "DynamicNamespace" | "DynamicReference" | 
                  "DynamicUpdating" | "DynamicWrapperBox" | "DynamicWrapperBoxOptions" | 
                  "EdgeCapForm" | "EdgeColor" | "EdgeDashing" | "EdgeJoinForm" | 
                  "EdgeOpacity" | "EdgeThickness" | "EditButtonSettings" | 
                  "EliminationOrder" | "EllipticReducedHalfPeriods" | "EmbeddingObject" | 
                  "EmphasizeSyntaxErrors" | "Empty" | "EnableConsolePrintPacket" | 
                  "EndAdd" | "EndFrontEndInteractionPacket" | "EngineEnvironment" | 
                  "Enter" | "EqualColumns" | "EqualRows" | "EquatedTo" | "ErrorBoxOptions" | 
                  "ErrorNorm" | "ErrorPacket" | "ErrorsDialogSettings" | "Evaluated" | 
                  "EvaluationMode" | "EvaluationOrder" | "EventEvaluator" | 
                  "EventHandlerTag" | "ExactRootIsolation" | "ExitDialog" | 
                  "ExpectationE" | "ExportPacket" | "ExpressionPacket" | "ExpressionUUID" | 
                  "ExternalCall" | "ExternalFunctionName" | "Fail" | 
                  "FEDisableConsolePrintPacket" | "FEEnableConsolePrintPacket" | 
                  "FileHandler" | "FileInformation" | "FileName" | "FilledCurveBox" | 
                  "FilledCurveBoxOptions" | "Fine" | "FitAll" | "FlashSelection" | 
                  "FlushPrintOutputPacket" | "FoldWhile" | "FoldWhileList" | "Font" | 
                  "FontName" | "FontOpacity" | "FontPostScriptName" | "FontReencoding" | 
                  "FormatRules" | "FormatValues" | "FrameInset" | "Frameless" | 
                  "FrontEndObject" | "FrontEndResource" | "FrontEndResourceString" | 
                  "FrontEndValueCache" | "FrontEndVersion" | "FrontFaceColor" | 
                  "FrontFaceOpacity" | "FullAxes" | "Generic" | 
                  "GeometricTransformation3DBox" | 
                  "GeometricTransformation3DBoxOptions" | "GeometricTransformationBox" | 
                  "GeometricTransformationBoxOptions" | "GestureHandlerTag" | 
                  "GetBoundingBoxSizePacket" | "GetContext" | "GetFileName" | 
                  "GetFrontEndOptionsDataPacket" | "GetLinebreakInformationPacket" | 
                  "GetMenusPacket" | "GetPageBreakInformationPacket" | 
                  "GlobalPreferences" | "GlobalSession" | "GraphElementData" | 
                  "Graphics3DBox" | "Graphics3DBoxOptions" | "GraphicsBaseline" | 
                  "GraphicsBox" | "GraphicsBoxOptions" | "GraphicsColor" | 
                  "GraphicsComplex3DBox" | "GraphicsComplex3DBoxOptions" | 
                  "GraphicsComplexBox" | "GraphicsComplexBoxOptions" | 
                  "GraphicsContents" | "GraphicsData" | "GraphicsGridBox" | 
                  "GraphicsGroup3DBox" | "GraphicsGroup3DBoxOptions" | 
                  "GraphicsGroupBox" | "GraphicsGroupBoxOptions" | "GraphicsGrouping" | 
                  "GraphicsHighlightColor" | "GraphicsStyle" | "GraphRoot" | 
                  "GridBoxAlignment" | "GridBoxBackground" | "GridBoxDividers" | 
                  "GridBoxFrame" | "GridBoxItemSize" | "GridBoxItemStyle" | 
                  "GridBoxOptions" | "GridBoxSpacings" | "GridElementStyleOptions" | 
                  "GroupTogetherGrouping" | "GroupTogetherNestedGrouping" | "HalfSpace" | 
                  "HeadCompose" | "HelpBrowserLookup" | "HelpBrowserNotebook" | "Hessian" | 
                  "HexahedronBox" | "HexahedronBoxOptions" | "HomePage" | "Horizontal" | 
                  "HorizontalForm" | "HorizontalScrollPosition" | 
                  "HyperlinkCreationSettings" | "Hyperplane" | "HyphenationOptions" | 
                  "IconizedObject" | "IgnoreSpellCheck" | "ImageCache" | 
                  "ImageCacheValid" | "ImageMarkers" | "ImageOffset" | "ImageRangeCache" | 
                  "ImageSizeCache" | "ImageSizeRaw" | "IncludeSingularTerm" | "Indent" | 
                  "IndentingNewlineSpacings" | "IndentMaxFraction" | 
                  "IndexCreationOptions" | "IndexTag" | "Inequality" | "InexactNumbers" | 
                  "InformationData" | "InformationDataGrid" | "InlineCounterAssignments" | 
                  "InlineCounterIncrements" | "InlineRules" | "InputFieldBox" | 
                  "InputFieldBoxOptions" | "InputGrouping" | "InputSettings" | 
                  "InputToBoxFormPacket" | "InsertionPointObject" | "Inset3DBox" | 
                  "Inset3DBoxOptions" | "InsetBox" | "InsetBoxOptions" | "Integral" | 
                  "Interlaced" | "InterpolationPrecision" | "InterpretationFunction" | 
                  "InterpretTemplate" | "InterruptSettings" | "Into" | 
                  "InvisibleApplication" | "InvisibleTimes" | "ItemBox" | 
                  "ItemBoxOptions" | "Jacobian" | "JoinedCurveBox" | 
                  "JoinedCurveBoxOptions" | "K" | "KernelExecute" | "Ket" | 
                  "LabeledSlider" | "LambertW" | "LanguageOptions" | "Launch" | 
                  "LayoutInformation" | "Lexicographic" | "LicenseID" | "Line3DBox" | 
                  "Line3DBoxOptions" | "LinearFilter" | "LineBox" | "LineBoxOptions" | 
                  "LineBreak" | "LinebreakSemicolonWeighting" | "LineColor" | 
                  "LineOpacity" | "LineWrapParts" | "LinkConnectedQ" | "LinkError" | 
                  "LinkFlush" | "LinkHost" | "LinkMode" | "LinkOptions" | "LinkReadHeld" | 
                  "LinkService" | "LinkWriteHeld" | "Listen" | "ListPickerBoxBackground" | 
                  "LiteralSearch" | "LocalizeDefinitions" | "LocatorBox" | 
                  "LocatorBoxOptions" | "LocatorCentering" | "LocatorPaneBox" | 
                  "LocatorPaneBoxOptions" | "LongEqual" | "LongForm" | "Loopback" | 
                  "MachineID" | "MachineName" | "MacintoshSystemPageSetup" | "MainSolve" | 
                  "MaintainDynamicCaches" | "MakeRules" | "MatchLocalNameQ" | "Material" | 
                  "MathematicaNotation" | "MathMLText" | "MaxBend" | "MaxPoints" | "Menu" | 
                  "MenuAppearance" | "MenuEvaluator" | "MenuItem" | "MenuList" | 
                  "MergeDifferences" | "MessageObject" | "MessageOptions" | 
                  "MessagesNotebook" | "MetaCharacters" | "MethodOptions" | 
                  "MinRecursion" | "MinSize" | "Mode" | "Modular" | "Momentary" | 
                  "MonomialOrder" | "MouseAppearanceTag" | "MouseButtons" | 
                  "MousePointerNote" | "MultiLetterItalics" | "MultiLetterStyle" | 
                  "Multiplicity" | "NamespaceBox" | "NamespaceBoxOptions" | "NBernoulliB" | 
                  "NeedCurrentFrontEndPackagePacket" | 
                  "NeedCurrentFrontEndSymbolsPacket" | "NestedScriptRules" | 
                  "NetworkPacketRecordingDuring" | "NewPrimitiveStyle" | "Next" | 
                  "NonAssociative" | "NormalGrouping" | "NotebookCreateReturnObject" | 
                  "NotebookDefault" | "NotebookFindReturnObject" | 
                  "NotebookGetLayoutInformationPacket" | 
                  "NotebookGetMisspellingsPacket" | "NotebookInterfaceObject" | 
                  "NotebookOpenReturnObject" | "NotebookPutReturnObject" | 
                  "NotebookResetGeneratedCells" | "NotebookSaveAs" | 
                  "NotebookSetupLayoutInformationPacket" | "NProductFactors" | 
                  "NSumTerms" | "NValues" | "OLEData" | "Open" | "OpenerBox" | 
                  "OpenerBoxOptions" | "OpenFunctionInspectorPacket" | 
                  "OpenSpecialOptions" | "OptionQ" | "OptionsPacket" | "OptionValueBox" | 
                  "OptionValueBoxOptions" | "OutputFormData" | "OutputGrouping" | 
                  "OutputMathEditExpression" | "Over" | "OverlayBox" | 
                  "OverlayBoxOptions" | "Package" | "PageHeight" | "PaneBox" | 
                  "PaneBoxOptions" | "PanelBox" | "PanelBoxOptions" | "PaneSelectorBox" | 
                  "PaneSelectorBoxOptions" | "PaperWidth" | "Parameter" | 
                  "ParameterVariables" | "ParentConnect" | "ParentForm" | "Parenthesize" | 
                  "ParentList" | "PartialD" | "PasteAutoQuoteCharacters" | "PausedTime" | 
                  "PeriodicInterpolation" | "PermissionsGroupMemberQ" | "Perpendicular" | 
                  "Pivoting" | "PlotRangeClipPlanesStyle" | "Point3DBox" | 
                  "Point3DBoxOptions" | "PointBox" | "PointBoxOptions" | "Polygon3DBox" | 
                  "Polygon3DBoxOptions" | "PolygonBox" | "PolygonBoxOptions" | 
                  "PolygonHoleScale" | "PolygonScale" | "PolynomialForm" | "Polynomials" | 
                  "PopupMenuBox" | "PopupMenuBoxOptions" | "PostScript" | "Precedence" | 
                  "PredictionRoot" | "Previous" | "PrimaryPlaceholder" | "PrintForm" | 
                  "PrismBox" | "PrismBoxOptions" | "PrivateFrontEndOptions" | 
                  "ProbabilityPr" | "ProcessStateDomain" | "ProcessTimeDomain" | 
                  "ProgressIndicatorBox" | "ProgressIndicatorBoxOptions" | "PromptForm" | 
                  "PyramidBox" | "PyramidBoxOptions" | "RadioButtonBox" | 
                  "RadioButtonBoxOptions" | "RandomSeed" | "RangeSpecification" | 
                  "Raster3DBox" | "Raster3DBoxOptions" | "RasterBox" | "RasterBoxOptions" | 
                  "RationalFunctions" | "RawArray" | "RawMedium" | "RectangleBox" | 
                  "RectangleBoxOptions" | "RecurringDigitsForm" | "RefBox" | 
                  "ReferenceMarkers" | "ReferenceMarkerStyle" | "Reinstall" | "Removed" | 
                  "RepeatedString" | "ResetMenusPacket" | "ResourceAcquire" | 
                  "ResourceSubmissionObject" | "ResourceSystemBase" | 
                  "ReturnEntersInput" | "ReturnInputFormPacket" | "RotationBox" | 
                  "RotationBoxOptions" | "RoundImplies" | "RowBackgrounds" | "RowHeights" | 
                  "RuleCondition" | "RuleForm" | "SaveAutoDelete" | "ScaledMousePosition" | 
                  "ScheduledTaskInformationData" | "ScriptForm" | "ScriptRules" | 
                  "SectionGrouping" | "Selection" | "SelectionCell" | 
                  "SelectionCellCreateCell" | "SelectionCellDefaultStyle" | 
                  "SelectionCellParentStyle" | "SelectionDebuggerTag" | 
                  "SelectionDuplicateCell" | "SelectionPlaceholder" | 
                  "SelectionSetStyle" | "SelectWithContents" | "SelfLoops" | 
                  "ServiceResponse" | "Setbacks" | "SetBoxFormNamesPacket" | 
                  "SetEvaluationNotebook" | "SetFileLoadingContext" | 
                  "SetNotebookStatusLine" | "SetOptionsPacket" | 
                  "SetSecuredAuthenticationKey" | "SetSpeechParametersPacket" | 
                  "SetterBox" | "SetterBoxOptions" | "SetValue" | "ShowAutoConvert" | 
                  "ShowCodeAssist" | "ShowControls" | "ShowGroupOpenCloseIcon" | 
                  "ShowInvisibleCharacters" | "ShowPredictiveInterface" | 
                  "ShowSyntaxStyles" | "ShrinkWrapBoundingBox" | "SingleEvaluation" | 
                  "SingleLetterStyle" | "Slider2DBox" | "Slider2DBoxOptions" | 
                  "SliderBox" | "SliderBoxOptions" | "Socket" | "SolveDelayed" | 
                  "SoundAndGraphics" | "Space" | "SpaceForm" | "SpanningCharacters" | 
                  "SpeakTextPacket" | "SpellingSuggestionsPacket" | "SphereBox" | 
                  "StartupSound" | "StringBreak" | "StringByteCount" | "StrokeForm" | 
                  "StructuredArrayHeadQ" | "StyleHints" | "StyleKeyMapping" | 
                  "StyleNames" | "SubValues" | "SurdForm" | "Syntax" | "SystemException" | 
                  "SystemGet" | "SystemInformationData" | "SystemStub" | "SystemTest" | 
                  "Tab" | "TableView" | "TableViewBox" | "TableViewBoxBackground" | 
                  "TableViewBoxOptions" | "TabViewBox" | "TabViewBoxOptions" | 
                  "TagBoxNote" | "TagStyle" | "TemplateArgBox" | "TemplateEvaluate" | 
                  "TemplateSlotSequence" | "TemplateUnevaluated" | "TemplateVerbatim" | 
                  "TemporaryVariable" | "TensorQ" | "TetrahedronBox" | 
                  "TetrahedronBoxOptions" | "Text3DBox" | "Text3DBoxOptions" | "TextBand" | 
                  "TextBoundingBox" | "TextBox" | "TextForm" | "TextLine" | 
                  "TextParagraph" | "ThisLink" | "TitleGrouping" | "ToColor" | "Toggle" | 
                  "ToggleFalse" | "TogglerBox" | "TogglerBoxOptions" | "TooBig" | 
                  "TooltipBox" | "TooltipBoxOptions" | "TotalHeight" | "TraceAction" | 
                  "TraceInternal" | "TraceLevel" | "TradingChart" | "TraditionalNotation" | 
                  "TraditionalOrder" | "TransparentColor" | "TrapSelection" | 
                  "TubeBezierCurveBox" | "TubeBezierCurveBoxOptions" | "TubeBox" | 
                  "TubeBoxOptions" | "TubeBSplineCurveBox" | 
                  "TubeBSplineCurveBoxOptions" | "UntrackedVariables" | "Up" | 
                  "UpdateDynamicObjects" | "UpdateDynamicObjectsSynchronous" | 
                  "UseGraphicsRange" | "UserDefinedWavelet" | "Using" | "V2Get" | "Value" | 
                  "ValueBox" | "ValueBoxOptions" | "ValueForm" | "ValuesData" | 
                  "VectorGlyphData" | "Verbose" | "VerboseConvertToPostScriptPacket" | 
                  "Version" | "VersionNumber" | "Vertical" | "VerticalForm" | 
                  "ViewPointSelectorSettings" | "ViewPort" | "VirtualGroupData" | 
                  "VisibleCell" | "WaitUntil" | "WindowPersistentStyles" | 
                  "WindowSelected" | "WindowWidth" | "WolframAlphaDate" | 
                  "WolframAlphaQuantity" | "WolframAlphaResult" | "$ActivationGroupID" | 
                  "$ActivationUserRegistered" | "$AddOnsDirectory" | "$BoxForms" | 
                  "$CloudVersionNumber" | "$CloudWolframEngineVersionNumber" | 
                  "$ConditionHold" | "$DefaultFrontEnd" | "$DefaultMailbox" | 
                  "$DefaultPath" | "$FinancialDataSource" | "$GeoEntityTypes" | 
                  "$GeoLocationPrecision" | "$HTMLExportRules" | "$HTTPRequest" | 
                  "$InterfaceEnvironment" | "$LaunchDirectory" | "$LicenseProcesses" | 
                  "$LicenseSubprocesses" | "$LicenseType" | "$LinkSupported" | 
                  "$LoadedFiles" | "$MaxLicenseProcesses" | "$MaxLicenseSubprocesses" | 
                  "$MinorReleaseNumber" | "$NetworkLicense" | "$Off" | "$OutputForms" | 
                  "$PasswordFile" | "$PatchLevelID" | "$PermissionsGroupBase" | 
                  "$PipeSupported" | "$PreferencesDirectory" | "$PrintForms" | 
                  "$PrintLiteral" | "$RegisteredDeviceClasses" | "$RegisteredUserName" | 
                  "$ResourceSystemBase" | "$SecuredAuthenticationKeyTokens" | 
                  "$SetParentLink" | "$SoundDisplay" | "$StructuredArrayHeads" | 
                  "$SuppressInputFormHeads" | "$SystemMemory" | "$TraceOff" | "$TraceOn" | 
                  "$TracePattern" | "$TracePostAction" | "$TracePreAction" | 
                  "$UserAgentLanguages" | "$UserAgentMachine" | "$UserAgentName" | 
                  "$UserAgentOperatingSystem" | "$UserAgentVersion" | "$UserName", _] -> scanUndocumentedSymbols,

(*
Scan symbols that are documented as OBSOLETE
*)
LeafNode[Symbol, "$$Media" | "$AsynchronousTask" | "$DefaultFont" | "$EntityStores" | 
                  "$FormatType" | "$HTTPCookies" | "$InstallationDate" | 
                  "$MachineDomain" | "$ProductInformation" | "$ProgramName" | 
                  "$RandomState" | "$ScheduledTask" | "$TemporaryPrefix" | "$TextStyle" |
                  "$TopDirectory" | "$UserAddOnsDirectory" | "AbortScheduledTask" | 
                  "Active" | "AlgebraicRules" | "Alias" | "AmbientLight" | 
                  "AnatomyForm" | "AnimationCycleOffset" | "AnimationCycleRepetitions" |
                  "AnimationDisplayTime" | "AspectRatioFixed" | "AstronomicalData" | 
                  "AsynchronousTaskObject" | "AsynchronousTasks" | "AudioDevice" |
                  "ButtonEvaluator" | "ButtonExpandable" | "ButtonFrame" | 
                  "ButtonMargins" | "ButtonNote" | "ButtonStyle" | "CDFInformation" | 
                  "CellArray" | "ChebyshevDistance" | "ClassifierInformation" | 
                  "ClipFill" | "ColorOutput" | "ColumnForm" | "Compose" | 
                  "ConstrainedMax" | "ConstrainedMin" | "ContourGraphics" |
                  "ContourLevels" | "ContourLines" | "ContourSpacing" | 
                  "ConversionOptions" | "CreateScheduledTask" | "CreateTemporary" | 
                  "Date" | "Debug" | "DefaultColor" | "DefaultFont" | "DensityGraphics" |
                  "Display" | "DisplayString" | "DotPlusLayer" | "DragAndDrop" | 
                  "DSolveConstants" | "Dump" | "EdgeLabeling" | "EdgeRenderingFunction" |
                  "EvaluateScheduledTask" | "ExpectedValue" | "FactorComplete" | 
                  "FontForm" | "FormTheme" | "FromASCII" | "FromDate" | "FullOptions" | 
                  "GraphicsArray" | "GraphicsSpacing" | "GridBaseline" | "HeldPart" | 
                  "HiddenSurface" | "HomeDirectory" | "HTMLSave" | "ImageRotated" | 
                  "InstanceNormalizationLayer" | "LegendreType" | "LightSources" | 
                  "LinkOpen" | "Literal" | "LongestMatch" | "LUBackSubstitution" |
                  "MeshRange" | "NextScheduledTaskTime" | "NotebookCreate" | 
                  "OpenTemporary" | "PackingMethod" | "Plot3Matrix" | "PlotDivision" | 
                  "PlotJoined" | "PolygonIntersections" | "PredictorInformation" | 
                  "QuantityThread" | "Random" | "RasterArray" | "RecognitionThreshold" |
                  "Release" | "RemoveAsynchronousTask" | "RemoveScheduledTask" | 
                  "RenderAll" | "ReplaceHeldPart" | "ResetMedium" | 
                  "ResetScheduledTask" | "ResumePacket" | "RunScheduledTask" | 
                  "ScheduledTaskActiveQ" | "ScheduledTaskInformation" | 
                  "ScheduledTaskObject" | "ScheduledTasks" | "SelectionAnimate" | 
                  "SequenceAttentionLayer" | "SequenceForm" | "Shading" | 
                  "ShortestMatch" | "SingularValues" | "SkinStyle" | "Splice" | 
                  "StartAsynchronousTask" | "StartScheduledTask" | "StateDimensions" | 
                  "StopAsynchronousTask" | "StopScheduledTask" | "StyleForm" | 
                  "StylePrint" | "Subscripted" | "SurfaceColor" | "SurfaceGraphics" | 
                  "SuspendPacket" | "TeXSave" | "TextStyle" | "ThreadDepth" | 
                  "TimeWarpingCorrespondence" | "TimeWarpingDistance" | "ToASCII" | 
                  "ToDate" | "ToFileName" | "ToHeldExpression" | "URLFetch" | 
                  "URLFetchAsynchronous" | "URLSave" | "URLSaveAsynchronous" | 
                  "VertexCoordinateRules" | "VertexLabeling" | 
                  "VertexRenderingFunction" | "WaitAsynchronousTask" | "WindowMovable", _] -> scanObsoleteSymbols,

(*
Scan symbols that are documented as EXPERIMENTAL
*)
LeafNode[Symbol, "ActiveClassification" | "ActiveClassificationObject" | 
                  "ActivePrediction" | "ActivePredictionObject" | "AddToSearchIndex" | 
                  "AggregatedEntityClass" | "AggregationLayer" | "AngleBisector" | 
                  "AnnotationDelete" | "AnnotationRules" | "AnomalyDetection" | 
                  "AnomalyDetectorFunction" | "AppendLayer" | "Around" | "AroundReplace" | 
                  "AskAppend" | "AskConfirm" | "AskDisplay" | "AskedQ" | "AskedValue" | 
                  "AskFunction" | "Ask" | "AskState" | "AskTemplateDisplay" | 
                  "AssumeDeterministic" | "AsymptoticDSolveValue" | 
                  "AsymptoticIntegrate" | "AsymptoticRSolveValue" | "AsymptoticSolve" | 
                  "AsymptoticSum" | "AtomCoordinates" | "AtomCount" | 
                  "AtomDiagramCoordinates" | "AtomList" | "Atom" | "AttentionLayer" | 
                  "AudioAnnotate" | "AudioAnnotationLookup" | "AudioIdentify" | 
                  "AudioLooping" | "AudioPause" | "AudioPlay" | "AudioRecord" | 
                  "AudioStop" | "AudioStream" | "AudioStreams" | "Autocomplete" | 
                  "AutocompletionFunction" | "AxiomaticTheory" | "BaseDecode" | 
                  "BaseEncode" | "BasicRecurrentLayer" | "BatchNormalizationLayer" | 
                  "BatchSize" | "BayesianMaximization" | "BayesianMaximizationObject" | 
                  "BayesianMinimization" | "BayesianMinimizationObject" | 
                  "BlockchainAddressData-ARK" | "BlockchainAddressData-Ethereum" | 
                  "BlockchainAddressData" | "BlockchainBase" | "BlockchainBlockData-ARK" | 
                  "BlockchainBlockData-Bitcoin" | "BlockchainBlockData-Ethereum" | 
                  "BlockchainBlockData" | "BlockchainContractValue" | 
                  "BlockchainData-ARK" | "BlockchainData-Bitcoin" | 
                  "BlockchainData-Ethereum" | "BlockchainData" | "BlockchainGet" | 
                  "BlockchainKeyEncode" | "BlockchainPut" | "BlockchainTokenData" | 
                  "BlockchainTransaction-ARK" | "BlockchainTransaction-Bitcoin" | 
                  "BlockchainTransactionData-ARK" | "BlockchainTransactionData-Bitcoin" | 
                  "BlockchainTransactionData-Ethereum" | "BlockchainTransactionData" | 
                  "BlockchainTransaction-Ethereum" | "BlockchainTransaction" | 
                  "BlockchainTransactionSign-ARK" | "BlockchainTransactionSign-Bitcoin" | 
                  "BlockchainTransactionSign-Ethereum" | "BlockchainTransactionSign" | 
                  "BlockchainTransactionSubmit-ARK" | 
                  "BlockchainTransactionSubmit-Bitcoin" | 
                  "BlockchainTransactionSubmit-Ethereum" | 
                  "BlockchainTransactionSubmit" | "BondCount" | "BondList" | "Bond" | 
                  "BondQ" | "CatenateLayer" | "ChannelBase" | "ChannelBrokerAction" | 
                  "ChannelDatabin" | "ChannelListener" | "ChannelListeners" | 
                  "ChannelListen" | "ChannelObject" | "ChannelPreSendFunction" | 
                  "ChannelReceiverFunction" | "ChannelSend" | "ChannelSubscribers" | 
                  "CloudExpression" | "CloudExpressions" | "CloudRenderingMethod" | 
                  "CombinedEntityClass" | "CompiledCodeFunction" | "CompilerOptions" | 
                  "ComputeUncertainty" | "ConnectedMoleculeComponents" | 
                  "ConnectedMoleculeQ" | "ConnectionSettings" | 
                  "ConnectSystemModelComponents" | "ConstantArrayLayer" | 
                  "ConstantPlusLayer" | "ConstantTimesLayer" | "Containing" | 
                  "ContentFieldOptions" | "ContentLocationFunction" | "ContentObject" | 
                  "ContrastiveLossLayer" | "ConvolutionLayer" | "CreateChannel" | 
                  "CreateCloudExpression" | "CreateDataSystemModel" | 
                  "CreateSearchIndex" | "CreateSystemModel" | "CrossEntropyLossLayer" | 
                  "CTCLossLayer" | "CurrentNotebookImage" | "CurrentScreenImage" | 
                  "Curry" | "DatabaseConnect" | "DatabaseDisconnect" | 
                  "DatabaseReference" | "DeconvolutionLayer" | "DecryptFile" | 
                  "DefineResourceFunction" | "DeleteAnomalies" | "DeleteChannel" | 
                  "DeleteCloudExpression" | "DeleteSearchIndex" | "DerivedKey" | 
                  "DigitalSignature" | "DisableFormatting" | "DocumentWeightingRules" | 
                  "DotLayer" | "DropoutLayer" | "DynamicGeoGraphics" | "DynamicImage" | 
                  "ElementwiseLayer" | "EmbeddingLayer" | "EncryptFile" | 
                  "EntityFunction" | "EntityStore" | "EvaluationEnvironment" | 
                  "ExpirationDate" | "ExtendedEntityClass" | "ExternalEvaluate" | 
                  "ExternalFunction" | "ExternalObject" | "ExternalSessionObject" | 
                  "ExternalSessions" | "ExternalValue" | "ExtractLayer" | 
                  "FacialFeatures" | "FeatureDistance" | "FeatureExtraction" | 
                  "FeatureExtract" | "FeatureExtractorFunction" | "FeatureExtractor" | 
                  "FeatureSpacePlot3D" | "FeatureSpacePlot" | "FileConvert" | 
                  "FilteredEntityClass" | "FindAnomalies" | "FindChannels" | 
                  "FindEquationalProof" | "FindExternalEvaluators" | 
                  "FindGeometricConjectures" | "FindMoleculeSubstructure" | 
                  "FindSystemModelEquilibrium" | "FindTextualAnswer" | "FlattenLayer" | 
                  "FormControl" | "FunctionCompileExportByteArray" | 
                  "FunctionCompileExportLibrary" | "FunctionCompileExport" | 
                  "FunctionCompileExportString" | "FunctionCompile" | "GalleryView" | 
                  "GatedRecurrentLayer" | "GenerateDerivedKey" | 
                  "GenerateDigitalSignature" | "GenerateSecuredAuthenticationKey" | 
                  "GeometricAssertion" | "GeometricScene" | "HandlerFunctionsKeys" | 
                  "HandlerFunctions" | "Iconize" | "ImageAugmentationLayer" | 
                  "ImageBoundingBoxes" | "ImageCases" | "ImageContainsQ" | 
                  "ImageContents" | "ImageGraphics" | "ImagePosition" | 
                  "ImagePyramidApply" | "ImagePyramid" | "IncludeAromaticBonds" | 
                  "IncludeHydrogens" | "IncludeRelatedTables" | 
                  "InitialEvaluationHistory" | "InitializationObjects" | 
                  "InitializationValue" | "Initialize" | "InverseImagePyramid" | 
                  "InverseSpectrogram" | "KernelFunction" | "LearnDistribution" | 
                  "LearnedDistribution" | "LearningRateMultipliers" | "LinearLayer" | 
                  "LocalResponseNormalizationLayer" | "LocalSubmit" | 
                  "LongShortTermMemoryLayer" | "LossFunction" | "MailExecute" | 
                  "MailFolder" | "MailItem" | "MailSearch" | "MailServerConnection" | 
                  "MailServerConnect" | "MaxTrainingRounds" | "MaxWordGap" | 
                  "MeanAbsoluteLossLayer" | "MeanAround" | "MeanSquaredLossLayer" | 
                  "MergingFunction" | "Midpoint" | "MissingValuePattern" | 
                  "MoleculeContainsQ" | "MoleculeEquivalentQ" | "MoleculeGraph" | 
                  "MoleculeModify" | "Molecule" | "MoleculePattern" | "MoleculePlot3D" | 
                  "MoleculePlot" | "MoleculeProperty" | "MoleculeQ" | "MoleculeValue" | 
                  "NBodySimulationData" | "NBodySimulation" | "NetAppend" | 
                  "NetBidirectionalOperator" | "NetChain" | "NetDecoder" | "NetDelete" | 
                  "NetDrop" | "NetEncoder" | "NetEvaluationMode" | "NetExtract" | 
                  "NetFlatten" | "NetFoldOperator" | "NetGraph" | "NetInformation" | 
                  "NetInitialize" | "NetInsert" | "NetInsertSharedArrays" | "NetJoin" | 
                  "NetMapOperator" | "NetMapThreadOperator" | "NetMeasurements" | 
                  "NetModel" | "NetNestOperator" | "NetPairEmbeddingOperator" | 
                  "NetPortGradient" | "NetPort" | "NetPrepend" | "NetRename" | 
                  "NetReplace" | "NetReplacePart" | "NetSharedArray" | "NetStateObject" | 
                  "NetTake" | "NetTrain" | "NetTrainResultsObject" | 
                  "NetworkPacketCapture" | "NetworkPacketRecording" | 
                  "NetworkPacketTrace" | "NormalizationLayer" | "NumericArray" | 
                  "NumericArrayQ" | "NumericArrayType" | "OrderingLayer" | "PaddingLayer" | 
                  "Pagination" | "PartLayer" | "PartProtection" | "PerpendicularBisector" | 
                  "PersistenceLocation" | "PersistenceTime" | "PersistentObject" | 
                  "PersistentObjects" | "PersistentValue" | "PitchRecognize" | 
                  "PoolingLayer" | "PrependLayer" | "PreserveColor" | "ProofObject" | 
                  "PublisherID" | "RandomInstance" | "RarerProbability" | 
                  "RegisterExternalEvaluator" | "RelationalDatabase" | 
                  "RemoteAuthorizationCaching" | "RemoteConnectionObject" | 
                  "RemoteConnect" | "RemoteFile" | "RemoteRun" | "RemoteRunProcess" | 
                  "RemoveAudioStream" | "RemoveChannelListener" | 
                  "RemoveChannelSubscribers" | "ReplicateLayer" | "ReshapeLayer" | 
                  "ResizeLayer" | "ResourceFunction" | "ResourceRegister" | 
                  "ResourceRemove" | "ResourceSearch" | "ResourceSubmit" | 
                  "ResourceUpdate" | "SampledEntityClass" | "SearchAdjustment" | 
                  "SearchIndexObject" | "SearchIndices" | "SearchQueryString" | 
                  "SearchResultObject" | "SecuredAuthenticationKey" | 
                  "SecuredAuthenticationKeys" | "SequenceLastLayer" | 
                  "SequenceMostLayer" | "SequencePredict" | "SequencePredictorFunction" | 
                  "SequenceRestLayer" | "SequenceReverseLayer" | "ServiceRequest" | 
                  "ServiceSubmit" | "SessionSubmit" | "SetSystemModel" | "Snippet" | 
                  "SnubPolyhedron" | "SocketListener" | "SocketListen" | "SocketOpen" | 
                  "SocketReadMessage" | "SocketReadyQ" | "Sockets" | "SocketWaitAll" | 
                  "SocketWaitNext" | "SoftmaxLayer" | "SortedEntityClass" | "SourceLink" | 
                  "SpatialTransformationLayer" | "SpeechRecognize" | 
                  "StartExternalSession" | "StartWebSession" | "StereochemistryElements" | 
                  "SummationLayer" | "SynthesizeMissingValues" | "SystemInstall" | 
                  "SystemModeler" | "SystemModelExamples" | "SystemModelLinearize" | 
                  "SystemModel" | "SystemModelParametricSimulate" | "SystemModelPlot" | 
                  "SystemModelProgressReporting" | "SystemModelReliability" | 
                  "SystemModelSimulate" | "SystemModelSimulateSensitivity" | 
                  "SystemModelSimulationData" | "SystemModels" | "TargetDevice" | 
                  "TargetSystem" | "TaskAbort" | "TaskExecute" | "TaskObject" | 
                  "TaskRemove" | "TaskResume" | "Tasks" | "TaskSuspend" | "TaskWait" | 
                  "TextCases" | "TextContents" | "TextElement" | "TextPosition" | 
                  "TextSearch" | "TextSearchReport" | "TextStructure" | "ThreadingLayer" | 
                  "TotalLayer" | "TrainingProgressCheckpointing" | 
                  "TrainingProgressFunction" | "TrainingProgressMeasurements" | 
                  "TrainingProgressReporting" | "TrainingStoppingCriterion" | 
                  "TransposeLayer" | "TriangleCenter" | "TriangleConstruct" | 
                  "TriangleMeasurement" | "Typed" | "TypeSpecifier" | 
                  "UnconstrainedParameters" | "UnitVectorLayer" | 
                  "UnregisterExternalEvaluator" | "UpdateSearchIndex" | 
                  "URLDownloadSubmit" | "ValenceErrorHandling" | 
                  "ValuePreprocessingFunction" | "VectorAround" | "VerifyDerivedKey" | 
                  "VerifyDigitalSignature" | "VerifyInterpretation" | "WebAudioSearch" | 
                  "WebElementObject" | "WebExecute" | "WebImage" | "WebImageSearch" | 
                  "WebSearch" | "WebSessionObject" | "WebSessions" | "WebWindowObject" | 
                  "WikipediaSearch" | "ZoomCenter" | "ZoomFactor" | 
                  "$AllowExternalChannelFunctions" | "$BlockchainBase" | "$ChannelBase" | 
                  "$CookieStore" | "$CurrentTask" | "$CurrentWebSession" | 
                  "$DefaultNetworkInterface" | "$IncomingMailSettings" | 
                  "$InitializationContexts" | "$Initialization" | "$NetworkInterfaces" | 
                  "$NoValue" | "$PersistenceBase" | "$PersistencePath" | 
                  "$PreInitialization" | "$PublisherID" | "$ServiceCreditsAvailable" | 
                  "$SourceLink" | "$SSHAuthentication" | "$SummaryBoxDataSizeLimit" | 
                  "$TestFileName" | "$VoiceStyles", _] -> scanExperimentalSymbols,

(*

If LoadJavaClass["java.lang.System"] is called, then these symbols are created in System`

It is therefore dangerous to use these symbols in production code where it is unknown whether JLink will be used.


too noisy


SymbolNode[Symbol, "arraycopy" | "clearProperty" | "console" | "currentTimeMillis" | "err" | "exit" | "gc" |
                      "getenv" | "getProperties" | "getProperty" | "getSecurityManager" | "identityHashCode" |
                      "in" | "inheritedChannel" | "lineSeparator" | "load" | "loadLibrary" | "mapLibraryName" |
                      "nanoTime" | "out" | "runFinalization" | "runFinalizersOnExit" | "setErr" | "setIn" |
                      "setOut" | "setProperties" | "setProperty" | "setSecurityManager", _] -> scanJavaSystemSymbols,
*)

CallNode[LeafNode[Symbol, "LoadJavaClass" | "JLink`LoadJavaClass", _], { LeafNode[String, "\"java.lang.System\"", _] }, _] -> scanLoadJavaClassSystem,





(*
scan for a := a  and  a = a
possible results from batch renaming symbols
*)
CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], { LeafNode[Symbol, token_, _], LeafNode[Symbol, token_, _] }, _] -> scanSelfAssignments,


ContextNode[{LeafNode[String, "\"Private`\"", _]}, _, _] -> scanPrivateContextNode,



LeafNode[Symbol, "$HistoryLength" | "$Line", _] -> scanSessionSymbols,

CallNode[LeafNode[Symbol, "In" | "Out" | "InString", _], _, _] -> scanSessionCalls,


(*

too noisy

CallNode[LeafNode[Symbol, "Print" | "Echo", _], _, _] -> scanDebugCalls,
*)




(*

experimental

FileNode[_, _, _] -> scanFiles,
*)


(*

experimental

detect a?fooQ  when you meant a_?fooQ

currently too difficult to determine what is a pattern

CallNode[SymbolNode[Symbol, "PatternTest", _], {
                      lhs_ /; FreeQ[lhs, CallNode[SymbolNode[Symbol, "Pattern" | "Blank" | "BlankSequence" | "BlankNullSequence", _], _, _]],
                      _}, _] -> scanPatternTestMissingPattern,
*)



(*

experimental

detect calls like f[a_] := a_


TODO: A clever thing to do would be to detet when inside of Quiet[ ,{Rule::rhs, RuleDelayed::rhs}] and turn off this check

CallNode[SymbolNode[Symbol, "Set" | "SetDelayed", _], { lhs_, rhs_ } /;
            Intersection[Cases[lhs, CallNode[SymbolNode[Symbol, "Pattern", _], {SymbolNode[Symbol, name_, _], _}, _] :> name, {0, Infinity}],
                          Cases[rhs, CallNode[SymbolNode[Symbol, "Pattern", _], {SymbolNode[Symbol, name_, _], _}, _] :> name, {0, Infinity}]] != {}, _] -> scanRHSPatterns,
*)



CallNode[LeafNode[Symbol, "And", _], _, _] -> scanAnds,

CallNode[LeafNode[Symbol, "Or", _], _, _] -> scanOrs,

CallNode[LeafNode[Symbol, "Alternatives", _], _, _] -> scanAlternatives,



CallNode[LeafNode[Symbol, "Slot" | "SlotSequence", _], _, _] -> scanSlots,


CallNode[LeafNode[Symbol, "Refine" | "Reduce" | "Solve" | "FindInstance" | "Assuming", _], _, _] -> scanSolverCalls,


(*
cst of [x] is fine
ast of [x] is an error
*)
AbstractSyntaxErrorNode[_, _, _] -> scanAbstractSyntaxErrorNodes,



(*
Tags: SyntaxError NotContiguous
*)
KeyValuePattern[AbstractSyntaxIssues -> _] -> scanAbstractSyntaxIssues,



Nothing
|>








Attributes[scanBadCalls] = {HoldRest}

scanBadCalls[pos_List, astIn_] :=
 Module[{ast, node, data, head, issues},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  head = node[[1]];
  name = head["String"];
  data = head[[3]];

  issues = {};

    Switch[name,
    "String",
      AppendTo[issues, Lint["BadCall", "``String`` is not a function.", "Error", <| Source -> data[Source], CodeActions->{ CodeAction["Replace ``String`` with ``StringQ``", ReplaceNode, <| "ReplacementNode" -> ToNode[StringQ], Source -> data[Source] |>] }, ConfidenceLevel -> 0.90 |>]]
    ,
      "Integer",
      AppendTo[issues, Lint["BadCall", "``Integer`` is not a function.", "Error", <| Source -> data[Source], CodeActions->{ CodeAction["Replace ``Integer`` with ``IntegerQ``", ReplaceNode, <| "ReplacementNode" -> ToNode[IntegerQ], Source -> data[Source] |>] }, ConfidenceLevel -> 0.90 |>]]
    ,
      "Real",
      AppendTo[issues, Lint["BadCall", "``Real`` is not a function.", "Error", <| Source -> data[Source], CodeActions->{ CodeAction["Replace ``Real`` with ``Developer`RealQ``", ReplaceNode, <| "ReplacementNode" -> ToNode[Developer`RealQ], Source -> data[Source] |>] }, ConfidenceLevel -> 0.90 |>]]
    ,
      "True",
      AppendTo[issues, Lint["BadCall", "``True`` is not a function.", "Error", <| Source -> data[Source], CodeActions->{ CodeAction["Replace ``True`` with ``TrueQ``", ReplaceNode, <| "ReplacementNode" -> ToNode[TrueQ], Source -> data[Source] |>] }, ConfidenceLevel -> 0.95 |>]]
    ,
    _,
      AppendTo[issues, Lint["BadCall", format[name] <> " is not a function.", "Error", <|data, ConfidenceLevel -> 0.90|>]]
  ];

  issues
]


(*

not a good scan

Attributes[scanFailureCalls] = {HoldRest}

scanFailureCalls[pos_List, astIn_] :=
 Module[{ast, node, children, data},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  If[Length[children] == 1,
  	{Lint["FailureCall", {"Calling ", LintBold["Failure"], " as a function. Did you mean ", LintBold["FailureQ"],
      "? This may be ok if ", LintBold["Failure"], " is handled programmatically."}, "Error", data]}
  	,
  	{}
  ]
  ]
*)





Attributes[scanAssocs] = {HoldRest}

scanAssocs[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, selecteds, issues, keys, srcs, actions},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  If[!MatchQ[children, { CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _], _, _]... }],
    (*
    Association does not have Rule arguments.
    *)
    Throw[{}]
  ];
  
    keys = children[[All, 2, 1]];

    duplicates = Keys[Select[CountsBy[keys, ToFullFormString], # > 1&]];
   selecteds = Select[children, Function[{rule}, ToFullFormString[rule[[2, 1]]] === #]]& /@ duplicates;

   Do[

      If[!empty[selected],
       srcs = #[[3, Key[Source]]]& /@ selected;

       actions = MapIndexed[CodeAction["Delete key " <> ToString[#2[[1]]], DeleteNode, <|Source->#|>]&, srcs];

       AppendTo[issues, Lint["DuplicateKeys", "``Association`` has duplicated keys.", "Error",
          <| Source->First[srcs], "AdditionalSources"->Rest[srcs], CodeActions->actions, ConfidenceLevel -> 1.0 |> ]];
      ];

      ,
      {selected, selecteds}
   ];

    issues

  ]]





Attributes[scanRules] = {HoldRest}

scanRules[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, selecteds, issues, keys, srcs},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};
  
  keys = children[[All, 2, 1]];

  (*
  heuristic

  if something like {_ -> {}, _ -> {}} then do not warn, it is just a pattern
  *)
  If[AnyTrue[keys, MatchQ[#, CallNode[LeafNode[Symbol, "Blank", _], _, _]]&],
    Throw[issues]
  ];

    duplicates = Keys[Select[CountsBy[keys, ToFullFormString], # > 1&]];
   selecteds = Select[children, Function[{rule}, ToFullFormString[rule[[2, 1]]] === #]]& /@ duplicates;

   Do[

      If[!empty[selected],

       (*
       It is perfectly valid to have things like {1 -> NetPort["Output"], 1 -> 2 -> NetPort["Sum"]} in NetGraph

       So make Remark for now
       *)

       srcs = #[[3, Key[Source]]]& /@ selected;

       AppendTo[issues, Lint["DuplicateKeys", "Duplicate keys in list of rules.", "Remark",
            <| Source->First[srcs], "AdditionalSources"->Rest[srcs], ConfidenceLevel -> 0.75 |>]];
      ];

      ,
      {selected, selecteds}
   ];

   issues

  ]]




Attributes[scanWhichs] = {HoldRest}

scanWhichs[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, issues, span, duplicates, selected, lintData, srcs},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  
  issues = {};

  If[empty[children],
    AppendTo[issues, 
     Lint["WhichArguments", "``Which`` does not have any arguments.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];

  If[!EvenQ[Length[children]],
    AppendTo[issues, 
     Lint["WhichArguments", "``Which`` does not have even number of arguments.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];


  If[MatchQ[children[[1]], LeafNode[Symbol, "$OperatingSystem", _]],
    span = children[[1]][[3]];
   AppendTo[issues, 
    Lint["SwitchWhichConfusion", "``Which`` has ``$OperatingSystem`` in first place.\n\
Did you mean ``Switch``?", "Error", <|span, ConfidenceLevel -> 0.75|>]];
  ];

  If[MatchQ[children[[-2]], CallNode[LeafNode[Symbol, "Blank", _], _, _]],
    lintData = children[[-2]][[3]];
   AppendTo[issues, 
    Lint["SwitchWhichConfusion", "``_`` is not a test.", "Error", <| lintData, CodeActions->{CodeAction["Replace ``_`` with ``True``", ReplaceNode, <| "ReplacementNode" -> ToNode[True], Source->lintData[Source] |>]}, ConfidenceLevel -> 1.0 |>]];
  ];


  Scan[(If[MatchQ[#, CallNode[LeafNode[Symbol, "Set", _], _, _]],
    AppendTo[issues, Lint["WhichSet", "``Which`` has ``=`` as a clause.\n\
Did you mean ``==``?", "Error", <|#[[3]], ConfidenceLevel -> 0.85|>]];
  ];)&, children[[;;;;2]]];


    duplicates = Keys[Select[CountsBy[children[[;;;;2]], ToFullFormString], # > 1&]];
   selected = Flatten[Select[children[[;;;;2]], Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];

   If[!empty[selected],
       srcs = #[[3, Key[Source]]]& /@ selected;

      AppendTo[issues, Lint["DuplicateClauses", "Duplicate clauses in ``Which``.", "Error",
          <| Source->First[srcs], "AdditionalSources"->Rest[srcs], ConfidenceLevel -> 1.0 |> ]];
   ];

  issues
  ]]




Attributes[scanSwitchs] = {HoldRest}

scanSwitchs[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, src, cases, duplicates, issues, selected, srcs, span},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  
  issues = {};

  If[Length[children] == 1,
   AppendTo[issues, Lint["SwitchArguments", "``Switch`` only has one argument.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
   Throw[issues];
  ];


  If[!OddQ[Length[children]],
   AppendTo[issues, Lint["SwitchArguments", "``Switch`` does not have odd number of arguments.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
   Throw[issues];
  ];

  If[MatchQ[children[[1]], LeafNode[Symbol, "$OperatingSystem", _]],
   cases = Cases[children[[2;;-1;;2]], LeafNode[String, "\"Linux\"", _], {0, Infinity}];
   If[cases =!= {},
    src = cases[[1, 3, Key[Source] ]];
    AppendTo[issues, Lint["OperatingSystemLinux", "``\"Linux\"`` is not a value of ``$OperatingSystem``.", "Error", <|Source->src, ConfidenceLevel -> 0.95, CodeActions->{CodeAction["Replace Linux with Unix", ReplaceNode, <|Source->src, "ReplacementNode"->ToNode["Unix"]|>]}|>]];
   ]
  ];

  (*
  Something like:
  Switch[a,
  1->2,
  3->4,
  _,5]
  *)
  If[Length[children] >= 5,
    If[MatchQ[children[[2;;-3]], { CallNode[LeafNode[Symbol, "Rule", _], _, _].. }],
      AppendTo[issues, Lint["SwitchArguments", "``Switch`` does not take ``Rules`` for arguments.", "Error", <|data, ConfidenceLevel -> 0.95|>]];
    ]
  ];

  (*
   Switch has True in last place like this: Switch[a,1,b,True,c]
   *)
  If[MatchQ[children[[-2]], LeafNode[Symbol, "True", _]],
   (*
    
    heuristic 
  
   presence of False makes it less likely that True is unintended
   *)
   If[FreeQ[children[[2;;-4;;2]], LeafNode[Symbol, "False", _]],
    span = children[[-2]][[3]];
    AppendTo[issues, Lint["SwitchWhichConfusion", "``Switch`` has ``True`` in last place.\n\
Did you mean ``_``?", "Warning", <|span, ConfidenceLevel -> 0.75|>]];
   ]
  ];


  duplicates = Keys[Select[CountsBy[children[[2;;;;2]], ToFullFormString], # > 1&]];
  selected = Flatten[Select[children[[2;;;;2]], Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];

  If[!empty[selected],
       srcs = #[[3, Key[Source]]]& /@ selected;

      AppendTo[issues, Lint["DuplicateClauses", "Duplicate clauses in ``Switch``.", "Error",
          <| Source->First[srcs], "AdditionalSources"->Rest[srcs], ConfidenceLevel -> 1.0 |> ]];
   ];


  (*
  pairs = Partition[children[[2;;]], 2];

  Scan[(
    form = #[[1]];
    value = #[[2]];
    formPatternNames = Cases[form, CallNode[LeafNode[Symbol, "Pattern", _], {LeafNode[Symbol, n_, _], _}, _] :> n, {0, Infinity}];

    Scan[(
      
      valuePatterns = Cases[value, LeafNode[Symbol, #, _], {0, Infinity}];
      If[empty[valuePatterns],
        (*
        too noisy
        add a Remark about unused named pattern in Switch? *)
        Null
        ,
        (*
        too many false positives, so make this a Remark for now
        
        experimental

        Scan[(AppendTo[issues, Lint["NamedPatternInSwitch", "Named pattern in ``Switch``: ``" <> ToFullFormString[#] <> "``.\n\
The pattern ``" <> ToFullFormString[#] <> "`` occurs in the matching form, but ``Switch`` does not support pattern replacement.\n\
This may be ok if ``" <> ToFullFormString[#] <> "`` is set before being used.\n\
Consider removing the named pattern ``" <> ToFullFormString[#] <> "``.", "Remark", #[[3]]]])&, valuePatterns]*)
        Null
      ]

      )&, formPatternNames];

    )&, pairs];*)

  issues
]]



Attributes[scanIfs] = {HoldRest}

scanIfs[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, issues, selected, srcs},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  
  issues = {};

  Which[
    Length[children] == 0,
      AppendTo[issues, Lint["IfArguments", "``If`` has zero arguments.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
      Throw[issues];
      ,
    Length[children] == 1,
      AppendTo[issues, Lint["IfArguments", "``If`` only has one argument.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
      Throw[issues];
  ];

  If[MatchQ[children[[1]], CallNode[LeafNode[Symbol, "Set", _], _, _]],
    AppendTo[issues, Lint["IfSet", "``If`` has ``=`` as first argument.\n\
Did you mean ``==``?", "Warning", <| children[[1, 3]], ConfidenceLevel -> 0.85|>]];
  ];

  srcs = {};
  If[Length[children] >= 3,
      duplicates = Keys[Select[CountsBy[children[[2;;3]], ToFullFormString], # > 1&]];
      selected = Flatten[Select[children[[2;;3]], Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];

      If[!empty[selected],

            srcs = #[[3, Key[Source]]]& /@ selected;

            AppendTo[issues, Lint["DuplicateClauses", "Both branches are the same.", "Warning", <|Source->First[srcs], "AdditionalSources"->Rest[srcs], ConfidenceLevel -> 0.95|>]]
      ];
  ];

  issues
]]



(*

experimental

must handle Condition


Attributes[scanReplaces] = {HoldRest}

scanReplaces[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, issues, selected, rules, lhss},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  
  If[Length[children] == 2,
    rules = children[[2]];
    ,
    (*
    operator form
    *)
    rules = children[[1]];
  ];

  If[!MatchQ[rules, CallNode[LeafNode[Symbol, "List", _], _, _]],
    Throw[{}];
  ];

  Scan[(If[!MatchQ[#, CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _], { _, _ }, _]],
    Throw[{}];
    ];)&, rules[[2]]];

  issues = {};

  (*
  get the lhss of all of the rules
  *)
  lhss = rules[[2, All, 2, 1]];

  duplicates = Keys[Select[CountsBy[lhss, ToFullFormString], # > 1&]];
  selected = Flatten[Select[lhss, Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];
  Scan[(AppendTo[issues, Lint["DuplicateClauses", "Duplicate clauses in ``Replace``.", "Error", #[[3]]]])&, selected];

  issues
]]

*)














Attributes[scanPatterns] = {HoldRest}

scanPatterns[pos_List, astIn_] :=
Catch[
 Module[{ast, node, patSymbol, name, rhs, children, patterns, issues},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  
  children = node[[2]];
  data = node[[3]];

  issues = {};

  If[Length[children] != 2,
    AppendTo[issues, Lint["PatternArguments", "``Pattern`` takes 2 arguments.", "Error", <|data, ConfidenceLevel -> 0.85|>]];
    Throw[issues];
  ];

  patSymbol = children[[1]];
  name = patSymbol["String"];
  rhs = children[[2]];

  patterns = Cases[rhs, CallNode[LeafNode[Symbol, "Pattern", _], _, _], {0, Infinity}];
  Scan[(
    If[#[[2, 1]]["String"] == name,
      AppendTo[issues, Lint["DuplicateNamedPattern", "Duplicate named pattern " <> format[name] <> ".",
            "Error", <| Source -> #[[2, 1, 3, Key[Source]]], "AdditionalSources" -> { patSymbol[[3, Key[Source]]] }, ConfidenceLevel -> 0.95 |> ]];
    ];
  )&, patterns];

  issues
]]



Attributes[scanControls] = {HoldRest}

scanControls[pos_List, astIn_] :=
Catch[
 Module[{ast, node, data, parentPos, parent, s},
  If[pos == {},
    (* top node, no parent *)
    Throw[{}]
  ];
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  s = node["String"];
  data = node[[3]];
  
  parentPos = Most[pos];
  parent = Extract[ast, {parentPos}][[1]];
  While[ListQ[parent],
   parentPos = Most[parentPos];
   parent = Extract[ast, {parentPos}][[1]];
   ];

   If[MatchQ[parent, CallNode[node, _, _]],
    Throw[{}]
   ];

  {Lint["Control", format[s] <> " appears but is not called.\n\
Did you mean " <> format[s<>"[]"] <>"?", "Warning", <|data, ConfidenceLevel -> 0.85|>]}
  ]]




Attributes[scanModules] = {HoldRest}

scanModules[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, selected, params, issues, vars, used, unusedParams},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  If[empty[children],
    AppendTo[issues, Lint["ModuleArguments", "``Module`` does not have 2 arguments.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];

  (*
  Used as a pattern, so no issues
  *)
  If[MatchQ[children[[1]], CallNode[LeafNode[Symbol, "Pattern" | "Blank" | "BlankSequence" | "BlankNullSequence", _], _, _]],
    Throw[issues]
  ];

  If[Length[children] != 2,
    AppendTo[issues, Lint["ModuleArguments", "``Module`` does not have 2 arguments.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];

  (*
      Module[Evaluate[]] denotes meta-programming
  *)
  If[MatchQ[children[[1]], CallNode[LeafNode[Symbol, "Evaluate", _], _, _]],
    Throw[issues]
  ];

  If[!MatchQ[children[[1]], CallNode[LeafNode[Symbol, "List", _], _, _]],
    AppendTo[issues, Lint["ModuleArguments", "``Module`` does not have a ``List`` for argument 1.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];


  params = children[[1,2]];
   vars = # /. {CallNode[LeafNode[Symbol, "Set"|"SetDelayed", _], {sym:LeafNode[Symbol, _, _], _}, _] :> sym,
            sym:LeafNode[Symbol, _, _] :> sym,
            (*

            Compiler syntax includes:
            Module[{ Typed[x, "Integer64"] }, x]

            TODO: support this

            CallNode[SymbolNode["Typed", {}, _], { sym:SymbolNode[_, _, _], _ }, _] :> sym
            *)
            err_ :> (AppendTo[issues, Lint["ModuleArguments", "Variable " <> format[ToFullFormString[err]] <>
              " does not have proper form.", "Error", <|#[[3]], ConfidenceLevel -> 0.85|>]]; Nothing)}& /@ params;

    duplicates = Keys[Select[CountsBy[vars, ToFullFormString], # > 1&]];
    selected = Flatten[Select[vars, Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];

    If[!empty[selected],
       srcs = #[[3, Key[Source]]]& /@ selected;

      AppendTo[issues, Lint["DuplicateVariables", "Duplicate variables in ``Module``.", "Error",
          <| Source->First[srcs], "AdditionalSources"->Rest[srcs], ConfidenceLevel -> 1.0 |> ]];
      ];

  used = ToFullFormString /@ Cases[children[[2]], LeafNode[Symbol, _, _], {0, Infinity}];
  unusedParams = Select[vars, Function[{c}, !MemberQ[used, ToFullFormString[c]]]];

  Scan[AppendTo[issues, Lint["UnusedVariables", "Unused variable in ``Module``: " <> format[ToFullFormString[#]] <> ".", "Warning", <| #[[3]], CodeActions->{CodeAction["Delete", DeleteNode, <|Source->#[[3, Key[Source]]]|>]}, ConfidenceLevel -> 1.0 |> ]]&, unusedParams];

  issues
]]


Attributes[scanDynamicModules] = {HoldRest}

scanDynamicModules[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, selected, params, issues, vars, used, unusedParams},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  If[empty[children],
    AppendTo[issues, Lint["DynamicModuleArguments", "``DynamicModule`` does not have 2 arguments.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];

  (*
  Being used as a pattern, so no issues
  *)
  If[MatchQ[children[[1]], CallNode[LeafNode[Symbol, "Pattern" | "Blank" | "BlankSequence" | "BlankNullSequence", _], _, _]],
    Throw[issues]
  ];

  If[!(Length[children] >= 2),
    AppendTo[issues, Lint["DynamicModuleArguments", "``DynamicModule`` does not have 2 arguments.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];

  (*

  DynamicModule takes options

  If[Length[children] != 2,
    AppendTo[issues, Lint["DynamicModuleArguments", {LintBold["DynamicModule"], " does not have 2 arguments. This may be ok if ",
                              LintBold["DynamicModule"], " is handled programmatically."}, "Error", data]];
    Throw[issues]
  ];
  *)

  (*
      DynamicModule[Evaluate[]] denotes meta-programming
  *)
  If[MatchQ[children[[1]], CallNode[LeafNode[Symbol, "Evaluate", _], _, _]],
    Throw[issues]
  ];

  If[!MatchQ[children[[1]], CallNode[LeafNode[Symbol, "List", _], _, _]],
    AppendTo[issues, Lint["DynamicModuleArguments", "``DynamicModule`` does not have a ``List`` for argument 1.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];


  params = children[[1,2]];
   vars = # /. {CallNode[LeafNode[Symbol, "Set"|"SetDelayed", _], {sym:LeafNode[Symbol, _, _], _}, _] :> sym,
            sym:LeafNode[Symbol, _, _] :> sym,
            err_ :> (AppendTo[issues, Lint["DynamicModuleArguments", "Variable " <> format[ToFullFormString[err]] <> " does not have proper form.", "Error", <|#[[3]], ConfidenceLevel -> 0.85|>]]; Nothing)}& /@ params;
    duplicates = Keys[Select[CountsBy[vars, ToFullFormString], # > 1&]];
    selected = Flatten[Select[vars, Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];

    If[!empty[selected],
       srcs = #[[3, Key[Source]]]& /@ selected;

      AppendTo[issues, Lint["DuplicateVariables", "Duplicate variables in ``DynamicModule``.", "Error",
          <| Source->First[srcs], "AdditionalSources"->Rest[srcs], ConfidenceLevel -> 1.0 |> ]];
      ];

  used = ToFullFormString /@ Cases[children[[2]], LeafNode[Symbol, _, _], {0, Infinity}];
  unusedParams = Select[vars, Function[{c}, !MemberQ[used, ToFullFormString[c]]]];

  Scan[AppendTo[issues, Lint["UnusedVariables", "Unused variable in ``DynamicModule``: " <> format[ToFullFormString[#]] <> ".", "Warning", <|#[[3]], ConfidenceLevel -> 1.0|>]]&, unusedParams];

  issues
]]



(*

With has an undocumented syntax for allowing variables to refer to previously With'd variables

In[32]:= With[{a=1}, {b=Hold[a]}, b+1]
Out[32]= 1+Hold[1]
*)
Attributes[scanWiths] = {HoldRest}

scanWiths[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, selected, paramLists, issues, varsAndVals, vars, vals, usedBody, unusedParams},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  If[empty[children],
    AppendTo[issues, Lint["WithArguments", "``With`` does not have 2 or more arguments.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
    Throw[issues];
  ];

  (*
  Being used as a pattern, so no issues
  *)
  If[MatchQ[children[[1]], CallNode[LeafNode[Symbol, "Pattern" | "Blank" | "BlankSequence" | "BlankNullSequence", _], _, _]],
    Throw[issues]
  ];

  If[Length[children] < 2,
    AppendTo[issues, Lint["WithArguments", "``With`` does not have 2 or more arguments.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
    Throw[issues];
  ];

  If[!MatchQ[Most[children], {CallNode[LeafNode[Symbol, "List", _], _, _]...}],
    AppendTo[issues, Lint["WithArguments", "``With`` does not have a ``List`` for most arguments.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
    Throw[issues];
  ];


  (*
      With[Evaluate[]] denotes meta-programming
  *)
  If[MatchQ[children[[1]], CallNode[LeafNode[Symbol, "Evaluate", _], _, _]],
    Throw[issues]
  ];

  (*
      Use {_, ___} to match 1 or more children instead of using { __ }
      because of bug 382974

      MatchQ[f[], f[__]...] returns True

      Related bugs: 382974
  *)
  If[!MatchQ[Most[children], {CallNode[LeafNode[Symbol, "List", _], { _, ___ }, _]...}],
    AppendTo[issues, Lint["WithArguments", "``With`` does not have a ``List`` with arguments for most arguments.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
    Throw[issues];
  ];

  paramLists = Most[children][[All, 2]];
   
   varsAndVals = Function[{list}, # /. {CallNode[LeafNode[Symbol, "Set"|"SetDelayed", _], {sym:LeafNode[Symbol, _, _], val_}, _] :> {sym, val},
            err_ :> (AppendTo[issues, Lint["WithArguments", "Variable " <> format[ToFullFormString[err]] <> " does not have proper form.\n\
This may be ok if ``With`` is handled programmatically.", "Error", <|#[[3]], ConfidenceLevel -> 0.85|>]]; Nothing)}& /@ list] /@ paramLists;

   varsAndVals = DeleteCases[varsAndVals, {}];

   If[empty[varsAndVals], Throw[issues]];

   {vars, vals} = Transpose[Transpose /@ varsAndVals];

    duplicates = Keys[Select[CountsBy[#, ToFullFormString], # > 1 &]]& /@ vars;
      selected = Flatten[Function[{duplicates, vars}, (Select[vars, Function[{c}, ToFullFormString[c] === #]])& /@ duplicates] @@@ Transpose[{duplicates, vars}]];

      If[!empty[selected],
       srcs = #[[3, Key[Source]]]& /@ selected;

      AppendTo[issues, Lint["DuplicateVariables", "Duplicate variables in ``With``.", "Error",
          <| Source->First[srcs], "AdditionalSources"->Rest[srcs], ConfidenceLevel -> 1.0 |> ]];
      ];

  usedBody = ToFullFormString /@ Cases[Last[children], LeafNode[Symbol, _, _], {0, Infinity}];

  usedAtVariousScopes = FoldList[Join[#1, ToFullFormString /@ Cases[#2, LeafNode[Symbol, _, _], {0, Infinity}]]&, usedBody, vals // Reverse] // Reverse;

  unusedParams = Function[{vars, useds}, Select[vars, Function[{c}, !MemberQ[useds, ToFullFormString[c]]]]] @@@ Transpose[{vars, Most[usedAtVariousScopes]}];

  unusedParams = Flatten[unusedParams];

  Scan[AppendTo[issues, Lint["UnusedVariables", "Unused variable in ``With``: " <> format[ToFullFormString[#]] <> ".", "Warning", <|#[[3]], ConfidenceLevel -> 1.0|>]]&, unusedParams];

  issues
]]



Attributes[scanBlocks] = {HoldRest}

scanBlocks[pos_List, astIn_] :=
Catch[
 Module[{ast, node, head, children, data, duplicates, selected, params, issues, varsWithSet, varsWithoutSet, toDelete},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  head = node[[1]];
  children = node[[2]];
  data = node[[3]];
  issues = {};

  If[empty[children],
    AppendTo[issues, Lint["BlockArguments", format[head["String"]] <> " does not have 2 arguments.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];

  (*
  Being used as a pattern, so no issues
  *)
  If[MatchQ[children[[1]], CallNode[LeafNode[Symbol, "Pattern" | "Blank" | "BlankSequence" | "BlankNullSequence", _], _, _]],
    Throw[issues]
  ];

  If[Length[children] != 2,
    AppendTo[issues, Lint["BlockArguments", format[head["String"]] <> " does not have 2 arguments.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];

  (*
      Block[Evaluate[]] denotes meta-programming
  *)
  If[MatchQ[children[[1]], CallNode[LeafNode[Symbol, "Evaluate", _], _, _]],
    Throw[issues]
  ];

  If[!MatchQ[children[[1]], CallNode[LeafNode[Symbol, "List", _], _, _]],
    AppendTo[issues, Lint["BlockArguments", format[head["String"]] <> " does not have a ``List`` for argument 1.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];

  params = children[[1,2]];

  varsWithSet = {};
  varsWithoutSet = {};

  Scan[# /. {
    CallNode[LeafNode[Symbol, "Set"|"SetDelayed", _], {sym:LeafNode[_, _, _], _}, _] :> (AppendTo[varsWithSet, sym]),
    sym:LeafNode[Symbol, _, _] :> (AppendTo[varsWithoutSet, sym]),
    err_ :> (AppendTo[issues, Lint["BlockArguments", "Variable " <> format[ToFullFormString[err]] <> " does not have proper form.", "Error", <|#[[3]], ConfidenceLevel -> 0.85|>]])}&, params];

  vars = varsWithSet ~Join~ varsWithoutSet;

  duplicates = Keys[Select[CountsBy[vars, ToFullFormString], # > 1&]];
  selected = Flatten[Select[vars, Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];
  
  If[!empty[selected],
       srcs = #[[3, Key[Source]]]& /@ selected;

      AppendTo[issues, Lint["DuplicateVariables", "Duplicate variables in ``Block``.", "Error",
          <| Source->First[srcs], "AdditionalSources"->Rest[srcs], ConfidenceLevel -> 1.0 |> ]];
      ];

  (*
  Give unused Block variables its own tag
  *)

  used = ToFullFormString /@ Cases[children[[2]], LeafNode[Symbol, _, _], {0, Infinity}];
  unusedParams = Select[vars, Function[{c}, !MemberQ[used, ToFullFormString[c]]]];

  (*
  Now we will use heuristics to pare down the list of unused variables in Block
  *)

  (*
  if you have Block[{x = 1}, b]  then it is probably on purpose
  i.e., setting x to a value shows intention
  *)
  toDelete = varsWithSet;
  unusedParams = Complement[unusedParams, toDelete];

  (*
  Blocking fully-qualified symbol is probably on purpose
  *)
  toDelete = Select[unusedParams, fullyQualifiedSymbolQ];
  unusedParams = Complement[unusedParams, toDelete];

  (*
  after removing fully-qualified symbols, now scan for lowercase symbols and only let those through

  on the assumption that lowercase symbols will be treated as "local" variables
  *)
  unusedParams = Select[unusedParams, lowercaseSymbolQ];
  
  Scan[AppendTo[issues, Lint["UnusedBlockVariables", "Unused variable in " <> format[head["String"]] <> ": " <>
    format[ToFullFormString[#]] <> ".", "Warning", <|#[[3]], ConfidenceLevel -> 0.90|>]]&, unusedParams];

  issues
]]

(*
if there is a ` anywhere in the symbol, then assume it is fully-qualified
*)
fullyQualifiedSymbolQ[LeafNode[Symbol, s_, _]] :=
  StringContainsQ[s, "`"]

lowercaseSymbolQ[LeafNode[Symbol, s_, _]] :=
  StringMatchQ[s, RegularExpression["[a-z].*"]]





Attributes[scanOptionals] = {HoldRest}

scanOptionals[pos_List, astIn_] :=
 Module[{ast, node, children, data, issues, opt, pats},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  issues = {};

  (*
  scan for e.g., a_:b:c
  a named pattern in 2nd arg of Optional
  *)
  opt = children[[2]];
  pats = Cases[opt, CallNode[LeafNode[Symbol, "Pattern", _], _, _], {0, Infinity}];
  Scan[(
    AppendTo[issues, Lint["NamedPatternInOptional", "Named pattern " <> format[ToFullFormString[#[[2]][[1]]]] <> " in ``Optional``.", "Error", <|#[[3]], ConfidenceLevel -> 0.95|>]]
  )&, pats];

  issues
]


Attributes[scanBadSymbols] = {HoldRest}

scanBadSymbols[pos_List, astIn_] :=
 Module[{ast, node, name, data, issues, src},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  name = node["String"];
  data = node[[3]];

  issues = {};

  src = data[Source];

  Switch[name,
    "Failed",
      AppendTo[issues, Lint["BadSymbol", "``Failed`` does not exist in **System`** context.", "Error", <|Source->src, ConfidenceLevel -> 0.75, CodeActions->{CodeAction["Replace with ``$Failed``", ReplaceNode, <|Source->src, "ReplacementNode"->ToNode[$Failed]|>]} |>]]
    ,
    "Boolean",
      AppendTo[issues, Lint["BadSymbol", "``Boolean`` does not exist in **System`** context.", "Error", <|Source->src, ConfidenceLevel -> 0.75, CodeActions->{CodeAction["Replace with ``True|False``", ReplaceNode, <|Source->src, "ReplacementNode"->ToNode[True|False]|>]}|>]]
    ,
    "Match",
      AppendTo[issues, Lint["BadSymbol", "``Match`` does not exist in **System`** context.", "Error", <|Source->src, ConfidenceLevel -> 0.75, CodeActions->{CodeAction["Replace with ``MatchQ``", ReplaceNode, <|Source->src, "ReplacementNode"->ToNode[MatchQ]|>]}|>]]
    ,
    _,
      (* everything else *)
      AppendTo[issues, Lint["BadSymbol", "``" <> name <> "`` does not exist in **System`** context.", "Error", <|Source->src, ConfidenceLevel -> 0.75|>]]
  ];

  issues
]

Attributes[scanUndocumentedSymbols] = {HoldRest}

scanUndocumentedSymbols[pos_List, astIn_] :=
 Module[{ast, node, name, data, issues},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  name = node["String"];
  data = node[[3]];

  issues = {};

  Switch[name,
    "Fail" | "System`Fail",
      AppendTo[issues, Lint["UndocumentedSymbol", "Undocumented symbol: ``Fail``.\n\
Symbol ``Fail`` is an undocumented **System`** symbol.\n\
Did you mean ``$Failed``?", "Warning", <| data, ConfidenceLevel -> 0.55 |>]]
    ,
    _,
      AppendTo[issues, Lint["UndocumentedSymbol", format[name] <> " is not documented.", "Remark", <|data, ConfidenceLevel -> 0.55 |>]]
  ];

  issues
]


Attributes[scanObsoleteSymbols] = {HoldRest}

scanObsoleteSymbols[pos_List, astIn_] :=
 Module[{ast, node, name, data, issues},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  name = node["String"];
  data = node[[3]];

  issues = {};

  AppendTo[issues, Lint["ObsoleteSymbol", format[name] <> " is obsolete.", "Warning", <|data, ConfidenceLevel -> 0.55 |>]];

  issues
]



Attributes[scanExperimentalSymbols] = {HoldRest}

scanExperimentalSymbols[pos_List, astIn_] :=
 Module[{ast, node, name, data, issues},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  name = node["String"];
  data = node[[3]];

  issues = {};

  AppendTo[issues, Lint["ExperimentalSymbol", format[name] <> " is experimental.", "Warning", <|data, ConfidenceLevel -> 0.55 |>]];

  issues
]


(*

too noisy

Attributes[scanJavaSystemSymbols] = {HoldRest}

scanJavaSystemSymbols[pos_List, astIn_] :=
 Module[{ast, node, name, data, issues},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  name = node["Name"];
  data = node[[3]];

  issues = {};

  AppendTo[issues, Lint["BadJavaSymbol", "Bad Java symbol: ``" <> name <> "``.\n\
It is possible that JLink can create this symbol in System` and interfere with the symbol's definition.", "Remark", data]];

  issues
]

*)










Attributes[scanSelfAssignments] = {HoldRest}

scanSelfAssignments[pos_List, astIn_] :=
Catch[
 Module[{ast, node, var, data, parentPos, parent},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  var = node[[2]][[1]];
  data = node[[3]];

  (*
  It is a common idiom to do With[{a = a}, foo], so do not warn about that

  And there are enough occurrences of Block and Module, so add those too
  *)
  If[Length[pos] >= 4,
    parentPos = Drop[pos, -4];
    parent = Extract[ast, {parentPos}][[1]];
    If[MatchQ[parent, CallNode[LeafNode[Symbol, "Block" | "DynamicModule" | "Module" | "With", _], _, _]],

      (* and make sure to only skip  With[{a = a}, foo]  and still report   With[{}, a=a] *)
      If[pos[[-3]] == 1,
        Throw[{}]
      ]
    ]
  ];

  {Lint["SelfAssignment", "Self assignment: " <> format[ToFullFormString[var]] <> ".", "Warning", <|data, ConfidenceLevel -> 0.90|>]}
]]





Attributes[scanLoadJavaClassSystem] = {HoldRest}

scanLoadJavaClassSystem[pos_List, astIn_] :=
Catch[
 Module[{ast, node, var, data},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  var = node[[2]][[1]];
  data = node[[3]];

  {Lint["LoadJavaClassSystem", "``LoadJavaClass[\"java.lang.System\"]`` redefines symbols in **System`** context.\n\
This can interfere with system functionality.\n\
Did you mean ``LoadJavaCLass[\"java.lang.System\", AllowShortContext->False]``?", "Warning", <|data, ConfidenceLevel -> 0.95|>]}
]]



Attributes[scanPrivateContextNode] = {HoldRest}

scanPrivateContextNode[pos_List, astIn_] :=
Catch[
 Module[{ast, node, str, strData},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];

  str = node[[1]][[1]];
  strData = str[[3]];

  {Lint["SuspiciousPrivateContext", "Suspicious context: ``\"Private`\"``.\n\
Did you mean ``\"`Private`\"``?", "Error", <|strData, ConfidenceLevel -> 0.95|>]}
]]



Attributes[scanSessionSymbols] = {HoldRest}

scanSessionSymbols[pos_List, astIn_] :=
Catch[
 Module[{ast, node, data},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  data = node[[3]];

  {Lint["SessionSymbol", "Suspicious use of session symbol " <> format[node["String"]] <> ".", "Warning", <|data, ConfidenceLevel -> 0.55|>]}
]]



Attributes[scanSessionCalls] = {HoldRest}

scanSessionCalls[pos_List, astIn_] :=
Catch[
 Module[{ast, node, data, head},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  head = node[[1]];
  data = node[[3]];

  {Lint["SessionSymbol", "Suspicious use of session function " <> format[head["String"]] <> ".", "Warning", <|data, ConfidenceLevel -> 0.55|>]}
]]



(*

too noisy

Attributes[scanDebugCalls] = {HoldRest}

scanDebugCalls[pos_List, astIn_] :=
Catch[
 Module[{ast, node, data, head},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  head = node[[1]];
  data = node[[3]];

  {Lint["DebugSymbol", "Suspicious use of debug function ``" <> head["String"] <> "``.", "Warning", data]}
]]
*)


(*

too noisy

experimental

Attributes[scanPatternTestMissingPattern] = {HoldRest}

scanPatternTestMissingPattern[pos_List, astIn_] :=
Catch[
 Module[{ast, node, data},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  data = node[[3]];

  {Lint["PatternTestMissingPattern", "``PatternTest`` is missing a pattern on the LHS.", "Error", data]}
]]
*)




(*

too noisy

experimental

Attributes[scanRHSPatterns] = {HoldRest}

scanRHSPatterns[pos_List, astIn_] :=
Catch[
 Module[{ast, node, data, children, lhs, rhs, lhsPatNames, rhsPatSyms, badSyms, issues},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  data = node[[3]];

  children = node[[2]];
  lhs = children[[1]];
  rhs = children[[2]];

  lhsPatNames = Cases[lhs, CallNode[SymbolNode[Symbol, "Pattern", _], {SymbolNode[Symbol, name_, _], _}, _] :> name, {0, Infinity}];
  rhsPatSyms = Cases[rhs, CallNode[SymbolNode[Symbol, "Pattern", _], {sym:SymbolNode[Symbol, _, _], _}, _] :> sym, {0, Infinity}];

  badSyms = Select[rhsPatSyms, MemberQ[lhsPatNames, #["Name"]]&];

  issues = {};

  Scan[(
    AppendTo[issues, Lint["RHSPattern", "Pattern ``" <> #["Name"] <> "`` appears on the RHS.", "Error", #[[3]]]]
  )&, badSyms];

  issues
]]

*)



(*

experimental

must handle Conditions:

f[] := g[] /; cond

f[] := Module[{}, g[] /; cond]

f[] := Module[{}, a[];b[];g[] /; cond]



Attributes[scanFiles] = {HoldRest}

scanFiles[pos_List, astIn_] :=
Catch[
 Module[{ast, node, data, defs, issues, children, lhss, duplicates, selected},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  defs = Cases[children, CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], _, _]];

  lhss = defs[[All, 2, 1]];

  duplicates = Keys[Select[CountsBy[lhss, ToFullFormString], # > 1&]];
  selected = Flatten[Select[lhss, Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];
  Scan[AppendTo[issues, Lint["DuplicateDefinitions", "Duplicate definition", "Error", #[[3]]]]&, selected];

  issues
]]
*)





Attributes[scanAnds] = {HoldRest}

scanAnds[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, selected, issues, consts},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};
  
  consts = Cases[children, LeafNode[Symbol, "True"|"False", _]];
  Scan[(AppendTo[issues, Lint["LogicalConstant", "Logical constant in ``And``.", "Warning", <|#[[3]], ConfidenceLevel -> 0.95|>]])&, consts];


  duplicates = Keys[Select[CountsBy[children, ToFullFormString], # > 1&]];
  selected = Flatten[Select[children, Function[{key}, ToFullFormString[key] === #]]& /@ duplicates, 1];

  If[!empty[selected],
       srcs = #[[3, Key[Source]]]& /@ selected;

      AppendTo[issues, Lint["DuplicateClauses", "Duplicate clauses in ``And``.", "Error",
          <| Source->First[srcs], "AdditionalSources"->Rest[srcs], ConfidenceLevel -> 0.95 |> ]];
      ];

  issues

  ]]

Attributes[scanOrs] = {HoldRest}

scanOrs[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, selected, issues, consts},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};
  
  consts = Cases[children, LeafNode[Symbol, "True"|"False", _]];
  Scan[(AppendTo[issues, Lint["LogicalConstant", "Logical constant in ``Or``.", "Warning", <|#[[3]], ConfidenceLevel -> 0.95|>]])&, consts];

  duplicates = Keys[Select[CountsBy[children, ToFullFormString], # > 1&]];
  selected = Flatten[Select[children, Function[{key}, ToFullFormString[key] === #]]& /@ duplicates, 1];

  If[!empty[selected],
       srcs = #[[3, Key[Source]]]& /@ selected;

      AppendTo[issues, Lint["DuplicateClauses", "Duplicate clauses in ``Or``.", "Error",
          <| Source->First[srcs], "AdditionalSources"->Rest[srcs], ConfidenceLevel -> 0.95 |> ]];
      ];

  issues

  ]]


Attributes[scanAlternatives] = {HoldRest}

scanAlternatives[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, selected, issues, blanks},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};
  
  (*
      if this is _ | PatternSequence[] then this is ok
  *)
  If[MatchQ[children, {CallNode[LeafNode[Symbol, "Blank", _], {}, _],
                        CallNode[LeafNode[Symbol, "PatternSequence", _], {}, _]}] ||
      MatchQ[children, {CallNode[LeafNode[Symbol, "PatternSequence", _], {}, _],
                        CallNode[LeafNode[Symbol, "Blank", _], {}, _]}],
      Throw[issues]
  ];

  (*
      only test for _
      patterns like a_ may occur in Alternatives
  *)
  blanks = Cases[children, CallNode[LeafNode[Symbol, "Blank", _], {}, _]];

  Scan[(
      AppendTo[issues, Lint["Blank", "Blank in ``Alternatives``.", "Warning",
                        <|#[[3]],
                              ConfidenceLevel -> 0.95|> ]])&, blanks];

  duplicates = Keys[Select[CountsBy[children, ToFullFormString], # > 1&]];
  selected = Flatten[Select[children, Function[{key}, ToFullFormString[key] === #]]& /@ duplicates, 1];

  If[!empty[selected],
       srcs = #[[3, Key[Source]]]& /@ selected;

      AppendTo[issues, Lint["DuplicateClauses", "Duplicate clauses in ``Alternatives``.", "Error",
          <| Source->First[srcs], "AdditionalSources"->Rest[srcs], ConfidenceLevel -> 0.95 |> ]];
      ];

  issues

  ]]






Attributes[scanSlots] = {HoldRest}

scanSlots[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, foundFunction, parent},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  parentPos = pos;
  foundFunction = False;
  While[True,
      If[parentPos == {},
            Break[]
      ];
      parentPos = Drop[parentPos, -1];
      parent = Extract[ast, parentPos];
      If[ListQ[parent],
            parentPos = Drop[parentPos, -1];
            parent = Extract[ast, parentPos];
      ];
      If[MatchQ[parent, CallNode[LeafNode[Symbol, "Function", _], _, _]],
            foundFunction = True;
            Break[]
      ];
  ];

  If[!foundFunction,
      (*
      This is not more confident because there are lots of examples of using # with no containing Function:

      Algebra work
      doing Function @@ {#}
      etc.

      *)
      AppendTo[issues, Lint["MissingFunction", "There is no containing ``Function``.", "Error", <| Source->data[Source], ConfidenceLevel -> 0.90 |>]]
  ];

  issues

]]





Attributes[scanSolverCalls] = {HoldRest}

scanSolverCalls[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  cases = Cases[children, CallNode[LeafNode[Symbol, "EvenQ" | "OddQ" | "PrimeQ", _], _, _], Infinity];

  Scan[(AppendTo[issues, Lint["BadSolverCall", "*Q function in symbolic solver. Did you mean to do this?", "Error", <|Source -> #[[3, Key[Source] ]], ConfidenceLevel -> 0.90|>]])&, cases];

  issues

]]




Attributes[scanAbstractSyntaxErrorNodes] = {HoldRest}

scanAbstractSyntaxErrorNodes[pos_List, astIn_] :=
 Module[{ast, node, tag, data, tagString, children},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  tag = node[[1]];
  children = node[[2]];
  data = node[[3]];

  tagString = Block[{$ContextPath = {"AbstractSyntaxError`", "System`"}, $Context = "Lint`Scratch`"}, ToString[tag]];

  Switch[tagString,
    "UnhandledCharacter",
        leaf = children[[1]];
        {Lint["UnhandledCharacter", "Unhandled character: " <> format[leaf[[2]]] <> ".", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "ExpectedOperand",
        {Lint["ExpectedOperand", "Expected an expression.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "LinearSyntaxBang",
        {Lint["LinearSyntaxBang", "Invalid syntax for ``\\!``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "NonAssociativePatternTest",
        {Lint["NonAssociativePatternTest", "Invalid syntax for ``?``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "NonAssociativeDirectedEdge",
        {Lint["NonAssociativeDirectedEdge", "Invalid syntax for ``\\[DirectedEdge]``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "NonAssociativeUndirectedEdge",
        {Lint["NonAssociativeUndirectedEdge", "Invalid syntax for ``\\[UndirectedEdge]``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "OpenParen",
        {Lint["OpenParen", "Invalid syntax for ``()``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "OpenSquare",
        {Lint["OpenSquare", "Invalid syntax for ``[]``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "GroupMissingCloser",
        {Lint["GroupMissingCloser", "Missing closing bracket.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "GroupMissingOpener",
        {Lint["GroupMissingOpener", "Missing opening bracket.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "UnterminatedString",
        {Lint["UnterminatedString", "Unterminated string.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "EmptyString",
        {Lint["EmptyString", "Empty string.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "UnterminatedComment",
        {Lint["UnterminatedComment", "Unterminated comment.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    _,
        {Lint[tagString, "Syntax error.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
  ]
]





Attributes[scanAbstractSyntaxIssues] = {HoldRest}

(*
Just directly convert AbstractSyntaxIssues to Lints
*)
scanAbstractSyntaxIssues[pos_List, astIn_] :=
Module[{ast, data, issues, syntaxIssues},
  ast = astIn;
  data = Extract[ast, {pos}][[1]];
  issues = data[AbstractSyntaxIssues];

  syntaxIssues = Cases[issues, SyntaxIssue[_, _, _, _]];

  Lint @@@ syntaxIssues
]



(*

too noisy

scanAlts[pos_, actual_] :=
 
 Module[{node, parentPos, parent, span, opts},
  node = Extract[actual, pos];
  opts = node[[-1]];
  parentPos = Most[pos];
  parent = Extract[actual, parentPos];
  Switch[parent,
   _,
   span = opts[Source];
   {Lint["Weird Alternatives", "Warning", <|Source -> span|>]}
   ]
  ]
*)




(*

too noisy

scanStrings[StringNode[s_, {}, opts_]] :=
Module[{span, origLen},
  span = opts[Source];
  origLen = span[[2, 2]] - span[[1, 2]] + 1;
  If[StringLength[s] != origLen,
    Lint["Unrecognized character", "Error", <|Source -> span|>]
    ,
    {}
  ]
]
*)



(*

too noisy

scanSetDelayeds[
  BinaryNode[SetDelayed, {left_, right_}, opts_?AssociationQ]] :=
 Module[{warnings, opLocation, duplicates, selected, name1, 
   name2, span1, span2},

   warnings = {};


(*
too noisy


  name1 = DeclarationName[left];
  If[name1 === $Failed,
   AppendTo[warnings, 
    Lint["Internal failure", "Fatal", <|Source -> opts[Source]|>]]
   ];
  If[MatchQ[right, BinaryNode[Set, _, _]],
   name2 = DeclarationName[right[[2, 1]]];
   If[name2 === $Failed,
    AppendTo[warnings, 
     Lint["Internal failure", 
      "Fatal", <|Source -> opts[Source]|>]]
    ];
   If[name1 =!= name2,
    span1 = left[[-1]][Source];
    span2 = right[[2, 1, -1]][Source];
    AppendTo[warnings, 
     Lint["Memoization of different symbols", 
      "Warning", <|Source -> {span1[[1]], span2[[2]]}|>]]
    ];
   ];
   XPrint["scanSetDelayeds returning: ", warnings];

   *)
   warnings
  ]

scanSetDelayeds[args___] := (
  Message[scanSetDelayeds::unhandled, {args}];
  $Failed
)
*)



End[]


EndPackage[]
