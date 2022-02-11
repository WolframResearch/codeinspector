
Needs["CodeInspector`"]

Needs["CodeParser`"]




Test[
	CodeInspect["Confirm[expr]"]
	,
	{InspectionObject["NoSurroundingEnclose", "``Confirm`` has no tag or surrounding ``Enclose``.", "Error", <|Source -> {{1, 1}, {1, 8}}, ConfidenceLevel -> 0.9|>]}
	,
	TestID->"Confirm-20220211-C5P4I5"
]

Test[
	CodeInspect["Confirm[expr, info]"]
	,
	{InspectionObject["NoSurroundingEnclose", "``Confirm`` has no tag or surrounding ``Enclose``.", "Error", <|Source -> {{1, 1}, {1, 8}}, ConfidenceLevel -> 0.9|>]}
	,
	TestID->"Confirm-20220211-K3X6R8"
]

Test[
	CodeInspect["ConfirmBy[expr, f]"]
	,
	{InspectionObject["NoSurroundingEnclose", "``ConfirmBy`` has no tag or surrounding ``Enclose``.", "Error", <|Source -> {{1, 1}, {1, 10}}, ConfidenceLevel -> 0.9|>]}
	,
	TestID->"Confirm-20220211-X2D1H7"
]

Test[
	CodeInspect["ConfirmBy[expr, f, info]"]
	,
	{InspectionObject["NoSurroundingEnclose", "``ConfirmBy`` has no tag or surrounding ``Enclose``.", "Error", <|Source -> {{1, 1}, {1, 10}}, ConfidenceLevel -> 0.9|>]}
	,
	TestID->"Confirm-20220211-E7M1T9"
]

Test[
	CodeInspect["ConfirmMatch[expr, form]"]
	,
	{InspectionObject["NoSurroundingEnclose", "``ConfirmMatch`` has no tag or surrounding ``Enclose``.", "Error", <|Source -> {{1, 1}, {1, 13}}, ConfidenceLevel -> 0.9|>]}
	,
	TestID->"Confirm-20220211-T5Q5V2"
]

Test[
	CodeInspect["ConfirmMatch[expr, form, info]"]
	,
	{InspectionObject["NoSurroundingEnclose", "``ConfirmMatch`` has no tag or surrounding ``Enclose``.", "Error", <|Source -> {{1, 1}, {1, 13}}, ConfidenceLevel -> 0.9|>]}
	,
	TestID->"Confirm-20220211-R5K0P9"
]

Test[
	CodeInspect["ConfirmAssert[test]"]
	,
	{InspectionObject["NoSurroundingEnclose", "``ConfirmAssert`` has no tag or surrounding ``Enclose``.", "Error", <|Source -> {{1, 1}, {1, 14}}, ConfidenceLevel -> 0.9|>]}
	,
	TestID->"Confirm-20220211-O2E6U9"
]

Test[
	CodeInspect["ConfirmAssert[test, info]"]
	,
	{InspectionObject["NoSurroundingEnclose", "``ConfirmAssert`` has no tag or surrounding ``Enclose``.", "Error", <|Source -> {{1, 1}, {1, 14}}, ConfidenceLevel -> 0.9|>]}
	,
	TestID->"Confirm-20220211-K1M6T3"
]

Test[
	CodeInspect["Enclose[Confirm[expr]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-X1O2R2"
]

Test[
	CodeInspect["Enclose[Confirm[expr, info]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-X6A4X2"
]

Test[
	CodeInspect["Enclose[ConfirmBy[expr, f]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-O2N4N6"
]

Test[
	CodeInspect["Enclose[ConfirmBy[expr, f, info]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-K2W8N9"
]

Test[
	CodeInspect["Enclose[ConfirmMatch[expr, form]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-O6C1J8"
]

Test[
	CodeInspect["Enclose[ConfirmMatch[expr, form, info]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-K6O8S2"
]

Test[
	CodeInspect["Enclose[ConfirmAssert[test]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-B5W4N2"
]

Test[
	CodeInspect["Enclose[ConfirmAssert[test, info]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-E2N1F9"
]

Test[
	CodeInspect["Enclose[f[g[h[Confirm[expr]]]]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-P5C0D9"
]

Test[
	CodeInspect["Enclose[f[g[h[Confirm[expr, info]]]]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-P6R7Q6"
]

Test[
	CodeInspect["Enclose[f[g[h[ConfirmBy[expr, f]]]]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-A9M7R7"
]

Test[
	CodeInspect["Enclose[f[g[h[ConfirmBy[expr, f, info]]]]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-D5G7I1"
]

Test[
	CodeInspect["Enclose[f[g[h[ConfirmMatch[expr, form]]]]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-X8H6T4"
]

Test[
	CodeInspect["Enclose[f[g[h[ConfirmMatch[expr, form, info]]]]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-F2G8Q0"
]

Test[
	CodeInspect["Enclose[f[g[h[ConfirmAssert[test]]]]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-X3U6C1"
]

Test[
	CodeInspect["Enclose[f[g[h[ConfirmAssert[test, info]]]]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-G1I1T5"
]





Test[
	CodeInspect["Confirm[expr, info, tag]"]
	,
	{InspectionObject["NoSurroundingEnclose", "``Confirm`` has no surrounding ``Enclose``.", "Remark", <|Source -> {{1, 1}, {1, 8}}, ConfidenceLevel -> 0.9|>]}
	,
	TestID->"Confirm-20220211-J5P5R6"
]

Test[
	CodeInspect["ConfirmBy[expr, f, info, tag]"]
	,
	{InspectionObject["NoSurroundingEnclose", "``ConfirmBy`` has no surrounding ``Enclose``.", "Remark", <|Source -> {{1, 1}, {1, 10}}, ConfidenceLevel -> 0.9|>]}
	,
	TestID->"Confirm-20220211-Q2R8F9"
]

Test[
	CodeInspect["ConfirmMatch[expr, form, info, tag]"]
	,
	{InspectionObject["NoSurroundingEnclose", "``ConfirmMatch`` has no surrounding ``Enclose``.", "Remark", <|Source -> {{1, 1}, {1, 13}}, ConfidenceLevel -> 0.9|>]}
	,
	TestID->"Confirm-20220211-N7E2L7"
]

Test[
	CodeInspect["ConfirmAssert[test, info, tag]"]
	,
	{InspectionObject["NoSurroundingEnclose", "``ConfirmAssert`` has no surrounding ``Enclose``.", "Remark", <|Source -> {{1, 1}, {1, 14}}, ConfidenceLevel -> 0.9|>]}
	,
	TestID->"Confirm-20220211-U6T7H9"
]

Test[
	CodeInspect["Enclose[Confirm[expr, info, tag]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-L4M0N0"
]

Test[
	CodeInspect["Enclose[ConfirmBy[expr, f, info, tag]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-F1M1D1"
]

Test[
	CodeInspect["Enclose[ConfirmMatch[expr, form, info, tag]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-H5K2Y1"
]

Test[
	CodeInspect["Enclose[ConfirmAssert[test, info, tag]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-C0D2O9"
]

Test[
	CodeInspect["Enclose[f[g[h[Confirm[expr, info, tag]]]]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-A6L0Q6"
]

Test[
	CodeInspect["Enclose[f[g[h[ConfirmBy[expr, f, info, tag]]]]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-I1G6L5"
]

Test[
	CodeInspect["Enclose[f[g[h[ConfirmMatch[expr, form, info, tag]]]]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-K5C4I5"
]

Test[
	CodeInspect["Enclose[f[g[h[ConfirmAssert[test, info, tag]]]]]"]
	,
	{}
	,
	TestID->"Confirm-20220211-X4J3J4"
]







