
# Compatibility


## Source Compatibility

CodeInspector has source compatibility with 11.0+


## FrontEnd Compatibility

Any source .wl files that have `(* ::Package::"Tags" *)` or `(* ::Code::Initialization::"Tags" *)` syntax may only be edited with a version 12.3+ FE 


## WolframVersion

WolframVersion in PacletInfo is 12.1+ to maintain the same minimum required version as CodeParser


## Earlier Versions

Manually modify WolframVersion in PacletInfo.wl to allow paclet installation.
