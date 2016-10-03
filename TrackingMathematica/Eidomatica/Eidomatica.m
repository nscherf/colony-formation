(* Mathematica Package *)

(* Created by the Wolfram Workbench Oct 27, 2011 *)

BeginPackage["Eidomatica`"]
(* Exported symbols added here with SymbolName::usage *)
AbortWhenOverride::usage="AbortWhenOverride if TRUE the function given this option aborts when file would be overriden."
Iterations::usage = "Iterations is an option for various functions specifying the maximal number of iterations done."
StepSize::usage  = "StepSize is an option for various functions specifying the step size done in each iteration step."
ReturnAll::usage = "ReturnAll is an option for various functions specifying if only last or all results of the iteration are returned."
InterpolationMethod::usage = "InterpolationMethod is an option for eVectorFieldPlot[field] and eRYBtoRGB[color] specifying with which method the RYB color is converted to RGB."
ReturnType::usage="ReturnType is an option for eDisplaceByVectorField[] and CreateMask[] which specifies if the return type is an image or a matrix, possible values are \"Matrix\" or \"Image\"."
SizeConstraint::usage="SizeConstraint is an option for eDeleteObjects[], eCreateMetaData[], eRelabel[] and eFluidTracking[] deleting all objects smaller than the given size."
eVersion::usage="eVersion gives the current version of the Eidomatica package."

$librariesFound::usage="$librariesFound True if libEidomatica is found else False."
llVectorFieldPlot::usage="llVectorFieldPlot do not use this function!"
llVersion::usage="llVersion do not use this function!"


Begin["`Private`"]
(* Implementation of the package *)

(*****************************************************
 Initial Preparations
   a) load the opencv
   b) add the path to the examples to the system
   c) kill the binary when needed 
*)
$mlbinpath = {"Eidomatica", "Binaries"};
$mlbinary = "opencv"

$opencvLink=FileNames[$mlbinary, ToFileName[Prepend[$mlbinpath, #]] & /@ $Path];
opencv::nolink="Can't find \"opencv\". Some functions may not work.";

If[!(Or @@ (StringMatchQ[StringReplace[First[#],"\""->""], "*opencv*"] & /@ Links[])),
  If[{}===$opencvLink,
    Message[opencv::nolink],
    $opencvLink=Install[First[$opencvLink]]
  ]
]

lib::nolink="Can't find \"libEidomatica\" at `1`. Some functions may work."
library=FileNameJoin[{$UserBaseDirectory, "Applications/Eidomatica/LibraryResources", $SystemID, "libEidomatica"}];
$libEidomatica=FindLibrary[library];
$librariesFound=If[$libEidomatica === $Failed,
	Message[lib::nolink, library];
 	False,
 	llVectorFieldPlot = LibraryFunctionLoad[$libEidomatica,"llVectorFieldPlot", {{Real, 3, "Constant"}}, {Real, 3}];
 	llVersion = LibraryFunctionLoad[$libEidomatica,"llVersion", {}, "UTF8String"];
 	True
];

eVersion[]:=Block[
	{versionFile, eidomaticaVersion = "Unknown", eidomaticaLibVersion = "Unknown"},
	versionFile = FindFile["Eidomatica/eVersion.txt"];
	If[!SameQ[versionFile,$Failed],
		eidomaticaVersion = ToString@Import[versionFile]
	];
	If[$librariesFound,
		eidomaticaLibVersion = ToString[llVersion[]]
	];
	Print["Revisions: {Eidomatica Package: " <> eidomaticaVersion <> ", libEidomatica: " <> eidomaticaLibVersion <> "}"];
]

End[]

EndPackage[]

