(* Mathematica Package *)

BeginPackage["Eidomatica`Tracking`"]
Needs["GraphUtilities`"]
Needs["Eidomatica`"]
Needs["Eidomatica`Internal`"]
Needs["Eidomatica`Utilities`"]
Needs["Eidomatica`TrackingUtilities`"]

(* Exported symbols added here with SymbolName::usage *)  

eDistanceTracking::usage="eDistanceTracking[]";
eFluidTracking::usage="eFluidTracking[binaryImagePath, flowFieldPath, exportPath] compute fluid tracks over all images."
	InitialLabelMatrix::usage = "InitialLabelMatrix uses the given intial label matrix instead of the first binary image."
	Overlap::usage = "Overlap the overlap constraint (percentage of overlap)."
	IncludeAppearingCells::usage = "IncludeAppearingCells if True than appearing objects are included in the tracking process."
	FileNameDivision::usage="FileNameDivision the file name for the file where the divisions are saved in."
labelPropagate::usage="labelPropagate do not use this function!!"
	

Begin["`Private`"] (* Begin Private Context *)

(*Distance based tracking*)
Clear[eDistanceTracking];
eDistanceTracking::nobool = nobool;
Options[eDistanceTracking]={AbortWhenOverride->False};
eDistanceTracking::override = override;
eDistanceTracking[project_TrackingProject,opts:OptionsPattern[]]:=Module[
	{metaFiles,objectives,tracks,largestID,fileName,newProject},
	(*option validation*)
	IsBoolean[eDistanceTracking,"AskWhenOverride",OptionValue[AbortWhenOverride]];
	
	(*implementation*)
	metaFiles = eGetFileNames[project, "MetaFiles"];
	objectives = {"ObjectID", "ObjectCenter"};
	tracks = ParallelTable[
  		distanceBasedMapping[objectives /. Import[metaFiles[[i]]], objectives /. Import[metaFiles[[i + 1]]]]
  		,{i,Length@metaFiles - 1}, DistributedContexts -> Automatic
  	];
	tracks=WeakComponents[Flatten@tracks];
	
	(*add single tracks*)
	largestID=Max["ObjectID"/.Import[Last@metaFiles]];
	tracks=Sort[Join[tracks,List/@Complement[Table[i, {i, largestID}], Flatten[tracks]]], Length@#1 > Length@#2 &];
	
	fileName = FileNameJoin[{("ProjectPath" /. Level[project, 1]),"tracks.m"}];
	If[FileExistsQ[fileName] && OptionValue[AbortWhenOverride],
		Message[eDistanceTracking::override];
		Abort[]
	];
	Export[fileName,tracks];
	eLogString["eDistanceTracking : Track file saved at " <> fileName];
	newProject=eUpdateProject[project,"TrackFile"];
	eUpdateMetaData[newProject];
	eLogString["eDistanceTracking : Tracking [successful]."];
	newProject
]

Clear[distanceBasedMapping];
distanceBasedMapping[meta1:{{_Integer, {_?NumericQ, _?NumericQ}} ..}, meta2:{{_Integer, {_?NumericQ, _?NumericQ}} ..}] := Module[
	{meta1IDs, meta1Centers, meta2IDs, meta2Centers, distancesFrom, distancesTo, minimalDistancesFrom, minimalDistancesTo, from, to},
  	{meta1IDs, meta1Centers} = Transpose@meta1;
 	{meta2IDs, meta2Centers} = Transpose@meta2;
 	distancesFrom = Outer[Norm[#1 - #2] &, meta1Centers, meta2Centers, 1];
 	distancesTo = Outer[Norm[#1 - #2] &, meta2Centers, meta1Centers, 1];
 	minimalDistancesFrom = Flatten[Ordering[#, 1] & /@ distancesFrom];
 	minimalDistancesTo = Flatten[Ordering[#, 1] & /@ distancesTo];
 	DeleteCases[
 		MapIndexed[
 			(from = First[#2];
     		to = #1;
     		If[minimalDistancesTo[[to]] == from, 
      			meta1IDs[[from]] -> meta2IDs[[to]]
      		]
      		) &, minimalDistancesFrom
      	], Null
	]
]

Clear[labelPropagate]
Options[labelPropagate]={LabelSize->"32Bit",Overlap->.5,IncludeAppearingCells->True,SizeConstraint->None,ConstraintType->"Standard"};
labelPropagate::nonum = nonum;
labelPropagate::nobool = nobool;
labelPropagate::wdim = "Label matrices have different dimensions, the dimensionality has to be equal.";
labelPropagate[displacedLabelMatrix:{{__Integer}..},segmentedLabelMatrix:{{__Integer}..},opts:OptionsPattern[]]:=Module[
	{overlap,labelSize,includeAppearing,pack,unpack,associations,forward,backward,union,intersection,divisions,mergers,divisionMergers,cellIDoffsets,unique,displacedMasks,segmentedMasks,mask,cloneID,offset,tmp,newCellIDOffsets},

	{overlap,labelSize,includeAppearing}=OptionValue[{Overlap,LabelSize,IncludeAppearingCells}];
	
	(*option validation*)
	NumberCheck[labelPropagate,Overlap,overlap];
	MemberCheck[labelPropagate,LabelSize,labelSize];
	IsBoolean[labelPropagate,IncludeAppearingCells,includeAppearing];
	If[!SameQ[Dimensions@displacedLabelMatrix,Dimensions@segmentedLabelMatrix],
		Message[labelPropagate::wdim];
		Abort[]
	]; 
	
	(*implementation*)
	If[labelSize=="32Bit",
		pack=ePackLabel32;
		unpack=eUnpackLabel32,
		pack=ePackLabel64;
		unpack=eUnpackLabel64
	];
	associations=Tally[Transpose@{Flatten@displacedLabelMatrix,Flatten@segmentedLabelMatrix}];
	forward=associate[associations,overlap,"forward"];
	backward=associate[associations,overlap,"backward"];
	union=DeleteCases[Union[forward,backward],Rule[x_,y_]/;(x==0||y==0)];
	intersection=DeleteCases[Intersection[forward,backward],Rule[x_,y_]/;(x==0||y==0)];
	divisions=Cases[GatherBy[intersection,First],x_/;Length@x>1];
	mergers=Flatten@Cases[GatherBy[union,Last],x_/;Length@x>1];
	divisionMergers=Intersection[Flatten@divisions,Flatten@mergers];(*cell divides an instantly merges with other cell*)
	mergers=Complement[mergers,divisionMergers];(*remove division-mergers from mergers*)
	unique=Complement[intersection,Flatten@divisions,mergers];
	
	Print[++$counter];
	cellIDoffsets=Dispatch@Map[First@First@#->Max[Last/@#]&,GatherBy[Map[unpack,$assignedIDs],First]];
	displacedMasks=getMasks[displacedLabelMatrix];(*components with the same label, not necessarily connected*)
	segmentedMasks=ComponentMeasurements[segmentedLabelMatrix,"Mask"];(*a mask for each connected component*)
	mask=SparseArray[ConstantArray[0,Dimensions@displacedLabelMatrix]];
	mask=Fold[#1+(First@#2*(Last@#2/.segmentedMasks))&,mask,unique];(*take masks from segmentation for unique mappings*)
	If[divisions!={},
		tmp={};
		mask+=Total@
			Flatten[
				MapIndexed[(
					cloneID=First@unpack@First@#1;
					newCellIDOffsets=Dispatch@Join[Map[First@First@# -> Max[Last /@ #] &, GatherBy[Map[unpack, Last/@Flatten[tmp]],First]],{cloneID->-1}];
					offset=Max[cloneID/.cellIDoffsets,cloneID/.newCellIDOffsets];
					AppendTo[tmp,First@#1->(pack[cloneID,offset+1])];
					If[!MemberQ[divisionMergers,#1],
						(pack[cloneID,offset+1])*(Last@#1/.segmentedMasks),(*normal division*)
						(pack[cloneID,offset+1])*(First@#1/.displacedMasks)*(Last@#1/.segmentedMasks)(*division-merger*)
					])&,
				divisions,{2}],(*take masks from segmentation for divisions*)
				{2,1}
			];
		AppendTo[$divisionsList,tmp]
	];
	mask=Fold[
		(tmp=ComponentMeasurements[MorphologicalComponents[((First@#2/.displacedMasks)*(Last@#2/.segmentedMasks))],{"Count","Mask"}];
		#1+(First@#2)*Last@Last@First@Cases[tmp, x_ /; First@Last@x == Max[First /@ Last /@ tmp]])&
		,mask,mergers];(*take intersection between displacement and segmentation for mergers*)
	If[includeAppearing,
		mask+=Total@Map[(
			++$largestCloneID;
			pack[$largestCloneID,1]*(Last@#1/.segmentedMasks))&,
			appearing[associations]
		]
	];
	mask
]

(*Fluid based tracking*)
Clear[eFluidTracking];
Options[eFluidTracking]=Union@Flatten[{Options[labelPropagate], Options[eRelabel], Options[eDisplaceByVectorField], Options[eDeleteObjects], {AbortWhenOverride -> True, InitialLabelMatrix -> None,FileNameDivision->"./divisions.m"}}];
eFluidTracking::nobool = nobool;
eFluidTracking::nomem = nomem;
eFluidTracking::invinit = "Invalid option value for InitialLabelMatrix. The option has te be either 'None' or a matrix of integers.";
eFluidTracking::invdir = "Given export directory `1` does not exist or is not empty.";
eFluidTracking::override = "There are files in the given directory `1` and override option is not turned on."
eFluidTracking::wfc = "Wrong file count. If image path contains n images there have to be n-1 flows."
eFluidTracking[binPath_String,flowPath_String,path_String,opts:OptionsPattern[]]:=Module[
	{imageNames,flowNames},
	imageNames = FileNames["Images" /. Eidomatica`TrackingUtilities`Private`$searchPattern, {binPath}, IgnoreCase -> True];
	flowNames = FileNames["Flows" /. Eidomatica`TrackingUtilities`Private`$searchPattern, {flowPath}, IgnoreCase -> True];
	eFluidTracking[imageNames,flowNames,path,opts];
]
eFluidTracking[imageNames:{___String},flowNames:{___String},path_String,opts:OptionsPattern[]]:=Module[
	{index=1,opts1,opts2,opts3,opts4,opts5,override,initialLabelMatrix,labelSize,divisions,digits,initial,pack,unpack,iterate},
	
	opts1=Sequence@@FilterRules[{opts},First/@Options[labelPropagate]];
	opts2=Sequence@@Join[DeleteCases[FilterRules[{opts},First/@Options[eDisplaceByVectorField]],ReturnType->_],{ReturnType->"Matrix"}];
	opts3=Sequence@@FilterRules[{opts},First/@Options[eRelabel]];
	opts4=Sequence@@FilterRules[{opts},First/@Options[eDeleteObjects]];
	opts5=Sequence@@Join[DeleteCases[{opts4},SizeConstraint->_],{SizeConstraint->(
			Switch[OptionValue[SizeConstraint],
				None|{None,None},
				None,
				{_Integer,None},
				{IntegerPart@N[First[OptionValue[SizeConstraint]]/3],None},
				{None,_Integer},
				{None,IntegerPart@N[Last[OptionValue[SizeConstraint]]/3]},
				{_Integer,_Integer},
				IntegerPart@N[OptionValue[SizeConstraint]/3]
			]
		)}];
	
	eLogString["labelPropagate: " <> ToString[DeleteDuplicates[Join[{opts1},Options[labelPropagate]], First@#1 === First@#2 &]]];
	eLogString["eDisplaceByVectorField: " <> ToString[DeleteDuplicates[Join[{opts2},Options[eDisplaceByVectorField]], First@#1 === First@#2 &]]];
	eLogString["eRelabel: " <> ToString[DeleteDuplicates[Join[{opts3},Options[eRelabel]], First@#1 === First@#2 &]]];
	eLogString["eDeleteObjects: " <> ToString[DeleteDuplicates[Join[{opts4},Options[eDeleteObjects]], First@#1 === First@#2 &]]];
	eLogString["eDeleteObjects for displaced masks: " <> ToString[DeleteDuplicates[Join[{opts5},Options[eDeleteObjects]], First@#1 === First@#2 &]]];
	eLogString["Found " <> ToString[Length@imageNames] <> " images."];
	eLogString["Found " <> ToString[Length@flowNames] <> " flows."];
	
	{override, initialLabelMatrix, labelSize,divisions} = OptionValue[{AbortWhenOverride,InitialLabelMatrix,LabelSize,FileNameDivision}];
	
	(*option validation and parameter check*)
	IsBoolean[eFluidTracking,AbortWhenOverride,override];
	If[!(MatrixQ[initialLabelMatrix,IntegerQ] || SameQ[initialLabelMatrix,None]),
		Message[eFluidTracking::invinit];
		Abort[]
	];
	MemberCheck[eFluidTracking,LabelSize,labelSize,{"32Bit","64Bit"}];
	If[!DirectoryQ[path],
		Message[eFluidTracking::invdir,path];
		Abort[]
	];
	If[(!override &&Length@FileNames["*.mat",{path}]>0&&FileExistsQ[divisions]),
		Message[eFluidTracking::override,path];
		Abort[]
	];
	If[(Length@imageNames-1) != Length@flowNames,
		Message[eFluidTracking::wfc];
		Abort[]
	];
	
	(*implementation*)
	digits=Length[IntegerDigits[Length[imageNames]+1]];
	If[initialLabelMatrix===None,
		initial=eRelabel[MorphologicalComponents[Import[First@imageNames]],opts3],
		initial=eRelabel[initialLabelMatrix,opts3]
	];
	If[labelSize=="32Bit",
		pack=ePackLabel32;
		unpack=eUnpackLabel32,
		pack=ePackLabel64;
		unpack=eUnpackLabel64
	];
	
	$counter = 1;
	Print[$counter];
	$divisionsList = {};
	$largestCloneID=Max[First/@unpack/@Union@Normal@Flatten[initial]];
	$assignedIDs=DeleteCases[Union@Flatten[Normal[initial]],0];
	iterate[initialLabel_,{imageName_String,flowName_String}]:=Block[
		{label},
		label=labelPropagate[eDeleteObjects[eDisplaceByVectorField[initialLabel,eBinaryImport[flowName],opts2],opts5],eDeleteObjects[MorphologicalComponents[Import[imageName]],opts4],opts1];
		$assignedIDs=Union[$assignedIDs,DeleteCases[Union@Flatten[Normal[label]],0]];
		Export[eComposeFileName[FileNameJoin[{path,"label"}],".mat",index,digits],label];
		++index;
		label
	];
	Export[eComposeFileName[FileNameJoin[{path,"label"}],".mat",index,digits],initial];
	++index;
	Fold[iterate[#1,#2]&,
		initial,Transpose@{Drop[imageNames,1],flowNames}
	];
	Export[divisions,$divisionsList];
]

Clear[getMasks];
getMasks[matrix:{{__Integer}..}]:=Last[First[#]]->SparseArray[#,Dimensions[matrix]]/Last[First[#]]&/@GatherBy[DeleteCases[Flatten[MapIndexed[#2->#1&,matrix,{2}]],_->0],Last]

Clear[associate]
associate[association:{{{_Integer,_Integer},_Integer}..},overlap_?NumericQ,direction_String:"backward"]:=Block[
	{f,same,g,total,rules},
	f[x_]:=Sort[x,Last@#1>= Last@#2&];
	Switch[direction,
		"backward",
		same=f/@GatherBy[association,Last@First@#&],
		"forward",
		same=f/@GatherBy[association,First@First@#&]
	];
	g[x_]:=(
		total=Total[Last/@x];
		Cases[x,y_/;(Last@y/total)>= overlap/Length[x]]
	);
	rules=Flatten[Rule@@@#&/@Map[First,DeleteCases[(g/@same),{}],{2}]];
	Switch[direction,
		"backward",
		DeleteCases[rules,0->_],
		"forward",
		DeleteCases[rules,_->0]
	]
]

Clear[appearing]
appearing[association:{{{_Integer,_Integer},_Integer}..}]:=Block[
	{appearingCells},
	appearingCells=Cases[association,{{0,y_},_}/;!MemberQ[Last[First@#]&/@DeleteCases[association,{{0,_},_}],y]];
	(First@#->Last@#)&/@(First/@appearingCells)
]

End[] (* End Private Context *)

EndPackage[]