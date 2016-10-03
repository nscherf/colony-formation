(* Mathematica Package *)

BeginPackage["Eidomatica`Utilities`"]
Needs["Eidomatica`"]
Needs["Eidomatica`Internal`"]
(* Exported symbols added here with SymbolName::usage *)

eBinaryExport::usage = "eBinaryExport[fileName, vectorField] saves the given vector field under the given file name in binary format."
eBinaryImport::usage = "eBinaryImport[fileName] loads the vector field saved in binary format under the given file name."
eComposeFileName::usage = "eComposeFileName[prefix,postfix,index,indicatorLength] gives a file name with the prefix followed by an index and terminated by postfix. The length of the index part and accordingly the leading zeros is specified by indicatorLength (default 4)."
eChunkedParallelExecution::usage = "eChunkedParallelExecution[function,list,chunkSize] parallel mapping of given function on chunks of size chunkSize generated from list. After processing of each chunk parallel kernels are closed to free memory, if further chunks need to be processed kernels will be automatically started."
eCreateOutline::usage="eCreateOutline[mask,pts] returnes an outline (with pts points) from the given mask image."
eDisplaceByVectorField::usage="eDisplaceByVectorField[labelMatrix,vectorField] displaces the values in the labelMatrix by the given vector field."
	Interpolated::usage="Interpolated is an option for eDisplaceByVectorField[] (either 'True' or 'False'). If 'True' displacement is interpolated."
	BoundaryCondition::usage="BoundaryCondition is an option for eDisplaceByVectorField[] (either 'Fixed' or 'Periodic'). When 'Fixed' displacements outside the image domain are cropped."
eDivergence::usage = "eDivergence[vectorField] computes the magnitude of the vector field's source or sink at all points of the given vector field."
eExportList::usage = "eExportList[list,path,optional_prefix] exports a list of objects into the given path with given format."
	ExportFormat::usage="ExportFormat is an option for eExportList specifying the export format, valid formats are \"PNG\", \"JPG\" and \"TIF\"."
eManipulate::usage = "eManipulate[list] takes list and displays the elements from 1 to Length@list."
eFiniteDifferences::usage = "eFiniteDifferences[matrix] computes the partial derivatives in x and y direction of the given matrix."
eRYBColor::usage = "eRYBColor[angle] creates the RYB color corresponding to the given angle."
eRYBtoRGB::usage = "eRYBtoRGB[color] converts the given RYB color to the RGB color space."
eToPolarCS::usage = "eToPolarCS[point] converts the given point from cartesian to the polar coordinate system. The function returns a point {length,angle}, where angle is in the Interval [-Pi,Pi]."
eToCartesianCS::usage = "eToCartesianCS[vectorField] transforms the given vector field in order that the vectors are given with respect to the cartesian coordinate system."
eToImageCS::usage = "eToImageCS[vectorField] transforms the given vector field in order that the vectors are given with respect to the image coordinate system."
eVectorFieldDimensions::usage = "eVectorFieldDimensions[vectorField] returns the dimensions of the given vector field."
eVectorFieldData::usage = "eVectorFieldData[vectorField] returns the data from a given VectorField[]."
eVectorFieldInfo::usage = "eVectorFieldInfo[vectorField] prints information for the specific vector field."
VectorField::usage = "VectorField[] represents a vector field either in the 'Cartesian' coordinate system or the 'Matrix' coordinate system."
eLogString::usage = "eLogString[message] creates a log message with date and time."

Begin["`Private`"] (* Begin Private Context *) 

Clear[eBinaryExport]
eBinaryExport::idir = "Given directory '`1`' does not exist.";
eBinaryExport[file_String, vectorField_VectorField] := Block[
  	{stream},
  	If[! DirectoryQ@DirectoryName[file],
   		Message[eBinaryExport::idir, file];
   		Abort[]
   	];
  	stream = OpenWrite[file, BinaryFormat -> True];
  	BinaryWrite[stream, eVectorFieldDimensions@vectorField, "UnsignedInteger32"];(*write dimensions*)
  	BinaryWrite[stream, "CoordinateSystem" /. vectorField[[2]], "TerminatedString"];(*write coordinate system*)
  	Map[BinaryWrite[stream, Flatten@#, "Real32"] &, eVectorFieldData@vectorField, {1}];(*write data*)
  	Close[stream];
  	file
]

Clear[eBinaryImport];
eBinaryImport::if = "Given file '`1`' does not exist.";
eBinaryImport[file_String] := Block[
	{stream, magic, dimensions, coordinateSystem, data, field,end, error, alpha, vortexWeight, mu, lambda, boundary, method, actualTime,actualError, parameters},
  	If[! FileExistsQ[file],
   		Message[eBinaryImport];
   		Abort[]
   	];
  	stream = OpenRead[file, BinaryFormat -> True];
  	magic = BinaryRead[stream, "UnsignedInteger32"];
  	If[magic==1447379762,
  		If[$ProcessorType == "x86-64",
  			BinaryReadList[stream,"Real64",2];
  			dimensions=BinaryReadList[stream,"Integer32",2];
  			data=BinaryReadList[stream,"Real64"];
  			field=VectorField[Transpose@Partition[Transpose[Partition[data,Length@data/2]],First@dimensions]],
  			Print["Reading dip flows from 32bit machines currently not supported!"];
  			Abort[]
  		]
  	];
  	If[magic==1278906352,
  		If[$ProcessorType == "x86-64",
  			BinaryReadList[stream,"Real64",2];
  			dimensions=BinaryReadList[stream,"Integer32",2];
  			{end, error, alpha, vortexWeight, mu, lambda}=BinaryReadList[stream,"Real64",6];
  			{boundary, method}=BinaryReadList[stream,"TerminatedString",2];
  			{actualError,actualTime}=BinaryReadList[stream,"Real64",2];
  			data=BinaryReadList[stream,"Real64"];
  			parameters={"EndTime"->end, "MismatchError"->error, "SmoothWeight"->alpha, "VortexWeight"->vortexWeight, "LameMu"->mu, "Lambda"->lambda, "BoundaryCondition"->boundary, "Method"->method, "ActualEndTime"->actualTime, "ActualMismatchError"->actualError};
  			field=VectorField[Transpose@Partition[Transpose[Partition[data,Length@data/2]],First@dimensions],"Cartesian", parameters],
  			Print["Reading dip flows from 32bit machines currently not supported!"];
  			Abort[]
  		]
  	];
  	If[magic!=1447379762 && magic!=1278906352,
  		dimensions=Join[{magic},BinaryReadList[stream,"UnsignedInteger32",2]];
  		coordinateSystem = BinaryRead[stream, "TerminatedString"];
  		data = BinaryReadList[stream, "Real32"];
  		field = VectorField[Fold[Partition[#1, #2] &, data, Drop[Reverse@dimensions, -1]], coordinateSystem]
  	];
  	Close[stream];
  	field
]

Clear[eChunkedParallelExecution]
eChunkedParallelExecution[function_Function, list_, chunkSize_Integer: 16,processors_Integer:$ProcessorCount] := Block[
  	{partitioned, tmp, result},
  	tmp = Partition[PadRight[list, Length@list + chunkSize - Mod[Length@list, chunkSize], {0}], chunkSize];
  	partitioned = DeleteCases[Append[Drop[tmp, -1], Drop[Last@tmp, -(chunkSize - Mod[Length@list, chunkSize])]], {}];
  	Print["Number partitions: ", Length@partitioned];
  	result = {};
  	Do[
   		LaunchKernels[processors];
   		Monitor[
   		AppendTo[result, ParallelMap[function, partitioned[[i]],DistributedContexts->Automatic]],
   		i];
   		CloseKernels[],
   		{i, 1, Length@partitioned}
	];
  	Flatten[result, {1, 2}]
]

Clear[eComposeFileName];
eComposeFileName[prefix_String,postfix_String,index_Integer,indicatorLength_Integer:4]:= FileNameJoin[{prefix}]<>StringTake[ToString@PaddedForm[index, indicatorLength, NumberPadding -> {"0", ""}], -indicatorLength]<>postfix

(*implement displacement of color images!!!*)
Clear[eDisplaceByVectorField]
Options[eDisplaceByVectorField]={Interpolated->False,BoundaryCondition->"Fixed",ReturnType->"Image"};
eDisplaceByVectorField::nobool = nobool;
eDisplaceByVectorField::nomem = nomem;
eDisplaceByVectorField::wdim = "Dimensions of image and vector field are not compatible";
eDisplaceByVectorField[image:{{__?NumericQ}..},vectorField_VectorField,opts:OptionsPattern[]]:=Block[
	{interpolated,boundaryCondition,returnType,data=Transpose@image,dimensions,vf,df,displaced,idata,f},

	(*options validation*)
	{interpolated,boundaryCondition,returnType}=OptionValue[{Interpolated,BoundaryCondition,ReturnType}];
	IsBoolean[eDisplaceByVectorField,Interpolated,interpolated];
	MemberCheck[eDisplaceByVectorField,BoundaryCondition,boundaryCondition,{"Periodic","Fixed"}];
	MemberCheck[eDisplaceByVectorField,ReturnType,returnType,{"Image","Matrix"}];
	
	vf=eVectorFieldData@vectorField;
	If[!SameQ[Dimensions@data,Drop[Dimensions@vf,-1]],
		Message[eDisplaceByVectorField::wdim];
		Abort[]
	];
	
	(*implementation*)
	dimensions= Dimensions@data;
	Switch[boundaryCondition,
		"Periodic",
		df[{x_,y_},{dx_,dy_}]:=Sequence@@{Mod[x-dx,First@dimensions,1],Mod[y-dy,Last@dimensions,1]},(*displacement function*)
		"Fixed",
		df[{x_,y_},{dx_,dy_}]:=Sequence@@{Clip[x-dx,{1,First@dimensions}],Clip[y-dy,{1,Last@dimensions}]}
	];
	
	f[pos_]:=Block[ 
		{floor,pixels,x,y,intensities,maxFrequency,max,pixel}, 
		floor=Floor[pos];
		{x,y}=dimensions;
		pixels=Union@({df[#,{0,0}]}&/@(floor+#&/@{{0,0},{0,1},{1,1},{1,0}}));
		intensities=Tally@Extract[data,pixels];
		maxFrequency=Max[Last/@intensities];
		max=Select[intensities,Last@#==maxFrequency&,4];
		If[Length@max>1||Length@max==0,
			pixel={df[Round[pos],{0,0}]};
			data[[Sequence@@pixel]],
			max[[1,1]] 
		]
	];
	If[interpolated,
		(*idata=ListInterpolation[data];
		displaced=MapIndexed[idata[df[#2,#1]]&,vf,{2}]*)
		DistributeDefinitions[vf,f,df];
		displaced=ParallelTable[f[{x,y}-vf[[x,y]]],{x,1,First@dimensions},{y,1,Last@dimensions}],
		displaced=MapIndexed[data[[df[#2,Round@#1]]]&,vf,{2}]
	];
	Switch[returnType,
		"Matrix",
		Transpose@displaced,
		"Image",
		Image@Transpose@displaced
	]
]
eDisplaceByVectorField[image_SparseArray,vectorField_VectorField,opts:OptionsPattern[]]:=eDisplaceByVectorField[Normal[image],vectorField,opts]
eDisplaceByVectorField[image_SparseArray,vectorField_String,opts:OptionsPattern[]]:=eDisplaceByVectorField[Normal[image],eBinaryImport[vectorField],opts]
eDisplaceByVectorField[image_Image,vectorField_String,opts:OptionsPattern[]]:=eDisplaceByVectorField[image,eBinaryImport[vectorField],opts]
eDisplaceByVectorField[image_Image,vectorField_VectorField,opts:OptionsPattern[]]:=eDisplaceByVectorField[ImageData@image,vectorField,opts]
eDisplaceByVectorField[image_Image,vectorFieldList:{__String},opts:OptionsPattern[]]:=Fold[eDisplaceByVectorField[#1,eBinaryImport[#2],opts]&,image,vectorFieldList]
eDisplaceByVectorField[image_Image,vectorFieldList:{__VectorField},opts:OptionsPattern[]]:=Fold[eDisplaceByVectorField[#1,#2,opts]&,image,vectorFieldList]

Clear[eDivergence];
eDivergence[field : {{{_?NumericQ, _?NumericQ} ..} ..}] := Block[
	{xComp, yComp},
	xComp = field[[All, All, 1]];
	yComp = field[[All, All, 2]];
	MapThread[
		Append, {MapThread[Prepend, {ListConvolve[{{1, -1}}, Drop[xComp, None, -1]], xComp[[All, 1]]}], -xComp[[All, -2]]}] +
		Append[Prepend[ListConvolve[{{1}, {-1}}, Drop[yComp, -1, None]], yComp[[1, All]]], -yComp[[-2, All]]]
]

Clear[exportList]
Options[exportList]={AbortWhenOverride->False};
exportList[list_List,path_String,prefix_String,postfix_String,exporter_Symbol,opts:OptionsPattern[]]:=Block[
	{digits},
  	digits=Length[IntegerDigits[Length[list]]];
  	MapIndexed[exporter[eComposeFileName[FileNameJoin[{path,prefix}],postfix,First@#2,digits],#1]&,list]
]

Clear[eExportList]
Options[eExportList]={AbortWhenOverride->False,ExportFormat->"PNG"};
SetAttributes[eExportList,HoldFirst];
eExportList::nomem = nomem;
eExportList::invdir="Given export directory does not exist or is not empty.";
eExportList[list_,path_String,prefix_String:"image",opts:OptionsPattern[]]:=Block[
	{ask,exportFormats,postfix},
	ask=OptionValue[AbortWhenOverride];
	If[! DirectoryQ[path] || (ask && 
    	Length@FileNames["*.mat", {path}] > 0),
  		Message[eExportList::invdir];
   		Abort[]
  	];
	exportFormats=Cases[$ExportFormats, x_ /; StringMatchQ[x, RegularExpression["[A-Z|0-9]+"]]];
	MemberCheck[eExportList,ExportFormat,OptionValue[ExportFormat],exportFormats];
	postfix=StringJoin[".",ToLowerCase[OptionValue[ExportFormat]]];
	If[MatchQ[list,{__VectorField}],
		exportList[list,path,"vf",".dat",eBinaryExport,Sequence@@FilterRules[{opts},{AbortWhenOverride}]],
		exportList[list,path,prefix,postfix,Export,Sequence@@FilterRules[{opts},{AbortWhenOverride}]]
	]
]

Clear[eManipulate];
eManipulate[list_List]:=Manipulate[Check[list[[i]],$Failed],{i,1,Length@list,1}]

	
Clear[eFiniteDifferences];
eFiniteDifferences[matrix:{{__?NumericQ} ..}] := Module[
	{},
	Transpose[
		{PadRight[ListConvolve[{{1, -1}}, matrix], Dimensions[matrix]], 
    	PadRight[ListConvolve[{{1}, {-1}}, matrix], Dimensions[matrix]]}, 
    {3, 1, 2}]
]

Clear[eRYBColor]
eRYBColor[angle_] := Which[
  angle <= -1/3, {1, 0, 0} + (angle + 1)/(2/3)*{-1, 1, 0},
  angle >= 1/3, {1, 0, 0} + (angle - 1)/(-2/3)*{-1, 0, 1},
  True, {0, 1, 0} + (angle + 1/3)/(2/3)*{0, -1, 1}
]
 
 Clear[eRYBtoRGB];
Options[eRYBtoRGB] = {InterpolationMethod -> "Linear"};
eRYBtoRGB::invop = "InterpolationMethod has to be either \"Linear\" or \"Cubic\"";
eRYBtoRGB[color : {red_, yellow_, blue_}, opts : OptionsPattern[]] := Module[
	{method, a, b, c, d, e, f, g, h, i1, i2, j1, j2, w1, w2, if},
  
  	(*option validation*)
  	method = OptionValue[InterpolationMethod];
  	If[MemberQ[{"Linear", "Cubic"}, method],
   		Switch[method,(*defintion of interpolation function*)
    	"Linear",
     		if[x_] := x,
    	"Cubic", 
    		if[x_] := 3 x^2 - 2 x^3
    	],
   		Message[eRYBtoRGB::invop];
   		Abort[]
   	];
  
  	(*implementation*)
  	{a, b, c, d, e, f, g, h} = {{1., 1., 1.}, {1., 0., 0.}, {1., .5, 0.}, {1., 1., 0.}, {.163, .373, .6}, {.5, 0., .5}, {.2, .094, 0.}, {0., .66, .2}};(*RYB interpolation cube*)
  	(*trilinear cubic? interpolation*)
  	i1 = a + if[red] (b - a);
  	i2 = e + if[red] (f - e);
  	j1 = d + if[red] (c - d);
  	j2 = h + if[red] (g - h);
  	w1 = i1 + if[blue] (i2 - i1);
  	w2 = j1 + if[blue] (j2 - j1);
  	w1 + if[yellow] (w2 - w1)
]

Clear[eToCartesianCS];
eToCartesianCS[vectorField_VectorField] := 
	If[MatchQ["Cartesian", "CoordinateSystem" /. vectorField[[2]]],
  		vectorField,
  		eToCartesianCS[eVectorFieldData[vectorField]]
	]
eToCartesianCS[vectorField : {{{_?NumericQ, _?NumericQ} ..} ..}] := 
	VectorField[Map[{Last@#, -First@#} &, vectorField, {2}], "Cartesian"]

Clear[eToImageCS];
eToImageCS[vectorField_VectorField] := 
	If[MatchQ["Image", "CoordinateSystem" /. vectorField[[2]]],
  		vectorField,
  		eToImageCS[eVectorFieldData[vectorField]]
	]
eToImageCS[vectorField : {{{_?NumericQ, _?NumericQ} ..} ..}] := 
	VectorField[Map[{-Last@#, First@#} &, vectorField, {2}],"Image"]

(*polar transform with angle given in the range [-Pi..Pi]*)
Clear[eToPolarCS];
eToPolarCS[field:{{{_?NumericQ, _?NumericQ} ..} ..}] := ParallelMap[eToPolarCS, field, {2}]
eToPolarCS[{x_, y_}] := polar[x, y]
polar = Compile[{{u, _Real}, {v, _Real}}, 
  N@{Norm[{u, v}], 
    Which[u > 0, ArcTan[v/u], u < 0 && v >= 0, ArcTan[v/u] + Pi, 
     u < 0 && v < 0, ArcTan[v/u] - Pi, u == 0 && v > 0, Pi/2, 
     u == 0 && v < 0, -Pi/2, u == 0 && v == 0, 0.]}
]

Clear[eVectorFieldDimensions];
eVectorFieldDimensions[vectorField_VectorField]:=Dimensions@eVectorFieldData@vectorField

Clear[eVectorFieldData];
eVectorFieldData[vectorField_VectorField] := vectorField[[1]]

Clear[eVectorFieldInfo];
eVectorFieldInfo[vectorField_VectorField]:=vectorField[[2]]

Clear[VectorField];
VectorField::invp = "Invalid second parameter. CoordinateSystem can be either 'Cartesian' or 'Image'";
VectorField[field : {{{_?NumericQ, _?NumericQ} ..} ..}, coord_String:"Cartesian"] := 
	If[MemberQ[{"Cartesian", "Image"}, coord],
  		VectorField[field, "CoordinateSystem"->coord],
  		Message[VectorField::invp];
  		Abort[]
]
VectorField[field : {{{_?NumericQ, _?NumericQ} ..} ..}, coord_String, param:{__Rule}] := 
	If[MemberQ[{"Cartesian", "Image"}, coord],
  		VectorField[field, Join[{"CoordinateSystem"->coord}, param]],
  		Message[VectorField::invp];
  		Abort[]
]
Format[VectorField[field : {{{_?NumericQ, _?NumericQ} ..} ..}, __]] := VectorField["<>"]


Clear[compRose, clockMove]
compRose = {"N" -> {-1, 0}, "NE" -> {-1, 1}, "E" -> {0, 1}, "SE" -> {1, 1}, "S" -> {1, 0}, "SW" -> {1, -1}, "W" -> {0, -1}, "NW" -> {-1, -1}}(*positions around central pixel*);
clockMove = {"N", "NE", "E", "SE", "S", "SW", "W", "NW"};
(*boundary update step*)
Clear[searchStep]
searchStep[fpx_List, bpx_List, objectLevel_?NumericQ, data_, fpx0_List, fpx1_List] := Module[
  	{sPos, mov, hit, lastBG},
  	sPos = Position[clockMove /. compRose, bpx - fpx];
  	mov = RotateLeft[clockMove, Flatten@sPos] /. compRose;
  	hit = Flatten@First[Position[Table[data[[Sequence @@ (fpx + mov[[i]])]], {i, 1, Length[mov]}], objectLevel]];
  	lastBG = If[Sequence @@ hit - 1 == 0, -1, Sequence @@ hit - 1];
  	{fpx + mov[[Sequence @@ hit]], fpx + mov[[lastBG]]}
]
(*tracking of the object boundary after Moore, see [Gonzalez, Woods: Digital Image Processing 2008, p. 196 ff] *)
Clear[boundaryTracking]
boundaryTracking[img_?ImageQ /; ImageChannels[img] == 1, objectLevel_?NumericQ] := Module[
  	{dat, pxUpLeft, iStep, fpx1, outline, bpx1},
  	dat = ImageData[ImagePad[img, 1, Black], ImageType[img]];
  	pxUpLeft = First[Sort[Position[dat, objectLevel]]];
  	iStep = searchStep[pxUpLeft, "W" /. compRose, objectLevel, dat, pxUpLeft, {0, 0}];
  	fpx1 = First[iStep];
  	bpx1 = iStep[[2]];
  	outline = First /@ 
  		Prepend[
  			Prepend[
      			Drop[NestWhileList[
        			searchStep[#[[1]], #[[2]], objectLevel, dat, pxUpLeft, 
          			fpx1] &, iStep, #1[[1]] != pxUpLeft && #2[[1]] != bpx1 &, 2], -1
          		], iStep
          	], {pxUpLeft, pxUpLeft - ("W" /. compRose)}
        ];
  	{ImagePad[Image[Fold[ReplacePart[#1, #2 -> objectLevel] &, dat /. objectLevel -> 0, outline]], -1], # - {0.5, 1.5} & /@ outline}
]

(*wrapper function, takes input label image and returns an outline \
polygon with the desired number of points given by npoints*)
Clear[eCreateOutline];
eCreateOutline[mask_Image, npts_Integer,"ImageCS"] :=Block[
	{x,y,pts},
	{x,y}=ImageDimensions[mask];
	pts=Table[#[x], {x, 1 - 1/npts, 0, -1/npts}] &@BSplineFunction[boundaryTracking[mask, 1.][[2]], "SplineClosed" -> True, SplineDegree -> 2];
	Map[{Last@#, y - First@#} &, pts]
]
eCreateOutline[mask_Image, npts_Integer,"CartesianCS"]:=Table[#[x], {x, 1 - 1/npts, 0, -1/npts}] &@BSplineFunction[boundaryTracking[mask, 1.][[2]], "SplineClosed" -> True, SplineDegree -> 2]

Clear[eLogString];
eLogString[message_String]:=Print[StringJoin[{DateString[{"Year", "-", "Month", "-", "Day", " ", "Time"}], " ", message}]]
End[] (* End Private Context *)

EndPackage[]