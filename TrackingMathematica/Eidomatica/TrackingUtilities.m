(* Mathematica Package *)

BeginPackage["Eidomatica`TrackingUtilities`"]
Needs["Eidomatica`"]
Needs["Eidomatica`Utilities`"]
Needs["Eidomatica`Internal`"]
(* Exported symbols added here with SymbolName::usage *)

(*functions and options*)
eCreateTrackingProject::usage="eCreateTrackingProject[path] creates a TrackingProject in the given path, including directory creation and search for images, and returns the project.
eCreateTrackingProject[] opens a dialog for directory selection, after search for images and directory creations is accomplished. After the new project is returned."
	ComputeFlows::usage="ComputeFlows is an option for eCreateTrackingProject, if 'True' flows are computed (neccessary for fluid tracking)."
	RegisterImages::usage="RegisterImages"
eGetFileNames::usage="eGetFileNames[project,objective] returns the file names in project for the given objective."
TrackingProject::usage="TrackingProject[] represents a tracking project with corresponding files and image processing pipelines."
eSaveTrackingProject::usage="eSaveTrackingProject[project] saves the tracking meta data in project to ProjectPath/TrackingProject.m.
eSaveTrackingProject[project,fileName] saves the tracking meta data in project to the given fileName."
eLoadTrackingProject::usage="eLoadTrackingProject[] opens a FileDialog to open a tracking project file and returns the project.
eLoadTrackingProject[fileName] loads the specific tracking project given by fileName and returns the project."
eValidateProject::usage="eValidateProject[project] validates the given project, if given files exist etc."
eUpdateProject::usage="eUpdateProject[project] updates given project, new files in standard directories get added etc. and returns the updated project.
eUpdateProject[project,objective] updates the just the given objective of the project and returns the updated project."
eUpdateMetaData::usage="eUpdateMetaData[project] updates the meta data of the project (every object gets an track id), iff the project has a TrackFile."
eRegisterImages::usage="eRegisterImages[project] registers the images using the given RegisterPipeline and returns the updated project."
eSegmentImages::usage="eSegmentImages[project] segments either the raw images or the registered images (if present) and returns the updated project."
eComputeFlows::usage="eComputeFlows[project] computes the flows between images according to the given FlowPipeline and returns the updated project."
	FlowDirection::usage="FlowDirection is an option for eComputeFlows and determines if \"Forward\" the flow pipe gets the images in order (flow[i1,i2]) and if \"Backward\" the images are computed by flow[i2,i1]."
eCreateMetaData::usage="eCreateMetaData[project] creates the meta files for the project if there are any segmented images and returns the updated project."
	OutlineControlPoints::usage"OutlineControlPoints specifies the number of control points used for the outline of the identified objects."
eSetSegmentationPipeline::usage="eSetSegmentationPipeline[project,pipeline] set the SegmentationPipeline for the project where pipeline is a function receiving an image and returning one!! The updated project is returned."
eSetRegisterPipeline::usage="eSetRegisterPipeline[project, pipeline] sets the RegisterPipeline for the project where pipeline is a function returning an image!! The updated project is returned."
eSetFlowPipeline::usage="eSetFlowPipeline[project,pipeline] sets the FlowPipeline for the project where pipeline is a function receiving two images (two slots) and returning a VectorField. The updated project is returned."

(*fluid tracking utilities*)
ePackLabel32::usage="ePackLabel32[cloneID,cellID]"
eUnpackLabel32::usage="eUnpackLabel32[id]"
ePackLabel64::usage="ePackLabel64[cloneID,cellID]"
eUnpackLabel64::usage="eUnpackLabel64[id]"
eRelabel::usage="eRelabel[labelMatrix]"
eDeleteObjects::usage="eDeleteObjects[labelMatrix]"
	ConstraintType::usage="ConstraintType set the type of the contraint function to either 'Standard' where the size constraint is equal at every image position or to 'Mercator' where the size is adjusted according to distortion in the Mercator projection."
	MaximalSize::usage="MaximalSize is an option setting the upper bound for the size constraint on the mercator projection."
eUnpackLabelMatrix::usage="eUnpackLabelMatrix[labelMatrix]"
	Target::usage="Target"
eExtractTracksFromLabels::usage = "eExtractTracksFromLabels[filenames] given a list of label file names tracks without division etc. are extracted."


(*options found in several functions*)
ExportFileFormat::usage="ExportFileFormat"
LabelSize::usage="LabelSize"


Begin["`Private`"] (* Begin Private Context *) 
$searchPattern = {
	"Images" -> {"*.tif", "*.tiff", "*.png", "*.jpg", "*.jpeg"}, 
	"RegisteredImages" -> {"*.tif", "*.tiff", "*.png", "*.jpg", "*.jpeg"},
	"Flows" -> {"*.dat"},
	"MetaFiles" -> {"*.m"}, 
	"LabelImages" -> {"*.m", "*.mat", "*.tif", "*.tiff", "*.png", "*.jpg", "*.jpeg"}
};
$imageDirectory="img";
$registrationDirectory="reg";
$segmentationDirectory="bin";
$flowDirectory="flows";
$labelDirectory="label";
$metaDirectory="meta";

Clear[eCreateTrackingProject];
eCreateTrackingProject::noimg = "Did not found any images.";
eCreateTrackingProject::nodir = 
  "The directory '`1`' does not exist.";
Options[eCreateTrackingProject] = {RegisterImages -> False, ComputeFlows->False};
eCreateTrackingProject[path_String: "", opts : OptionsPattern[]] := Module[
	{projectPath, imagePath, regPath, flowPath, metaPath, labelPath, imageNames, regNames, flowNames, metaNames, labelNames, trackFile},
  
  	IsBoolean[eCreateTrackingProject,"RegisterImages",OptionValue[RegisterImages]];
  	IsBoolean[eCreateTrackingProject,"ComputeFlows",OptionValue[ComputeFlows]];
  	
  	(*search project directory for project specific directories*)
  	If[path == "",(*check if path is set*)
   		projectPath = SystemDialogInput["Directory", NotebookDirectory[], WindowTitle -> "Select project directory..."];
 		If[projectPath === $Canceled,
    		Abort[]
    	],
   		If[DirectoryQ[path],
    		projectPath = path,
    		Message[eCreateTrackingProject::nodir, path];
    		Abort[]
    	]
   	];
  
  	(*select image directory*)
  	If[DirectoryQ[FileNameJoin[{projectPath, $imageDirectory}]],
   		imagePath = FileNameJoin[{projectPath, $imageDirectory}],
   		eLogString["eCreateTrackingProject : It is STRONGLY recommended to put the images under <project-path>/img! Otherwise you may face unexpected behaviour."];
   		DialogInput[DialogNotebook[{TextCell["No image directory found. Please select the directory containing the images!"], Button["Proceed", DialogReturn[1]]}]];
   		imagePath = SystemDialogInput["Directory", projectPath, WindowTitle -> "Select image directory..."];
   		If[imagePath === $Canceled,
    		Abort[]
    	]
   	];
  
  	(*registered images*)
  	If[DirectoryQ[FileNameJoin[{projectPath, $registrationDirectory}]],
   		regPath = FileNameJoin[{projectPath, $registrationDirectory}],
   		If[OptionValue[RegisterImages],
    		regPath = CreateDirectory[FileNameJoin[{projectPath, $registrationDirectory}]],
    		regPath = ""
    	]
   	];
   	
   	(*flows*)
   	If[DirectoryQ[FileNameJoin[{projectPath, $flowDirectory}]],
   		flowPath = FileNameJoin[{projectPath, $flowDirectory}],
   		If[OptionValue[ComputeFlows],
    		flowPath = CreateDirectory[FileNameJoin[{projectPath, $flowDirectory}]],
    		flowPath = ""
    	]
   	];
  
  	(*select label path*)
  	If[DirectoryQ[FileNameJoin[{projectPath, $segmentationDirectory}]],
   		labelPath = FileNameJoin[{projectPath, $segmentationDirectory}],
   		labelPath = CreateDirectory[FileNameJoin[{projectPath, $segmentationDirectory}]]
   	];
   	
   	(*select meta directory*)
  	If[DirectoryQ[FileNameJoin[{projectPath, $metaDirectory}]],
   		metaPath = FileNameJoin[{projectPath, $metaDirectory}],
   		metaPath = CreateDirectory[FileNameJoin[{projectPath, $metaDirectory}]]
   	];
  
  	(*check for existing files*)
  	imageNames = getFileNames[imagePath, "Images"];
  	If[Length@imageNames > 0,
   		eLogString["eCreateTrackingProject : Found " <> ToString[Length@imageNames] <> " images."],
   		Message[eCreateTrackingProject::noimg]
   	];
   	If[OptionValue[RegisterImages],
  		regNames = getFileNames[regPath, "RegisteredImages"];
  		If[Length@regNames > 0,
   			eLogString["eCreateTrackingProject : Found " <> ToString[Length@regNames] <> " registered images. Registered images are used for further processing."]
   		],
   		regNames={}
   	];
   	If[OptionValue[ComputeFlows],
   		flowNames = getFileNames[regPath, "Flows"];
  		If[Length@flowNames > 0,
   			eLogString["eCreateTrackingProject : Found " <> ToString[Length@flowNames] <> " flow files."]
   		],
   		flowNames={}
   	];
  	metaNames = getFileNames[metaPath, "MetaFiles"];
  	If[Length@metaNames > 0,
   		eLogString["eCreateTrackingProject : Found " <> ToString[Length@metaNames] <> " meta files."]
   	];
  	labelNames = getFileNames[labelPath, "LabelImages"];
  	If[Length@labelNames > 0,
   		eLogString["eCreateTrackingProject : Found " <> ToString[Length@labelNames] <> " label images."]
   	];
   	trackFile = FileNames["tracks.m",{projectPath}];
   	If[trackFile!={},
   		trackFile=First@trackFile;
   		eLogString["eCreateTrackingProject : Found track file " <> trackFile <> "."],
   		trackFile = ""
   	];
   	
  	TrackingProject[projectPath, imagePath, regPath, flowPath, metaPath, labelPath, 
  		imageNames, regNames, flowNames, metaNames, labelNames, trackFile]
]

Clear[getFileNames]
getFileNames[path_String, objective_String] := Block[
  	{},
  	FileNameTake /@ FileNames[objective /. $searchPattern, {path}, IgnoreCase -> True]
]

Clear[eGetFileNames];
eGetFileNames::nomem = nomem;
eGetFileNames[project_TrackingProject,objective_String]:=Module[
	{data,path,fileNames},
	MemberCheck[eGetFileNames,"objective",objective,{"Images","RegisteredImages","Flows","LabelImages","MetaFiles"}];
	data=Level[project,1];
	{path,fileNames}=objective/.data;
	FileNameJoin[{path,#}]&/@fileNames
]

Clear[TrackingProject];
TrackingProject[
	projectPath_String, 
	imagePath_String, 
	regPath_String,
	flowPath_String,
  	metaPath_String, 
  	labelPath_String, 
  	imageNames:{__String}, 
  	regNames:{___String},
  	flowNames:{___String},
  	metaNames:{___String}, 
  	labelNames:{___String},
  	trackFile_String
  	] := TrackingProject[
 		"SelfReference" -> "", 
 		"ProjectPath" -> projectPath, 
  		"Images" -> {imagePath, imageNames}, 
  		"RegisteredImages" -> {regPath, regNames},
  		"Flows" -> {flowPath, flowNames},
  		"MetaFiles" -> {metaPath, metaNames}, 
  		"LabelImages" -> {labelPath, labelNames}, 
  		"RegisterPipeline" -> "", 
  		"SegmentationPipeline" -> "",
  		"FlowPipeline" -> "",
  		"TrackFile" -> trackFile
  		]
Format[TrackingProject[__Rule]] := TrackingProject["<>"]

Clear[eSaveTrackingProject];
eSaveTrackingProject[project_TrackingProject, fileName_String: ""] := Module[
  	{default = FileNameJoin[{"ProjectPath" /. Level[project, 1], "TrackingProject.m"}]},
  	If[fileName == "",
   		Export[default, 
   		ReplacePart[project, 1 -> ("SelfReference" -> default)]],
   		Export[fileName, project]
   	]
]

Clear[eLoadTrackingProject];
eLoadTrackingProject::invpro = "The selected file does not contain a 'TrackingProject'.";
eLoadTrackingProject::nocons = "The TrackingProject loaded is not consistent with the files on the hard disk.";
eLoadTrackingProject[fileName_String: ""] := Module[
	{init, name, project},
  	If[fileName == "",
  		init = NotebookDirectory[];
  		If[init==$Failed,
   			name = SystemDialogInput["FileOpen", WindowTitle -> "Select project file..."],
   			name = SystemDialogInput["FileOpen", init, WindowTitle -> "Select project file..."]
  		];
   	If[! (name === $Canceled),
    	project = 
     	ReplacePart[Import[name], 1 -> ("SelfReference" -> name)],
    	Abort[]
    ],
   	project = ReplacePart[Import[fileName], 1 -> ("SelfReference" -> fileName)]];
  
  	(*check head*)
  	If[! (Head[project] === TrackingProject),
   		Message[eLoadTrackingProject::invpro];
   		Abort[]
   	];
  
  	(*validate project*)
  	If[! eValidateProject[project],
   		Message[eLoadTrackingProject::nocons]
   	];
  	project
]

Clear[eValidateProject];
eValidateProject::nodir = "The directory '`1`' given in the project file does not exist.";
eValidateProject::dirch = "The content of the `1` directory at `2` has changed. To fix this issue call eUpdateProject[project,`1`] to update the given directory \
structure or eUpdateProject[project] to update the hole project.";
eValidateProject::ppch = "The project directory may have changed. To fix this issue call eUpdateProject[project,`1`] to update the given directory structure \
or eUpdateProject[project] to update the hole project.";
eValidateProject::noTrackf= "The specified track file `1` does not exist.";
eValidateProject[project_TrackingProject] := Module[
	{data, directories,file}, 
	
	data = Level[project, 1];
  	(*check directories*)
  	directories = DeleteCases[First /@ ({{"ProjectPath"}, "Images", "RegisteredImages", "MetaFiles", "LabelImages"} /. data), ""];
  	If[! Apply[And, DirectoryQ /@ directories],
   		Message[eValidateProject::nodir, #[[1]]] & /@ Select[Transpose[{DirectoryQ /@ directories, directories}], #[[2]] == False &];
   		If[SameQ[DirectoryName["SelfReference" /. data], "ProjectPath" /. data],
    		Return[False],
    		Message[eValidateProject::ppch, "\"ProjectPath\""];
    		Return[False]
    	];
   	];
  	
  	(*check for trackfile*)
  	file="TrackFile"/.data;
  	If[file != "" && !FileExistsQ[file],
  		Message[eValidateProject::notrackf,file];
  		Return[False]
  	];
  	
  	(*check files*)
  	Return[eValidateProject[project,{"Images","RegisteredImages","Flows","LabelImages","MetaFiles"}]]
  	
]
eValidateProject[project_TrackingProject,objectives:{___String}]:=Block[
	{data,changedDirectories,path,filePaths},
	data = Level[project,1];
	changedDirectories = 
  	Map[
  		({path, filePaths} = (# /. data); # -> !SameQ[(FileNameTake /@ FileNames[# /. $searchPattern, {path},IgnoreCase->True]), filePaths]) &,
    	objectives
  	];
  	If[Apply[Or, Last /@ changedDirectories],
  		Message[eValidateProject::dirch, #, First[# /. data]] & /@ (First /@
       	Select[changedDirectories, #[[2]] == True &]);
   		Return[False]
   	];
  	Return[True]
]
eValidateProject[project_TrackingProject,objective_String]:=eValidateProject[project,{objective}]

Clear[eUpdateProject]
eUpdateProject::ufail = "Update of project failed.";
eUpdateProject::uobj = "The given objective `1` is unknown.";
eUpdateProject::nomem = "Illegal parameter value for '`1`'. Value has to be one of the following `2`.";
eUpdateProject[project_TrackingProject] := Module[
	{selfReferencePath, projectPath, newProject,objectives},
  	{selfReferencePath, projectPath} = {DirectoryName["SelfReference" /. #], "ProjectPath" /. #} &@Level[project, 1];
  	If[DirectoryQ[projectPath],
   		newProject = eCreateTrackingProject[projectPath],
   		If[DirectoryQ[selfReferencePath],
    		eCreateTrackingProject[selfReferencePath],
    		Message[eUpdateProject::ufail];
    		Abort[]
    	]
   	];
   	objectives={"SegmentationPipeline","RegisterPipeline","FlowPipeline"};
   	Fold[setPipeline[#1,Sequence@@#2]&,newProject,Transpose[{objectives,objectives/.Level[project,1]}]]
]
eUpdateProject[project_TrackingProject, "TrackFile"] := Module[
	{data,fileName},
	data=Level[project,1];
	fileName=FileNameJoin[{("ProjectPath"/.data),"/tracks.m"}];
	If[FileExistsQ[fileName],
		ReplacePart[project, Position[data, "TrackFile" -> _] -> Rule["TrackFile",fileName]]
	]
]
eUpdateProject[project_TrackingProject, objective_String] := Module[
	{data, path, files},
  	MemberCheck[eUpdateProject,objective,objective,{"Images","RegisteredImages","Flows","LabelImages","MetaFiles"}];
  	data = Level[project, 1];
  	path = First[objective /. data];
  	files = getFileNames[path, objective];
  	If[Length@files > 0,
   		eLogString["eUpdateProject : Project got updated. Added " <> ToString[Length@files] <> " additional files."]
   	];
  	ReplacePart[project, Position[data, objective -> _] -> Rule[objective,{path, files}]]
]

Clear[eUpdateMetaData];
eUpdateMetaData[project_TrackingProject]:=Module[
	{tracks,metaFiles,IDtoTrackMap,meta,newMeta,pos,trackID},
	If[!eValidateProject[project,"MetaFiles"],
		Abort[]
	];
	tracks=Import["TrackFile"/.Level[project,1]];
	metaFiles=eGetFileNames[project,"MetaFiles"];
	IDtoTrackMap=Dispatch@Flatten@Table[#->i&/@tracks[[i]],{i,Length@tracks}];
	ParallelTable[
		meta=Import[metaFiles[[i]]];
		newMeta=
		Map[(
			pos=Position[#, "TrackID" -> _];
			trackID=("ObjectID"/.#)/.IDtoTrackMap;
			If[pos=={},
				Append[#,Rule["TrackID",trackID]],
				ReplacePart[#, First@pos -> Rule["TrackID",trackID]]
			]
			)&
			,meta
		];
		Export[metaFiles[[i]],newMeta]
		,{i,Length@metaFiles},DistributedContexts->Automatic
	];
]

Clear[eRegisterImages];
eRegisterImages::nomem= nomem;
eRegisterImages::nobool = nobool;
eRegisterImages::noimg = "No images present or inconsistency of project with hardisk. Update project first!";
eRegisterImages::nopipe = "RegisterPipeline in project is not set.";
eRegisterImages::override = override;
Options[eRegisterImages] = {ExportFileFormat -> "PNG", AbortWhenOverride -> False};
eRegisterImages[project_TrackingProject, opts : OptionsPattern[]] := Block[
	{format,ask, data, regPath, registerPipeline, imagePath, imageNames, images, digits, image},
	
	(*option validation*)
	{format,ask} = {ExportFileFormat,AbortWhenOverride} /. {opts} /. Options[eRegisterImages];
  	MemberCheck[eRegisterImages,"ExportFileFormat",OptionValue[ExportFileFormat],{"PNG","JPG","TIFF"}];
  	IsBoolean[eRegisterImages,"AbortWhenOverride",ask];
  	
  	(*implementation*)
  	Switch[format,
  		"PNG",
  		format=".png",
  		"JPG",
  		format=".jpg",
  		"TIFF",
  		format=".tif"
  	];
  	data = Level[project, 1];
  	regPath = First["RegisteredImages" /. data];
  	registerPipeline = "RegisterPipeline" /. data;
  	If[registerPipeline=="",
  		Message[eRegisterImages::nopipe];
  		Abort[]
  	];
  	If[ask && Length[getFileNames[regPath,"RegisteredImages"]] > 0,(*if 'reg' directory is not empty*)
   		Message[eRegisterImages::override];
   		Abort[]
   	];
  	{imagePath, imageNames} = "Images" /. Level[project, 1];
  	If[imageNames=={}||!eValidateProject[project,"Images"],(*if empty or not valid*)
  		Message[eRegisterImages::noimg];
  		Abort[]
  	];
  	images = FileNameJoin[{imagePath, #}] & /@ imageNames;
  	digits = Length[IntegerDigits[Length[images]]];
  	Table[
    	Export[eComposeFileName[regPath <> "/reg", format, i, digits], registerPipeline[Import[images[[i]]]]],
   		{i, 1, Length@images}
   	];
  	eLogString["eRegisterImages : Registering of images [successful]."];
  	eUpdateProject[project, "RegisteredImages"]
]

Clear[eSegmentImages];
Options[eSegmentImages] = {ExportFileFormat -> "PNG", AbortWhenOverride -> False};
eSegmentImages::noimg = "No images present or inconsistency of project with hardisk. Update project first!";
eSegmentImages::nopipe = "SegmentationPipeline in project is not set.";
eSegmentImages::override = override;
eSegmentImages[project_TrackingProject, opts : OptionsPattern[]] := Module[
	{format, ask, data, segPath, segmentationPipeline, imagePath, imageNames, objective, images, digits},
	
	(*option validation*)
	{format,ask} = {ExportFileFormat,AbortWhenOverride} /. {opts} /. Options[eRegisterImages];
  	MemberCheck[eRegisterImages,"ExportFileFormat",OptionValue[ExportFileFormat],{"PNG","JPG","TIFF"}];
  	IsBoolean[eRegisterImages,"AbortWhenOverride",ask];
  	
  	(*implementation*)
  	Switch[format,
  		"PNG",
  		format=".png",
  		"JPG",
  		format=".jpg",
  		"TIFF",
  		format=".tif"
  	];
  	data = Level[project, 1];
  	segPath = First["LabelImages" /. data];
  	If[ask && Length[getFileNames[segPath,"LabelImages"]] > 0,(*if $labelDirectory directory is not empty*)
  		Message[eSegmentImages::override];
   		Abort[]
   	];
  	segmentationPipeline = "SegmentationPipeline" /. data;
  	If[segmentationPipeline=="",
  		Message[eSegmentImages::nopipe];
  		Abort[]
  	];
  	If[First["RegisteredImages" /. data] == "",
  		objective="Images";
   		{imagePath, imageNames} = "Images" /. Level[project, 1],
   		objective="RegisteredImages";
   		{imagePath, imageNames} = "RegisteredImages" /. Level[project, 1]
   	];
  	eLogString["eSegmentImages : Using images from " <> imagePath <> " for segmentation."];
  	If[imageNames=={}||!eValidateProject[project,objective],(*if empty or not valid*)
  		Message[eSegmentImages::noimg];
  		Abort[]
  	];
  	images = FileNameJoin[{imagePath, #}] & /@ imageNames;
  	digits = Length[IntegerDigits[Length[images]]];
  	Table[
   		Export[eComposeFileName[segPath <> "/" <> $segmentationDirectory, format, i, digits],segmentationPipeline[Import[images[[i]]]]]
   		,{i,Length@images}
  	];
  	eLogString["eSegmentImages : Segmentation of images [successful]."];
  	eUpdateProject[project, "LabelImages"]
]

Clear[eComputeFlows];
Options[eComputeFlows]={FlowDirection->"Backward"}
eComputeFlows::nomem = nomem;
eComputeFlows::noimg = "No images present or inconsistency of project with hardisk. Update project first!"
eComputeFlows::nopipe = "FlowPipeline in project is not set.";
eComputeFlows[project_TrackingProject,opts:OptionsPattern[]]:=Module[
	{direction,data,flowPath,flowPipeline,objective,imagePath,imageNames, images, digits},
	direction=OptionValue[FlowDirection];
	MemberCheck[eComputeFlows,"FlowDirection",direction,{"Forward","Backward"}];
	data = Level[project,1];
	flowPath = First["Flows" /. data];
	flowPipeline = "FlowPipeline" /. data;
	If[flowPipeline=="",
  		Message[eComputeFlows::nopipe];
  		Abort[]
  	];
  	If[First["RegisteredImages" /. data] == "",
  		objective="Images";
   		{imagePath, imageNames} = "Images" /. Level[project, 1],
   		objective="RegisteredImages";
   		{imagePath, imageNames} = "RegisteredImages" /. Level[project, 1]
   	];
  	eLogString["eComputeFlows : Using images from " <> objective <> " for flow computation."];
  	If[imageNames=={}||!eValidateProject[project,objective],(*if empty or not valid*)
  		Message[eComputeFlows::noimg];
  		Abort[]
  	];
  	images = eGetFileNames[project,objective];
  	digits = Length[IntegerDigits[Length[images]]];
  	MapIndexed[
   		eBinaryExport[eComposeFileName[flowPath <> "/reg", ".dat", First@#2, digits], flowPipeline[Import[First@#1],Import[Last@#1]]] &,
   	Switch[direction,
   		"Forward", Transpose[{Drop[images,-1],Drop[images,1]}],
   		"Backward", Transpose[{Drop[images,1],Drop[images,-1]}]
   	]];
   	eLogString["eComputeFlows : Flow computation [successful]."];
  	eUpdateProject[project, "Flows"]
]

Clear[eCreateMetaData];
Options[eCreateMetaData] = {OutlineControlPoints -> 50,SizeConstraint -> None, ConstraintType->"Standard", MaximalSize->500};
eCreateMetaData::noint = noint;
eCreateMetaData::noseg = "No label files present or inconsistency of project with hardisk. Segment images first or update project!"
eCreateMetaData[project_TrackingProject, opts : OptionsPattern[]] := Module[
	{ctrPoints, pixels, opts1, data, imagePath, imageNames, labelImages, metaPath, digits, label, measures, meta, offset = 0},
	(*option validation*)
	{ctrPoints,pixels}=OptionValue[{OutlineControlPoints,SizeConstraint}];
	opts1 = Sequence@@FilterRules[{opts},First/@Options[eDeleteObjects]];
	IntegerCheck[eCreateMetaData,"OutlineControlPoints",ctrPoints];
	
	(*implementation*)
  	data = Level[project, 1];
  	{imagePath, imageNames} = "LabelImages" /. data;
  	If[imageNames=={}||!eValidateProject[project,"LabelImages"],(*if empty or not valid*)
  		Message[eCreateMetaData::noseg];
  		Abort[]
  	];
  	labelImages = FileNameJoin[{imagePath, #}] & /@ imageNames;
  	metaPath = First["MetaFiles" /. data];
  	digits = Length[IntegerDigits[Length[labelImages]]];
  	eLogString["eCreateMetaData : Attention pixel size constraint is set to " <> ToString[pixels] <> ". Objects smaller or greater than that will not be tracked ({minimalSize,maximalSize})."];
  	Table[
   		label = eDeleteObjects[MorphologicalComponents[Import[labelImages[[i]]],0], opts1];
   		measures = Last /@ ComponentMeasurements[label, {"Label", "Centroid", "BoundingBox", "Count", "Elongation", "Length", "Width", "Mask"}];
   		meta =
    	ParallelTable[{
    		"ObjectID" -> o+offset, 
      		"ObjectCenter" -> measures[[o, 2]], 
      		"ObjectBoundingBox" -> Round[measures[[o, 3]]],
      		"ObjectArea" -> measures[[o, 4]], 
      		"ObjectCompactness" -> measures[[o, 5]], 
      		"ObjectMajorDiameter" -> measures[[o, 6]], 
      		"ObjectMinorDiameter" -> measures[[o, 7]], 
      		"Outline" -> (N[Round[#*100]/100]&/@eCreateOutline[Image[Normal[measures[[o, 8]]]], ctrPoints,"ImageCS"])}, 
      		{o, Length@measures},DistributedContexts->Automatic
		];
   		Export[eComposeFileName[metaPath <> "/meta", ".m", i, digits], meta];
   		offset += Length[measures]
   		,{i, Length@labelImages}
   	];
	eLogString["eCreateMetaData : Export of meta data [successful]."];
  	eUpdateProject[project, "MetaFiles"]
]

Clear[setPipeline];
setPipeline[project_TrackingProject, objective_String, pipeline_Function]:=Module[
	{data},
  	data = Level[project, 1];
  	ReplacePart[project, Position[data, objective -> _] -> objective -> pipeline]
]
setPipeline[project_TrackingProject, objective_String, ""]:=project

Clear[eSetSegmentationPipeline];
eSetSegmentationPipeline[project_TrackingProject, pipeline_] := setPipeline[project,"SegmentationPipeline",pipeline]

Clear[eSetRegisterPipeline];
eSetRegisterPipeline[project_TrackingProject, pipeline_] := setPipeline[project,"RegisterPipeline",pipeline]

Clear[eSetFlowPipeline];
eSetFlowPipeline[project_TrackingProject, pipeline_] := setPipeline[project,"FlowPipeline",pipeline]

(*utilities for fluid tracking*)
Clear[ePackLabel64];
ePackLabel64[cloneID_Integer /; cloneID < 4294967295, cellLabel_Integer /; cellLabel < 4294967295] := 
	BitShiftLeft[cloneID, 32] + cellLabel;

Clear[eUnpackLabel64];
eUnpackLabel64[label_Integer] := {BitShiftRight[BitAnd[label, 18446744069414584320], 30], BitAnd[label, 4294967295]}

Clear[ePackLabel32]
ePackLabel32[cloneID_Integer /; cloneID < 65536, cellLabel_Integer /; cellLabel < 65536] := 
	BitShiftLeft[cloneID, 16] + cellLabel;

Clear[eUnpackLabel32]
eUnpackLabel32[label_Integer] := {BitShiftRight[BitAnd[label, 4294901760], 16], BitAnd[label, 65535]}

Clear[eDeleteObjects];
eDeleteObjects::nonum = nonum;
eDeleteObjects::nomem = nomem;
Options[eDeleteObjects] = {SizeConstraint -> None, ConstraintType->"Standard", MaximalSize->500};
eDeleteObjects::nonum = nonum;
eDeleteObjects::invsc="Invalid value for option SizeConstraint. SizeConstraint has to be of the form {minimalSize,maximalSize}, with the following pattern None | {None, None} | {None, _Integer} | {_Integer, None} | {_Integer, _Integer}."
eDeleteObjects[labelMatrix : {{__Integer} ..}, opts : OptionsPattern[]] := Block[
	{size,minSize,maxSize, type, max, xdim, ydim, minSizef,maxSizef}, 
	{size,type,max} = OptionValue[{SizeConstraint,ConstraintType,MaximalSize}];
	MemberCheck[eDeleteObjects,"ConstraintType", type, {"Standard", "Mercator"}];
	NumberCheck[eDeleteObjects, "MaximalSize",max];
	If[MatchQ[size, None | {None, None} | {None, _Integer} | {_Integer, None} | {_Integer, _Integer}],
		Switch[size,
			None|{None,None},
			minSize=0;
			maxSize=Infinity,
			{None,_Integer},
			minSize=0;
			maxSize=Last@size,
			{_Integer,None},
			minSize=First@size;
			maxSize=Infinity,
			{_Integer,_Integer},
			minSize=First@size;
			maxSize=Last@size
		],
		Message[eDeleteObjects::invsc];
		Abort[]
	];
				
  	{xdim, ydim} = Dimensions@labelMatrix;
  	If[type === "Standard",
   		minSizef[{x_, y_}] = minSize;
   		maxSizef[{x_, y_}] = maxSize,
   		minSizef[{x_, y_}] := Min[max, minSize*Sec[Rescale[y, {0, ydim}, {-Pi/2, Pi/2}]]^2];
   		maxSizef[{x_, y_}] := maxSize*Sec[Rescale[y, {0, ydim}, {-Pi/2, Pi/2}]]^2
   	];
   	If[MatchQ[size,None|{None,None}],
   		labelMatrix,
  		labelMatrix*Normal@Total[Last[#][[2]] & /@ Cases[ComponentMeasurements[labelMatrix, {"Count", "Mask", "Centroid"}], x_ /; (First@Last@x > minSizef[Last@Last[x]]&& First@Last@x < maxSizef[Last@Last[x]])]]
   	]
]

Clear[eRelabel];
eRelabel::nomem = nomem;
Options[eRelabel] = Union@Flatten@{LabelSize -> "32Bit",Options[eDeleteObjects]};
eRelabel[labelMatrix : {{__Integer} .. }, opts : OptionsPattern[]] := Block[
  	{bitDepth,size,opts1,pack,select},
  	{bitDepth,size} = OptionValue[{LabelSize,SizeConstraint}];
  	opts1 = Sequence@@FilterRules[{opts},First/@Options[eDeleteObjects]];
  	MemberCheck[eRelabel,"LabelSize",bitDepth,{"32Bit","64Bit"}];
  	If[bitDepth == "32Bit",
   		pack = ePackLabel32,
   		pack = ePackLabel64
   	];
   	If[MatchQ[size,None|{None,None}],
  		Normal@Total@(MapIndexed[Last@#1*pack[First@#2, 1] &, ComponentMeasurements[labelMatrix, "Mask"]]),
  		Normal@Total@(MapIndexed[Last@#1*pack[First@#2, 1] &, ComponentMeasurements[eDeleteObjects[labelMatrix,opts1], "Mask"]])
   	]
]

Clear[eUnpackLabelMatrix];
eUnpackLabelMatrix::nomem = nomem;
Options[eUnpackLabelMatrix] = {LabelSize -> "32Bit",Target->"CloneIDs"};
eUnpackLabelMatrix[labelMatrix : {{__Integer} ..}, opts : OptionsPattern[]] := Block[
	{bitDepth,target,unpack, labels, rules},
	{bitDepth,target} = OptionValue[{LabelSize,Target}];
	MemberCheck[eUnpackLabelMatrix,"LabelSize",bitDepth,{"32Bit","64Bit"}];
	MemberCheck[eUnpackLabelMatrix,"Target",target,{"CloneIDs","CellIDs"}];
  	If[bitDepth == "32Bit",
   		unpack = eUnpackLabel32,
   		unpack = eUnpackLabel64
   	];
  	labels = DeleteCases[Union@Flatten[labelMatrix], 0];
  	If[target=="CloneIDs",
  		rules = Dispatch[#->First[unpack[#]]&/@labels],
  		rules = Dispatch[#->Last[unpack[#]]&/@labels]
  	];
  	Replace[labelMatrix, rules, {2}]
]

Clear[eExtractTracksFromLabels]
eExtractTracksFromLabels[labels:{__String}]:=Block[
	{ids,tracks},
	DistributeDefinitions[labels];
	ids=ParallelTable[First@# -> {Last@#, i} & /@ ComponentMeasurements[IntegerPart@First@Import[labels[[i]]],"Centroid"],{i,1,Length@labels}];
	tracks=First@First@#->(Last/@#)&/@GatherBy[Flatten@ids,First@#&]
]

End[] (* End Private Context *)

EndPackage[]