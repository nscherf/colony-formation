(* Mathematica Package *)

BeginPackage["Eidomatica`Visualization`"]
Needs["Eidomatica`"]
Needs["Eidomatica`Internal`"]
Needs["Eidomatica`Utilities`"]

(* Exported symbols added here with SymbolName::usage *)
ePlotContours::usage="ePlotContours[image, contours] plots contours into the image. PlotContours[contours] plots simply the contours."
eVectorFieldPlot::usage = "eVectorFieldPlot[field] visualizes the given vector field in the specified manner.
eVectorFieldPlot[field,image] plot the given vector field as arrows onto the given image."
	Chunks::usage = "Chunks is an option for eVectorFieldPlot[field] specifying the number of chunks the color wheel is divided to. This option affects the plot only, if the option 'VisualizationsStyle' is set to \"Angle\"."
	MinimalRange::usage = "MinimalRange is an option for eVectorFieldPlot[] specifying the minimal size of vectors to show in the plot, vectors smaller than this value are omitted."
	ShowColorWheel::usage = "ShowColorWheel is an option for eVectorFieldPlot[field] specifying if the color wheel corresponding to the resulting plot is shown too."
	VisualizationStyle::usage = "VisualizationStyle is an option for eVectorFieldPlot[field] specifying the style for the visualization. Possible values are \"Angle\", \"Arrows\", \"Plain\" and \"Surface\"."

Begin["`Private`"] (* Begin Private Context *) 

(*Plot contours*)
Clear[ePlotContours];
ePlotContours[image_Image,contours:{{{_?NumericQ,_?NumericQ}..}..},opts:OptionsPattern[Plot]]:=Module[
{closed},
	closed=Join[#,{First@#}]&/@contours;
	Show[{
		image,
		ListLinePlot[closed,opts]
	}]
]
ePlotContours[image_Image,contour:{{_?NumericQ,_?NumericQ}..},opts:OptionsPattern[Plot]]:=ePlotContours[image,{contour},opts]
ePlotContours[contours:{{{_?NumericQ,_?NumericQ}..}..},opts:OptionsPattern[Plot]]:=Module[
	{closed},
	closed=Join[#,{First@#}]&/@contours;
	ListLinePlot[closed,opts]
]
ePlotContours[contour:{{_?NumericQ,_?NumericQ}..},opts:OptionsPattern[Plot]]:=ePlotContours[{contour},opts]

Clear[eVectorFieldPlot];
eVectorFieldPlot::noint = noint;
eVectorFieldPlot::nonum = nonum;
eVectorFieldPlot::nobool = nobool;
eVectorFieldPlot::nopos = nopos;
eVectorFieldPlot::nomem = nomem;
Options[eVectorFieldPlot] = {ShowColorWheel -> False, VisualizationStyle -> "Plain", MinimalRange -> .01, Chunks -> 12, InterpolationMethod -> "Linear"};
eVectorFieldPlot[field : {{{_?NumericQ, _?NumericQ} ..} ..}, opts : OptionsPattern[]] := Module[
	{modName=eVectorFieldPlot, showWheel, style, interpolationMethod, range, normalizedField, polarField, visualization, color, chunks, inside, ydim},
  
  	(*options validation*)
  	{chunks, range, showWheel, style,interpolationMethod} = OptionValue[{Chunks, MinimalRange, ShowColorWheel, VisualizationStyle,InterpolationMethod}];
  		(*chunks*)
  		IntegerCheck[modName,"Chunks",chunks];
  		(*range*)
  		NumberCheck[modName,"MinimalRange",range];
  		IsPositive[modName,"MinimalRange",range];
  		(*color wheel*)
  		IsBoolean[modName,"ShowColorWheel",showWheel];
  		(*style*)
  		MemberCheck[modName,"VisualizationStyle",style,{"Angle","Arrows","Plain","Surface"}];
  		(*interpolation method*)
  		MemberCheck[modName,"InterpolationMethod",interpolationMethod,{"Linear","Cubic"}];
  	
  	(*implementation*)
  
	color[length_, angle_] := 
		If[length <= range, 
			{0., 0., 0.}, 
			eRYBtoRGB[eRYBColor[inside[angle]], InterpolationMethod -> interpolationMethod]
		];
  	visualization = 
  	Switch[style,
    	"Angle",
		polarField = Map[{First@#, Last@#/Pi} &, eToPolarCS[Chop[field]], {2}];(*transform to polar coordinates and map angle to range -1..1*)
    	inside[angle_] := First@Nearest[Range[-1, 1, 2/chunks], angle];
    	Image[ParallelMap[color[First@#, Last@#] &, polarField, {2}, DistributedContexts->Automatic], ColorSpace -> "RGB"],
    	"Arrows",
    		ydim = First@Dimensions[field];
    		Graphics[{Arrowheads[Small], 
    			MapIndexed[Arrow[{{Last@#2, ydim - First@#2} + {-.5, .5}, {Last@#2, ydim - First@#2} + {First@#1, Last@#1} + {-.5, .5}}] &, 
       			field, {2}]}
       		],
    	"Plain",
    		If[$librariesFound,
    			Image[llVectorFieldPlot[field],ColorSpace->"HSB"],
    			normalizedField = field/Max[Max[Norm /@ Flatten[field, {1, 2}]],1];
    			Image[Map[{Mod[-.5Last@#/Pi,1], First@#, .9} &, eToPolarCS[Chop[normalizedField]], {2}], ColorSpace -> "HSB"]
    		],
    	"Surface",
    		color = Function[{x, y, z}, Hue[Sequence @@ ({Last@#, First@#} &@(polarField[[Max[1, Round@x], Max[1, Round@y]]])), .8]];
    		ListPlot3D[Map[Norm, Transpose@field, {2}], PlotRange -> All, ColorFunction -> color, ColorFunctionScaling -> False, 
     			Mesh -> None, AspectRatio -> Divide @@ Reverse@Take[Dimensions[field], 2]
     		]
	];
  	If[showWheel,
   		If[SameQ[style, "Angle"],
    		{visualization, Image[DiskMatrix[99, 201]*Table[color[First@#, Last@#/Pi] &@eToPolarCS[{x, y}], {y, 100, -100, -1}, {x, 100, -100, 1}], ColorSpace -> "RGB", ImageSize -> 200]},
    		{visualization, Image[DiskMatrix[99, 201]*Table[{((Last@#)/Pi + 1)/2, First@#/99, .9} &@eToPolarCS[{x, y}], {y, 100, -100, -1}, {x, 100, -100, -1}], ColorSpace -> "HSB", ImageSize -> 200]}
    	],
   		visualization
	]
]
eVectorFieldPlot[field_VectorField, opts : OptionsPattern[]] := eVectorFieldPlot[eVectorFieldData[eToCartesianCS[field]], opts]
eVectorFieldPlot[field_VectorField, image_Image, opts : OptionsPattern[]] := Show[{Graphics[{Opacity[.6], Raster[Reverse@ImageData@image]}], eVectorFieldPlot[field, VisualizationStyle -> "Arrows"]}]
End[] (* End Private Context *)

EndPackage[]