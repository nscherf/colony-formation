**Registration**

- code in ./DigitalImagesLibrary folder
- use Makefile (written for MacOS X version)
- works for g++ 4.2
- currently requires Intel Threading Building Blocks and Intel Compiler framework to work (free for Linux)
- precompiled binaries available for Mac OS X and Linux
- required libraries in lib folder

**Segmentation**

- currently done interactively with Ilastik (open source)
- http://www.ilastik.org/
- save binary masks of segmented objects in separate folder (image_directory must be supplied to tracking script later on)
- *IMPORTANT: final label images must be converted to 8Bit integer images !*

**Tracking**
- folder TrackingMathematica
- Eidomatica package must be copied to Applications folder for Mathematica (in ~/.Mathematica/Applications under Linux or ~/Library/Mathematica/Applications for Mac OS X)
- Python script ./tracking performs colony tracking. See ./tracking -h for help
- see tracking-script.m for Mathematica code

- *IMPORTANT: first line of tracking-script.m has to be adapted to local installation path of Mathematica*
(default is /Applications/Mathematica.app/Contents/MacOS/MathKernel -script)


**Visualization**
- code still experimental 
- basic idea can be found in notebook **vizualization.nb**


