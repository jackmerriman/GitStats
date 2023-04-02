BY JACK MERRIMAN
Contained here is the replication code from the authors, it all ran smoothly without any errors that couldn't be resolved by backdating my R version.

the code I wrote for the twist can be found in the file jackscript.R, the table and figure produced are in the twist folder. My keynote file and pdf of the slides are here too.




BY THE AUTHORS OF THE PAPER

Replication files for
   Kyosuke Kikuta "Do Politically Irrelevant Events Cause Conflict? The Cross-continental Effects of European Professional Football on Protests in Africa"

Requirements
   R 4.0.2 (https://cran.r-project.org/bin/windows/base/old/)
   RStudio 2021.09.0+351 (https://rstudio.com/)
   checkpoint 1.0.0 (https://cran.r-project.org/web/packages/checkpoint/index.html)
   
Code written by
   Kyosuke Kikuta (kyosuke.kkt@gmail.com). Institute of Developing Economies, Japan External Trade Organization

Last update
   22-June-2022
      
Notes
   - This file provides the description of the replication materials and the instruction for the replication.
   - All necessary R packages are included as replication materials and are managed by the checkpoint package. There is no need to install the R packages except for the checkpoint package.
   - The R scripts use relative paths, so there is no need to specify the working directories.
   - Depending on environment, the replication can take 10-60 min.
   - The replication uses 5-6Gb memory. Please make sure that your computer has a sufficient size of memories (16Gb should be enough).
   - Windows PC is recommended for this replication.
   - Please ignore warning messages.

Instruction 
   1. Open "rmaster.R" with R Studio.
   2. Make sure that the version of your R is 4.0.2.
   3. Run all of the lines of "rmaster.R".
   4. The results are saved in "results" folder.

Folders and files
   data: This folder contains data files.
   rcodes: This folder contains the R scripts that are called by "rmaster.R."
   results: This folder contains the results of the main and supplementary analyses. The subfolder "eps" contains EPS files for publication.
   rpkgs: This folder contains the R packages that are used in this replication (they are automatically called by the checkpoint package).
   readme.txt: Description and instruction of replication.
   rmaster.R: R file for replication.
   
	