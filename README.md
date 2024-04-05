# AgResilienceBrazil
This repository contains simple replication code for the analysis found in "Empirical Modeling of Agricultural Climate Risk" (PNAS 2024) by J. Burney, C. McIntosh, B. Lopez-Videla, K. Samphantharak, A. Gori Maia.


## Instructions
+ Data have been deposited in this repository: [https://doi.org/10.7910/DVN/JDFBLF](https://doi.org/10.7910/DVN/JDFBLF). Note: due to data sharing restrictions, we are unable to deposit/provide the underlying financial institution data.
+ Download data and code into a common directory (e.g., "Replication/").
+ The main replication file is "Simple_Replication.R" and assumes you are running from within the common directory (e.g., "Replication/").
+ The main file calls several others: "Ag_Analysis.R", "Fancier_Fig2_SimpleRep.R", "Projections_SimpleRep.R", and "Projections_Figs_SimpleRep.R"
+ Required packages: tidyverse,fixest,modelsummary,magick,pander,glue,margins,ggeffects,ggpubr,RColorBrewer,sp,rgdal,raster,rworldmap,sf,zoo

## Questions?
Contact the authors with any questions; we are happy to walk through the analysis, even if we are unable to share disaggregated FI data.
