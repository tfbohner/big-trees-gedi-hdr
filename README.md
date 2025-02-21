This is the repository to accompany:
Future drought and mortality risk for the world’s forests and tallest trees


Teresa Bohner, Laura Duncanson, Efthymios Nikolopoulos, Amy E. Frazier, Diogo S.A. Araujo, Cameryn Brock,  César Hinojo-Hinojo, Joana M. Krieger, Brian Maitner, Cory Merow, Gabriel M. Moulatlet, Patrick R. Roehrdanz, Lei Song and Brian J. Enquist


1 drought metrics.R calculates the drought metrics from the raw drought time series layers (Nikolopoulous and Araujo). To re-run any of the subsequent analyses, scripts to calculate these metrics will not need to be re-run as the derived metrics are saved as intermediate data files (shared).

2 gedi fine res.R Produces numerous intermediate data files for subsequent analysis. It matches the grids for the drought metrics, GEDI, biomes, and biogeographic realms to produce tables with the matched values as well as rasters at the correct grids for plotting and downstream analyses. This also should not need to be re-run. 

3 summary tables and z scores.R Produces summaries for the manuscript. Produces summary statistics for each biome and realm for historic drought metrics as well as future scenarios. This script also calculates the z-scores used in the manuscript as well as the z-score figure for the manuscript. I’ll try and split this up and clean this up as I can. 

All of the scripts to generate figures (scripts that do not produce any intermediate data products or summaries) are in the figure generation folder. 
