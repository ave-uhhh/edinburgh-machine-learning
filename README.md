Alternative analysis of Edinburgh census data via self organizing maps
=======================================================================

File/Folder Description
------
som.R	Main script for data preprocessing, SOM training, clustering, and mapping
SIMD_Edinburgh_2020csv.csv	SIMD census data for Edinburgh (2020)
sc_dz_11.shp	Shapefile of Edinburgh data zones
coolBlueHotRed.R	Custom color palette for SOM visualizations
som_heatmaps6.pdf	PDF output of SOM heatmaps for all variables
simd_map_clustered.csv	Final spatial dataset with cluster assignments

Methodology Highlights
------
* Merges SIMD data with spatial polygons for mapping
* Uses the kohonen package to train a 15×15 hexagonal SOM grid
* Visualizes training progress, node quality, and variable heatmaps
* Evaluates cluster stability using WCSS and visualizes cluster boundaries
* Assigns clusters to spatial units
* Generates ggplot maps of clustered data zones across Edinburgh

Requirements
------
install.packages(c("kohonen", "ggplot2", "sf", "gridExtra", "grid", "broom", "dplyr"))
