install.packages("kohonen")
library(kohonen) # for SOM
library(ggplot2) # to make pretty maps
library(sf) # to manage spatial data
library(gridExtra) # for organising maps on the display 
library(grid)
library(broom) # in case you use tidy instead of fortify
library(dplyr) 


#---------------------
# Data Preprocessing
#--------------------

setwd("/Users/avacorry-roberts1/Documents/Edinburgh/VA/assessment2/Assessment Data/SIMD_2020")

# Read in the CSV file for the SIMD data
simd_data <- read.csv(file="SIMD_Edinburgh_2020csv.csv", header=TRUE, na.strings = c("*", "n/a", "N/A"))

# Examine data class and structure
class(simd_data)
str(simd_data)
names(simd_data)
nrow(simd_data)

# Clean data
## Remove the non SIMD indicator data and rates
simd_data <- simd_data %>%
  select(-c(SIMD2020v2_Rank, SIMD_2020v2_Percentile, SIMD2020v2_Vigintile, 
            SIMD2020v2_Decile, SIMD2020v2_Quintile, SIMD2020v2_Income_Domain_Rank, 
            SIMD2020_Employment_Domain_Rank, SIMD2020_Health_Domain_Rank, 
            SIMD2020_Education_Domain_Rank, SIMD2020_Access_Domain_Rank, 
            SIMD2020_Crime_Domain_Rank, SIMD2020_Housing_Domain_Rank, income_rate,
            employment_rate,crime_rate, overcrowded_rate,nocentralheating_rate))
## Define area label columns to exclude from numeric conversion
exclude_cols <- c("Data_Zone", "Intermediate_Zone", "Council_area")
## Identify character columns containing '%'
problematic_cols <- sapply(simd_data, function(col) {
  is.character(col) && any(grepl("[%]", col))
})
## Remove % and convert those columns to numeric
simd_data[problematic_cols] <- lapply(simd_data[problematic_cols], function(col) {
  as.numeric(gsub("[%]", "", col))  
})
## Convert all other non-excluded columns to numeric
numeric_cols <- setdiff(names(simd_data), exclude_cols)
simd_data[numeric_cols] <- lapply(simd_data[numeric_cols], as.numeric)

#-------------------------
# Spatial Data Processing
#-------------------------

# Read in the data zone shapefile for Edinburgh, already matched up by row with the census data by DataZone
simd_map <- st_read(dsn=".", layer="sc_dz_11")

# Convert the spatial object into latitude and longitude for easier use with ggmap
simd_map <- st_transform(simd_map, crs = 4326)

# Convert spatial polygon to dataframe including columns of spatial information
simd_fort <- st_sf(simd_map)
simd_fort <- st_cast(simd_fort, "POLYGON")

# Plot the spatial polygons data frame
#plot(simd_map)

# Merge the new dataframe with the SIMD 2020 data using their shared column (data zone)
simd_merge <- merge(simd_fort, simd_data, by.x = "DataZone", by.y = "Data_Zone")

#--------------
# Data Training
#--------------
colnames(simd_data)
# Choose the variables with which to train the SOM
data_train <- select(simd_data, Total_population, income_count, ALCOHOL, DRUG, SMR, DEPRESS, LBWT, not_participating, 
                     University, overcrowded_count, nocentralheating_count, drive_petrol, drive_GP, 
                     drive_post, drive_primary, drive_retail, drive_secondary, PT_GP, PT_post, 
                     PT_retail, broadband) 

# Standardise the data with z-scores and convert to a matrix
data_train_matrix <- as.matrix(scale(data_train))

# Keep the column names of data_train as names in the new matrix 
names(data_train_matrix) <- names(data_train)

# Define the size and topology of the SOM grid
N <- nrow(data_train_matrix)*ncol(data_train_matrix)
N <- 5*sqrt(N)
print(sqrt(N))
# Round 15.63125 down
som_grid <- somgrid(xdim = 15, ydim=15, topo="hexagonal") 

# Train the SOM model 
som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=1000, 
                 alpha=c(0.3,0.01), 
                 keep.data = TRUE )

#--------------------
# SOM Visualisations
#--------------------

# Load custom palette to use in visualisations
source('coolBlueHotRed.R')

# Plot of the training progress
plot(som_model, type = "changes")

# Plot of the code spread
plot(som_model, type = "codes", codeRendering = "segments", palette.name = coolBlueHotRed)

# Plot of object count within each nodes
plot(som_model, type = "counts", main="Node Counts",
     palette.name=coolBlueHotRed, shape="straight")

# Plot of node quality (mean distance of objects mapped on a unit to codebook vecotr)
plot(som_model, type = "quality", main="Node Quality/Distance",
     palette.name=coolBlueHotRed, shape="straight")

# Plot neighbour distances or U-matrix (sum of distances to immediate neighbour)
plot(som_model, type="dist.neighbours", main = "SOM Neighbour Distances"
     , palette.name=grey.colors, shape="straight")

# Plot heatmap of each variable at normalized values
pdf("som_heatmaps6.pdf")
par(mfrow = c(5, 3))
for (i in 1:ncol(getCodes(som_model))) {
  plot(som_model, type = "property", property = getCodes(som_model)[,i],
       main=paste(colnames(getCodes(som_model))[i], "properties"),
       palette.name=coolBlueHotRed, shape = "straight", keepMargins = TRUE)
}
dev.off()

# Practical heatmap method
## Plot the heatmap for each variable at scaled / normalised values
var <- 1 #define the variable to plot

plot(som_model, type = "property", property = getCodes(som_model)[,var],
     main=colnames(getCodes(som_model))[var], 
     palette.name=coolBlueHotRed)

#----------------
# Clustering SOM
#----------------

# Show the Within Cluster Sum of Squares metric for k-means for different clustering sizes
# Can be used as a "rough" indicator of the ideal number of clusters
mydata <- getCodes(som_model)

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))

for (i in 2:20) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
# Plot the WCSS or cluster number
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", 
     main="Within cluster sum of squares (WCSS)")

# Form clusters on grid
## Use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(getCodes(som_model))), 6) # Run with 5, 8, and 11 clusters

# Colour palette definition designed for discrete data that is colour blind friendly   
pretty_palette <- c("#CC79A7", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                    "#D55E00" ) 

# Show the same plot with the codes instead of just colours
plot(som_model, type="codes", codeRendering = "segments", bgcol = pretty_palette[som_cluster], 
     main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)

#--------------
# Mapping Areas
#--------------

# Create a label dataset for the SOM (random selection of Intermediate Zone names)
## Extract names of data zones from map data
geog_names <- simd_data$Intermediate_Zone
## Remove duplicates names 
geog_names[duplicated(geog_names)] <- NA
## Find the index of the names which are not NA
naset <- which(!is.na(geog_names))
## Make all but 10 of the placenames in the names dataset NA
naset <- sample(naset, length(naset)-10)
geog_names[naset] <- NA

# Replot nodes as color-coded clusters
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster],
     main = "Clusters", labels = geog_names, shape="straight", pch = NA)
add.cluster.boundaries(som_model, som_cluster)

# Create dataframe of the data zone and of the cluster unit
cluster_details <- data.frame(id=simd_data$Data_Zone, 
                              cluster= som_cluster[som_model$unit.classif])

# Merge cluster details onto the fortified spatial polygon dataframe created earlier
mappoints <- merge(simd_merge, cluster_details, by.x = "DataZone", by.y = "id")

# Map the areas and colour by cluster
ggplot(data = mappoints, aes(fill = factor(cluster))) +
  scale_fill_manual(values = pretty_palette)+
  geom_sf(color = "transparent") +
  theme(legend.position = "bottom") +
  ggtitle("SOM clusters") +
  coord_sf() +
  theme()

# Combine the census and cluster data onto our original spatial polygons simd_map
simd_map <- merge(simd_map, simd_data, 
                  by.x="DataZone", by.y="Data_Zone")
simd_map <- merge(simd_map, cluster_details,
                  by.x="DataZone", by.y="id")

# Specify the file path where you want to save the shapefile 
output_path <- "/Users/avacorry-roberts1/Documents/Edinburgh/VA/assessment2/Assessment Data/simd_map_clustered.csv"

# Write the 'sf' object to a shapefile
st_write(simd_map, dsn=output_path, append=TRUE)














