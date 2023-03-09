#Script to visualize 3D point cloud generated from UAV photo orthomosaic and 
#to render/ merge the point cloud with NDVI values

#-------------------------------------------------------------------------------
library(terra)# for working with raster
library(sf) # for working spatial class
library(lidR)# for working with  LAZ files
library(rayshader) # for 3D viz
library(rgl) # for interactive plots
library(ggplot2)
library(magick)
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
setwd("E:/Allan Wafula/My Works/MyCodes/R - Lidar Processing/Wote_Farm_PC+NDVI")
getwd()
#-------------------------------------------------------------------------------




# ------------------------------------------------------------------------------
#read laz files
las = readLAS("2.laz")
las
#check the data
las_check(las)
#plot the point cloud
plot(las, bg = "white", axis = FALSE, clear_artifacts = FALSE, legend = TRUE)
exportPath = tempfile()
par3d(zoom = 0.4)
movie3d(spin3d(), duration = 150, movie = exportPath)
# Convert the movie to a GIF file

# ------------------------------------------------------------------------------




# ------------------------------------------------------------------------------
#load NDVI image
ndvi <- rast('2.tif')
ndvi #check values
#NDVi values are out of -1 ~ 1 range, remove out of range values
ndvi[ndvi < -1] <- NA
ndvi[ndvi > 1] <- NA
ndvi #check values
#plot NDVI
plot(ndvi, col = rev(terrain.colors(10)), legend = FALSE)
# ------------------------------------------------------------------------------




# ------------------------------------------------------------------------------
#merge NDVI with point cloud
ndvi_pc <- merge_spatial(las, ndvi, "ndvi")
#check the data
las_check(ndvi_pc)
#plot NDVI-point cloud
exportPath = tempfile()
ndvi_pc_plot <- plot(ndvi_pc, bg = "white", color = "ndvi", axis = FALSE, 
                     clear_artifacts = FALSE, legend = TRUE, mapview = TRUE)
movie3d(spin3d(), duration = 15, movie = exportPath)
# ------------------------------------------------------------------------------




# ------------------------------------------------------------------------------
#Check the point cloud z profile to prepare for classification
#Create a transect
p1 <- c(412115.4, 9738280)
p2 <- c(412350, 9738260)
las_tr <- clip_transect(las, p1, p2, width = 10, xz = TRUE)
plot(las_tr, bg = "white", axis = TRUE, clear_artifacts = FALSE, legend = TRUE) #view the transect
#Plot the transect using ggplot
ggplot(las_tr@data, aes(X,Z, color = Z)) + 
  geom_point(size = 0.5) + 
  coord_equal() + 
  theme_minimal() +
  scale_color_gradientn(colours = height.colors(50))
#Plot the transect using a custom function
plot_crossection <- function(las,
                             p1 = c(min(las@data$X), mean(las@data$Y)),
                             p2 = c(max(las@data$X), mean(las@data$Y)),
                             width = 4, colour_by = NULL)
{
  colour_by <- enquo(colour_by)
  data_clip <- clip_transect(las, p1, p2, width)
  p <- ggplot(data_clip@data, aes(X,Z)) + geom_point(size = 0.5) + coord_equal() 
  
  if (!is.null(colour_by))
    p <- p + aes(color = !!colour_by) + labs(color = "")
  
  return(p)
}
plot_crossection(las, colour_by = factor(Classification))
# ------------------------------------------------------------------------------





# ------------------------------------------------------------------------------
#Ground classification
las <- classify_ground(las, algorithm = pmf(ws = 3, th = 1))
#Visualize the classification
plot(las, color = "Classification", size = 3, bg = "white") #Visualize the classification
#Inspect the classification by plotting a transect
p1 <- c(412115.4, 9738280)
p2 <- c(412350, 9738260)
plot_crossection(las, p1 , p2, colour_by = factor(Classification))
# ------------------------------------------------------------------------------


