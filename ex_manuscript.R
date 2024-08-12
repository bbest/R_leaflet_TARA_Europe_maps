# Load the libraries
library(raster) library(leaflet)
#Download from NOAA the specific NetCDF file using wget
system("wget https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-i nterpolation/v2.1/access/avhrr/202405/oisst-avhrr-v02r01.20240515.nc")
#Open the downloaded NetCDF file with the raster function
r <- raster("oisst-avhrr-v02r01.20240515.nc") #Crop the raster on the Labrador Sea area
r <- crop(r, extent(295, 322, 50, 68))
#Create a simple color palette
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r),na.color = "transparent")
m <- leaflet() %>% #Returns a Leaflet map widget
  addTiles() %>% #Adds default map tiles
  #Adds the layer (i.e., SST)
  addRasterImage(r, colors = pal, project = FALSE, opacity = 1) %>% #Adds the color bar
  addLegend(pal = pal, values = values(r), title = "SST") m #Print the map