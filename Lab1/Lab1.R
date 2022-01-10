install.packages(c("geojsonio","ggplot2","broom","sp","ggspatial")) 
library(geojsonio)
library(sp)
library(broom)
library(ggplot2)
library(ggspatial)
library(maps)



cities <- read.csv("https://raw.githubusercontent.com/am2222/GESC258/master/Lab1/data/canadian_population_centers.csv") #loading dataset into R as a dataframe

prov.popmean <- aggregate(cities$population, list(cities$province_name), FUN=mean) #mean of population per each province 

# next we want to calculate spatial mean center of the cities per each province
prov.latmean <- aggregate(cities$lat, list(cities$province_name), FUN=mean)  #mean of lat coordinate per each province 
prov.lonmean <- aggregate(cities$lng, list(cities$province_name), FUN=mean) #mean of lng coordinate per each province 

prov <- cbind(prov.popmean,
              prov.latmean$x , 
              prov.lonmean$x)
# rename column names
colnames(prov) <- c('province_name', 'population','lat', 'lng')

# reading data from its url
spdf <- geojson_read("https://github.com/am2222/GESC258/raw/master/Lab1/data/georef-canada-province.geojson",  what = "sp")
# convert it to an object we are able to plot 
spdf_fortified <- tidy(spdf)
# Plot it
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  coord_map("conic", lat0 = 30) +
  geom_point(data=prov, 
             aes(x=lng, y=lat, colour=population), size=5, alpha=I(0.7)) +
  annotate(geom = "text", x = -125, y = 40, label = "Your Name/Date", 
           fontface = "italic", color = "grey22", size =4) 
