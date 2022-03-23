install.packages('raster', repos = 'http://r-forge.r-project.org/', type = 'source')
install.packages("spcosa")
install.packages("rgdal")
install.packages("sf")
install.packages("exactextractr")
library(exactextractr)
library(raster)
library(leaflet)
library(spcosa)
library(sp)
library(sf)
grd <- expand.grid(s1 = 1:100, s2 = 1:50)
gridded(grd) <- ~ s1 * s2

biomass <- raster("https://www.dropbox.com/s/zvzqb4a0qvhulye/bc_NFI_MODIS250m_2011_kNN_Structure_Biomass_TotalLiveAboveGround_v1_victoria_4326.tif?dl=1")
bimass_pallet <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(biomass),na.color = "transparent")
m <- leaflet() %>% addTiles() %>%
  addRasterImage(biomass, colors = bimass_pallet, opacity = 0.8) %>%
  addLegend(pal = bimass_pallet, values = values(biomass),title = "Biomass Total\n Live Above Ground") 

values(dem)[values(dem) <= 0] = NA
#writeRaster(dem,'/home/.../DEM_0.tif', overwrite=TRUE)
dem <- raster("https://www.dropbox.com/s/nrxzy6bemzg4tks/dem_lowresolution.tif?dl=1")
dem_pallet <- colorNumeric(c("#008435", "#33cc00", "#f4f071","#f4bd45","#99642b","#ffffff"), values(dem),na.color = "transparent")

leaflet() %>% addTiles() %>%
  addRasterImage(dem, colors = dem_pallet, opacity = 0.8) %>%
  addLegend(pal = dem_pallet, values = values(dem),title = "DEM") 

reclassify_dem <- raster("https://www.dropbox.com/s/2zgo6f62i5dfavp/dem_reclassify_low_resolution.tif?dl=1")
values(reclassify_dem)[values(reclassify_dem) <= 0] = NA
#writeRaster(reclassify_dem,'C:\\Users\\Majid\\OneDrive - Wilfrid Laurier University\\258 Materials\\GESC258\\lab4\\DEM_reclassify_0.tif', overwrite=TRUE)

reclassify_pallet <- colorNumeric(c("#e7e1ef","#dd1c77"), values(reclassify_dem),na.color = "transparent")

leaflet() %>% addTiles() %>%
  addRasterImage(reclassify_dem, colors = reclassify_pallet, opacity = 0.8) %>%
  addLegend(pal = reclassify_pallet, values = values(reclassify_dem),title = "DEM") 

grid <- st_make_grid(reclassify_dem,what = "centers",n = c(20, 20))

leaflet() %>% addTiles() %>% addCircles(data = grid)
g <- st_as_sf(grid)
f <- extract(biomass, g)
mm <- st_sf(a=f,grid)
mm <- mm[!is.na(mm$a),]
leaflet() %>% addTiles() %>% addCircles(data = mm)  
mm$a

hist(mm$a)


m 
inp <- rgdal::readGDAL("https://www.dropbox.com/s/zvzqb4a0qvhulye/bc_NFI_MODIS250m_2011_kNN_Structure_Biomass_TotalLiveAboveGround_v1_victoria_4326.tif?dl=1")
stratification <- stratify(inp, nStrata = 30, nTry = 1)
plot(stratification)

m%>% leaflet::add(sample_1@coords[,1],sample_1@coords[,2])
sample_size <- 30
boundary<-as.data.frame(r,xy=T)
boundary <- boundary[!is.nan(boundary$bc_NFI_MODIS250m_2011_kNN_Structure_Biomass_TotalLiveAboveGround_v1_victoria_4326.tif.dl.1),]
random<-boundary[boundary[,3]==0,][
  sample(nrow(subset(boundary,boundary[,3]==0)),sample_size),]

random2<-as.numeric(row.names(random))
random3<-boundary[random2,2:1]

sample_1 <- sampleRandom(r, size=10, cells=TRUE, sp=TRUE)
m %>% addSimpleGraticule()
s_x <- sample_1@coords[,1]
s_y <- s_x <- sample_1@coords[,2]
m <- m %>% leaflet::addCircleMarkers(sample_1@coords[,1],sample_1@coords[,2])

makegrid(r, n = 50, nsig = 2, 100,pretty = TRUE)
population_mean <- cellStats(r, stat='mean', na.rm=TRUE)
population_stdev <- cellStats(r, stat='sd', na.rm=TRUE,asSample=F)

mean(sample_1$bc_NFI_MODIS250m_2011_kNN_Structure_Biomass_TotalLiveAboveGround_v1_victoria_4326.tif.dl.1)
hist(r$bc_NFI_MODIS250m_2011_kNN_Structure_Biomass_TotalLiveAboveGround_v1_victoria_4326.tif.dl.1)
spsample(r, 100, type="regular")



x <- sampleRegular(biomass, size=100, asRaster=TRUE,xy=T)
leaflet() %>% addTiles() %>%
  addRasterImage(x, colors = bimass_pallet, opacity = 0.8)




```{r}
p <- plot.new() # create a new plot
plot.window(xlim=c(0, 50), ylim=c(min(p_data), max(p_data)))  # set our plot's ylim and xlim
box()
axis(side=1, at=seq(0,50, by=1), labels=seq(0,50, by=1))
axis(side=2, at=seq(min(p_data),max(p_data), by=10), labels=seq(min(p_data),max(p_data), by=10))
mtext("Biomass", side = 2, las=3, line=3 ) # you can change values of las and line to see their effect on your axis's title
mtext("Sample Size", side = 1, las=1, line=3)

points(x = sample_size, y = sample_1_mean, pch=20, cex=1)
# hack: we draw arrows but with very special "arrowheads"
arrows(sample_size, lower, sample_size, upper, length=0.05, angle=90, code=3)
points(x = sample_size, y = p_mean, pch=20, cex=1,col="orange")

####new lines
points(x = sample_size, y = sample_2_mean, pch=20, cex=1, col="green")
arrows(sample_size, sample_2_lower, sample_size, sample_2_upper, length=0.05, angle=90, code=3,, col="green")
```


```{r}
plot.new() # create a new plot
plot.window(xlim=c(0, 50), ylim=c(min(p_data), max(p_data)))  # set our plot's ylim and xlim
box()
axis(side=1, at=seq(0,50, by=1), labels=seq(0,50, by=1))
axis(side=2, at=seq(min(p_data),max(p_data), by=10), labels=seq(min(p_data),max(p_data), by=10))
mtext("Biomass", side = 2, las=3, line=3 ) # you can change values of las and line to see their effect on your axis's title
mtext("Sample Size", side = 1, las=1, line=3)

points(x = sample_size, y = sample_1_mean, pch=20, cex=1)
# hack: we draw arrows but with very special "arrowheads"
arrows(sample_size, lower, sample_size, upper, length=0.05, angle=90, code=3)
text(x = sample_size, y = sample_1_mean, "Simple random sample's mean")

points(x = sample_size, y = p_mean, pch=20, cex=1,col="orange")
text(x = sample_size, y = p_mean, "Exact Population's mean")
```

```{r}
plot.new() # create a new plot
plot.window(xlim=c(0, 50), ylim=c(min(p_data), max(p_data)))  # set our plot's ylim and xlim
box()
axis(side=1, at=seq(0,50, by=1), labels=seq(0,50, by=1))
axis(side=2, at=seq(min(p_data),max(p_data), by=10), labels=seq(min(p_data),max(p_data), by=10))
mtext("Biomass", side = 2, las=3, line=3 ) # you can change values of las and line to see their effect on your axis's title
mtext("Sample Size", side = 1, las=1, line=3)

points(x = sample_size, y = sample_1_mean, pch=20, cex=1)
# hack: we draw arrows but with very special "arrowheads"
arrows(sample_size, lower, sample_size, upper, length=0.05, angle=90, code=3)
text(x = sample_size, y = sample_1_mean, "Simple random sample's mean")

points(x = sample_size, y = p_mean, pch=20, cex=1,col="orange")
text(x = sample_size, y = p_mean, "Exact Population's mean")
```