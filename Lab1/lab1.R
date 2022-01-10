install.packages("spData");
install.packages('spDataLarge',
                 repos='https://nowosad.github.io/drat/', type='source');
library(spData) 
data(world)


cities <- read.csv("https://github.com/am2222/GESC258/raw/master/Lab1/data/canadacities.csv")
c.pops <- cities$population # extract populations vector
c.pops.n <- length(c.pops) # get the length of the populations vector, which is the number of observations
c.pops.sum <- sum(c.pops) # sum up the heights vector
c.pops.xbar <- c.pops.sum / c.pops.n 
sprintf("The arithmetic mean populations in the canadian cities data set is: %s", round(c.pops.xbar,1))

#Of course there is as well an in-built function called mean().

mean(c.pops)

#check our result and the output of the mean function
all.equal(mean(c.pops),c.pops.xbar )
#cities$pop.density <- cities$pop.size / cities$area_km2
#cities$pop.weight <- cities$pop.size / sum(cities$pop.size)

plot(c.pops)
median(c.pops)
plot(c.pops, ylim = c(min(c.pops),max(c.pops)*1.3)) #plot figure
abline(h = mean(c.pops), 
       col='red', 
       lwd = 3) # add horizontal line

abline(h = median(c.pops), 
       col='green', 
       lwd = 3) # add horizontal line

legend('topright', 
       legend = c("Median","Arithmetic mean"), 
       col = c("green","red"), 
       lty = "solid") # add legend
# we see some outlayers. lets remove them

uniqv <- unique(c.pops)
uniqv[which.max(tabulate(match(c.pops, uniqv)))]
#Measures of Dispersion
quant.vars <- c("city", "province_name", "lat", "lng", "population")
cities.quant <- cities[quant.vars]
head(cities.quant, 10)


#mean
cities.quant.mean <- mean(cities.quant$population)
#median
cities.quant.median<- median(cities.quant$population)
#variance
cities.quant.var <- var(cities.quant$population)
#standard deviation
cities.quant.sd <- sd(cities.quant$population)
# concatenate the vectors and round to 2 digits
cities.quant.stats <- round(cbind(cities.quant.mean,
                                  cities.quant.median, 
                                  cities.quant.var, 
                                  cities.quant.sd),2)
# rename column names
colnames(cities.quant.stats) <- c('mean', 'median','variance', 'standard deviation')
cities.quant.stats

#Use of the Standard Deviation
#By using the mean and standard deviation, we can find the proportion or percentage of the total observations that fall within a given interval about the mean.

#Chebyshev's Theorem
#Chebyshev's theorem gives a lower bound for the area under a curve between two points that are on opposite sides of the mean and at the same distance from the mean.

#For any number k greater than 1, at least 1???1/k2 of the data values lie within k standard deviations of the mean.

#Let us use R to gain some intuition for Chebyshev's theorem.

k <- seq(1,4,by = 0.1)
auc <- 1-(1/k^2)
auc.percent <- round(auc*100)
cbind(k,auc.percent)


#To put it in words: Let us pick a value for k: k=2. This means that at least 75% of the data values lie within 2 standard deviations of the mean.

#Let us plot Chebyshev's theorem with R:
  
  plot(k, 
       auc.percent, 
       col = 'blue', 
       pch = 19, 
       xlab = 'k', 
       ylab = 'percent', 
       main = 'Chebyshev\'s theorem' )


  #get ragne
  #The range as a measure of dispersion is simple to calculate. It is obtained by taking the difference between the largest and the smallest values in a data set.
  
  #Range=Largest value???Smallest value
  apply(students.quant, 2, range)
 # The range, like the mean, has the disadvantage of being influenced by outliers. Consequently, the range is not a good measure of dispersion to use for a data set that contains outliers. Another disadvantage of using the range as a measure of dispersion is that its calculation is based on two values only: the largest and the smallest. All other values in a data set are ignored when calculating the range. Thus, the range is not a very satisfactory measure of dispersion (Mann 2012).
  #A measure of position determines the position of a single value in relation to other values in a sample or a population data set. Unlike the mean and the standard deviation, descriptive measures based on quantiles are not sensitive to the influence of a few extreme observations. For this reason, descriptive measures based on quantiles are often preferred over those based on the mean and standard deviation (Weiss 2010).
  
  #Quantiles are cut points dividing the range of the data into contiguous intervals with equal probabilities. Certain quantiles are particularly important: The median of a data set divides the data into two equal parts: the bottom 50% and the top 50%. Quartiles divide the data four equal parts and percentiles divide it into hundredths, or 100 equal parts. Note that the median is also the 50th percentile. Deciles divide a data set into tenths (10 equal parts), and the quintiles divide a data set into fifths (5 equal parts). There is always one less quantile than the number of groups created (e.g. There are 3 quartiles dividing the data into 4 equal parts!).
  
  
cities_filtered <- cities[cities$population<=cities.quant.mean+ 2*cities.quant.sd & cities$population>=cities.quant.mean-2*cities.quant.sd,]  
plot(c.pops, ylim = c(cities.quant.mean-2*cities.quant.sd,cities.quant.mean+ 2*cities.quant.sd)) #plot figure
abline(h = mean(c.pops), 
       col='red', 
       lwd = 3) # add horizontal line

abline(h = median(c.pops), 
       col='green', 
       lwd = 3) # add horizontal line

legend('topright', 
       legend = c("Median","Arithmetic mean"), 
       col = c("green","red"), 
       lty = "solid") # add legend

prov.popmean <- aggregate(cities.quant$population, list(cities.quant$province_name), FUN=mean) 
prov.latmean <- aggregate(cities.quant$lat, list(cities.quant$province_name), FUN=mean) 
prov.lonmean <- aggregate(cities.quant$lng, list(cities.quant$province_name), FUN=mean) 

prov <- cbind(prov.popmean,
      prov.latmean$x , 
      prov.lonmean$x)
# rename column names
colnames(prov) <- c('province_name', 'population','lat', 'lng')


# Let's read the jeoJson file that is stored on the web with the geojsonio library:
library(geojsonio) #install.packages("geojsonio")
spdf <- geojson_read("georef-canada-province.geojson",  what = "sp")
# plot the selected are with sp
library(sp)
par(mar=c(0,0,0,0))
plot(spdf, col="grey")


library(broom)
spdf_fortified <- tidy(spdf)

# Plot it
library(ggplot2)
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  theme_void() +
  coord_map()+
  geom_point(data=prov, 
             aes(x=lng, y=lat), colour="Deep Pink", 
             fill="Pink",pch=21, size=5, alpha=I(0.7))


ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  theme_void() +
  coord_map()+
  geom_point(data=prov, 
             aes(x=lng, y=lat, colour=population), size=5, alpha=I(0.7))+
geom_text(data= prov,aes(x=lng, y=lat, label=province_name),
          color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  annotate(geom = "text", x = -90, y = 26, label = "Gulf of Mexico", 
           fontface = "italic", color = "grey22", size = 6)

geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(prov), " countries)"))

hist(cities_filtered$density, breaks = 'sturges')


library(dbplyr)
library(dplyr)
library(tidyr)
crime <- read.csv("CSI_rankings_over10000_2020(E).csv",skip=5,nrows=325,col.names = c("Respondent Code",	"Police Service",	"Population",
                                                                            "Overall Crime Severity Index value","Overall Crime Severity Index rank","Violent Crime Severity Index index",		"Violent Crime Severity Index rank",
                                                                            "Non-violent Crime Severity Index index","Non-violent Crime Severity Index rank"))
crime <- crime %>% separate(Police.Service, c("a", "b","c","d"),sep=",") 
cj <- cities %>% left_join(crime, by=c("city"="a"))



editTable(cities, outdir="C:/Users/Majid/OneDrive - Wilfrid Laurier University/258 Materials/GESC258/Lab1/data", outfilename="cities")
crime <- dget("newDF.txt")
cities <- dget("cities.txt")
cj <- cities %>% left_join(crime, by=c("city"="a"))
colnames(cj)

names(cj)[names(cj) == "Overall.Crime.Severity.Index.value"] <- "overall_csi_index"
names(cj)[names(cj) == "Overall.Crime.Severity.Index.rank"] <- "overall_csi_rank"

names(cj)[names(cj) == "Violent.Crime.Severity.Index.index"] <- "violent_csi_index"
names(cj)[names(cj) == "Violent.Crime.Severity.Index.rank"] <- "violent_csi_rank"

names(cj)[names(cj) == "Non.violent.Crime.Severity.Index.index"] <- "nonviolent_csi_index"
names(cj)[names(cj) == "Non.violent.Crime.Severity.Index.rank"] <- "nonviolent_csi_rank"

cj2 <- cj %>% select("city","city_ascii","province_id","province_name", "lat" ,"lng","population", "density","overall_csi_index","overall_csi_rank",
                    "violent_csi_index","violent_csi_rank","nonviolent_csi_index","nonviolent_csi_rank")
write.csv(cj2,"canadian_population_centers.csv")
