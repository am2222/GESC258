GESC 258- Lab1
================

## GESC 258- Geographical Research Methods

Lab 1 - Introduction to R ASSIGNED in Lab1, January 14th, DUE DATES in
the MyLearningSpace dropbox  
before Lab 2 on January 21th Exercise Worth: 5% of final grade

R-Studio is one of the most widely used software packages in data
science, statistics and data analysis more generally. Many employers in
the environmental field now greatly value skills in R - so even though
the learning curve can be a little steeper, it is well worth the effort
to learn this approach to data analysis. Attend your first lab session
to briefly meet your TA instructor and to get the course software
installed on your computer. We will be using an open source software
package called R and R-Studio for most of the work in this course. In
this lab you will:  
• Familiarize yourself with the R studio interface  
• Learn the basic of R programing • Create simple graphs

As a supplement to the lab materials, I highly recommend using YaRrr\!
The Pirate’s Guide to R - which has lots of great information and
resources for learning R. You can read chapter 2 for how to get it
installed on your machine. The TA will verify everyone has a working
installation of R on their machine in lab.

## Part I: Getting started with R

You can include R code in the document as follows:

## Part II: Statistical Analysis in R

In this section, we’ll analyze a dataset called cities. The dataset
contains data from `1748` population centers acrous Canada. Run the
following lines to load this dataset into R.

``` r
cities <- read.csv("canadian_population_centers.csv") #Hint: type ?read.csv into R console to learn how this function works
```

> “Hint: type ?read.csv into R console to learn how this function works”

\[
f(x)=\frac{1}{\sqrt{2\pi}\sigma}e^{-\frac{(x-\mu)^2}{2\sigma^2}}
\]

``` r
knitr::kable(head(cities))
```

| X | city      | city\_ascii | province\_id | province\_name   |     lat |        lng | population | density | overall\_csi\_index | overall\_csi\_rank | violent\_csi\_index | violent\_csi\_rank | nonviolent\_csi\_index | nonviolent\_csi\_rank |
| -: | :-------- | :---------- | :----------- | :--------------- | ------: | ---------: | ---------: | ------: | ------------------: | -----------------: | ------------------: | -----------------: | ---------------------: | --------------------: |
| 1 | Toronto   | Toronto     | ON           | Ontario          | 43.7417 |  \-79.3733 |    5429524 |  4334.4 |               57.84 |                168 |               90.41 |                 95 |                  45.99 |                   196 |
| 2 | Montréal  | Montreal    | QC           | Quebec           | 45.5089 |  \-73.5617 |    3519595 |  3889.0 |               67.29 |                135 |               92.11 |                 90 |                  58.20 |                   157 |
| 3 | Vancouver | Vancouver   | BC           | British Columbia | 49.2500 | \-123.1000 |    2264823 |  5492.6 |              104.67 |                 58 |               99.81 |                 74 |                 106.18 |                    55 |
| 4 | Calgary   | Calgary     | AB           | Alberta          | 51.0500 | \-114.0667 |    1239220 |  1501.1 |               79.96 |                101 |               78.26 |                118 |                  80.38 |                    95 |
| 5 | Edmonton  | Edmonton    | AB           | Alberta          | 53.5344 | \-113.4903 |    1062643 |  1360.9 |              115.55 |                 40 |              127.42 |                 43 |                 111.00 |                    51 |
| 6 | Ottawa    | Ottawa      | ON           | Ontario          | 45.4247 |  \-75.6950 |     989567 |   334.0 |               49.05 |                197 |               56.38 |                194 |                  46.30 |                   192 |

``` r
tail(cities)
```

    ##         X              city        city_ascii province_id province_name     lat
    ## 1743 1743    Baie-du-Febvre    Baie-du-Febvre          QC        Quebec 46.1300
    ## 1744 1744        Durham-Sud        Durham-Sud          QC        Quebec 45.6667
    ## 1745 1745         Melbourne         Melbourne          QC        Quebec 45.5800
    ## 1746 1746   Nipawin No. 487   Nipawin No. 487          SK  Saskatchewan 53.2881
    ## 1747 1747 Duck Lake No. 463 Duck Lake No. 463          SK  Saskatchewan 52.9596
    ## 1748 1748              Oyen              Oyen          AB       Alberta 51.3522
    ##            lng population density overall_csi_index overall_csi_rank
    ## 1743  -72.7200       1010    10.4                NA               NA
    ## 1744  -72.3333       1008    10.8                NA               NA
    ## 1745  -72.1700       1004     5.8                NA               NA
    ## 1746 -104.0544       1004     1.1                NA               NA
    ## 1747 -106.2089       1004     1.0                NA               NA
    ## 1748 -110.4739       1001   189.6                NA               NA
    ##      violent_csi_index violent_csi_rank nonviolent_csi_index
    ## 1743                NA               NA                   NA
    ## 1744                NA               NA                   NA
    ## 1745                NA               NA                   NA
    ## 1746                NA               NA                   NA
    ## 1747                NA               NA                   NA
    ## 1748                NA               NA                   NA
    ##      nonviolent_csi_rank
    ## 1743                  NA
    ## 1744                  NA
    ## 1745                  NA
    ## 1746                  NA
    ## 1747                  NA
    ## 1748                  NA

\#Measures of Central Tendency There are three main measures of central
tendency: the mean, the median, and the mode;

``` r
c.pops <- cities$population # extract populations vector
c.pops.n <- length(c.pops) # get the length of the populations vector, which is the number of observations
c.pops.sum <- sum(c.pops) # sum up the heights vector
c.pops.xbar <- c.pops.sum / c.pops.n 
sprintf("The arithmetic mean populations in the canadian cities data set is: %s", round(c.pops.xbar,1))
```

    ## [1] "The arithmetic mean populations in the canadian cities data set is: 23100.2"

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
