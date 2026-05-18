# Geographically Weighted Regression

# GWmodel3 is the latest library for this in R
# https://gwmodel-lab.github.io/GWmodel3/index.html
# I couldn't install it - possibly because I'm on Windows, I am not sure. 

# devtools::install_github("GWmodel-Lab/GWmodel3")
# need Rtools to build from source
# library(GWmodel3)
## didn't work, can't install from source. 
# missing GSL?

# GWmodel is an older version
# works - but needs sp library (which is approaching retirement)

# I used this practical https://rpubs.com/gwmodel/176883
# going to demo it. It may or may not work on your machines. 

# We are creating multiple regression models. Do each one for a 
# specified area - a moving window as the article mentions. 

# load the library

library(GWmodel)

# load the data
data(LondonHP)

#view the data
head(data.frame(londonhp))
#we have a range of data on house purchases in London
# houses purchase price (PURCHASE) and floor area (FLOORSZ) 
# There are also variables relating to social conditions in the 
# neighbourhood of the property - for example the proportion of 
# the working population employed in professional or managerial jobs (PROF) .

# Here, the y-variable will be housing cost per square meter of floor area.
londonhp$PPSQM <- londonhp$PURCHASE / londonhp$FLOORSZ
head(data.frame(londonhp))

#see histogram
hist(londonhp$PPSQM,main="Price per Square Meter (Pounds)",xlab="Cost per Sq. Meter",ylab='Frequency')

# From this there is a fairly clear distribution shape with a small 
# number of very costly properties(in terms of cost per unit of floor area). 

# We can also look some summary statistics

mean(londonhp$PPSQM)
sd(londonhp$PPSQM)

# needs a new library for skewness
library(e1071)
skewness(londonhp$PPSQM)
# From this value of around 2.1 it can be seen that this distribution
# is strongly positively skewed - suggesting a large upper tail - 
# that is, a central group of typically-priced houses together with 
# a long tail of relatively expensive ones - much as the histogram 
# showed.

# can also apply this to PROF
skewness(londonhp$PROF)
# very slightly negative, but essentially 0. 
# so data has a symmetrical distribution

hist(londonhp$PROF,main="Proportion of Workforce in Professional occupations",xlab="Proportion",ylab='Frequency')

## A Relationship in the Data?

linmod <- lm(PPSQM~PROF,data=londonhp) # Store the regression model to use in a plot later
summary(linmod)

# shows link between PROF (professional employment) and price

# can also see visually, with a trend line:

plot(PPSQM~PROF,data=londonhp,xlab='Proportion Professional/Managerial',ylab='Cost per Square Metre')
abline(linmod)

# is this the same everywhere in London?
# can use a coplot - extract data for different areas (x/y coords)
# and plot

panel.lm <- function(x,y,...) {
  points(x, y, pch=16)
  abline(lm(y~x))
}
coplot(PPSQM~PROF|coords.x1*coords.x2,data=data.frame(londonhp),panel=panel.lm,overlap=0.8)

# relation seems to vary with space. 
# so we will use GWR to explore this.

## Geographically Weighted Regression

# look at the data

data(LondonBorough)
plot(londonborough)
plot(londonhp, pch=16, col='firebrick',add=TRUE)
#(clear plot if you want to)

# put the observations in a grid

grd <- SpatialGrid(GridTopology(c(503400,155400),c(1000,1000),c(60,48)))
plot(grd)
plot(londonborough,add=TRUE,col=adjustcolor('navyblue',alpha.f=0.5))

# for each grid cell, plot the distances between the points

DM <- gw.dist(dp.locat=coordinates(londonhp),rp.locat=coordinates(grd))

gwr.res <- gwr.basic(PPSQM~PROF, data=londonhp, regression.points=grd, bw=10000, dMat=DM,kernel='gaussian')

gwr.res 

#GWR stats output
# fixed bandwidth = 10,000m

# we see the original non-spatial regression (same as above)
# and the range of the spatial regressions:

# ****************Summary of GWR coefficient estimates:******************
#               Min.  1st Qu.   Median  3rd Qu.    Max.
# Intercept   32.324  310.362  477.109  588.854  976.88
# PROF      1342.396 2447.330 3071.700 3680.827 4415.53

# can also get a map:

image(gwr.res$SDF,'PROF')
plot(londonborough,add=TRUE)
plot(londonhp,add=TRUE,pch=16,col='blueviolet')
#high values yellow, low values red

# and a contour plot:

plot(londonborough,border='lightgrey')
contour(gwr.res$SDF,'PROF',lwd=3,add=TRUE)
plot(londonhp,add=TRUE,pch=16,col=adjustcolor('blueviolet',alpha.f=0.4))

# the slope (relationship) is greater in central north london > 4000
# lower in south-east london ~1500

# can also calculate standard error - how reliable these estimates are

library(boot)
set.seed(4676)
gwrcoef <- function(hpdf,i) gwr.basic(PPSQM~PROF, data=londonhp[i,], regression.points=grd, bw=10000, dMat=DM[i,],kernel='gaussian')$SDF$PROF
bootres <- boot(londonhp,gwrcoef,100)
gwr.res$SDF$bsePROF <- sqrt(apply(bootres$t,2,var))
image(gwr.res$SDF,'bsePROF')
plot(londonborough,add=TRUE)

#high values yellow, low values red
#more potential for error around the higher values and lowest values - the extremes

# can also do this in a different way

gwr.res$SDF$biasPROF <- bootres$t0 - apply(bootres$t,2,mean)
image(gwr.res$SDF,'biasPROF')
plot(londonborough,add=TRUE)

# although this gives a different output to the image in prac
# but error still around the high values - but less so the low values

