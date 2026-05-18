# A geographer’s introduction to space-time regression with GAMs using stgam
# HTML at https://cran.r-project.org/web/packages/stgam/vignettes/space-time-gam-intro_rev.html
# RMD https://github.com/lexcomber/stgam/blob/master/vignettes/space-time-gam-intro_rev.Rmd

# Using stgam spatial and temporal with existing mgcv library
# With a focus on prediction but perhaps more importantly
# on inference (process understanding).

# It highlights the importance of investigations of spatial and / or temporal
# variability before constructing space-time Generalized Additive Models (GAMs).

# It uses sf, and not sp (like GWR example)

library(cols4all)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(cowplot)
library(tmap)

# load the package and data
library(stgam)
data("hp_data")
data("lb")

# lets read the information about the data

help(hp_data) #A dataset of a sample terraced houses sales in the London area
# for 2018 to 2024.
help(lb) #A spatial dataset of of the boundaries of the 33 London Boroughs
# extracted from the GWModel package, cleaned and converted to sf.

# look at the data
qtm(lb)

hp_data

# The analyses will model price per unit area (the priceper variable) in units of pounds per spare metre.
# GAM smooth models will be constructed that regress this against other
# variables in the data including location and time.

# The print out of the first 10 records above shows that the dataset contains
# a time variable (dot) in date format as well as location in metres (X and Y
# from the OSGB projection).

# It often more useful to represent time as continuous scalar values and to
# have location in kilometres rather than metres. The code below uses the
# earliest date in the dataset to create a variable called days to represent
# time (here 100s of days since the earliest date) and rescales the locational
# data, but retaining the original coordinates for mapping:

hp_data <-
  hp_data |>
  # create continuous time variable
  mutate(days = as.numeric(dot - min(dot))/100) |>
  relocate(days, .after = dot) |>
  # scale location and retain original coordinates
  mutate(Xo = X, Yo = Y) |>
  mutate(X = X/1000, Y = Y/1000)

hp_data

# see days and X Y
View(hp_data)

# map the data layers
lb |>
  ggplot() + geom_sf() +
  geom_point(data = hp_data, aes(x = Xo, y = Yo, col = priceper)) +
  scale_color_viridis_c(option = "magma") +
  theme_bw()  +xlab("") + ylab("")

# Pricer higher nearer central London
# no terraced houses in the City of London LAD

# Updating lb data to km (from m) to help with modelling later on
# transform to km
lb <- st_transform(lb, pipeline = "+proj=pipeline +step +proj=unitconvert +xy_out=km")
# remove the projection to avoid confusing ggplot
st_crs(lb) <- NA

lb

# examin the boxplots to see the spread of the data

# boxplots
hp_data |>
  select(lad, price, priceper, tfa, days, beds, cef, pef, X, Y) |>
  pivot_longer(-lad) |>
  ggplot(aes(x = value), fil) +
  geom_boxplot(fill="dodgerblue") +
  facet_wrap(~name, scales = "free") +
  theme_bw()

# (priceper) has a healthy tail.
# (left skewed)

# histograms
hp_data |>
  select(lad, price, priceper, tfa, days, beds, cef, pef, X, Y) |>
  pivot_longer(-lad) |>
  ggplot(aes(x = value), fil) +
  geom_histogram(aes(y=after_stat(density)),bins = 30,
                 fill="tomato", col="white") +
  geom_density(alpha=.5, fill="#FF6666") +
  facet_wrap(~name, scales = "free") +
  theme_bw()

# also histograms and correlations

# correlations
hp_data |>
  select(priceper, price, tfa,  beds, cef, pef) |>
  cor() |> round(3)

# reasonable correlations with the target variable and
# no collinearity amongst the predictor variables
# not looking at time or space yet ("Looking at the variables globally")

# Guide: should have a minimum of about 100 locations and
# a minimum of 50 observation time periods.
# Remember this when working with your own data.

# Section 2: Detecting variability

# Now extending the viability tests to look at space, time and space-time.
# This is done by constructing a series of regression models,
# of different forms and with different parameters.

# an OLS model as an initial step
# predicting priceper (price per area)
# using
# cef Current energy efficiency rating
# pef Potential energy efficiency rating
# beds number of bedrooms
m_ols <- lm(priceper ~ cef + pef + beds, data = hp_data)
summary(m_ols)

# Model is week (0.06 R squared)
# model summary indicates that the some of the variables (pef and beds) are
# significant predictors of the target variable

anova(m_ols)

# ANOVA confirms this
# shows how much variance in priceper is explained by each predictor
# and whether that contribution is statistically significant

# Dummy with LADs to look at space
m_dummy1 <- lm(priceper ~ tfa:lad, data = hp_data)
summary(m_dummy1)
anova(m_dummy1)

#variation in coefficients between boroughs (ladE09000002) shows space is important.

# Dummy with Time
m_dummy2 <- lm(priceper ~ tfa:days, data = hp_data)
summary(m_dummy2)
anova(m_dummy2)

# ditto with 'days'

# Thus, despite the models being globally weak with low R2 values, there is
# evidence of spatial and temporal interactions with the target variable that
# warrant further more formal exploration with respect to potential space-time
# trends. GAMs with smooths offer a route to investigate these.

# using GAM smooths

# create simulated data
set.seed(12)
x  <- runif(500)
mu <- sin(2 * (4 * x - 2)) + 2 * exp(-(16 ^ 2) * ((x - .5) ^ 2))
y  <- rnorm(500, mu, .3)
# plot x and y
ggplot() +
  geom_point(aes(x,y)) +
  theme_bw()

# we can't fit a straight line to this.

# a GAM illustration with a spline using s
gam_s_example <- gam(y~s(x))
# extract the smooth fit
y.s <- gam_s_example$fitted.values
# plot
ggplot() +
  geom_point(aes(x,y), col = "grey") +
  geom_line(aes(x, y = y.s), lwd = 1) +
  theme_bw()

# but can fit a spline / smooth (both terms mean the same thing)

# can do this over time (days)

# the first GAM
gam.1 <- gam(priceper~s(days), data = hp_data)
summary(gam.1)

# Note the use of the actual data variable dot rather than days in the code
# to have a friendly x-axis in the plot, and the use of the predict function
# to extract the standard errors:

# create a data frame with x, predicted y, standard error
x <- hp_data$dot
y <- gam.1$fitted.values
se <- predict(gam.1, se = TRUE, hp_data)$se.fit
u <- y+se
l <- y-se
df <- data.frame(x, y, u, l)
# plot!
ggplot(df, aes(x, y, ymin = l, ymax = u)) +
  geom_ribbon(fill = "lightblue") +
  geom_line() +
  theme_bw() +
  xlab("Date") + ylab("priceper")

# This shows variation in the modelled relationship of priceper with time.
# increase of outdoor space, most likely as a result of the pandemic

# spatial variability

# the second GAM
gam.2 <- gam(priceper~s(X,Y), data = hp_data)
summary(gam.2)

#map
plot(gam.2, asp = 1)

# R^2 is much better = 0.554
# map shows prices higher in central london, but isn't a great map

# 1. create a grid object the study area from the LB  data
l_grid <-
  st_make_grid(lb, square=FALSE,n=50) |>
  st_sf() |>
  st_join(lb) |>
  filter(!is.na(name))
# rename the geometry, sort row names
st_geometry(l_grid) = "geometry"
rownames(l_grid) <- 1:nrow(l_grid)
# create and add coordinates X and Y
coords <- l_grid |> st_centroid() |> st_coordinates()
#> Warning: st_centroid assumes attributes are constant over geometries
l_grid <- l_grid |> bind_cols(coords)

#You may wish to inspect this object:

l_grid
plot(st_geometry(l_grid))

# Before continuing with the mapping procedure:

  # 2. predict over the grid
  yhat <- predict(gam.2, newdata = l_grid)
l_grid |> mutate(yhat = yhat) |>
  # 4.and plot
  ggplot() +
  geom_sf(aes(fill = yhat), col = NA) +
  # adjust default shading
  scale_fill_continuous_c4a_seq("brewer.yl_or_rd", name = "priceper") +
  # add context
  geom_sf(data = lb, fill = NA) +
  # apply and modify plot theme
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm"))

# This models the spatial trends (variations over space) of the target variable.
# It shows space is important in this model.

# Space and time together

# the third GAM
gam.3 <- gam(priceper~te(X,Y,days, d = c(2,1), bs=c('tp','cr')), data = hp_data)
summary(gam.3)

# uses te() to smooth rather than s()
# because space and time work over different scales.

plot(gam.3, asp = 1)
# basic plot changes of time and space (over 9 time chunks, ~365 days)

# 1. create time intervals (see the creation of days variable above)
pred_days = seq(365, 2555, 365)/100
# 2. create coefficient estimates for each time period (n = 7)
res_out <- NULL
for (i in pred_days){
  res.i <- calculate_vcs(input_data = l_grid |> mutate(days = i),
                         mgcv_model = gam.3,
                         terms = NULL)
  res_out <- cbind(res_out, res.i$yhat)
}
# 3. name with years and join to the grid
colnames(res_out) <- paste0("Y_", 2018:2024)

l_grid |> cbind(res_out) |>
  # select the variables and pivot longer
  select(-name, -lad, -X, -Y) |>
  pivot_longer(-geometry) |>
  # make the new days object a factor (to enforce plotting order)
  mutate(name = factor(name, levels = paste0("Y_", 2018:2024))) |>

  # 4. and plot
  ggplot() +
  geom_sf(aes(fill = value), col = NA) +
  # adjust default shading
  scale_fill_continuous_c4a_seq("brewer.yl_or_rd", name = "Predicted \n'priceper'") +
  # facet
  facet_wrap(~name, ncol = 3) +
  # apply and modify plot theme
  theme_bw() +
  theme(
    legend.position = "inside",
    legend.direction = "horizontal",
    #legend.position.inside = c(0.7, 0.15),
    legend.text=element_text(size=10),
    legend.title=element_text(size=12),
    strip.background = element_rect(fill="white", colour = "white"),
    strip.text = element_text(size = 8, margin = margin(b=4)),
    legend.key.width = unit(1.5, "cm"),
    axis.title=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank())

# by year instead

# can apply smooths to time and space separately rather than together.
# avoid assuming time and space interact

# the fourth GAM
gam.4 <- gam(priceper~s(X,Y) + s(days), data = hp_data)
summary(gam.4)

plot(gam.4, page = 1)

# this gives us a similar output to the previous model.

# including pef (potential energy efficiency) in the model

# the fifth GAM
gam.5 <- gam(priceper~s(X,Y) + s(days) + pef, data = hp_data)
summary(gam.5)

# model summary indicates that pef is a significant predictor of
# priceper and improves the model fit.

# should predictor variables be included in the model: in parametric form or in a smooth?
# do we need the splines? -> next section

plot(gam.5, page = 1)

# Variations in the response variable (priceper) in time, space and
# space-time were explored through elementary model fitting and
# plotting the smooth graphical trends.

# Such investigations are an important initial step.
# They provide evidence of space-time variations in the response variable
# - i.e. whether the effects in space and time are present in the data
# - and can help determine which predictor variables may be of further
# interest. They guide subsequent analysis and avoid making assumptions
# about the presence of space-time interactions, for example by
# plugging every predictor variable into a space-time smooth of some
# kind.

## Section 3 - Effects of Time and Space with predictor variables

# previously we included  s(X,Y) + s(days) and the output gave us
# one Intercept

# instead, we can do each with an Intercept:

gam.5.new <- gam(priceper ~0 + Intercept + s(X,Y,by=Intercept) + s(days, by=Intercept) + pef,
                  data = hp_data |> mutate(Intercept = 1))
summary(gam.5.new)

# which gives us an intercept for each

# we will do this by default going on from here:

hp_data <-
  hp_data |>
  mutate(Intercept = 1)

View(hp_data)

# time

gam.t <- gam(priceper~0 + Intercept + s(X,Y,by=Intercept) + s(days, by=Intercept) +
               pef + s(days, by = pef),
             data = hp_data)
summary(gam.t)

# The summary of this model indicates that the relationship of pef
# with priceper changes over time and has a strong linear negative
# trend (the smooth s(days):pef is significant as is the pef
# parametric term).

# can split this out by specified time periods

vcs <- calculate_vcs(input_data = hp_data, mgcv_model = gam.t, terms = c("Intercept", "pef"))
head(vcs)

# and plotted

vcs |>
  mutate(u = b_pef + se_pef,
         l = b_pef - se_pef) |>
  ggplot(aes(x = dot, y = b_pef, ymin = l, ymax = u)) +
  geom_ribbon(fill = "lightblue") +
  geom_line() +
  theme_bw() +
  xlab("Date") + ylab("pef")

## can do the same with space

gam.s <- gam(priceper~0 + Intercept + s(X,Y,by=Intercept) + s(days, by=Intercept) +
               pef + s(X,Y, by = pef),
             data = hp_data)
summary(gam.s)

# Here is is evident at pef is varying significantly over space
# (see s(X,Y):pef) but is not significant as a global fixed term.
# That is, the coefficient estimate global slope, is not significantly
# different from zero, but is when varying over space and allowing
# for Intercept to vary over space and time. This finding could be
# related to the age of the houses, which were built in clusters in
# different locations.

# can map these

# 1.over observation locations
vcs <- calculate_vcs(input_data = hp_data,
                     mgcv_model = gam.s,
                     terms = c("Intercept", "pef"))
tit <-expression(paste(""*beta[`pef`]*""))
p1 <-
  ggplot() + geom_sf(data = lb, col = "lightgrey") +
  geom_point(data = vcs, aes(x = X, y = Y, colour = b_pef), alpha = 1) +
  scale_colour_continuous_c4a_div("brewer.rd_yl_bu", name = tit) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm"),) +
  xlab("") + ylab("")
# 2. over grid - recall it needs an intercept term and a days value!
vcs <- calculate_vcs(input_data = l_grid |> mutate(Intercept = 1, days = mean(hp_data$days)),
                     mgcv_model = gam.s,
                     terms = c("Intercept", "pef"))
p2 <-
  ggplot() +
  geom_sf(data = vcs, aes(fill = b_pef), col = NA) +
  scale_fill_continuous_c4a_div("brewer.rd_yl_bu", name = tit) +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm"),) +
  xlab("") + ylab("")
plot_grid(p1, p2)

# pef varies over space, with negative values in the centre to positive
# values in the other regions

## space-time I

gam.st1 <- gam(priceper~0 + Intercept + s(X,Y,by=Intercept) + s(days, by=Intercept) +
                 pef + te(X,Y,days,d = c(2,1),bs=c('tp','cr'), by = pef),
               data = hp_data)
summary(gam.st1)

# the increasingly negative relationship of pef with the target variable
# over time is evident and the spatial distributions generally indicate
# a lower relationship with the target variable in the east of the
# study area and increasingly negative one to the west.

# calculate the varying coefficient estimates
vcs <- calculate_vcs(input_data = hp_data, mgcv_model = gam.st1, terms = c("Intercept", "pef"))
# temporal trends
p_time <-
  vcs |>
  select(dot, b_Intercept, b_pef) |>
  pivot_longer(-dot) |>
  mutate(name = recode(name,
                       "b_Intercept" = '""*beta[Intercept]',
                       "b_pef" = '""*beta[pef]')) |>
  ggplot(aes(x = dot, y = value)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  facet_wrap(~name,  labeller = label_parsed, scale = "free", ncol = 1) +
  theme_bw() + xlab("Year") + ylab("")
# spatial trends
tit <-expression(paste(""*beta[`Intercept`]*""))
p_sp1 <-
  ggplot() + geom_sf(data = lb, col = "lightgrey") +
  geom_point(data = vcs, aes(x = X, y = Y, colour = b_Intercept), alpha = 1) +
  scale_colour_continuous_c4a_seq("brewer.yl_gn_bu", name = tit) +
  theme_bw() +
  xlab("") + ylab("")
tit <-expression(paste(""*beta[`pef`]*""))
p_sp2 <-
  ggplot() + geom_sf(data = lb, col = "lightgrey") +
  geom_point(data = vcs, aes(x = X, y = Y, colour = b_pef), alpha = 1) +
  scale_colour_continuous_c4a_div("brewer.rd_yl_bu", name = tit) +
  theme_bw() +
  xlab("") + ylab("")

plot_grid(p_time, plot_grid(p_sp1, p_sp2, ncol = 1), nrow = 1, rel_widths = c(3.5,6))

# intercept and coefficient change over time

# can also split by time

# 1. create time intervals (as above)
pred_days = seq(365, 2555, 365)/100
# 2. create coefficient estimates for each time period (n = 7)
res_out <- matrix(nrow = nrow(l_grid), ncol = 0)
for (i in pred_days){
  res.i <- calculate_vcs(input_data = l_grid |> mutate(days = i),
                         mgcv_model = gam.st1,
                         terms = c("Intercept", "pef"))
  # select just the coefficient estimates of interest
  res.i <- res.i |> st_drop_geometry() |> select(starts_with("b_pef"))
  res_out <- cbind(res_out, res.i)
}

# 3. name with years and join to the grid
colnames(res_out) <- paste0("Y", "_", 2018:2024)
# define a title
tit <-expression(paste(""*beta[`pef`]*""))
l_grid |> cbind(res_out) |>
  # select the variables and pivot longer
  select(starts_with("Y_")) |>
  # rename
  rename(`2018` = "Y_2018", `2019` = "Y_2019", `2020` = "Y_2020",
         `2021` = "Y_2021", `2022` = "Y_2022", `2023` = "Y_2023", `2024` = "Y_2024") |>
  pivot_longer(-geometry) |>
  # make the new days object a factor (to enforce plotting order)
  mutate(name = factor(name, 2018:2024)) |>
  # 4. and plot
  ggplot() +
  geom_sf(aes(fill = value), col = NA) +
  # adjust default shading
  scale_fill_continuous_c4a_div(name = tit) +
  # facet
  facet_wrap(~name, ncol = 3) +
  # apply and modify plot theme
  theme_bw() +
  theme(
    legend.position = "inside",
    legend.direction = "horizontal",
    #legend.position.inside = c(0.7, 0.15),
    legend.text=element_text(size=10),
    legend.title=element_text(size=12),
    strip.background = element_rect(fill="white", colour = "white"),
    strip.text = element_text(size = 8, margin = margin(b=4)),
    legend.key.width = unit(1.5, "cm"),
    axis.title=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank())


## space time II - with separate smooths (rather than combined)

gam.st2 <- gam(priceper~0 + Intercept +
                 s(X,Y,by=Intercept) + s(days, by=Intercept) +
                 pef + s(X,Y, by = pef) + s(days, by = pef),
               data = hp_data)
summary(gam.st2)

# Here the fixed global term is again not significant, and interestingly
# neither is separate temporal smooth, as confirmed by the plots of
# the smooths.Taken together, this confirms what was found in the
# previous subsection with a combined TP smooth: the spatial trend in
# the relationship of pef with priceper that changes in intensity
# over time is confirmed, but the interaction over space does not
# change over time. This suggests a super-imposition of spatial and
# temporal trends: the temporal smooths with days is not significant.

# It suggests the need to to consider model form, and to determine how
# space and time should be included in smooths. This is done in the
# next section using an automated approach to model selection.

# This avoids super-imposing space-time trends by simply constructing
# a single space-time smooth. In the example above, the spatial effect
# was clear but was proportionately the same over time.

# 4. Working with stgam: model selection

# the stgam packages creates and compares multiple models to pick the
# best.

# AIC, AICc and BIC are used to compare models.
# For GAMs, a Generalized Cross-Validation (GCV) is recommended.
# The best model is one that minimises the GCV score.

df <- data.frame(Model = c("Time", "Space", "Space-Time I", "Space-Time II"),
                 GCV = c(gam.t$gcv.ubre, gam.s$gcv.ubre, gam.st1$gcv.ubre, gam.st2$gcv.ubre))
# rank the models
df |> arrange(GCV)

# Space-Time II gam.st2 is the best

## Model selection

# In a space-time model there are 6 options for each predictor variable:
# It is omitted.
# It is included as a parametric response with no smooth.
# It is included in parametric form and in a spatial smooth with location.
# It is included in parametric form and in a temporal smooth with time.
# It is included in parametric form and in a single space-time smooth.
# It is included in parametric form and in 2 separate space and time smooths.

# With our data These include cef (Current energy efficiency rating)
# pef (Potential energy efficiency rating) and beds (Number of bedrooms),
# as well as location (X and Y) and time (days).

# So with 3 variables, there are 5 x 6^3 models = 1080.

detectCores()-1
# I have 15 cores so let's use them!

# this code we can run in parallel:

library(doParallel)
t1 <- Sys.time()
stvc_mods <- evaluate_models(
  input_data = hp_data,
  target_var = "priceper",
  vars = c("pef", "beds"),
  coords_x = "X",
  coords_y = "Y",
  VC_type = "STVC",
  time_var = "days",
  #ncores = 2)
  ncores = 15)
Sys.time() - t1  # about 10 minutes (less with more cores!)

# 4.727089 mins

# we can extract the best models

mod_comp <- gam_model_rank(stvc_mods, n= 10)
# have a look
mod_comp |> select(-f)

# top 7 all have space and time for each predictor variable, but in
# different combinations of smooths
# pef s_T + s_S - separate smooths
# pef t2_ST = combined smooths

# we can pick out the best model

f <- as.formula(mod_comp$f[1])
f

# included the Intercept in a single space-time TP smooth
# separate space and time smooths for pef and beds.

# Intercept - 1 + te(X, Y, days, d = c(2, 1), bs = c("tp","cr"), by = Intercept)
# te(X, Y, days, d = c(2, 1), bs = c("tp","cr"), by = pef)
# s(X, Y, by = beds) + s(days, by = beds)

# specify the model
gam.m <- gam(f, data = hp_data, method = "REML")
# check k
k.check(gam.m)

# A summary of the model can be examined and this shows that nearly
# all of the terms are significant except the the temporal smooth for
# beds.

summary(gam.m)

# there are various ways of modelling the output

vcs <- calculate_vcs(input_data = hp_data,
                     mgcv_model = gam.m,
                     terms = c("Intercept", "pef", "beds"))

vcs |> select(starts_with("b_")) |>
  apply(2, summary) |> round(1)

# over time

vcs |>
  select(dot, starts_with("b_")) |>
  rename(`Intercept` = b_Intercept,
         `Potential Energy Efficiency` = b_pef,
         `Bedrooms` = b_beds) |>
  pivot_longer(-dot) |>
  mutate(name = factor(name,
                       levels=c("Intercept","Potential Energy Efficiency", "Bedrooms"))) |>
  group_by(dot, name) |>
  summarise(
    lower = quantile(value, 0.25),
    median = median(value),
    upper = quantile(value, 0.75)
  ) |>
  ggplot(aes(x = dot, y = median)) +
  geom_point(col = "blue", alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~name, scale = "free_y") +
  theme_bw() + xlab("") + ylab("") +
  theme(strip.background = element_rect(fill="white"))
# `summarise()` has grouped output by 'dot'. You can override using the `.groups` argument.
# `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
#
# and space
# make spatial data

vcs_sf <-
  vcs |>
  st_as_sf(coords = c("X", "Y"), remove = F)
# plot
ggplot()  +
  geom_sf(data = lb) +
  geom_sf(data = vcs_sf, aes(col = b_pef)) +
  scale_colour_continuous_c4a_div(palette="brewer.rd_yl_bu",
                                  name = "Potential\nEnergy Efficiency") +
  facet_wrap(~yot) +
  theme_bw() +
  theme(
    legend.position = "inside",
    legend.direction = "horizontal",
    #legend.position.inside = c(0.7, 0.15),
    legend.text=element_text(size=10),
    legend.title=element_text(size=12),
    strip.background = element_rect(fill="white", colour = "white"),
    strip.text = element_text(size = 8, margin = margin(b=4)),
    legend.key.width = unit(1.5, "cm"),
    axis.title=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank())

# and apply this to a grid (whole of london)

# create time slices
years <- 2018:2024
# calculate over the grid for each time slice
res_out <- matrix(nrow = nrow(l_grid), ncol = 0)
for (i in 1:length(years)){
  # convert years to days
  day.val = (years[i]-2018) * 365 / 100
  res.i <- calculate_vcs(input_data = l_grid |> mutate(days = day.val),
                         mgcv_model = gam.m,
                         terms = c("Intercept", "pef", "beds"))
  # select all the coefficient estimates
  res.i <-
    res.i |>
    st_drop_geometry() |>
    select(starts_with("b_"),
           starts_with("se_"))
  # rename them
  names(res.i) <- paste0(names(res.i), "_", years[i])
  # bind to the result
  res_out <- cbind(res_out, res.i)
  cat(years[i], "\t")
}
#> 2018     2019    2020    2021    2022    2023    2024
# title
tit <-expression(paste(""*beta[`beds`]*""))
# join to the grid
l_grid |> cbind(res_out) |>
  # select the variables and pivot longer
  select(starts_with("b_pef")) |>
  # rename
  rename(`2018` = "b_pef_2018", `2019` = "b_pef_2019",
         `2020` = "b_pef_2020", `2021` = "b_pef_2021",
         `2022` = "b_pef_2022", `2023` = "b_pef_2023",
         `2024` = "b_pef_2024") |>
  pivot_longer(-geometry) |>
  # make the new days object a factor (to enforce plotting order)
  mutate(name = factor(name, levels = 2018:2024)) |>
  # 4. and plot
  ggplot() +
  geom_sf(aes(fill = value), col = NA) +
  # adjust default shading
  scale_fill_continuous_c4a_div("brewer.rd_yl_bu", name = tit) +
  # facet
  facet_wrap(~name, ncol = 3) +
  # apply and modify plot theme
  theme_bw() +
  theme(
    legend.position = "inside",
    legend.direction = "horizontal",
    #legend.position.inside = c(0.7, 0.15),
    legend.text=element_text(size=10),
    legend.title=element_text(size=12),
    strip.background = element_rect(fill="white", colour = "white"),
    strip.text = element_text(size = 8, margin = margin(b=4)),
    legend.key.width = unit(1.5, "cm"),
    axis.title=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank())


# The maps reflect the relatively small changes in the relationship
# with priceper over time plotted above.

# we can evaluate multiple GCV models
# but this is based on exploratory analysis in section 3
# looking at the relationships and whether they vary of space, time
# or both.

