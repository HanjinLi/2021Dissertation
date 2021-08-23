library(tidyverse)
library(readxl)
library(PerformanceAnalytics)
library(lubridate)
library(car)
library(MASS)
library(leaps)
library(spdep)
library(sf)
library(spgwr)
library(tree)
library(randomForest)
library(gbm)
library(e1071)
library(Metrics)
library(dlookr)
library(tmap)
library(GD)
library(sjPlot)
library(modelsummary)

##Data processing
df <- read_csv('./AoLR Data collection 2017.csv',col_types = cols(`1st pump attendance` = col_character(),
                                                                       `2nd pump attendance` = col_character(),
                                                                       `3rd pump attendance` = col_character()))
head(df)
names(df)
#delete the space in dataset
names(df) <- gsub(" ","",names(df))
head(df)

##EDA
str(df)
describe(df)

tab_df(describe(df)[1:14,c('variable','mean','sd','p00','p100')])
#We calculate the total number of fires and the average response time
#convert the time(mintues to seconds)
df <- df %>% rowwise() %>% mutate(NumberofAccidents=sum(c_across('pumpfires':'2+pumpspecialservices')),
                                  `1stpumpattendance`=ms(`1stpumpattendance`) %>% seconds() %>% as.integer(),
                                  `2ndpumpattendance`=ms(`2ndpumpattendance`) %>% seconds() %>% as.integer(),
                                  `3rdpumpattendance`=ms(`3rdpumpattendance`) %>% seconds() %>% as.integer(),
                                  MeanAttedence=mean(c_across("1stpumpattendance":"3rdpumpattendance"),na.rm=T)
)
head(df)

df <- df %>% dplyr::select(Boroughcode:Smokers,NumberofAccidents:MeanAttedence) %>% tibble()
df

#Use Shapiro-Wilk test to check normality of variables
dlookr::normality(df)
tab_df(dlookr::normality(df))
plot_normality(df)

#spatial join, draw spatial distribution map
shp <- st_read('./statistical-gis-boundaries-london/ESRI/London_Ward.shp')
df_sf <- inner_join(shp,df,by=c('GSS_CODE'='WardCode'))
#st_write(df_sf,'data/df_sf.shp')

tm_popu <- tm_shape(df_sf) +
  tm_polygons('Population') +
  tm_layout(legend.position = c(0.8,0.05),
            bg.color = 'white')

tmap_save(tm_popu,'./report/population.png')

tm_names <- names(df_sf)[11:24]
for (i in tm_names) {
  tm_out <- tm_shape(df_sf) +
    tm_polygons(i) +
    tm_layout(legend.position = c(0.8,0.05),
              bg.color = 'white')
  tmap_save(tm_out,paste0('./report/',i,'.png'))
}

#NumberofAccidents and MeanAttedence polygons
tm_popu <- tm_shape(df_sf) +
  tm_polygons('NumberofAccidents') +
  tm_layout(legend.position = c(0.8,0.05),
            bg.color = 'white')
tm_popu
tmap_save(tm_popu,'./report/NumberofAccidents.png')

tm_popu <- tm_shape(df_sf) +
  tm_polygons('MeanAttedence') +
  tm_layout(legend.position = c(0.8,0.05),
            bg.color = 'white')
tm_popu
tmap_save(tm_popu,'./report/MeanAttedence.png')



##Correlation analysis
df %>% dplyr::select(Population:Smokers,NumberofAccidents) %>% 
  chart.Correlation(histogram=TRUE, pch=19)

#scatter diagram
par(mfrow=c(2,2))
plot(df$Population, df$NumberofAccidents,xlab='Population',ylab='NumberofAccidents')
abline(lm(df$NumberofAccidents~df$Population))

plot(df$HRbuildings, df$NumberofAccidents,xlab='HRbuildings',ylab='NumberofAccidents')
abline(lm(df$NumberofAccidents~df$HRbuildings))

plot(df$crime, df$NumberofAccidents,xlab='Crime',ylab='NumberofAccidents')
abline(lm(df$NumberofAccidents~df$crime))

plot(df$Smokers, df$NumberofAccidents,xlab='Smokers',ylab='NumberofAccidents')
abline(lm(df$NumberofAccidents~df$Smokers))

#Analyze the correlation between response time and each factor
df %>% dplyr::select(Population:Smokers,MeanAttedence) %>% 
  chart.Correlation(histogram=TRUE, pch=19)

par(mfrow=c(1,2))
plot(df$Density, df$MeanAttedence,xlab='Density',ylab='MeanAttedence')
abline(lm(df$MeanAttedence~df$Density))

plot(df$`65+age`, df$MeanAttedence,xlab='65+age',ylab='MeanAttedence')
abline(lm(df$MeanAttedence~df$`65+age`))


##OLS
fm <- NumberofAccidents ~ Population + `Deprivation-2015` + HRbuildings + Heritage + localconcerns + crime + Nocentralheat + Smokers
fit_NumberofAccidents <- lm(fm,data = df)
summary(fit_NumberofAccidents)

#multicollinearity
modelsummary(fit_NumberofAccidents)

vif(fit_NumberofAccidents)
tab_df(vif(fit_NumberofAccidents))

sqrt(vif(fit_NumberofAccidents)) >2 

fit_NumberofAccidents_step<-step(fit_NumberofAccidents)
summary(fit_NumberofAccidents_step)

#find best model
models <- list(
  "OLS 1"     = lm(NumberofAccidents ~ Population + `Deprivation-2015` + HRbuildings + 
                     Heritage + localconcerns + crime + Nocentralheat + Smokers, data = df),
  "OLS 2"     = lm(NumberofAccidents ~ Population + `Deprivation-2015` + HRbuildings + 
                     Heritage + localconcerns + crime + Smokers, data = df),
  "OLS 3"     = lm(NumberofAccidents ~ Population + `Deprivation-2015` + HRbuildings + 
                     Heritage + localconcerns + crime, data = df)
)

modelsummary(models,statistic = "{std.error} ({p.value})",output = "ols_table.docx")

fit_leaps <- regsubsets(fm,data = df)
plot(fit_leaps,scale='adjr2')

#final model
fm <- NumberofAccidents ~ Population + `Deprivation-2015` + HRbuildings + Heritage + localconcerns + crime  + Smokers
fit_NumberofAccidents <- lm(fm,data = df_sf)
summary(fit_NumberofAccidents)

par(mfrow=c(2,2))
plot(fit_NumberofAccidents)

par(mfrow=c(2,2))
#HRbuildings、Heritage、crime, scatter plot
plot(df$HRbuildings %>% sqrt(), df$NumberofAccidents)
plot(df$Heritage %>% sqrt(), df$NumberofAccidents)
plot(df$crime %>% sqrt(), df$NumberofAccidents)
#lm after sqrt（HRbuildings、Heritage、crime）
df$HRbuildings_sq <- df$HRbuildings %>% sqrt()
df$Heritage_sq <- df$Heritage %>% sqrt()
df$crime_sq <- df$crime %>% sqrt()
df_sf <- inner_join(shp,df,by=c('GSS_CODE'='WardCode'))
fm_sqrt <- NumberofAccidents ~ Population + `Deprivation-2015` + HRbuildings_sq + 
  Heritage_sq + localconcerns + crime_sq + Smokers

fit_NumberofAccidents_sqrt <- lm(fm_sqrt,data = df_sf)
summary(fit_NumberofAccidents_sqrt)

par(mfrow=c(2,2))
plot(fit_NumberofAccidents_sqrt)

#lm D-W test
durbinWatsonTest(fit_NumberofAccidents) %>% 
  tab_df()

#Moran test of OLS residuals
df_sf$ols_residuals <- residuals(fit_NumberofAccidents)
df_sf$ols_predicted <- predict(fit_NumberofAccidents)
nb <- poly2nb(df_sf,queen = T)
lw <- nb2listw(nb, style="B", zero.policy=TRUE)
#moran.test(df_sf$ols_residuals, lw)
lm.morantest(fit_NumberofAccidents, lw, alternative="two.sided")

lm.morantest(fit_NumberofAccidents, lw, alternative="two.sided") %>% tab_df()

#The spatial distribution of the NumberofAccidents variable and the residual  of the lm model.
tm_shape(df_sf) +
  tm_polygons("NumberofAccidents") +
  tm_layout(legend.position = c(0.85,0.1))
#tm_scale_bar(position = c("left", "bottom")) +
#tm_compass(position = c("right", "top"))

tm_shape(df_sf) +
  tm_polygons("ols_residuals") +
  tm_layout(legend.position = c(0.85,0.1))
#tm_scale_bar(position = c("left", "bottom")) +
#tm_compass(position = c("right", "top"))


##Hotspot
tm_shape(df_sf) +
  tm_polygons("NumberofAccidents") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "top"))

set.ZeroPolicyOption(TRUE)
localG <- localG(df_sf$NumberofAccidents, listw=lw, zero.policy=T)
df_sf$localG <- localG

tm_shape(df_sf) +
  tm_polygons("localG",palette=c('blue','red'),size=16)
#tm_graticules(lines=F) +
#tm_compass(position=c("right", "top"))+
#tm_scale_bar(position=c("left", "bottom"))

##GWR
df_sp <- as(df_sf, 'Spatial')
fm_gwr <- NumberofAccidents ~ Population + Deprivation.2015 + HRbuildings + Heritage + localconcerns + crime  + Smokers
GWRbandwidth <- gwr.sel(fm_gwr, data=df_sp, adapt=T, method="cv")

GWRModel <- gwr(fm_gwr, data=df_sp, adapt=GWRbandwidth, hatmatrix = TRUE,se.fit = TRUE) #run the gwr model
GWRModel

GWR_re <- GWRModel$SDF %>% as('sf')
df_sf$localR2 <- GWR_re$localR2
#GWR_all <- GWR_re %>% dplyr::select(localR2) %>% st_drop_geometry() %>% (df_sf)

#GWR residual
tm_shape(GWR_re) +
  tm_polygons("gwr.e",title ="GWR_Residuals") +
  tm_layout(legend.position = c(0.85,0.1))
#tm_scale_bar(position = c("left", "bottom")) +
#tm_compass(position = c("right", "top"))

#localR2
tm_shape(GWR_re) +
  tm_polygons("localR2",title ="GWR_LocalR2") 
#tm_scale_bar(position = c("left", "bottom")) +
#tm_compass(position = c("right", "top"))

#localR2 greater than 0.8
GWR_re$ifover08 <- GWR_re$localR2 >=0.8
tm_shape(GWR_re) +
  tm_polygons("ifover08",title ="LocalR2 greater \nthan 0.8?")+
  tm_layout(legend.position = c(0.8,0.1))

df_sf %>% filter(localR2 <= 0.80) -> GWR_re_0.8

df_sf %>% filter(localR2 > 0.80) -> GWR_re_over_0.8

#<0.8
GWR_re_0.8 %>% st_drop_geometry() %>% dplyr::select(Population:NumberofAccidents) %>% 
  dplyr::select(Population,`Deprivation-2015`,HRbuildings,Heritage,localconcerns,crime,Smokers,NumberofAccidents) %>% 
  chart.Correlation(histogram=TRUE, pch=19)
#over 0.8
GWR_re_over_0.8 %>% st_drop_geometry() %>% dplyr::select(Population:NumberofAccidents) %>% 
  dplyr::select(Population,`Deprivation-2015`,HRbuildings,Heritage,localconcerns,crime,Smokers,NumberofAccidents) %>%
  chart.Correlation(histogram=TRUE, pch=19)


##analysis of average response time
#OLS
fm_time <- MeanAttedence ~ Density + `65+age`
df_time <- df %>% drop_na()
lm_MeanAttedence <- lm(fm_time,data = df_time)
summary(lm_MeanAttedence)

#check spatial autocorrelation
df_sf_time <- residuals(lm_MeanAttedence) %>% names() %>% as.integer() %>% df_time[.,]
df_sf_time <- right_join(shp,df_time,by=c('GSS_CODE'='WardCode'))
df_sf_time$ols_re_time <- residuals(lm_MeanAttedence)
df_sf_time <- df_sf_time %>% .[!st_is_empty(.),]

nb_time <- poly2nb(df_sf_time)
lw_time <- nb2listw(nb_time, style="B", zero.policy=TRUE)

moran.test(df_sf_time$ols_re_time, lw_time)
#spatial lag
lag_time <- lagsarlm(fm_time, data=df_sf_time, lw_time)
summary(lag_time)
#spatial error
err_time <- errorsarlm(fm_time, data=df_sf_time, lw_time)
summary(err_time)

models_time <- list(
  "OLS"     = lm(fm_time,data = df_time),
  "lagsarlm"  = lagsarlm(fm_time, data=df_sf_time, lw_time),
  "errorsarlm"     = errorsarlm(fm_time, data=df_sf_time, lw_time)
)

modelsummary(models_time,statistic = "{std.error} ({p.value})",output = "model_time_table.docx")



##Geodetector
#gobal factor detector
#number of fires
df_gd <- df %>% data.frame()
fm_gd <- NumberofAccidents ~ Population + Deprivation.2015 + HRbuildings + 
  Heritage + localconcerns + crime + Smokers
fire_gd <- gd(fm_gd, data = df_gd)
fire_gd$Factor  %>% mutate(qv=round(qv,3),
                           sig=round(sig,3)) %>% 
  tab_df()

#response time
fm_gd_time <- MeanAttedence ~ Population + Deprivation.2015 + HRbuildings +
  Heritage + localconcerns + crime + Smokers
time_gd <- gd(MeanAttedence ~., data = df_gd)
time_gd$Factor  %>% mutate(qv=round(qv,3),
                           sig=round(sig,3)) %>%
  tab_df()

#local factor detector
#number of fires
tm_shape(shp) +
  tm_polygons('BOROUGH') +
  tm_layout(legend.outside = T)

#response time q value
my_gd_time <- function(df){
  gd_re <- gd(MeanAttedence ~., data = df_gd)
  re <- gd_re$Factor %>% .[which.max(.$qv),] %>% data.frame()
  return(re)
}

gd_all_time <- NULL
for (i in df_gd$Boroughname %>% unique(.)) {
  idf <- df_gd %>% filter(Boroughname==i) %>% my_gd_time(.)
  if(nrow(idf)!=0)
    idf <- data.frame(idf,Borough=i)
  gd_all_time <- bind_rows(gd_all_time,idf)
}

gd_all_time %>% tab_df()

my_gd <- function(df){
  gd_re <- gd(fm_gd, data = df)
  re <- gd_re$Factor %>% .[which.max(.$qv),] %>% data.frame()
  return(re)
}

gd_all <- NULL
for (i in df_gd$Boroughname %>% unique(.)) {
  idf <- df_gd %>% filter(Boroughname==i) %>% my_gd(.)
  if(nrow(idf)!=0)
    idf <- data.frame(idf,Borough=i)
  gd_all <- bind_rows(gd_all,idf)
}

gd_all %>% tab_df()

#plot the the most influence factor in each boroughs
borough <- st_read('./statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp')
gd_all_borough <- left_join(borough,gd_all,by=c('NAME'='Borough'))
tm_shape(gd_all_borough) +
  tm_polygons('variable',title='Variable with max q') +
  tm_text('variable') +
  tm_layout(legend.position = c(0.8,0.05))

gd_all_time_borough <- left_join(borough,gd_all_time,by=c('NAME'='Borough'))
tm_shape(gd_all_time_borough) +
  tm_polygons('variable',title='Variable with max q') +
  tm_text('variable') +
  tm_layout(
    legend.outside = T)
