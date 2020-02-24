## Libraries
library(tidyverse)
library(magrittr)
library(doParallel)
library(countrycode)
library(plotly)

### Read in line list data from (https://github.com/CSSEGISandData/COVID-19)-----------
##Note these links may change!
#Cases
# coronaData_c <- read.csv('https://raw.githubusercontent.com/moritz-wagner/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv',
#                          check.names = FALSE)
# #Deaths
# coronaData_d <- read.csv('https://raw.githubusercontent.com/moritz-wagner/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv',
#                          check.names = FALSE)
# #Recovereds
# coronaData_r <- read.csv('https://raw.githubusercontent.com/moritz-wagner/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv',
#                          check.names = FALSE)

#Cases
coronaData_c <- read.csv('COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv',
                         check.names = FALSE)
#Deaths
coronaData_d <- read.csv('COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv',
                         check.names = FALSE)
#Recovereds
coronaData_r <- read.csv('COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv',
                         check.names = FALSE)

coronaData_c %<>% gather("date","confirmed",-c(1:5)) %>% mutate_if(is.numeric, ~replace_na(., 0))
coronaData_r %<>% gather("date","recovered",-c(1:5)) %>% mutate_if(is.numeric, ~replace_na(., 0))
coronaData_d %<>% gather("date","death",-c(1:5)) %>% mutate_if(is.numeric, ~replace_na(., 0))

coronaData <- full_join(coronaData_c[-c(3:5)],coronaData_d[-c(3:5)])
coronaData <- full_join(coronaData,coronaData_r[-c(3:5)])
#Dateformats
coronaData$date <- as.POSIXct(strptime(coronaData$date,format="%m/%d/%y"))
colnames(coronaData)[c(1,2)] <- c("Province.State","Country.Region")

##Only need latest date and countries outside of China 
coronaData %>% 
  mutate(isChina = (str_detect(Country.Region,
                               "China"))) %>% 
  subset(!isChina) %>% 
  group_by(Country.Region) %>% 
  mutate(latest_date=max(date)) %>% subset(date==latest_date) %>% 
  #Counting all cases, so deaths and recovereds as well
  ungroup() %>% mutate(cases=confirmed+death+recovered) %>% 
  group_by(Country.Region) %>% summarise(cases=sum(cases)) -> coronaData_exports

#Check current number of exported cases
sum(coronaData_exports$cases)


##Get MOBS data on relative risk of importation from China (https://www.mobs-lab.org/2019ncov.html)----------
data_flights <- read.csv('epirisk.csv')

##Get GHS data--------
data_ghs <- read.csv('ghs_index.csv',header = FALSE,col.names = c('Country','value'))

#dataset
df <- coronaData_exports %>% select(Country.Region,cases)
colnames(df) <- c('Country','Cases_lm')
df$Country <- as.character(df$Country)
#Standardise country names for all datasources
df$Country <- countrycode(df$Country,origin = 'country.name',destination = 'country.name')
data_flights$label <- countrycode(data_flights$label,origin = 'country.name',destination = 'country.name')
data_ghs$Country <- countrycode(data_ghs$Country,origin = 'country.name',destination = 'country.name')
#Check countries that don't don't exist in all datasets: Macau, Hong Kong, Cruise ship (NA), Taiwan
df$Country[!df$Country%in%data_flights$label]
df$Country[!df$Country%in%data_ghs$Country]
data_ghs$Country[!data_ghs$Country%in%data_flights$label]
data_flights$label[!data_flights$label%in%data_ghs$Country]

#Combine datasets to use RR instead of passengers for regression
df <- cbind(df,data_flights[match(df$Country,data_flights$label),c('population','risk')])
df <- cbind(df,ghs=data_ghs[match(df$Country,data_ghs$Country),c('value')])
df %>% subset(!is.na(risk)) -> df


##Inital plot
df %>% ggplot(aes(x=risk,y=Cases_lm,colour=ghs))+geom_point()+
  geom_text(aes(x=risk,y=Cases_lm, label=Country), hjust=0.5 , vjust=-0.8, size=3.8)+
  ylab("Reported cases")+
  xlab("Relative risk of importation")


##Regression analysis with new data as done before (https://www.medrxiv.org/content/10.1101/2020.02.04.20020495v1.full.pdf)------------
pkgs <- c('doParallel', 'foreach')
lapply(pkgs, require, character.only = T)
registerDoParallel(cores = 4)

# functions
# inspired by: https://stackoverflow.com/questions/17922637/prediction-intervals-for-poisson-regression-on-r
boot_pi <- function(model, pdata, n, p) {
  odata <- model$data
  lp <- (1 - p) / 2
  up <- 1 - lp
  set.seed(2020)
  seeds <- round(runif(n, 1, 1000), 0)
  boot_y <- foreach(i = 1:n, .combine = rbind) %dopar% {
    set.seed(seeds[i])
    bdata <- odata[sample(seq(nrow(odata)), size = nrow(odata), replace = TRUE), ]
    bpred <- predict(update(model, data = bdata), type = "response", newdata = pdata)
    rpois(length(bpred), lambda = bpred)
  }
  boot_ci <- t(apply(boot_y, 2, quantile, c(lp, up)))
  return(data.frame(pred = predict(model, newdata = pdata, type = "response"), lower = boot_ci[, 1], upper = boot_ci[, 2]))
}



# fit the data
mdl <- glm(Cases_lm ~ log(risk), data = df, family=poisson( link="log" ) )
mdl <- glm(Cases_lm ~ log(risk) + ghs , data = df, family=poisson( link="log" ) )
#Add new data for prediction, which includes countries that have yet to see importations
# new_data <- df
df$exported <- '1'
df2 <- data_flights[!data_flights$label%in%df$Country,c(4:6)]
colnames(df2)[1] <- "Country"
df2$Cases_lm <- 0
df2$exported <- '0'
df2$ghs <- data_ghs[match(df2$Country,data_ghs$Country),"value"]
df2 <- bind_rows(df,df2) %>% na.omit()
new_data <- df2

# compute the prediction intervals 1---------
PI<-boot_pi(mdl, new_data, 50000, 0.95)

# visualisation
df2$fit<-PI$pred
df2$lwr<-PI$lower
df2$upr<-PI$upper

df2 %>% 
  # subset(exported==1) %>%
  ggplot()+
  geom_point(aes(x=risk,y=Cases_lm,color=exported),size=3, alpha=1)+
  geom_line(aes(x=risk,y=fit),size=1, colour="grey80")+
  geom_line(aes(x=risk,y=lwr),size=1, colour="darkred", linetype=2)+
  geom_line(aes(x=risk,y=upr),size=1, colour="grey80", linetype=2)+
  geom_text(aes(x=risk,y=Cases_lm, label=Country), hjust=0.5 , vjust=-0.8, size=3.8)+
  ylab("Reported cases")+
  xlab("RR of importation")

# Add continents
# df$Continent <- countrycode(sourcevar = df[, "Country"],
#                             origin = "country.name",
#                             destination = "continent")

df2$Continent <- countrycode(sourcevar = df2[, "Country"],
                             origin = "country.name",
                             destination = "continent")

df2 %>% 
  subset(exported==1 | Continent%in%'Africa') %>% 
  ggplot(aes(x=reorder(Country,risk)))+
  geom_bar(aes(y=Cases_lm),alpha=.5,stat = "identity")+
  geom_pointrange(aes(y=fit,ymin=lwr,ymax=upr,color=Continent))+
  coord_flip()+
  ggtitle("Reported exported cases  vs\n95% CI of expected exported cases")+
  theme(axis.title.y = element_blank())+
  ylab("Cases")


df2 %>% 
  subset(Continent%in%'Africa') %>% 
  ggplot(aes(x=reorder(Country,risk)))+
  geom_bar(aes(y=Cases_lm),alpha=.5,stat = "identity")+
  geom_pointrange(aes(y=fit,ymin=lwr,ymax=upr,color=Continent))+
  coord_flip()+
  ggtitle("Reported exported cases  vs\n95% CI of expected exported cases")+
  theme(axis.title.y = element_blank())+
  ylab("Expected cases")+theme_classic()#+ylim(0,10)


df2 %>% 
  ggplot(aes(x=reorder(Country,risk)))+
  geom_bar(aes(y=Cases_lm),alpha=.5,stat = "identity")+
  geom_pointrange(aes(y=fit,ymin=lwr,ymax=upr,color=Continent))+
  coord_flip()+
  ggtitle("Reported exported cases  vs\n95% CI of expected exported cases\n(whole world)")+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())


## Create predictions assuming countries had top ghs index (US)
#Add new data for prediction, which includes countries that have yet to see importations
# new_data <- df
df$exported <- '1'
df3 <- data_flights[!data_flights$label%in%df$Country,c(4:6)]
colnames(df3)[1] <- "Country"
df3$Cases_lm <- 0
df3$exported <- '0'
df3 <- bind_rows(df,df3) 
##Set ghs of all predictions to top (equivalent to US)
df3$ghs <- sort(data_ghs$value,decreasing = TRUE)[1]
new_data <- df3 %>% na.omit()

# compute the prediction intervals 2----------
PI2<-boot_pi(mdl, new_data, 50000, 0.95)

# visualisation
df3$fit2<-PI2$pred
df3$lwr2<-PI2$lower
df3$upr2<-PI2$upper

colnames(df3)[5] <- 'ghs2'
df3 <- full_join(df3,df2)

df3 %>% 
  # subset(exported==1) %>%
  ggplot()+
  geom_point(aes(x=risk,y=Cases_lm,color=exported), size=3 ,alpha=1)+
  geom_line(aes(x=risk,y=fit2),size=1, colour="grey80")+
  geom_line(aes(x=risk,y=lwr2),size=1, colour="darkred", linetype=2)+
  geom_line(aes(x=risk,y=upr2),size=1, colour="grey80", linetype=2)+
  geom_text(aes(x=risk,y=Cases_lm, label=Country), hjust=0.5 , vjust=-0.8, size=3.8)+
  ylab("Reported cases")+
  xlab("RR of importation")

# Add continents
df3$Continent <- countrycode(sourcevar = df3[, "Country"],
                             origin = "country.name",
                             destination = "continent")

df3 %>% 
  subset(exported==1 | Continent%in%'Africa') %>% 
  ggplot(aes(x=reorder(Country,risk)))+
  geom_bar(aes(y=Cases_lm),alpha=.5,stat = "identity")+
  geom_pointrange(aes(y=fit,ymin=lwr,ymax=upr,color=Continent))+
  geom_pointrange(aes(y=fit2,ymin=lwr2,ymax=upr2,color=Continent),linetype=2)+
  coord_flip()+
  ggtitle("Reported exported cases  vs\n95% CI of expected exported cases")+
  theme(axis.title.y = element_blank())+
  ylab("Cases")


df3 %>% 
  subset(Continent%in%'Africa') %>% 
  ggplot(aes(x=reorder(Country,risk)))+
  geom_bar(aes(y=Cases_lm),alpha=.5,stat = "identity")+
  geom_pointrange(aes(y=fit,ymin=lwr,ymax=upr),color="red")+
  geom_crossbar(aes(y=fit2,ymin=lwr2,ymax=upr2))+
  coord_flip()+
  ggtitle("Reported exported cases  vs\n95% CI of expected exported cases")+
  theme(axis.title.y = element_blank())+
  ylab("Cases")+theme_classic()#+ylim(0,10)


df3 %>% 
  ggplot(aes(x=reorder(Country,risk)))+
  geom_bar(aes(y=Cases_lm),alpha=.5,stat = "identity")+
  geom_pointrange(aes(y=fit,ymin=lwr,ymax=upr,color=Continent))+
  # geom_pointrange(aes(y=fit,ymin=lwr,ymax=upr),color="red")+
  geom_crossbar(aes(y=fit2,ymin=lwr2,ymax=upr2))+
  coord_flip()+
  ggtitle("Reported exported cases  vs\n95% CI of expected exported cases\n(whole world)")+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())->p
ggplotly(p)


write_csv(df2,'/Users/mwagner/Documents/PhD/PIPS/work/ncov/intro/importation risk/risk.csv')


##Add map data------------
library(rnaturalearth)
library(mapview)
library(leaflet)

worldmap <- ne_countries(returnclass = "sf")
worldmap$Country <- countrycode(worldmap$name,origin = 'country.name',destination = 'country.name')

df3 %<>% mutate(exp.obs.cases=paste0(round(fit,2),' (',round(lwr,2),'-',round(upr,2),')'),
                exp.cases=paste0(round(fit2,2),' (',round(lwr2,2),'-',round(upr2,2),')'))

map.plot <- full_join(worldmap,df3)
map.plot %<>% select(c('name','Cases_lm','fit','fit2','exp.obs.cases','exp.cases','continent')) %>% mutate_if(is.numeric,function (x) round(x,digits=2))
mapview(map.plot,zcol = "fit2",col.regions=colorRampPalette(c('green','orange', 'red')))
map.plot %>% subset(continent=="Africa") %>% 
  mapview(zcol = "fit2",col.regions=colorRampPalette(c('green','orange', 'red')))
# spdf_africa %>% subset(continent=="Africa") -> Africa

mapviewOptions(default = TRUE)
Africa$logrisk <- log(Africa$risk)
Africa %>% mutate(risk=ifelse(is.na(risk),"missing",risk))
mapview(Africa,zcol = "risk",col.regions=colorRampPalette(c('green','orange', 'red')),legend=FALSE) -> m

mapshot(m,file = 'africa_map.png')


##Leaflet
# Use leaflet to display a blank map with OpenStreetMap imagery 
map <-leaflet() %>%                           ## using the pipe operator to chain operations together: here the blank map is chained to 
  addProviderTiles(providers$OpenStreetMap)   ## the first leaflet option we are applying
map

map.plot.leaflet <- map %>%                               
  addPolygons(data=map.plot)
map.plot.leaflet

My_gradient <- c("white", "brown1" )       ## brown1 is actually red
palfit <-  colorBin(My_gradient, domain = map.plot$Cases_lm, bins = c(0,.1,1,5,10,max(map.plot$fit,na.rm = TRUE)))  
palfit2 <-  colorBin(My_gradient, domain = map.plot$fit2, bins = c(0,.1,1,5,10,max(map.plot$fit,na.rm = TRUE)))
palfit3 <-  colorBin(My_gradient, domain = map.plot$fit, bins = c(0,.1,1,5,10,max(map.plot$fit,na.rm = TRUE)))  

# palfit <-  colorNumeric(My_gradient, domain = map.plot$Cases_lm)  
# palfit2 <-  colorNumeric(My_gradient, domain = map.plot$fit2)
# palfit3 <-  colorNumeric(My_gradient, domain = map.plot$fit)

Map_Nepal <- map %>%                             ## use the pipe operator to add elements 
  ## to the OpenStreet base map      
  addPolygons(data = map.plot,                       ## Use data from the spatial polygons dataframe Nepal2
              color = 'black',                   ## line color    
              weight =2,                         ## line width
              opacity = 1,                       ## line opacity
              fillColor =  ~palfit(fit),      ## fill polygons using the palette we generated
              fillOpacity = 1,                   ## opaque fill
              group = "Expected observed number of cases")         ## allows this element to be linked e.g. to legend           
Map_Nepal

Map_Nepal <- Map_Nepal %>%
  ## E.g. add the "Number of cases" group to the layers panel.   
  addLayersControl(overlayGroups = c("Expected observed number of cases")) %>%   
  ## Add a Legend  
  addLegend(pal = palfit,                           ##  use same palette as for polygons     
            values = map.plot$fit,                    ##  use same values 
            opacity = 1,                              ##  self-explanatory
            title = "Expected observed number of cases",                ##  "
            position = "bottomleft",                  ##  "
            group = "Expected observed number of cases",                ## links this legend with the polygons
            labFormat = labelFormat(digits = 0))      ## define decimal places     
Map_Nepal

Map_Nepal <- Map_Nepal %>%
  addPolygons(data = map.plot, 
              weight = 2, 
              opacity = 1,
              color = 'black',
              fillColor =  ~palfit2(fit2),     ## colour by rate
              fillOpacity = 1,
              group = "Expected number of cases") %>%             ## new group
  
  # Layers control
  addLayersControl(overlayGroups = c("Expected number of cases", "Expected observed number of cases"),            ## as before but with 2 groups
                   options = layersControlOptions(collapsed = FALSE))%>%  ## this keeps the panel expanded
  # Legend
  addLegend(pal = palfit2, 
            values = map.plot$fit2, 
            opacity = 1, 
            title = "Expected number of cases",
            position = "bottomleft", 
            group = "Expected number of cases",
            labFormat = labelFormat(digits = 0))

Map_Nepal
