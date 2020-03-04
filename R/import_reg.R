## Libraries
library(tidyverse)
library(magrittr)
library(doParallel)
library(countrycode)
library(plotly)

### Read in line list data from (https://github.com/CSSEGISandData/COVID-19)-----------
#Cases
coronaData_c <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv',
                         check.names = FALSE)
#Deaths
coronaData_d <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv',
                         check.names = FALSE)
#Recovereds
coronaData_r <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv',
                         check.names = FALSE)

# #Cases
# coronaData_c <- read.csv('data/time_series_19-covid-Confirmed.csv',
#                          check.names = FALSE)
# #Deaths
# coronaData_d <- read.csv('data/time_series_19-covid-Deaths.csv',
#                          check.names = FALSE)
# #Recovereds
# coronaData_r <- read.csv('data/time_series_19-covid-Recovered.csv',
#                          check.names = FALSE)

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
data_flights <- read.csv('data/epirisk.csv')

##Get GHS data--------
data_ghs <- read.csv('data/ghs_index.csv',header = FALSE,col.names = c('Country','value'))

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


##Regression analysis with new data as done before (https://github.com/c2-d2/cov19flightimport)------------
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
mdl <- glm(Cases_lm ~ log(risk) + ghs , data = df, family=poisson( link="log" ) )

# compute the prediction intervals including countries that have yet to see cases assuming each country's given GHS index---------
df$exported <- '1'
df2 <- data_flights[!data_flights$label%in%df$Country,c(4:6)]
colnames(df2)[1] <- "Country"
df2$Cases_lm <- 0
df2$exported <- '0'
df2$ghs <- data_ghs[match(df2$Country,data_ghs$Country),"value"]
df2 <- bind_rows(df,df2) %>% na.omit()
new_data <- df2

PI<-boot_pi(mdl, new_data, 50000, 0.95)

df2$fit<-PI$pred
df2$lwr<-PI$lower
df2$upr<-PI$upper

# compute the prediction intervals including countries that have yet to see cases assuming each country's has the top GHS index---------
df$exported <- '1'
df3 <- data_flights[!data_flights$label%in%df$Country,c(4:6)]
colnames(df3)[1] <- "Country"
df3$Cases_lm <- 0
df3$exported <- '0'
df3 <- bind_rows(df,df3)
##Set ghs of all predictions to top (equivalent to US)
df3$ghs <- sort(data_ghs$value,decreasing = TRUE)[1]
new_data <- df3 %>% na.omit()

PI2<-boot_pi(mdl, new_data, 50000, 0.95)

df3$fit2<-PI2$pred
df3$lwr2<-PI2$lower
df3$upr2<-PI2$upper

colnames(df3)[5] <- 'ghs2'

#Combine predictions
df3 <- full_join(df3,df2)

#Same plot as https://github.com/c2-d2/cov19flightimport
df3 %>%
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
  # subset(Continent%in%'Africa') %>%
  ggplot(aes(x=reorder(Country,risk)))+
  geom_bar(aes(y=Cases_lm),alpha=.7,stat = "identity")+
  geom_pointrange(aes(y=fit,ymin=lwr,ymax=upr),color="red")+
  geom_crossbar(aes(y=fit2,ymin=lwr2,ymax=upr2))+
  coord_flip()+
  # ggtitle("Reported exported cases  vs\n95% CI of expected exported cases")+
  theme(axis.title.y = element_blank())+ylab("Cases") -> p

png('plots/risk_world.png',width = 8,height = 18,units = "in",res = 300)
p
dev.off()

df3 %>%
  subset(Continent%in%'Africa') %>%
  ggplot(aes(x=reorder(Country,risk)))+
  geom_bar(aes(y=Cases_lm),alpha=.7,stat = "identity")+
  geom_pointrange(aes(y=fit,ymin=lwr,ymax=upr),color="red")+
  geom_crossbar(aes(y=fit2,ymin=lwr2,ymax=upr2))+
  coord_flip()+
  # ggtitle("Reported exported cases  vs\n95% CI of expected exported cases")+
  theme(axis.title.y = element_blank())+ylab("Cases") -> p

png('plots/risk_africa.png',width = 8,height = 9,units = "in",res = 300)
p
dev.off()

df3 %>%
  # subset(Continent%in%'Africa') %>%
  ggplot(aes(x=reorder(Country,risk)))+
  geom_bar(aes(y=Cases_lm),alpha=.7,stat = "identity")+
  geom_pointrange(aes(y=fit,ymin=lwr,ymax=upr),color="red")+
  geom_crossbar(aes(y=fit2,ymin=lwr2,ymax=upr2))+
  coord_flip()+
  # ggtitle("Reported exported cases  vs\n95% CI of expected exported cases")+
  theme(axis.title.y = element_blank())+ylab("Cases")+
  facet_wrap(~Continent,scales = "free",ncol = 2) -> p

png('plots/risk_by_continent.png',width = 10,height = 15,units = "in",res = 300)
p
dev.off()


##Add map data------------
library(rnaturalearth)
library(sf)
library(mapview)
library(leaflet)

worldmap <- ne_countries(returnclass = "sf")
worldmap$Country <- countrycode(worldmap$name,origin = 'country.name',destination = 'country.name')

df3 %<>% mutate(exp.obs.cases=paste0(round(fit,2),' (',round(lwr,2),'-',round(upr,2),')'),
                exp.cases=paste0(round(fit2,2),' (',round(lwr2,2),'-',round(upr2,2),')'))

map.plot <- full_join(worldmap,df3)
g <- foreach(i=1:nrow(map.plot)) %dopar%  {
  df3 %>%
    subset(Continent%in%map.plot[i,'Continent']) -> df3.g
  a <- ifelse(df3.g$Country[order(df3.g$risk)]%in%map.plot[i,'Country'],"red","black")
  df3.g %>% 
    ggplot(aes(x=reorder(Country,risk)))+
    geom_bar(aes(y=Cases_lm),alpha=.7,stat = "identity")+
    geom_pointrange(aes(y=fit,ymin=lwr,ymax=upr),color="red")+
    geom_crossbar(aes(y=fit2,ymin=lwr2,ymax=upr2))+
    theme(axis.text.y = element_text(colour = a))+
    coord_flip()+
    theme(axis.title.y = element_blank())+ylab("Cases")+
    ggtitle(map.plot[i,'Continent'])-> g
  g
}


map.plot %<>% select(c('name','continent','Cases_lm','fit','fit2','exp.obs.cases','exp.cases')) %>% 
  mutate_if(is.numeric,function (x) round(x,digits=2))
names(map.plot)[1:3] <- c("Country","Continent","Cases")

map.plot %>%
  subset(Continent=="Africa") %>%
  mapview(zcol = c("fit2"),
          label=map.plot$name,
          col.regions=colorRampPalette(c('white', 'red')),
          # at=round(c(0,1,5,10,20,max(map.plot$fit2,na.rm = TRUE)),digits = 0),
          layer.name = c('Expected Cases'),
          popup=popupTable(map.plot,
                           zcol=c('Country','Continent','Cases','exp.obs.cases','exp.cases'),
                           feature.id = FALSE,row.numbers = FALSE),
          legend=FALSE,
          alpha.regions=.5
  ) -> m

mapshot(m,file = paste0(getwd(), "/plots/africamap.png"))


map.plot %>%
  # subset(continent=="Africa") %>%
  mapview(zcol = c("fit2"),
          label=map.plot$name,
          col.regions=colorRampPalette(c('white', 'red')),
          at=round(c(0,1,5,10,20,max(map.plot$fit2,na.rm = TRUE)),digits = 0),
          layer.name = c('Expected Cases'),
          popup=popupTable(map.plot,
                           zcol=c('Country','Continent','Cases','exp.obs.cases','exp.cases'),
                           feature.id = FALSE,row.numbers = FALSE)) -> m1

map.plot %>%
  # subset(continent=="Africa") %>%
  mapview(zcol = c("fit"),
          label=map.plot$name,
          col.regions=colorRampPalette(c('white', 'red')),
          at=round(c(0,1,5,10,20,max(map.plot$fit2,na.rm = TRUE)),digits = 0),
          layer.name = c('Expected Observed Cases'),
          popup=popupTable(map.plot,
                           zcol=c('Country','Continent','Cases','exp.obs.cases','exp.cases'),
                           feature.id = FALSE,row.numbers = FALSE),
          hide=TRUE) -> m2

map.plot %>%
  # subset(continent=="Africa") %>%
  mapview(zcol = c("Cases"),
          label=map.plot$name,
          col.regions=colorRampPalette(c('white', 'red')),
          at=round(c(0,1,5,10,20,max(map.plot$fit2,na.rm = TRUE)),digits = 0),
          layer.name = c('Observed Cases'),
          popup=popupTable(map.plot,
                           zcol=c('Country','Continent','Cases','exp.obs.cases','exp.cases'),
                           feature.id = FALSE,row.numbers = FALSE),
          hide=TRUE) -> m3

m1+m2+m3

mapshot(m1,file = paste0(getwd(), "/plots/worldmap.png"))

map.plot %>%
  # subset(continent=="Africa") %>%
  mapview(zcol = c("fit2"),
          label=map.plot$name,
          col.regions=colorRampPalette(c('white', 'red')),
          at=round(c(0,1,5,10,20,max(map.plot$fit2,na.rm = TRUE)),digits = 0),
          layer.name = c('Expected Cases'),
          # popup=popupTable(map.plot,
          #                  zcol=c('Country','Continent','Cases','exp.obs.cases','exp.cases'),
          #                  feature.id = FALSE,row.numbers = FALSE),
          hide=TRUE,
          popup=popupGraph(g)) -> m4

m4+m2+m3

mapshot(m4,file = paste0(getwd(), "/plots/worldmap.png"))

# map.plot %>%
#   mutate(risk=fit2-fit) %>%
#   mutate(risk=(fit2-fit)/fit2) %>%
#   mapview(zcol = c("risk"),
#           label=map.plot$name,
#           col.regions=colorRampPalette(c('white', 'red')),
#           # at=round(c(0,1,5,10,20,max(map.plot$fit2,na.rm = TRUE)),digits = 0),
#           layer.name = c('Number of cases that may be missed'),
#           popup=popupTable(map.plot,
#                            zcol=c('Country','Continent','Cases','exp.obs.cases','exp.cases'),
#                            feature.id = FALSE,row.numbers = FALSE)
#   ) -> m4
# 
# m1+m2+m3+m4
