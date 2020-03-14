rm(list=ls())
library(tidyverse)
library(cowplot)
#https://github.com/datasets/covid-19/blob/master/time-series-19-covid-combined.csv
today <- Sys.Date() # Get today's date
#todaymonth<-str_sub(today,7,7)
#todayday<-str_sub(today,9,10)
#x<-str_c(c("X",todaymonth),collapse="")
#today<-str_c(c(x,todayday,"20"),collapse=".")
 
options(scipen=100000) # set the option for when to start using scientific notation


# Now scrape the data
covid_ts_confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
covid_ts_recovered<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
covid_ts_deaths<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")

#covid_ts<-left_join(covid_ts_confirmed,covid_ts_recovered,covid_ts_deaths,by="Country.Region")

dates<-c()
for (i in 1:length(names(covid_ts_confirmed))-4){
  dates[i]<-Sys.Date()-length(names(covid_ts_confirmed))+4+i
}


for(i in 5:length(names(covid_ts_confirmed))){
   names(covid_ts_confirmed)[i]<-as.character(as.Date.numeric(dates[i-4],"1970-01-01"))
}

for(i in 5:length(names(covid_ts_recovered))){
  names(covid_ts_recovered)[i]<-as.character(as.Date.numeric(dates[i-4],"1970-01-01"))
}

for(i in 5:length(names(covid_ts_deaths))){
  names(covid_ts_deaths)[i]<-as.character(as.Date.numeric(dates[i-4],"1970-01-01"))
}

country_original_confirmed <- covid_ts_confirmed %>% 
  group_by(
    Country.Region
  ) %>% 
  summarize_if(is.numeric, sum, na.rm = TRUE)%>%
  gather(date, number, "2020-01-22":toString(today))

country_original_recovered <- covid_ts_recovered %>% 
  group_by(
    Country.Region
  ) %>% 
  summarize_if(is.numeric, sum, na.rm = TRUE)%>%
  gather(date, number, "2020-01-22":toString(today))

country_original_deaths <- covid_ts_deaths %>% 
  group_by(
    Country.Region
  ) %>% 
  summarize_if(is.numeric, sum, na.rm = TRUE)%>%
  gather(date, number, "2020-01-22":toString(today))


#pull out just a few

country_confirmed <- country_original_confirmed %>% 
  filter( # only keep a few countries
    Country.Region %in% c("US", "China","Australia","Canada","Chile","France","Germany","Italy","Japan", "Mexico", "New Zealand","Spain",  "South Korea")
  )

country_recovered <- country_original_recovered %>% 
  filter( # only keep a few countries
    Country.Region %in% c("US", "China","Australia","Canada","Chile","France","Germany","Italy","Japan", "Mexico", "New Zealand","Spain",  "South Korea")
  )
country_deaths <- country_original_deaths %>% 
  filter( # only keep a few countries
    Country.Region %in% c("US", "China","Australia","Canada","Chile","France","Germany","Italy","Japan", "Mexico", "New Zealand","Spain",  "South Korea")
  )

country_original<-left_join(country_confirmed,country_recovered,by=c("Country.Region","Lat", "Long","date")) 

country_original<-left_join(country_original,country_deaths,by=c("Country.Region","Lat", "Long","date")) 

country<-country_original%>%
  rename(Confirmed=number.x,
         Recovered=number.y,
         Deaths=number)
                          

# Pull out just the US
US<- country %>% 
  filter(Country.Region == "US") %>% 
  ungroup(Country.Region) %>% 
  select(-Country.Region,-Lat,-Long) %>%
  gather(key = "Type", value="Number", -date)

# Pull out just China  
China<- country %>% 
  filter(Country.Region == "China") %>% 
  ungroup(Country.Region) %>% 
  select(-Country.Region,-Lat,-Long)  %>%
  gather(key = "Type", value="Number", -date)

  

#Pull out just Italy
Italy <- country %>% 
  filter(Country.Region == "Italy") %>% 
  ungroup(Country.Region) %>% 
  select(-Country.Region,-Lat,-Long)  %>%
  gather(key = "Type", value="Number", -date)

# Pull China out of the original dataframe
CountriesNotChina <- country %>% 
  filter(
    Country.Region != "Mainland China"
  )


#Confirmed Cases up to yesterday

p1 <- ggplot(data=CountriesNotChina, aes(x=as.Date(date), color=Country.Region)) + 
  geom_line(aes(y=Confirmed)) + 
  # theme(legend.position="none")+
  scale_y_log10(limits = c(1,1e+5)) +
  labs(
    x = "Date",
    y = "Total Confirmed Cases",
    color="Country",
    title=paste("Confirmed Cases by Country",today, sep=" ")
)

# Deaths up to yesterday
p2 <- ggplot(data=CountriesNotChina, aes(x=as.Date(date), color=Country.Region)) + 
  geom_line(aes(y=Deaths)) + 
  # theme(legend.position="none")+
  scale_y_log10(limits = c(1,1e+5)) +
  labs(
    x = "Date",
    y = "Total Deaths",
    color="Country",
    title=paste("Deaths as of",today, sep=" ")
  )

# Recoveries up to yesterday
p3 <- ggplot(data=CountriesNotChina, aes(x=as.Date(date), color=Country.Region)) + 
  geom_line(aes(y=Recovered)) + 
  # theme(legend.position="none")+
  scale_y_log10(limits = c(1,1e+5)) +
  labs(
    x = "Date",
    y = "Total Recovered",
    color="Country",
    title=paste("Recovered Cases as of",today, sep=" ")
  )

# US Cases up to yesterday
p4 <- ggplot(data=US, aes(x=as.Date(date), color=Type)) + 
  geom_line(aes(y=Number)) + 
  scale_y_log10(limits = c(1,1e+5)) +
  labs(
    x = "Date",
    y = "US",
    color="US",
    title=paste("Totals in US as of",today, sep=" ")
  )

# Chinese Cases up to yesterday
p5 <- ggplot(data=China, aes(x=as.Date(date),color=Type)) + 
  geom_line(aes(y=Number)) + 
  scale_y_log10(limits = c(1,1e+5), breaks=10^seq(0, 5, 1)) +
  labs(
    x = "Date",
    y = "China",
    color="China",
    title=paste("Totals in China as of",today, sep=" ")
)

p6 <- ggplot(data=Italy, aes(x=as.Date(date),color=Type)) + 
  geom_line(aes(y=Number)) + 
  scale_y_log10(limits = c(1,1e+5)) +
  labs(
    x = "Date",
    y = "Italy",
    color="Type",
    title=paste("Totals in Italy as of",today, sep=" ")
  )


# Plot Time Series Data
png(paste("COVIDTimeSeries",paste(today,".png",sep=""),sep=""), width=1500, height=1000)
plot_grid(p1,p2,p3,p4,p5,p6)
dev.off()

