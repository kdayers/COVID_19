rm(list=ls())
library(tidyverse)
library(cowplot)
#https://github.com/datasets/covid-19/blob/master/time-series-19-covid-combined.csv
today <- Sys.Date() # Get today's date
options(scipen=100000) # set the option for when to start using scientific notation

# Now scrape the data
covid_ts <- read.csv("https://raw.githubusercontent.com/datasets/covid-19/master/time-series-19-covid-combined.csv")

# This DF groups by country and date and then summarizes.  
# I'm doing this so I don't have to plot by city.  
# ... could always go back and group by city.
country_original <- covid_ts %>% 
  group_by(
    Country.Region,
    Date
  ) %>% 
  summarize(
    Confirmed = sum(Confirmed, na.rm=TRUE),
    Recovered = sum(Recovered, na.rm=TRUE),
    Deaths = sum(Deaths, na.rm=TRUE)
  ) 

country <- country_original %>% 
  filter( # only keep a few countries
    Country.Region %in% c("US", "Mainland China","Australia","Canada","Chile","France","Germany","Italy","Japan", "Mexico", "New Zealand","Spain",  "South Korea")
  )

# Pull out just the US
US <- country %>% 
  filter(Country.Region == "US") %>% 
  ungroup(Country.Region) %>% 
  select(-Country.Region) %>%
  gather(key = "Type", value="Number", -Date)

# Pull out just China  
China <- country %>% 
  filter(Country.Region == "Mainland China") %>% 
  ungroup(Country.Region) %>% 
  select(-Country.Region) %>%
  gather(key = "Type", value="Number", -Date)

Italy <- country %>% 
  filter(Country.Region == "Italy") %>% 
  ungroup(Country.Region) %>% 
  select(-Country.Region) %>%
  gather(key = "Type", value="Number", -Date)


# Pull China out of the original dataframe
CountriesNotChina <- country %>% 
  filter(
    Country.Region != "Mainland China"
  )


# Confirmed Cases up to yesterday
p1 <- ggplot(data=CountriesNotChina, aes(x=as.Date(Date), color=Country.Region)) + 
  geom_line(aes(y=Confirmed)) + 
  # theme(legend.position="none")+
  scale_y_log10(limits = c(1,1e+5)) +
  labs(
    x = "Date",
    y = "Total Confirmed Cases",
    color="Country",
    title=paste("Confirmed Cases by Country",today-2, sep=" ")
  )

# Deaths up to yesterday
p2 <- ggplot(data=CountriesNotChina, aes(x=as.Date(Date), color=Country.Region)) + 
  geom_line(aes(y=Deaths)) + 
  # theme(legend.position="none")+
  scale_y_log10(limits = c(1,1e+5)) +
  labs(
    x = "Date",
    y = "Total Deaths",
    color="Country",
    title=paste("Deaths as of",today-2, sep=" ")
  )

# Recovereds up to yesterday
p3 <- ggplot(data=CountriesNotChina, aes(x=as.Date(Date), color=Country.Region)) + 
  geom_line(aes(y=Recovered)) + 
  # theme(legend.position="none")+
  scale_y_log10(limits = c(1,1e+5)) +
  labs(
    x = "Date",
    y = "Total Recovered",
    color="Country",
    title=paste("Recovered Cases as of",today-2, sep=" ")
  )

# US Cases up to yesterday
p4 <- ggplot(data=Italy, aes(x=as.Date(Date), color=Type)) + 
  geom_line(aes(y=Number)) + 
  scale_y_log10(limits = c(1,1e+5)) +
  labs(
    x = "Date",
    y = "Italy",
    color="Italy",
    title=paste("Totals in Italy as of",today-2, sep=" ")
  )

# Chinese Cases up to yesterday
p5 <- ggplot(data=China, aes(x=as.Date(Date), color=Type)) + 
  geom_line(aes(y=Number)) + 
  scale_y_log10(limits = c(1,1e+5), breaks=10^seq(0, 5, 1)) +
  labs(
    x = "Date",
    y = "China",
    color="China",
    title=paste("Totals in China as of",today-2, sep=" ")
  )
p6 <- ggplot(data=US, aes(x=as.Date(Date), color=Type)) + 
  geom_line(aes(y=Number)) + 
  scale_y_log10(limits = c(1,1e+5)) +
  labs(
    x = "Date",
    y = "US",
    color="US",
    title=paste("Totals in US as of",today-2, sep=" ")
  )





library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  rename(
    Country.Region = name
  ) 

world$Country.Region[world$Country.Region=="China"] <- "Mainland China"
world$Country.Region[world$Country.Region=="United States"] <- "US"
world$Country.Region <- as.factor(world$Country.Region)

TodayByCountry <- country_original %>% 
  filter(as.character(Date) == (today-2)) %>% 
  select(-Date)
world_confirmed <- left_join(world, TodayByCountry, by="Country.Region")

# class(world)
# ggplot(data = world_confirmed) + 
#   geom_sf(color = "black", fill = "lightgreen")

p7 <- ggplot(data = world_confirmed) +
  geom_sf(color = "black", aes(fill = Confirmed)) + 
  labs(
    title = paste("Confirmed as of",today-2,sep=" ")
  )

p8 <- ggplot(data = world_confirmed) +
  geom_sf(color = "black", aes(fill = Deaths)) + 
  labs(
    title = paste("Deaths as of",today-2,sep=" ")
  )
p9 <- ggplot(data = world_confirmed) +
  geom_sf(color = "black", aes(fill = Recovered)) + 
  labs(
    title = paste("Recovered as of",today-2,sep=" ")
  )


# Plot Time Series Data
png(paste("TimeSeries",paste(today-2,".png",sep=""),sep=""), width=1500, height=1000)
plot_grid(p1,p2,p3,p4,p5,p6)
dev.off()
# Plot maps
png(paste("WorldMap",paste(today-2,".png",sep=""),sep=""), width=1500, height=1000)
plot_grid(p7,p8,p9, ncol=1)
dev.off()



