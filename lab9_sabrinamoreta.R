library(tidyverse)
library(lubridate)

report_03_11_2020 <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-11-2020.csv")) %>%
  rename(Country.Region = "Country/Region", Province.State = "Province/State")

head(report_03_11_2020)

str(report_03_11_2020)

#plot of confirmed cases in US states as of March 11th.
report_03_11_2020 %>% 
  filter (Country.Region == "US") %>% 
  ggplot(aes(x = Confirmed,  y = reorder(Province.State, Confirmed))) + 
  geom_point() +
  ggtitle("Confirmed cases for each US State") +
  ylab("Country/Region") +
  xlab("Confirmed Cases")

report_02_17_2020 <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/02-17-2020.csv")) %>%
  rename(Country.Region = "Country/Region", Province.State = "Province/State")
#GRAPH 1
#Confirmed cases in China as of Febuary 17th 2020. 
report_02_17_2020 %>% 
  filter (Country.Region == "Mainland China") %>% 
  ggplot(aes(x = Confirmed,  y = reorder(Province.State, Confirmed))) + 
  geom_point() +
  ggtitle("Confirmed cases for each providence in China") +
  ylab("Country/Region") +
  xlab("Confirmed Cases")


#if we try to graph by Country.Region there are entries that have the 
#same character (e.g. China and US) so we need to summarise the data we 
#are interested in graphing.
report_03_11_2020 %>% 
  group_by(Country.Region) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  arrange(desc(Deaths))

#there are 116 countries which are a lot to put on a single graph, so let’s 
#just start with the countries with the most cases. In past labs we have 
#only made bar graphs from histogram data, but values can be used where 
#height of the bar will represent the value in a column of the data frame. 
#This is done by using stat=“identity” instead of the default, stat=“bin”.
report_03_11_2020 %>% 
  group_by(Country.Region) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  arrange(desc(Deaths)) %>% 
  slice(1:20) %>% 
  ggplot(aes(x = Deaths,  y = reorder(Country.Region, Deaths))) + 
  geom_bar(stat = 'identity') +
  ggtitle("The 20 countries with the most reported COV19-related deaths") +
  ylab("Country/Region") +
  xlab("Deaths")

#GRAPH 2
report_02_17_2020 %>% 
  group_by(Country.Region) %>% 
  summarise(Confirmed = sum(Confirmed)) %>% 
  arrange(desc(Confirmed)) %>% 
  slice(1:10) %>% 
  ggplot(aes(y = Confirmed,  x = reorder(Country.Region, Confirmed))) + 
  geom_bar(stat = 'identity') +
  ggtitle("The 10 countries with the most reported COV19-related confirmed cases") +
  ylab("Country/Region") +
  xlab("Confirmed Cases") +
  coord_flip()

#TIME SERIES

time_series_confirmed <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>%
  rename(Province.State = "Province/State", Country.Region = "Country/Region")

head(time_series_confirmed)

#converting from short to long formart. 
time_series_confirmed_long <- time_series_confirmed %>% 
  pivot_longer(-c(Province.State, Country.Region, Lat, Long),
               names_to = "Date", values_to = "Confirmed") %>% 
  group_by(Country.Region,Date) %>% 
  summarise(Confirmed = sum(Confirmed))
# convert date to data format
time_series_confirmed_long$Date <- mdy(time_series_confirmed_long$Date)

#new format
head(time_series_confirmed_long)

#us confirmed covid-19 cases
time_series_confirmed_long %>% 
  filter (Country.Region == "US") %>% 
  ggplot(aes(x = Date,  y = Confirmed)) + 
  geom_point() +
  geom_line() +
  ggtitle("US Confirmed COVID-19 Cases")

#adding more countries to previous graph
time_series_confirmed_long %>% 
  filter (Country.Region %in% c("China","Japan", "Korea, South",
                                "Italy","Spain", "US")) %>% 
  ggplot(aes(x = Date,  y = Confirmed)) + 
  geom_point() +
  geom_line() +
  ggtitle("Confirmed COVID-19 Cases") +
  facet_wrap(~Country.Region, ncol=2, scales="free_y")

time_series_recovered <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")) %>%
  rename(Province.State = "Province/State", Country.Region = "Country/Region")

head(time_series_recovered)

time_series_recovered_long <- time_series_recovered %>% 
  pivot_longer(-c(Province.State, Country.Region, Lat, Long),
               names_to = "Date", values_to = "recovered") %>% 
  group_by(Country.Region,Date) %>% 
  summarise(recovered = sum(recovered))
# convert date to data format
time_series_recovered_long$Date <- mdy(time_series_recovered_long$Date)

head(time_series_recovered_long)

#GRAPH 1
time_series_recovered_long %>% 
  filter (Country.Region %in% c("Australia","Japan", "Thailand",
                                "Italy","Spain", "Canada")) %>% 
  ggplot(aes(x = Date,  y = recovered)) + 
  geom_point() +
  geom_line() +
  ggtitle("Recovered COVID-19 Cases") +
  facet_wrap(~Country.Region, ncol=2, scales="free_y")

#GRAPH 2
time_series_confirmed_long %>% 
  filter (Country.Region %in% c("Canda","France","Italy", 
                                "Dominican Republic", "Jordan")) %>% 
  ggplot(aes(x = Date,  y = Confirmed, color = Country.Region)) + 
  geom_point() +
  geom_line() +
  ggtitle("Confirmed COVID-19 Cases")

#DATA ON MAPS
library(maps)
library(viridis)

world <- map_data("world")

mybreaks <- c(1, 20, 100, 1000, 50000)

#GRAPH 1
ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(data=time_series_confirmed, aes(x=Long, y=Lat, size=`3/17/20`, color=`3/17/20`),stroke=F, alpha=0.7) +
  scale_size_continuous(name="Cases", trans="log", range=c(1,7),breaks=mybreaks, labels = c("1-19", "20-99", "100-999", "1,000-49,999", "50,000+")) +
  # scale_alpha_continuous(name="Cases", trans="log", range=c(0.1, 0.9),breaks=mybreaks) +
  scale_color_viridis_c(option="inferno",name="Cases", trans="log",breaks=mybreaks, labels = c("1-19", "20-99", "100-999", "1,000-49,999", "50,000+")) +
  theme_void() + 
  guides( colour = guide_legend()) +
  labs(caption = "") +
  theme(
    legend.position = "bottom",
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#ffffff", color = "blue"), 
    panel.background = element_rect(fill = "#ffffff", color = "red"), 
    legend.background = element_rect(fill = "#ffffff", color = "yellow")
  )

#GRAPH 2
time_series_confirmed_long2 <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>%
  rename(Province.State = "Province/State", Country.Region = "Country/Region") %>%
  pivot_longer(-c(Province.State, Country.Region, Lat, Long),
               names_to = "Date", values_to = "cumulative_cases") %>%
  mutate(Date = mdy(Date) - days(1),
         Place = paste(Lat,Long,sep="_")) %>%
  group_by(Place,Date) %>%
  summarise(cumulative_cases = ifelse(sum(cumulative_cases)>0,
                                      sum(cumulative_cases),NA_real_),
            Lat = mean(Lat),
            Long = mean(Long)) %>%
  mutate(Pandemic_day = as.numeric(Date - min(Date)))

head(time_series_confirmed_long2)

static <- ggplot(subset(time_series_confirmed_long2,Date %in% seq(min(Date),max(Date),7)),
                 aes(x = Long, y = Lat, size = cumulative_cases/1000)) +
  borders("world", colour = NA, fill = "grey90") +
  theme_bw() +
  geom_point(shape = 21, color='purple', fill='purple', alpha = 0.5) +
  labs(title = 'COVID-19 spread',x = '', y = '',
       size="Cases (x1000))") +
  theme(legend.position = "right") +
  coord_fixed(ratio=1)+
  facet_wrap(.~Date,nrow=3)
static


