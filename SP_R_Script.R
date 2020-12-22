#Set Working Directory & Import CSV Files

setwd("C:/Users/darra/OneDrive/Documents/Software Project")

map_avg_temp <- read.csv("average_temperature_map.csv", header = TRUE, sep = ",")

cases_deaths <- read.csv("covid19_cases_deaths_over_time.csv", header = TRUE, sep = ",")

total_cases_deaths <- read.csv("total_cases_deaths.csv", header = TRUE, row.names = 1, sep = ",")


#Pre-Process Data

#Remove Columns -

cases_deaths <- cases_deaths[ , -c(4,5,7,9,10,12,13,14,15)]


#Change Column Names

colnames(cases_deaths)[1] <- "date"

colnames(cases_deaths)[3] <- "total_cases"

colnames(cases_deaths)[4] <- "new_cases"

colnames(cases_deaths)[5] <- "total_deaths"

colnames(cases_deaths)[6] <- "new_deaths"

#Create Subsets

cases = subset(cases_deaths, select = -c(total_deaths, new_deaths))

deaths = subset(cases_deaths, select = -c(total_cases, new_cases))

#Visualisations

library(ggplot2)
library(plotly)
library(dplyr)
library(readr)

#Cases Graph

cases_graph = plot_geo(map_avg_temp,
                             locationmode = 'USA-states',
                             frame = ~ month_id) %>%
  
  add_markers(
    y = ~lat, x = ~lon, locations = ~state_abbreviation,
    size = ~cases, marker = list(sizeref = 1, color = 'white', sizemode = 'diameter')) %>%

  add_trace (locations = ~state_abbreviation,
             text = paste0('State: ', map_avg_temp$state, '<br>Temperature: ', map_avg_temp$average_temperature,'°F', '<br>Cases: ', map_avg_temp$cases),
             hoverinfo = 'text',
             z = ~average_temperature,
             zmin = 0,
             zmax = max(map_avg_temp$average_temperature),
             color = ~average_temperature) %>%
  
  layout(geo = list(scope = 'usa'),
         title = "USA Covid 19 Cases By State 2020") %>%

  config(displayModeBar = FALSE) %>%
  
  colorbar(ticksuffix = "°F")

cases_graph

#Deaths Graph

deaths_graph = plot_geo(map_avg_temp,
                        locationmode = 'USA-states',
                        frame = ~ month_id) %>%
  
  add_markers(
    y = ~lat, x = ~lon, locations = ~state_abbreviation,
    size = ~deaths, marker = list(sizeref = 1, color = 'red', sizemode = 'diameter')) %>%
  
  add_trace (locations = ~state_abbreviation,
             text = paste0('State: ', map_avg_temp$state, '<br>Temperature: ', map_avg_temp$average_temperature,'°F', '<br>Deaths: ', map_avg_temp$deaths),
             hoverinfo = 'text',
             z = ~average_temperature,
             zmin = 0,
             zmax = max(map_avg_temp$average_temperature),
             color = ~average_temperature) %>%
  
  layout(geo = list(scope = 'usa'),
         title = "USA Covid 19 Deaths By State 2020") %>%
  
  config(displayModeBar = FALSE) %>%
  
  colorbar(ticksuffix = "°F")

deaths_graph

#Cluster Analysis 1

scatter_plot = subset(map_avg_temp, select = c(state, month, average_temperature, cases, deaths))

scatter_plot <- scatter_plot[-c(1,2,13,14,25,26,49,50,61,62,73,74,85,86,97,109,110,121,122,134,
                                145,146,157,158,169,170,181,182,193,205,206,217,218,229,230,241,
                                242,253,254,265,266,277,278,289,290,301,302,313,314,325,326,337,
                                338,349,350,361,362,373,374,385,386,397,398,408,409,421,422,433,
                                434,445,446,457,458,469,470,481,482,493,494,505,506,517,518,541,
                                542,553,554,565,566), ]

View(scatter_plot)

str(scatter_plot)

plot(average_temperature ~ cases, scatter_plot)

#Cluster Analysis 2

z <- scatter_plot[,-c(1,2)]

m <- apply(z,2,mean)

sd <- apply(z,2,sd)

z <- scale(z,m,sd)

distance <- dist(z)

print(distance,digits=3)

hc.c <- hclust(distance)

plot(hc.c)

#Cluster Analysis 3

library(factoextra)

total_cases_deaths <- scale(total_cases_deaths)

res.dist <- get_dist(total_cases_deaths, method = "pearson")
head(round(as.matrix(res.dist), 2))[, 1:6]

res.km <- eclust(total_cases_deaths, "kmeans", nstart = 25)

#Cluster Analysis 4

fviz_silhouette(res.km)