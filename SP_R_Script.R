#Libraries required
  library(ggplot2)
  library(plotly)
  library(readr)
  library(factoextra)
  library(cluster)
  library(gridExtra)


#Set Working Directory & Import CSV Files
  setwd("D:/Software Project/RStudiofiles")
  
  fulldf <- read.csv("United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv", header = TRUE, sep = ",")
  map_avg_temp <- read.csv("average_temperature_map.csv", header = TRUE, sep = ",")
  total_cases_temp <- read.csv("total_cases_temp.csv", header = TRUE, row.names = 1, sep = ",")
  total_deaths_temp <- read.csv("total_deaths_temp.csv", header = TRUE, row.names = 1, sep = ",")
  july_cases_temp <- read.csv("july_cases_temp.csv", header = TRUE, row.names = 3, sep = ",")
  july_deaths_temp <- read.csv("july_deaths_temp.csv", header = TRUE, row.names = 3, sep = ",")
  november_deaths_temp <- read.csv("november_deaths_temp.csv", header = TRUE, row.names = 3, sep = ",")
  november_cases_temp <- read.csv("november_cases_temp.csv", header = TRUE, row.names = 3, sep = ",")
  december_deaths_temp <- read.csv("december_deaths_temp.csv", header = TRUE, row.names = 3, sep = ",")
  december_cases_temp <- read.csv("december_cases_temp.csv", header = TRUE, row.names = 3, sep = ",")
  
  
#Main data frame pre-processing
  #Removing columns
    fulldf <- fulldf[ , -c(4,5,7,9,10,12,13,14,15)]
  #Sorting main data frame by state
    fulldf <- fulldf[order(fulldf$state),]
    
    
#Maps

  #Cases Map

cases_map = plot_geo(map_avg_temp,
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

cases_map

  #Deaths Map

deaths_map = plot_geo(map_avg_temp,
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

deaths_map


#Clusters

  #kmeans clustering cases
    k2 <- kmeans(total_cases_temp, centers = 2, nstart = 25)
    fviz_cluster(k2, data = total_cases_temp)
    
    k3 <- kmeans(total_cases_temp, centers = 3, nstart = 25)
    k4 <- kmeans(total_cases_temp, centers = 4, nstart = 25)
    k5 <- kmeans(total_cases_temp, centers = 5, nstart = 25)
    
    p1 <- fviz_cluster(k2, geom = "point", data = total_cases_temp) + ggtitle("k = 2")
    p2 <- fviz_cluster(k3, geom = "point", data = total_cases_temp) + ggtitle("k = 3")
    p3 <- fviz_cluster(k4, geom = "point", data = total_cases_temp) + ggtitle("k = 4")
    p4 <- fviz_cluster(k5, geom = "point", data = total_cases_temp) + ggtitle("k = 5")
    grid.arrange(p1, p2, p3, p4, nrow = 2)
    
    wssplot <- function(data, max_clusters=15) {
      wss <- (nrow(data)-1)*sum(apply(data,2,var))
      for (k in 2:max_clusters){
        wss[k] <- sum(kmeans(data, centers=k)$withinss)
      }
      plot(1:max_clusters, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
      }
    
    set.seed(42)
    wssplot(total_cases_temp,10)

    #Best number of centers
      k2 <- kmeans(total_cases_temp, centers = 3, nstart = 25)
      fviz_cluster(k2, data = total_cases_temp, main = "Cluster Plot Cases VS Average Temperature")


  #kmeans clustering deaths
    k2 <- kmeans(total_deaths_temp, centers = 2, nstart = 25)
    fviz_cluster(k2, data = total_deaths_temp)
    
    k3 <- kmeans(total_deaths_temp, centers = 3, nstart = 25)
    k4 <- kmeans(total_deaths_temp, centers = 4, nstart = 25)
    k5 <- kmeans(total_deaths_temp, centers = 5, nstart = 25)
    
    p1 <- fviz_cluster(k2, geom = "point", data = total_deaths_temp) + ggtitle("k = 2")
    p2 <- fviz_cluster(k3, geom = "point", data = total_deaths_temp) + ggtitle("k = 3")
    p3 <- fviz_cluster(k4, geom = "point", data = total_deaths_temp) + ggtitle("k = 4")
    p4 <- fviz_cluster(k5, geom = "point", data = total_deaths_temp) + ggtitle("k = 5")
    grid.arrange(p1, p2, p3, p4, nrow = 2)
    
    wssplot <- function(data, max_clusters=15) {
      wss <- (nrow(data)-1)*sum(apply(data,2,var))
      for (k in 2:max_clusters){
        wss[k] <- sum(kmeans(data, centers=k)$withinss)
      }
      plot(1:max_clusters, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
      }
    
    set.seed(42)
    wssplot(total_deaths_temp,10)

    #Best number of centers
      k2 <- kmeans(total_deaths_temp, centers = 3, nstart = 25)
      fviz_cluster(k2, data = total_deaths_temp, main = "Cluster Plot Deaths VS Average Temperature")
    
 #Clusters for months with the highest cases + deaths
      
  #kmeans clustering July cases
    k2 <- kmeans(july_cases_temp, centers = 2, nstart = 25)
    fviz_cluster(k2, data = july_cases_temp)
      
    k3 <- kmeans(july_cases_temp, centers = 3, nstart = 25)
    k4 <- kmeans(july_cases_temp, centers = 4, nstart = 25)
    k5 <- kmeans(july_cases_temp, centers = 5, nstart = 25)
      
    p1 <- fviz_cluster(k2, geom = "point", data = july_cases_temp) + ggtitle("k = 2")
    p2 <- fviz_cluster(k3, geom = "point", data = july_cases_temp) + ggtitle("k = 3")
    p3 <- fviz_cluster(k4, geom = "point", data = july_cases_temp) + ggtitle("k = 4")
    p4 <- fviz_cluster(k5, geom = "point", data = july_cases_temp) + ggtitle("k = 5")
    grid.arrange(p1, p2, p3, p4, nrow = 2)
      
      wssplot <- function(data, max_clusters=15) {
        wss <- (nrow(data)-1)*sum(apply(data,2,var))
        for (k in 2:max_clusters){
          wss[k] <- sum(kmeans(data, centers=k)$withinss)
        }
        plot(1:max_clusters, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
        }
      
      set.seed(42)
      wssplot(july_cases_temp,10)
      
      #Best number of centers
        k2 <- kmeans(july_cases_temp, centers = 3, nstart = 25)
        fviz_cluster(k2, data = july_cases_temp, main = "Cluster Plot July Cases VS Temperature")
        
        
  #kmeans clustering July deaths
    k2 <- kmeans(july_deaths_temp, centers = 2, nstart = 25)
    fviz_cluster(k2, data = july_deaths_temp)
        
    k3 <- kmeans(july_deaths_temp, centers = 3, nstart = 25)
    k4 <- kmeans(july_deaths_temp, centers = 4, nstart = 25)
    k5 <- kmeans(july_deaths_temp, centers = 5, nstart = 25)
        
    p1 <- fviz_cluster(k2, geom = "point", data = july_deaths_temp) + ggtitle("k = 2")
    p2 <- fviz_cluster(k3, geom = "point", data = july_deaths_temp) + ggtitle("k = 3")
    p3 <- fviz_cluster(k4, geom = "point", data = july_deaths_temp) + ggtitle("k = 4")
    p4 <- fviz_cluster(k5, geom = "point", data = july_deaths_temp) + ggtitle("k = 5")
    grid.arrange(p1, p2, p3, p4, nrow = 2)
        
      wssplot <- function(data, max_clusters=15) {
        wss <- (nrow(data)-1)*sum(apply(data,2,var))
        for (k in 2:max_clusters){
          wss[k] <- sum(kmeans(data, centers=k)$withinss)
        }
        plot(1:max_clusters, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
        }
        
      set.seed(42)
      wssplot(july_deaths_temp,10)  
        
      #Best number of centers
        k2 <- kmeans(july_deaths_temp, centers = 3, nstart = 25)
        fviz_cluster(k2, data = july_deaths_temp, main = "Cluster Plot July Deaths VS Temperature")
        
 
  #kmeans clustering November cases
    k2 <- kmeans(november_cases_temp, centers = 2, nstart = 25)
    fviz_cluster(k2, data = november_cases_temp)
        
    k3 <- kmeans(november_cases_temp, centers = 3, nstart = 25)
    k4 <- kmeans(november_cases_temp, centers = 4, nstart = 25)
    k5 <- kmeans(november_cases_temp, centers = 5, nstart = 25)
        
    p1 <- fviz_cluster(k2, geom = "point", data = november_cases_temp) + ggtitle("k = 2")
    p2 <- fviz_cluster(k3, geom = "point", data = november_cases_temp) + ggtitle("k = 3")
    p3 <- fviz_cluster(k4, geom = "point", data = november_cases_temp) + ggtitle("k = 4")
    p4 <- fviz_cluster(k5, geom = "point", data = november_cases_temp) + ggtitle("k = 5")
    grid.arrange(p1, p2, p3, p4, nrow = 2)
        
      wssplot <- function(data, max_clusters=15) {
        wss <- (nrow(data)-1)*sum(apply(data,2,var))
        for (k in 2:max_clusters){
          wss[k] <- sum(kmeans(data, centers=k)$withinss)
        }
        plot(1:max_clusters, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
        }
        
      set.seed(42)
      wssplot(november_cases_temp,10)
        
      #Best number of centers
        k2 <- kmeans(november_cases_temp, centers = 3, nstart = 25)
        fviz_cluster(k2, data = november_cases_temp, main = "Cluster Plot November Cases VS Temperature")
        
        
  #kmeans clustering November deaths
    k2 <- kmeans(november_deaths_temp, centers = 2, nstart = 25)
    fviz_cluster(k2, data = november_deaths_temp)
        
    k3 <- kmeans(november_deaths_temp, centers = 3, nstart = 25)
    k4 <- kmeans(november_deaths_temp, centers = 4, nstart = 25)
    k5 <- kmeans(november_deaths_temp, centers = 5, nstart = 25)
        
    p1 <- fviz_cluster(k2, geom = "point", data = november_deaths_temp) + ggtitle("k = 2")
    p2 <- fviz_cluster(k3, geom = "point", data = november_deaths_temp) + ggtitle("k = 3")
    p3 <- fviz_cluster(k4, geom = "point", data = november_deaths_temp) + ggtitle("k = 4")
    p4 <- fviz_cluster(k5, geom = "point", data = november_deaths_temp) + ggtitle("k = 5")
    grid.arrange(p1, p2, p3, p4, nrow = 2)
        
      wssplot <- function(data, max_clusters=15) {
        wss <- (nrow(data)-1)*sum(apply(data,2,var))
        for (k in 2:max_clusters){
          wss[k] <- sum(kmeans(data, centers=k)$withinss)
        }
        plot(1:max_clusters, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
        }
        
      set.seed(42)
      wssplot(november_deaths_temp,10)  
        
      #Best number of centers
        k2 <- kmeans(november_deaths_temp, centers = 3, nstart = 25)
        fviz_cluster(k2, data = november_deaths_temp, main = "Cluster Plot November Deaths VS Temperature") 
        
        
  #kmeans clustering December cases
    k2 <- kmeans(december_cases_temp, centers = 2, nstart = 25)
    fviz_cluster(k2, data = december_cases_temp)
        
    k3 <- kmeans(december_cases_temp, centers = 3, nstart = 25)
    k4 <- kmeans(december_cases_temp, centers = 4, nstart = 25)
    k5 <- kmeans(december_cases_temp, centers = 5, nstart = 25)
        
    p1 <- fviz_cluster(k2, geom = "point", data = december_cases_temp) + ggtitle("k = 2")
    p2 <- fviz_cluster(k3, geom = "point", data = december_cases_temp) + ggtitle("k = 3")
    p3 <- fviz_cluster(k4, geom = "point", data = december_cases_temp) + ggtitle("k = 4")
    p4 <- fviz_cluster(k5, geom = "point", data = december_cases_temp) + ggtitle("k = 5")
    grid.arrange(p1, p2, p3, p4, nrow = 2)
        
      wssplot <- function(data, max_clusters=15) {
        wss <- (nrow(data)-1)*sum(apply(data,2,var))
        for (k in 2:max_clusters){
           wss[k] <- sum(kmeans(data, centers=k)$withinss)
        }
        plot(1:max_clusters, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
        }
        
      set.seed(42)
      wssplot(december_cases_temp,10)  
        
      #Best number of centers
        k2 <- kmeans(december_cases_temp, centers = 3, nstart = 25)
        fviz_cluster(k2, data = december_cases_temp, main = "Cluster Plot December Cases VS Temperature")       
        
        
  #kmeans clustering December deaths
    k2 <- kmeans(december_deaths_temp, centers = 2, nstart = 25)
    fviz_cluster(k2, data = december_deaths_temp)
        
    k3 <- kmeans(december_deaths_temp, centers = 3, nstart = 25)
    k4 <- kmeans(december_deaths_temp, centers = 4, nstart = 25)
    k5 <- kmeans(december_deaths_temp, centers = 5, nstart = 25)
        
    p1 <- fviz_cluster(k2, geom = "point", data = december_deaths_temp) + ggtitle("k = 2")
    p2 <- fviz_cluster(k3, geom = "point", data = december_deaths_temp) + ggtitle("k = 3")
    p3 <- fviz_cluster(k4, geom = "point", data = december_deaths_temp) + ggtitle("k = 4")
    p4 <- fviz_cluster(k5, geom = "point", data = december_deaths_temp) + ggtitle("k = 5")
    grid.arrange(p1, p2, p3, p4, nrow = 2)
        
      wssplot <- function(data, max_clusters=15) {
        wss <- (nrow(data)-1)*sum(apply(data,2,var))
        for (k in 2:max_clusters){
          wss[k] <- sum(kmeans(data, centers=k)$withinss)
        }
        plot(1:max_clusters, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
        }
        
      set.seed(42)
      wssplot(december_deaths_temp,10)  
        
      #Best number of centers
        k2 <- kmeans(december_deaths_temp, centers = 3, nstart = 25)
        fviz_cluster(k2, data = december_deaths_temp, main = "Cluster Plot December Deaths VS Temperature")
        
  
#Linear regression
  #Cases
    ordertct <- total_cases_temp [order(total_cases_temp$avg_temp),]
    modelcases <- lm(tot_cases ~ avg_temp, data = ordertct)
    summary(ordertct$avg_temp)
  
    newx <- seq(39.5, 70.0, by=0.05)
    plot(ordertct$avg_temp, ordertct$tot_cases, xlab="Average Temperature", ylab="Cases", main="Average Temperature & Total Cases")
    abline(modelcases, col="lightblue", lwd = 2.5)
  
    conf_interval <- predict(modelcases, newdata=data.frame(avg_temp=newx), interval="confidence", level = 0.95)
    lines(newx, conf_interval[,2], col="blue", lty=2)
    lines(newx, conf_interval[,3], col="blue", lty=2)
  
    pred_interval <- predict(modelcases, newdata=data.frame(avg_temp=newx), interval="prediction", level = 0.95)
    lines(newx, pred_interval[,2], col="orange", lty=2)
    lines(newx, pred_interval[,3], col="orange", lty=2)
    #Model summary
      summary(modelcases)
  
  #Deaths
    ordertdt <- total_deaths_temp [order(total_deaths_temp$avg_temp),]
    modeldeaths <- lm(tot_deaths ~ avg_temp, data = ordertdt)
    summary(ordertdt$avg_temp)
    
    newx <- seq(39.5, 70.0, by=0.05)
    plot(ordertdt$avg_temp, ordertdt$tot_deaths, xlab="Average Temperature", ylab="Deaths", main="Average Temperature & Total Deaths")
    abline(modeldeaths, col="lightblue", lwd = 2.5)
    
    conf_interval <- predict(modeldeaths, newdata=data.frame(avg_temp=newx), interval="confidence", level = 0.95)
    lines(newx, conf_interval[,2], col="blue", lty=2)
    lines(newx, conf_interval[,3], col="blue", lty=2)
    
    pred_interval <- predict(modeldeaths, newdata=data.frame(avg_temp=newx), interval="prediction", level = 0.95)
    lines(newx, pred_interval[,2], col="orange", lty=2)
    lines(newx, pred_interval[,3], col="orange", lty=2)
    #Model summary
      summary(modeldeaths)
      
      
#Normality tests
  normailtytestcases <- fulldf$new_case[1:5000]
  shapiro.test(normailtytestcases)  
      
  normailitytestdeaths <- fulldf$new_death[1:5000]
  shapiro.test(normailitytestdeaths)  
      
  plot(modelcases)
  plot(modeldeaths)