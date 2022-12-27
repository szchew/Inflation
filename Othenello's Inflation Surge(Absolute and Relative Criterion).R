#Libraries Used
library(wbstats)
library(fredr)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)
Argentina, Australia, Austria, Belgium, Brazil, Bulgaria,
Canada, Chile, China, Colombia, Croatia, Czech Republic, Denmark, Egypt, Estonia, Finland, France,
Germany, Greece, Hong Kong, Hungary, India, Indonesia, Ireland, Israel, Italy, Japan, Korea, Latvia,
Lithuania, Malaysia, Mexico, Netherlands, New Zealand, Nigeria, Norway, Peru, Philippines, Poland, Por-
  tugal, Romania, Russia, Saudi Arabia, Singapore, Slovak Republic, Slovenia, Spain, South Africa, Sweden,
Switzerland, Thailand, Turkey, Ukraine, the United Kingdom, and the United States

fredr_set_key("2cf4ed60332254125e3f4abcd8e59920")
#Import Fed Interest Rate and Transforming the data
fedfunds<-fredr(series_id = "FEDFUNDS",frequency = 'm') %>%
  drop_na() %>%
  select(date,value) %>%
  rename("monthly_fed_interest_rate" = "value") %>%
  select(date,monthly_fed_interest_rate) 

#Importing Inflation Information and Transforming it for use
data <- read.csv("C:/Users/chews/Downloads/month.csv") %>%
  drop_na() %>%
  group_by(Country.Name) %>%
  mutate(inflation = ((cpi_month-lag(cpi_month,n=12))/lag(cpi_month,n=12)) * 100) %>%
  select(Country.Name,Time.Period,cpi_month,inflation) %>%
  rename(date = Time.Period,country = Country.Name) %>%
  mutate(date = ym(date)) %>%
  left_join(fedfunds,by = c("date"="date")) %>%
  drop_na() %>%
  group_by(country) %>%
  mutate(inflation_rate_change = inflation - lag(inflation,n=12))

#Getting the 90th percentile of the Othenello's inflation
nith <- quantile(data$inflation_rate_change,na.rm = T,probs = 0.9)

#Create a separate dataframe to obtain Unique countries 
#to use in a for loop later
uniqueCountries <- unique(data$country)

cty <- "United States"
#Generate the different graphs required
#Both Othenello's Absolute/Realtive Criterion
for(cty in uniqueCountries) {
  #Transforming current country's data for use later 
  currCountryData <- data %>%
    filter(country == cty) %>%
    mutate(absolute = ifelse(inflation_rate_change>nith,1,0)) %>%
    mutate(last_10_mean = rollmean(inflation_rate_change, k = 10, fill = NA, align = "right")) %>%
    mutate(last_10_sd = rollapply(inflation_rate_change,width=10,FUN=sd,fill = NA,align = "right")) %>%
    mutate(relative = ifelse(inflation_rate_change > last_10_mean + 1.65*last_10_sd,1,0)) %>%
    mutate(diffAbsolute = lag(absolute,n=1)) %>%
    mutate(diffRelative = lag(relative,n=1))
  q<-quantile(currCountryData$inflation_rate_change,probs=0.9,na.rm = TRUE)
  ##Finding out the start and end of inflation surge
  #based on Othenello's Absolute Criterion
  absoluteInflation.start <- currCountryData %>%
    filter(absolute == 1) %>%
    select(date) %>%
    rename("absolute_inflation_start" = "date")
  
  absoluteInflation.end <- currCountryData %>%
    filter(diffAbsolute == 1) %>%
    select(date) %>%
    rename(absolute_inflation_end = "date")
  
  
  #Some absolute inflation start year may not have an ending year
  #Example Year 2021 is considered a surge, but due to its recency
  #The algorithm cannot detect its end year
  #This if condition accounts for this edge case(We set the 
  #end of the inflation surge period to be 2022 in this case)
  if (nrow(absoluteInflation.end) < nrow(absoluteInflation.start)) {
    absoluteInflation.end[nrow(absoluteInflation.end) + 1,] = list(cty,as.Date("2022-11-01"))
  }
  #Combine the Start and End of the inflation surge into one
  # and drop the unnecessary frames
  absoluteInflation <- cbind(absoluteInflation.start,absoluteInflation.end)
  rm(absoluteInflation.start,absoluteInflation.end)
  #Plot Othenello's Absolute Criterion Graph
  absoluteGraph<-ggplot(data = currCountryData, aes(x = date)) +
    geom_line(aes(y = inflation,color = "Inflation"),linetype="solid",linewidth = 0.8) +
    geom_line(aes(y = monthly_fed_interest_rate,color="FRED Annual Interest Rate"),linetype="solid",linewidth = 0.8) +
    annotate("rect", xmin = absoluteInflation$absolute_inflation_start, xmax = absoluteInflation$absolute_inflation_end, ymin = -Inf, ymax = +Inf,
             alpha = .4,fill = "pink") +
    scale_linetype_manual("",
                          values = c("Inflation" = "line",
                                     "FRED Annual Interest Rate"="line")) +
    scale_color_manual("",
                       values = c("Inflation" = "red",
                                  "FRED Annual Interest Rate" = "blue")) +
    ylab("")+
    theme_bw() +
    theme(
      axis.title.x=element_blank(),
      axis.title.y.right = element_text(angle = 90),
      legend.key=element_blank(),
      legend.box.background = element_rect(fill='transparent',colour = NA),
      legend.background = element_rect(fill='transparent'),
      legend.key.size = unit(1.5, 'cm'),
      legend.position = c(0.87,0.91),
      legend.title = element_blank(),
      axis.text.x=element_text(size=15),
      axis.text.y=element_text(size=15),
      text = element_text(size = 15),
      plot.title = element_text(hjust = 0.5))+
    ggtitle(paste(cty," - Ottonello's Absolute Criterion",sep=""))
 # show(absoluteGraph)
  #Create new directory and save graph
  newDirName <- "C:/Users/chews/OneDrive - National University of Singapore/Documents/Ottonello's Absolute Criterion Graphs"
  dir.create(newDirName)
  ggsave(filename=paste(cty,".svg",sep=""),path=newDirName, device = "svg", width=15, height=8.5)
  
  
  
  
  
  
  
  ##Finding out the start and end of inflation surge
  #based on Othenello's Relative Criterion
  relativeInflation.start <- currCountryData %>%
    filter(relative == 1) %>%
    select(date) %>%
    rename(relative_inflation_start = "date")
  
  relativeInflation.end <- currCountryData %>%
    filter(diffRelative == 1) %>%
    select(date) %>%
    rename(relative_inflation_end = "date")
  #Accounts for edge case(Similar to above)
  if (nrow(relativeInflation.end) < nrow(relativeInflation.start)) {
    relativeInflation.end[nrow(relativeInflation.end) + 1,] = 2022
  }
  
  #Combine the Start and End of the inflation surge into one
  # and drop the unnecessary frames
  relativeInflation <- cbind(relativeInflation.start,relativeInflation.end)
  rm(relativeInflation.start,relativeInflation.end)
  #Plot Othenello's Relative Criterion Graph
  relativeGraph<-ggplot(data = currCountryData, aes(x = date)) +
    geom_line(aes(y = inflation,color = "Inflation"),linetype="solid",linewidth = 0.8) +
    geom_line(aes(y = monthly_fed_interest_rate,color="FRED Annual Interest Rate"),linetype="solid",linewidth = 0.8) +
    annotate("rect", xmin = relativeInflation$relative_inflation_start, xmax = relativeInflation$relative_inflation_end, ymin = -Inf, ymax = +Inf,
             alpha = .4,fill = "pink") +
    scale_linetype_manual("",
                          values = c("Inflation" = "line",
                                     "FRED Annual Interest Rate"="line")) +
    scale_color_manual("",
                       values = c("Inflation" = "red",
                                  "FRED Annual Interest Rate" = "blue")) +
    ylab("")+
    theme_bw() +
    theme(
      axis.title.x=element_blank(),
      axis.title.y.right = element_text(angle = 90),
      legend.key=element_blank(),
      legend.box.background = element_rect(fill='transparent',colour = NA),
      legend.background = element_rect(fill='transparent'),
      legend.key.size = unit(1.5, 'cm'),
      legend.position = c(0.87,0.91),
      legend.title = element_blank(),
      axis.text.x=element_text(size=15),
      axis.text.y=element_text(size=15),
      text = element_text(size = 15),
      plot.title = element_text(hjust = 0.5))+
    ggtitle(paste(cty," - Ottonello's Relative Criterion",sep=""))
  #show(relativeGraph)
  
  #Create new directory and save graph
  newDirName <- "C:/Users/chews/OneDrive - National University of Singapore/Documents/Ottonello's Relative Criterion Graphs"
  dir.create(newDirName)
  ggsave(filename=paste(cty,".svg",sep=""),path=newDirName, device = "svg", width=15, height=8.5)
  
}




