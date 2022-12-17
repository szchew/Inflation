#Packages used
library(quantmod)
library(fredr)
library(lubridate)
library(dplyr)
library(tidyr)
library(pracma)
library(ggplot2)
library(zoo)
library(svglite)


#Vary the different conditions to detect the peaks
#and troughs

#1) First, we set the key to retrieve data from FRED

#2)Next, We create a time of rolling mean of inflation rate = 5
#(For each time period t, we take observation t-2,t-1,t,t+1,t+2
# and find its average)

#3)We create a new variable, ma_replace_amt_with_0
#This variable will replace those moving average with inflation rate
#< 1.8(What I used) with 0 --> This is to allow for the algorithm 
# to better identify the trough

#We also create ma_minTroughHeight - This is a variable that indicates the
#minimum height required by the algorithm to identify a trough
#(Why 1.8 instead of 2 as required?) --> Moving Average captures the general
#Pattern of the inflation rate , and sometimes pattern might be irregular
#1.8 serves as a point of reference for the algorithm that there is likely a trough of 2 around this 
#time period and seeks to limit the identification of trough to this area 

#A slightly lower value of 1.8 helps to better capture the trough (A higher value can
#obsfucate the identification of trough due to noise inherent in inflation rate)

#4)For peak_minPeakHeight (Minimum Height of the Rolling Mean of Inflation Rate),similar concept applies:
#(Why 4.4 and not 5) - 4.4 helps to capture the 'true' peak as we are dealing with rolling mean, where
#which sometimes might not be captured due to sudden inflation rate volatility

#Note that the value 1.8 and 4.4 below have no particular meaning
#I trialled and error with different numbers and I found that they work best
fredr_set_key("2cf4ed60332254125e3f4abcd8e59920")
rollmean_amt<-5
ma_replace_amt_with_0<-1.8 
ma_minTroughHeight<-1.8
peak_minPeakHeight<-4.4




#Produce Graph with Varying Min Peak Distance of 6 Months to 24 months
for (minpeakdistance in seq(6,24,1)){ 
  #Produce Graph with Varying Min Surging Inflation Period of 6 Months to 12 months
  for (surgingInflationPeriod in seq(6,12,1)){
    #Create New Folder Name to store the graphs
    newDirName <- paste("C:/Users/chews/OneDrive - National University of Singapore/Documents/Final Graph/Min Peak Distance ",minpeakdistance," Months, Surging Inflation Period ",surgingInflationPeriod," Months",sep="")
    dir.create(newDirName)
    unique <-read.csv("C:/Users/chews/Downloads/month.csv")
    unique <- unique %>% distinct(Country.Name)
    #Process for each differnet countries
    for (country in unique$Country.Name) {
      currStage <- paste("MinPeakDist:",minpeakdistance,",Surge:",surgingInflationPeriod,",Cty:",country,sep="")
      print(currStage)
      fedfunds<-fredr(series_id = "FEDFUNDS")
      month<-read.csv("C:/Users/chews/Downloads/month.csv") 
      month<- month%>%
        filter(Country.Name ==country)
      month$Time.Period <- ym(month$Time.Period)
      
      #Inflation Rate Formula as requested
      month<- month %>%
        mutate(ir_month = (cpi_month-lag(cpi_month,12))/lag(cpi_month,12)*100) %>%
        fill(ir_month, .direction = 'down')
      
      #Create Rolling Mean and filter out unnecessary data
      #I roll mean twice below because inflation rate is still
      #quite noisy even after rolling them once
      fedfunds<- fedfunds %>%
        left_join(month,by=c('date'='Time.Period')) %>%
        select(date,value,cpi_month,ir_month)%>%
        mutate(ma = zoo::rollmean(ir_month, k =rollmean_amt, fill = NA))%>%
        mutate(ma = zoo::rollmean(ma, k =rollmean_amt, fill = NA))%>%
        mutate(ma = replace(ma, ma < ma_replace_amt_with_0, 0))%>%  
        filter(!is.na(ir_month))%>%
        mutate(rownum=row_number())%>%
        filter(!is.na(ma))%>%
        filter(!is.na(cpi_month))
      
      #Finding the peaks using the rolling mean
      peak<-findpeaks(fedfunds$ma,minpeakheight = peak_minPeakHeight,minpeakdistance=minpeakdistance,peakpat=c("[+]{1,}[-]{1,}"))
      
      # peak<- do.call(rbind,list(peak,peak2))
      
      #Convert the peaks to a data frame
      peak <- as.data.frame(peak)
      
      
      #Select the Peak Dates and the Trough Dates if there exist peaks
      #If there is no peak, we create a
      if (nrow(peak)>0){
        peak <- peak %>%
          rename(max_ir_month=V1,peak=V2,trough=V3)%>%
          select(max_ir_month,peak,trough)%>%
          left_join(fedfunds,by=c("peak"="rownum")) %>%
          left_join(fedfunds,by=c("trough"="rownum")) %>%
          rename(peak_date=date.x,trough_date=date.y)%>%
          select(peak_date,trough_date) 
      } else {
        peak<-setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("peak_date","trough_dates"))
        peak$peak_date<-as_date(peak$peak_date)
        peak$trough_date<-as_date(peak$trough_date)
      }
      
      #peak <- peak %>%
      #  filter(lag_1<ma_minTroughHeight|lag_2<ma_minTroughHeight|lag_3<ma_minTroughHeight|lag_4<ma_minTroughHeight)
      
      #Drop all peaks with no troughs(For the case at the start of the graph, when peaks does not
      #necessarily have trough)
      peak<- peak %>% 
        drop_na() %>%
        select(peak_date,trough_date)
      #Get rid of all peaks and troughs with time period <= surgingInflationPeriod
      peak<-peak %>%
        filter(interval(ymd(peak$trough_date), ymd(peak$peak_date))%/% months(1)>=surgingInflationPeriod)
      
      #Because of the rolling mean, a time period is able to detect values rollmean_amt%/%2 time period ahead
      #Hence we need to shift the peak_date and trough_date behind by rollmean_amt%/%2 to account for this
      peak$peak_date <- peak$peak_date %m+% months(rollmean_amt%/%2 + rollmean_amt%/%2)
      peak$trough_date<-peak$trough_date %m+% months(rollmean_amt%/%2 + rollmean_amt%/%2)
      
      
      #Detect peak and trough especially for Covid-period sing most countries
      #do not have a peak that can be identified 
      #Have to code this out manually for this period
      special<- fedfunds%>%
        filter(date>'2019-12-01')
      if (max(special$ma,na.rm = T)>peak_minPeakHeight) {
        p<-special[max(special$ma,na.rm = T) == special$ma & !is.na(special$ma),"date"]
        special <- special %>%
          filter(date<p$date[1] & ma!=0 & ma>ma_minTroughHeight)
        t<-special[min(special$ir_month,na.rm = T) == special$ir_month ,"date"]
        if (nrow(special %>% filter(date == t$date[1] & ma>ma_minTroughHeight))) {
          peak[nrow(peak) + 1,] = c(p,t)
        }
      }
      
      
      
      
      labels<-c(Country.Name="Country Name",Time.Period="Time Period",
                cpi_month = "Monthly CPI",ir_month="12 Lag Monthly Inflation")
      #Generate the Graph
      g<-ggplot(data = fedfunds, aes(x = date)) +
        geom_line(aes(y = ir_month,color = "Monthly Inflation Rate"),linetype="solid",linewidth = 0.8) +
        geom_line(aes(y = value,color="FED Monthly Interest Rate"),linetype="solid",linewidth = 0.8) +
        annotate("rect", xmin = peak$trough_date, xmax = peak$peak_date, ymin = -Inf, ymax = +Inf,
                 alpha = .4,fill = "pink") +
        scale_linetype_manual("",
                              values = c("Monthly Inflation Rate" = "line",
                                         "FRED Monthly Interest Rate"="line")) +
        scale_color_manual("",
                           values = c("Monthly Inflation Rate" = "red",
                                      "FED Monthly Interest Rate" = "blue")) +
        ylab("")+
        theme_bw() +
        theme(
          axis.title.x=element_blank(),
          axis.title.y.right = element_text(angle = 90),
          legend.key=element_blank(),
          legend.box.background = element_rect(fill='transparent',colour = NA),
          legend.background = element_rect(fill='transparent'),
          legend.key.size = unit(1.5, 'cm'),
          legend.position = c(0.89,0.91),
          legend.title = element_blank(),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5))+
        ggtitle(paste(country))
      #show(g)
      #Save the Graph
      ggsave(filename=paste(country,".svg",sep=""),path=newDirName, device = "svg", width=15, height=8.5)
    }
  }
}