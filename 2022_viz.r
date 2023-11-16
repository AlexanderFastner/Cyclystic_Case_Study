## ----setup, include=FALSE----------------------------------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, echo = TRUE)


## ----Imports, message=FALSE--------------------------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(lubridate)
library(magrittr)
library(dplyr)
library(data.table)
library(scales)
library(viridis)


## ----Load Data, message=FALSE, include=FALSE---------------------------------------------------------------
df_2022 <- data.table(read.table("C:/Users/alex/GoogleDataAnalysis/Capstone/cleanedData/2022/df_2022.csv", sep = ",", header = TRUE))


## ----Format and get summary, echo = FALSE, include=FALSE---------------------------------------------------
df_2022$X <- NULL
knitr::kable(summary(df_2022))


## ----add days of week using lubridate, echo=FALSE----------------------------------------------------------
df_2022$day_of_week <- lubridate::wday(df_2022$started_at, label=TRUE, abbr=FALSE)


## ----add ride length, echo=FALSE---------------------------------------------------------------------------
df_2022$started_at <- ymd_hms(df_2022$started_at)
df_2022$ended_at <- ymd_hms(df_2022$ended_at)

df_2022 <- df_2022[, ride_length := as.integer((ended_at - started_at)/60)]
df_2022 <- df_2022[ride_length >= 1,]


## ----Show updates, echo = FALSE, include=FALSE-------------------------------------------------------------
knitr::kable(head(df_2022), caption = "2022 updated table", "simple")


## ----data cleaning, include = FALSE------------------------------------------------------------------------
df_2022 <- df_2022[ride_length > 0,]
df_2022 <- df_2022[complete.cases(df_2022),]
df_2022 <- df_2022[!(df_2022$start_station_name == ""), ]
df_2022 <- df_2022[!(df_2022$start_station_id == ""), ]
df_2022 <- df_2022[!(df_2022$end_station_name == ""), ]
df_2022 <- df_2022[!(df_2022$end_station_id == ""), ]


## ----save a copy of clean data, include=FALSE--------------------------------------------------------------
#filter
under_1440mins <- data.table(df_2022[ride_length < 1440,])
under_1440mins$hoursBin <- cut(under_1440mins$ride_length, breaks = c(seq(0, 1440, by = 60)))
under_1440mins$binned_ride_length <- cut(under_1440mins$ride_length, breaks = c(0, 10, 30, 60, 120, 240, 480, 720, 1440), labels = c("0-10", "10-30", "30-60", "60-120", "120-240", "240-480", "480-720", "720-1440"))

write.csv(under_1440mins, "2022_cleaned.csv")


## ----filter dataset, echo=FALSE, include=FALSE-------------------------------------------------------------
#filter
under_1440mins <- data.table(df_2022[ride_length < 1440,])
under_1440mins$hoursBin <- cut(under_1440mins$ride_length, breaks = c(seq(0, 1440, by = 60)))
under_1440mins$binned_ride_length <- cut(under_1440mins$ride_length, breaks = c(0, 10, 30, 60, 120, 240, 480, 720, 1440), labels = c("0-10", "10-30", "30-60", "60-120", "120-240", "240-480", "480-720", "720-1440"))

under_60 <- data.table(df_2022[ride_length < 60,])
under_60$minBin <- cut(under_60$ride_length, breaks = c(seq(0, 60, by = 5)))
under_60$started_at <- format(under_60$started_at, "%H:%M")
under_60$started_at <- lubridate::hour(hm(under_60$started_at)) * 100 + lubridate::minute(hm(under_60$started_at))
#transform to factor for correct ordering in plot
under_60$weekday_factor <- factor(under_60$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

under_120mins <- data.table(df_2022[ride_length < 120,])
under_120mins$minBin <- cut(under_120mins$ride_length, breaks = c(seq(0, 120, by = 5)))



## ---- echo = FALSE-----------------------------------------------------------------------------------------
#subset only members
members_dt <- under_1440mins[member_casual == "member",]

#transformation
mdt <- data.table(table(members_dt$binned_ride_length, members_dt$day_of_week))
mdt <- rename(mdt, day_of_week = V2)
mdt <- rename(mdt, binned_ride_length = V1)
mdt <- rename(mdt, value = N)
#transform to factor for correct ordering in plot
mdt$weekday_factor <- factor(mdt$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
bin_colors <- c("#7ad151", "#44bf70", "#22a884", "#21918c", "#2a788e", "#355f8d", "#414487", "#440154")

#total # of members rides breakdown
ggplot(mdt, aes(x = weekday_factor, y = value, fill = binned_ride_length)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = bin_colors) +
  ggtitle("Distribution of rides by Members by Weekday: 2022") + 
  xlab("Weekday") +
  ylab("Total number of rides")



## ----echo = FALSE------------------------------------------------------------------------------------------
#subset casual users
casual_dt <- under_1440mins[member_casual == "casual" ,]
cdt <- data.table(table(casual_dt$binned_ride_length, casual_dt$day_of_week))
cdt <- rename(cdt, day_of_week = V2)
cdt <- rename(cdt, binned_ride_length = V1)
cdt <- rename(cdt, value = N)

#transform to factor for correct ordering in plot
cdt$weekday_factor <- factor(cdt$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#combine into one table for comparison
m <- data.table(aggregate(mdt$value, by=list(mdt$weekday_factor), FUN=sum))
c <- data.table(aggregate(cdt$value, by=list(cdt$weekday_factor), FUN=sum))
m  <- rename(m, day_of_week = Group.1)
m  <- rename(m, value = x)
c <- rename(c, day_of_week = Group.1)
c <- rename(c, value = x)
m$member <- "member"
c$member <- "casual"
combined_dt <- merge.data.frame(m, c, all = TRUE)
combined_dt <- combined_dt %>% group_by(day_of_week) %>% mutate(Percent_of_Total = value/sum(value)*100)

#make colors
my_colors <- c("#0072B2", "#D55E00")

#total # of members rides 
ggplot(combined_dt, aes(x = day_of_week, y = Percent_of_Total, fill = member)) + 
  geom_bar(stat = "identity") +
  ggtitle("Members vs Casual usage by Weekday: 2022") + 
  xlab("Weekday") +
  ylab("Total number of rides") +
  geom_hline(yintercept = 50, color = "black", linewidth = 1)+
  scale_fill_manual(values = my_colors)


## ---- echo = FALSE-----------------------------------------------------------------------------------------
#subset to get only members
under_60_members <- under_60[member_casual == "member",]

#transform to factor for correct ordering in plot
under_60_members$weekday_factor <- factor(under_60_members$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#find mean 
mean_value <- as.integer(mean(under_60_members$started_at))

#plot
ggplot(under_60_members, aes(x = day_of_week, y = started_at, group=weekday_factor)) + 
  geom_boxplot(fill = "#0072B2") +
  scale_y_continuous(limits = c(0, 2400), breaks = seq(0, 2400, by = 100), labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24")) +
  ggtitle("Distribution of ride Start times for Members: 2022") + 
  xlab("Weekday") +
  ylab("Ride start time Hours") +
  geom_hline(yintercept = mean_value, color = "black", linewidth = 0.5)


## ---- echo=FALSE-------------------------------------------------------------------------------------------
#subset data
months_dt <- under_120mins
months_dt$minBin <- NULL

#get months in right format
months_dt$month <- sapply(months_dt$started_at, FUN = lubridate::month, label = TRUE, abbr = FALSE)

#transform to factor for correct ordering in plot
months_dt$month_factor <- factor(months_dt$month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

#get subset of only members  
members_months <- months_dt[member_casual == "member",]  
mean_value <- as.integer(nrow(members_months)/11)
  
#plot    
ggplot(members_months, aes(x = month_factor))+
  geom_bar(aes(y = ..count.., fill = ifelse(..count.. > mean_value, "#D55E00", "#0072B2")), show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Members") + 
  xlab("Month") +
  ylab("Total number of rides") +
  geom_hline(yintercept = mean_value, color = "black", linewidth = 0.5)


## ---- echo = FALSE-----------------------------------------------------------------------------------------
sn <- df_2022[df_2022$member_casual == "member",]
sn <- table(sn$start_station_name)
sn <- sn[order(sn, decreasing = TRUE)]
knitr::kable(head(sn, n = 3), "simple", col.names = NULL)


## ---- echo = FALSE-----------------------------------------------------------------------------------------
en <- df_2022[df_2022$member_casual == "member",]
en <- table(en$end_station_name)
en <- en[order(en, decreasing = TRUE)]
knitr::kable(head(en, n = 3), "simple", col.names = NULL)


## ----clean up RAM, include=FALSE---------------------------------------------------------------------------
under_60 <- NULL
under_60_casual <- NULL
under_60_members <- NULL
under_120mins <- NULL
under_1440mins <- NULL
months_dt <- NULL
members_months <- NULL
m <- NULL
c <- NULL
mdt <- NULL
cdt <- NULL
casual_dt <- NULL
combined_dt <- NULL
members_dt <- NULL
sid <- NULL
sn <- NULL
eid <- NULL
en <- NULL


## ----plot 24 hours, echo=FALSE-----------------------------------------------------------------------------
#filter
under_1440mins <- data.table(df_2022[ride_length < 1440,])
under_1440mins$hoursBin <- cut(under_1440mins$ride_length, breaks = c(seq(0, 1440, by = 60)))

ggplot(under_1440mins, aes(x = ride_length, y = count)) +
    geom_bar(aes(y = ..count..), color = "black", fill = "#D55E00", show.legend = FALSE) +
    scale_x_binned(limits = c(0, 1440), breaks = seq(0, 1440, 60), labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24")) +
    scale_y_continuous(labels = scales::comma) +
    ggtitle("Distribution of ride duration: 2022") + 
    xlab("Ride duration in Hours") +
    ylab("Number of rides")+
    theme(plot.margin = unit(c(0,0,0,0), "cm"))


## ----plot 1 and 2 hours, echo=FALSE, fig.show="hold", out.width="50%"--------------------------------------
#filter
under_60 <- data.table(df_2022[ride_length < 60,])
under_60$minBin <- cut(under_60$ride_length, breaks = c(seq(0, 60, by = 5)))

ggplot(under_60, aes(x = ride_length)) +
  geom_bar(aes(y = ..count..), color = "black", fill = "#D55E00", show.legend = FALSE) +
  scale_x_binned(limits = c(0, 60), breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60)) +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Distribution of ride duration under 1 hour: 2022") + 
  xlab("Ride duration in minutes") +
  ylab("Number of rides")+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

#filter
under_120mins <- data.table(df_2022[ride_length < 120,])
under_120mins$minBin <- cut(under_120mins$ride_length, breaks = c(seq(0, 120, by = 5)))

ggplot(under_120mins, aes(ride_length)) +
  geom_bar(aes(y = ..count..), color = "black", fill = "#D55E00", show.legend = FALSE) +
  scale_x_binned(limits = c(0, 120), breaks = seq(0, 120, 10), labels = c("0", "10", "20", "30", "40", "50", "60", "70", "80", "90", "100", "110", "120")) +  
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Distribution of ride duration under 2 hours: 2022") + 
  xlab("Ride duration in minutes")+
  ylab("Number of rides")


## ----Distribution of rides by time of day, include = FALSE-------------------------------------------------
#change formatting for this plot
under_60$started_at <- format(under_60$started_at, "%H:%M")
under_60$started_at <- lubridate::hour(hm(under_60$started_at)) * 100 + lubridate::minute(hm(under_60$started_at))

#transform to factor for correct ordering in plot
under_60$weekday_factor <- factor(under_60$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#get mean value
mean_value <- as.integer(mean(under_60$started_at))

#plot
ggplot(under_60, aes(x = weekday_factor, y = started_at)) + 
  geom_boxplot(fill = "purple") +
  scale_y_continuous(limits = c(0, 2400), breaks = seq(0, 2400, by = 100), labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24")) +
  ggtitle("Distribution of ride Start times: 2022") + 
  xlab("Weekday") +
  ylab("Ride start time Hours") +
  geom_hline(yintercept = mean_value, color = "black", linewidth = 0.5)


## ----Distribution of rides by time of day for Members, echo = FALSE----------------------------------------
#subset to get only members
under_60_members <- under_60[member_casual == "member",]

#transform to factor for correct ordering in plot
under_60_members$weekday_factor <- factor(under_60_members$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#find mean 
mean_value <- as.integer(mean(under_60_members$started_at))

#plot
ggplot(under_60_members, aes(x = day_of_week, y = started_at, group=weekday_factor)) + 
  geom_boxplot(fill = "#0072B2") +
  scale_y_continuous(limits = c(0, 2400), breaks = seq(0, 2400, by = 100), labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24")) +
  ggtitle("Distribution of ride Start times for Members: 2022") + 
  xlab("Weekday") +
  ylab("Ride start time Hours") +
  geom_hline(yintercept = mean_value, color = "black", linewidth = 0.5)


## ----Distribution of rides by time of day for Casual users, echo = FALSE-----------------------------------
#subset to get only members
under_60_casual <- under_60[member_casual == "casual",]

#transform to factor for correct ordering in plot
under_60_casual$weekday_factor <- factor(under_60_casual$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#find mean 
mean_value <- as.integer(mean(under_60_casual$started_at))

#plot
ggplot(under_60_casual, aes(x = day_of_week, y = started_at, group=weekday_factor)) + 
  geom_boxplot(fill = "#D55E00") +
  scale_y_continuous(limits = c(0, 2400), breaks = seq(0, 2400, by = 100), labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24")) +
  ggtitle("Distribution of ride Start times for Casual: 2022") + 
  xlab("Weekday") +
  ylab("Ride start time Hours") +
  geom_hline(yintercept = mean_value, color = "black", linewidth = 0.5)


## ----distribution of number of rides by Weekday, include = FALSE-------------------------------------------
#transform to factor for correct ordering in plot
df_2022$weekday_factor <- factor(df_2022$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#plot
ggplot(df_2022, aes(weekday_factor)) +
  geom_bar(fill = "#009E73", show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Distribution of rides by Weekday: 2022") + 
  xlab("Weekday") +
  ylab("Count")


## ----Breakdown of ride length by Weekday, echo = FALSE-----------------------------------------------------
#create bins for ride_length
under_1440mins$binned_ride_length <- cut(under_1440mins$ride_length, breaks = c(0, 10, 30, 60, 120, 240, 480, 720, 1440), labels = c("0-10", "10-30", "30-60", "60-120", "120-240", "240-480", "480-720", "720-1440"))

#create sums table to plot. # of rides in each bin, for each day
bd <- data.table(table(under_1440mins$binned_ride_length, under_1440mins$day_of_week))
bd <- rename(bd, day_of_week = V2)
bd <- rename(bd, binned_ride_length = V1)
bd <- rename(bd, value = N)

#transform to factor for correct ordering in plot
bd$weekday_factor <- factor(bd$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#colors
bin_colors <- c("#7ad151", "#44bf70", "#22a884", "#21918c", "#2a788e", "#355f8d", "#414487", "#440154")

#plot total # of rides 
ggplot(bd, aes(x = weekday_factor, y = value, fill = binned_ride_length)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = bin_colors) +
  ggtitle("Distribution of rides by Weekday: 2013") + 
  xlab("Weekday") +
  ylab("Total number of rides")


## ----clear up RAM, include=FALSE---------------------------------------------------------------------------
under_60 <- NULL
under_60_casual <- NULL
under_60_members <- NULL


## ----Breakdown of ride length by Weekday for Mmembers/Casual, echo = FALSE, fig.show="hold", out.width="50%"----
#subset only members
members_dt <- under_1440mins[member_casual == "member",]

#transformation
mdt <- data.table(table(members_dt$binned_ride_length, members_dt$day_of_week))
mdt <- rename(mdt, day_of_week = V2)
mdt <- rename(mdt, binned_ride_length = V1)
mdt <- rename(mdt, value = N)
#transform to factor for correct ordering in plot
mdt$weekday_factor <- factor(mdt$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#total # of members rides breakdown
ggplot(mdt, aes(x = weekday_factor, y = value, fill = binned_ride_length)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = bin_colors) +
  ggtitle("Distribution of rides by Members by Weekday: 2022") + 
  xlab("Weekday") +
  ylab("Total number of rides")

#subset casual users
casual_dt <- under_1440mins[member_casual == "casual" ,]

#transformation
cdt <- data.table(table(casual_dt$binned_ride_length, casual_dt$day_of_week))
cdt <- rename(cdt, day_of_week = V2)
cdt <- rename(cdt, binned_ride_length = V1)
cdt <- rename(cdt, value = N)

#transform to factor for correct ordering in plot
cdt$weekday_factor <- factor(cdt$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#total # of casual rides breakdown
ggplot(cdt, aes(x = weekday_factor, y = value, fill = binned_ride_length)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = bin_colors) +
  ggtitle("Distribution of rides by Casual Users by Weekday: 2022") + 
  xlab("Weekday") +
  ylab("Total number of rides")


## ----combined analysis for members vs casual users, echo = FALSE-------------------------------------------
#combine into one table for comparison
m <- data.table(aggregate(mdt$value, by=list(mdt$weekday_factor), FUN=sum))
c <- data.table(aggregate(cdt$value, by=list(cdt$weekday_factor), FUN=sum))
m  <- rename(m, day_of_week = Group.1)
m  <- rename(m, value = x)
c <- rename(c, day_of_week = Group.1)
c <- rename(c, value = x)
m$member <- "member"
c$member <- "casual"
combined_dt <- merge.data.frame(m, c, all = TRUE)
combined_dt <- combined_dt %>% group_by(day_of_week) %>% mutate(Percent_of_Total = value/sum(value)*100)

#make colors
my_colors <- c("#0072B2", "#D55E00")

#total # of members rides 
ggplot(combined_dt, aes(x = day_of_week, y = Percent_of_Total, fill = member)) + 
  geom_bar(stat = "identity") +
  ggtitle("Members vs Casual usage by Weekday: 2022") + 
  xlab("Weekday") +
  ylab("Total number of rides") +
  geom_hline(yintercept = 50, color = "black", linewidth = 1)+
  scale_fill_manual(values = my_colors)


## ----reset RAM, include=FALSE------------------------------------------------------------------------------
under_1440mins <- NULL
cdt <-NULL
bd <- NULL
c <- NULL
m <- NULL
combined_dt <- NULL
gc()


## ----Distribution of rides by Month for Members/Casual users, echo = FALSE, fig.show="hold", out.width="50%"----
#subset data
months_dt <- under_120mins
months_dt$minBin <- NULL

#get months in right format
months_dt$month <- sapply(months_dt$started_at, FUN = lubridate::month, label = TRUE, abbr = FALSE)

#transform to factor for correct ordering in plot
months_dt$month_factor <- factor(months_dt$month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

#calculate mean
casual_months <- months_dt[member_casual == "casual",]  
mean_value <- as.integer(nrow(casual_months)/11)

#plot bar plot 
ggplot(casual_months, aes(x = month_factor))+
  geom_bar(aes(y = ..count.., fill = ifelse(..count.. > mean_value, "#D55E00", "#0072B2")), show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Casual") + 
  xlab("Month") +
  ylab("Total number of rides") +
  geom_hline(yintercept = mean_value, color = "black", linewidth = 0.5)

#get subset of only members  
members_months <- months_dt[member_casual == "member",]  
mean_value <- as.integer(nrow(members_months)/11)
  
#plot    
ggplot(members_months, aes(x = month_factor))+
  geom_bar(aes(y = ..count.., fill = ifelse(..count.. > mean_value, "#D55E00", "#0072B2")), show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Members") + 
  xlab("Month") +
  ylab("Total number of rides") +
  geom_hline(yintercept = mean_value, color = "black", linewidth = 0.5)


## ----most popular start locations, echo = FALSE------------------------------------------------------------
#count unique starts
sid <- table(df_2022$start_station_id)
sid <- sid[order(sid, decreasing = TRUE)]
#head(sid, n = 10)
sn <- table(df_2022$start_station_name)
sn <- sn[order(sn, decreasing = TRUE)]
knitr::kable(head(sn, n = 10), "simple", col.names = NULL)


## ----most popular end locations, echo = FALSE--------------------------------------------------------------
#count unique ends
eid <- table(df_2022$end_station_id)
eid <- eid[order(eid, decreasing = TRUE)]
#head(eid, n = 10)
en <- table(df_2022$end_station_name)
en <- en[order(en, decreasing = TRUE)]
knitr::kable(head(en, n = 10), "simple", col.names = NULL)

#reset to save memory
sid <- NULL
sn <- NULL
eid <- NULL
en <- NULL

