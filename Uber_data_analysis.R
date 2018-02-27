# Uber data analysis
library(tidyverse)
library(lubridate)
library(ggthemes)
library(reshape2)

setwd("/Volumes/Data/DataSciences/Upgrad/Exploratory Data Analysis")
uber_data_set <- read.csv("Uber Request Data.csv",stringsAsFactors = F)
head(uber_data_set,3)
str(uber_data_set)

# Standardizing the datasets
uber_data_set$Request.id <- as.factor(uber_data_set$Request.id)
uber_data_set$Driver.id <- as.character(uber_data_set$Driver.id)

# converting the datasets to lowercase.
uber_data_set$Pickup.point <- tolower(uber_data_set$Pickup.point)
uber_data_set$Status <- tolower(uber_data_set$Status)

# Parsing the "request.timestamp" dates and correcting the "/" time issues request
uber_data_set$new_request_ts <- parse_date_time(x = uber_data_set$Request.timestamp,orders = c("%d-%m-%Y %H:%M:%S","%d/%m/%Y %H:%M"))

# cleansing the "drop.timestap"
uber_data_set$new_drop_ts <- parse_date_time(x = uber_data_set$Drop.timestamp,orders = c("%d-%m-%Y %H:%M:%S","%d/%m/%Y %H:%M"))

head(uber_data_set)

#------------------------------------
# Parsing the dataset for duplicates and Unique values
#------------------------------------

# Uniques
sapply(uber_data_set, n_distinct)

# Uniques in the dataset
unique(uber_data_set$Status)
unique(uber_data_set$Pickup.point)

# duplicates
sapply(uber_data_set, function(x) sum(duplicated(x)))

# find nas in the dataset
sapply(uber_data_set, function(x) sum(is.na(x)))

# duplicate time_stamps in the data
uber_data_set[which(duplicated(uber_data_set$new_request_ts)),]

# Datasets with NAs
# Dataset with "No Cars Available" have no Driver.IDs and "No Drop Stamps"
# replacing the missing Driver.IDS for "No Cars Available"
uber_data_set$Driver.id[which(is.na(uber_data_set$Driver.id))] <- "MissingDriverID"

# Converted Driver ID to factors
uber_data_set$Driver.id <- as.factor(uber_data_set$Driver.id)

#-------------------------------------------------
# Deriving the additional column variables from the request.timestamp
#-------------------------------------------------

# extract day and hours
uber_data_set[,"request_day"] <- format(uber_data_set$new_request_ts,"%d")
# extract hours
uber_data_set[,"request_hours"] <- format(uber_data_set$new_request_ts,"%H")

#-------------------------------------------------
# Extracting date,hrs, mins, the drop.timestamp
#-------------------------------------------------

# extract day and hours
uber_data_set[,"drop_day"] <- format(uber_data_set$new_drop_ts,"%d")
# extract drop hours
uber_data_set[,"drop_hrs"] <- format(uber_data_set$new_drop_ts,"%H")

#-------------------------------------------------
# Creating a Derieved metric trip.duration
#-------------------------------------------------
req_drop_time_difference_mins <- difftime(time1 = uber_data_set$new_drop_ts,time2 = uber_data_set$new_request_ts,units = c("mins"))

# rounding upto 1 digits
uber_data_set$trip_duration <- round(req_drop_time_difference_mins,digits = 1)


# Dividing the hours into timeslots


# Morning
#-------
# Early morning    4am to 8 am
# Late morning     8 am to 12pm
# 
# Afternoon       12 pm to 6 pm
#-------
# Early afternoon   1 to 3 pm
# Late afternoon    3 to 6 pm
# 
# Evening          6 pm to 12 pm
#-------
# Early evening   6 to 9 pm
# Late evening    9 pm to 12 pm
#-------
# Night          11 pm to 4 am 

#--------------------------------------------------------------
# Logic #1: to convert hours to timeslots ( this logic runs slower)
#--------------------------------------------------------------
# x <- as.numeric(uberDF$request_hours)
# 
# case_when(
#   
#   x <= 4    ~  "night",
#   x <= 8    ~  "early morning",
#   x < 12    ~  "late morning",
#   x <= 15   ~  "early afternoon",
#   x <= 18   ~  "late afternoon",
#   x <= 21   ~  "early evening",
#   x <= 23   ~  "late evening",
#   x <= 24   ~  "night",  
#   TRUE ~ as.character(x)
# )
# #--------------------------------------------

# Logic #2 to convert to timeslots


uber_data_set$timeslot <- cut(as.numeric(uber_data_set$request_hours), 
                              breaks=c(0,4,8,12,15,18,21,24), 
                              labels=c("night","early_morning","late_morning","early_afternoon","late_afternoon","early_evening","late_evening"),
                              include.lowest=T)
head(uber_data_set)


# Preserving the uber_data_set and and Using UberDF for analysis
uberDF <- uber_data_set

#Writing the data to a csv for tableau analysis
write.csv(x = uberDF ,file = "uberDF.csv")

#------------------------------------------------------------------------------
# There are six attributes associated with each request made by a customer:
#   
# Request id: A unique identifier of the request
# Time of request: The date and time at which the customer made the trip request
# Drop-off time: The drop-off date and time, in case the trip was completed 
# Pick-up point: The point from which the request was made
# Driver id: The unique identification number of the driver
# Status of the request: The final status of the trip, that can be either completed, cancelled by the driver or no cars available
# Derived metrics are: timeslot, Req_hours, trip duration
#------------------------------------------------------------------------------

#********************
# Segmented Univariate Analysis
#********************

# identifying data patterns
# boxplot to calcuate the median trip duration and identify outliers in data
ggplot(uberDF,aes(x = Pickup.point,y = trip_duration)) + 
  geom_boxplot(na.rm = T) + 
  scale_y_continuous(expand = c(0,0))

# Segmented Unvariate analysis of each Status by Pickup.Point
ggplot(uberDF,aes(Status,fill = Pickup.point)) + 
  geom_bar() + 
  geom_text(stat="count",aes(label=..count..),position = position_stack(vjust = 0.5)) +
  theme(legend.position = 1,axis.text = element_text(angle = 90, hjust = 1))+
  labs(title = "Status by Number of trips") + ylab("Number of Trips")

# Univariate analysis of timeslots
ggplot(uberDF,aes(timeslot,fill=Pickup.point)) + geom_bar() +
  geom_text(stat="count",aes(label=..count..),position = position_stack(vjust = 0.5)) +
  theme(legend.position = 1,axis.text = element_text(angle = 45, hjust = 1)) +
  labs(title="Count of trip frequency by timeslots") + ylab("Number of trips")


# Plotting Request timestamp (hours)
ggplot(uberDF,aes(uberDF$request_hours,fill = Pickup.point)) + geom_bar() + xlab ("Request_Time(hours)") +
  theme(legend.position = 1,axis.text = element_text(angle = 45, hjust = 1)) +
  labs(title="Trip frequency by each hour") + ylab("Number of trips")


########################################################################################################################
# Breaking down the analysis further - Segmented Univariate analysis
# Analysis of of Demand and Supply at each HOUR OF request_timestamp FOR 11TH 12, 13, 14, AND 14TH OF JULY.
########################################################################################################################


# NOTE: As the data is purely comparitive analysis, Using bar charts/Column charts predominantly using goem_col(), geom_bar() for 2-D analysis of data for comparision.

# Organic Supply
uber_trips <- uber_data_set %>%
  group_by(Pickup.point,Status,request_hours) %>% 
  summarise(no_trips=n()) 


head(uber_trips)

# Total Supply+ Demand. 
ggplot(uber_trips,aes(x = request_hours, y = no_trips,fill=Status)) +geom_col() + 
  facet_wrap(c("Pickup.point"), scales = "free_y",nrow = 2) +
  labs(title = "Total Supply + Demand") + labs(subtitle = "Demand = cancelled + no cars vailable; Supply = trip completed") +
  ylab("Number of trips") + xlab("Request(hours)") +
  scale_color_tableau("tableau20") + 
  theme_classic() +
  theme(legend.position = "bottom")

# Plot for "Supply" # of requests at airport and city request times when the 'trip completed'.

ggplot(subset(uber_trips,Status == "trip completed"),aes(x = request_hours, y = no_trips,fill = Status)) +geom_col() + 
  facet_wrap(c("Pickup.point"), scales = "free_y",nrow = 2) +
  ylab("Number of trips") + xlab("Request(hours)") + labs(title="Supply - Trips Completed") +
  scale_fill_tableau("tableau10")  +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(angle = 90, hjust = 1))


# Total Demand when "no cars available" and trips "cancelled"
# Plot for % of airport and city request times when demand is higher. 
ggplot(subset(uber_trips,Status != "trip completed"),aes(x = request_hours, y = no_trips, fill=Status)) +geom_col() + 
  facet_wrap(c("Pickup.point"), scales = "free_y",nrow = 2) +
  ylab("Number of trips") + xlab("Request(hours)") + labs(title="Total Demand - cancelled, no cars available") +
  scale_fill_tableau("tableau10") +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(angle = 45, hjust = 1))



#############################################
### RESULTS EXPECTED for the assignment
#############################################



#############################################
# 1. Visually identify the most pressing problems for Uber. 
#############################################

# Hint: Create plots to visualise the frequency of requests that get cancelled or show 'no cars available'; 
#
# identify the most problematic types of requests (city to airport / airport to city etc.) 
# and the time slots (early mornings, late evenings etc.) using plots
# Problematic request types are Cancellations and "No Cars Available"


ggplot(subset(uberDF,Status!="trip completed"),aes(timeslot, fill=Status)) + 
  geom_bar() +  
  geom_text(stat="count",aes(label=..count..),position = position_stack(vjust = 0.5),check_overlap = T) +
  facet_wrap(~Pickup.point,scales = "free_y",nrow = 2) +
  labs(title = "Problem trip types from airport/city and city/airport") + ylab("trip request frequency ") +
  scale_fill_tableau("tableau10") +
  theme_classic() +
  theme(legend.position = "bottom",axis.text = element_text(angle = 45, hjust = 1))

# Commentary
# At Airport:
# Problematic "timeslots at the airport" --> early evening (6pm - 9pm) and late_afternoon (3 to 6 pm) 
# Problematic Request types: "No Cars available"

# In City
# Problematic "timeslots in city" --> early_morning (4 am to 8 am) and late_morning (8am to 11:59am)
# Problematic request types: "cancelled"


#############################################
# 2. Find out the gap between supply and demand and show the same using plots:
#############################################


# Find the time slots when the highest gap exists
# Total Demand (D) = Sum of Total Requests When "no cars available" + Total "Cancellations"
# Toal Supply (S) = Total requests that show the status "trip completed"
# Demand Supply Gap (G) = D-S
# Less the G, more organic is the supply. Conversely, Higher the gap large the demad


uberDF_DemndSupply_gap <- uberDF %>% 
  group_by(Pickup.point,timeslot,Status) %>% 
  tally() %>%               # used for row count
  spread(Status,n) %>%      # spread the status variables
  mutate(demand=sum(cancelled,`no cars available`),gap=sum(cancelled,`no cars available`)-`trip completed`) %>% # calculate the gpa
  arrange(desc(gap))

head(uberDF_DemndSupply_gap)
# write the it csv file to Tableau analysis
write.csv(uberDF_DemndSupply_gap,file = "uber_DemandSupply_gap.csv")

#############################################
# 2.(a) timeslots the time slots when the highest gap exists
#############################################

# highest gap for City, early_morning(4am to 9am), Highest gap for Airport is early_evening (6pm to 9pm)
head(uberDF_DemndSupply_gap,2)

# Pickup.point    timeslot    cancelled `no cars available` `trip completed`   gap
#   airport     early_evening        72                 797              218   651
#    city       early_morning       653                 309              373   589

# least gap for City and airports 
# Pickup.point      timeslot cancelled `no cars available` `trip completed`   gap
# city        early_evening        35                  41              251  -175
# airport     early_morning        15                  14              308  -279
tail(uberDF_DemndSupply_gap,2)

#############################################
# 2(b) Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots
#############################################

# Answer:- From the below analysis
#At the airport - For an airport to city - the early_evening slot is problematic with "no cars available" with largest demand gap
#In the city - For a city to airpot trip - the early_morning slot is problematic with "cancelled" requests with largest demand gap


# using melt function to reshape the different status and gaps 
# Comparitive study using the line plot studying the variations of data.
# Using the Demand_supply gap, and melt() from reshape 2 library
# plotting the intercepts here timeslots are low and high



melt(data = uberDF_DemndSupply_gap,value.name = "value") %>%   # melting Demand Supply_gap
  subset(variable %in% c("demand","gap","trip completed")) %>% # Subset demand, gap and trip completed
  ggplot(aes(x = timeslot,y=value)) + 
  geom_line(aes(col = variable,group=variable)) +              # using line chart to study trend and Demand Supply gap and analysis
  scale_colour_manual(values = c("bisque3", "azure3","darkblue")) +   # Using manual Colors
  geom_point(aes(colour=variable, shape=variable, group=variable), size=2) + # representing Points +
  facet_wrap(~Pickup.point,scales = "free_y",ncol = 2) +
  geom_vline(xintercept = c(2,6),linetype="dashed", size=0.2) +  # represents max and min gaps at airport and city
  theme_classic() + 
  labs(title ="Demand and Supply Gap") + ylab("trip frequency") + 
  labs(subtitle = "Problem trips from airport to city and city to airport") +
  theme(axis.text = element_text(angle = 60, hjust = 1),legend.position = "bottom")
  
  

  


#############################################
# 3. What do you think is the reason for this issue for the supply-demand gap? 
# Write the answer in less than 100 words. You may accompany the write-up with plot(s).
#############################################

#--------------------------------------------------------------------------------------------------
# To analyse the problem we can perform a segmented univariate analysis.
# segmented Univariate analysis by each Request type, "no cars available","trip completed", "cancelled"
#--------------------------------------------------------------------------------------------------


# Checking the demand when "no cars avaiable", time of requests, Airport and City facet by pickup.point.
# using a bar graph to compare the requests

ggplot(subset(uberDF,Status %in% c("no cars available","trip completed")), aes(x = factor(request_hours),fill = Status)) + geom_bar() + 
  facet_wrap(~c(Status), scales = "free_y", nrow = 2, strip.position = "top") +
  labs(title = "Demand and Supply when 'No Cars Available'") +
  xlab("Hours") + ylab("trip frequency") +
  scale_fill_tableau("tableau10") +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(angle = 90, hjust = 1))

# Commentary 
# At the airpot "no cars available" status is acute between 5pm to 22pm.
# From "trips completed" chart it appears that supply is constant and not adequate to meet the peak demand
# indicates, there are more flights arriving at the airport and no taxis are available to serve the passengers at the airport
# This is also the time when partner drivers end their schedule and less taxis are available from the city.




# Bar Plot for Status = Cancelled, time of reqeusts, Airport and City facet by pickup.point. comparing the reuqest count
#--------------------------------------------------------------------------------------------------
ggplot(filter(uberDF,Status %in% c("cancelled","trip completed"),Pickup.point == "city"), aes(x = factor(request_hours),fill = Status)) + geom_bar() + 
  facet_wrap(~Status, scales = "free_y", nrow = 2, strip.position = "top") + 
  labs(title = "Demand and Supply when trips 'Cancelled'") +
  xlab("Hours") + ylab("trip frequency") +
  scale_fill_tableau("tableau10") +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(angle = 90, hjust = 1))

# Commentary:
# "cancelled" plot implies that, from City, a large number of cancellations are being identified in the City between 4 am and 9 am.
# "trip completed" chart implies that "trip completed" is normal although not adequate to meet the demand.
#  Several passengers are taking the morning flights and hence there are more flights departing from the city.
# this demand suddenly dips after 9am, and it indicates that are less flights departing from the city. Hence lesser demand.
# It also indicates that there no incoming flights during this time as the trip completed doesnot peak. So drivers fearing idle waiting time, would cancel the trips, between 4 am to 9 am. 



# Over all Demand and Supply of Trips by Request hours _ frequency
#--------------------------------------------------------------------------------------------------

ggplot(uberDF,aes(x = factor(timeslot))) + geom_bar(aes(fill = Pickup.point)) +
  facet_wrap(~Status, nrow = 3,  scales = "free_y") +
  labs(title = "Supply Demand of Trips") +
  xlab("Hours") + ylab("trip frequency") +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_tableau('tableau20') +
  theme(axis.text = element_text(angle = 45, hjust = 1))



#Summary:
# Possible reason for "no cars available" could be:
# From City, despite good demand there tends be a large number of cancelations betwee 4am and 10 am.
# From Airpot, there tends be significant Demand between 5pm and 11pm and there are "no cars available"
# Reason for "no cars available" from the trend could be several late evening flights arriving into the city and drivers completing their day in the evening coincide

# Possible Reason for "cancelled trips" could be:
# Morning flights arrive at the airport at the later part of the day.
# Possibility #1: longer idle times at the airport as the data suggests as lesser flights arrive later part of late_morning morning and early_afternoon.
# Possibility #2: too early for the drivers to start the trip


#############################################
# 4. Recommend some ways to resolve the supply-demand gap.
#############################################

# 1. "incentivize the trips" when the demand for taxis is higher, this can push the drivers meet the demand to some extent
# 2. "invite new partner drivers"
# 3. "Work shifts"- encourage drivers to work in different 8 hour overlapping shifts between 3am - 11 am, 8 am to 4pm,  3 pm to 11 pm 9 pm to 5 am stagger work the drivers
# 4. "inform drivers" of arrivals and departures of flights in-advance.
# 5. "Charge airport convenience fees" for airport trips to compensate the driver idle times.
# 6. "Allow passengers" to pre-book airport taxis






















































