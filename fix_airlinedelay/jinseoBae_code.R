#######FINAL PROJECT#########
####DATE: MAy 4th, 2019 #####
####NAME: Jin Seo  Bae#######
####### IST 387 #############

####Phase 1#### Mitigate Missing Data
library(readr) #to use read_csv()

setwd("//hd.ad.syr.edu/02/738e09/Documents/Downloads") #show where my csv file is in which directory
bae<- read_csv(file="bae.csv") #read csv file

View(bae) #View my assigned data
summary(bae) #To see overview of dataset before looking into deeper

bae$Destination.City [is.na(bae$Destination.City)]#There is no N/A in this column

bae$Orgin.City [is.na(bae$Orgin.City)]#There is no N/A in this column

bae$Satisfaction [is.na(bae$Satisfaction)]#There is no N/A in this column

bae$Airline.Status [is.na(bae$Airline.Status)]#There is no N/A in this column

bae$Airline.Code [is.na(bae$Airline.Code)]#There is no N/A in this column

bae$Airline.Name [is.na(bae$Airline.Name)]#There is no N/A in this column

bae$Age [is.na(bae$Age)]#There is no N/A in this column

bae$Gender [is.na(bae$Gender)]#There is no N/A in this column

bae$Price.Sensitivity [is.na(bae$Price.Sensitivity)]#There is no N/A in this column

bae$Year.of.First.Flight [is.na(bae$Year.of.First.Flight)]#There is no N/A in this column

bae$No.of.Flights.p.a. [is.na(bae$No.of.Flights.p.a.)]#There is no N/A in this column

bae$X..of.Flight.with.other.Airlines [is.na(bae$X..of.Flight.with.other.Airlines)]#There is no N/A in this column

bae$Type.of.Travel [is.na(bae$Type.of.Travel)]#There is no N/A in this column

bae$No..of.other.Loyalty.Cards [is.na(bae$No..of.other.Loyalty.Cards)]#There is no N/A in this column

bae$Shopping.Amount.at.Airport [is.na(bae$Shopping.Amount.at.Airport)]#There is no N/A in this column

bae$Eating.and.Drinking.at.Airport [is.na(bae$Eating.and.Drinking.at.Airport)]#There is no N/A in this column

bae$Class [is.na(bae$Class)]#There is no N/A in this column

bae$Day.of.Month [is.na(bae$Day.of.Month)]#There is no N/A in this column

bae$Flight.date [is.na(bae$Flight.date)]#There is no N/A in this column

bae$Origin.State [is.na(bae$Origin.State)]#There is no N/A in this column

bae$Destination.State [is.na(bae$Destination.State)]#There is no N/A in this column

bae$Scheduled.Departure.Hour [is.na(bae$Scheduled.Departure.Hour)]#There is no N/A in this column

bae$Departure.Delay.in.Minutes [is.na(bae$Departure.Delay.in.Minutes)]#There is 41 N/As in this column
#So I used its mean to replace N/As in this column
#replacing the NA values to the mean of the othere data in the column
bae$Departure.Delay.in.Minutes[is.na(bae$Departure.Delay.in.Minutes)]<-mean(na.omit(bae$Departure.Delay.in.Minutes))

bae$Arrival.Delay.in.Minutes [is.na(bae$Arrival.Delay.in.Minutes)] #There is 52 N/As in this column
#So I used its mean to replace N/As in this column
#replacing the NA values to the mean of the othere data in the column
bae$Arrival.Delay.in.Minutes [is.na(bae$Arrival.Delay.in.Minutes)] <- mean(na.omit(bae$Arrival.Delay.in.Minutes))

bae$Flight.cancelled [is.na(bae$Flight.cancelled)]#There is no N/A in this column

bae$Flight.time.in.minutes [is.na(bae$Flight.time.in.minutes)] #There is 52 N/As in this column
#So I used its mean to replace N/As in this column
#replacing the NA values to the mean of the othere data in the column
bae$Flight.time.in.minutes [is.na(bae$Flight.time.in.minutes)] <- mean(na.omit(bae$Flight.time.in.minutes))

bae$Flight.Distance [is.na(bae$Flight.Distance)]#There is no N/A in this column

bae$Arrival.Delay.greater.5.Mins [is.na(bae$Arrival.Delay.greater.5.Mins)]#There is no N/A in this column

bae$olong [is.na(bae$olong)] #There is no N/A in this column

bae$olat [is.na(bae$olat)] #There is no N/A in this column

bae$dlong [is.na(bae$dlong)] #There is no N/A in this column

bae$dlat [is.na(bae$dlat)] #There is no N/A in this column

###Phase 2### Summarize variables

#Following columns are formed with numeric variables, so I used histogram to see their shapes with all values in each variable
hist(as.numeric(bae$Age), main = "Age", xlab = "Age", ylab ="How many")#shape of this histogram is symmetric
hist(as.numeric(bae$Satisfaction), xlab = "satisfaction",main ="Satisfaction", breaks=5)#shape of this histogram is long left tail
hist(as.numeric(bae$Price.Sensitivity), xlab = "Price Sensitivity", main = "Price Sensitivity", breaks=5) #shape of this histogram is long right tail
hist(as.numeric(bae$Year.of.First.Flight), xlab = "Year of first flight" , main="Year of first flight",breaks = 12) #shape of this histogram is long right tail
hist(as.numeric(bae$No.of.Flights.p.a.), xlab = "Num of flights PA", main = "Num of Flights PA",breaks = 5)#shape of this histogram is long right tail
hist(as.numeric(bae$X..of.Flight.with.other.Airlines), xlab = "Other Airline flights", main = "Other Airline flights")#shape of this histogram is long right tail
hist(as.numeric(bae$Day.of.Month), main = "Day of Month", xlab = "Day of Month", breaks = 100) #Shape of histogram is Symmetric
hist(as.numeric(bae$No..of.other.Loyalty.Cards), xlab= "num of other loyalty cards", main = "Num of other loyalty cards", breaks = 10)#shape of this histogram is long right tail
hist(as.numeric(bae$Shopping.Amount.at.Airport), xlab= "Amount of shopping at Airport", main = "Amount of shopping at airport")#shape of this histogram is long right tail
hist(as.numeric(bae$Eating.and.Drinking.at.Airport), xlab = "Eating and drinking at airport", main ="Eating and Drinking at Airport", breaks = 5)#shape of this histogram is long right tail
hist(as.numeric(bae$Scheduled.Departure.Hour), xlab= "scheduled departure hour", main="Scheduled departure hour") #shape of this histogram is symmetric
hist(as.numeric(bae$Departure.Delay.in.Minutes), xlab= "departure delay in minutes", main="Departure delay in minutes", breaks = 10)#shape of this histogram is long right tail
hist(as.numeric(bae$Arrival.Delay.in.Minutes), xlab = "Arrival delay in minutes" , main="Arrival delay in minutes")#shape of this histogram is long right tail
hist(as.numeric(bae$Flight.time.in.minutes), xlab="flight time in minutes", main="Flight time in minutes")#shape of this histogram is long right tail
hist(as.numeric(bae$Flight.Distance), xlab = "Flight Distance", main="Flight Distance") #shape of this histogram is long right tail

#Following columns are formed with factor variables, So I used table() to see their values in several categories in each column
table(as.factor(bae$Gender)) #factors formed with female and male categories
table(as.factor(bae$Destination.City)) #Factors formed with various cities of states in the United States
table(as.factor(bae$Type.of.Travel)) #Factors formed with business travel, mileage tickeets, and personal travel categories
table(as.factor(bae$Origin.State)) #Factors formed with States in the United States categories
table(as.factor(bae$Orgin.City)) #factors formed with various cities of states in the United States
table(as.factor(bae$Destination.State))#Factors formed with States in the United States categories
table(as.factor(bae$Airline.Status)) #factors formed with Blue, Gold, Platinum, and Silver categories
table(as.factor(bae$Class)) #factors formed with Business, Economy, Economy Plus categories
table(as.factor(bae$Flight.date)) #factors formed with various dates from 1/1/14 to 3/31/14 categories
table(as.factor(bae$Airline.Code)) #factors formed with Airline code categories of 14 different airlines
table(as.factor(bae$Airline.Name))#factors formed with Airline name categories of 14 different airlines
table(as.factor(bae$Flight.cancelled)) #factors formed with Yes and no categories
table(as.factor(bae$Arrival.Delay.greater.5.Mins)) #factors formed with Yes and no categories

###Phase 3### Predict Satisfaction from Other Variables.

#To determine variables are numeric or factor
num_sat <- as.numeric(bae$Satisfaction)#numeric

unique(bae$Destination.City) #factor
fac_destcity <- as.factor(bae$Destination.City)

unique(bae$Type.of.Travel) #factor
fac_type_travel<- as.factor(bae$Type.of.Travel)

unique(bae$Origin.State) #factor
fac_origin_state <- as.factor(bae$Origin.State)

unique(bae$Orgin.City) #factor
fac_Origin_city <- as.factor(bae$Orgin.City)

unique(bae$Airline.Status) #factor
fac_airline_sta <- as.factor(bae$Airline.Status)

unique(bae$Age)#numeric
num_age <- as.numeric(bae$Age)

unique(bae$Gender) #factor
fac_gender <- as.factor(bae$Gender)

unique(bae$Price.Sensitivity)#numeric
num_sensiprice<- as.numeric(bae$Price.Sensitivity)

unique(bae$Destination.State) #factor
fac_DestiState<- as.factor(bae$Destination.State)

unique(bae$Year.of.First.Flight)#numeric
num_year_flight <- as.numeric(bae$Year.of.First.Flight)

unique(bae$No.of.Flights.p.a.)#numeric
num_numflight <- as.numeric(bae$No.of.Flights.p.a.)

unique(bae$X..of.Flight.with.other.Airlines)#numeric
num_otherariline <- as.numeric(bae$X..of.Flight.with.other.Airlines)

unique(bae$No..of.other.Loyalty.Cards)#numeric
num_otherloyalty <- as.numeric(bae$No..of.other.Loyalty.Cards)

unique(bae$Shopping.Amount.at.Airport) #numeric
num_amount_airport <- as.numeric(bae$Shopping.Amount.at.Airport)

unique(bae$Eating.and.Drinking.at.Airport)#numeric
num_eating_drinking <- as.numeric(bae$Eating.and.Drinking.at.Airport)

unique(bae$Scheduled.Departure.Hour)#numeric
num_Schedu_depar_hr <- as.numeric(bae$Scheduled.Departure.Hour)

unique(bae$Departure.Delay.in.Minutes)#numeric
num_Depar_delayMin <- as.numeric(bae$Departure.Delay.in.Minutes)

unique(bae$Arrival.Delay.in.Minutes)#numeric
num_Arriv_delayMin <-as.numeric(bae$Arrival.Delay.in.Minutes)

unique(bae$Flight.time.in.minutes)#numeric
num_Flight_timeMin <- as.numeric(bae$Flight.time.in.minutes)

unique(bae$Flight.Distance)#numeric
num_Flight_distance <- as.numeric(bae$Flight.Distance)

unique(bae$Class) #factor
fac_class <- as.factor(bae$Class)

unique(bae$Day.of.Month)#numeric
num_month <- as.numeric(bae$Day.of.Month)

unique(bae$Flight.date) #factor
fac_flightdate <- as.factor(bae$Flight.date)

unique(bae$Airline.Code) #factor
fac_airlinecode <- as.factor(bae$Airline.Code)

unique(bae$Airline.Name) #factor
fac_airlinename <- as.factor(bae$Airline.Name)

unique(bae$Flight.cancelled) #factor
fac_flightCancell <- as.factor(bae$Flight.cancelled)

unique(bae$Arrival.Delay.greater.5.Mins) #factor
fac_fivemin <- as.factor(bae$Arrival.Delay.greater.5.Mins)

#To compare one each to find columns that are significantly related with satisfaction column
lmseni <- lm(num_sat ~ num_sensiprice) #***
summary(lmseni)

lmyearflight <- lm(num_sat~ num_year_flight)
summary(lmyearflight) #none of them showed sigificnat relationship with satisfaction column


lmairlinesta <- lm(num_sat~ fac_airline_sta)#***
summary(lmairlinesta)

lmAcode <- lm(num_sat~ fac_airlinecode)
summary(lmAcode)
#just code EV seem to be little bit significant because p-value was closest to 0.03 but bigger (0.04) other than that code, there was no codes that are significantly related with customer's satisfaction

lmfacairname <- lm(num_sat~ fac_airlinename)
summary(lmfacairname) #none of them showed sigificnat relationship with satisfaction column

lmclass <- lm(num_sat~ fac_class)
summary(lmclass) #none of them showed sigificnat relationship with satisfaction column

lmdesCity <- lm(num_sat~ fac_destcity)
summary(lmdesCity) 
#There were few city of states seemed to have little relationship with satisfaction column, but hard to see their relationship as significant because they are over 0.03 of p-value

lmdesState <- lm(num_sat~ fac_DestiState)
summary(lmdesState) 
#There were few states seemed to have little relationship with satisfaction column, but hard to see their relationship as significant because they are over 0.03 of p-value


lmfivemin <- lm(num_sat~ fac_fivemin) #***
summary(lmfivemin)

lmflightCancell <- lm(num_sat~ fac_flightCancell)
summary(lmflightCancell)#none of them showed sigificnat relationship with satisfaction column

lmflightDate <- lm(num_sat~ fac_flightdate)
summary(lmflightDate)
#There were few dates seemed to have little relationship with satisfaction column, but hard to see their relationship as significant because they are over 0.03 of p-value

lmgender <- lm(num_sat~ fac_gender) #***
summary(lmgender)

lmOriCity <- lm(num_sat~ fac_Origin_city)
summary(lmOriCity)#none of them showed sigificnat relationship with satisfaction column

lmOriState <- lm(num_sat~ fac_origin_state)
summary(lmOriState)#none of them showed sigificnat relationship with satisfaction column

lmtypeTra <- lm(num_sat~ fac_type_travel) #Personal Travel ***,mileage ticket **
summary(lmtypeTra)

lmage <- lm(num_sat~ num_age) #***
summary(lmage)

lmamountAirport <- lm(num_sat~ num_amount_airport)
summary(lmamountAirport)#none of them showed sigificnat relationship with satisfaction column

lmArriveDelaymin <- lm(num_sat~ num_Arriv_delayMin) #***
summary(lmArriveDelaymin)

lmdepartDelaymin <- lm(num_sat~ num_Depar_delayMin) #***
summary(lmdepartDelaymin)

lmEatDrink <- lm(num_sat~ num_eating_drinking)
summary(lmEatDrink)#none of them showed sigificnat relationship with satisfaction column

lmFlightDistance <- lm(num_sat~ num_Flight_distance)
summary(lmFlightDistance)#none of them showed sigificnat relationship with satisfaction column

lmFlightTimemin <- lm(num_sat~ num_Flight_timeMin)
summary(lmFlightTimemin)#none of them showed sigificnat relationship with satisfaction column

lmmonth <- lm(num_sat~ num_month)
summary(lmmonth)#none of them showed sigificnat relationship with satisfaction column

lmNumflight <- lm(num_sat~ num_numflight) #***
summary(lmNumflight)

lmOtherAirline <- lm(num_sat~ num_otherariline) #**
summary(lmOtherAirline)

lmLoyalty <- lm(num_sat~ num_otherloyalty) #***
summary(lmLoyalty)

lmDeparthr <- lm(num_sat~ num_Schedu_depar_hr) #*
summary(lmDeparthr)

#Collect columns (*** only) that seemed to have a significant relationship with satisfaction column
#Then do multiple lm() with Satisfaction again by combining other columns that showed sigificant relationship with satistaction column.
#After combining them, by usign multiple lm(), I will determine whether they still has siginificant relationship with satisfaciton column comparing with other columns

lmcollected <- lm(num_sat ~ num_otherloyalty+num_numflight+num_Depar_delayMin+num_Arriv_delayMin+num_age+fac_type_travel+fac_gender+fac_fivemin+fac_airline_sta+num_sensiprice)
summary(lmcollected) #Found out num_numFlight, fac_type_travel-(PersonalTravel), fac_gender (Male), fac_fivemin (Yes), fac_airline_Sta (Gold, platinum, silver) are significantly still related wiht satisfaction column
#With summary() function, I could find out the information of Adjusted R-squre, which is more accurate than multiple r-squared, because
#if there are more values added in lm(), multiple r-squared number will increase no matter what so if it is multiple lm() then better to look into adjusted r-squared value.

###Phase 4### Map Low Satisfaction Routes
library(sqldf) #To bring Bae dataset to draw curve and usmap
library(ggplot2) #to plot them with curve
library(ggmap) #to get us map

#creating a new dataset with only the lowest satisfaction rate
usMap <- borders("state", colour="black", fill="white") #getting map of the United States
lowsat_bae <- sqldf("select * from bae where Satisfaction == 1")

#creating a plot with flight departure and destination
lowsat_usa_bae <- ggplot() + usMap +
  geom_curve(data=lowsat_bae,
             aes(x=olong, y=olat, xend=dlong, yend=dlat),
             col="#006400",
             size=.3,
             curvature=0.2) +
  geom_point(data=lowsat_bae,
             aes(x=olong, y=olat), 
             colour="blue",
             size=1.5) +
  geom_point(data=lowsat_bae,
             aes(x=dlong, y=dlat), 
             colour="black") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        plot.title=element_text(hjust=0.5, size=12))

#To see the result of plotting and curve into map
lowsat_usa_bae

###Phase 5### Make Sense of Low Satisfaction Segments. 
#To determine cause of customer's low satisfaction, I had to use histogram to see which values to trust, because if the graph is too right or left tailed,
#it is hard to see those values to be trusted and accurate to use for analasis.Especially because if they are tilted to right or left side, then mean of their values
#are very different than median values, unless their graphs are symmetric.This fact also helped me to understand what to compare with satisfaction column.

#Then, I will use phase 3 collected columns that showed significant relationship with satisfaction column by comparing with other (***)columns at single lm().
#Combining results from lm() and hist(), I determine which columns to use boxplot to see significant relationship and number of outliners.
#<number of flight, type of travel (personal travel), gender (male), airline status (gold, platinum, silver), Arrival delay greater than five minute (yes)>

#I runned a ggplot to draw bars to see the changes of each columns by satisfaction.
#example: if satisfaction is high, then what would change or which factor of each column has highest satisfaction.
ggplot(bae, aes(x= No.of.Flights.p.a., y = Satisfaction)) + geom_bar(stat="identity", fill = "blue") 
ggplot(bae, aes(x= No.of.Flights.p.a., y = Satisfaction)) + geom_area(fill = "blue")
#As num or flights PA is lower, satisfaction is higher. 

ggplot(bae, aes(x= Type.of.Travel, y = Satisfaction)) + geom_bar(stat="identity",fill = "pink")
#Business travel has highest satisfaction, besides business travel, personal travel has higher satisfacion then mileage tickets
#For types of travel, Personal travel showed significant relationship with satisfaction than other factors (when I was using lm() to see p-value)

ggplot(bae, aes(x= Gender, y = Satisfaction)) + geom_bar(stat="identity",fill = "black")
#There were more female felt satisfied with this airline than male
#However, according to lm() between gender column and satisfaction, male get more affected on satisfaction (more significantly related than female)

ggplot(bae, aes(x= Airline.Status, y = Satisfaction)) + geom_bar(stat="identity", fill = "dark green")
#For Aireline Status, blue has highest satisfation, second highest is silver. However, when I was runnin lm() between satisfaciton column and
#Airline status column, gold, platinum, and silver showed very strong significant relationship with satisfaction amount.

ggplot(bae, aes(x= Arrival.Delay.greater.5.Mins, y = Satisfaction)) + geom_bar(stat="identity", fill = "yellow")
#For arrival delay greater 5 min column, Of course People who waited more than 5 mins are significantly related with satisfaction of their travel

#All of this graphs and lm() proved that number of satisfaction does not ALWAYS significantly related with the factors. 
#because some of them were not even in the list when I was doing lm() between them, but when I was looking at ggplot (geom_Bar) it shows highest
#satisfaction from customers. So highest satisfaction does not always prove their relationshp (significantly related)

#Therefore, according to my research, people who waited more than 5 min of arrival delay and airline status, and reason of travel affects a lot with their air travel.