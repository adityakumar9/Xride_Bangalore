library(dplyr)
library(car)
library(ggplot2)
library(XML)
library(irr)
library(ROCR)
library(gains)
library(sqldf)
library(RSQLite)
library(ggmap)
library(lubridate)
library(caret)

# Set Location ------------------------------------------------------------

data<- read.csv("C:\\Users\\Shubham\\Downloads\\hiring\\data.csv", sep = ",", stringsAsFactors = FALSE)
dim(data)
names(data)


# Senety Checks -----------------------------------------------------------

head(data$online_booking)
unique(data$online_booking)

sapply(data, function(x)length(unique(x)))
class(data$online_booking)

XRID_types<-sapply(data, function(x)class(x))

# Compute Missing Values --------------------------------------------------

Total_Missing<-sapply(data, function(x)sum(x=="NULL"))
Total_Row<- nrow(data)
Missing_Percentage<- (Total_Missing/Total_Row)*100
Missing_Percentage


# data cleaning -----------------------------------------------------------

colnames(data)

str(data)

summary(data)

data$package_id<- as.factor(data$package_id)
data$package_id<- as.numeric(data$package_id)

sum(data$package_id=="NULL")
sum(data$to_area_id=="NULL")
sum(data$from_city_id=="NULL")
sum(data$to_city_id=="NULL")

sum(data$from_date=="NULL")
sum(data$to_date=="NULL")

sum(data$to_date=="NULL")/nrow(data)
data%>%group_by(data$package_id)%>%summarize(n())
sum(is.na(data$travel_type_id))

# basic plots -------------------------------------------------------------

hist(data$travel_type_id, col = "orange", labels = TRUE)

columns<- table(data$online_booking, data$mobile_site_booking)
barplot(columns, col = c("red","yellow"), beside = TRUE)

#create visualization ----------------------------------------------------

library(leaflet)
my_map<- leaflet()%>% addTiles()
my_map

my_map<- my_map%>%addMarkers(lat = 12.77663 , lng = 77.38693 )

From_df<- data.frame(lat= runif(467, min = 12.77663, max = 13.366072),
                     long= runif(467, min = 77.38693, max = 77.78642))

From_df
head(From_df)
dim(From_df)

From_df%>% leaflet() %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())
colnames(data)


p<-ggplot(data,aes(x=package_id))
p+geom_bar() 


# Univarite Analysis ------------------------------------------------------

p1<-ggplot(data, aes(x=package_id, y=..density.., fill=1))
p1+geom_histogram(stat = "bin", bins = 15)+geom_density(alpha=0.5)+
  guides(fill=FALSE) + labs(y="Density", title = "Density Graph")+
  theme_bw()+ facet_grid(~online_booking)+ theme(plot.title = element_text(size=10, hjust = 0.5))


p2<-ggplot(data, aes(x=travel_type_id, y=..density.., fill=1))
p2+geom_histogram(stat = "bin", bins = 15)+geom_density(alpha=0.5)+
  guides(fill=FALSE) + labs(y="Density", title = "Density Graph")+
  theme_bw()+ facet_grid(~online_booking)+ theme(plot.title = element_text(size=10, hjust = 0.5))



p3<-ggplot(data, aes(x=package_id, y=..density.., fill=1))
p3+geom_histogram(stat = "bin", bins = 15)+geom_density(alpha=0.5)+
  guides(fill=FALSE) + labs(y="Density", title = "Density Graph")+
  theme_bw()+ facet_grid(~mobile_site_booking)+ theme(plot.title = element_text(size=10, hjust = 0.5))


p4<-ggplot(data, aes(x=travel_type_id, y=..density.., fill=1))
p4+geom_histogram(stat = "bin", bins = 15)+geom_density(alpha=0.5)+
  guides(fill=FALSE) + labs(y="Density", title = "Density Graph")+
  theme_bw()+ facet_grid(~mobile_site_booking)+ theme(plot.title = element_text(size=10, hjust = 0.5))

p5<-ggplot(data, aes(x=package_id, y=..density.., fill=1))
p5+geom_histogram(stat = "bin", bins = 15)+geom_density(alpha=0.5)+
  guides(fill=FALSE) + labs(y="Density", title = "Density Graph")+
  theme_bw()+ facet_grid(~travel_type_id)+ theme(plot.title = element_text(size=10, hjust = 0.5))


p6<-ggplot(data, aes(x=travel_type_id, y=..density.., fill=1))
p6+geom_histogram(stat = "bin", bins = 15)+geom_density(alpha=0.5)+
  guides(fill=FALSE) + labs(y="Density", title = "Density Graph")+
  theme_bw()+ facet_grid(~Car_Cancellation)+ theme(plot.title = element_text(size=10, hjust = 0.5))



# Missing value imputation for lat long -----------------------------------
str(data)
summary(data)
sum(data$from_lat=="NULL")
sum(data$from_long=="NULL")

sum(data$to_lat=="NULL")
sum(data$to_long=="NULL")

data$from_lat[which(data$from_lat=="NULL")]<- "12.97"
data$from_long[which(data$from_long=="NULL")]<- "77.64"

data$to_lat[which(data$to_lat=="NULL")]<- "12.98"
data$to_long[which(data$to_long=="NULL")]<- "77.64"


# Calculate Distance between two points -----------------------------------

data$from_long<- as.numeric(data$from_long)
data$to_long<- as.numeric(data$to_long)

data$from_lat<- as.numeric(data$from_lat)
data$to_lat<- as.numeric(data$to_lat)

long1<- data$from_long
long2<- data$to_long

lat1<- data$from_lat
lat2<- data$to_lat


gcd.slc <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long1-long2)) * R
  d<- d*3.1415926/180
  return(d) # Distance in km
}

distance_in_Km<-data.frame(gcd.slc(long1, lat1, long2, lat2))


xrid<- list[]
xrid<-distance_in_Km
colnames(xrid)<-"distance_in_KM"
data<- cbind(data, xrid)


# Split Date and Time -----------------------------------------------------

mdyHM<- mdy_hm(data$from_date)
mdyHM

data$From_XRideDate<- as.Date(mdyHM)
Xtimes<- format(mdyHM, "%H.%M")
data$From_Time<- Xtimes
data$From_Month<- format(data$From_XRideDate, "%b")

mdybk<- mdy_hm(data$booking_created)
head(mdybk)

data$Booking_Date<- as.Date(mdybk)
XtimesBk<- format(mdybk, "%H.%M")
data$Booking_Time<- XtimesBk
data$Booking_Month<- format(data$Booking_Date, "%b")

data$sub_distance<- cut(data$distance_in_KM, 3, c("Short_D", "Medium_D", "Long_D"))

# Multivariate Analysis ---------------------------------------------------

p7<- ggplot(data, aes(x=travel_type_id), alpha=0.5 )
p7+ geom_bar(stat = "count", aes(fill=sub_distance), position = "dodge") + facet_grid(~mobile_site_booking)+
  labs(x="travel_type_id", fill="sub_distance", y="Count", title="travel_type_id impact on mobile_site_booking")+
  theme(legend.position = c(0.75,0.9), plot.title = element_text(size=16, hjust = 0.5), 
        legend.key.size = unit(0.5, "cm"), legend.title= element_text(size=8), legend.text = element_text(size=8),
        axis.title.x =element_text(size=16, vjust = 0.5, hjust = 0.5), axis.title.y = element_text(size=16, vjust = 0.5, hjust = 0.5),
        legend.direction = "horizontal", panel.background = element_blank())


library(scales)


p7<- ggplot(data, aes(x=From_Date), alpha=0.5 )
p7+ geom_bar(stat = "count", aes(fill=sub_distance), position = "dodge")+ scale_x_date(breaks = date_breaks("months"), labels = date_format("%b")) +
  facet_grid(~mobile_site_booking)+
  labs(x="From_Month", fill="sub_distance", y="Count", title="From_Date impact on mobile_site_booking")+
  theme(legend.position = c(0.75,0.9), plot.title = element_text(size=16, hjust = 0.5), 
        legend.key.size = unit(0.5, "cm"), legend.title= element_text(size=8), legend.text = element_text(size=8),
        axis.title.x =element_text(size=16, vjust = 0.5, hjust = 0.5), axis.title.y = element_text(size=16, vjust = 0.5, hjust = 0.5),
        legend.direction = "horizontal", panel.background = element_blank())

p8<- ggplot(data, aes(x=From_Month), alpha=0.5 )
p8+ geom_bar(stat = "count", aes(fill=sub_distance), position = "dodge")+ facet_grid(~mobile_site_booking)+
  labs(x="From_Month", fill="sub_distance", y="Count", title="From_Date impact on mobile_site_booking")+
  theme(legend.position = c(0.75,0.9), plot.title = element_text(size=16, hjust = 0.5), 
        legend.key.size = unit(0.5, "cm"), legend.title= element_text(size=8), legend.text = element_text(size=8),
        axis.title.x =element_text(size=16, vjust = 0.5, hjust = 0.5), axis.title.y = element_text(size=16, vjust = 0.5, hjust = 0.5),
        legend.direction = "horizontal", panel.background = element_blank())


p9<- ggplot(data, aes(x=package_id), alpha=0.5 )
p9+ geom_bar(stat = "count", aes(fill=sub_distance), position = "dodge")+ facet_grid(~travel_type_id)+
  labs(x="package_id", fill="sub_distance", y="Count", title="package_id impact on travel_type_id")+
  theme(legend.position = c(0.75,0.9), plot.title = element_text(size=16, hjust = 0.5), 
        legend.key.size = unit(0.5, "cm"), legend.title= element_text(size=8), legend.text = element_text(size=8),
        axis.title.x =element_text(size=16, vjust = 0.5, hjust = 0.5), axis.title.y = element_text(size=16, vjust = 0.5, hjust = 0.5),
        legend.direction = "horizontal", panel.background = element_blank())


# SQL Queries for extract the data ----------------------------------------

XData<-data
colnames(XData)

table1<- sqldf("SELECT DISTINCT (vehicle_model_id) FROM XData ORDER BY vehicle_model_id  ")
table1

table2<- sqldf("SELECT vehicle_model_id, COUNT (vehicle_model_id) AS Count
               FROM XData GROUP BY vehicle_model_id ")
table2

table3<- sqldf("SELECT package_id, COUNT (package_id) AS Count
               FROM XData GROUP BY package_id ")
table3

table4<- sqldf("SELECT travel_type_id, COUNT (travel_type_id) AS Count
               FROM XData GROUP BY travel_type_id ")
table4

table5<- sqldf("SELECT from_area_id, COUNT (from_area_id) AS Count
               FROM XData GROUP BY from_area_id ")

table6<- sqldf("SELECT From_XRideDate , COUNT (From_XRideDate) AS Count
               FROM XData WHERE travel_type_id= 2 GROUP BY From_XRideDate ")
table6

table7<- sqldf("SELECT From_XRideDate , COUNT (From_XRideDate) AS Count
               FROM XData WHERE travel_type_id= 2 AND online_booking=1  GROUP BY From_XRideDate ")
table7

table8<- sqldf("SELECT travel_type_id, COUNT (travel_type_id) AS Count_Of_travel_type_id,
                SUM(package_id) AS Total_Package
               FROM XData WHERE online_booking=1  GROUP BY travel_type_id ")
table8

table9<- sqldf("SELECT From_Month, COUNT (From_Month) AS Count_Of_From_Month,
                SUM(distance_in_KM) AS Total_distance_in_KM
               FROM XData WHERE online_booking=1  GROUP BY From_Month ")
table9


table10<- sqldf("SELECT From_Month, COUNT (From_Month) AS Count_Of_From_Month,
                SUM(distance_in_KM) AS Total_distance_in_KM
               FROM XData WHERE mobile_site_booking=1 GROUP BY From_Month  ")
table10

table11<- sqldf("SELECT MAX(distance_in_KM ) AS distance, From_Month,
                COUNT(From_Month) AS Month
               FROM XData WHERE mobile_site_booking=1 GROUP BY From_Month ")
table11


table12<- sqldf("SELECT vehicle_model_id, COUNT(vehicle_model_id ) AS Total_vehicle_model_id
                  FROM XData WHERE online_booking=1 GROUP BY vehicle_model_id ")
table12


table13<- sqldf("SELECT sub_distance, SUM(distance_in_KM ) AS Total_Distance
                  FROM XData WHERE travel_type_id=2 GROUP BY sub_distance ")
table13


table14<- sqldf("SELECT From_Month  , SUM(distance_in_KM ) AS Total_Distance
                  FROM XData WHERE travel_type_id=2 GROUP BY From_Month ")
table14


table15<- sqldf("SELECT From_Month  , AVG(distance_in_KM ) AS AVG_Distance, 
                  COUNT(from_area_id) AS No_Of_Area_Id
                  FROM XData WHERE travel_type_id=2 GROUP BY From_Month ")
table15

# Selecting Variables -----------------------------------------------------
colnames(data)
summary(data)
str(data)

data$From_Time<- as.numeric(data$From_Time)
data$Booking_Time<- as.numeric(data$Booking_Time)


data$From_Time_zone<- cut(data$From_Time, 8, c("0_to_3am", "3_to_6am", "6_to_9am", "9_to_12am", 
                                               "12_to_15pm", "15_to_18pm", "18_to_21pm", "21_to_24pm"))

data$Booking_Time_zone<- cut(data$Booking_Time, 8, c("0_to_3am", "3_to_6am", "6_to_9am", "9_to_12am", 
                                                     "12_to_15pm", "15_to_18pm", "18_to_21pm", "21_to_24pm"))


Xride<- select(data, id,vehicle_model_id, travel_type_id,From_Month, From_Time_zone , online_booking,
               mobile_site_booking, Car_Cancellation, Booking_Time_zone, Booking_Month, sub_distance)

summary(Xride)


# Convert The Variable Classes into factor --------------------------------

Xride$From_Time_zone<-as.factor(Xride$From_Time_zone)
Xride$Booking_Time_zone<-as.factor(Xride$Booking_Time_zone)
Xride$From_Month<- as.factor(Xride$From_Month)
Xride$Booking_Month<- as.factor(Xride$Booking_Month)
Xride$online_booking<- as.factor(Xride$online_booking)
str(Xride)
# Using Pipe line functions -----------------------------------------------

Xride%>% filter(Xride$travel_type_id==2)%>%nrow()

Xride%>%filter(Xride$online_booking==1)%>% nrow()

Xride%>%filter(Xride$mobile_site_booking==1)%>% nrow()

Xride%>%group_by(Xride$Booking_Time)%>%summarise(n())

Xride%>%group_by(Xride$travel_type_id)%>%summarize(n())

data%>%filter(data$Car_Cancellation==1)%>%nrow()

# Model Iteration{Split data into test and train dataset} -------------------------------------------

set.seed(200)
index<- sample(nrow(Xride), 0.70*nrow(Xride), replace = FALSE)
train<- Xride[index,]
test<-Xride[-index,]

class(train$online_booking)

# Building First Model ----------------------------------------------------

mod<- glm(online_booking~., data = train[-1], family = "binomial")
summary(mod)

step(mod, direction = "both")

mod1<- glm(formula = online_booking ~ vehicle_model_id + travel_type_id + 
             From_Month + From_Time_zone + mobile_site_booking + Car_Cancellation + 
             Booking_Time_zone + Booking_Month + sub_distance, family = "binomial", 
           data = train)
summary(mod1)


# Creating Dummy Variables for significant variables ----------------------

train$FM_Aug_D<- ifelse(train$From_Month=="Aug",1,0)
train$FM_Jan_D<- ifelse(train$From_Month=="Jan",1,0)
train$FM_Feb_D<- ifelse(train$From_Month=="Feb",1,0)
train$FM_Jun_D<- ifelse(train$From_Month=="Jun",1,0)
train$FM_May_D<- ifelse(train$From_Month=="May",1,0)
train$FM_Jul_D<- ifelse(train$From_Month=="Jul",1,0)
train$FM_Oct_D<- ifelse(train$From_Month=="Oct",1,0)
train$FM_Nov_D<- ifelse(train$From_Month=="Nov",1,0)
train$From_9_to_12am_D<- ifelse(train$From_Time_zone=="9_to_12am",1,0)
train$Booking_3_to_6am_D<-ifelse(train$Booking_Time_zone=="3_to_6am",1,0)
train$Booking_6_to_9am_D<-ifelse(train$Booking_Time_zone=="6_to_9am",1,0)
train$Booking_9_to_12am_D<-ifelse(train$Booking_Time_zone=="9_to_12am",1,0)
train$Booking_12_to_15pm_D<-ifelse(train$Booking_Time_zone=="12_to_15pm",1,0)
train$Booking_15_to_18pm_D<-ifelse(train$Booking_Time_zone=="15_to_18pm",1,0)
train$Booking_18_to_21pm_D<-ifelse(train$Booking_Time_zone=="18_to_21pm",1,0)
train$BM_Jan_D<- ifelse(train$Booking_Month=="Jan",1,0)
train$BM_Feb_D<- ifelse(train$Booking_Month=="Feb",1,0)
train$BM_Jun_D<- ifelse(train$Booking_Month=="Jun",1,0)
train$BM_Oct_D<- ifelse(train$Booking_Month=="Oct",1,0)
train$BM_Nov_D<- ifelse(train$Booking_Month=="Nov",1,0)
train$sub_distanceMedium_D<- ifelse(train$sub_distance=="Medium_D",1,0)
train$sub_distanceLong_D<- ifelse(train$sub_distance=="Long_D",1,0)

test$FM_Aug_D<- ifelse(test$From_Month=="Aug",1,0)
test$FM_Jan_D<- ifelse(test$From_Month=="Jan",1,0)
test$FM_Feb_D<- ifelse(test$From_Month=="Feb",1,0)
test$FM_Jun_D<- ifelse(test$From_Month=="Jun",1,0)
test$FM_May_D<- ifelse(test$From_Month=="May",1,0)
test$FM_Jul_D<- ifelse(test$From_Month=="Jul",1,0)
test$FM_Oct_D<- ifelse(test$From_Month=="Oct",1,0)
test$FM_Nov_D<- ifelse(test$From_Month=="Nov",1,0)
test$From_9_to_12am_D<- ifelse(test$From_Time_zone=="9_to_12am",1,0)
test$Booking_3_to_6am_D<-ifelse(test$Booking_Time_zone=="3_to_6am",1,0)
test$Booking_6_to_9am_D<-ifelse(test$Booking_Time_zone=="6_to_9am",1,0)
test$Booking_9_to_12am_D<-ifelse(test$Booking_Time_zone=="9_to_12am",1,0)
test$Booking_12_to_15pm_D<-ifelse(test$Booking_Time_zone=="12_to_15pm",1,0)
test$Booking_15_to_18pm_D<-ifelse(test$Booking_Time_zone=="15_to_18pm",1,0)
test$Booking_18_to_21pm_D<-ifelse(test$Booking_Time_zone=="18_to_21pm",1,0)
test$BM_Jan_D<- ifelse(test$Booking_Month=="Jan",1,0)
test$BM_Feb_D<- ifelse(test$Booking_Month=="Feb",1,0)
test$BM_Jun_D<- ifelse(test$Booking_Month=="Jun",1,0)
test$BM_Oct_D<- ifelse(test$Booking_Month=="Oct",1,0)
test$BM_Nov_D<- ifelse(test$Booking_Month=="Nov",1,0)
test$sub_distanceMedium_D<- ifelse(test$sub_distance=="Medium_D",1,0)
test$sub_distanceLong_D<- ifelse(test$sub_distance=="Long_D",1,0)


# Rerun The Model With significant variables -------------------------------------------

mod2<- glm(formula = online_booking ~ travel_type_id + FM_Aug_D + FM_Jul_D + FM_Jan_D + FM_Feb_D + FM_Jun_D +
             FM_May_D + FM_Oct_D + FM_Nov_D + BM_Jan_D + BM_Feb_D + BM_Jun_D + BM_Oct_D + BM_Nov_D + From_9_to_12am_D+
             Booking_3_to_6am_D + Booking_6_to_9am_D + Booking_9_to_12am_D + Booking_12_to_15pm_D + Booking_15_to_18pm_D+ 
             Booking_18_to_21pm_D + sub_distanceMedium_D + sub_distanceLong_D + Car_Cancellation ,
           family = "binomial", data = train)

summary(mod2)


mod3<- glm(formula = online_booking ~ travel_type_id + FM_Aug_D + FM_Jan_D + FM_Feb_D + FM_Jun_D +
             FM_Oct_D + FM_Nov_D + BM_Jan_D + BM_Feb_D + BM_Jun_D + BM_Oct_D + BM_Nov_D + From_9_to_12am_D+
             Booking_3_to_6am_D + Booking_6_to_9am_D + Booking_9_to_12am_D + Booking_12_to_15pm_D + Booking_15_to_18pm_D+ 
             Booking_18_to_21pm_D + sub_distanceMedium_D + sub_distanceLong_D + Car_Cancellation ,
           family = "binomial", data = train)

summary(mod3)

hist(mod3$residuals)

vif(mod3)

mod4<- glm(formula = online_booking ~ travel_type_id + FM_Aug_D + From_9_to_12am_D+ Booking_3_to_6am_D +
             Booking_6_to_9am_D + Booking_9_to_12am_D + Booking_12_to_15pm_D + Booking_15_to_18pm_D+ 
             Booking_18_to_21pm_D + sub_distanceMedium_D + sub_distanceLong_D + Car_Cancellation ,
           family = "binomial", data = train)

summary(mod4)

vif(mod4)

# Create Predictions ------------------------------------------------------

pred<- predict(mod4, type = "response", newdata = test)
head(pred)
qqPlot(pred)
#Xdata<-Xride$online_booking/nrow(Xride)
#table(Xdata)

# (Xride$online_booking==1)=15270
# nrow(Xride)=43431

15270/43431

pred1<- ifelse(pred>= 0.3515922,1,0)

kappa2(data.frame(test$online_booking, pred1))

library(caret)
confusionMatrix(table(pred1, test$online_booking), positive = "1")

pred2<- prediction(predict(mod4, type = "response", newdata = test), test$online_booking)
roc<- performance(pred2, "tpr" , "fpr")
plot(roc, colorize=TRUE, print.cutoffs.at=seq(0.1, by=0.1))

pref<-performance(pred2, "auc")
auc<- unlist(pref@y.values)
auc

test$online_booking<- as.numeric(test$online_booking)
gains(test$online_booking, predict(mod4, type = "response", newdata = test), groups = 10)

test$prob<- predict(mod4, type = "response", newdata = test)

quantile(test$prob, probs = c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1))

targeted<- test[test$prob>0.2919648 & test$prob<= 0.8489240, "id"]
length(targeted)


# Question And Answer ---------------------------------------------------
# As given dataset we could see the by the graph the online booking is much higher than
# Mobile Site Booking so I consider the Online booking the the target variable and others Independent 
# variables

##What are the top 10 factors driving likelihood of online booking at Xride dataset?

head(sort(abs(mod4$coefficients), decreasing = TRUE),10)
# 1) Car_Cancellation
# 2) Booking_6_to_9am
# 3) Booking_9_to_12am
# 4) Booking_12_to_15pm
# 5) Booking_15_to_18pm
# 6) Booking_18_to_21pm
# 7) Booking_3_to_6am 
# 8) sub_distanceMedium   
# 9) sub_distanceLong             
#10) From_month_Aug


