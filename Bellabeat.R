
## Bellabeat Case Study

install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

## Data Source 1 : Fitbit Fitness Tracker Dataset.

fitbit_activity <- read_csv("C:/Users/my pc/Desktop/GA/Google Data Analytics Capstone Complete a Case Study/FitBit-Data/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")


activity_info <- fitbit_activity %>%
  select(Id,ActivityDate,TotalSteps,FairlyActiveMinutes,LightlyActiveMinutes,VeryActiveMinutes,SedentaryMinutes,Calories)
summary(activity_info)

# Print number of Unique Ids.
no_ids <- unique(activity_info$Id)
print(no_ids)
# The dataset has 33 unique Ids against the previously thought 30 Ids suggesting that the dataset may have been updated.


# Graphical Representation of the relationship between Step Count and Calories burnt.

ggplot(activity_info) + 
  geom_jitter(mapping= aes(x = TotalSteps, y = Calories )) +
  ggtitle("Total Steps Vs Calories Burnt")


### It can be noted from the graph that as the step count increases, the calories burnt also rises.


activity_min <- activity_info %>%
  select(FairlyActiveMinutes,LightlyActiveMinutes,VeryActiveMinutes,SedentaryMinutes)


# Boxplot representation of active minutes.

boxplot(activity_min,                         # Data
        #col = rainbow(ncol(activity_min)),   # Color
        col = rgb(0.5, 0.5, 1, alpha = 0.5),
        lwd = 1.0,                            # Line width
        ylab = "Minutes",                     # Y axis label
        main = "Active Minutes Recorded by FitBit Users between 03.12.2016-05.12.2016",  # Main Title of chart
        outpch = 23,                          # Outlier symbol
        outbg = "blue",                       # Outlier color
        whiskcol = "orange",                  # Whisker color
        whisklty = 5,                         # Whisker line type
        lty = 1)                              # Line type



# Graphical Representation of the relationship between Minutes active and Calories burnt.

FairlyActiveMinutes <- activity_info$FairlyActiveMinutes
LightlyActiveMinutes <- activity_info$LightlyActiveMinutes
VeryActiveMinutes <- activity_info$VeryActiveMinutes
SedentaryMinutes <- activity_info$SedentaryMinutes
Calories <- activity_info$Calories

par(mfrow = c(2,2))
plot(FairlyActiveMinutes, Calories, main ="Fairly Active Minutes vs. Calories")
plot(LightlyActiveMinutes, Calories, main ="Lightly Active Minutes vs. Calories")
plot(VeryActiveMinutes, Calories, main ="Very Active Minutes vs. Calories")
plot(SedentaryMinutes, Calories, main ="Sedentary Minutes vs. Calories")

### From the representations, it could be seen that :-

# (1) Calories are shed faster when nature of the activities performed are Very Active or Fairly Active.
# (2) Lightly active tasks when performed over a course of time help shed more calories.
# (3) Sedentary Activities do not help shed calories fast enough.

###


# Reading and Analyzing Sleep data.

sleep_data <- read_csv("C:/Users/my pc/Desktop/GA/Google Data Analytics Capstone Complete a Case Study/FitBit-Data/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")

sleep_time <- sleep_data %>%
  select(Id,SleepDay,TotalMinutesAsleep)
sleep_time$sleepTime_Hours <- sleep_time$TotalMinutesAsleep / 60
summary(sleep_time)

class(sleep_time$SleepDay)

# Convert Character to Date format.

sleep_time$SleepDay_new <- as.Date(sleep_time$SleepDay, "%m/%d/%Y")
class(sleep_time$SleepDay_new)

sleep_average <- aggregate(sleep_time$sleepTime_Hours, by=list(sleep_time$Id, sleep_time$SleepDay_new), sum)
colnames(sleep_average) <- c('Id' ,'SleepDay', 'TotalSleepTime_Hours')

# Print number of Sleep Ids.

no_sleep_ids <- unique(sleep_time$Id)
print(no_sleep_ids)

# Sleep Data of 24 users are present.

str(sleep_average)

# Group Sleep Patterns. Recommended sleep time for adults as mentioned by CDC is 7 or more hours per night.

q1 <- sleep_average %>%
  filter (TotalSleepTime_Hours >= 7.0) %>%
    mutate(class = 'Good Sleep') 
q2 <- sleep_average %>%
  filter (TotalSleepTime_Hours < 7.0 & TotalSleepTime_Hours >= 6.0) %>%
    mutate(class = 'Moderate Sleep')
q3 <- sleep_average %>%
  filter (TotalSleepTime_Hours < 6.0) %>%
  mutate(class = 'Less Sleep') 
     

res <- rbind(q1,q2,q3)

# Graphical Representation of Sleep Pattern.

ggplot(res) + 
  geom_jitter(mapping= aes(x = SleepDay, y = TotalSleepTime_Hours, colour = class )) +
  ggtitle("Sleep Pattern Recorded from 24 FitBit Users")


# Reading and Analyzing heart data.
heart_data <- read_csv("C:/Users/my pc/Desktop/GA/Google Data Analytics Capstone Complete a Case Study/FitBit-Data/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")
summary(heart_data)


# Print number of Heart rate Ids.

no_heartRate_ids <- unique(heart_data$Id)
print(no_heartRate_ids)

## Heart Rate recorded for 7 fitbit Users.

list_of_Ids <- heart_data %>%
  group_by(Id) %>%
  summarise_at(vars(Value), list(Mean_Heart_Rate = mean))


## A normal resting heart rate for adults ranges from 60 to 100 beats per minute.


normal_heart_rate <- list_of_Ids %>%
  filter(Mean_Heart_Rate >= 60 & Mean_Heart_Rate <= 100) %>%
  mutate(Heart_health = 'Normal Range')

unusual_heart_rate <- list_of_Ids %>%
  filter(Mean_Heart_Rate < 60 | Mean_Heart_Rate > 100) %>%
  mutate(Heart_health = 'Unusual Range')

# It can be understood from the list that all of the 7 FitBit Users had their average heart rate all within the Normal Range.


# Reading and Analyzing weight data.

weight_data <- read_csv("C:/Users/my pc/Desktop/GA/Google Data Analytics Capstone Complete a Case Study/FitBit-Data/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
weight_log <- weight_data %>%
  select(Id,Date,WeightKg,BMI)
summary(weight_log)

bmi_Ids <- weight_log %>%
  group_by(Id) %>%
  summarise_at(vars(BMI), list(Mean_BMI_Value = mean))


# If your BMI is 18.5 to 24.9, it falls within the normal or Healthy Weight range. If your BMI is 25.0 to 29.9, it falls within the overweight range. If your BMI is 30.0 or higher, it falls within the obese range.

w1 <- bmi_Ids %>%
  filter (Mean_BMI_Value >= 18.5 & Mean_BMI_Value <= 24.9) %>%
  mutate(weight_class = 'Normal') 

w2 <- bmi_Ids %>%
  filter (Mean_BMI_Value >= 25.0 & Mean_BMI_Value <= 29.9) %>%
  mutate(weight_class = 'Overweight') 

w3 <- bmi_Ids %>%
  filter (Mean_BMI_Value >= 30.0) %>%
  mutate(weight_class = 'Obese') 

wt <- rbind(w1, w2, w3)

# Print number of Weight Log Ids.

no_weightLog_ids <- unique(weight_log$Id)
print(no_weightLog_ids)

# Weight Logged by 8 Users.

# Calculate percentage for each category of bmi index.

bmi <- wt %>% 
  group_by(weight_class) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(percentage = `n` / sum(`n`)) %>% 
  arrange(percentage) %>%
  mutate(labels = scales::percent(percentage))

# Pie Chart Representation of Body Mass Index of FitBit Users.

ggplot(bmi, aes(x = "", y = percentage, fill = weight_class)) +
  geom_col() +  geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) + 
  coord_polar(theta = "y") + ggtitle("BMI Range from Observed Data") 



## DATA SOURCE 2:- An Open Dataset for Human Activity Analysis using Smart Devices.

df <- read_csv("C:/Users/my pc/Desktop/GA/Google Data Analytics Capstone Complete a Case Study/Human-Activity analysis/report.csv")

data_source2 <- df %>%
  select(index,activity_type,duration)

# Determine the number of unique activities.

unique_activities <- unique(data_source2$activity_type)
print(unique_activities)
summary(data_source2)

activity_duration <- data_source2 %>%
  group_by(activity_type) %>%
  summarise(Total_duration = sum(duration)) 

# Sort Activities based on their maximum duration.

order_of_activity <- arrange(activity_duration, desc(Total_duration))

order_of_activity$Total_duration <- as.numeric(order_of_activity$Total_duration)

# Duration of Activities in Hours.

g1 <- order_of_activity %>%
    mutate(total_duration_in_hours = Total_duration / 3600 )

# Plot Activities and their duration.

ggplot(g1) +  geom_col(aes(x = activity_type, y = total_duration_in_hours), color = "red" ) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Most Common Activity Types As Observed from 29th June 2017 to 12th July 2017") + 
  xlab("Activity Type") + ylab("Total Duration in Hours") 

#+ theme(legend.box="horizontal",legend.key=element_blank(), legend.title=element_blank(),legend.position="top")


## Reading smartwatch_filtered.csv.

smart <- read_csv("C:/Users/my pc/Desktop/GA/Google Data Analytics Capstone Complete a Case Study/Human-Activity analysis/smartwatch_filtered.csv")

summary(smart)

heart <- smart[smart$source == 'heart_rate', ]

str(heart)

heart$beatsPerMinute <- substring(heart$values, 3, 6)
heart$beatsPerMinute <- as.numeric(heart$beatsPerMinute)
str(heart)


## A normal resting heart rate for adults ranges from 60 to 100 beats per minute.


normal_heart_rate_numbers <- heart %>%
  filter(beatsPerMinute >= 60 & beatsPerMinute <= 100) %>%
  mutate(Heart_health = 'Normal Range')

unusual_heart_rate_numbers <- heart %>%
  filter((beatsPerMinute < 60 & is.na(beatsPerMinute)!= TRUE) | (beatsPerMinute > 100 & is.na(beatsPerMinute)!= TRUE)) %>%
  mutate(Heart_health = 'Unusual Range')

other <- heart %>%
  filter(is.na(beatsPerMinute)== TRUE) %>%
  mutate(beatsPerMinute = 0) %>%
  mutate(Heart_health = 'Data Not Available')

hrt <- rbind(normal_heart_rate_numbers, unusual_heart_rate_numbers, other)

summary(heart$beatsPerMinute)

## It can be noted from the summary statistics that the mean reading for heart rate was 69 which is within the normal range.
 ## A total of 9 observations measured for heart rate showed 0.0 as their reading indicating that the smartwatch may have been idle or being charged.

# Calculate percentage of each category of Heart Health.

df <- hrt %>% 
  group_by(Heart_health) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

# Pie Chart Representation of Heart Health.

ggplot(df, aes(x = "", y = perc, fill = Heart_health)) +
  geom_col() +  geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) + 
  coord_polar(theta = "y") + ggtitle("Average Heart Rate from Observed Data") 

# Filter out Pressure Readings.

pressure <- smart[smart$source == 'pressure', ]
summary(pressure)
pressure$reading <- substring(pressure$values, 3, 8)
pressure$reading <- as.numeric(pressure$reading)
summary(pressure$reading)

# Filter out Step Counters.

step_count <- smart[smart$source == 'step_counter', ]
summary(step_count)
step_count$count_of_steps <- substring(step_count$values, 3, 5)
step_count$count_of_steps <- as.numeric(step_count$count_of_steps)
str(step_count)
summary(step_count)

# Mean step count is too low indicative of mild activities.

st_ct <- step_count %>%
  group_by(timestamp) %>%
  summarise( Total_Steps = sum(count_of_steps)) 

str(st_ct$timestamp)

# Extract Hour Readings from the data.

st_ct$ts <- substring(st_ct$timestamp, 0, 2)

s1 <- st_ct %>%
  filter(ts >= 00 & ts <= 11) %>%
  mutate(Time = 'Time 1')

s2 <- st_ct %>%
  filter(ts >= 12 & ts <= 23) %>%
  mutate(Time = 'Time 2')

s3 <- st_ct %>%
  filter(ts >= 24 & ts <= 35) %>%
  mutate(Time = 'Time 3')

s4 <- st_ct %>%
  filter(ts >= 36 & ts <= 47) %>%
  mutate(Time = 'Time 4')

s5 <- st_ct %>%
  filter(ts >= 48 & ts <= 60) %>%
  mutate(Time = 'Time 5')

d1 <- data.frame(TimeStamp = c("Time 1", "Time 2", "Time 3", "Time 4", "Time 5"), 
                 labels = c("00:00:00 to 11:59:59", "12:00:00 to 23:59:59", "24:00:00 to 35:59:59", "36:00:00 to 47:59:59", "48:00:00 to 60:00:00"),
           TotalStepCount = c(sum(s1$Total_Steps), sum(s2$Total_Steps), sum(s3$Total_Steps), sum(s4$Total_Steps), sum(s5$Total_Steps)),
           AvgStepCount = c(mean(s1$Total_Steps), mean(s2$Total_Steps), mean(s3$Total_Steps), mean(s4$Total_Steps), mean(s5$Total_Steps)))


# DotChart representation of Average Step Count.

dotchart(d1$AvgStepCount, 
         labels = c("00:00:00 to 11:59:59", "12:00:00 to 23:59:59", "24:00:00 to 35:59:59", "36:00:00 to 47:59:59", "48:00:00 to 60:00:00"),
         cex = 0.9,  
         pch = 22,
         color = "blue",
         xlab = "Step Count",
         main = "Average Step Count Measured Over 15 days")


## DATA SOURCE 3 :  Smartphone and Smartwatch Activity and Biometrics dataset.

### Since 51 subjects perform 18 predetermined activities for 3 minutes, we will be considering only one subject/id for our analysis. ID no. 1605 is randomly chosen.

# Reading Gyro Sensor data for Subject : 1605.

wisdm_1605 <- read_csv("C:/Users/my pc/Desktop/GA/Google Data Analytics Capstone Complete a Case Study/WISDM Dataset/wisdm-dataset/wisdm-dataset/raw/watch/gyro/data_1605_gyro_watch.txt")
                 
names(wisdm_1605) <- c('participant_id' , 'activity_code' , 'timestamp', 'x', 'y', 'z')
summary(wisdm_1605)   

# Clean the 'z' column.

wisdm_1605$z<- gsub(";"," ", wisdm_1605$z)
wisdm_1605$z <- as.numeric(wisdm_1605$z)

summary(wisdm_1605) 

# Grouping of Activities adopted from the research paper.

w1 <- wisdm_1605 %>%
  filter (activity_code == "A" | activity_code == "B" | activity_code == "C" | activity_code == "E" | activity_code == "M" | activity_code == "D") %>%
  mutate(Group = 'Non-Hand Oriented Activities') 

w2 <- wisdm_1605 %>%
  filter (activity_code == "P" | activity_code == "O" | activity_code == "F" | activity_code == "Q" | activity_code == "R" | activity_code == "G" | activity_code == "S") %>%
  mutate(Group = 'Hand Oriented Activities - General') 

w3 <- wisdm_1605 %>%
  filter (activity_code == "J" | activity_code == "H" | activity_code == "L" | activity_code == "I" | activity_code == "K") %>%
  mutate(Group = 'Hand Oriented Activities - Eating') 

avty_1605 <- rbind(w1, w2, w3)

res_1605 <- avty_1605 %>%
             select(Group,x,y,z) %>%
            group_by(Group) %>%
            summarise(X = mean(x, na.rm=T), Y = mean(y, na.rm=T), Z = mean(z, na.rm=T)) 

# Install scatterplot3d package for 3D Scatter Plot Representation.

install.packages("scatterplot3d")   
library(scatterplot3d)

# colors <- c("#999999", "#E69F00", "#56B4E9", "#888888", "#E19F00", "#E26F00",
#             "#666666", "#E20F00", "#96B4E9", "#567435", "#E78F00", "#E97F00",
#             "#472999", "#E88F00", "#10B4E2", "#499388", "#E13F00", "#E56F00")

colors <- c("#E26F00", "#10B4E2", "#499388")

Y <- res_1605[1:3, 1:4]
plot1 <- scatterplot3d(Y[1:3, 2:4],  pch = 16, color = colors, box = FALSE, grid = FALSE, type = "h",
                       main="Average Gyro Sensor data for 18 activities obtained in September 2019",
                       xlab = " Sensor Data X ", ylab = " Sensor Data Y", zlab = " Sensor Data Z")

# Install Package FactoClass.

install.packages("FactoClass")
library(FactoClass)

plot1$points3d(Y[1:3, 2:4],pch = 2)
addgrids3d(Y[1:3, 2:4], grid = c("xy", "xz", "yz"))

# xyz.convert function which converts coordinates from 3D (x, y, z) to 2D-projection (x, y) of scatterplot3d. Useful to plot objects into existing plot.

txt<- plot1$xyz.convert(Y[1:3, 2:4])
text(txt,labels = Y$Group,cex = 0.8,col = "blue",pos = 3)


# Reading Accel Sensor Data for Subject : 1605.

accel_1605 <- read_csv("C:/Users/my pc/Desktop/GA/Google Data Analytics Capstone Complete a Case Study/WISDM Dataset/wisdm-dataset/wisdm-dataset/raw/watch/accel/data_1605_accel_watch.txt")

names(accel_1605) <- c('participant_id' , 'activity_code' , 'timestamp', 'x', 'y', 'z')
summary(accel_1605)    

accel_1605$z<- gsub(";"," ", accel_1605$z)
accel_1605$z <- as.numeric(accel_1605$z)

summary(accel_1605) 

a1 <- accel_1605 %>%
  filter (activity_code == "A" | activity_code == "B" | activity_code == "C" | activity_code == "E" | activity_code == "M" | activity_code == "D") %>%
  mutate(Group = 'Non-Hand Oriented Activities') 

a2 <- accel_1605 %>%
  filter (activity_code == "P" | activity_code == "O" | activity_code == "F" | activity_code == "Q" | activity_code == "R" | activity_code == "G" | activity_code == "S") %>%
  mutate(Group = 'Hand Oriented Activities - General') 

a3 <- accel_1605 %>%
  filter (activity_code == "J" | activity_code == "H" | activity_code == "L" | activity_code == "I" | activity_code == "K") %>%
  mutate(Group = 'Hand Oriented Activities - Eating') 

actvty_1605 <- rbind(a1, a2, a3)

acl_1605 <- actvty_1605 %>%
  select(Group,x,y,z) %>%
  group_by(Group) %>%
  summarise(X = mean(x, na.rm=T), Y = mean(y, na.rm=T), Z = mean(z, na.rm=T)) 


# colors <- c("#999999", "#E69F00", "#56B4E9", "#888888", "#E19F00", "#E26F00",
#             "#666666", "#E20F00", "#96B4E9", "#567435", "#E78F00", "#E97F00",
#             "#472999", "#E88F00", "#10B4E2", "#499388", "#E13F00", "#E56F00")

col <- c("#472999", "#E88F00", "#10B4E2")

P <- acl_1605[1:3, 1:4]
plot2 <- scatterplot3d(P[1:3, 2:4],  pch = 16, color = col, box = FALSE, grid = FALSE, type = "h", 
                       main="Average Accelerometer Sensor data for 18 activities obtained in September 2019",
                       xlab = " Sensor Data X ",
                       ylab = " Sensor Data Y",
                       zlab = " Sensor Data Z")

plot2$points3d(P[1:3, 2:4],pch = 5)
addgrids3d(P[1:3, 2:4], grid = c("xy", "xz", "yz"))

legend(2, 8, legend= c("Non-Hand Oriented Activities", "Hand Oriented Activities - General", "Hand Oriented Activities - Eating"), 
       fill = c("#472999", "#E88F00", "#10B4E2"))



