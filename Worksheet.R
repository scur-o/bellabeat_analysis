install.packages("tidyverse")
library(tidyverse)
library(lubridate)
install.packages("Rtools")
tinytex::install_tinytex()
calories <- read.csv("Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")

wideCal <- calories %>% 
  pivot_wider(names_from = Id, values_from = Calories)

View(wideCal)

wideCal[c(5,6,12,13,19,20,26,27),] %>% 
  summary()

wideCal[-c(5,6,12,13,19,20,26,27),] %>% 
  summary()



# Pie chart on SedentaryMinutes vs. ActiveMinutes

activity <- read.csv("Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
View(activity)

testtest <- activity %>% 
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes)

testplt <- colMeans(testtest)
View(testplt)

hours <- round(testplt / 60, digits = 1)
lbls <- paste(names(testplt), hours, "hours")

pie(testplt, col = c("#00e4ff", "#15b5cd", "#1a879c", "#185d6d"), labels = lbls)










# Data Manipulation: Turning Activity hour M/D/Y H/M/S to H/M/S
## Since the original format was in string, I had to convert the string to datetime in order to extract the HMS by reconverting to HMS in string format

intensities <- read.csv("Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")

intensities$ActivityHour <- mdy_hms(intensities$ActivityHour)
intensities$ActivityHour = format(intensities$ActivityHour, "%H:%M:%S")


class(intensities$ActivityHour)
                                      
View(intensities) 




# Grouping hours and averaging mean of intensities

avgIntensities <- intensities %>% 
  group_by(ActivityHour) %>% 
  summarize(TotalAvgIntensity = mean(TotalIntensity))

View(avgIntensities)


# Plotting the groupped intensities
ggplot(avgIntensities, aes(x=ActivityHour, y=TotalAvgIntensity)) +
  geom_bar(stat = "identity", aes(fill=TotalAvgIntensity)) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Average Hourly Intensity",
       x = "Hour",
       y = "Total Average Intensity")










