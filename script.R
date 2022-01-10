library(ggplot2)
library(dplyr)
library(esquisse)
library(lubridate)

data <- read.csv('data/Indicators_of_Anxiety_or_Depression_Based_on_Reported_Frequency_of_Symptoms_During_Last_7_Days.csv') 
data_1 <- read.csv('data/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv')

data_2 <- data_1 %>% dplyr::select(submission_date, state, conf_cases)
data_3 <- data_2[complete.cases(data_2), ]
data_3$submission_date = as_date(data_3$submission_date)
data_4 <- data_3[order(data_3$submission_date),]

head(data_4)



esquisse::esquisser()

# p <- ggplot(data_4, aes(x=submission_date, y=conf_cases)) +
#  geom_line() +
#  xlab("")
# p

