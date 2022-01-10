library(ggplot2)
library(dplyr)
library(esquisse)
library(lubridate)
library(zoo)
options(scipen = 999)


data <- read.csv('data/Indicators_of_Anxiety_or_Depression_Based_on_Reported_Frequency_of_Symptoms_During_Last_7_Days.csv')
dataw <- data[which(data$Indicator == 'Symptoms of Depressive Disorder'
          & data$Group == 'By Sex'
          ),]





data_1 <- read.csv('data/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv')

data_2 <- data_1 %>% dplyr::select(submission_date, state, conf_cases)

data_3 <- data_2[complete.cases(data_2), ]

data_3$submission_date <- mdy(data_3$submission_date)

#data_4 <- data_3[order(data_3$submission_date), ]

data_3 <- data_3 %>%
    dplyr::arrange(desc(submission_date)) %>%
    dplyr::mutate(submission_date_7da = zoo::rollmean(data_3$conf_cases, k = 7, fill = NA))


#esquisse::esquisser()

#data_3$Month <- as.Date(cut(data_3$submission_date, breaks = "month"))

ggplot(data_3) +
  aes(x = submission_date, y = submission_date_7da) +
  #stat_summary(fun = "mean") +
  #scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") +
  scale_x_date(limit=c(as.Date("2020-05-05"),as.Date("2021-12-13"))) +
  geom_line() +
  geom_smooth() +
  theme_light()
  

