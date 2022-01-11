library(ggplot2)
library(dplyr)
library(esquisse)
library(lubridate)
library(zoo)
options(scipen = 999)


data <- read.csv('data/Indicators_of_Anxiety_or_Depression_Based_on_Reported_Frequency_of_Symptoms_During_Last_7_Days.csv')

data_w1 <- data[which(data$Indicator == 'Symptoms of Depressive Disorder'
          & data$Group == 'By Sex'
          & data$Subgroup == 'Female'
          ),]

data_w2 <- data_w1 %>% dplyr::select(Time.Period.End.Date, Value)

data_w2$Time.Period.End.Date <- mdy(data_w2$Time.Period.End.Date)
#data_w2$Date <- as.Date(data_w2$Time.Period.End.Date, format='%m/%d/%y')

data_w3 <- data_w2[complete.cases(data_w2), ]


data_1 <- read.csv('data/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv')

data_2 <- data_1 %>% dplyr::select(submission_date, new_case)
data_2 <- data_2 %>%
  filter(new_case > 0)

data_2$submission_date <- mdy(data_2$submission_date)

data_3 <- data_2 %>%
  group_by(submission_date) %>%
  summarize(new_case_daily = sum(new_case, na.rm = TRUE))

data_joined <- merge(x = data_w3, y = data_3, by.x=c('Time.Period.End.Date'), by.y=c('submission_date'))

# data_3 <- data_3 %>%
#     dplyr::arrange(desc(submission_date)) %>%
#     dplyr::mutate(submission_date_7da = zoo::rollmean(data_3$conf_cases, k = 7, fill = NA))

cor(data_joined[1:20, 2], data_joined[1:20, 3])
data_joined[ , 'Correlation_Coef'] <- NA
for (i in 1:nrow(data_joined)) {
  data_joined[i, 4] = as.numeric(cor(data_joined[1:i, 2], data_joined[1:i, 3]))
}
data_joined[1, 4] = 0
data_joined$Correlation_Coef <- format(round(data_joined$Correlation_Coef, 2), nsmall = 2)
#esquisse::esquisser()


 ggplot() +
 aes(x = Time.Period.End.Date, y = Correlation_Coef, fill = Value, colour = Correlation_Coef, 
 size = Correlation_Coef) +
 geom_point(shape = "circle") +
 scale_fill_distiller(palette = "YlGn", 
 direction = 1) +
 scale_color_distiller(palette = "YlGn", direction = 1) +
 theme_classic() +
 theme(legend.position = "none")


ggplot(data = data_joined) +
  geom_line(aes(x = Time.Period.End.Date, y = new_case_daily / 10000, color = 'blue')) +
  geom_line(aes(x = Time.Period.End.Date, y = Value, color = 'red')) +
  aes(x = Time.Period.End.Date, y = Correlation_Coef * 10 + 50, fill = Correlation_Coef, colour = as.factor(Correlation_Coef)) +
  geom_point(shape = 'circle') +
  # scale_fill_distiller(palette = "YlGn", direction = 1) +
  # scale_color_distiller(palette = "YlGn", direction = 1) +

  lims(y = c(0, 100)) +
  scale_x_date(limit=c(as.Date("2020-05-05"), as.Date("2021-12-13")),
               date_breaks = "3 month",
               date_labels = "%Y/%m" ) +
  theme_classic()
  

