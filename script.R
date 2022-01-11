library(dplyr)
library(ggplot2)
library(lubridate)

#Load dataset "Indicators of Anxiety or Depression Based on Reported Frequency of Symptoms During Last 7 Days" (https://data.cdc.gov):
#[Dataset description](https://data.cdc.gov/NCHS/Indicators-of-Anxiety-or-Depression-Based-on-Repor/8pt5-q6wp)
data_survey <- read.csv("https://data.cdc.gov/api/views/8pt5-q6wp/rows.csv?accessType=DOWNLOAD") 
head(data_survey)

#Filter down to focus group: Var.1: "Depression Value" Var.2: "Female"
data_survey_1 <- data_survey[which(data_survey$Indicator == 'Symptoms of Depressive Disorder'
          & data_survey$Group == 'By Sex'
          & data_survey$Subgroup == 'Female'
          ),]

#Convert Date field / drop NA
data_survey_2 <- data_survey_1 %>% dplyr::select(Time.Period.End.Date, Value)
data_survey_2$Time.Period.End.Date <- mdy(data_survey_2$Time.Period.End.Date)
data_survey_3 <- data_survey_2[complete.cases(data_survey_2), ]

ggplot(data_survey_3) +
  aes(x = Time.Period.End.Date, y = Value, colour = Value) +
  geom_line(size = 1L) +
  scale_color_distiller(palette = 'Reds', direction = 1) +
  labs(
    title = 'Depression Value per day',
    x = NULL,
    y = 'Depression Value'
  ) +
  theme_classic() +
  ylim(0, 40)


#Load dataset "United States COVID-19 Cases and Deaths by State over Time" (https://data.cdc.gov):
#[Dataset description](https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36)
data_COVID19 <- read.csv("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD")
head(data_survey)

#Filter down focus group: Var.1: "Date" Var.2: "Number of cases"
data_COVID19_2 <- data_COVID19 %>% dplyr::select(submission_date, new_case)
data_COVID19_2$submission_date <- mdy(data_COVID19_2$submission_date)
data_COVID19_2 <- data_COVID19_2 %>% filter(new_case > 0 & submission_date <= as.Date('2021-12-13'))

#Grouping number of cases by day
data_COVID19_3 <- data_COVID19_2 %>%
  group_by(submission_date) %>%
  summarize(new_case_daily = sum(new_case, na.rm = TRUE))

ggplot(data_COVID19_3) +
  aes(x = submission_date, y = new_case_daily, colour = new_case_daily) +
  geom_line(size = 1L) +
  scale_color_distiller(palette = "Blues", direction = 1) +
  labs(
    title = "COVID19 cases per day",
    x = NULL,
    y = 'COVID-19 Cases'
  ) +
  ylim(0, 700000) +
  scale_x_date(limit=c(as.Date('2020-06-01'), as.Date('2021-12-13'))) +
  theme_classic() 


#Merge datasets. Number of cases and Depression Value aggregated by day   
data_joined <- merge(x = data_survey_3, y = data_COVID19_3, by.x=c('Time.Period.End.Date'), by.y=c('submission_date'))

#Add new column: rolling Correlation coefficient (Pearson method)
data_joined[ , 'Correlation_Coef'] <- NA
for (i in 1:nrow(data_joined)) {
  data_joined[i, 4] = as.numeric(cor(data_joined[1:i, 2], data_joined[1:i, 3]))
}
data_joined[1, 4] = 0
data_joined$Correlation_Coef <- round(data_joined$Correlation_Coef, digits = 2)



#Combine graphs for EDA: Depression Value + COVID19 cases + rolling Corr. coef.
ggplot(data = data_joined) +
  geom_line(aes(x = Time.Period.End.Date, y = new_case_daily / 10000), color='blue') +
  geom_line(aes(x = Time.Period.End.Date, y = Value), color='red') +
  aes(x = Time.Period.End.Date, y = Correlation_Coef * 10 + 50, color = Correlation_Coef) +
  geom_point(shape = 'circle') +
  lims(y = c(0, 100)) +
  scale_x_date(limit=c(as.Date('2020-06-01'), as.Date('2021-12-13')),
               date_breaks = '3 month',
               date_labels = '%Y/%m' ) +
  labs(x = NULL, y = NULL, title = 'COVID-19 cases | Depression Value | Rolling Correlation coef') +
  theme_classic()

#Build LR model
linear_regression = lm(Value ~ new_case_daily, data = data_joined)

#P-value (0.04814) is less than 0.05 which means the relationship is statistically significant
#and indicates strong evidence against the null hypothesis.
summary(linear_regression)

