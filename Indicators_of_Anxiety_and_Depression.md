Indicators of Anxiety or Depression Based on Reported Frequency of Symptoms. 
========================================================
author: Filipp Trubin
date: 01/09/2022
autosize: true


Slide With Code
========================================================


```{r}
library(ggplot2)
library(dplyr)
library(esquisse)
library(lubridate)

#data <- read.csv("https://data.cdc.gov/api/views/8pt5-q6wp/rows.csv?accessType=DOWNLOAD") 
data_1 <- read.csv("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD")
head(data_1)
as_date(data_1$submission_date)
```


```{r}
esquisse::esquisser()

p <- ggplot(data_1, aes(x=submission_date, y=tot_cases)) +
  geom_line() + 
  xlab("")
p
```


```{r, echo=FALSE}
#plot(data_1$tot_cases)
```



