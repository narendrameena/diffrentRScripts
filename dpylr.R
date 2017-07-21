install.packages("dplyr")

library(dplyr)

msleep <- read.csv("msleep_ggplot2.csv")
head(msleep)
sleepData <- select(msleep, name, sleep_total )
head(sleepData)

head(select(msleep, -name))

head(select(msleep,name:order))

head(select(msleep,starts_with("sl")))

filter(msleep,sleep_total >= 16)

msleep %>%
  select(name, sleep_total) %>%
  head

msleep %>%  select(name,order, sleep_total) %>% arrange(order,desc(sleep_total)) %>% filter(sleep_total >= 16)
