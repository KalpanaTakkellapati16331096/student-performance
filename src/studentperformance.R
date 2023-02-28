yield_data <- read.csv("C:/Users/kalpa/Desktop/Assignment_1-main/Assignment_1-main/StudentPerformance/data_raw/StudentsPerformance.csv")
clean_yield_data<-na.omit(yield_data[yield_data != "N", ])
write.csv(clean_yield_data,"C:/Users/kalpa/Desktop/Assignment_1-main/Assignment_1-main/StudentPerformance/clean_yield_data.csv")

#Loading  clean Data

clean_yield_data<-read.csv2("C:/Users/kalpa/Desktop/Assignment_1-main/Assignment_1-main/StudentPerformance/clean_yield_data.csv")
clean_yield_data
library(tidyverse) # metapackage with lots of helpful functions
library(dplyr)
library(ggplot2)
data=read.csv("C:/Users/kalpa/Desktop/Assignment_1-main/Assignment_1-main/StudentPerformance/data_raw/StudentsPerformance.csv")
Summ<-summary(clean_yield_data)
data %>%
  group_by(race.ethnicity) %>%
  summarise(mathMean = mean(math.score),readingMean =  mean(reading.score),writingMean = mean(writing.score)) %>%
  ggplot(data = ., aes(x = race.ethnicity, y = mathMean, 
                       fill = race.ethnicity)) + geom_bar(stat = "identity")
ggsave("C:/Users/kalpa/Desktop/Assignment_1-main/Assignment_1-main/StudentPerformance/results/race.ethnicity.jpg")
total <- table(data$race.ethnicity)
plot(table(data$race.ethnicity))
ggsave("C:/Users/kalpa/Desktop/Assignment_1-main/Assignment_1-main/StudentPerformance/results/data$race.ethnicity.jpg")
x1 <- data %>% 
  group_by(race.ethnicity) %>%
  filter(math.score > 80) 
table(x1$race.ethnicity)
x1 %>% ggplot() + geom_bar(aes(x1$race.ethnicity,fill = x1$race.ethnicity))
ggsave("C:/Users/kalpa/Desktop/Assignment_1-main/Assignment_1-main/StudentPerformance/results/x1$race.ethnicity.jpg")
data %>% 
  group_by(gender) %>%
  select(math.score,reading.score,writing.score) %>%
  filter(math.score > 80, reading.score > 80, writing.score > 80) %>%
  ggplot(data = ., aes(x = gender, y = math.score, 
                       fill = gender)) + geom_bar(stat = "identity")
ggsave("C:/Users/kalpa/Desktop/Assignment_1-main/Assignment_1-main/StudentPerformance/results/Gender.jpg")
level1 <- data %>% 
  group_by(parental.level.of.education) %>%
  summarise(mathTotal = sum(math.score))

level1 %>% ggplot(data = ., aes(x = parental.level.of.education, y = mathTotal, 
                                fill = parental.level.of.education)) + geom_bar(stat = "identity") + 
  labs(title="Education Level Chart",
       subtitle="Reading Scores") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
ggsave("C:/Users/kalpa/Desktop/Assignment_1-main/Assignment_1-main/StudentPerformance/results/Education level chart.jpg")

