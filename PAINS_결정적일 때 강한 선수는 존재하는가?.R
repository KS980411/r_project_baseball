---
title: "clutch"
output: html_document
date: "2022-09-18"
---
## 데이터 읽기 및 라이브러리 설치

```{r}
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)

getwd()
setwd("/Users/choeunsol/R")
basic <-as_tibble(read.csv("basic.csv"))
clutch <- as_tibble(read.csv("clutch.csv"))
```

## 데이터 결합 및 플롯 세팅

```{r}
baseball <- inner_join(basic, clutch, by = "Name")
baseball <- baseball %>% select(Name, PA, wOBA, WPA, Clutch, RE24, wRC., Off) %>% filter(!is.na(Clutch))

lm <- lm(data = baseball, Clutch ~ Off) # 회귀 직선 계산을 위해서 넣어줍니다. 플롯에서 회귀직선과 r^2 값을 표현해줄 겁니다.
df <- data.frame(baseball$Off, predict(lm)) # 회귀 직선을 labeling하기 위한 데이터 프레임입니다.

r_squared <- cor(baseball$Clutch, baseball$Off) ^ 2
median <- baseball %>% pull(Clutch) %>% median() # Clutch의 median
median_2 <- baseball %>% pull(Off) %>% median() # Off WAR의 median
baseball <- baseball %>% mutate(category = case_when(Off >= median_2 & Clutch >= median ~ "High-High", Off >= median_2 & Clutch < median ~ "High-Low", Off < median_2 & Clutch >=median ~ "Low-High", TRUE ~ "Low-Low"))
```

** 박스 플롯 산출 **을 위해 Off WAR와 Clutch를 median 값으로 나누어 "High-High" ~ "Low-Low"까지 4개의 범주를 나눠줍니다.

```{r}
index_x <- 420
index_y <- -4.7
text <- "R^2 0.03"
text_over <- "R^2 0.04"
index <- data.frame(x = index_x, y = index_y, text = text)

baseball %>% ggplot(aes(Off, Clutch)) + geom_point(alpha = 0.3) + ggtitle("Clutch and Offensive WAR") + geom_hline(yintercept = median, color = "blue") + geom_line(data = df, aes(baseball.Off, predict.lm.), color = "red", linetype = "dotdash") + theme_economist() + xlab("Offensive WAR") + geom_label(data = index, aes(x, y, label = text), color = "red")
```

### 5000타석 이상에서의 플롯

데이터가 동일 선상에서 비교가 힘드므로(각자 커리어가 다르므로)
5000타석의 데이터를 따로 만들어 plotting 합니다.

```{r}
over_thpa <- baseball %>% filter(PA >= 5000)
nrow(over_thpa)

lm.over <- lm(data = over_thpa, Clutch ~ Off)
median.over <- over_thpa %>% pull(Clutch) %>% median()
r_over <- cor(over_thpa$Off, over_thpa$Clutch) ^ 2
df.over <- data.frame(x = over_thpa$Off, y = predict(lm.over))

over_thpa %>% ggplot(aes(Off, Clutch)) + geom_point(alpha = 0.3) + ggtitle("Clutch and Offensive War (Over 5000PA)") + geom_hline(yintercept = median.over) + geom_line(data = df.over, aes(x, y), color = "red", linetype = "dotdash") + theme_economist() + geom_label(data = index, aes(x, y, label = text_over), color = "red")
```

## 박스플롯과 아웃라이어

```{r}
# boxplot and outlier

outlier <- function(x)
{
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

outlier_st <- baseball %>% group_by(category) %>% summarize(index_low = quantile(Clutch, 0.25) - 1.5 * IQR(Clutch), index_high = quantile(Clutch, 0.75) + 1.5 * IQR(Clutch))

# 아까 나누어 준 카테고리 변수에서 아웃라이어들을 산출합니다.
high_high <- baseball %>% filter(category == "High-High") %>% filter(Clutch > 5.02 | Clutch < -2.75) %>% pull(Name)
high_low <- baseball %>% filter(category == "High-Low") %>% filter(Clutch > 1.88 | Clutch < -6.98) %>% pull(Name)
low_high <- baseball %>% filter(category == "Low-High") %>% filter(Clutch > 3.94 | Clutch < -2.38) %>% pull(Name)
low_low <- baseball %>% filter(category == "Low-Low") %>% filter(Clutch > 1.43 | Clutch < -5.07)


split_result <- strsplit(baseball$Name, split = " ") # 가독성을 위해 성만 따줍니다.
baseball$last_name <- unlist(lapply(split_result, FUN = "[", 2))

baseball <- baseball %>% mutate(index = case_when(Name %in% c(high_high, high_low, low_high, low_low) ~ last_name , TRUE ~ ""))


baseball %>% ggplot(aes(category, Clutch, fill = category)) + geom_boxplot(show.legend = FALSE, outlier.shape = 12) + geom_label_repel(aes(label = index), segment.size = 0.2, max.overlaps = 20) + theme(axis.title.x = element_blank()) + theme_economist() + coord_flip() + theme(legend.position = "none") + xlab("")
```

## 연도 별 연관성

```{r}
setwd("/Users/choeunsol/R")
time_baseball <- as_tibble(read.csv("leaderboard_1722.csv"))
time_baseball_b <- time_baseball %>% filter(Season == 2021) %>% group_by(Name) %>% summarize(Clutch = sum(Clutch))
time_baseball_a <- time_baseball %>% filter(Season == 2022) %>% group_by(Name) %>% summarize(Clutch = sum(Clutch))
dataset <- inner_join(time_baseball_b, time_baseball_a, by = "Name")
colnames(dataset) <- c("Name", "Clutch.2020", "Clutch.2021")

r.squared.time <- with(dataset, cor(Clutch.2020, Clutch.2021)) # r_squared 0.08
lm.time <- with(dataset, lm(Clutch.2021 ~ Clutch.2020))
df.time <- data.frame(x = dataset$Clutch.2020, y = predict(lm.time))
text.time <- data.frame(x = 1.5, y = -0.3, label = "R^2 = 0.08")

dataset %>% ggplot(aes(Clutch.2020, Clutch.2021)) + geom_point() + xlab("2020") + ylab("2021") + geom_line(data = df.time, aes(x, y), color = "red", linetype = "dotdash", alpha = 0.3) + geom_label(data = text.time, aes(x,y, label = label), color = "red") + theme_economist() + ggtitle("Clutch Prediction of 2021 on 2020 Data")
```

## 아웃라이어 인덱스 파일

```{r}
library(openxlsx)

outlier_list <- baseball %>% filter(Name %in% c(c(high_high, high_low, low_high, low_low))) %>% select(-c(index, last_name))
write.xlsx(outlier_list, "outlier_list.xlsx", TRUE)
```
