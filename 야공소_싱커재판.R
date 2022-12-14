---
title: "sinker"
author: "2017150411 조광은"
date: '2020 8 28 '
output: html_document
---

```{r}
setwd("C:/Users/OOTP/Documents/야구공작소")
pitch_2015 <- read.csv("sinker.1.csv", header = TRUE)
pitch_2016 <- read.csv("sinker.2.csv", header = TRUE)
pitch_2017 <- read.csv("sinker.3.csv", header = TRUE)
pitch_2018 <- read.csv("sinker.4.csv", header = TRUE)
pitch_2019 <- read.csv("sinker.5.csv", header = TRUE)

sinker <- rbind(pitch_2015, pitch_2016, pitch_2017, pitch_2018, pitch_2019)
sinker[is.na(sinker)] <- 0

sinkerp <- ifelse(sinker$n_sift_formatted >= 15, "Sinkerballer", "Other")
sort <- ifelse(sinker$p_starting_p / sinker$p_game < 0.8, "Bullpen", "Starter")


xiso <- sinker$xslg - sinker$xba
table(sinkerp)
table(sort) 

sinker <- cbind(sinker[,-16], xiso, sinkerp, sort)
```

# 싱커는 장타를 억제할 수 없는가?

```{r}
install.packages("ggplot2")
library(ggplot2)

ggplot(data = sinker, mapping = aes(x = year, y = xslg, size = p_formatted_ip)) + geom_point(mapping = aes(color = sinkerp)) + geom_smooth(aes(x = year, y = xslg, linetype = sinkerp)) + facet_wrap(~ sort, nrow = 1)

ggplot(data = sinker, mapping = aes(x = year, y = xiso, size = p_formatted_ip)) + geom_point(mapping = aes(color = sinkerp)) + geom_smooth(aes(x = year, y = xiso, linetype = sinkerp)) + facet_wrap(~ sort, nrow = 1)

ggplot(data = sinker, mapping = aes(x = year, y = xiso)) + geom_smooth(aes(x = year, y = xiso, linetype = sinkerp)) + facet_wrap(~ sort, nrow = 1)

ggplot(data = sinker, mapping = aes(x = year, y = exit_velocity_avg, size = p_formatted_ip)) + geom_point(mapping = aes(color = sinkerp)) + geom_abline(aes(x = year, y = exit_velocity_avg, linetype = sinkerp)) + facet_wrap(~ sort, nrow = 1)
```

# 싱커는 타구의 질을 제어할 수 있는가?

```{r}
ggplot(data = sinker, mapping = aes(x = year, y = hard_hit_percent, size  = p_formatted_ip)) + geom_point(mapping = aes(color = sinkerp)) + geom_smooth(aes(x = year, y = hard_hit_percent, linetype = sinkerp)) + facet_wrap(~ sort, nrow = 1)

ggplot(data = sinker, mapping = aes(x = year, y = groundballs_percent, size  = p_formatted_ip)) + geom_point(mapping = aes(color = sinkerp)) + geom_smooth(aes(x = year, y = groundballs_percent, linetype = sinkerp)) + facet_wrap(~ sort, nrow = 1)
```

# 타구 속도와 타구 각도

```{r}
ggplot(data = sinker, mapping = aes(x = year, y = exit_velocity_avg, size  = p_formatted_ip)) + geom_point(mapping = aes(color = sinkerp)) + geom_smooth(aes(x = year, y = exit_velocity_avg, linetype = sinkerp)) + facet_wrap(~ sort, nrow = 1)

ggplot(data = sinker, mapping = aes(x = year, y = launch_angle_avg, size  = p_formatted_ip)) + geom_point(mapping = aes(color = sinkerp)) + geom_smooth(aes(x = year, y = launch_angle_avg, linetype = sinkerp)) + facet_wrap(~ sort, nrow = 1)
```

# 그 외

```{r}
ggplot(data = sinker, mapping = aes(x = year, y = whiff_percent)) + labs(x="연도", y="헛스윙률") + theme(legend.title = element_blank()) + stat_summary(fun = median, geom = 'line', aes(linetype = sinkerp)) + facet_wrap(~sort, nrow = 1)

ggplot(data = sinker, mapping = aes(x = year, y = whiff_percent, size  = p_formatted_ip)) + geom_smooth(aes(x = year, y = whiff_percent, linetype = sinkerp)) + facet_wrap(~ sort, nrow = 1)

ggplot(data = sinker, mapping = aes(x = year, y = woba, size  = p_formatted_ip)) + geom_point(mapping = aes(color = sinkerp)) + geom_smooth(aes(x = year, y = woba, linetype = sinkerp)) + facet_wrap(~ sort, nrow = 1)

ggplot(data = sinker, mapping = aes(x = year, y = xwoba, size  = p_formatted_ip)) + geom_point(mapping = aes(color = sinkerp)) + geom_smooth(aes(x = year, y = xwoba, linetype = sinkerp)) + facet_wrap(~ sort, nrow = 1)

ggplot(data = sinker, mapping = aes(x = year, y = xba, size  = p_formatted_ip)) + geom_point(mapping = aes(color = sinkerp)) + geom_smooth(aes(x = year, y = xba, linetype = sinkerp)) + facet_wrap(~ sort, nrow = 1)
```

# 내감 후 플롯

```{r}
install.packages("showtext")
library("showtext")
font_add_google("Noto Sans KR", "notosanskr")
showtext_auto()
sinker$sort <- factor(sinker$sort, levels = c("Starter", "Bullpen"))

library(ggthemes)

ggplot(data = sinker, mapping = aes(x = year, y = xiso)) + theme_fivethirtyeight() + labs(x="연도", y="기대 순장타율") + theme(legend.title = element_blank()) + stat_summary(fun = median, geom = 'line', aes(colour = sinkerp)) + facet_wrap(~sort, nrow = 1)

ggplot(data = sinker, mapping = aes(x = year, y = exit_velocity_avg)) + theme_fivethirtyeight() + labs(x="연도", y="타구 속도") + theme(legend.title = element_blank()) + scale_y_continuous(breaks = c(87.0, 87.5, 88.0, 88.5), labels = paste0(c(87.0, 87.5, 88.0, 88.5), "mph")) + stat_summary(fun = median, geom = 'line', aes(colour = sinkerp)) + facet_wrap(~sort, nrow = 1)

ggplot(data = sinker, mapping = aes(x = year, y = groundballs_percent)) + labs(x="연도", y="땅볼 비율") + theme_fivethirtyeight() + theme(legend.title = element_blank()) + scale_y_continuous(breaks = c(42, 45, 48), labels = paste0(c(42, 45, 48), "%")) + stat_summary(fun = median, geom = 'line', aes(colour = sinkerp)) + facet_wrap(~sort, nrow = 1)

ggplot(data = sinker, mapping = aes(x = year, y = whiff_percent)) + theme_fivethirtyeight() + labs(x="연도", y="헛스윙률") + theme(legend.title = element_blank()) + scale_y_continuous(breaks = 22:27, labels = paste0(22:27, "%")) + stat_summary(fun = median, geom = 'line', aes(colour = sinkerp)) + facet_wrap(~sort, nrow = 1)

ggplot(data = sinker, mapping = aes(x = year, y = xba)) + labs(x="연도", y="기대 타율") + theme_fivethirtyeight() + theme(legend.title = element_blank()) + stat_summary(fun = median, geom = 'line', aes(colour = sinkerp)) + facet_wrap(~sort, nrow = 1)

```
