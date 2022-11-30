---
title: "vbreak"
author: "2017150411 조광은"
date: '2021 3 1 '
output: html_document
---

# 데이터

```{r}
library(dplyr)
library(ggplot2)

setwd("C:/Users/OOTP/Documents/R")
getwd()
vbreak_1 <- read.csv("vbreak_151617.csv")
vbreak_2 <- read.csv("vbreak_181920.csv")
vbreak <- rbind(vbreak_1, vbreak_2)
head(vbreak)

vbreak_1920 <- vbreak_2 %>% filter(year == 2019 | year == 2020) %>% filter(!is.na(n_ff_formatted))
arsenal_1 <- read.csv("arsenal2019.csv")
arsenal_1$year <- 2019
arsenal_2 <- read.csv("arsenal2020.csv")
arsenal_2$year <- 2020
arsenal <- rbind(arsenal_1, arsenal_2)
vbreak_whiff <- inner_join(vbreak_1920, arsenal, by = c("last_name", "first_name","year"))
vbreak_whiff

vbreak <- vbreak %>% mutate(flyvpop = round(100 * popups_percent/flyballs_percent, 1)) %>% filter(n_ff_formatted > 30)
vbreak_whiff <- vbreak_whiff %>% mutate(flyvpop = round(100 * popups_percent/flyballs_percent, 1))
nrow(vbreak)
set.seed(1)
vbreak_train <- sample_frac(vbreak, size = 0.7)
```

# 회귀

그대로 수직 무브먼트로 하면 정규성이 만족되지 않는다. 따라서 정규성을 만족할 수 있게 로그변환을 해준다.
수직 무브먼트 값이 모두 음수값이므로 로그변환을 위해 바꿔준다.

```{r}
library(geepack)
vbreak_reg <- lm(formula = log(-(ff_avg_break_z)) ~ iz_contact_percent + ff_avg_speed + flyvpop + oz_contact_percent, data = vbreak_train)
summary(vbreak_reg)

## 그림으로 검정
plot(vbreak_reg)

## 정규성 검정
shapiro.test(rstandard(vbreak_reg))
```

## 아웃라이어 제거

```{r}
cook <- cooks.distance(vbreak_reg)
cookst <- 4 / (799 - 18 - 1)
plot(cook)
outlier_index <- cook[cook > cookst]
outlier_index_df <- as.data.frame(outlier_index)[,1]

vbreak_train_without_1 <- vbreak_train[-outlier_index_df, ]
vbreak_outlier <- vbreak_train[outlier_index_df, ]
vbreak_reg_without_1 <- lm(formula = (-ff_avg_break_z) ~ iz_contact_percent + ff_avg_speed + flyvpop + oz_contact_percent, data = vbreak_train_without_1)
summary(vbreak_reg_without_1)
plot(vbreak_reg_without_1)

shapiro.test(rstandard(vbreak_reg_without_1))
```

## 박스콕스 변환

```{r}
library(MASS)
bc_vbreak <- boxcox(vbreak_reg_without_1)
lambda <- bc_vbreak$x[which.max(bc_vbreak$y)]
lambda

vbreak_reg_adj <- lm(formula = (-(ff_avg_break_z))^lambda ~ iz_contact_percent + ff_avg_speed + flyvpop + oz_contact_percent, data = vbreak_train_without_1)
summary(vbreak_reg_adj)
shapiro.test(rstandard(vbreak_reg_adj))
plot(vbreak_reg_adj)

## 다중공선성
library(car)
vif(vbreak_reg)

## 정규성 검정

shapiro.test(residuals(vbreak_reg_without_1))
bc_vbreak_without <- boxcox(vbreak_reg_without_1)
lambda_without <- bc_vbreak$x[which.max(bc_vbreak_without$y)]
lambda_without
vbreak_reg_without_2 <- lm((-ff_avg_break_z)^lambda_without ~ iz_contact_percent + ff_avg_speed + flyvpop + oz_contact_percent, data = vbreak_train_without_1)
summary(vbreak_reg_without_2)
shapiro.test(resid(vbreak_reg_without_2))

fitted.value <- -(fitted(vbreak_reg_without_1) ^ (1/lambda_without))
realvalue <- vbreak_train_without_1$ff_avg_break_z
head(fitted.value,30) ; head(realvalue,30)
vbreak_train_without_1$fitted.value <- fitted.value
```

# 테스트

