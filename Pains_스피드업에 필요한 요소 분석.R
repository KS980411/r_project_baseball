---
title: "KBO gameday"
output: html_document
date: "2022-11-13"
---

## 데이터 읽기

```{r}
library(readr)
gameday <- read_csv("gameday.csv")
```

## 데이터 전처리

### 투수 교체 횟수

Raw Data에서는 게임에 나오는 투수들의 수를 기록했다.

하지만 엄밀히 말해서 우리가 원하는 것은 투수를 얼마나 썼냐가 아닌 투수를 얼마나 교체했냐이기 때문에 조사한 데이터에서 2를 빼(선발 제외) 투수 교체 횟수를 뽑아준다.

### 이닝

우리는 동일한 환경에서 어떤 요소가 경기 시간에 영향을 미치는지 보고싶으므로 시간 변수를 조정할 필요가 있다.

연장 12회(24이닝)에서 기록한 경기 시간과 정규 이닝 9회에서 얻은 게임 시간은 다를 수밖에 없으므로 이를 고려하여 Y 변수인 시간에 이닝을 나눠주어 이닝 당 평균 시간을 구한다.

앞으로 분석에서도 이닝 당 평균 시간이 Y임을 분명히 하고 분석에 들어가도록 한다.

```{r}
library(dplyr)
gameday <- gameday %>% mutate(pitcher_rf = pitcher - 2) %>% mutate(real_time = time / inning)
```

### 상관도

변수들 간의 상관 관계를 본다.

```{r}
var_gameday <- gameday %>% select(runs, hits, error, fourball, pitcher_rf)
cor_gameday <- cor(var_gameday)

library(corrplot)
corrplot(cor_gameday, method = "number")

library(psych)
fa_result <- fa(var_gameday, nfactors = 2, rotation = "varimax")
fa_result$RMSEA
```

상관 관계가 높은 hits/runs 그리고 pitcher_rf, fourball으로 FA를 시도했으나 RMSEA를 보면 FA 모형이 적합하지 않다.

따라서 다중회귀에서 변수 선택을 통해 dimension을 줄여나갈 것이다.

### 외부 변수 고려

## 다중회귀분석

```{r}
library(MASS)
gameday_full <- lm(real_time ~ runs + hits + error + fourball + pitcher_rf, data = gameday)
summary(gameday_full)
step(gameday_full, direction = "both")
```

변수 선택의 관점에서 볼 때는 변수를 빼지 않는 것이 가장 적합하다.

```{r}
plot(gameday_full)
plot(gameday_full, 4)
mean(hatvalues(gameday_full)) ; hatvalues(gameday_full)[402]
```

아웃라이어 402번을 데이터셋에서 빼놓고 다시 해보자.

```{r}
gameday_outlier <- gameday[402,]
gameday <- gameday[-402, ]
gameday_full <- lm(real_time ~ runs + hits + error + fourball + pitcher_rf, data = gameday)
plot(gameday_full)
```

잔차분석을 위해서 몇 가지 test를 진행한다.

위의 플롯으로 대충 확인이 가능하지만, 정량적으로 통계량을 구해 다시 검증해보자.

```{r}
library(car)
library(lmtest)
vif(gameday_full) # predictor들 사이의  다중공선성 조사
dwtest(gameday_full) # autocorrelation 조사
shapiro.test(gameday_full$residuals) # 정규성 조사
```

Autocorrelation, Normality, Multicollinearity 모두 정상적으로, 잔차 가정을 모두 만족하는 것으로 보인다.

변수 선택에 있어 full model이 합리적인 선택이나, 앞에서 보았듯이 Run과 Hit 사이의 유효한 상관관계가 있고, 분석에서도 이 둘은 중복된 영역을 가리킬 가능성이 높으며, Runs은 full model에서도 유의하지 않기 때문에 Hits만을 독립 변수에 놓고 다중회귀 한 모델을 최종적으로 보자.

```{r}
lm_reduced <- lm(real_time ~ hits + error + fourball + pitcher_rf, data = gameday)
summary(lm_reduced)
plot(lm_reduced)

plot(lm_reduced, 4)

## 잔차분석

vif(lm_reduced) # predictor들 사이의  다중공선성 조사
dwtest(lm_reduced) # autocorrelation 조사
shapiro.test(lm_reduced$residuals) # 정규성 조사

summary(lm_reduced)
```

-   안타, 에러, 포볼이 하나도 없고 투수가 이닝을 완벽히 끝낸다면 한 회는 약 6.84분이 걸린다.

-   안타는 하나 당 평균 경기 시간을 0.09분 정도 늘린다. 2022 시즌 KBO 타자가 타석에서 안타를 기록할 확률은 안타를 타석으로 나눈 22.9%이다. 즉, 안타의 관점으로 봤을 때 리그 평균의 안타 확률을 기록하는 타자는 경기 0.38분을 지연시켰다. (0.23 \* 1.67)

    -   타율을 지표로 쓰지 않은 이유는 타율의 분모가 되는 타수는 볼넷과 사구, 희생타 등을 포함하지 않기 때문이다. 따라서 타석을 분모로 사용하여 동일한 상황에서 경기 시간의 변동을 보고자 한다.

-   실책은 하나 당 경기를 1.45분 정도 늘린다. 2022 시즌 KBO 야수가 실책을 기록할 확률은 2.1% 이다. 리그 평균의 필딩율을 가진 야수는 하나의 타구 당 경기 0.03분을 지연시켰다(0.021 \* 1.45) . 또한, 타석에서 일어나는 사건이 실책을 유발하는 인플레이 타구 lm_외에도 삼진, 볼넷 등 다른 경우의 수가 있으므로 이 변동은 더 내려간다. 2022시즌 리그 인플레이 타구 비율은 70.1%로, 이를 감안하면 동일한 1타석에서 실책이라는 이벤트는 경기 시간을 0.02분 지연시킨다.

-   볼넷은 하나 당 경기를 2.86분 정도 늘린다. 2022 시즌 KBO 투수가 사사구를 허용할 확률은 9% 내외. 리그 평균의 제구력을 가진 투수는 한 타자 당 경기를 약 0.26분을 지연시켰다. (2.86 \* 0.09)

-   투수교체는 하나 당 경기를 2.62분 정도 늘린다.

```{r}
median(gameday$pitcher_rf) ; mean(gameday$pitcher_rf)
```

경기 당 투수 교체는 대략 평균 7.3회 일어나지만, 연장을 고려하면 7회 정도로 계산하는 것이 적절하다. 다른 변수를 고정한 채 게임에서 투수 교체로 지연된 시간은 대략 20분 정도이다.

## 본 분석의 결론

-   여기서 말하는 경기 시간이란, 9회말까지 진행되는 9이닝(18 반이닝) 경기를 가정한다.

첫 번째로, 안타(타자) / 볼넷(투수) / 실책(야수) / 투수 교체(감독) 모두 경기 시간에 유의미한 영향을 끼친다. 이는 야구의 모든 영역이 경기 시간에 영향을 끼친다고 해석할 수 있다.

두 번째로, 단위 하나 당 경기 시간을 가장 많이 늘리는 것은 볼넷이다. 볼넷은 하나 당 경기를 2.8분 늘린다.

세 번째로, 각각 이벤트의 확률을 고려해보면, 전체 경기 시간에 가장 많이 영향을 미치는 것은 안타이다. 볼넷, 실책, 투수교체보다 안타의 확률이 월등하게 높기 때문이다. 완전히 평준화된 타석 상황에서, 한 타석 당 발생하는 이벤트 중 "가장 많은 빈도"로 경기를 지연시키는 것은 안타라는 것이다. 따라서 이론적으로는 경기 내 요소 중에서 안타를 줄이는 것이 가장 효과적일 것이다.

네 번째로, 그럼에도 스피드업의 방안으로 인위적인 투고타저를 만드는 것에 대해서는 보다 현실적으로 바라보아야 한다. 타구가 안타가 될 변인은 일반적으로 타구 각도와 타구 속도인데, 이 중 타구 속도는 공인구로 조정할 수 있지만 타구 각도는 조절할 수 없다. 따라서 리그 차원에서 이를 줄이기 위해 조정할 수 있는 요소가 제한적이다. 또한, 스피드업의 배경이 야구의 인기와 직결된다는 점을 고려한다면, 스피드업을 위해 타격 결과를 제한하는 것은 오히려 역효과를 불러올 수 있다. 앞서 full model에서 득점이 전체 경기 시간에 미치는 영향이 유의하지 않다는 결과를 참고하면, 결과적으로 득점에 미치는 영향을 조정하는 것이 의미가 없을 수 있다.

다섯 번째로, 투수 교체는 생각보다 많은 영향을 끼친다. 위의 게임에서의 상황은 연속적으로 게임이 진행되지만, 이닝 중의 투수 교체는 게임을 강제적으로 멈추게 한다. 또한, 위에서 측정한 변수는 이닝 전의 투수 교체를 포함한 수치이기 때문에 직접적으로 경기 시간에 영향을 미치는 이닝 중 투수 교체는 하나 당 2.62분보다 경기 시간을 더 늘릴 것이다.

여섯 번째로, 위의 변수는 실제로 스피드업의 목표인 투수의 인터벌 축소를 고려하지 않았다. 리그 전체적으로 가장 강제하기 쉬운 요소를 고려하지 않았기에 직접적으로 스피드업에 적용하는 것은 무리가 있다.

따라서, 본 연구는 다음과 같은 개선 방향을 제시한다.

첫째, 단위 당 가장 많은 변동을 불러오는 볼넷을 인위적으로 조정할 필요가 있다. 스트라이크 존의 확대가 그 방법이 될 수 있을 것이다. 이는 볼의 제한 개수(4개) 전에 타격 이벤트를 발생시킬 가능성을 높일 것이다.

둘째, 안타 확률 조정을 위해 공인구를 조정하는 것은 섣불리 접근해서는 안 된다. 일반적으로 득점은 리그의 인기와 직결된다. 물론 최근 일본 프로야구리그(NPB)는 극도의 투고타저임에도 불구하고 많은 관중을 동원하고 있지만, 이는 탄탄한 지역적 기반과 일본 국내에서 야구가 점하고 있는 위치를 생각하면 그대로 한국 리그에 적용시키기에 무리가 있다. 따라서 합리적인 개선 방향은, 앞서 언급했던 스트라이크 존 확대와 공인구 반발력 기준 축소를 같이 진행하면서 득점은 많이 나되, 인플레이 타구의 확률을 줄이는 것이 될 것이다.

셋째, 투수 교체의 제한은 경기 시간 축소를 위해서 필수적이다. 2022시즌 KBO리그의 선발 평균 이닝은 5.33(일반적으로 5.1이닝) 이닝으로, 불펜을 일반적으로 1이닝으로 끊어간다면 4번의 투수 교체가 수반된다. 여기에 좌완 원포인트 혹은 외국인 타자들의 상대로 언더핸드를 올리는 등 이닝을 쪼깬다면 추가적인 투수 교체가 필요할 것이다. 이러한 상황을 막기 위해, MLB가 도입한 '3타자 상대 룰'처럼 한 투수 당 상대하는 타자의 하한선을 두는 것도 좋은 방법이 될 것이다.

넷째, 추가적인 스피드업을 위해서는 이러한 조정뿐만 아니라 투수의 인터벌 축소를 위한 피치클락 도입, 공수 교대 시간 제한 등 다각도로 경기 시간을 줄여나가는 시도를 해야할 것이다.
