---
title: "Rvest"
author: "2017150411 조광은"
date: '2021 1 23 '
output: html_document
---

```{r}
library(rvest)
library(dplyr)
```

# 퓨처스리그 데이터 크롤링

```{r}
url_kw='https://www.koreabaseball.com/Futures/Player/Hitter.aspx'
html_kw=read_html(url_kw, encoding = 'UTF-8') # 한국어 데이터는 UTF-8 한국어 기준
table=html_nodes(x=html_kw, xpath ='//*[@id="cphContents_cphContents_cphContents_udpRecord"]/div[2]/table') # 사이트에 검사 눌러서 해당 테이블 확인
table1=html_table(table)

Sys.getlocale()
Sys.setlocale("LC_ALL", "English")

table1=html_table(table)

Sys.setlocale("LC_ALL", "Korean")
table1
```

