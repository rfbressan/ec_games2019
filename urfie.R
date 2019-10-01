---
title: "Using R for Introductory Econometrics"
output: html_document
---
  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


#' 

#' http://www.urfie.net/downloads.html

library(wooldridge) # Load data sets
library(plm)
library(estimatr)
library(lmtest)
library(car)
library(MASS)
library(tidyverse)
library(pmdplyr)
library(broom)
library(forcats)
library(purrr)
library(stargazer)

#' Carregando os dados 

data("gpa3")

#' Análise descritiva

#' Primeiro separar variáveis qualitativas das quantitativas

quali <- gpa3 %>% 
  select(term, season, frstsem, spring, female, black, football) %>% 
  mutate_all(as.factor)

quanti <- gpa3 %>% 
  select(-c(term, season, frstsem, spring, female, black, football, white))

#' Descrição das qualitativas

desc_quali <- quali %>% 
  pivot_longer(everything()) %>% 
  count(name, value) %>% 
  pivot_wider(names_from = value, values_from = n)

#+ results = "asis"

stargazer(desc_quali, summary = FALSE, rownames = FALSE, type = "html")

#' Descrição das quantitativas
  
desc_quanti <- quanti %>% 
  pivot_longer(everything()) %>% 
  group_by(name) %>% 
  summarise(media = mean(value, na.rm = TRUE), 
            mediana = median(value, na.rm = TRUE),
            desv_pad = sd(value, na.rm = TRUE),
            na = sum(is.na(value)))

#+ results = "asis"

stargazer(desc_quanti, summary = FALSE, rownames = FALSE, type = "html")

#' Boxplot para detecção de outliers
#' 

quanti %>% 
  select(-id) %>% 
  mutate_all(scale) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x = name, y = value, color = name)) +
  geom_boxplot() +
  labs(title = "Boxplot",
       y = "Valor",
       x = "Variável") +
  guides(color = FALSE) +
  theme_classic()


#' Multiple OLS Regression. Example 8.2

reg <- gpa3 %>% 
  filter(spring == 1) %>% 
  lm(cumgpa ~ sat + hsperc + tothrs + female + black + white, 
     data = .)

#' Coeficientes com erros padrões **não robustos**
#+ results = "asis" 

stargazer(reg, type = "html")

#' Coeficientes com erros padrões **robustos**
#+ results = "asis"

stargazer(coeftest(reg, vcov. = hccm), type = "html", 
          title = "Resultados robustos", align = TRUE,
          no.space = TRUE)

#' Teste F para black=white=0

myH0 <- c("black", "white")

#+ results = "asis"
stargazer(linearHypothesis(reg, myH0, vcov. = hccm), type = "html")

#' Regressão robusta com `rlm` do pacote MASS

rob_reg <- gpa3 %>% 
  filter(spring == 1) %>% 
  rlm(cumgpa ~ sat + hsperc + tothrs + female + black + white, 
      data = .)

#+ results = "asis"
stargazer(rob_reg, type = "html")

#' Mínimos quadrados ponderados

data("k401ksubs")
wlsreg <- k401ksubs %>% 
  filter(fsize == 1) %>% 
  lm(nettfa ~ inc+I((age-25)^2)+male+e401k,
     weights = 1/inc,
     data = .)

#+ results = "asis"
stargazer(coeftest(wlsreg), coeftest(wlsreg, vcov. = hccm),
          type = "html",
          no.space = TRUE,
          align = TRUE,
          column.labels = c("Padrão", "Robusto"))

#' Teste de Breusch-Pagan para heterocedasticidade

bp <- bptest(wlsreg)

#+ results = "asis"
bp

#' Teste RESET de Ramsey

data("hprice1")
orig <- hprice1 %>% 
  lm(price ~ lotsize+sqrft+bdrms, data = .)

#+ results = "asis"
resettest(orig)
