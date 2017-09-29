---
title: "R Notebook for withdrawal effects on pilocarpine-induced seizure (LaNeC)"
author:
- Caio Maximino^[Universidade Federal do Sul e Sudeste do Pará]
- Monica Gomes Lima^[Universidade do Estado do Pará]
- Suianny Nayara da Silva Chaves^[Universidade Federal do Sul e Sudeste do Pará]
output:
  github_document: default
  html_notebook: default
subtitle: From project Behavioral and biochemical effects of ethanol withdrawal in
  zebrafish
tags:
- ethanol withdrawal
- zebrafish
- seizure
abstract: |
  Behavioral data from the in vivo pilocarpine-induced seizure experiment. Datasets represent latencies to reach Score IV seizures, organized as survival objects. In the dataset, 'start' refers to the beggining of the experiment (i.e., time 0), while 'stop' refers to the onset of the seizure event.
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook for the data analysis of the research project "Behavioral and biochemical effects of ethanol withdrawal in zebrafish". 

Data is produced by members from Laboratório de Neurociências e Comportamento "Frederico Guilherme Graeff", affiliated to Universidade Federal do Sul e Sudeste do Pará and Universidade do Estado do Pará. The package will include primary data for a behavioral experiment on the effects of ethanol withdrawal on zebrafish anxiety-like behavior, as well as data and scripts for a meta-analysis of behavioral data on zebrafish.

When you execute code within the notebook, the results appear beneath the code.

Behavioral data from the in vivo pilocarpine-induced seizure experiment. Datasets represent latencies to reach Score IV seizures, organized as survival objects. In the dataset, 'start' refers to the beggining of the experiment (i.e., time 0), while 'stop' refers to the onset of the seizure event.

[![DOI](https://zenodo.org/badge/95811139.svg)](https://zenodo.org/badge/latestdoi/95811139)

Load needed libraries:
```{r}
if(!require(survival)){
    install.packages("survival")
    library(ggplot2)
}
if(!require(RCurl)){
    install.packages("RCurl")
    library(RCurl)
}
```


Load data from github and inspect it:
```{r}
x <- getURL("https://raw.githubusercontent.com/lanec-unifesspa/etoh-withdrawal/master/seizure/latenciestoIV.csv")
piloseiz <- read.csv(text = x)
piloseiz$Group <- as.factor(piloseiz$Group)
View(piloseiz)
```

Define dataset as a survival object
```{r}
latsurv <- Surv(time = piloseiz$start, time2 = piloseiz$stop, event = piloseiz$seizure)
```

Apply log-rank model to analyze differences between latencies
```{r}
survdiff(Surv(piloseiz$stop, piloseiz$seizure) ~ Group, data = piloseiz)
```

Produce figure on cumulative hazards
```{r}
plot(survfit(Surv(stop, seizure) ~ Group, data = piloseiz), conf.int = TRUE, fun = "cumhaz", xlab="Time (s)", ylab = "Cumulative risk of seizure", col = c("green2", "red"))
```