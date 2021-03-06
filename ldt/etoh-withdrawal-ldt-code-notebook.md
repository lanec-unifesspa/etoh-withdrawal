R Notebook for light/dark test data (LaNeC)
================
Caio Maximino[1]

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook for the data analysis of the research project "Behavioral and biochemical effects of ethanol withdrawal in zebrafish".

Data is produced by members from Laboratório de Neurociências e Comportamento "Frederico Guilherme Graeff", affiliated to Universidade Federal do Sul e Sudeste do Pará and Universidade do Estado do Pará. The package will include primary data for a behavioral experiment on the effects of ethanol withdrawal on zebrafish anxiety-like behavior, as well as data and scripts for a meta-analysis of behavioral data on zebrafish.

When you execute code within the notebook, the results appear beneath the code.

[![DOI](https://zenodo.org/badge/95811139.svg)](https://zenodo.org/badge/latestdoi/95811139)

Load libraries used in the analyses and plots

``` r
if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
}
```

    ## Loading required package: ggplot2

``` r
if(!require(coin)){
    install.packages("coin")
    library(coin)
}
```

    ## Loading required package: coin

    ## Loading required package: survival

``` r
if(!require(RCurl)){
    install.packages("RCurl")
    library(RCurl)
}
```

    ## Loading required package: RCurl

    ## Loading required package: bitops

Download data for withdrawal

``` r
x <- getURL("https://raw.githubusercontent.com/lanec-unifesspa/etoh-withdrawal/master/ldt/data-ldt.csv")
y <- read.csv(text = x)
View(y)
```

Approximative Two-Sample Fisher-Pitman Permutation Test for time on white

``` r
oneway_test(TB ~ Gr, data = y, distribution="approximate"(B=10000))
```

    ## 
    ##  Approximative Two-Sample Fisher-Pitman Permutation Test
    ## 
    ## data:  TB by Gr (CTRL, WD)
    ## Z = -2.1207, p-value = 0.0266
    ## alternative hypothesis: true mu is not equal to 0

Approximative Two-Sample Fisher-Pitman Permutation Test for entry duration

``` r
oneway_test(DE ~ Gr, data = y, distribution="approximate"(B=10000))
```

    ## 
    ##  Approximative Two-Sample Fisher-Pitman Permutation Test
    ## 
    ## data:  DE by Gr (CTRL, WD)
    ## Z = -0.92305, p-value = 0.4238
    ## alternative hypothesis: true mu is not equal to 0

Approximative Two-Sample Fisher-Pitman Permutation Test for erratic swimming

``` r
oneway_test(NE ~ Gr, data = y, distribution="approximate"(B=10000))
```

    ## 
    ##  Approximative Two-Sample Fisher-Pitman Permutation Test
    ## 
    ## data:  NE by Gr (CTRL, WD)
    ## Z = -1.9389, p-value = 0.0547
    ## alternative hypothesis: true mu is not equal to 0

Approximative Two-Sample Fisher-Pitman Permutation Test for risk assessment

``` r
oneway_test(RA ~ Gr, data = y, distribution="approximate"(B=10000))
```

    ## 
    ##  Approximative Two-Sample Fisher-Pitman Permutation Test
    ## 
    ## data:  RA by Gr (CTRL, WD)
    ## Z = -1.9895, p-value = 0.0443
    ## alternative hypothesis: true mu is not equal to 0

Approximative Two-Sample Fisher-Pitman Permutation Test for freezing

``` r
oneway_test(Fr ~ Gr, data = y, distribution="approximate"(B=10000))
```

    ## 
    ##  Approximative Two-Sample Fisher-Pitman Permutation Test
    ## 
    ## data:  Fr by Gr (CTRL, WD)
    ## Z = -0.8082, p-value = 0.7031
    ## alternative hypothesis: true mu is not equal to 0

Approximative Two-Sample Fisher-Pitman Permutation Test for thigmotaxis

``` r
oneway_test(Th ~ Gr, data = y, distribution="approximate"(B=10000))
```

    ## 
    ##  Approximative Two-Sample Fisher-Pitman Permutation Test
    ## 
    ## data:  Th by Gr (CTRL, WD)
    ## Z = -0.41291, p-value = 0.6973
    ## alternative hypothesis: true mu is not equal to 0

Approximative Two-Sample Fisher-Pitman Permutation Test for transitions to white

``` r
oneway_test(Tr ~ Gr, data = y, distribution="approximate"(B=10000))
```

    ## 
    ##  Approximative Two-Sample Fisher-Pitman Permutation Test
    ## 
    ## data:  Tr by Gr (CTRL, WD)
    ## Z = -0.91765, p-value = 0.3757
    ## alternative hypothesis: true mu is not equal to 0

Approximative Two-Sample Fisher-Pitman Permutation Test for squares crossed on white

``` r
oneway_test(SQ ~ Gr, data = y, distribution="approximate"(B=10000))
```

    ## 
    ##  Approximative Two-Sample Fisher-Pitman Permutation Test
    ## 
    ## data:  SQ by Gr (CTRL, WD)
    ## Z = -2.5956, p-value = 0.0044
    ## alternative hypothesis: true mu is not equal to 0

Plot: Time on white

``` r
ggplot(y, aes(x=Gr, y = TB), color=Gr) + geom_dotplot(binaxis='y', stackdir='center', alpha=0.5, dotsize = 0.75) + coord_cartesian(ylim=c(0,900)) + stat_summary(fun.data=mean_cl_boot, geom="pointrange", color="red") + labs(x = "Group", y = "Time spent on the white compartment (s)")
```

    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.

![](etoh-withdrawal-ldt-code-notebook_files/figure-markdown_github/unnamed-chunk-11-1.png)

Plot: Entry duration

``` r
ggplot(y, aes(x=Gr, y = DE), color=Gr) + geom_dotplot(binaxis='y', stackdir='center', alpha=0.5, dotsize = 0.75) + coord_cartesian(ylim=c(0,100))  + stat_summary(fun.data=mean_cl_boot, geom="pointrange", color="red") + labs(x = "Group", y = "Entry duration (s)")
```

    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.

![](etoh-withdrawal-ldt-code-notebook_files/figure-markdown_github/unnamed-chunk-12-1.png)

Plot: Erratic swimming

``` r
ggplot(y, aes(x=Gr, y = Fr)) + geom_dotplot(binaxis='y', stackdir='center', alpha=0.5, dotsize = 0.75) + stat_summary(fun.data=mean_cl_boot, geom="pointrange", color="red") + labs(x = "Group", y = "Erratic swimming events (N)")
```

    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.

![](etoh-withdrawal-ldt-code-notebook_files/figure-markdown_github/unnamed-chunk-13-1.png)

Plot: Freezing

``` r
ggplot(y, aes(x=Gr, y = Fr)) + geom_dotplot(binaxis='y', stackdir='center', alpha=0.5, dotsize = 0.75) + stat_summary(fun.data=mean_cl_boot, geom="pointrange", color="red") + labs(x = "Group", y = "Freezing duration (s)")
```

    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.

![](etoh-withdrawal-ldt-code-notebook_files/figure-markdown_github/unnamed-chunk-14-1.png)

Plot: Risk assessment

``` r
ggplot(y, aes(x=Gr, y = Th)) + geom_dotplot(binaxis='y', stackdir='center', alpha=0.5, dotsize = 0.75) + stat_summary(fun.data=mean_cl_boot, geom="pointrange", color="red") + labs(x = "Group", y = "Risk assessment (n)")
```

    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.

![](etoh-withdrawal-ldt-code-notebook_files/figure-markdown_github/unnamed-chunk-15-1.png)

Plot: Thigmotaxis

``` r
ggplot(y, aes(x=Gr, y = Th)) + geom_dotplot(binaxis='y', stackdir='center', alpha=0.5, dotsize = 0.75) + stat_summary(fun.data=mean_cl_boot, geom="pointrange", color="red") + labs(x = "Group", y = "Thigmotaxis (s)")
```

    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.

![](etoh-withdrawal-ldt-code-notebook_files/figure-markdown_github/unnamed-chunk-16-1.png)

Plot: Transitions to white

``` r
ggplot(y, aes(x=Gr, y = Tr)) + geom_dotplot(binaxis='y', stackdir='center', alpha=0.5, dotsize = 0.75) + stat_summary(fun.data=mean_cl_boot, geom="pointrange", color="red") + labs(x = "Group", y = "Transitions to white (N)")
```

    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.

![](etoh-withdrawal-ldt-code-notebook_files/figure-markdown_github/unnamed-chunk-17-1.png)

Plot: Squares crossed

``` r
ggplot(y, aes(x=Gr, y = SQ)) + geom_dotplot(binaxis='y', stackdir='center', alpha=0.5, dotsize = 0.75) + stat_summary(fun.data=mean_cl_boot, geom="pointrange", color="red") + labs(x = "Group", y = "Squares crossed on white (N)")
```

    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.

![](etoh-withdrawal-ldt-code-notebook_files/figure-markdown_github/unnamed-chunk-18-1.png)

Download data for chronic treatment (negative controls)

``` r
x <- getURL("https://raw.githubusercontent.com/lanec-unifesspa/etoh-withdrawal/master/ldt/chronic.csv")
chr <- read.csv(text = x)
View(chr)
```

Approximative Two-Sample Fisher-Pitman Permutation Test for time on white

``` r
oneway_test(TB ~ Gr, data = chr, distribution="approximate"(B=10000))
```

    ## 
    ##  Approximative Two-Sample Fisher-Pitman Permutation Test
    ## 
    ## data:  TB by Gr (CTRL, EtOH)
    ## Z = -1.9535, p-value = 0.0471
    ## alternative hypothesis: true mu is not equal to 0

Approximative Two-Sample Fisher-Pitman Permutation Test for entry duration

``` r
oneway_test(DE ~ Gr, data = chr, distribution="approximate"(B=10000))
```

    ## 
    ##  Approximative Two-Sample Fisher-Pitman Permutation Test
    ## 
    ## data:  DE by Gr (CTRL, EtOH)
    ## Z = 1.2638, p-value = 0.2592
    ## alternative hypothesis: true mu is not equal to 0

Approximative Two-Sample Fisher-Pitman Permutation Test for erratic swimming

``` r
oneway_test(NE ~ Gr, data = chr, distribution="approximate"(B=10000))
```

    ## 
    ##  Approximative Two-Sample Fisher-Pitman Permutation Test
    ## 
    ## data:  NE by Gr (CTRL, EtOH)
    ## Z = 2.5206, p-value = 0.0111
    ## alternative hypothesis: true mu is not equal to 0

Approximative Two-Sample Fisher-Pitman Permutation Test for freezing

``` r
oneway_test(Fr ~ Gr, data = chr, distribution="approximate"(B=10000))
```

    ## 
    ##  Approximative Two-Sample Fisher-Pitman Permutation Test
    ## 
    ## data:  Fr by Gr (CTRL, EtOH)
    ## Z = 0.39324, p-value = 0.7384
    ## alternative hypothesis: true mu is not equal to 0

Approximative Two-Sample Fisher-Pitman Permutation Test for risk assessment

``` r
oneway_test(RA ~ Gr, data = chr, distribution="approximate"(B=10000))
```

    ## 
    ##  Approximative Two-Sample Fisher-Pitman Permutation Test
    ## 
    ## data:  RA by Gr (CTRL, EtOH)
    ## Z = 0.47794, p-value = 0.7958
    ## alternative hypothesis: true mu is not equal to 0

Approximative Two-Sample Fisher-Pitman Permutation Test for thigmotaxis

``` r
oneway_test(Th ~ Gr, data = chr, distribution="approximate"(B=10000))
```

    ## 
    ##  Approximative Two-Sample Fisher-Pitman Permutation Test
    ## 
    ## data:  Th by Gr (CTRL, EtOH)
    ## Z = 2.2879, p-value = 0.0136
    ## alternative hypothesis: true mu is not equal to 0

Approximative Two-Sample Fisher-Pitman Permutation Test for transitions to white

``` r
oneway_test(Tr ~ Gr, data = chr, distribution="approximate"(B=10000))
```

    ## 
    ##  Approximative Two-Sample Fisher-Pitman Permutation Test
    ## 
    ## data:  Tr by Gr (CTRL, EtOH)
    ## Z = -1.7903, p-value = 0.0771
    ## alternative hypothesis: true mu is not equal to 0

Approximative Two-Sample Fisher-Pitman Permutation Test for squares crossed

``` r
oneway_test(SQ ~ Gr, data = chr, distribution="approximate"(B=10000))
```

    ## 
    ##  Approximative Two-Sample Fisher-Pitman Permutation Test
    ## 
    ## data:  SQ by Gr (CTRL, EtOH)
    ## Z = -1.6434, p-value = 0.103
    ## alternative hypothesis: true mu is not equal to 0

Plot: Time on white

``` r
ggplot(chr, aes(x=Gr, y = TB), color=Gr) + geom_dotplot(binaxis='y', stackdir='center', alpha=0.5, dotsize = 0.75) + coord_cartesian(ylim=c(0,900)) + stat_summary(fun.data=mean_cl_boot, geom="pointrange", color="red") + labs(x = "Group", y = "Time spent on the white compartment (s)")
```

    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.

![](etoh-withdrawal-ldt-code-notebook_files/figure-markdown_github/unnamed-chunk-28-1.png)

Plot: Entry duration

``` r
ggplot(chr, aes(x=Gr, y = DE), color=Gr) + geom_dotplot(binaxis='y', stackdir='center', alpha=0.5, dotsize = 0.75) + coord_cartesian(ylim=c(0,100))  + stat_summary(fun.data=mean_cl_boot, geom="pointrange", color="red") + labs(x = "Group", y = "Entry duration (s)")
```

    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.

![](etoh-withdrawal-ldt-code-notebook_files/figure-markdown_github/unnamed-chunk-29-1.png)

Plot: Erratic swimming

``` r
ggplot(chr, aes(x=Gr, y = Fr)) + geom_dotplot(binaxis='y', stackdir='center', alpha=0.5, dotsize = 0.75) + stat_summary(fun.data=mean_cl_boot, geom="pointrange", color="red") + labs(x = "Group", y = "Erratic swimming events (N)")
```

    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.

![](etoh-withdrawal-ldt-code-notebook_files/figure-markdown_github/unnamed-chunk-30-1.png)

Plot: Freezing

``` r
ggplot(chr, aes(x=Gr, y = Fr)) + geom_dotplot(binaxis='y', stackdir='center', alpha=0.5, dotsize = 0.75) + stat_summary(fun.data=mean_cl_boot, geom="pointrange", color="red") + labs(x = "Group", y = "Freezing duration (s)")
```

    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.

![](etoh-withdrawal-ldt-code-notebook_files/figure-markdown_github/unnamed-chunk-31-1.png)

Plot: Risk assessment

``` r
ggplot(chr, aes(x=Gr, y = Th)) + geom_dotplot(binaxis='y', stackdir='center', alpha=0.5, dotsize = 0.75) + stat_summary(fun.data=mean_cl_boot, geom="pointrange", color="red") + labs(x = "Group", y = "Risk assessment (s)")
```

    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.

![](etoh-withdrawal-ldt-code-notebook_files/figure-markdown_github/unnamed-chunk-32-1.png)

Plot: Thigmotaxis

``` r
ggplot(chr, aes(x=Gr, y = Th)) + geom_dotplot(binaxis='y', stackdir='center', alpha=0.5, dotsize = 0.75) + stat_summary(fun.data=mean_cl_boot, geom="pointrange", color="red") + labs(x = "Group", y = "Thigmotaxis (s)")
```

    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.

![](etoh-withdrawal-ldt-code-notebook_files/figure-markdown_github/unnamed-chunk-33-1.png)

Plot: Transitions to white

``` r
ggplot(chr, aes(x=Gr, y = Tr)) + geom_dotplot(binaxis='y', stackdir='center', alpha=0.5, dotsize = 0.75) + stat_summary(fun.data=mean_cl_boot, geom="pointrange", color="red") + labs(x = "Group", y = "Transitions to white (N)")
```

    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.

![](etoh-withdrawal-ldt-code-notebook_files/figure-markdown_github/unnamed-chunk-34-1.png)

Plot: Squares crossed

``` r
ggplot(chr, aes(x=Gr, y = SQ)) + geom_dotplot(binaxis='y', stackdir='center', alpha=0.5, dotsize = 0.75) + stat_summary(fun.data=mean_cl_boot, geom="pointrange", color="red") + labs(x = "Group", y = "Squares crossed on white (N)")
```

    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.

![](etoh-withdrawal-ldt-code-notebook_files/figure-markdown_github/unnamed-chunk-35-1.png)

[1] Universidade Federal do Sul e Sudeste do Pará

[2] Universidade do Estado do Pará

[3] Universidade Federal do Sul e Sudeste do Pará
