#Load R libraries for the analyses
library(readr)
library(metafor)

#Read the csv dataset; should be edited to address the specific location of the file in your machine
etoh_withdrawal_metanalysis <- read_csv("etoh-withdrawal-metanalysis.csv") 

#Calculate standardized mean differences for each study/experiment
mod1 <- escalc(measure="SMD", m1i=mctrl, m2i=mtrat, n1i=nctrl, n2i=ntrat, sd1i=sdctrl, sd2i=sdtrat, data = etoh_withdrawal_metanalysis) 

#Fit a mixed-effects model on the calculated SMDs
res.mod1 <- rma(yi, vi, mods = ~ teste + conc + t.trat + t.abst + strain, data = mod1, slab=paste(conc))

#Run a permutation test, with 1,000 iterations, on the effects of the moderators from the previous model
permres <- permutest(res.mod1, iter = 1000, retpermdist = TRUE) 

#Regression Test for Funnel Plot Asymmetry
regtest(res.mod1, model="lm") 

#Influential studies analysis; generated Table S1
influence(res.mod1) 

#Forest plot (Figure 1A)
forest(res.mod1)

#Generate histogram for the permutation distributoin of the test statistic for behavioral test (Figure 1B)
hist(permres$zval.perm[,2], breaks = 140, freq = FALSE, xlim = c(-5, 5),
ylim = c(0, 0.4), main = "", xlab = "Value of Test Statistic")
abline(v = res$zval[2], lwd = 2, lty = "dashed")
abline(v = 0, lwd = 2)
curve(dnorm, from = -5, to = 5, add = TRUE, lwd = 2,
col = rgb(1, 0, 0, alpha = 0.7))
lines(density(permres$zval.perm[,2]), lwd = 2,
col = rgb(0, 0, 1, alpha = 0.7))

#Generate histogram for the permutation distributoin of the test statistic for EtOH concentration (Figure 1C)
hist(permres$zval.perm[,3], breaks = 140, freq = FALSE, xlim = c(-5, 5),
ylim = c(0, 0.4), main = "", xlab = "Value of Test Statistic")
abline(v = res$zval[3], lwd = 2, lty = "dashed")
abline(v = 0, lwd = 2)
curve(dnorm, from = -5, to = 5, add = TRUE, lwd = 2,
col = rgb(1, 0, 0, alpha = 0.7))
lines(density(permres$zval.perm[,3]), lwd = 2,
col = rgb(0, 0, 1, alpha = 0.7))

#Generate histogram for the permutation distributoin of the test statistic for exposure time (Figure 1D)
hist(permres$zval.perm[,4], breaks = 140, freq = FALSE, xlim = c(-5, 5),
ylim = c(0, 0.4), main = "", xlab = "Value of Test Statistic")
abline(v = res$zval[4], lwd = 2, lty = "dashed")
abline(v = 0, lwd = 2)
curve(dnorm, from = -5, to = 5, add = TRUE, lwd = 2,
col = rgb(1, 0, 0, alpha = 0.7))
lines(density(permres$zval.perm[,4]), lwd = 2,
col = rgb(0, 0, 1, alpha = 0.7))

#Generate histogram for the permutation distributoin of the test statistic for withdrawal duration (Figure 1E)
hist(permres$zval.perm[,5], breaks = 140, freq = FALSE, xlim = c(-5, 5),
ylim = c(0, 0.4), main = "", xlab = "Value of Test Statistic")
abline(v = res$zval[5], lwd = 2, lty = "dashed")
abline(v = 0, lwd = 2)
curve(dnorm, from = -5, to = 5, add = TRUE, lwd = 2,
col = rgb(1, 0, 0, alpha = 0.7))
lines(density(permres$zval.perm[,5]), lwd = 2,
col = rgb(0, 0, 1, alpha = 0.7))

#Generate histogram for the permutation distributoin of the test statistic for strain/phenotype (Figure 1F)
hist(permres$zval.perm[,6], breaks = 140, freq = FALSE, xlim = c(-5, 5),
ylim = c(0, 0.4), main = "", xlab = "Value of Test Statistic")
abline(v = res$zval[6], lwd = 2, lty = "dashed")
abline(v = 0, lwd = 2)
curve(dnorm, from = -5, to = 5, add = TRUE, lwd = 2,
col = rgb(1, 0, 0, alpha = 0.7))
lines(density(permres$zval.perm[,6]), lwd = 2,
col = rgb(0, 0, 1, alpha = 0.7))

#Funnel plot (Figure 1G)
funnel(res.mod1, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline = 0)

