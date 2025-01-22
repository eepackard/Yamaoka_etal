library(readr)
library(tidyverse)
library(lme4)
library(lmerTest)
library(lsmeans)
library(Matrix)
library(visreg)
library(ggplot2)
library(nlme)
library(car)
library(dplyr)

#load data
seedlingsjmar2<-read_excel("raw_data/seedlingsjan22.xlsx")
#make month, status and site as factors
seedlingsjmar2$month<-as.factor(seedlingsjmar2$month)
seedlingsjmar2$month <- fct_relevel(seedlingsjmar2$month, "Jun","Jul","Aug","Sep","Oct")
seedlingsjmar2$site <- as.factor(seedlingsjmar2$site)
lpi.wf$status <- as.factor(lpi.wf$status)
lpi.cc$status <- as.factor(lpi.cc$status)
pp.wf$status <- as.factor(pp.wf$status)

#vectors of site types with months in the correct order----
lpi.wf<- seedlingsjmar2[which(seedlingsjmar2$site %in% c("GE","GW","Law")),c(1:32)]
lpi.cc<- seedlingsjmar2[which(seedlingsjmar2$site %in% c("12N3","12N2","12N4")),c(1:32)]
pp.wf<- seedlingsjmar2[which(seedlingsjmar2$site %in% c("EE","EW","CL")),c(1:32)]

#write clean data
write_csv(pp.wf,"clean_data/pp.wf.csv")
write_csv(lpi.wf,"clean_data/lpi.wf.csv")
write_csv(lpi.cc,"clean_data/lpi.cc.csv")


#biomass-----

ppwf.biomass.date <- lmer(logbiomass ~ month  + (1|site), data = pp.wf)
visreg(ppwf.biomass.date, xvar = "month",whitespace=0.4, las=1,
       ylab="log biomass", main= "ppwf", xlab="Sampling Date", points=list(cex=0.35), overlay= TRUE)
summary(ppwf.biomass.date)
plot(ppwf.biomass.date)
shapiro.test(residuals(ppwf.biomass.date))

##comparing m vs nm months
ppwf.biomass.julstat <- lmer(logbiomass ~ status + (1|site), data = ppwfjul)
visreg(ppwf.biomass.julstat, xvar = "status")
summary(ppwf.biomass.julstat)
anova(ppwf.biomass.julstat, p.adj = "bonferroni")
anova(ppwf.biomass.julstat)
plot(ppwf.biomass.julstat)
shapiro.test(residuals(ppwf.biomass.julstat))

ppwf.logtotaln.augstat <- lmer(logbiomass ~ status + (1|site), data = ppwfaug)
visreg(ppwf.logtotaln.augstat, xvar = "status")
anova(ppwf.logtotaln.augstat)
plot(ppwf.logtotaln.augstat)
shapiro.test(residuals(ppwf.logtotaln.augstat))


lpiwf.biomass.date <- lmer(logbiomass ~ month + (1|site), data = lpi.wf)
visreg(lpiwf.biomass.date, xvar = "month",whitespace=0.4, las=1,
       ylab="biomass", main= "lpwf", xlab="Sampling Date", points=list(cex=0.5), overlay= TRUE)
summary(lpiwf.biomass.date)
plot(lpiwf.biomass.date)
shapiro.test(residuals(lpiwf.biomass.date))

##comparing m vs nm months
lpiwf.biomass.julstat <-lmer(logbiomass ~ status + (1|site), data = lpiwfjul)
visreg(lpiwf.biomass.julstat, xvar = "status")
anova(lpiwf.biomass.julstat)
plot(lpiwf.biomass.julstat)
shapiro.test(residuals(lpiwf.biomass.julstat))
leveneTest(residuals(lpiwf.biomass.julstat) ~ lpiwfjul$status)
boxplot(residuals(lpiwf.biomass.julstat) ~ lpiwfjul$status)

lpiwf.logchangen.augstat <- lmer(logbiomass ~ status + (1|site), data = lpiwfaug)
visreg(lpiwf.logchangen.augstat, xvar = "status")
anova(lpiwf.logchangen.augstat)
plot(lpiwf.logchangen.augstat)
shapiro.test(residuals(lpiwf.logchangen.augstat))


lpicc.biomass.date <- lmer(logbiomass ~ month  + (1|site), data = lpi.cc)
anova(lpicc.biomass.date)
qqnorm(seedlingsjmar2$biomass, pch = 1, frame = FALSE)
qqline(seedlingsjmar2$biomass, col = "steelblue", lwd = 2)
plot(lpicc.biomass.date)
shapiro.test(residuals(lpicc.biomass.date))

##comparing m vs nm months
lpicc.biomass.julstat <-lmer(logbiomass ~ status + (1|site), data = lpiccjul)
visreg(lpicc.biomass.julstat, xvar = "status")
anova(lpicc.biomass.julstat)
plot(lpicc.biomass.julstat)
shapiro.test(residuals(lpicc.biomass.julstat))

lpicc.biomass.augstat <- lmer(logbiomass ~ status + (1|site), data = lpiccaug)
visreg(lpicc.biomass.augstat, xvar = "status")
anova(lpicc.biomass.augstat)
plot(lpicc.biomass.augstat)
shapiro.test(residuals(lpicc.biomass.augstat))


#logged total N----
ppwf.totaln.datestatm1 <- lmer(logtotalN ~ month + (1|site), data = pp.wf)
visreg(ppwf.totaln.datestat, xvar = "month",whitespace=0.4, las=1, 
       ylab="log total n", main= "pp", xlab="Sampling Date", overlay= TRUE)
summary(ppwf.totaln.datestat)
plot(ppwf.totaln.datestat)
shapiro.test(residuals(ppwf.totaln.datestat))
##comparing m vs nm months
ppwf.logtotaln.julstat <- lmer(logtotalN ~ status + (1|site), data = ppwfjul)
visreg(ppwf.logtotaln.julstat, xvar = "status")
summary(ppwf.logtotaln.julstat)
anova(ppwf.logtotaln.julstat)
plot(ppwf.logtotaln.julstat)
shapiro.test(residuals(ppwf.logtotaln.julstat))

ppwf.logtotaln.augstat <- lmer(logtotalN ~ status + (1|site), data = ppwfaug)
visreg(ppwf.logtotaln.augstat, xvar = "status")
anova(ppwf.logtotaln.augstat)
plot(ppwf.logtotaln.augstat)
shapiro.test(residuals(ppwf.logtotaln.augstat))


lpiwf.totaln.datestat <- lmer(logtotalN ~ month + (1|site), data = lpi.wf)
visreg(lpiwf.totaln.datestat, xvar = "month",whitespace=0.4, las=1, 
       ylab="log total n", main= "lpi wildfire", xlab="Sampling Date", overlay= TRUE)
summary(lpiwf.totaln.datestat)
plot(lpiwf.totaln.datestat)
shapiro.test(residuals(lpiwf.totaln.datestat))
leveneTest(residuals(lpiwf.totaln.datestat) ~ lpi.wf$logtotalN)
leveneTest(lpiwf.totaln.datestat ~ month)

leveneTest(logtotalN ~ month, data=lpi.wf)


##comparing m vs nm months
lpiwf.logtotaln.julstat <-lmer(logtotalN ~ status + (1|site), data = lpiwfjul)
visreg(lpiwf.logtotaln.julstat, xvar = "status")
anova(lpiwf.logtotaln.julstat)
plot(lpiwf.logtotaln.julstat)
shapiro.test(residuals(lpiwf.logtotaln.julstat))
leveneTest(residuals(lpiwf.logtotaln.julstat) ~ lpiwfjul$status)
boxplot(residuals(lpiwf.logtotaln.julstat) ~ lpiwfjul$status)
leveneTest(logtotalN ~ status, data=lpiwfjul)

lpiwf.logchangen.augstat <- lmer(logtotalN ~ status + (1|site), data = lpiwfaug)
visreg(lpiwf.logchangen.augstat, xvar = "status")
anova(lpiwf.logchangen.augstat)
plot(lpiwf.logchangen.augstat)
shapiro.test(residuals(lpiwf.logchangen.augstat))

lpicc.totaln.datestat <- lmer(logtotalN ~ month + (1|site), data = lpi.cc)
visreg(lpicc.totaln.datestat, xvar = "month",whitespace=0.4, las=1, 
       ylab="log total n", main= "lpi cc", xlab="Sampling Date", overlay= TRUE)
summary(lpicc.totaln.datestat)
plot(lpicc.totaln.datestat)
shapiro.test(residuals(lpicc.totaln.datestat))

####comparing m vs nm months
lpicc.logtotaln.julstat <-lmer(logtotalN ~ status + (1|site), data = lpiccjul)
visreg(lpicc.logtotaln.julstat, xvar = "status")
anova(lpicc.logtotaln.julstat)
plot(lpicc.logtotaln.julstat)
shapiro.test(residuals(lpicc.logtotaln.julstat))

lpicc.logtotaln.augstat <- lmer(logtotalN ~ status + (1|site), data = lpiccaug)
visreg(lpicc.logtotaln.augstat, xvar = "status")
anova(lpicc.logtotaln.augstat)
summary(lpicc.logtotaln.augstat)
plot(lpicc.logtotaln.augstat)
shapiro.test(residuals(lpicc.logtotaln.augstat))


#percent n m vs nm-added after, dont forget to inlcude thes pvalues in the adjustment ----
ppwf.pcnt.n.date <- lmer(pcnt.N.shoot ~ month  + (1|site), data = pp.wf)
visreg(ppwf.pcnt.n.date, xvar = "month",whitespace=0.4, las=1,
       ylab="pcnt n", main= "ppwf", xlab="Sampling Date", points=list(cex=0.35), overlay= TRUE)
summary(ppwf.pcnt.n.date)
anova(ppwf.pcnt.n.date)
plot(ppwf.pcnt.n.date)
shapiro.test(residuals(ppwf.pcnt.n.date))
##use tukey to see if there are differences between dates 
lsmeans( ppwf.pcnt.n.date, pairwise~month, adjust="tukey")

ppwf.pcnt.N.shoot.julstat <- lmer(pcnt.N.shoot ~ status + (1|site), data = ppwfjul)
visreg(ppwf.pcnt.N.shoot.julstat, xvar = "status")
summary(ppwf.pcnt.N.shoot.julstat)
anova(ppwf.pcnt.N.shoot.julstat)
plot(ppwf.pcnt.N.shoot.julstat)
shapiro.test(residuals(ppwf.pcnt.N.shoot.julstat))

ppwf.pcnt.N.shoot.augstat <- lmer(pcnt.N.shoot ~ status + (1|site), data = ppwfaug)
visreg(ppwf.pcnt.N.shoot.augstat, xvar = "status")
anova(ppwf.pcnt.N.shoot.augstat)
plot(ppwf.pcnt.N.shoot.augstat)
shapiro.test(residuals(ppwf.pcnt.N.shoot.augstat))

lpiwf.pcnt.n.date <- lmer(pcnt.N.shoot ~ month  + (1|site), data = lpi.wf)
visreg(lpiwf.pcnt.n.date, xvar = "month",whitespace=0.4, las=1,
       ylab="pcnt n", main= "lpwf", xlab="Sampling Date", points=list(cex=0.35), overlay= TRUE)
summary(lpiwf.pcnt.n.date)
anova(lpiwf.pcnt.n.date)
plot(lpiwf.pcnt.n.date)
shapiro.test(residuals(lpiwf.pcnt.n.date))
lsmeans( lpiwf.pcnt.n.date, pairwise~month, adjust="tukey")


lpiwf.pcnt.N.shoot.julstat <-lmer(pcnt.N.shoot ~ status + (1|site), data = lpiwfjul)
visreg(lpiwf.pcnt.N.shoot.julstat, xvar = "status")
anova(lpiwf.pcnt.N.shoot.julstat)
plot(lpiwf.pcnt.N.shoot.julstat)
shapiro.test(residuals(lpiwf.pcnt.N.shoot.julstat))


lpiwf.pcnt.N.shoot.augstat <- lmer(pcnt.N.shoot ~ status + (1|site), data = lpiwfaug)
visreg(lpiwf.pcnt.N.shoot.augstat, xvar = "status")
anova(lpiwf.pcnt.N.shoot.augstat)
plot(lpiwf.pcnt.N.shoot.augstat)
shapiro.test(residuals(lpiwf.pcnt.N.shoot.augstat))


lpicc.pcnt.n.date <- lmer(pcnt.N.shoot ~ month  + (1|site), data = lpi.cc)
visreg(lpicc.pcnt.n.date, xvar = "month",whitespace=0.4, las=1,
       ylab="pcnt n", main= "ccwf", xlab="Sampling Date", points=list(cex=0.35), overlay= TRUE)
summary(lpicc.pcnt.n.date)
anova(lpicc.pcnt.n.date)
plot(lpicc.pcnt.n.date)
shapiro.test(residuals(lpicc.pcnt.n.date))
lsmeans( lpicc.pcnt.n.date, pairwise~month, adjust="tukey")


lpicc.pcnt.N.shoot.julstat <-lmer(pcnt.N.shoot ~ status + (1|site), data = lpiccjul)
visreg(lpicc.pcnt.N.shoot.julstat, xvar = "status")
anova(lpicc.pcnt.N.shoot.julstat)
plot(lpicc.pcnt.N.shoot.julstat)
shapiro.test(residuals(lpicc.pcnt.N.shoot.julstat))

lpicc.pcnt.N.shoot.augstat <- lmer(pcnt.N.shoot ~ status + (1|site), data = lpiccaug)
visreg(lpicc.pcnt.N.shoot.augstat, xvar = "status")
anova(lpicc.pcnt.N.shoot.augstat)
summary(lpicc.pcnt.N.shoot.augstat)
plot(lpicc.pcnt.N.shoot.augstat)
shapiro.test(residuals(lpicc.pcnt.N.shoot.augstat))



###not logged total n M vs NM ----
####comparing m vs nm months
ppwf.totaln.julstat <- lmer(totalNmg ~ status + (1|site), data = ppwfjul)
visreg(ppwf.totaln.julstat, xvar = "status")
summary(ppwf.totaln.julstat)
anova(ppwf.totaln.julstat)
plot(ppwf.totaln.julstat)
shapiro.test(residuals(ppwf.totaln.julstat))
lsmeans(ppwf.totaln.julstat, test.effs=NULL, method.grad='simple')

ppwf.totaln.augstat <- lmer(totalNmg ~ status + (1|site), data = ppwfaug)
visreg(ppwf.totaln.augstat, xvar = "status")
anova(ppwf.totaln.augstat)
plot(ppwf.totaln.augstat)
shapiro.test(residuals(ppwf.totaln.augstat))

lpiwf.totaln.julstat <-lmer(totalNmg ~ status + (1|site), data = lpiwfjul)
visreg(lpiwf.totaln.julstat, xvar = "status")
anova(lpiwf.totaln.julstat)
plot(lpiwf.totaln.julstat)
shapiro.test(residuals(lpiwf.totaln.julstat))

lpiwf.totaln.augstat <- lmer(totalNmg ~ status + (1|site), data = lpiwfaug)
visreg(lpiwf.totaln.augstat, xvar = "status")
anova(lpiwf.totaln.augstat)
plot(lpiwf.totaln.augstat)
shapiro.test(residuals(lpiwf.totaln.augstat))


lpicc.totaln.julstat <-lmer(totalNmg ~ status + (1|site), data = lpiccjul)
visreg(lpicc.totaln.julstat, xvar = "status")
anova(lpicc.totaln.julstat)
summary(lpicc.totaln.julstat) #50% different
plot(lpicc.totaln.julstat)
shapiro.test(residuals(lpicc.totaln.julstat))

lpicc.totaln.augstat <- lmer(totalNmg ~ status + (1|site), data = lpiccaug)
visreg(lpicc.totaln.augstat, xvar = "status")
anova(lpicc.totaln.augstat)
summary(lpicc.totaln.augstat)#95% dffere
plot(lpicc.totaln.augstat)
shapiro.test(residuals(lpicc.totaln.augstat))


#total C-----

ppwf.totalc.datestat <- lmer(logtotalc ~ month + (1|site), data = pp.wf)
visreg(ppwf.totalc.datestat, xvar = "month",whitespace=0.4, las=1, 
       ylab="log total c", main= "pp wildfire", xlab="Sampling Date", overlay= TRUE)
summary(ppwf.totalc.datestat)
plot(ppwf.totalc.datestat)
shapiro.test(residuals(ppwf.totalc.datestat))

lpiwf.totalc.datestat <- lmer(logtotalc ~ month + (1|site), data = lpi.wf)
visreg(lpiwf.totalc.datestat, xvar = "month",whitespace=0.4, las=1, 
       ylab="log total c", main= "lpi wildfire", xlab="Sampling Date", overlay= TRUE)
summary(lpiwf.totalc.datestat)
plot(lpiwf.totalc.datestat)
shapiro.test(residuals(lpiwf.totalc.datestat))

lpicc.totalc.datestat <- lmer(logtotalc ~ month + (1|site), data = lpi.cc)
visreg(lpicc.totalc.datestat, xvar = "month",whitespace=0.4, las=1, 
       ylab="log total c", main= "lpi cc", xlab="Sampling Date", overlay= TRUE)
summary(lpicc.totalc.datestat)
plot(lpicc.totalc.datestat)
shapiro.test(residuals(lpicc.totalc.datestat))

#shoot n15----
ppwf.n15shoot.datestat <- lmer(X15N.shoot ~ month + (1|site), data = pp.wf)
visreg(ppwf.n15shoot.datestat, xvar = "month",whitespace=0.4, las=1, line = list(col=FALSE),
       ylab="Shoot δ 15N", main= "pp", xlab="Sampling Date", points=list(cex=0.75), overlay= TRUE)
summary(ppwf.n15shoot.datestat)
anova(ppwf.n15shoot.datestat)
plot(ppwf.n15shoot.datestat)
shapiro.test(residuals(ppwf.n15shoot.datestat))

##comparing m vs nm months
ppwf.n15shoot.julstat <- lmer(X15N.shoot ~ status + (1|site), data = ppwfjul)
visreg(ppwf.n15shoot.julstat, xvar = "status")
anova(ppwf.n15shoot.julstat)
plot(ppwf.n15shoot.julstat)
shapiro.test(residuals(ppwf.n15shoot.julstat))

ppwf.n15shoot.augstat <- lmer(X15N.shoot ~ status + (1|site), data = ppwfaug)
visreg(ppwf.n15shoot.augstat, xvar = "status")
anova(ppwf.n15shoot.augstat)
plot(ppwf.n15shoot.augstat)
shapiro.test(residuals(ppwf.n15shoot.augstat))


lpiwf.n15shoot.datestat <- lmer(X15N.shoot ~ month  + (1|site), data = lpi.wf)
visreg(lpiwf.n15shoot.datestat, xvar = "month",whitespace=0.4, las=1, 
       ylab="Shoot δ 15N", main= "lpi wildfire", xlab="Sampling Date", overlay= TRUE)
plot(lpiwf.n15shoot.datestat)
shapiro.test(residuals(lpiwf.n15shoot.datestat))
visreg(lpiwf.n15shoot.datestat, xvar = "month",whitespace=0.4, las=1,line = list(col=FALSE),  
       ylab="Shoot δ 15N", main= "lpi wildfire", xlab="Sampling Date", points=list(cex=0.75), overlay= TRUE)
summary(lpiwf.n15shoot.datestat)
plot(lpiwf.n15shoot.datestat)
shapiro.test(residuals(lpiwf.n15shoot.datestat))

##comparing m vs nm months
lpiwf.n15shoot.julstat <- lmer(X15N.shoot ~ status + (1|site), data = lpiwfjul)
visreg(lpiwf.n15shoot.julstat, xvar = "status")
anova(lpiwf.n15shoot.julstat)
plot(lpiwf.n15shoot.julstat)
shapiro.test(residuals(lpiwf.n15shoot.julstat))

lpiwf.n15shoot.augstat <- lmer(X15N.shoot ~ status + (1|site), data = lpiwfaug)
visreg(lpiwf.n15shoot.augstat, xvar = "status")
anova(lpiwf.n15shoot.augstat)
plot(lpiwf.n15shoot.augstat)
shapiro.test(residuals(lpiwf.n15shoot.augstat))


lpicc.n15shoot.datestat <- lmer(X15N.shoot ~ month  + (1|site), data = lpi.cc)
visreg(lpicc.n15shoot.datestat, xvar = "month",whitespace=0.4, las=1, line = list(col=FALSE), 
       ylab="Shoot δ 15N", main= "lpi cc", xlab="Sampling Date", overlay= TRUE)
summary(lpicc.n15shoot.datestat)
plot(lpicc.n15shoot.datestat)
shapiro.test(residuals(lpicc.n15shoot.datestat))
####comparing m vs nm months
lpicc.n15shoot.julstat <-lmer(X15N.shoot ~ status + (1|site), data = lpiccjul)
visreg(lpicc.n15shoot.julstat, xvar = "status")
anova(lpicc.n15shoot.julstat)
plot(lpicc.n15shoot.julstat)
shapiro.test(residuals(lpicc.n15shoot.julstat))

lpicc.n15shoot.augstat <- lmer(X15N.shoot ~ status + (1|site), data = lpiccaug)
visreg(lpicc.n15shoot.augstat, xvar = "status")
anova(lpicc.n15shoot.augstat)
plot(lpicc.n15shoot.augstat)
shapiro.test(residuals(lpicc.n15shoot.augstat))

#root n15 ----
ppwf.n15root.datestat <- lmer(X15N.root ~ month + (1|site), data = pp.wf)
visreg(ppwf.n15root.datestat, xvar = "month",whitespace=0.4, las=1, line = list(col=FALSE),
       ylab="Root δ 15N", main= "pp", xlab="Sampling Date", overlay= TRUE)
summary(ppwf.n15root.datestat)
plot(ppwf.n15root.datestat)
shapiro.test(residuals(ppwf.n15root.datestat))
##comparing m vs nm months
ppwf.n15root.julstat <- lmer(X15N.root ~ status + (1|site), data = ppwfjul)
visreg(ppwf.n15root.julstat, xvar = "status")
anova(ppwf.n15root.julstat)
plot(ppwf.n15root.julstat)
shapiro.test(residuals(ppwf.n15root.julstat))

ppwf.n15root.augstat <- lmer(X15N.root ~ status + (1|site), data = ppwfaug)
visreg(ppwf.n15root.augstat, xvar = "status")
anova(ppwf.n15root.augstat)
plot(ppwf.n15root.augstat)
shapiro.test(residuals(ppwf.n15root.augstat))


lpiwf.n15root.datestat <- lmer(X15N.root ~ month + (1|site), data = lpi.wf)
visreg(lpiwf.n15root.datestat, xvar = "month",whitespace=0.4, las=1, line = list(col=FALSE),  
       ylab="Root δ 15N", main= "lpi wildfire", xlab="Sampling Date", points=list(cex=0.75), overlay= TRUE)
summary(lpiwf.n15root.datestat)
plot(lpiwf.n15root.datestat)
shapiro.test(residuals(lpiwf.n15root.datestat))
##comparing m vs nm months
lpiwf.n15root.julstat <- lmer(X15N.root ~ status + (1|site), data = lpiwfjul)
visreg(lpiwf.n15root.julstat, xvar = "status")
anova(lpiwf.n15root.julstat)
plot(lpiwf.n15root.julstat)
shapiro.test(residuals(lpiwf.n15root.julstat))

lpiwf.n15root.augstat <- lmer(X15N.root ~ status + (1|site), data = lpiwfaug)
visreg(lpiwf.n15root.augstat, xvar = "status")
anova(lpiwf.n15root.augstat)
plot(lpiwf.n15root.augstat)
shapiro.test(residuals(lpiwf.n15root.augstat))


lpicc.n15root.datestat <- lmer(X15N.root ~ month  + (1|site), data = lpi.cc)
visreg(lpicc.n15root.datestat, xvar = "month",whitespace=0.4, las=1, line = list(col=TRUE),
       ylab="Root δ 15N", main= "lpi cc", xlab="Sampling Date", overlay= TRUE)
summary(lpicc.n15root.datestat)
anova(lpicc.n15root.datestat)
plot(lpicc.n15root.datestat)
shapiro.test(residuals(lpicc.n15root.datestat))

##comparing m vs nm months
lpicc.n15root.julstat <-lmer(X15N.root ~ status + (1|site), data = lpiccjul)
plot(lpicc.n15root.julstat)
visreg(lpicc.n15root.julstat, xvar = "status")
anova(lpicc.n15root.julstat)
plot(lpicc.n15root.julstat)
shapiro.test(residuals(lpicc.n15root.julstat))

lpicc.n15root.augstat <- lmer(X15N.root ~ status + (1|site), data = lpiccaug)
plot(lpicc.n15root.augstat)
visreg(lpicc.n15root.augstat, xvar = "status")
anova(lpicc.n15root.augstat)
plot(lpicc.n15root.augstat)
shapiro.test(residuals(lpicc.n15root.augstat))

#shoot c13 ----
ppwf.c13shoot.datestat <- lmer(X13C.shoot ~ month + (1|site), data = pp.wf)
visreg(ppwf.c13shoot.datestat, xvar = "month",whitespace=0.4, las=1, line = list(col=FALSE),
       ylab="Shoot δ 13C", main= "pp", xlab="Sampling Date", overlay= TRUE)
summary(ppwf.c13shoot.datestat)
plot(ppwf.c13shoot.datestat)
shapiro.test(residuals(ppwf.c13shoot.datestat))
##comparing m vs nm months
ppwf.c13shoot.julstat <- lmer(X13C.shoot ~ status + (1|site), data = ppwfjul)
visreg(ppwf.c13shoot.julstat, xvar = "status")
anova(ppwf.c13shoot.julstat)
plot(ppwf.c13shoot.julstat)
shapiro.test(residuals(ppwf.c13shoot.julstat))

ppwf.c13shoot.augstat <- lmer(X13C.shoot ~ status + (1|site), data = ppwfaug)
visreg(ppwf.c13shoot.augstat, xvar = "status")
anova(ppwf.c13shoot.augstat)
plot(ppwf.c13shoot.augstat)
shapiro.test(residuals(ppwf.c13shoot.augstat))

lpiwf.c13shoot.datestat <- lmer(X13C.shoot ~ month + (1|site), data = lpi.wf)
visreg(lpiwf.c13shoot.datestat, xvar = "month",whitespace=0.4, las=1, 
       ylab="Shoot δ 13C", main= "lpi wildfire", xlab="Sampling Date", overlay= TRUE)
summary(lpiwf.c13shoot.datestat)
plot(lpiwf.c13shoot.datestat)
shapiro.test(residuals(lpiwf.c13shoot.datestat))
##comparing m vs nm months**
lpiwf.c13shoot.julstat <-lmer(X13C.shoot ~ status + (1|site), data = lpiwfjul)
visreg(lpiwf.c13shoot.julstat, xvar = "status")
anova(lpiwf.c13shoot.julstat)
plot(lpiwf.c13shoot.julstat)
shapiro.test(residuals(lpiwf.c13shoot.julstat))

lpiwf.c13shoot.augstat <- lmer(X13C.shoot ~ status + (1|site), data = lpiwfaug)
visreg(lpiwf.c13shoot.augstat, xvar = "status")
anova(lpiwf.c13shoot.augstat)
plot(lpiwf.c13shoot.augstat)
shapiro.test(residuals(lpiwf.c13shoot.augstat))


lpicc.c13shoot.datestat <- lmer(X13C.shoot ~ month  + (1|site), data = lpi.cc)
visreg(lpicc.c13shoot.datestat, xvar = "month",whitespace=0.4, las=1, line = list(col=FALSE),
       ylab="Shoot δ 13C", main= "lpi cc", xlab="Sampling Date", overlay= TRUE)
summary(lpicc.c13shoot.datestat)
plot(lpicc.c13shoot.datestat)
shapiro.test(residuals(lpicc.c13shoot.datestat))
##comparing m vs nm months
lpicc.c13shoot.julstat <-lmer(X13C.shoot ~ status + (1|site), data = lpiccjul)
plot(lpicc.c13shoot.julstat)
visreg(lpicc.c13shoot.julstat, xvar = "status")
shapiro.test(residuals(lpicc.c13shoot.julstat))
anova(lpicc.c13shoot.julstat)

lpicc.c13shoot.augstat <- lmer(X13C.shoot ~ status + (1|site), data = lpiccaug)
visreg(lpicc.c13shoot.augstat, xvar = "status")
anova(lpicc.c13shoot.augstat)
plot(lpicc.c13shoot.augstat)
shapiro.test(residuals(lpicc.c13shoot.augstat))


#root c13-----
ppwf.c13root.datestat <- lmer(X13C.root ~ month  + (1|site), data = pp.wf)
visreg(ppwf.c13root.datestat, xvar = "month",whitespace=0.4, las=1, line = list(col=FALSE),
       ylab="Root δ 13C", main= "pp", xlab="Sampling Date", overlay= TRUE)
summary(ppwf.c13root.datestat)
plot(ppwf.c13root.datestat)
shapiro.test(residuals(ppwf.c13root.datestat))
##comparing m vs nm months
ppwf.c13root.julstat <- lmer(X13C.root ~ status + (1|site), data = ppwfjul)
visreg(ppwf.c13root.julstat, xvar = "status")
anova(ppwf.c13root.julstat)
plot(ppwf.c13root.julstat)
shapiro.test(residuals(ppwf.c13root.julstat))

ppwf.c13root.augstat <- lmer(X13C.root ~ status + (1|site), data = ppwfaug)
visreg(ppwf.c13root.augstat, xvar = "status")
anova(ppwf.c13root.augstat)
plot(ppwf.c13root.augstat)
shapiro.test(residuals(ppwf.c13root.augstat))
lpiwf.c13root.datestat <- lmer(X13C.root ~ month + (1|site), data = lpi.wf)
plot(lpiwf.c13root.datestat)
shapiro.test(residuals(lpiwf.c13root.datestat))


lpiwf.c13root.datestat <- lmer(X13C.root ~ month + (1|site), data = lpi.wf)
visreg(lpiwf.c13root.datestat, xvar = "month",whitespace=0.4, las=1, 
       ylab="Root δ 13C", main= "lpi wildfire", xlab="Sampling Date", overlay= TRUE)
summary(lpiwf.c13root.datestat)
####comparing m vs nm months*
lpiwf.c13root.julstat <-lmer(X13C.root ~ status + (1|site), data = lpiwfjul)
visreg(lpiwf.c13root.julstat, xvar = "status")
anova(lpiwf.c13root.julstat)
plot(lpiwf.c13root.julstat)
shapiro.test(residuals(lpiwf.c13root.julstat))

lpiwf.c13root.augstat <- lmer(X13C.root ~ status + (1|site), data = lpiwfaug)
visreg(lpiwf.c13root.augstat, xvar = "status")
anova(lpiwf.c13root.augstat)
plot(lpiwf.c13root.augstat)
shapiro.test(residuals(lpiwf.c13root.augstat))



ppwf.c13root.datestat <- lmer(X13C.root ~ month  + (1|site), data = pp.wf)
visreg(ppwf.c13root.datestat, xvar = "month",whitespace=0.4, las=1, line = list(col=FALSE),
       ylab="Root δ 13C", main= "pp", xlab="Sampling Date", overlay= TRUE)
summary(ppwf.c13root.datestat)
plot(ppwf.c13root.datestat)
shapiro.test(residuals(ppwf.c13root.datestat))
##comparing m vs nm months
ppwf.c13root.julstat <- lmer(X13C.root ~ status + (1|site), data = ppwfjul)
visreg(ppwf.c13root.julstat, xvar = "status")
anova(ppwf.c13root.julstat)
plot(ppwf.c13root.julstat)
shapiro.test(residuals(ppwf.c13root.julstat))

ppwf.c13root.augstat <- lmer(X13C.root ~ status + (1|site), data = ppwfaug)
visreg(ppwf.c13root.augstat, xvar = "status")
anova(ppwf.c13root.augstat)
plot(ppwf.c13root.augstat)
shapiro.test(residuals(ppwf.c13root.augstat))


#shoot vs root ----

test <- t.test(X15N.root , X15N.shoot, data = ppwfaug, paired = TRUE)
t.test(X15N.shoot , X15N.root, data = ppwfaug, paired = TRUE)
t.test(X15N.root, X15N.shoot, paired = TRUE, alternative = "two.sided")


ppwf.c13root.augstat <- lmer(X13C.root ~ status + (1|site), data = ppwfaug)
visreg(ppwf.c13root.augstat, xvar = "status")
anova(ppwf.c13root.augstat)
plot(ppwf.c13root.augstat)
shapiro.test(residuals(ppwf.c13root.augstat))
lpiwf.c13root.datestat <- lmer(X13C.root ~ month + (1|site), data = lpi.wf)
plot(lpiwf.c13root.datestat)
shapiro.test(residuals(lpiwf.c13root.datestat))
#example of qqnorm and boxplot----
qqnorm(lpi.wf$X13C.shoot, pch = 1, frame = FALSE)
qqline(lpi.wf$X13C.shoot, col = "steelblue", lwd = 2)
lpiwf.c13shoot.datestat <- lmer(X13C.shoot ~ month + (1|site), data = lpi.wf)
visreg(lpiwf.c13shoot.datestat, xvar = "month",whitespace=0.4, las=1, 
       ylab="Shoot δ 13C", main= "lpi wildfire", xlab="Sampling Date", overlay= TRUE)
summary(lpiwf.c13shoot.datestat)
plot(lpiwf.c13shoot.datestat)
shapiro.test(residuals(lpiwf.c13shoot.datestat))
# remove ourtliers using z scores (more or less than +3 or -3)?
z_scores <- (lpi.wf$X13C.shoot-mean(lpi.wf$X13C.shoot))/sd(lpi.wf$X13C.shoot)
z_scores

zscores <- (lpi.wf$X13C.shoot-mean(lpi.wf$X13C.shoot))/sd(lpi.wf$X13C.shoot)
zscores

boxplot(lpi.wf$X13C.shoot)
boxplot(lpi.wf$X13C.shoot, plot=FALSE)$out


#adjust p values ---- use fdr bc false negative of bonferonni is to high----


pval<-c(0.02516,0.3565,0.9343,0.3918)#Ponderosa pine wildfire July
p.adjust (pval, method="bonferroni")
p.adjust (pval, method="fdr") 

pval<-c(0.1818,0.1587,0.9671,0.008143)#Ponderosa pine wildfire August
p.adjust (pval, method="bonferroni")
p.adjust (pval, method="fdr") 

pval<-c(0.3014,	0.1212,	0.04213, 0.7531)#Lodgepole pine wildfire July 
p.adjust (pval, method="bonferroni")
p.adjust (pval, method="fdr")

pval<-c(0.0297, 0.0796, 0.0569, 0.1287)#Lodgepole pine wildfire August 
p.adjust (pval, method="bonferroni")
p.adjust (pval, method="fdr")


pval<-c(0.0148 , 0.0001611, 0.7439,0.04886 )#Lodgepole pine clearcut July 
p.adjust (pval, method="bonferroni")
p.adjust (pval, method="fdr")

pval<-c(0.001178, 0.009398, 0.2669, 0.4241)#Lodgepole pine clearcut August  
p.adjust (pval, method="bonferroni")
p.adjust (pval, method="fdr")

#adjust p values WITH % N ADDED---- use fdr still----


pval<-c(0.02516,0.3565,0.9343,0.3918, 0.04142)#Ponderosa pine wildfire July
p.adjust (pval, method="bonferroni")
p.adjust (pval, method="fdr") 

pval<-c(0.1818,0.1587,0.9671,0.008143, 0.136)#Ponderosa pine wildfire August
p.adjust (pval, method="bonferroni")
p.adjust (pval, method="fdr") 

pval<-c(0.3014,	0.1212,	0.04213, 0.7531, 0.6299)#Lodgepole pine wildfire July 
p.adjust (pval, method="bonferroni")
p.adjust (pval, method="fdr")

pval<-c(0.0297, 0.0796, 0.0569, 0.1287, 0.8276)#Lodgepole pine wildfire August 
p.adjust (pval, method="bonferroni")
p.adjust (pval, method="fdr")


pval<-c(0.0148 , 0.0001611, 0.7439,0.04886, 0.5583)#Lodgepole pine clearcut July 
p.adjust (pval, method="bonferroni")
p.adjust (pval, method="fdr")

pval<-c(0.001178, 0.009398, 0.2669, 0.4241, 0.06281)#Lodgepole pine clearcut August  
p.adjust (pval, method="bonferroni")
p.adjust (pval, method="fdr")

