###Analysis script
###VDEM negative reform for ebal
###July 2019
rm(list = setdiff(ls(), lsf.str())) #Remove all except functions


library(lme4)
library(lmtest)
library(sandwich)
library(MASS)
library(ggplot2)
library(interplot)
library(MatchIt)
library(MatchingFrontier)
library(Zelig)

#####Loading dataset####
vdem.nodems <- read.csv("./vdem-2018-no-dems-post1945-polity-sept2018-condensed.csv")
vdem.nodems$comp_by_oppaut <- scale(vdem.nodems$e_van_comp.lag * vdem.nodems$oppaut.lag)
vdem.nodems$ss.diff.lag <- vdem.nodems$lowchamb.seatshare.largest.lag - vdem.nodems$lowchamb.second.seatshare.lag

vdem.nodems$polcomp.lag2 <- vdem.nodems$polcomp.lag
vdem.nodems$polcomp.lag2[vdem.nodems$polcomp.lag == -77] <- 0
vdem.nodems$polcomp.lag2[vdem.nodems$polcomp.lag == -66 | vdem.nodems$polcomp.lag == -88] <- NA

vdem.nodems$parreg2.lag <- vdem.nodems$parreg.lag
vdem.nodems$parreg2.lag[vdem.nodems$parreg.lag == -77] <- 0
vdem.nodems$parreg2.lag[vdem.nodems$parreg.lag == -66 | vdem.nodems$parreg.lag == -88] <- NA

vdem.nodems$parcomp2.lag <- vdem.nodems$parcomp.lag
vdem.nodems$parcomp2.lag[vdem.nodems$parcomp.lag == -77] <- 0
vdem.nodems$parcomp2.lag[vdem.nodems$parcomp.lag == -66 | vdem.nodems$parcomp.lag == -88] <- NA

vdem.nodems$regtrans2.lag <- vdem.nodems$regtrans.lag
vdem.nodems$regtrans2.lag[vdem.nodems$regtrans.lag < -2 | vdem.nodems$regtrans.lag > 3] <- NA

vdem.nodems$jureform.lag <- as.factor(vdem.nodems$jureform.lag)
vdem.nodems$jureform.lag <- relevel(vdem.nodems$jureform.lag, ref= "1")



#####
#Entropy balancing

###Entropy balancing using dejure.jind.negshockmajor as treatment (i.e. and increase in JI)
library(ebal)

myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "dejure.jind.negshockmajor", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "ss.diff.lag", "years.since.election")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)
dataset.matching.complete$ss.diff.lag.inv <- 100 - dataset.matching.complete$ss.diff.lag


myvars <- c("democracy.duration.lag", "oppaut.lag",  "ss.diff.lag.inv", "years.since.election",
            "polity2.lag", "loggpdpc.lag", "e_miurbani",
            "hc.ind.2lag",  "lc.ind.2lag", "transitional", "exec.respectcon.lag", "altinfo.lag", "e_peaveduc",
            "leg.constraints.lag", "COWcode", "year")  #"lji.lag"
ebal.covariates <- dataset.matching.complete[myvars]
#ebal.covariates.var <- ebal.covariates

ebal.test <- ebalance(Treatment=dataset.matching.complete$dejure.jind.negshockmajor, X = ebal.covariates[c(-15, -16)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==0,],2,mean)
library(stargazer)
stargazer(cbind(t.means, c.means.bal, c.means), type="html", 
          out="C:/Users/Cole/Dropbox/Judicial independence project/ebal adjusted means full data.html")



dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$dejure.jind.negshockmajor == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$dejure.jind.negshockmajor == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
elirreg.negshockmajor.ssdiff.base <- lmer(v2elirreg.inv~dejure.jind.negshockmajor +
                                            ss.diff.lag.inv  + elexec + 
                                            #e_polity2 +
                                            e_migdppcln + 
                                            #e_miurbani + 
                                            v2eldommon + 
                                            log(e_mipopula) + (1 | COWcode), REML=FALSE 
                                          #+ transitional + altinfo.lag
                                          , weights=ebal.test.w,
                                          data = dataset.matching.complete.w)
summary(elirreg.negshockmajor.ssdiff.base)


mm.elirreg.negshockmajor.ssdiff.all <- lmer(v2elirreg.inv~dejure.jind.negshockmajor +
                                              ss.diff.lag.inv + dejure.jind.negshockmajor*ss.diff.lag.inv   + elexec + 
                                              # e_polity2 +
                                              e_migdppcln + 
                                              #e_miurbani + 
                                              v2eldommon + 
                                              log(e_mipopula) #+ transitional + altinfo.lag 
                                            + (1 | COWcode), REML=FALSE, 
                                            weights=ebal.test.w,
                                            data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.ssdiff.all)
p2 <- interplot(mm.elirreg.negshockmajor.ssdiff.all, var1="dejure.jind.negshockmajor", var2="ss.diff.lag.inv",
                hist=TRUE) + theme_bw() + labs(x="Prior election seat-share margin (inverse)", y="Marginal effect" , 
                                               title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2)   #Negative reform increases fraud in low comp conditions


png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/ebal seat share irreg.png", height=5,
    width=7, units="in", res=300)
p2
dev.off()

lrtest(elirreg.negshockmajor.ssdiff.base, mm.elirreg.negshockmajor.ssdiff.all)

summary(dataset.matching.complete.w$ss.diff.lag.inv)  
ss1 <- .65*0 + .0041*33.4 - .0064*33.4*0
ss2 <- .65*1 + .0041*33.4 - .0064*33.4*1
ss2-ss1
(ss2 - ss1) / 5.24

ss2 <- .65*1 + .0041*33.4 - .0064*33.4*1
ss3 <- .65*1 + .0041*87.8 - .0064*87.8*1  #DV range = 2.86 + 2.38 = 5.24
(ss2-ss3) / 5.24 


###

elintim.negshockmajor.ssdiff.base <- lmer(v2elintim.inv~dejure.jind.negshockmajor +
                                            ss.diff.lag.inv  + elexec + 
                                            #e_polity2 +
                                            e_migdppcln + 
                                            #e_miurbani + 
                                            v2eldommon + 
                                            log(e_mipopula) + (1 | COWcode), REML=FALSE 
                                          #+ transitional + altinfo.lag
                                          , weights=ebal.test.w,
                                          data = dataset.matching.complete.w)
summary(elintim.negshockmajor.ssdiff.base)


mm.elintim.negshockmajor.ssdiff.all <- lmer(v2elintim.inv~dejure.jind.negshockmajor +
                                              ss.diff.lag.inv + dejure.jind.negshockmajor*ss.diff.lag.inv   + elexec + 
                                              # e_polity2 +
                                              e_migdppcln + 
                                              #e_miurbani + 
                                              v2eldommon + 
                                              log(e_mipopula) #+ transitional + altinfo.lag 
                                            + (1 | COWcode), REML=FALSE, 
                                            weights=ebal.test.w,
                                            data = dataset.matching.complete.w)
summary(mm.elintim.negshockmajor.ssdiff.all)
p2.intim <- interplot(mm.elintim.negshockmajor.ssdiff.all, var1="dejure.jind.negshockmajor", var2="ss.diff.lag.inv",
                      hist=TRUE) + theme_bw() + labs(x="Prior election seat-share margin (inverse)", y="Marginal effect" , 
                                                     title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2)  #Boost in JI reduces intimidation in less comp areas


png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/ebal seat share intim.png", height=5,
    width=7, units="in", res=300)
p2.intim  #Same as above
dev.off()

lrtest(elintim.negshockmajor.ssdiff.base, mm.elintim.negshockmajor.ssdiff.all)



##Core civil society


myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "dejure.jind.negshockmajor", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "core.civil.society.lag")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)


myvars <- c("democracy.duration.lag", "oppaut.lag",  "core.civil.society.lag", 
            "polity2.lag", "loggpdpc.lag", "e_miurbani",
            "hc.ind.2lag",  "lc.ind.2lag", "transitional", "exec.respectcon.lag", "altinfo.lag", "e_peaveduc",
            "leg.constraints.lag", "COWcode", "year")  #"lji.lag"
ebal.covariates <- dataset.matching.complete[myvars]
#ebal.covariates.var <- ebal.covariates

ebal.test <- ebalance(Treatment=dataset.matching.complete$dejure.jind.negshockmajor, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$dejure.jind.negshockmajor == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$dejure.jind.negshockmajor == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
elirreg.negshockmajor.civil.base <- lmer(v2elirreg.inv~dejure.jind.negshockmajor +
                                           core.civil.society.lag + elexec + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula)  + (1 | COWcode), REML=FALSE 
                                         #+ transitional + altinfo.lag
                                         , weights=ebal.test.w,
                                         data = dataset.matching.complete.w)
summary(elirreg.negshockmajor.civil.base)


mm.elirreg.negshockmajor.civil.all <- lmer(v2elirreg.inv~dejure.jind.negshockmajor +
                                             core.civil.society.lag + dejure.jind.negshockmajor*core.civil.society.lag   + elexec + 
                                             # e_polity2 +
                                             e_migdppcln + 
                                             #e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) #+ transitional + altinfo.lag 
                                           + (1 | COWcode), REML=FALSE, 
                                           weights=ebal.test.w,
                                           data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.civil.all)
p3 <- interplot(mm.elirreg.negshockmajor.civil.all, var1="dejure.jind.negshockmajor", var2="core.civil.society.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged civil society index", y="Marginal effect" , 
                                               title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) #In low CS openness areas, JI boost reduces fraud


png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/ebal core civil society irreg.png", height=5,
    width=7, units="in", res=300)
p3
dev.off()

lrtest(elirreg.negshockmajor.civil.base, mm.elirreg.negshockmajor.civil.all)


###

elintim.negshockmajor.civil.base <- lmer(v2elintim.inv~dejure.jind.negshockmajor +
                                           core.civil.society.lag + elexec + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula)  + (1 | COWcode), REML=FALSE 
                                         #+ transitional + altinfo.lag
                                         , weights=ebal.test.w,
                                         data = dataset.matching.complete.w)
summary(elintim.negshockmajor.civil.base)


mm.elintim.negshockmajor.civil.all <- lmer(v2elintim.inv~dejure.jind.negshockmajor +
                                             core.civil.society.lag + dejure.jind.negshockmajor*core.civil.society.lag   + elexec + 
                                             # e_polity2 +
                                             e_migdppcln + 
                                             #e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) #+ transitional + altinfo.lag 
                                           + (1 | COWcode), REML=FALSE, 
                                           weights=ebal.test.w,
                                           data = dataset.matching.complete.w)
summary(mm.elintim.negshockmajor.civil.all)
p3.intim <- interplot(mm.elintim.negshockmajor.civil.all, var1="dejure.jind.negshockmajor", var2="core.civil.society.lag",
                      hist=TRUE) + theme_bw() + labs(x="Lagged civil society index", y="Marginal effect" , 
                                                     title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) #Same as above


png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/ebal core civil society intim.png", height=5,
    width=7, units="in", res=300)
p3.intim
dev.off()

lrtest(elintim.negshockmajor.civil.base, mm.elintim.negshockmajor.civil.all)




### Legislative opposition oversight
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "dejure.jind.negshockmajor", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "opposition.oversight.lag")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)


myvars <- c("democracy.duration.lag", "oppaut.lag",  "opposition.oversight.lag", 
            "polity2.lag", "loggpdpc.lag", "e_miurbani",
            "hc.ind.2lag",  "lc.ind.2lag", "transitional", "exec.respectcon.lag", "altinfo.lag", "e_peaveduc",
            "leg.constraints.lag", "COWcode", "year")  #"lji.lag"
ebal.covariates <- dataset.matching.complete[myvars]
#ebal.covariates.var <- ebal.covariates

ebal.test <- ebalance(Treatment=dataset.matching.complete$dejure.jind.negshockmajor, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$dejure.jind.negshockmajor == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$dejure.jind.negshockmajor == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
elirreg.negshockmajor.oversight.base <- lmer(v2elirreg.inv~dejure.jind.negshockmajor +
                                               opposition.oversight.lag + elexec + 
                                               #e_polity2 +
                                               e_migdppcln + 
                                               #e_miurbani + 
                                               v2eldommon + 
                                               log(e_mipopula)  + (1 | COWcode), REML=FALSE
                                             #+ transitional + altinfo.lag
                                             , weights=ebal.test.w,
                                             data = dataset.matching.complete.w)
summary(elirreg.negshockmajor.oversight.base)


mm.elirreg.negshockmajor.oversight <- lmer(v2elirreg.inv~dejure.jind.negshockmajor +
                                             opposition.oversight.lag + dejure.jind.negshockmajor*opposition.oversight.lag   + elexec + 
                                             # e_polity2 +
                                             e_migdppcln + 
                                             #e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) #+ transitional + altinfo.lag 
                                           + (1 | COWcode), REML=FALSE, 
                                           weights=ebal.test.w,
                                           data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.oversight)
p4 <- interplot(mm.elirreg.negshockmajor.oversight, var1="dejure.jind.negshockmajor", var2="opposition.oversight.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged opposition oversight", y="Marginal effect" , 
                                               title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2)   #When opp oversight is low, a positive reform reduces fraud

png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/ebal oversight.png", height=5,
    width=7, units="in", res=300)
p4
dev.off()

lrtest(elirreg.negshockmajor.oversight.base, mm.elirreg.negshockmajor.oversight)

###

elintim.negshockmajor.oversight.base <- lmer(v2elintim.inv~dejure.jind.negshockmajor +
                                               opposition.oversight.lag + elexec + 
                                               #e_polity2 +
                                               e_migdppcln + 
                                               #e_miurbani + 
                                               v2eldommon + 
                                               log(e_mipopula)  + (1 | COWcode), REML=FALSE
                                             #+ transitional + altinfo.lag
                                             , weights=ebal.test.w,
                                             data = dataset.matching.complete.w)
summary(elintim.negshockmajor.oversight.base)


mm.elintim.negshockmajor.oversight <- lmer(v2elintim.inv~dejure.jind.negshockmajor +
                                             opposition.oversight.lag + dejure.jind.negshockmajor*opposition.oversight.lag   + elexec + 
                                             # e_polity2 +
                                             e_migdppcln + 
                                             #e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) #+ transitional + altinfo.lag 
                                           + (1 | COWcode), REML=FALSE, 
                                           weights=ebal.test.w,
                                           data = dataset.matching.complete.w)
summary(mm.elintim.negshockmajor.oversight)
p4.intim <- interplot(mm.elintim.negshockmajor.oversight, var1="dejure.jind.negshockmajor", var2="opposition.oversight.lag",
                      hist=TRUE) + theme_bw() + labs(x="Lagged opposition oversight", y="Marginal effect" , 
                                                     title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2)  #Same as above

png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/ebal oversight intim.png", height=5,
    width=7, units="in", res=300)
p4.intim  
dev.off()

lrtest(elintim.negshockmajor.oversight.base, mm.elintim.negshockmajor.oversight)



###Main results table 
stargazer(elirreg.negshockmajor.ssdiff.base, mm.elirreg.negshockmajor.ssdiff.all, elirreg.negshockmajor.oversight.base, mm.elirreg.negshockmajor.oversight, elirreg.negshockmajor.civil.base,
          mm.elirreg.negshockmajor.civil.all,
          type="html", out="C:/Users/Cole/Dropbox/Judicial independence project/ebal results.html")










##polcomp


myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "dejure.jind.negshockmajor", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "polcomp.lag")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching$polcomp.lag2 <- dataset.matching$polcomp.lag
dataset.matching$polcomp.lag2[vdem.nodems$polcomp.lag == -66 |
                                vdem.nodems$polcomp.lag == -77 |
                                vdem.nodems$polcomp.lag == -88 ] <- 0
dataset.matching.complete <- na.omit(dataset.matching)




myvars <- c("democracy.duration.lag", "oppaut.lag",  "polcomp.lag2", 
            "polity2.lag", "loggpdpc.lag", "e_miurbani",
            "hc.ind.2lag",  "lc.ind.2lag", "transitional", "exec.respectcon.lag", "altinfo.lag", "e_peaveduc",
            "leg.constraints.lag", "COWcode", "year")  #"lji.lag"
ebal.covariates <- dataset.matching.complete[myvars]
#ebal.covariates.var <- ebal.covariates

ebal.test <- ebalance(Treatment=dataset.matching.complete$dejure.jind.negshockmajor, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$dejure.jind.negshockmajor == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$dejure.jind.negshockmajor == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
ols.elirreg.negshockmajor.comp.all <- lm(v2elirreg.inv~dejure.jind.negshockmajor +
                                           polcomp.lag2 + dejure.jind.negshockmajor*polcomp.lag2 + elexec + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) 
                                         #+ transitional + altinfo.lag
                                         , weights=ebal.test.w,
                                         data = dataset.matching.complete.w)
summary(ols.elirreg.negshockmajor.comp.all)
p1 <- interplot(ols.elirreg.negshockmajor.comp.all, var1="dejure.jind.negshockmajor", var2="polcomp.lag2",
                hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect") + geom_hline(yintercept=0, linetype=2) 


mm.elirreg.negshockmajor.polcomp <- lmer(v2elirreg.inv~dejure.jind.negshockmajor +
                                           polcomp.lag2 + dejure.jind.negshockmajor*polcomp.lag2   + elexec + 
                                           # e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) #+ transitional + altinfo.lag 
                                         + (1 | COWcode), REML=FALSE, 
                                         weights=ebal.test.w,
                                         data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.polcomp)
p.polcomp <- interplot(mm.elirreg.negshockmajor.polcomp, var1="dejure.jind.negshockmajor", var2="polcomp.lag2",
                       hist=TRUE) + theme_bw() + labs(x="Lagged political competition", y="Marginal effect" , 
                                                      title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) 



### State ownership
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "dejure.jind.negshockmajor", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "state.ownership.lag")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)


myvars <- c("democracy.duration.lag", "oppaut.lag",  "state.ownership.lag", 
            "polity2.lag", "loggpdpc.lag", "e_miurbani",
            "hc.ind.2lag",  "lc.ind.2lag", "transitional", "exec.respectcon.lag", "altinfo.lag", "e_peaveduc",
            "leg.constraints.lag", "COWcode", "year")  #"lji.lag"
ebal.covariates <- dataset.matching.complete[myvars]
#ebal.covariates.var <- ebal.covariates

ebal.test <- ebalance(Treatment=dataset.matching.complete$dejure.jind.negshockmajor, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$dejure.jind.negshockmajor == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$dejure.jind.negshockmajor == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
ols.elirreg.negshockmajor.comp.all <- lm(v2elirreg.inv~dejure.jind.negshockmajor +
                                           state.ownership.lag + dejure.jind.negshockmajor*state.ownership.lag + elexec + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) 
                                         #+ transitional + altinfo.lag
                                         , weights=ebal.test.w,
                                         data = dataset.matching.complete.w)
summary(ols.elirreg.negshockmajor.comp.all)
p1 <- interplot(ols.elirreg.negshockmajor.comp.all, var1="dejure.jind.negshockmajor", var2="state.ownership.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect") + geom_hline(yintercept=0, linetype=2) 


mm.elirreg.negshockmajor.stateown <- lmer(v2elirreg.inv~dejure.jind.negshockmajor +
                                            state.ownership.lag + dejure.jind.negshockmajor*state.ownership.lag   + elexec + 
                                            # e_polity2 +
                                            e_migdppcln + 
                                            #e_miurbani + 
                                            v2eldommon + 
                                            log(e_mipopula) #+ transitional + altinfo.lag 
                                          + (1 | COWcode), REML=FALSE, 
                                          weights=ebal.test.w,
                                          data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.stateown)
p.stateowned <- interplot(mm.elirreg.negshockmajor.stateown, var1="dejure.jind.negshockmajor", var2="state.ownership.lag",
                          hist=TRUE) + theme_bw() + labs(x="State ownership of the economy (lagged)", y="Marginal effect" , 
                                                         title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) 


###daigonal accountability
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "dejure.jind.negshockmajor", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "diagacc.lag")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)


myvars <- c("democracy.duration.lag", "oppaut.lag",  "diagacc.lag", 
            "polity2.lag", "loggpdpc.lag", "e_miurbani",
            "hc.ind.2lag",  "lc.ind.2lag", "transitional", "exec.respectcon.lag", "altinfo.lag", "e_peaveduc",
            "leg.constraints.lag", "COWcode", "year")  #"lji.lag"
ebal.covariates <- dataset.matching.complete[myvars]
#ebal.covariates.var <- ebal.covariates

ebal.test <- ebalance(Treatment=dataset.matching.complete$dejure.jind.negshockmajor, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$dejure.jind.negshockmajor == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$dejure.jind.negshockmajor == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
ols.elirreg.negshockmajor.comp.all <- lm(v2elirreg.inv~dejure.jind.negshockmajor +
                                           diagacc.lag + dejure.jind.negshockmajor*diagacc.lag + elexec + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) 
                                         #+ transitional + altinfo.lag
                                         , weights=ebal.test.w,
                                         data = dataset.matching.complete.w)
summary(ols.elirreg.negshockmajor.comp.all)
p1 <- interplot(ols.elirreg.negshockmajor.comp.all, var1="dejure.jind.negshockmajor", var2="diagacc.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect") + geom_hline(yintercept=0, linetype=2) 


mm.elirreg.negshockmajor.diag <- lmer(v2elirreg.inv~dejure.jind.negshockmajor +
                                        diagacc.lag + dejure.jind.negshockmajor*diagacc.lag   + elexec + 
                                        # e_polity2 +
                                        e_migdppcln + 
                                        #e_miurbani + 
                                        v2eldommon + 
                                        log(e_mipopula) #+ transitional + altinfo.lag 
                                      + (1 | COWcode), REML=FALSE, 
                                      weights=ebal.test.w,
                                      data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.diag)
p.diag <- interplot(mm.elirreg.negshockmajor.diag, var1="dejure.jind.negshockmajor", var2="diagacc.lag",
                    hist=TRUE) + theme_bw() + labs(x="Diagonal accountability (lagged)", y="Marginal effect" , 
                                                   title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) 


###

mm.elintim.negshockmajor.diag <- lmer(v2elintim.inv~dejure.jind.negshockmajor +
                                        diagacc.lag + dejure.jind.negshockmajor*diagacc.lag   + elexec + 
                                        # e_polity2 +
                                        e_migdppcln + 
                                        #e_miurbani + 
                                        v2eldommon + 
                                        log(e_mipopula) #+ transitional + altinfo.lag 
                                      + (1 | COWcode), REML=FALSE, 
                                      weights=ebal.test.w,
                                      data = dataset.matching.complete.w)
summary(mm.elintim.negshockmajor.diag)
p.diag.intim <- interplot(mm.elintim.negshockmajor.diag, var1="dejure.jind.negshockmajor", var2="diagacc.lag",
                          hist=TRUE) + theme_bw() + labs(x="Diagonal accountability (lagged)", y="Marginal effect" , 
                                                         title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) 




###parreg2

myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "dejure.jind.negshockmajor", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "parreg2.lag")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)


myvars <- c("democracy.duration.lag", "oppaut.lag",  "parreg2.lag", 
            "polity2.lag", "loggpdpc.lag", "e_miurbani",
            "hc.ind.2lag",  "lc.ind.2lag", "transitional", "exec.respectcon.lag", "altinfo.lag", "e_peaveduc",
            "leg.constraints.lag", "COWcode", "year")  #"lji.lag"
ebal.covariates <- dataset.matching.complete[myvars]
#ebal.covariates.var <- ebal.covariates

ebal.test <- ebalance(Treatment=dataset.matching.complete$dejure.jind.negshockmajor, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$dejure.jind.negshockmajor == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$dejure.jind.negshockmajor == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
ols.elirreg.negshockmajor.comp.all <- lm(v2elirreg.inv~dejure.jind.negshockmajor +
                                           parreg2.lag + dejure.jind.negshockmajor*parreg2.lag + elexec + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) 
                                         #+ transitional + altinfo.lag
                                         , weights=ebal.test.w,
                                         data = dataset.matching.complete.w)
summary(ols.elirreg.negshockmajor.comp.all)
p1 <- interplot(ols.elirreg.negshockmajor.comp.all, var1="dejure.jind.negshockmajor", var2="opposition.oversight.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect") + geom_hline(yintercept=0, linetype=2) 


mm.elirreg.negshockmajor.comp.all <- lmer(v2elirreg.inv~dejure.jind.negshockmajor +
                                            parreg2.lag + dejure.jind.negshockmajor*parreg2.lag   + elexec + 
                                            # e_polity2 +
                                            e_migdppcln + 
                                            #e_miurbani + 
                                            v2eldommon + 
                                            log(e_mipopula) #+ transitional + altinfo.lag 
                                          + (1 | COWcode), REML=FALSE, 
                                          weights=ebal.test.w,
                                          data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.comp.all)
p2 <- interplot(mm.elirreg.negshockmajor.comp.all, var1="dejure.jind.negshockmajor", var2="parreg2.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect" , 
                                               title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) 


###parcomp
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "dejure.jind.negshockmajor", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "parcomp2.lag")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)


myvars <- c("democracy.duration.lag", "oppaut.lag",  "parcomp2.lag", 
            "polity2.lag", "loggpdpc.lag", "e_miurbani",
            "hc.ind.2lag",  "lc.ind.2lag", "transitional", "exec.respectcon.lag", "altinfo.lag", "e_peaveduc",
            "leg.constraints.lag", "COWcode", "year")  #"lji.lag"
ebal.covariates <- dataset.matching.complete[myvars]
#ebal.covariates.var <- ebal.covariates

ebal.test <- ebalance(Treatment=dataset.matching.complete$dejure.jind.negshockmajor, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$dejure.jind.negshockmajor==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$dejure.jind.negshockmajor == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$dejure.jind.negshockmajor == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
ols.elirreg.negshockmajor.comp.all <- lm(v2elirreg.inv~dejure.jind.negshockmajor +
                                           parcomp2.lag + dejure.jind.negshockmajor*parcomp2.lag + elexec + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) 
                                         #+ transitional + altinfo.lag
                                         , weights=ebal.test.w,
                                         data = dataset.matching.complete.w)
summary(ols.elirreg.negshockmajor.comp.all)
p1 <- interplot(ols.elirreg.negshockmajor.comp.all, var1="dejure.jind.negshockmajor", var2="opposition.oversight.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect") + geom_hline(yintercept=0, linetype=2) 


mm.elirreg.negshockmajor.comp.all <- lmer(v2elirreg.inv~dejure.jind.negshockmajor +
                                            parcomp2.lag + dejure.jind.negshockmajor*parcomp2.lag   + elexec + 
                                            # e_polity2 +
                                            e_migdppcln + 
                                            #e_miurbani + 
                                            v2eldommon + 
                                            log(e_mipopula) #+ transitional + altinfo.lag 
                                          + (1 | COWcode), REML=FALSE, 
                                          weights=ebal.test.w,
                                          data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.comp.all)
p2 <- interplot(mm.elirreg.negshockmajor.comp.all, var1="dejure.jind.negshockmajor", var2="parcomp2.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect" , 
                                               title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) 
