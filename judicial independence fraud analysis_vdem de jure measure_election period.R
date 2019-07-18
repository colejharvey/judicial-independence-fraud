###Analysis script
###VDEM judicial reform variable (positive reform for ebal) by election period
###Variable is reform.positive.lag.electionperiod.binary
###July 2019
###Post-CPS analysis
###Save this version for robustness check if necessary
rm(list = setdiff(ls(), lsf.str())) #Remove all except functions

library(tidyverse)
library(lme4)
library(lmtest)
library(sandwich)
#library(MASS)
library(ggplot2)
library(interplot)


#####Loading dataset####
vdem.nodems <- read.csv("./vdem-2018-no-dems-post1945-polity-sept2018-condensed-tidy.csv")



#####
#Entropy balancing
library(ebal)


##polity2

myvars <- c("COWcode", "year", "transitional.cycle", "v2elirreg.inv", "v2elintim.inv", "lc.ind.cycle", 
            "hc.ind.cycle", "oppaut.cycle", "elexec", "education.cycle",  
            "polity2.cycle", "polity2.lag", "urban.cycle", "v2eldommon", "e_mipopula", 
            "country_name", "reform.positive.electionperiod.binary", "loggpdpc.lag", "loggpdpc.cycle",
            "democracy.duration.cycle", "exec.respectcon.cycle", "leg.constraints.cycle", 
            "altinfo.cycle")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)


myvars <- c("democracy.duration.cycle", "oppaut.cycle", "polity2.cycle", #"years.since.election",
             "loggpdpc.cycle", "urban.cycle",
            "hc.ind.cycle",  "lc.ind.cycle", "transitional.cycle", "exec.respectcon.cycle", "altinfo.cycle",
            "education.cycle",
            "leg.constraints.cycle")  #"lji.lag"
ebal.covariates <- dataset.matching.complete[myvars]
#ebal.covariates.var <- ebal.covariates

ebal.test <- ebalance(Treatment=dataset.matching.complete$reform.positive.electionperiod.binary, X = ebal.covariates)



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$reform.positive.electionperiod.binary==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$reform.positive.electionperiod.binary==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$reform.positive.electionperiod.binary==0,],2,mean)
library(stargazer)
stargazer(cbind(t.means, c.means.bal, c.means), type="html", 
          out="./Drafts/ebal adjusted means full data election period.html")



dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$reform.positive.electionperiod.binary == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$reform.positive.electionperiod.binary == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
elirreg.posreform.polity.base <- lmer(v2elirreg.inv~reform.positive.electionperiod.binary +
                                             elexec + 
                                        polity2.lag +
                                        loggpdpc.lag + 
                                            #e_miurbani + 
                                            v2eldommon + 
                                            log(e_mipopula) + (1 | COWcode), REML=FALSE 
                                          #+ transitional + altinfo.lag
                                          , weights=ebal.test.w,
                                          data = dataset.matching.complete.w)
summary(elirreg.posreform.polity.base)


mm.elirreg.posreform.polity.all <- lmer(v2elirreg.inv~reform.positive.electionperiod.binary +
                                               reform.positive.electionperiod.binary*polity2.lag   + elexec + 
                                          polity2.lag +
                                          loggpdpc.lag + 
                                              #e_miurbani + 
                                              v2eldommon + 
                                              log(e_mipopula) #+ transitional + altinfo.lag 
                                            + (1 | COWcode), REML=FALSE, 
                                            weights=ebal.test.w,
                                            data = dataset.matching.complete.w)
summary(mm.elirreg.posreform.polity.all)
p2 <- interplot(mm.elirreg.posreform.polity.all, var1="reform.positive.electionperiod.binary", var2="polity2.lag",
                hist=TRUE) + theme_bw() + labs(x="Polity score (1-year lag)", y="Marginal effect" , 
                                               title="Marginal effect of positive reform on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2)   #No effect for fraud


png("./Plots/ebal polity irreg election period.png", height=5,
    width=7, units="in", res=300)
p2
dev.off()

lrtest(elirreg.posreform.polity.base, mm.elirreg.posreform.polity.all)

summary(dataset.matching.complete.w$polity2.lag)  
ss1 <- -0.11*0 + -0.06*-5 + .04*-5*0
ss2 <- -0.11*1 + -0.06*-5 + .04*-5*1
ss2-ss1
(ss2 - ss1) / 5.24

ss2 <- -0.11*0 + -0.06*-5 + .04*-5*0
ss3 <- -0.11*0 + -0.06*5 + .04*5*0  #DV range = 2.86 + 2.38 = 5.24
(ss2-ss3) / 5.24 


###

elintim.posreform.polity.base <- lmer(v2elintim.inv~reform.positive.electionperiod.binary +
                                            polity2.lag  + elexec + 
                                            #e_polity2 +
                                        loggpdpc.lag + 
                                            #e_miurbani + 
                                            v2eldommon + 
                                            log(e_mipopula) + (1 | COWcode), REML=FALSE 
                                          #+ transitional + altinfo.lag
                                          , weights=ebal.test.w,
                                          data = dataset.matching.complete.w)
summary(elintim.posreform.polity.base)


mm.elintim.posreform.polity.all <- lmer(v2elintim.inv~reform.positive.electionperiod.binary +
                                              polity2.lag + reform.positive.electionperiod.binary*polity2.lag   + elexec + 
                                              # e_polity2 +
                                          loggpdpc.lag + 
                                              #e_miurbani + 
                                              v2eldommon + 
                                              log(e_mipopula) #+ transitional + altinfo.lag 
                                            + (1 | COWcode), REML=FALSE, 
                                            weights=ebal.test.w,
                                            data = dataset.matching.complete.w)
summary(mm.elintim.posreform.polity.all)
p2.intim <- interplot(mm.elintim.posreform.polity.all, var1="reform.positive.electionperiod.binary", var2="polity2.lag",
                      hist=TRUE) + theme_bw() + labs(x="Polity score", y="Marginal effect" , 
                                                     title="Marginal effect of negative JI shock on government intimidation") +
  geom_hline(yintercept=0, linetype=2)  #Boost in JI reduces intimidation in less comp areas


png("./Plots/ebal polity intim election period.png", height=5,
    width=7, units="in", res=300)
p2.intim
dev.off()

lrtest(elintim.posreform.polity.base, mm.elintim.posreform.polity.all)


##Core civil society


myvars <- c("COWcode", "year", "transitional.cycle", "v2elirreg.inv", "v2elintim.inv", "lc.ind.cycle", 
            "hc.ind.cycle", "oppaut.cycle", "elexec", "education.cycle",  
            "core.civil.society.cycle", "civil.society.lag", "urban.cycle", "v2eldommon", "e_mipopula", 
            "country_name", "reform.positive.electionperiod.binary", "loggpdpc.lag", "loggpdpc.cycle",
            "democracy.duration.cycle", "exec.respectcon.cycle", "leg.constraints.cycle", 
            "altinfo.cycle", 
            "years.since.election")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)


myvars <- c("democracy.duration.cycle", "oppaut.cycle", "core.civil.society.cycle", #"years.since.election",
            "loggpdpc.cycle", "urban.cycle",
            "hc.ind.cycle",  "lc.ind.cycle", "transitional.cycle", "exec.respectcon.cycle", "altinfo.cycle",
            "education.cycle",
            "leg.constraints.cycle", "COWcode", "year")  #"lji.lag"
ebal.covariates <- dataset.matching.complete[myvars]
#ebal.covariates.var <- ebal.covariates

ebal.test <- ebalance(Treatment=dataset.matching.complete$reform.positive.electionperiod.binary, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$reform.positive.electionperiod.binary==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$reform.positive.electionperiod.binary==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$reform.positive.electionperiod.binary==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$reform.positive.electionperiod.binary == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$reform.positive.electionperiod.binary == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
elirreg.posreform.civil.base <- lmer(v2elirreg.inv~reform.positive.electionperiod.binary +
                                           civil.society.lag + elexec + 
                                           #e_polity2 +
                                       loggpdpc.lag + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula)  + (1 | COWcode), REML=FALSE 
                                         #+ transitional + altinfo.lag
                                         , weights=ebal.test.w,
                                         data = dataset.matching.complete.w)
summary(elirreg.posreform.civil.base)


mm.elirreg.posreform.civil.all <- lmer(v2elirreg.inv~reform.positive.electionperiod.binary +
                                         civil.society.lag + reform.positive.electionperiod.binary*civil.society.lag   + elexec + 
                                             # e_polity2 +
                                         loggpdpc.lag + 
                                             #e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) #+ transitional + altinfo.lag 
                                           + (1 | COWcode), REML=FALSE, 
                                           weights=ebal.test.w,
                                           data = dataset.matching.complete.w)
summary(mm.elirreg.posreform.civil.all)
p3 <- interplot(mm.elirreg.posreform.civil.all, var1="reform.positive.electionperiod.binary", var2="civil.society.lag",
                hist=TRUE) + theme_bw() + labs(x="Civil society index (1-year lag)", y="Marginal effect" , 
                                               title="Marginal effect of positive reform on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) #In low CS openness areas, JI boost reduces fraud


png("./Plots/ebal core civil society irreg election period.png", height=5,
    width=7, units="in", res=300)
p3
dev.off()

lrtest(elirreg.posreform.civil.base, mm.elirreg.posreform.civil.all)


###

elintim.posreform.civil.base <- lmer(v2elintim.inv~reform.positive.electionperiod.binary +
                                           civil.society.lag + elexec + 
                                           #e_polity2 +
                                       loggpdpc.lag + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula)  + (1 | COWcode), REML=FALSE 
                                         #+ transitional + altinfo.lag
                                         , weights=ebal.test.w,
                                         data = dataset.matching.complete.w)
summary(elintim.posreform.civil.base)


mm.elintim.posreform.civil.all <- lmer(v2elintim.inv~reform.positive.electionperiod.binary +
                                             civil.society.lag + reform.positive.electionperiod.binary*civil.society.lag   + elexec + 
                                             # e_polity2 +
                                         loggpdpc.lag + 
                                             #e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) #+ transitional + altinfo.lag 
                                           + (1 | COWcode), REML=FALSE, 
                                           weights=ebal.test.w,
                                           data = dataset.matching.complete.w)
summary(mm.elintim.posreform.civil.all)
p3.intim <- interplot(mm.elintim.posreform.civil.all, var1="reform.positive.electionperiod.binary", var2="civil.society.lag",
                      hist=TRUE) + theme_bw() + labs(x="Civil society index (1-year lag)", y="Marginal effect" , 
                                                     title="Marginal effect of positive reform on government intimidation") +
  geom_hline(yintercept=0, linetype=2) #Same as above


png("./Plots/ebal core civil society intim election period.png", height=5,
    width=7, units="in", res=300)
p3.intim
dev.off()

lrtest(elintim.posreform.civil.base, mm.elintim.posreform.civil.all)




### Legislative opposition oversight
myvars <- c("COWcode", "year", "transitional.cycle", "v2elirreg.inv", "v2elintim.inv", "lc.ind.cycle", 
            "hc.ind.cycle", "oppaut.cycle", "elexec", "education.cycle",  
            "opposition.oversight.cycle", "opposition.oversight.lag", "urban.cycle", "v2eldommon", "e_mipopula", 
            "country_name", "reform.positive.electionperiod.binary", "loggpdpc.lag", "loggpdpc.cycle",
            "democracy.duration.cycle", "exec.respectcon.cycle", "leg.constraints.cycle", 
            "altinfo.cycle", 
            "years.since.election")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)


myvars <- c("democracy.duration.cycle", "oppaut.cycle", "opposition.oversight.cycle", #"years.since.election",
            "loggpdpc.cycle", "urban.cycle",
            "hc.ind.cycle",  "lc.ind.cycle", "transitional.cycle", "exec.respectcon.cycle", "altinfo.cycle",
            "education.cycle",
            "leg.constraints.cycle", "COWcode", "year")  #"lji.lag"
ebal.covariates <- dataset.matching.complete[myvars]
#ebal.covariates.var <- ebal.covariates

ebal.test <- ebalance(Treatment=dataset.matching.complete$reform.positive.electionperiod.binary, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$reform.positive.electionperiod.binary==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$reform.positive.electionperiod.binary==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$reform.positive.electionperiod.binary==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$reform.positive.electionperiod.binary == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$reform.positive.electionperiod.binary == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
elirreg.posreform.oversight.base <- lmer(v2elirreg.inv~reform.positive.electionperiod.binary +
                                               opposition.oversight.lag + elexec + 
                                               #e_polity2 +
                                           loggpdpc.lag + 
                                               #e_miurbani + 
                                               v2eldommon + 
                                               log(e_mipopula)  + (1 | COWcode), REML=FALSE
                                             #+ transitional + altinfo.lag
                                             , weights=ebal.test.w,
                                             data = dataset.matching.complete.w)
summary(elirreg.posreform.oversight.base)


mm.elirreg.posreform.oversight <- lmer(v2elirreg.inv~reform.positive.electionperiod.binary +
                                             opposition.oversight.lag + reform.positive.electionperiod.binary*opposition.oversight.lag   + elexec + 
                                             # e_polity2 +
                                         loggpdpc.lag + 
                                             #e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) #+ transitional + altinfo.lag 
                                           + (1 | COWcode), REML=FALSE, 
                                           weights=ebal.test.w,
                                           data = dataset.matching.complete.w)
summary(mm.elirreg.posreform.oversight)
p4 <- interplot(mm.elirreg.posreform.oversight, var1="reform.positive.electionperiod.binary", var2="opposition.oversight.lag",
                hist=TRUE) + theme_bw() + labs(x="Opposition oversight (1-year lag)", y="Marginal effect" , 
                                               title="Marginal effect of positive reform on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2)   #When opp oversight is low, a positive reform reduces fraud

png("./Plots/ebal irregularities oversight election period.png", height=5,
    width=7, units="in", res=300)
p4
dev.off()

lrtest(elirreg.posreform.oversight.base, mm.elirreg.posreform.oversight)

###

elintim.posreform.oversight.base <- lmer(v2elintim.inv~reform.positive.electionperiod.binary +
                                               opposition.oversight.lag + elexec + 
                                               #e_polity2 +
                                               e_migdppcln + 
                                               #e_miurbani + 
                                               v2eldommon + 
                                               log(e_mipopula)  + (1 | COWcode), REML=FALSE
                                             #+ transitional + altinfo.lag
                                             , weights=ebal.test.w,
                                             data = dataset.matching.complete.w)
summary(elintim.posreform.oversight.base)


mm.elintim.posreform.oversight <- lmer(v2elintim.inv~reform.positive.electionperiod.binary +
                                             opposition.oversight.lag + reform.positive.electionperiod.binary*opposition.oversight.lag   + elexec + 
                                             # e_polity2 +
                                             e_migdppcln + 
                                             #e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) #+ transitional + altinfo.lag 
                                           + (1 | COWcode), REML=FALSE, 
                                           weights=ebal.test.w,
                                           data = dataset.matching.complete.w)
summary(mm.elintim.posreform.oversight)
p4.intim <- interplot(mm.elintim.posreform.oversight, var1="reform.positive.electionperiod.binary", var2="opposition.oversight.lag",
                      hist=TRUE) + theme_bw() + labs(x="Opposition oversight (1-year lag)", y="Marginal effect" , 
                                                     title="Marginal effect of positive reform on government intimidation") +
  geom_hline(yintercept=0, linetype=2)  #Same as above

png("./Plots/ebal oversight intim election period.png", height=5,
    width=7, units="in", res=300)
p4.intim  
dev.off()

lrtest(elintim.posreform.oversight.base, mm.elintim.posreform.oversight)



###Main results table 
  ##Irregularities
library(stargazer)
stargazer(elirreg.posreform.polity.base, mm.elirreg.posreform.polity.all, elirreg.posreform.civil.base,
          mm.elirreg.posreform.civil.all, elirreg.posreform.oversight.base, mm.elirreg.posreform.oversight,
          type="html", out="./Drafts/ebal results irreg election period.html")
  ##Intimidation
stargazer(elintim.posreform.polity.base, mm.elintim.posreform.polity.all, elintim.posreform.civil.base,
          mm.elintim.posreform.civil.all, elintim.posreform.oversight.base, mm.elintim.posreform.oversight,
          type="html", out="./Drafts/ebal results intim election period.html")














###Appendix table and plots


###Alternative measures of independence: LJI and HC/LC ind from Vdem
####Adding LJI to vdem.nodems

judicial <- read.csv("./LJI-estimates-20140422.csv")
vdem.nodems$lji <- NA
vdem.nodems$lji.lag <- NA
vdem.nodems$lji.2lag <- NA
i <- 1

for(i in 1:nrow(vdem.nodems)){
  tryCatch({
    cow <- as.numeric(vdem.nodems$COW[i])
    group <- subset(judicial, judicial$ccode == cow)
    loopyear <- as.numeric(vdem.nodems$year[i])
    lag.year <- loopyear - 1
    lag.year2 <- lag.year - 1
    #if (group$year == loopyear) mergeddata$lji[i] <- group.year$LJI else mergeddata$lji <- NA
    group.year <- subset(group, group$year == loopyear)
    vdem.nodems$lji[i] <- group.year$LJI
    group.year <- subset(group, group$year == lag.year)
    vdem.nodems$lji.lag[i] <- group.year$LJI
    group.year <- subset(group, group$year == lag.year2)
    vdem.nodems$lji.2lag[i] <- group.year$LJI
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


####Plot of lji.2lag and reform.positive.electionperiod.binary
p.levels <- qplot(lji.2lag, jitter(reform.positive.electionperiod.binary), data=vdem.nodems, xlab="Latent judicial independence (lagged)",
                  ylab="Positive judicial reform (jittered)", main="Latent judicial independence prior to reform") +
  theme_bw()

png("./Plots/lji vs positive reform.png", height=5,
    width=7, units="in", res=300)
p.levels
dev.off()

####LJI by opp oversight, no balancing because not a binary treatment
mm.elirreg.lji.opp.base <- lmer(v2elirreg.inv~lji.lag +
                                  opposition.oversight.lag + elexec +
                                  e_peaveduc+ 
                                  #e_polity2 +
                                  e_migdppcln + 
                                  e_miurbani + 
                                  v2eldommon + 
                                  log(e_mipopula) 
                                + transitional
                                
                                + (1 | COWcode), REML=FALSE,
                                data = vdem.nodems)
summary(mm.elirreg.lji.opp.base)

mm.elirreg.lji.opp <- lmer(v2elirreg.inv~lji.lag +
                             opposition.oversight.lag + lji.lag*opposition.oversight.lag   + elexec +
                             e_peaveduc+ 
                             #e_polity2 +
                             e_migdppcln + 
                             e_miurbani + 
                             v2eldommon + 
                             log(e_mipopula) 
                           + transitional
                           
                           + (1 | COWcode), REML=FALSE,
                           data = vdem.nodems)
summary(mm.elirreg.lji.opp)
p.lji.opp <- interplot(mm.elirreg.lji.opp, var1="lji.lag", var2="opposition.oversight.lag", hist=TRUE) +
  labs(x="Opposition oversight (lagged)", y="Marginal effect", title="Marginal effect of latent judicial independence \non intentional voting irregularities") + 
  theme_bw() + geom_hline(yintercept=0, linetype=2)

png("./Plots/lji x oppoversight.png", height=5,
    width=7, units="in", res=300)
p.lji.opp
dev.off()

  ###hcind.lag by oppoversight
mm.elirreg.hcind.opp <- lmer(v2elirreg.inv~hc.ind.lag +
                             opposition.oversight.lag + hc.ind.lag*opposition.oversight.lag   + elexec +
                             e_peaveduc+ 
                             #e_polity2 +
                             e_migdppcln + 
                             e_miurbani + 
                             v2eldommon + 
                             log(e_mipopula) 
                           + transitional
                           
                           + (1 | COWcode), REML=FALSE,
                           data = vdem.nodems)
summary(mm.elirreg.hcind.opp)
p.hc.opp <- interplot(mm.elirreg.hcind.opp, var1="hc.ind.lag", var2="opposition.oversight.lag", hist=TRUE) +
  labs(x="Opposition oversight (lagged)", y="Marginal effect", title="Marginal effect of high-court independence \non intentional voting irregularities") + 
  theme_bw() + geom_hline(yintercept=0, linetype=2)

png("./Plots/hcind x oppoversight.png", height=5,
    width=7, units="in", res=300)
p.hc.opp
dev.off()


  ###lc.ind.lag and oppoversight

mm.elirreg.lcind.opp <- lmer(v2elirreg.inv~lc.ind.lag +
                               opposition.oversight.lag + lc.ind.lag*opposition.oversight.lag   + elexec +
                               e_peaveduc+ 
                               #e_polity2 +
                               e_migdppcln + 
                               e_miurbani + 
                               v2eldommon + 
                               log(e_mipopula) 
                             + transitional
                             
                             + (1 | COWcode), REML=FALSE,
                             data = vdem.nodems)
summary(mm.elirreg.lcind.opp)
p.lc.opp <- interplot(mm.elirreg.lcind.opp, var1="lc.ind.lag", var2="opposition.oversight.lag", hist=TRUE) +
  labs(x="Opposition oversight (lagged)", y="Marginal effect", title="Marginal effect of low-court independence \non intentional voting irregularities") + 
  theme_bw() + geom_hline(yintercept=0, linetype=2)

png("./Plots/lcind x oppoversight.png", height=5,
    width=7, units="in", res=300)
p.lc.opp
dev.off()

stargazer(mm.elirreg.lji.opp, mm.elirreg.hcind.opp, mm.elirreg.lcind.opp, type="html", 
          out="./Drafts/table alternative measures by oversight.html")

############ Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#########


png("./Plots/alternatives oversight multiplot.png", width=6, height=9, units="in", res=300)
multiplot(p.lji.opp, p.hc.opp, p.lc.opp, cols = 1)
dev.off()



####LJI by civil society, no balancing because not a binary treatment
mm.elirreg.lji.cci <- lmer(v2elirreg.inv~lji.lag +
                             core.civil.society.lag + lji.lag*core.civil.society.lag   + elexec +
                             e_peaveduc+ 
                             #e_polity2 +
                             e_migdppcln + 
                             e_miurbani + 
                             v2eldommon + 
                             log(e_mipopula) 
                           + transitional
                           
                           + (1 | COWcode), REML=FALSE,
                           data = vdem.nodems)
summary(mm.elirreg.lji.cci)
interplot(mm.elirreg.lji.cci, var1="lji.lag", var2="core.civil.society.lag", hist=TRUE)


####LJI by polity, no balancing because not a binary treatment

mm.elirreg.lji.comp <- lmer(v2elirreg.inv~lji.lag +
                              polity2.lag + lji.lag*polity2.lag   + elexec +
                              e_peaveduc+ 
                              #e_polity2 +
                              e_migdppcln + 
                              e_miurbani + 
                              v2eldommon + 
                              log(e_mipopula) 
                            + transitional
                            
                            + (1 | COWcode), REML=FALSE,
                            data = vdem.nodems)
summary(mm.elirreg.lji.comp)
interplot(mm.elirreg.lji.comp, var1="lji.lag", var2="polity2.lag", hist=TRUE)



###Un-processed data models
##Irregularities first, followed by govt intimidation


##Core civil society
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jureform.lag +  core.civil.society.lag +
                              elexec + #e_peaveduc+ 
                              e_polity2 +
                              e_migdppcln + 
                              e_miurbani + 
                              v2eldommon + 
                              log(e_mipopula) 
                            + transitional + altinfo.lag 
                            
                            + (1 | COWcode), REML=FALSE,
                            data = vdem.nodems)
summary(mm.elirreg.base.raw)

mm.elirreg.ccsi.raw <- lmer(v2elirreg.inv~jureform.lag + core.civil.society.lag + 
                              jureform.lag:core.civil.society.lag +
                              elexec + #e_peaveduc+ 
                              e_polity2 +
                              e_migdppcln + 
                              e_miurbani + 
                              v2eldommon + 
                              log(e_mipopula) 
                            + transitional + altinfo.lag
                            
                            + (1 | COWcode), REML=FALSE,
                            data = vdem.nodems)
summary(mm.elirreg.ccsi.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.ccsi.raw)
plot.raw.cc <- interplot(mm.elirreg.ccsi.raw, var1="jureform.lag", var2="core.civil.society.lag", hist=TRUE) +
  labs(x="Civil society openness", y="Marginal effect", title="Marginal effect of judicial reform \non intentional voting irregularities") + 
  theme_bw() + geom_hline(yintercept=0, linetype=2) #Unconvincing plots

png("./Plots/raw civil society irreg.png", width=7, height=5, units="in", res=300)
plot.raw.cc
dev.off()

####

mm.elintim.base.raw <- lmer(v2elintim.inv~jureform.lag + v2xcs_ccsi +
                              elexec + #e_peaveduc+ 
                              e_polity2 +
                              e_migdppcln + 
                              e_miurbani + 
                              v2eldommon + 
                              log(e_mipopula) 
                            + transitional + altinfo.lag 
                            
                            + (1 | COWcode), REML=FALSE,
                            data = vdem.nodems)
summary(mm.elintim.base.raw)

mm.elintim.ccsi.raw <- lmer(v2elintim.inv~jureform.lag + v2xcs_ccsi + 
                              jureform.lag:v2xcs_ccsi +
                              elexec + #e_peaveduc+ 
                              e_polity2 +
                              e_migdppcln + 
                              e_miurbani + 
                              v2eldommon + 
                              log(e_mipopula) 
                            + transitional + altinfo.lag
                            
                            + (1 | COWcode), REML=FALSE,
                            data = vdem.nodems)
summary(mm.elintim.ccsi.raw)

lrtest(mm.elintim.base.raw, mm.elintim.ccsi.raw)
interplot(mm.elintim.ccsi.raw, var1="jureform.lag", var2="v2xcs_ccsi") #When CC independence is low, attack on courts -> more intimidation than when no change









##Opposition oversight
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jureform.lag + opposition.oversight.lag +
                              elexec + #e_peaveduc+ 
                              e_polity2 +
                              e_migdppcln + 
                              e_miurbani + 
                              v2eldommon + 
                              log(e_mipopula) 
                            + transitional + altinfo.lag 
                            
                            + (1 | COWcode), REML=FALSE,
                            data = vdem.nodems)
summary(mm.elirreg.base.raw)

mm.elirreg.oppart.raw <- lmer(v2elirreg.inv~jureform.lag + opposition.oversight.lag + 
                                jureform.lag:opposition.oversight.lag +
                                elexec + #e_peaveduc+ 
                                e_polity2 +
                                e_migdppcln + 
                                e_miurbani + 
                                v2eldommon + 
                                log(e_mipopula) 
                              + transitional + altinfo.lag
                              
                              + (1 | COWcode), REML=FALSE,
                              data = vdem.nodems)
summary(mm.elirreg.oppart.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.oppart.raw)
plot.raw.oppover <- interplot(mm.elirreg.oppart.raw, var1="jureform.lag", var2="opposition.oversight.lag", hist=TRUE) +
  labs(x="Opposition oversight (lagged)", y="Marginal effect", title="Marginal effect of judicial reform \non intentional voting irregularities") + 
  theme_bw() + geom_hline(yintercept=0, linetype=2) #No effect for attack; boost reduces fraud when oversight is low


png("./Plots/raw oppoversity irreg.png", width=7, height=5, units="in", res=300)
plot.raw.oppover
dev.off()





##polity
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jureform.lag + polity2.lag +
                              elexec + #e_peaveduc+ 
                              e_polity2 +
                              e_migdppcln + 
                              e_miurbani + 
                              v2eldommon + 
                              log(e_mipopula) 
                            + transitional + altinfo.lag 
                            
                            + (1 | COWcode), REML=FALSE,
                            data = vdem.nodems)
summary(mm.elirreg.base.raw)

mm.elirreg.polity.raw <- lmer(v2elirreg.inv~jureform.lag + polity2.lag + 
                                  jureform.lag:polity2.lag +
                                  elexec + #e_peaveduc+ 
                                  e_polity2 +
                                  e_migdppcln + 
                                  e_miurbani + 
                                  v2eldommon + 
                                  log(e_mipopula) 
                                + transitional + altinfo.lag
                                
                                + (1 | COWcode), REML=FALSE,
                                data = vdem.nodems)
summary(mm.elirreg.polity.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.polity.raw)
plot.raw.polity <- interplot(mm.elirreg.polity.raw, var1="jureform.lag", var2="polity2.lag", hist=TRUE) +
  labs(x="Polity score", y="Marginal effect", title="Marginal effect of judicial reform \non intentional voting irregularities") + 
  theme_bw() + geom_hline(yintercept=0, linetype=2) #No relationship

png("./Plots/raw polity irreg.png", width=7, height=5, units="in", res=300)
plot.raw.polity
dev.off()




stargazer(mm.elirreg.polity.raw, mm.elirreg.ccsi.raw, mm.elirreg.oppart.raw, 
          type="html", 
          out="./Drafts/raw data models irreg.html")




#######
#CBPS section
####
library(CBPS)


 ###Polity
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "reform.positive.electionperiod.binary", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "state.ownership.lag", "core.civil.society.lag", "v2x_clpol",
            "diagacc.lag", "engaged.society.lag", "antisystem.lag", "v2psbars", "frassoc_thick.lag")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)

###Finding scores/weights 

cbps.out <- CBPS(reform.positive.electionperiod.binary~ democracy.duration.lag + oppaut.lag +
                   e_peaveduc +
                   #e_van_comp.lag  + 
                   polity2.lag + #polcomp.lag + parcomp.lag +
                   loggpdpc.lag + 
                   e_miurbani +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 +transitional
                 + exec.respectcon.lag  +  altinfo.lag + 
                   + leg.constraints.lag + state.ownership.lag, data=dataset.matching.complete)
summary(cbps.out)
balance(cbps.out)

###Output models



cbps.out <- CBPS(reform.positive.electionperiod.binary~ democracy.duration.lag + oppaut.lag + 
                   e_peaveduc +
                   # e_van_comp.lag  + 
                   polity2.lag + #polcomp.lag + parcomp.lag +
                   loggpdpc.lag + 
                   e_miurbani +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 +transitional
                 + exec.respectcon.lag  +  altinfo.lag + 
                   + leg.constraints.lag + core.civil.society.lag 
                 , data=dataset.matching.complete)
summary(cbps.out)
balance(cbps.out)


model.elirreg.negshockmajor.polity <- lmer(v2elirreg.inv~reform.positive.electionperiod.binary +
                                          polity2.lag + reform.positive.electionperiod.binary*polity2.lag + 
                                          elexec  + 
                                          #  e_polity2 +
                                          e_migdppcln + 
                                          #democracy.duration.lag + oppaut.lag +
                                          e_peaveduc +
                                          #e_van_comp.lag  + 
                                          #polcomp.lag2 + #polcomp.lag 
                                          
                                          e_miurbani +
                                          #  + hc.ind.2lag 
                                          # +  lc.ind.2lag
                                          +transitional
                                        + exec.respectcon.lag  +  altinfo.lag 
                                        + v2eldommon + 
                                          log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                        , weights=cbps.out$weights,
                                        data = cbps.out$data) 
summary(model.elirreg.negshockmajor.polity)
p.cbps.irreg.polity <- interplot(model.elirreg.negshockmajor.polity, var1="reform.positive.electionperiod.binary", var2="polity2.lag",
                              hist=TRUE) + theme_bw() + labs(x="Lagged Polity score", y="Marginal effect",
                                                             title="Marginal effect of positive judicial reform \non voting irregularities") +
  geom_hline(yintercept=0, linetype=2) 

png("./Plots/cbps polity irreg election period.png", height=5,
    width=7, units="in", res=300)
p.cbps.irreg.polity
dev.off()

 ###Civil society


myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "reform.positive.electionperiod.binary", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "state.ownership.lag", "core.civil.society.lag", "v2x_clpol",
            "diagacc.lag", "engaged.society.lag", "antisystem.lag", "v2psbars", "frassoc_thick.lag")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)

###Finding scores/weights 

cbps.out <- CBPS(reform.positive.electionperiod.binary~ democracy.duration.lag + oppaut.lag +
                   e_peaveduc +
                   #e_van_comp.lag  + 
                   polity2.lag + #polcomp.lag + parcomp.lag +
                   loggpdpc.lag + 
                   e_miurbani +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 +transitional
                 + exec.respectcon.lag  +  altinfo.lag + 
                   + leg.constraints.lag + state.ownership.lag, data=dataset.matching.complete)
summary(cbps.out)
balance(cbps.out)

###Output models



#core.civil.society.lag
cbps.out <- CBPS(reform.positive.electionperiod.binary~ democracy.duration.lag + oppaut.lag + 
                   e_peaveduc +
                   # e_van_comp.lag  + 
                   polity2.lag + #polcomp.lag + parcomp.lag +
                   loggpdpc.lag + 
                   e_miurbani +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 +transitional
                 + exec.respectcon.lag  +  altinfo.lag + 
                   + leg.constraints.lag + core.civil.society.lag 
                 , data=dataset.matching.complete)
summary(cbps.out)
balance(cbps.out)


model.elirreg.negshockmajor.ccs <- lmer(v2elirreg.inv~reform.positive.electionperiod.binary +
                                          core.civil.society.lag + reform.positive.electionperiod.binary*core.civil.society.lag + 
                                          elexec  + 
                                          #  e_polity2 +
                                          e_migdppcln + 
                                          #democracy.duration.lag + oppaut.lag +
                                          e_peaveduc +
                                          #e_van_comp.lag  + 
                                          #polcomp.lag2 + #polcomp.lag 
                                          + polity2.lag +
                                          # loggpdpc.lag  
                                          e_miurbani +
                                          #  + hc.ind.2lag 
                                          # +  lc.ind.2lag
                                          +transitional
                                        + exec.respectcon.lag  +  altinfo.lag 
                                        + v2eldommon + 
                                          log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                        , weights=cbps.out$weights,
                                        data = cbps.out$data) 
summary(model.elirreg.negshockmajor.ccs)
p.cbps.irreg.ccs <- interplot(model.elirreg.negshockmajor.ccs, var1="reform.positive.electionperiod.binary", var2="core.civil.society.lag",
                              hist=TRUE) + theme_bw() + labs(x="Lagged core civil society index", y="Marginal effect",
                                                             title="Marginal effect of positive judicial reform \non voting irregularities") +
  geom_hline(yintercept=0, linetype=2) 

png("./Plots/cbps cs irreg election period.png", height=5,
    width=7, units="in", res=300)
p.cbps.irreg.ccs
dev.off()


###

model.elintim.negshockmajor.ccs <- lmer(v2elintim.inv~reform.positive.electionperiod.binary +
                                          core.civil.society.lag + reform.positive.electionperiod.binary*core.civil.society.lag + 
                                          elexec  + 
                                          #  e_polity2 +
                                          e_migdppcln + 
                                          #democracy.duration.lag + oppaut.lag +
                                          e_peaveduc +
                                          #e_van_comp.lag  + 
                                          #polcomp.lag2 + #polcomp.lag 
                                          + polity2.lag +
                                          # loggpdpc.lag  
                                          e_miurbani +
                                          #  + hc.ind.2lag 
                                          # +  lc.ind.2lag
                                          +transitional
                                        + exec.respectcon.lag  +  altinfo.lag 
                                        + v2eldommon + 
                                          log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                        , weights=cbps.out$weights,
                                        data = cbps.out$data) 
summary(model.elintim.negshockmajor.ccs)
p.cbps.intim.ccs <- interplot(model.elintim.negshockmajor.ccs, var1="reform.positive.electionperiod.binary", var2="core.civil.society.lag",
                              hist=TRUE) + theme_bw() + labs(x="Lagged core civil society index", y="Marginal effect",
                                                             title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2)   #No interaction











#opposition.oversight.lag
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "reform.positive.electionperiod.binary", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "opposition.oversight.lag")  #, "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)

cbps.out <- CBPS(reform.positive.electionperiod.binary~ democracy.duration.lag + #oppaut.lag + 
                   e_peaveduc +
                   # e_van_comp.lag  + 
                   polity2.lag + #polcomp.lag + parcomp.lag +
                   loggpdpc.lag + 
                   e_miurbani +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 +transitional
                 + exec.respectcon.lag  +  altinfo.lag + 
                   + leg.constraints.lag + opposition.oversight.lag 
                 , data=dataset.matching.complete)
summary(cbps.out)
oversight.bal <- balance(cbps.out)

stargazer(oversight.bal, 
          type="html", 
          out="./Drafts/cbps balance oversight model irreg election period.html")


model.elirreg.negshockmajor.oppover <- lmer(v2elirreg.inv~reform.positive.electionperiod.binary +
                                              opposition.oversight.lag + reform.positive.electionperiod.binary*opposition.oversight.lag + 
                                              elexec  + 
                                              #  e_polity2 +
                                              e_migdppcln + 
                                               
                                              e_peaveduc +
                                              #e_van_comp.lag  + 
                                              #polcomp.lag2 + #polcomp.lag 
                                              + polity2.lag +
                                               
                                              e_miurbani 
                                            +transitional
                                            + exec.respectcon.lag  +  altinfo.lag 
                                            + v2eldommon + 
                                              log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                            , weights=cbps.out$weights,
                                            data = cbps.out$data) 
summary(model.elirreg.negshockmajor.oppover)
p.cbps.irreg.oppover <- interplot(model.elirreg.negshockmajor.oppover, var1="reform.positive.electionperiod.binary", var2="opposition.oversight.lag",
                                  hist=TRUE) + theme_bw() + labs(x="Opposition oversight lagged", y="Marginal effect",
                                                                 title="Marginal effect of positive judicial reform \non voting irregularities") +
  geom_hline(yintercept=0, linetype=2)    #Negative sig

png("./Plots/cbps oversight irreg election period.png", height=5,
    width=7, units="in", res=300)
p.cbps.irreg.oppover
dev.off()



###


model.elintim.negshockmajor.oppover <- lmer(v2elintim.inv~reform.positive.electionperiod.binary +
                                              opposition.oversight.lag + reform.positive.electionperiod.binary*opposition.oversight.lag + 
                                              elexec  + 
                                              #  e_polity2 +
                                              e_migdppcln + 
                                              
                                              e_peaveduc +
                                              #e_van_comp.lag  + 
                                              #polcomp.lag2 + #polcomp.lag 
                                              + polity2.lag +
                                              
                                              e_miurbani 
                                            +transitional
                                            + exec.respectcon.lag  +  altinfo.lag 
                                            + v2eldommon + 
                                              log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                            , weights=cbps.out$weights,
                                            data = cbps.out$data)  
summary(model.elintim.negshockmajor.oppover)
p.cbps.intim.oppover <- interplot(model.elintim.negshockmajor.oppover, var1="reform.positive.electionperiod.binary", var2="opposition.oversight.lag",
                                  hist=TRUE) + theme_bw() + labs(x="Opposition oversight lagged", y="Marginal effect",
                                                                 title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2)    #Negative sig







stargazer(model.elirreg.negshockmajor.polity, model.elirreg.negshockmajor.ccs, model.elirreg.negshockmajor.oppover, 
          type="html", 
          out="./Drafts/cbps outcome models irreg election period.html")


###Selection model for reform.positive.electionperiod.binary

model.selection <- glm(posreform.lag~ democracy.duration.lag +  
                         e_peaveduc +
                          polity2.lag + #polcomp.lag + parcomp.lag +
                         loggpdpc.lag + 
                         e_miurbani +
                         + hc.ind.2lag 
                       +  lc.ind.2lag
                       +transitional
                       + exec.respectcon.lag  +  altinfo.lag + 
                         + leg.constraints.lag + opposition.oversight.lag 
                       , data=vdem.nodems, family=binomial(link="logit"))
summary(model.selection)

######Entropy balancing using reform.positive.electionperiod.binary as treatment
###As a robustness check
###Here the unit of observation is the election period
###So need to get initial values of all control variables for the electoral period
###Those are then used as covariates for balancing reform.positive.electionperiod.binary

library(ebal)

##regtrans

myvars <- c("COWcode", "year", "transitional.prereform", "v2elirreg.inv", "v2elintim.inv", "lc.ind.prereform", 
            "hc.ind.prereform", "oppaut.prereform", "elexec", "education.prereform",  
            "opposition.oversight.prereform", "polity2.lag", "urban.prereform", "v2eldommon", "e_mipopula", 
            "country_name", "reform.positive.electionperiod.binary", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.prereform", "exec.respectcon.prereform", "leg.constraints.prereform", 
            "altinfo.prereform", "years.since.election")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)


myvars <- c("democracy.duration.prereform", "oppaut.prereform",  "polity2.lag", "years.since.election",
            "loggpdpc.lag", "urban.prereform",
            "hc.ind.prereform",  "lc.ind.prereform", "transitional.prereform", "exec.respectcon.prereform", "altinfo.prereform", "education.prereform",
            "leg.constraints.prereform", "COWcode", "year")  #"lji.lag"
ebal.covariates <- dataset.matching.complete[myvars]
#ebal.covariates.var <- ebal.covariates

ebal.test <- ebalance(Treatment=dataset.matching.complete$reform.positive.electionperiod.binary, X = ebal.covariates[c(-15, -16)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$reform.positive.electionperiod.binary==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$reform.positive.electionperiod.binary==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$reform.positive.electionperiod.binary==0,],2,mean)




dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$reform.positive.electionperiod.binary == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$reform.positive.electionperiod.binary == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
elirreg.posreform.polity.base <- lmer(v2elirreg.inv~reform.positive.electionperiod.binary +
                                        elexec + 
                                        opposition.oversight.prereform +
                                        e_migdppcln + 
                                        #e_miurbani + 
                                        v2eldommon + 
                                        log(e_mipopula) + (1 | COWcode), REML=FALSE 
                                      #+ transitional + altinfo.lag
                                      , weights=ebal.test.w,
                                      data = dataset.matching.complete.w)
summary(elirreg.posreform.polity.base)


mm.elirreg.posreform.polity.all <- lmer(v2elirreg.inv~reform.positive.electionperiod.binary +
                                          reform.positive.electionperiod.binary*opposition.oversight.prereform   + elexec + 
                                          opposition.oversight.prereform +
                                          e_migdppcln + 
                                          #e_miurbani + 
                                          v2eldommon + 
                                          log(e_mipopula) #+ transitional + altinfo.lag 
                                        + (1 | COWcode), REML=FALSE, 
                                        weights=ebal.test.w,
                                        data = dataset.matching.complete.w)
summary(mm.elirreg.posreform.polity.all)
p2 <- interplot(mm.elirreg.posreform.polity.all, var1="reform.positive.electionperiod.binary", var2="opposition.oversight.prereform",
                hist=TRUE) + theme_bw() + labs(x="Polity score (1-year lag)", y="Marginal effect" , 
                                               title="Marginal effect of positive reform on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2)   #No effect for fraud

png("./Plots/ebal regtrans irreg election period.png", height=5,
    width=7, units="in", res=300)
p1
dev.off()

lrtest(elirreg.posreform.regtrans.base, mm.elirreg.posreform.regtrans.all)




###

elintim.posreform.regtrans.base <- lmer(v2elintim.inv~reform.positive.electionperiod.binary +
                                          regtrans2.lag  + elexec + 
                                          #e_polity2 +
                                          e_migdppcln + 
                                          #e_miurbani + 
                                          v2eldommon + 
                                          log(e_mipopula) + (1 | COWcode), REML=FALSE 
                                        #+ transitional + altinfo.lag
                                        , weights=ebal.test.w,
                                        data = dataset.matching.complete.w)
summary(elintim.posreform.regtrans.base)


mm.elintim.posreform.regtrans.all <- lmer(v2elintim.inv~reform.positive.electionperiod.binary +
                                            regtrans2.lag + reform.positive.electionperiod.binary*regtrans2.lag   + elexec + 
                                            # e_polity2 +
                                            e_migdppcln + 
                                            #e_miurbani + 
                                            v2eldommon + 
                                            log(e_mipopula) #+ transitional + altinfo.lag 
                                          + (1 | COWcode), REML=FALSE, 
                                          weights=ebal.test.w,
                                          data = dataset.matching.complete.w)
summary(mm.elintim.posreform.regtrans.all)
p1.intim <- interplot(mm.elintim.posreform.regtrans.all, var1="reform.positive.electionperiod.binary", var2="regtrans2.lag",
                      hist=TRUE) + theme_bw() + labs(x="Change in Polity score", y="Marginal effect" , 
                                                     title="Marginal effect of positive reform on government intimidation") +
  geom_hline(yintercept=0, linetype=2)  #Boost in JI reduces intimidation in less comp areas


png("./Plots/ebal seat share intim election period.png", height=5,
    width=7, units="in", res=300)
p1.intim
dev.off()

lrtest(elintim.posreform.regtrans.base, mm.elintim.posreform.regtrans.all)
