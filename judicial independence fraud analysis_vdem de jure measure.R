###Analysis script
###VDEM judicial reform variable (positive reform for ebal)
###July 2019
###Post-CPS analysis

###Use reform_positive.lag and unlagged covariates for selection model
rm(list = setdiff(ls(), lsf.str())) #Remove all except functions

library(tidyverse)
library(lme4)
library(lmtest)
library(ggplot2)
library(ggeffects)
library(interplot)
library(interplot.medline)
library(interflex)
library(stargazer)
library(wfe)
library(plm)

#####Loading dataset####
vdem.nodems <- read.csv("./vdem-2018-no-dems-post1945-polity-sept2018-condensed-tidy.csv")

vdem.nodems.elections <- subset(vdem.nodems, is.na(vdem.nodems$v2elirreg.inv)==FALSE)
p.polity <- ggplot(vdem.nodems.elections, aes(x=e_polity2)) + 
  geom_histogram(color="black", fill="white", binwidth = 1) + 
  labs(x = "Polity score", y = "Count", title = "Observations in dataset by Polity score") +
  theme_bw()

png("./Plots/polity histogram.png", height=5,
    width=7, units="in", res=300)
p.polity
dev.off()

###Treatment pattern
library(PanelMatch)
DisplayTreatment(unit.id = "COWcode",
                 time.id = "year", legend.position = "none",
                 xlab = "year", ylab = "Country Code",
                 treatment = "reform_positive.lag", data = vdem.nodems.elections)

###Checking raw data via interflex
  ##Dot is median, thick bars are 25-75 percentiles, thin bars are 5-95 percentiles
inter.raw(data = vdem.nodems.elections, Y = "v2elirreg.inv", D = "reform_positive.lag", X = "polity2.adj.lag") #Not great, not awful


###Checking kernal estimator via interflex
inter.kernel(data = vdem.nodems.elections, Y = "v2elirreg.inv", D = "reform_positive.lag", X = "polity2.adj.lag",
             FE = c("COWcode","year"), na.rm=TRUE) #Non-linear at the extremes, but otherwise a good linear pattern for the bulk of the data


###2-way FE approach
vdem.nodems.elections$COW.factor <- factor(vdem.nodems.elections$COWcode)
vdem.nodems.elections$year.factor <- factor(vdem.nodems.elections$year)

model.2wfe <- lm(v2elirreg.inv ~ COW.factor + year.factor + reform_positive.lag + 
                                 polity2.adj.lag + reform_positive.lag*polity2.adj.lag, data = vdem.nodems.elections)
summary(model.2wfe)
interplot(model.2wfe, var1="reform_positive.lag", var2 = "polity2.adj.lag") #Produces same results as entropy bal
interplot.medline(model.2wfe, var1="reform_positive.lag", var2 = "polity2.adj.lag")
inter.binning(data=vdem.nodems.elections, Y = "v2elirreg.inv", D = "reform_positive.lag", X = "polity2.adj.lag",
              FE = c("COWcode","year"), na.rm=TRUE)
mydf <- ggeffect(model.2wfe, terms = c("polity2.adj.lag", "reform_positive.lag"))
ggplot(mydf, aes(x, predicted, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)
plot(mydf) #Marginal effects plot


###2-way FE with election-year controls

###2-way FE approach
vdem.nodems.elections$COW.factor <- factor(vdem.nodems.elections$COWcode)
vdem.nodems.elections$year.factor <- factor(vdem.nodems.elections$year)

model.2wfe.c <- lm(v2elirreg.inv ~ COW.factor + year.factor + v2eldommon + elexec + reform_positive.lag + 
                   polity2.adj.lag + reform_positive.lag*polity2.adj.lag, data = vdem.nodems.elections)
summary(model.2wfe.c)
interplot(model.2wfe.c, var1="reform_positive.lag", var2 = "polity2.adj.lag") #Produces same results as entropy bal
interplot.medline(model.2wfe.c, var1="reform_positive.lag", var2 = "polity2.adj.lag")
inter.binning(data=vdem.nodems.elections, Y = "v2elirreg.inv", D = "reform_positive.lag", X = "polity2.adj.lag",
              Z = c("elexec", "v2eldommon"),
              FE = c("COWcode","year"), na.rm=TRUE)
mydf <- ggeffect(model.2wfe.c, terms = c("polity2.adj.lag", "reform_positive.lag"))
ggplot(mydf, aes(x, predicted, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)
plot(mydf) #Marginal effects plot

###Weighted 2-way FE approach (Imai and Kim 2020)

small.data <- vdem.nodems.elections %>% dplyr::select(COW.factor, year.factor, v2elirreg.inv, reform_positive.lag, elexec, v2eldommon, polity2.adj.lag)
small.data$COW.factor <- as.numeric(small.data$COW.factor)
small.data$year.factor <- as.numeric(small.data$year.factor)
small.data$reform_positive.lag <- as.numeric(small.data$reform_positive.lag)
small.data$polity2.adj.lag <- as.numeric(small.data$polity2.adj.lag)
small.data$elexec <- as.numeric(small.data$elexec)
small.data$v2eldommon <- as.numeric(small.data$v2eldommon)




model.2wfe.w <- wfe(v2elirreg.inv ~  reform_positive.lag + elexec + 
                                     #loggpdpc.lag + 
                                     v2eldommon + 
                                     #log(e_mipopula) +
                                     polity2.adj.lag + 
                                     reform_positive.lag*polity2.adj.lag, data = vdem.nodems.elections,
                                     unit.index = "COWcode", time.index = "year", treat = "reform_positive.lag",
                                     estimator = "did", qoi = "ate")
summary(model.2wfe.w)  #Unit refers to countries here; currently not working (SEs are all zero)

###Weighted 2-way DiD approach (Imai and Kim 2020)

model.2wfe.did <- wfe(v2elirreg.inv ~  reform_positive.lag + elexec + 
                      #loggpdpc.lag + 
                      v2eldommon + 
                      #log(e_mipopula) +
                      polity2.adj.lag + 
                      reform_positive.lag*polity2.adj.lag, data = small.data,
                    unit.index = "COW.factor", time.index = "year.factor", treat = "reform_positive.lag",
                    method = "unit", estimator = "Mdid")
summary(model.2wfe.did)  #Currently not working


###Hausman test for RE vs FE

form <- v2elirreg.inv ~  reform_positive.lag + elexec + v2eldommon + polity2.adj.lag +  reform_positive.lag*polity2.adj.lag

wi <- plm(form, data = vdem.nodems.elections, model = "within", index = "COWcode")
re <- plm(form, data = vdem.nodems.elections, model = "random", index = "COWcode")
phtest(wi, re)
phtest(form, data = vdem.nodems.elections, index= "COWcode")
phtest(form, data = vdem.nodems.elections, method = "aux", index = "COWcode")

# robust Hausman test (regression-based)
phtest(form, data = vdem.nodems.elections, method = "aux", vcov = vcovHC, index = "COWcode")


#####
#Entropy balancing

###Entropy balancing using reform_positive.lag as treatment (i.e. and increase in JI)   
library(ebal)

 
##polity2.adj

myvars <- c("COWcode", "year", "transitional", "v2elparlel", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.2lag", "elexec", "education.2lag",  
            "polity2.adj.lag", "polity2.adj.2lag", "urban.2lag", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "reform_positive.lag", "e_migdppcln", "loggpdpc.2lag",
            "democracy.duration.2lag", "exec.respectcon.2lag", "leg.constraints.2lag", 
            "altinfo.2lag", "loggpdpc.lag", "years.since.election")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)


myvars <- c("democracy.duration.2lag", "oppaut.2lag",  "polity2.adj.2lag",
             "loggpdpc.2lag", "urban.2lag",
            "hc.ind.2lag",  "lc.ind.2lag", "transitional", "exec.respectcon.2lag", "altinfo.2lag", "education.2lag",
            "leg.constraints.2lag")  #"lji.lag"
ebal.covariates <- dataset.matching.complete[myvars]
#ebal.covariates.var <- ebal.covariates

ebal.test <- ebalance(Treatment=dataset.matching.complete$reform_positive.lag, X = ebal.covariates)



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$reform_positive.lag==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$reform_positive.lag==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$reform_positive.lag==0,],2,mean)
library(stargazer)
stargazer(cbind(t.means, c.means.bal, c.means), type="html", 
          out="./Drafts/ebal adjusted means full data.html")



dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$reform_positive.lag == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$reform_positive.lag == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
elirreg.posreform.polity.base <- lmer(v2elirreg.inv~reform_positive.lag + v2elparlel +
                                        polity2.adj.lag  + elexec + 
                                            #e_polity2 +
                                        loggpdpc.lag + 
                                            #e_miurbani + 
                                            v2eldommon + 
                                            log(e_mipopula) + (1 | COWcode), REML=FALSE 
                                          #+ transitional + altinfo.lag
                                          , weights=ebal.test.w,
                                          data = dataset.matching.complete.w)
summary(elirreg.posreform.polity.base)


mm.elirreg.posreform.polity.all <- lmer(v2elirreg.inv~reform_positive.lag + v2elparlel +
                                          polity2.adj.lag + reform_positive.lag*polity2.adj.lag   + elexec + 
                                              # e_polity2 +
                                          loggpdpc.lag + 
                                              #e_miurbani + 
                                              v2eldommon + 
                                              log(e_mipopula) #+ transitional + altinfo.lag 
                                            + (1 | COWcode), REML=FALSE, 
                                            weights=ebal.test.w,
                                            data = dataset.matching.complete.w)
summary(mm.elirreg.posreform.polity.all)
p2 <- interplot(mm.elirreg.posreform.polity.all, var1="reform_positive.lag", var2="polity2.adj.lag",
                hist=TRUE) + theme_bw() + labs(x="Adjusted Polity score (1-year lag)", y="Marginal effect" , 
                                               title="Marginal effect of positive reform on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2)   #No effect for fraud
inter.binning(data=dataset.matching.complete.w, Y = "v2elirreg.inv", D = "reform_positive.lag", X = "polity2.adj.lag",
              Z = c("v2elparlel", "elexec", "loggpdpc.lag", "v2eldommon", "e_mipopula"), weights = "ebal.test.w", na.rm=TRUE)



png("./Plots/ebal polity irreg.png", height=5,
    width=7, units="in", res=300)
p2
dev.off()

lrtest(elirreg.posreform.polity.base, mm.elirreg.posreform.polity.all)

summary(dataset.matching.complete.w$polity2.lag)  
ss1 <- -0.34*0 + -0.08*-4 + .04*-4*0
ss2 <- -0.34*1 + -0.08*-4 + .04*-4*1
ss2-ss1
(ss2 - ss1) / 5.24

ss2 <- -0.34*0 + -0.08*-4 + .04*-4*0
ss3 <- -0.34*0 + -0.08*3 + .04*3*0  #DV range = 2.86 + 2.38 = 5.24
(ss2-ss3) / 5.24 


mm.elirreg.posreform.polity.all.fe <- plm(v2elirreg.inv~reform_positive.lag + v2elparlel +
                                            polity2.adj.lag + reform_positive.lag*polity2.adj.lag   + elexec + 
                                            # e_polity2 +
                                            loggpdpc.lag + 
                                            #e_miurbani + 
                                            v2eldommon + 
                                            log(e_mipopula), weights = ebal.test.w, data = dataset.matching.complete.w, index = "COWcode")
summary(mm.elirreg.posreform.polity.all.fe)  ##FE model provides even stronger results
inter.binning(data=dataset.matching.complete.w, Y = "v2elirreg.inv", D = "reform_positive.lag", X = "polity2.adj.lag",
              Z = c("v2elparlel", "elexec", "loggpdpc.lag", "v2eldommon", "e_mipopula"), weights = "ebal.test.w",
              FE = c("COWcode","year"), na.rm=TRUE)


###

elintim.posreform.polity.base <- lmer(v2elintim.inv~reform_positive.lag +
                                        polity2.adj.lag  + elexec + 
                                        #e_polity2 +
                                        loggpdpc.lag + 
                                        #e_miurbani + 
                                        v2eldommon + 
                                        log(e_mipopula) + (1 | COWcode), REML=FALSE 
                                      #+ transitional + altinfo.lag
                                      , weights=ebal.test.w,
                                      data = dataset.matching.complete.w)
summary(elintim.posreform.polity.base)


mm.elintim.posreform.polity.all <- lmer(v2elintim.inv~reform_positive.lag +
                                          polity2.adj.lag + reform_positive.lag*polity2.adj.lag + elexec + 
                                          #e_polity2 +
                                          loggpdpc.lag + 
                                          #e_miurbani + 
                                          v2eldommon + 
                                          log(e_mipopula) + (1 | COWcode), REML=FALSE 
                                        #+ transitional + altinfo.lag
                                        , weights=ebal.test.w,
                                        data = dataset.matching.complete.w)
summary(mm.elintim.posreform.polity.all)
p2.intim <- interplot(mm.elintim.posreform.polity.all, var1="reform_positive.lag", var2="polity2.adj.lag",
                      hist=TRUE) + theme_bw() + labs(x="Adjusted Polity score", y="Marginal effect" , 
                                                     title="Marginal effect of positive judicial reform on government intimidation") +
  geom_hline(yintercept=0, linetype=2)  #Boost in JI reduces intimidation in less comp areas


png("./Plots/ebal polity intim.png", height=5,
    width=7, units="in", res=300)
p2.intim
dev.off()

lrtest(elintim.posreform.polity.base, mm.elintim.posreform.polity.all)


##Core civil society


myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.2lag", "elexec", "education.2lag",  
            "civil.society.lag", "civil.society.2lag", "urban.2lag", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "reform_positive.lag", "e_migdppcln", "loggpdpc.2lag",
            "democracy.duration.2lag", "exec.respectcon.2lag", "leg.constraints.2lag", 
            "altinfo.2lag", "loggpdpc.lag", "years.since.election")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)


myvars <- c("democracy.duration.2lag", "oppaut.2lag",  "civil.society.2lag",
            "loggpdpc.2lag", "urban.2lag",
            "hc.ind.2lag",  "lc.ind.2lag", "transitional", "exec.respectcon.2lag", "altinfo.2lag", "education.2lag",
            "leg.constraints.2lag")  #"lji.lag"
ebal.covariates <- dataset.matching.complete[myvars]
#ebal.covariates.var <- ebal.covariates

ebal.test <- ebalance(Treatment=dataset.matching.complete$reform_positive.lag, X = ebal.covariates)



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$reform_positive.lag==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$reform_positive.lag==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$reform_positive.lag==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$reform_positive.lag == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$reform_positive.lag == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
elirreg.posreform.civil.base <- lmer(v2elirreg.inv~reform_positive.lag +
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


mm.elirreg.posreform.civil.all <- lmer(v2elirreg.inv~reform_positive.lag +
                                             civil.society.lag + reform_positive.lag*civil.society.lag   + elexec + 
                                             # e_polity2 +
                                         loggpdpc.lag + 
                                             #e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) #+ transitional + altinfo.lag 
                                           + (1 | COWcode), REML=FALSE, 
                                           weights=ebal.test.w,
                                           data = dataset.matching.complete.w)
summary(mm.elirreg.posreform.civil.all)
p3 <- interplot(mm.elirreg.posreform.civil.all, var1="reform_positive.lag", var2="civil.society.lag",
                hist=TRUE) + theme_bw() + labs(x="Civil society index (1-year lag)", y="Marginal effect" , 
                                               title="Marginal effect of positive reform on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) #In low CS openness areas, JI boost reduces fraud


png("./Plots/ebal core civil society irreg.png", height=5,
    width=7, units="in", res=300)
p3
dev.off()

lrtest(elirreg.posreform.civil.base, mm.elirreg.posreform.civil.all)


###

elintim.posreform.civil.base <- lmer(v2elintim.inv~reform_positive.lag +
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


mm.elintim.posreform.civil.all <- lmer(v2elintim.inv~reform_positive.lag +
                                             civil.society.lag + reform_positive.lag*civil.society.lag   + elexec + 
                                             # e_polity2 +
                                         loggpdpc.lag + 
                                             #e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) #+ transitional + altinfo.lag 
                                           + (1 | COWcode), REML=FALSE, 
                                           weights=ebal.test.w,
                                           data = dataset.matching.complete.w)
summary(mm.elintim.posreform.civil.all)
p3.intim <- interplot(mm.elintim.posreform.civil.all, var1="reform_positive.lag", var2="civil.society.lag",
                      hist=TRUE) + theme_bw() + labs(x="Civil society index (1-year lag)", y="Marginal effect" , 
                                                     title="Marginal effect of positive reform on government intimidation") +
  geom_hline(yintercept=0, linetype=2) #Same as above


png("./Plots/ebal core civil society intim.png", height=5,
    width=7, units="in", res=300)
p3.intim
dev.off()

lrtest(elintim.posreform.civil.base, mm.elintim.posreform.civil.all)




### Legislative opposition oversight
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.2lag", "elexec", "education.2lag",  
            "opposition.oversight.lag", "opp.oversight.2lag", "urban.2lag", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "reform_positive.lag", "e_migdppcln", "loggpdpc.2lag",
            "democracy.duration.2lag", "exec.respectcon.2lag", "leg.constraints.2lag", 
            "altinfo.2lag", "loggpdpc.lag", "years.since.election")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)


myvars <- c("democracy.duration.2lag", "oppaut.2lag",  "opp.oversight.2lag",
            "loggpdpc.2lag", "urban.2lag",
            "hc.ind.2lag",  "lc.ind.2lag", "transitional", "exec.respectcon.2lag", "altinfo.2lag", "education.2lag",
            "leg.constraints.2lag")  #"lji.lag"
ebal.covariates <- dataset.matching.complete[myvars]
#ebal.covariates.var <- ebal.covariates

ebal.test <- ebalance(Treatment=dataset.matching.complete$reform_positive.lag, X = ebal.covariates)




# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$reform_positive.lag==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$reform_positive.lag==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$reform_positive.lag==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$reform_positive.lag == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$reform_positive.lag == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
elirreg.posreform.oversight.base <- lmer(v2elirreg.inv~reform_positive.lag +
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


mm.elirreg.posreform.oversight <- lmer(v2elirreg.inv~reform_positive.lag +
                                             opposition.oversight.lag + reform_positive.lag*opposition.oversight.lag   + elexec + 
                                             # e_polity2 +
                                         loggpdpc.lag + 
                                             #e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) #+ transitional + altinfo.lag 
                                           + (1 | COWcode), REML=FALSE, 
                                           weights=ebal.test.w,
                                           data = dataset.matching.complete.w)
summary(mm.elirreg.posreform.oversight)
p4 <- interplot(mm.elirreg.posreform.oversight, var1="reform_positive.lag", var2="opposition.oversight.lag",
                hist=TRUE) + theme_bw() + labs(x="Opposition oversight (1-year lag)", y="Marginal effect" , 
                                               title="Marginal effect of positive reform on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2)   #When opp oversight is low, a positive reform reduces fraud

png("./Plots/ebal irregularities oversight.png", height=5,
    width=7, units="in", res=300)
p4
dev.off()

lrtest(elirreg.posreform.oversight.base, mm.elirreg.posreform.oversight)

###

elintim.posreform.oversight.base <- lmer(v2elintim.inv~reform_positive.lag +
                                               opposition.oversight.lag + elexec + 
                                               #e_polity2 +
                                           loggpdpc.lag + 
                                               #e_miurbani + 
                                               v2eldommon + 
                                               log(e_mipopula)  + (1 | COWcode), REML=FALSE
                                             #+ transitional + altinfo.lag
                                             , weights=ebal.test.w,
                                             data = dataset.matching.complete.w)
summary(elintim.posreform.oversight.base)


mm.elintim.posreform.oversight <- lmer(v2elintim.inv~reform_positive.lag +
                                             opposition.oversight.lag + reform_positive.lag*opposition.oversight.lag   + elexec + 
                                             # e_polity2 +
                                         loggpdpc.lag + 
                                             #e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) #+ transitional + altinfo.lag 
                                           + (1 | COWcode), REML=FALSE, 
                                           weights=ebal.test.w,
                                           data = dataset.matching.complete.w)
summary(mm.elintim.posreform.oversight)
p4.intim <- interplot(mm.elintim.posreform.oversight, var1="reform_positive.lag", var2="opposition.oversight.lag",
                      hist=TRUE) + theme_bw() + labs(x="Opposition oversight (1-year lag)", y="Marginal effect" , 
                                                     title="Marginal effect of positive reform on government intimidation") +
  geom_hline(yintercept=0, linetype=2)  #Same as above

png("./Plots/ebal oversight intim.png", height=5,
    width=7, units="in", res=300)
p4.intim  
dev.off()

lrtest(elintim.posreform.oversight.base, mm.elintim.posreform.oversight)



###Main results table 
  ##Irregularities
library(stargazer)
stargazer(elirreg.posreform.polity.base, mm.elirreg.posreform.polity.all, elirreg.posreform.civil.base,
          mm.elirreg.posreform.civil.all, elirreg.posreform.oversight.base, mm.elirreg.posreform.oversight,
          type="html", out="./Drafts/ebal results irreg.html")

stargazer(elirreg.posreform.polity.base, mm.elirreg.posreform.polity.all, elirreg.posreform.civil.base,
          mm.elirreg.posreform.civil.all, elirreg.posreform.oversight.base, mm.elirreg.posreform.oversight,
          type="latex", digits=2) #Need to change to stars at <.05 and <.01 only
  ##Intimidation
stargazer(elintim.posreform.polity.base, mm.elintim.posreform.polity.all, elintim.posreform.civil.base,
          mm.elintim.posreform.civil.all, elintim.posreform.oversight.base, mm.elintim.posreform.oversight,
          type="html", out="./Drafts/ebal results intim.html")
stargazer(elintim.posreform.polity.base, mm.elintim.posreform.polity.all, elintim.posreform.civil.base,
          mm.elintim.posreform.civil.all, elintim.posreform.oversight.base, mm.elintim.posreform.oversight,
          type="latex", digits=2)














###Appendix table and plots
library(stargazer)

###Alternative measures of independence: LJI + jucon and HC/LC ind from Vdem
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



###Selection model, using the same dataset as ebal model with opposition oversight

myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.2lag", "elexec", "education.2lag",  
            "opp.oversight.2lag", "urban.2lag", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "reform_positive.lag", "loggpdpc.2lag",
            "democracy.duration.2lag", "exec.respectcon.2lag", "leg.constraints.2lag", 
            "altinfo.2lag", "loggpdpc.lag", "years.since.election")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)

model.selection <- glm(reform_positive.lag~democracy.duration.2lag + oppaut.2lag +  opp.oversight.2lag +
             loggpdpc.2lag + urban.2lag +
            hc.ind.2lag +  lc.ind.2lag + transitional + exec.respectcon.2lag + altinfo.2lag + education.2lag +
            +leg.constraints.2lag,  family=binomial(link="logit"), data = dataset.matching.complete)
summary(model.selection)
null.deviance <- model.selection$null.deviance
residual.deviance <- model.selection$deviance

  ##null hypothesis: the model as a whole is no better than the null model
  ##                 I.e., null hypothesis is that the model is just noise
  ##                 With a low enough p-value, can reject the null (i.e. increase confidence that the model fits the data well)  

p.value <- 1- pchisq((null.deviance-residual.deviance), df = (694-682))  #pchisq gives the proportion of the distribution to the left of the value, so subtract from 1 to get the p-value

stargazer(model.selection, type="html", out="./Drafts/selection model results.html")
stargazer(model.selection, type="latex", digits = 2)


####Plot of lji.2lag and posreform.lag
p.levels.lji <- qplot(lji.2lag, jitter(reform_positive.lag), data=vdem.nodems, xlab="Latent judicial independence (lagged)",
                  ylab="Positive judicial reform (jittered)", main="Latent judicial independence prior to reform") +
  theme_bw()

png("./Plots/lji vs positive reform.png", height=5,
    width=7, units="in", res=300)
p.levels.lji
dev.off()

p.levels.hcind <- qplot(hc.ind.2lag, jitter(reform_positive.lag), data=dataset.matching.complete, xlab="High-court independence (lagged)",
                      ylab="Positive judicial reform (jittered)", main="High-court independence prior to reform") +
  theme_bw()

png("./Plots/hcind vs positive reform.png", height=5,
    width=7, units="in", res=300)
p.levels.hcind
dev.off()


p.levels.lcind <- qplot(lc.ind.2lag, jitter(reform_positive.lag), data=dataset.matching.complete, xlab="Low-court independence (lagged)",
                        ylab="Positive judicial reform (jittered)", main="Low-court independence prior to reform") +
  theme_bw()

png("./Plots/lcind vs positive reform.png", height=5,
    width=7, units="in", res=300)
p.levels.lcind
dev.off()

p.levels.polity <- qplot(polity.2lag, jitter(reform_positive.lag), data=vdem.nodems, xlab="Polity score (lagged)",
                      ylab="Positive judicial reform (jittered)", main="Polity score prior to reform") +
  theme_bw()

png("./Plots/polity vs positive reform.png", height=5,
    width=7, units="in", res=300)
p.levels.polity
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
  labs(x="Opposition oversight (lagged)", y="Marginal effect", title="Latent judicial independence") + 
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
  labs(x="Opposition oversight (lagged)", y="Marginal effect", title="High-court independence") + 
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
  labs(x="Opposition oversight (lagged)", y="Marginal effect", title="Low-court independence") + 
  theme_bw() + geom_hline(yintercept=0, linetype=2)

png("./Plots/lcind x oppoversight.png", height=5,
    width=7, units="in", res=300)
p.lc.opp
dev.off()

##Jucon by oppoversight

mm.elirreg.jucon.opp <- lmer(v2elirreg.inv~jucon.lag +
                               opposition.oversight.lag + jucon.lag*opposition.oversight.lag   + elexec +
                               e_peaveduc+ 
                               #e_polity2 +
                               e_migdppcln + 
                               e_miurbani + 
                               v2eldommon + 
                               log(e_mipopula) 
                             + transitional
                             
                             + (1 | COWcode), REML=FALSE,
                             data = vdem.nodems)
summary(mm.elirreg.jucon.opp)
p.jucon.opp <- interplot(mm.elirreg.jucon.opp, var1="jucon.lag", var2="opposition.oversight.lag", hist=TRUE) +
  labs(x="Opposition oversight (lagged)", y="Marginal effect", title="Judicial constraints on the \nexecutive") + 
  theme_bw() + geom_hline(yintercept=0, linetype=2)

png("./Plots/jucon x oppoversight.png", height=5,
    width=7, units="in", res=300)
p.jucon.opp
dev.off()

##Table

stargazer(mm.elirreg.lji.opp, mm.elirreg.jucon.opp, mm.elirreg.hcind.opp, mm.elirreg.lcind.opp, type="html", 
          out="./Drafts/table alternative measures by oversight.html")

stargazer(mm.elirreg.lji.opp, mm.elirreg.jucon.opp, mm.elirreg.hcind.opp, mm.elirreg.lcind.opp, type="latex", digits=2)

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
multiplot(p.lji.opp, p.jucon.opp, p.hc.opp, p.lc.opp, cols = 2)
dev.off()



####LJI by civil society, no balancing because not a binary treatment
mm.elirreg.lji.cci <- lmer(v2elirreg.inv~lji.lag +
                             civil.society.lag + lji.lag*civil.society.lag   + elexec +
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
interplot(mm.elirreg.lji.cci, var1="lji.lag", var2="civil.society.lag", hist=TRUE)


####LJI by polity, no balancing because not a binary treatment

mm.elirreg.lji.comp <- lmer(v2elirreg.inv~lji.lag +
                              polity2.adj.lag + lji.lag*polity2.adj.lag   + elexec +
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
interplot(mm.elirreg.lji.comp, var1="lji.lag", var2="polity2.adj.lag", hist=TRUE)



###Un-processed data models
##Irregularities first, followed by govt intimidation

vdem.nodems$jureform.lag <- factor(vdem.nodems$jureform.lag)
vdem.nodems$jureform.lag <- relevel(vdem.nodems$jureform.lag, ref= "1")

##Core civil society
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jureform.lag +  civil.society.lag +
                              elexec + #e_peaveduc+ 
                              #polity2.adj.lag +
                              loggpdpc.lag + 
                              e_miurbani + 
                              v2eldommon + 
                              log(e_mipopula) 
                            + transitional  
                            
                            + (1 | COWcode), REML=FALSE,
                            data = vdem.nodems)
summary(mm.elirreg.base.raw)

mm.elirreg.ccsi.raw <- lmer(v2elirreg.inv~jureform.lag + civil.society.lag + 
                              jureform.lag:civil.society.lag +
                              elexec + #e_peaveduc+ 
                              #polity2.adj.lag +
                              loggpdpc.lag + 
                              e_miurbani + 
                              v2eldommon + 
                              log(e_mipopula) 
                            + transitional 
                            
                            + (1 | COWcode), REML=FALSE,
                            data = vdem.nodems)
summary(mm.elirreg.ccsi.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.ccsi.raw)
plot.raw.cc <- interplot(mm.elirreg.ccsi.raw, var1="jureform.lag", var2="civil.society.lag", hist=TRUE) +
  labs(x="Civil society openness", y="Marginal effect", title="Marginal effect of judicial reform \non intentional voting irregularities") + 
  theme_bw() + geom_hline(yintercept=0, linetype=2) #Unconvincing plots

png("./Plots/raw civil society irreg.png", width=7, height=5, units="in", res=300)
plot.raw.cc
dev.off()

####

mm.elintim.base.raw <- lmer(v2elintim.inv~jureform.lag + v2xcs_ccsi +
                              elexec + #e_peaveduc+ 
                              #polity2.lag +
                              loggpdpc.lag + 
                              e_miurbani + 
                              v2eldommon + 
                              log(e_mipopula) 
                            + transitional  
                            
                            + (1 | COWcode), REML=FALSE,
                            data = vdem.nodems)
summary(mm.elintim.base.raw)

mm.elintim.ccsi.raw <- lmer(v2elintim.inv~jureform.lag + v2xcs_ccsi + 
                              jureform.lag:v2xcs_ccsi +
                              elexec + #e_peaveduc+ 
                              #polity2.lag +
                              loggpdpc.lag + 
                              e_miurbani + 
                              v2eldommon + 
                              log(e_mipopula) 
                            + transitional 
                            
                            + (1 | COWcode), REML=FALSE,
                            data = vdem.nodems)
summary(mm.elintim.ccsi.raw)

lrtest(mm.elintim.base.raw, mm.elintim.ccsi.raw)
interplot(mm.elintim.ccsi.raw, var1="jureform.lag", var2="v2xcs_ccsi") #When CC independence is low, attack on courts -> more intimidation than when no change









##Opposition oversight
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jureform.lag + opposition.oversight.lag +
                              elexec + #e_peaveduc+ 
                              #polity2.lag +
                              loggpdpc.lag + 
                              e_miurbani + 
                              v2eldommon + 
                              log(e_mipopula) 
                            + transitional 
                            
                            + (1 | COWcode), REML=FALSE,
                            data = vdem.nodems)
summary(mm.elirreg.base.raw)

mm.elirreg.oppart.raw <- lmer(v2elirreg.inv~jureform.lag + opposition.oversight.lag + 
                                jureform.lag:opposition.oversight.lag +
                                elexec + #e_peaveduc+ 
                                #polity2.lag +
                                loggpdpc.lag + 
                                e_miurbani + 
                                v2eldommon + 
                                log(e_mipopula) 
                              + transitional 
                              
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
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jureform.lag + polity2.adj.lag +
                              elexec + #e_peaveduc+ 
                              loggpdpc.lag + 
                              e_miurbani + 
                              v2eldommon + 
                              log(e_mipopula) 
                            + transitional  
                            
                            + (1 | COWcode), REML=FALSE,
                            data = vdem.nodems)
summary(mm.elirreg.base.raw)

mm.elirreg.polity.raw <- lmer(v2elirreg.inv~jureform.lag + polity2.adj.lag + 
                                  jureform.lag:polity2.adj.lag +
                                elexec + 
                                loggpdpc.lag + 
                                e_miurbani+ 
                                v2eldommon + 
                                log(e_mipopula) 
                                + transitional
                                
                                + (1 | COWcode), REML=FALSE,
                                data = vdem.nodems)
summary(mm.elirreg.polity.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.polity.raw)
plot.raw.polity <- interplot(mm.elirreg.polity.raw, var1="jureform.lag", var2="polity2.adj.lag", hist=TRUE) +
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

myvars <- c("COWcode", "v2elirreg.inv", "democracy.duration.2lag", "oppaut.2lag",  "polity2.adj.2lag", "polity2.adj.lag",
            "loggpdpc.2lag", "urban.2lag",
            "hc.ind.2lag",  "lc.ind.2lag", "transitional", "exec.respectcon.2lag", "altinfo.2lag", "education.2lag",
            "leg.constraints.2lag", "reform_positive.lag", "elexec", "loggpdpc.2lag", "loggpdpc.lag", "v2eldommon", "e_mipopula")



dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)

###Finding scores/weights 

cbps.out <- CBPS(reform_positive.lag~ democracy.duration.2lag + oppaut.2lag +
                   education.2lag +
                   #e_van_comp.lag  + 
                   polity2.adj.2lag + #polcomp.lag + parcomp.lag +
                   loggpdpc.2lag + 
                   urban.2lag +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 + exec.respectcon.2lag  +  altinfo.2lag + 
                   + leg.constraints.2lag, data=dataset.matching.complete)
summary(cbps.out)
balance(cbps.out)

###Output models



cbps.out <- CBPS(reform_positive.lag~ democracy.duration.2lag + oppaut.2lag +
                   education.2lag +
                   #e_van_comp.lag  + 
                   polity2.adj.2lag + #polcomp.lag + parcomp.lag +
                   loggpdpc.2lag + 
                   urban.2lag +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 + exec.respectcon.2lag  +  altinfo.2lag + 
                   + leg.constraints.2lag, data=dataset.matching.complete)
summary(cbps.out)
balance(cbps.out)


model.elirreg.negshockmajor.polity <- lmer(v2elirreg.inv~reform_positive.lag +
                                             polity2.adj.lag + reform_positive.lag*polity2.adj.lag   + elexec + 
                                             # e_polity2 +
                                             loggpdpc.lag + 
                                             #e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                        , weights=cbps.out$weights,
                                        data = cbps.out$data) 
summary(model.elirreg.negshockmajor.polity)
p.cbps.irreg.polity <- interplot(model.elirreg.negshockmajor.polity, var1="reform_positive.lag", var2="polity2.adj.lag",
                              hist=TRUE) + theme_bw() + labs(x="Lagged adjusted Polity score", y="Marginal effect",
                                                             title="Marginal effect of positive judicial reform \non voting irregularities") +
  geom_hline(yintercept=0, linetype=2) 

png("./Plots/cbps polity irreg.png", height=5,
    width=7, units="in", res=300)
p.cbps.irreg.polity
dev.off()

 ###Civil society


myvars <- c("COWcode", "v2elirreg.inv", "democracy.duration.2lag", "oppaut.2lag", "civil.society.2lag",
            "civil.society.lag",
            "loggpdpc.2lag", "urban.2lag",
            "hc.ind.2lag",  "lc.ind.2lag", "transitional", "exec.respectcon.2lag", "altinfo.2lag", "education.2lag",
            "leg.constraints.2lag", "reform_positive.lag", "elexec", "loggpdpc.2lag", "loggpdpc.lag", "v2eldommon", "e_mipopula")

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)

###Finding scores/weights 


#core.civil.society.lag
cbps.out <- CBPS(reform_positive.lag~ democracy.duration.2lag + oppaut.2lag +
                   education.2lag +
                   #e_van_comp.lag  + 
                   civil.society.2lag + #polcomp.lag + parcomp.lag +
                   loggpdpc.2lag + 
                   urban.2lag +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 + exec.respectcon.2lag  +  altinfo.2lag + 
                   + leg.constraints.2lag, data=dataset.matching.complete)
summary(cbps.out)
balance(cbps.out)


model.elirreg.negshockmajor.ccs <- lmer(v2elirreg.inv~reform_positive.lag +
                                          civil.society.lag + reform_positive.lag*civil.society.lag   + elexec + 
                                          # e_polity2 +
                                          loggpdpc.lag + 
                                          #e_miurbani + 
                                          v2eldommon + 
                                          log(e_mipopula) + (1 | COWcode)
                                        , weights=cbps.out$weights,
                                        data = cbps.out$data) 
summary(model.elirreg.negshockmajor.ccs)
p.cbps.irreg.ccs <- interplot(model.elirreg.negshockmajor.ccs, var1="reform_positive.lag", var2="civil.society.lag",
                              hist=TRUE) + theme_bw() + labs(x="Lagged core civil society index", y="Marginal effect",
                                                             title="Marginal effect of positive judicial reform \non voting irregularities") +
  geom_hline(yintercept=0, linetype=2) 

png("./Plots/cbps cs irreg.png", height=5,
    width=7, units="in", res=300)
p.cbps.irreg.ccs
dev.off()


###

model.elintim.negshockmajor.ccs <- lmer(v2elintim.inv~posreform.lag +
                                          core.civil.society.lag + posreform.lag*core.civil.society.lag + 
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
p.cbps.irreg.ccs <- interplot(model.elintim.negshockmajor.ccs, var1="posreform.lag", var2="core.civil.society.lag",
                              hist=TRUE) + theme_bw() + labs(x="Lagged core civil society index", y="Marginal effect",
                                                             title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2)   #No interaction











#opposition.oversight.lag
myvars <- c("COWcode", "v2elirreg.inv", "democracy.duration.2lag", "oppaut.2lag", "opp.oversight.2lag",
            "opposition.oversight.lag",
            "loggpdpc.2lag", "urban.2lag",
            "hc.ind.2lag",  "lc.ind.2lag", "transitional", "exec.respectcon.2lag", "altinfo.2lag", "education.2lag",
            "leg.constraints.2lag", "reform_positive.lag", "elexec", "loggpdpc.2lag", "loggpdpc.lag", "v2eldommon", "e_mipopula")

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)

cbps.out <- CBPS(reform_positive.lag~ democracy.duration.2lag + oppaut.2lag +
                   education.2lag +
                   #e_van_comp.lag  + 
                   opp.oversight.2lag + #polcomp.lag + parcomp.lag +
                   loggpdpc.2lag + 
                   urban.2lag +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 + exec.respectcon.2lag  +  altinfo.2lag + 
                   + leg.constraints.2lag, data=dataset.matching.complete)
summary(cbps.out)
oversight.bal <- balance(cbps.out)

stargazer(oversight.bal, 
          type="html", 
          out="./Drafts/cbps balance oversight model irreg.html")


model.elirreg.negshockmajor.oppover <- lmer(v2elirreg.inv~reform_positive.lag +
                                              opposition.oversight.lag + reform_positive.lag*opposition.oversight.lag   + elexec + 
                                              # e_polity2 +
                                              loggpdpc.lag + 
                                              #e_miurbani + 
                                              v2eldommon + 
                                              log(e_mipopula) + (1 | COWcode)
                                            , weights=cbps.out$weights,
                                            data = cbps.out$data) 
summary(model.elirreg.negshockmajor.oppover)
p.cbps.irreg.oppover <- interplot(model.elirreg.negshockmajor.oppover, var1="reform_positive.lag", var2="opposition.oversight.lag",
                                  hist=TRUE) + theme_bw() + labs(x="Opposition oversight lagged", y="Marginal effect",
                                                                 title="Marginal effect of positive judicial reform \non voting irregularities") +
  geom_hline(yintercept=0, linetype=2)    #Negative sig

png("./Plots/cbps oversight irreg.png", height=5,
    width=7, units="in", res=300)
p.cbps.irreg.oppover
dev.off()



###


model.elintim.negshockmajor.oppover <- lmer(v2elintim.inv~jureform.lag +
                                              opposition.oversight.lag + jureform.lag*opposition.oversight.lag + 
                                              elexec  + 
                                              #  e_polity2 +
                                              e_migdppcln + 
                                              democracy.duration.lag + oppaut.lag +
                                              e_peaveduc +
                                              #e_van_comp.lag  + 
                                              #polcomp.lag2 + #polcomp.lag 
                                              + polity2.lag +
                                              loggpdpc.lag + 
                                              e_miurbani +
                                              + hc.ind.2lag 
                                            +  lc.ind.2lag
                                            +transitional
                                            + exec.respectcon.lag  +  altinfo.lag 
                                            + v2eldommon + 
                                              log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                            , weights=cbps.out$weights,
                                            data = cbps.out$data) 
summary(model.elintim.negshockmajor.oppover)
p.cbps.intim.oppover <- interplot(model.elintim.negshockmajor.oppover, var1="jureform.lag", var2="opposition.oversight.lag",
                                  hist=TRUE) + theme_bw() + labs(x="Opposition oversight lagged", y="Marginal effect",
                                                                 title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2)    #Negative sig







stargazer(model.elirreg.negshockmajor.polity, model.elirreg.negshockmajor.ccs, model.elirreg.negshockmajor.oppover, 
          type="html", 
          out="./Drafts/cbps outcome models irreg.html")

stargazer(model.elirreg.negshockmajor.polity, model.elirreg.negshockmajor.ccs, model.elirreg.negshockmajor.oppover, 
          type="latex", digits = 2)




