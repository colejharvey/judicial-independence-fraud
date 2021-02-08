myvars <- c("COWcode", "year", "transitional",    "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", "hc.ind.2lag", "oppaut.2lag", "elexec", "education.2lag",  "v2elparlel",
            "polity2.adj.lag", "polity2.adj.2lag", "urban.2lag",  "v2eldommon", "e_mipopula", 
            "country_name", "reform_positive.lag", "loggpdpc.2lag",
            "democracy.duration.2lag", "exec.respectcon.2lag", "leg.constraints.2lag", 
            "altinfo.2lag", "loggpdpc.lag", "govseat.frac.2lag")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems.elections[myvars]
dataset.matching.complete <- na.omit(dataset.matching)


myvars <- c("democracy.duration.2lag", "oppaut.2lag",  "polity2.adj.2lag",
            "loggpdpc.2lag", "urban.2lag",
            "hc.ind.2lag",  "lc.ind.2lag", "transitional", "exec.respectcon.2lag", "altinfo.2lag", "education.2lag",
            "leg.constraints.2lag", "govseat.frac.2lag")  #"lji.lag"
ebal.covariates <- dataset.matching.complete[myvars]
#ebal.covariates.var <- ebal.covariates

ebal.test <- ebalance(Treatment=dataset.matching.complete$reform_positive.lag, X = ebal.covariates)



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$reform_positive.lag==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$reform_positive.lag==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$reform_positive.lag==0,],2,mean)
#library(stargazer)
#stargazer(cbind(t.means, c.means.bal, c.means), type="html", 
#          out="./Drafts/ebal adjusted means full data.html")



dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$reform_positive.lag == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))
  dataset.matching.complete.controls <- dataset.matching.complete.controls %>% mutate(ebal.test.w = ...26)
  dataset.matching.complete.controls <- dataset.matching.complete.controls %>% dplyr::select(-...26)
  
dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$reform_positive.lag == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- bind_rows(dataset.matching.complete.treat, dataset.matching.complete.controls)


###regression
elirreg.posreform.polity.base <- lm(v2elirreg.inv~reform_positive.lag +
                                      
                                      polity2.adj.lag  + elexec +  v2elparlel +
                                      #e_polity2 +
                                      loggpdpc.lag + 
                                      #e_miurbani + 
                                      v2eldommon +  
                                      log(e_mipopula) +
                                      factor(COWcode), 
                                    weights=ebal.test.w,  
                                    data = dataset.matching.complete.w)
summary(elirreg.posreform.polity.base)


mm.elirreg.posreform.polity.all <- lm(v2elirreg.inv~reform_positive.lag
                                      + 
                                        polity2.adj.lag +
                                        reform_positive.lag*polity2.adj.lag
                                      + elexec + v2elparlel +
                                        # e_polity2 +
                                        loggpdpc.lag + 
                                        #e_miurbani + 
                                        v2eldommon + 
                                        log(e_mipopula) +
                                        factor(COWcode),  
                                      weights=ebal.test.w,
                                      data = dataset.matching.complete.w)
summary(mm.elirreg.posreform.polity.all)

lrtest(elirreg.posreform.polity.base, mm.elirreg.posreform.polity.all)




p3 <- interplot(mm.elirreg.posreform.polity.all, var1="reform_positive.lag", var2="polity2.adj.lag",
                hist=TRUE, adjCI = TRUE) + theme_bw() + labs(x="Adjusted Polity score (1-year lag)", y="Marginal effect") +
  geom_hline(yintercept=0, linetype=2)   #No effect for fraud
p3


p3.bin <- inter.binning(data=dataset.matching.complete.w, Y = "v2elirreg.inv", D = "reform_positive.lag", X = "polity2.adj.lag",
                        Z = c("elexec", "loggpdpc.lag", "v2eldommon", "e_mipopula"), weights = "ebal.test.w", na.rm=TRUE, FE = c("COWcode"), ylab = "Marginal effect", xlab = "Adjusted Polity score (lag)", theme.bw = T, cex.lab = .75, cex.axis = .75, bin.labs = F)

p3.bin.image <- p3.bin$graph + geom_hline(yintercept = 0, linetype=2)

inter.kernel(data=dataset.matching.complete.w, Y = "v2elirreg.inv", D = "reform_positive.lag", X = "polity2.adj.lag",
             Z = c("v2elparlel", "elexec", "loggpdpc.lag", "v2eldommon", "e_mipopula"), weights = "ebal.test.w", na.rm=TRUE, FE = c("COWcode"))









### Legislative opposition oversight
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.2lag", "elexec", "education.2lag",  
            "opposition.oversight.lag", "opp.oversight.2lag", "urban.2lag", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "reform_positive.lag", "loggpdpc.2lag",
            "democracy.duration.2lag", "exec.respectcon.2lag", "leg.constraints.2lag", 
            "altinfo.2lag", "loggpdpc.lag", "years.since.election", "v2elparlel", "govseat.frac")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems.elections[myvars]
dataset.matching.complete <- na.omit(dataset.matching)


myvars <- c("democracy.duration.2lag", "oppaut.2lag",  "opp.oversight.2lag",
            "loggpdpc.2lag", "urban.2lag",
            "hc.ind.2lag",  "lc.ind.2lag", "transitional", "exec.respectcon.2lag", "altinfo.2lag", "education.2lag",
            "leg.constraints.2lag", "govseat.frac")  #"lji.lag"
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
dataset.matching.complete.controls <- dataset.matching.complete.controls %>% mutate(ebal.test.w = ...28)
dataset.matching.complete.controls <- dataset.matching.complete.controls %>% dplyr::select(-...28)

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$reform_positive.lag == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- bind_rows(dataset.matching.complete.treat, dataset.matching.complete.controls)


stargazer(cbind(t.means, c.means.bal, c.means), type="html", 
          out="./Drafts/ebal adjusted means full data.html")

###regression
elirreg.posreform.oversight.base <- lm(v2elirreg.inv~reform_positive.lag +
                                         opposition.oversight.lag + elexec + 
                                         #e_polity2 +
                                         loggpdpc.lag + 
                                         #e_miurbani + 
                                         v2eldommon + 
                                         log(e_mipopula)  + v2elparlel + 
                                         factor(COWcode)
                                       #+ transitional + altinfo.lag
                                       , weights=ebal.test.w,
                                       data = dataset.matching.complete.w)
summary(elirreg.posreform.oversight.base)


mm.elirreg.posreform.oversight <- lm(v2elirreg.inv~reform_positive.lag +
                                       opposition.oversight.lag + reform_positive.lag*opposition.oversight.lag   + elexec + 
                                       # e_polity2 +
                                       loggpdpc.lag + 
                                       #e_miurbani + 
                                       v2eldommon + 
                                       log(e_mipopula) #+ transitional + altinfo.lag 
                                     + v2elparlel +
                                       factor(COWcode), 
                                     weights=ebal.test.w,
                                     data = dataset.matching.complete.w)
summary(mm.elirreg.posreform.oversight)

