###Analysis script
###VDEM judicial reform variable (positive reform for ebal)
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

###Un-processed data models
##Irregularities first, followed by govt intimidation


##Core civil society
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jureform.lag + v2xcs_ccsi +
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

mm.elirreg.ccsi.raw <- lmer(v2elirreg.inv~jureform.lag + v2xcs_ccsi + 
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
summary(mm.elirreg.ccsi.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.ccsi.raw)
interplot(mm.elirreg.ccsi.raw, var1="jureform.lag", var2="v2xcs_ccsi") #Unconvincing plots


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








##Lagged lower house seat second place share 
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jureform.lag + lowchamb.second.seatshare.lag +
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

mm.elirreg.ss.raw <- lmer(v2elirreg.inv~jureform.lag + lowchamb.second.seatshare.lag + 
                            jureform.lag:lowchamb.second.seatshare.lag +
                            elexec + #e_peaveduc+ 
                            e_polity2 +
                            e_migdppcln + 
                            e_miurbani + 
                            v2eldommon + 
                            log(e_mipopula) 
                          + transitional + altinfo.lag
                          
                          + (1 | COWcode), REML=FALSE,
                          data = vdem.nodems)
summary(mm.elirreg.ss.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.ss.raw)
interplot(mm.elirreg.ss.raw, var1="jureform.lag", var2="lowchamb.second.seatshare.lag") #No interaction


##Engaged society
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jureform.lag + engaged.society.lag +
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

mm.elirreg.engaged.raw <- lmer(v2elirreg.inv~jureform.lag + engaged.society.lag + 
                                 jureform.lag:engaged.society.lag +
                                 elexec + #e_peaveduc+ 
                                 e_polity2 +
                                 e_migdppcln + 
                                 e_miurbani + 
                                 v2eldommon + 
                                 log(e_mipopula) 
                               + transitional + altinfo.lag
                               
                               + (1 | COWcode), REML=FALSE,
                               data = vdem.nodems)
summary(mm.elirreg.engaged.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.engaged.raw)
interplot(mm.elirreg.engaged.raw, var1="jureform.lag", var2="engaged.society.lag") #Increase in JI = less fraud in less open societies

###

mm.elintim.base.raw <- lmer(v2elintim.inv~jureform.lag + engaged.society.lag +
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

mm.elintim.engaged.raw <- lmer(v2elintim.inv~jureform.lag + engaged.society.lag + 
                                 jureform.lag:engaged.society.lag +
                                 elexec + #e_peaveduc+ 
                                 e_polity2 +
                                 e_migdppcln + 
                                 e_miurbani + 
                                 v2eldommon + 
                                 log(e_mipopula) 
                               + transitional + altinfo.lag
                               
                               + (1 | COWcode), REML=FALSE,
                               data = vdem.nodems)
summary(mm.elintim.engaged.raw)

lrtest(mm.elintim.base.raw, mm.elintim.engaged.raw)
interplot(mm.elintim.engaged.raw, var1="jureform.lag", var2="engaged.society.lag") #No effect



##State owernship of economy
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jureform.lag + v2clstown +
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

mm.elirreg.stown.raw <- lmer(v2elirreg.inv~jureform.lag + v2clstown + 
                               jureform.lag:v2clstown +
                               elexec + #e_peaveduc+ 
                               e_polity2 +
                               e_migdppcln + 
                               e_miurbani + 
                               v2eldommon + 
                               log(e_mipopula) 
                             + transitional + altinfo.lag
                             
                             + (1 | COWcode), REML=FALSE,
                             data = vdem.nodems)
summary(mm.elirreg.stown.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.stown.raw)
interplot(mm.elirreg.stown.raw, var1="jureform.lag", var2="v2clstown")  #High state ownership + attack = more irregularity; and vice versa; same is true of boost to JI (oddly)

###

mm.elintim.base.raw <- lmer(v2elintim.inv~jureform.lag + v2clstown +
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

mm.elintim.stown.raw <- lmer(v2elintim.inv~jureform.lag + v2clstown + 
                               jureform.lag:v2clstown +
                               elexec + #e_peaveduc+ 
                               e_polity2 +
                               e_migdppcln + 
                               e_miurbani + 
                               v2eldommon + 
                               log(e_mipopula) 
                             + transitional + altinfo.lag
                             
                             + (1 | COWcode), REML=FALSE,
                             data = vdem.nodems)
summary(mm.elintim.stown.raw)

lrtest(mm.elintim.base.raw, mm.elintim.stown.raw)
interplot(mm.elintim.stown.raw, var1="jureform.lag", var2="v2clstown")  #Private owernship + boost to JI = less violence




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
interplot(mm.elirreg.oppart.raw, var1="jureform.lag", var2="opposition.oversight.lag") #No effect for attack; boost reduces fraud when oversight is low





##Comp
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jureform.lag + e_van_comp.lag +
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

mm.elirreg.comp.raw <- lmer(v2elirreg.inv~jureform.lag + e_van_comp.lag + 
                              jureform.lag:e_van_comp.lag +
                              elexec + #e_peaveduc+ 
                              e_polity2 +
                              e_migdppcln + 
                              e_miurbani + 
                              v2eldommon + 
                              log(e_mipopula) 
                            + transitional + altinfo.lag
                            
                            + (1 | COWcode), REML=FALSE,
                            data = vdem.nodems)
summary(mm.elirreg.comp.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.comp.raw)
interplot(mm.elirreg.comp.raw, var1="jureform.lag", var2="e_van_comp.lag") #No relationship


####

mm.elintim.base.raw <- lmer(v2elintim.inv~jureform.lag + e_van_comp.lag +
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

mm.elintim.comp.raw <- lmer(v2elintim.inv~jureform.lag + e_van_comp.lag + 
                              jureform.lag:e_van_comp.lag +
                              elexec + #e_peaveduc+ 
                              e_polity2 +
                              e_migdppcln + 
                              e_miurbani + 
                              v2eldommon + 
                              log(e_mipopula) 
                            + transitional + altinfo.lag
                            
                            + (1 | COWcode), REML=FALSE,
                            data = vdem.nodems)
summary(mm.elintim.comp.raw)

lrtest(mm.elintim.base.raw, mm.elintim.comp.raw)
interplot(mm.elintim.comp.raw, var1="jureform.lag", var2="e_van_comp.lag") #When comp is low, boost reduces violence




##regtrans
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jureform.lag + regtrans2.lag +
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

mm.elirreg.regtrans.raw <- lmer(v2elirreg.inv~jureform.lag + regtrans2.lag + 
                                  jureform.lag:regtrans2.lag +
                                  elexec + #e_peaveduc+ 
                                  e_polity2 +
                                  e_migdppcln + 
                                  e_miurbani + 
                                  v2eldommon + 
                                  log(e_mipopula) 
                                + transitional + altinfo.lag
                                
                                + (1 | COWcode), REML=FALSE,
                                data = vdem.nodems)
summary(mm.elirreg.regtrans.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.regtrans.raw)
interplot(mm.elirreg.regtrans.raw, var1="jureform.lag", var2="regtrans2.lag") #No relationship

###

mm.elintim.base.raw <- lmer(v2elintim.inv~jureform.lag + regtrans2.lag +
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

mm.elintim.regtrans.raw <- lmer(v2elintim.inv~jureform.lag + regtrans2.lag + 
                                  jureform.lag:regtrans2.lag +
                                  elexec + #e_peaveduc+ 
                                  e_polity2 +
                                  e_migdppcln + 
                                  e_miurbani + 
                                  v2eldommon + 
                                  log(e_mipopula) 
                                + transitional + altinfo.lag
                                
                                + (1 | COWcode), REML=FALSE,
                                data = vdem.nodems)
summary(mm.elintim.regtrans.raw)

lrtest(mm.elintim.base.raw, mm.elintim.regtrans.raw)
interplot(mm.elintim.regtrans.raw, var1="jureform.lag", var2="regtrans2.lag") #Attack + authoritarian shift = more intim, boost + authoritarian shift = less



##parcomp2

mm.elirreg.base.raw <- lmer(v2elirreg.inv~jureform.lag + parcomp2.lag +
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

mm.elirreg.parcomp.raw <- lmer(v2elirreg.inv~jureform.lag + parcomp2.lag + 
                                 jureform.lag:parcomp2.lag +
                                 elexec + #e_peaveduc+ 
                                 e_polity2 +
                                 e_migdppcln + 
                                 e_miurbani + 
                                 v2eldommon + 
                                 log(e_mipopula) 
                               + transitional + altinfo.lag
                               
                               + (1 | COWcode), REML=FALSE,
                               data = vdem.nodems)
summary(mm.elirreg.parcomp.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.parcomp.raw)
interplot(mm.elirreg.parcomp.raw, "jureform.lag", "parcomp2.lag")  #Unconvincing plot

####

mm.elintim.base.raw <- lmer(v2elintim.inv~jureform.lag + parcomp2.lag +
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

mm.elintim.parcomp.raw <- lmer(v2elintim.inv~jureform.lag + parcomp2.lag + 
                                 jureform.lag:parcomp2.lag +
                                 elexec + #e_peaveduc+ 
                                 e_polity2 +
                                 e_migdppcln + 
                                 e_miurbani + 
                                 v2eldommon + 
                                 log(e_mipopula) 
                               + transitional + altinfo.lag
                               
                               + (1 | COWcode), REML=FALSE,
                               data = vdem.nodems)
summary(mm.elintim.parcomp.raw)

lrtest(mm.elintim.base.raw, mm.elintim.parcomp.raw)
interplot(mm.elintim.parcomp.raw, "jureform.lag", "parcomp2.lag")  #Unconvincing plot

##parreg

mm.elirreg.base.raw <- lmer(v2elirreg.inv~jureform.lag + as.factor(parreg2.lag) +
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

mm.elirreg.parreg.raw <- lmer(v2elirreg.inv~jureform.lag + as.factor(parreg2.lag) + 
                                jureform.lag:(as.factor(parreg2.lag)) +
                                elexec + #e_peaveduc+ 
                                e_polity2 +
                                e_migdppcln + 
                                e_miurbani + 
                                v2eldommon + 
                                log(e_mipopula) 
                              + transitional + altinfo.lag
                              
                              + (1 | COWcode), REML=FALSE,
                              data = vdem.nodems)
summary(mm.elirreg.parreg.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.parreg.raw)
interplot(mm.elirreg.parreg.raw, "jureform.lag", "parreg2.lag")






#######
#CBPS section
####
library(CBPS)

myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jureform.lag", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "state.ownership.lag", "core.civil.society.lag", "v2x_clpol",
            "diagacc.lag", "engaged.society.lag", "antisystem.lag", "v2psbars", "frassoc_thick.lag")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)

###Finding scores/weights 

cbps.out <- CBPS(jureform.lag~ democracy.duration.lag + oppaut.lag +
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

#frassoc_thick.lag
cbps.out <- CBPS(jureform.lag~ democracy.duration.lag + oppaut.lag +
                   e_peaveduc +
                   #e_van_comp.lag  + 
                   polity2.lag + #polcomp.lag + parcomp.lag +
                   loggpdpc.lag + 
                   e_miurbani +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 +transitional
                 + exec.respectcon.lag  +  altinfo.lag + 
                   + leg.constraints.lag + frassoc_thick.lag, data=dataset.matching.complete)
summary(cbps.out)
balance(cbps.out)

model.elirreg.negshockmajor.frassoc <- lmer(v2elirreg.inv~jureform.lag +
                                              frassoc_thick.lag + jureform.lag*frassoc_thick.lag + elexec  + 
                                              #e_polity2 +
                                              e_migdppcln + 
                                              #e_miurbani + 
                                              v2eldommon + 
                                              log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                            , weights=cbps.out$weights,
                                            data = cbps.out$data) 
summary(model.elirreg.negshockmajor.frassoc)
p.cbps.irreg.frassoc <- interplot(model.elirreg.negshockmajor.frassoc, var1="jureform.lag", var2="frassoc_thick.lag",
                                  hist=TRUE) + theme_bw() + labs(x="Lagged core civil society index", y="Marginal effect",
                                                                 title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2)   #Significant positive


###

model.elintim.negshockmajor.frassoc <- lmer(v2elintim.inv~jureform.lag +
                                              frassoc_thick.lag + jureform.lag*frassoc_thick.lag + elexec  + 
                                              #e_polity2 +
                                              e_migdppcln + 
                                              #e_miurbani + 
                                              v2eldommon + 
                                              log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                            , weights=cbps.out$weights,
                                            data = cbps.out$data) 
summary(model.elintim.negshockmajor.frassoc)
p.cbps.intim.frassoc <- interplot(model.elintim.negshockmajor.frassoc, var1="jureform.lag", var2="frassoc_thick.lag",
                                  hist=TRUE) + theme_bw() + labs(x="Lagged core civil society index", y="Marginal effect",
                                                                 title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2)   #Significant positive



#state.ownership.lag
cbps.out <- CBPS(jureform.lag~ democracy.duration.lag + oppaut.lag +
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


model.elirreg.negshockmajor.own <- lmer(v2elirreg.inv~jureform.lag +
                                          state.ownership.lag + jureform.lag*state.ownership.lag + 
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
summary(model.elirreg.negshockmajor.own)
p.cbps.irreg.stown <- interplot(model.elirreg.negshockmajor.own, var1="jureform.lag", var2="state.ownership.lag",
                                hist=TRUE) + theme_bw() + labs(x="Lagged state ownership", y="Marginal effect",
                                                               title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2)

###

model.elintim.negshockmajor.own <- lmer(v2elintim.inv~jureform.lag +
                                          state.ownership.lag + jureform.lag*state.ownership.lag + 
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
summary(model.elintim.negshockmajor.own)
p.cbps.intim.stown <- interplot(model.elintim.negshockmajor.own, var1="jureform.lag", var2="state.ownership.lag",
                                hist=TRUE) + theme_bw() + labs(x="Lagged state ownership", y="Marginal effect",
                                                               title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 


#core.civil.society.lag
cbps.out <- CBPS(jureform.lag~ democracy.duration.lag + oppaut.lag + 
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


model.elirreg.negshockmajor.ccs <- lmer(v2elirreg.inv~jureform.lag +
                                          core.civil.society.lag + jureform.lag*core.civil.society.lag + 
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
p.cbps.irreg.ccs <- interplot(model.elirreg.negshockmajor.ccs, var1="jureform.lag", var2="core.civil.society.lag",
                              hist=TRUE) + theme_bw() + labs(x="Lagged core civil society index", y="Marginal effect",
                                                             title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 


###

model.elintim.negshockmajor.ccs <- lmer(v2elintim.inv~jureform.lag +
                                          core.civil.society.lag + jureform.lag*core.civil.society.lag + 
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
p.cbps.irreg.ccs <- interplot(model.elintim.negshockmajor.ccs, var1="jureform.lag", var2="core.civil.society.lag",
                              hist=TRUE) + theme_bw() + labs(x="Lagged core civil society index", y="Marginal effect",
                                                             title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2)   #No interaction









#oppaut.lag
cbps.out <- CBPS(jureform.lag~ democracy.duration.lag + oppaut.lag + e_peaveduc +
                   #e_van_comp.lag  + 
                   polity2.lag + #polcomp.lag + parcomp.lag +
                   loggpdpc.lag + 
                   e_miurbani +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 +transitional
                 + exec.respectcon.lag  +  altinfo.lag + 
                   + leg.constraints.lag
                 , data=dataset.matching.complete)
summary(cbps.out)
balance(cbps.out)


model.elirreg.negshockmajor.oppaut <- lmer(v2elirreg.inv~jureform.lag +
                                             oppaut.lag + jureform.lag*oppaut.lag 
                                           + elexec  + 
                                             #e_polity2 +
                                             e_migdppcln + 
                                             #e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                           , weights=cbps.out$weights,
                                           data = cbps.out$data) 
summary(model.elirreg.negshockmajor.oppaut)
p.cbps.irreg.oppaut <- interplot(model.elirreg.negshockmajor.oppaut, var1="jureform.lag", var2="oppaut.lag",
                                 hist=TRUE) + theme_bw() + labs(x="Opposition autonomy lagged", y="Marginal effect",
                                                                title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2)  #Positive significant: negative shock increases irreg at high oppaut

###

model.elintim.negshockmajor.oppaut <- lmer(v2elintim.inv~jureform.lag +
                                             oppaut.lag + jureform.lag*oppaut.lag 
                                           + elexec  + 
                                             #e_polity2 +
                                             e_migdppcln + 
                                             #e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                           , weights=cbps.out$weights,
                                           data = cbps.out$data) 
summary(model.elintim.negshockmajor.oppaut)
p.cbps.irreg.oppaut <- interplot(model.elintim.negshockmajor.oppaut, var1="jureform.lag", var2="oppaut.lag",
                                 hist=TRUE) + theme_bw() + labs(x="Opposition autonomy lagged", y="Marginal effect",
                                                                title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2)  #Positive significant: negative shock increases intimidation at high oppaut


#opposition.oversight.lag
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jureform.lag", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "opposition.oversight.lag")  #, "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)

cbps.out <- CBPS(jureform.lag~ democracy.duration.lag + #oppaut.lag + 
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
balance(cbps.out)


model.elirreg.negshockmajor.oppover <- lmer(v2elirreg.inv~jureform.lag +
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
summary(model.elirreg.negshockmajor.oppover)
p.cbps.irreg.oppover <- interplot(model.elirreg.negshockmajor.oppover, var1="jureform.lag", var2="opposition.oversight.lag",
                                  hist=TRUE) + theme_bw() + labs(x="Opposition oversight lagged", y="Marginal effect",
                                                                 title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2)    #Negative sig


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


#leg.investigates.lag
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jureform.lag", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "leg.investigates.lag")  #, "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)

cbps.out <- CBPS(jureform.lag~ democracy.duration.lag + oppaut.lag + 
                   e_peaveduc +
                   #e_van_comp.lag  + 
                   polity2.lag + #polcomp.lag + parcomp.lag +
                   loggpdpc.lag + 
                   e_miurbani +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 +transitional
                 + exec.respectcon.lag  +  altinfo.lag + 
                   + leg.constraints.lag  + leg.investigates.lag
                 , data=dataset.matching.complete)
summary(cbps.out)
balance(cbps.out)

model.elirreg.negshockmajor.leginvest <- lmer(v2elirreg.inv~jureform.lag +
                                                leg.investigates.lag + jureform.lag*leg.investigates.lag   + elexec  + 
                                                #e_polity2 +
                                                e_migdppcln + 
                                                #e_miurbani + 
                                                v2eldommon + 
                                                log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                              , weights=cbps.out$weights,
                                              data = cbps.out$data) 
summary(model.elirreg.negshockmajor.leginvest)
p.cbps.irreg.leginvest <- interplot(model.elirreg.negshockmajor.leginvest, var1="jureform.lag", var2="leg.investigates.lag",
                                    hist=TRUE) + theme_bw() + labs(x="Lagged legislative investigation", y="Marginal effect",
                                                                   title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2)  #Negative significant


###

model.elintim.negshockmajor.leginvest <- lmer(v2elintim.inv~jureform.lag +
                                                leg.investigates.lag + jureform.lag*leg.investigates.lag   + elexec  + 
                                                #e_polity2 +
                                                e_migdppcln + 
                                                #e_miurbani + 
                                                v2eldommon + 
                                                log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                              , weights=cbps.out$weights,
                                              data = cbps.out$data) 
summary(model.elintim.negshockmajor.leginvest)
p.cbps.intim.leginvest <- interplot(model.elintim.negshockmajor.leginvest, var1="jureform.lag", var2="leg.investigates.lag",
                                    hist=TRUE) + theme_bw() + labs(x="Lagged legislative investigation", y="Marginal effect",
                                                                   title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2)  #No real interaction


#e_van_comp
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "e_van_comp.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jureform.lag", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "e_van_comp.lag")  #, "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)

cbps.out <- CBPS(jureform.lag~ democracy.duration.lag + #oppaut.lag + 
                   e_peaveduc +
                   e_van_comp.lag  + 
                   polity2.lag + #polcomp.lag + parcomp.lag +
                   loggpdpc.lag + 
                   e_miurbani +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 +transitional
                 + exec.respectcon.lag  +  altinfo.lag + 
                   + leg.constraints.lag  
                 , data=dataset.matching.complete)
summary(cbps.out)
balance(cbps.out)

model.elirreg.negshockmajor.comp <- lmer(v2elirreg.inv~jureform.lag +
                                           e_van_comp.lag + jureform.lag*e_van_comp.lag   + elexec  + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                         , weights=cbps.out$weights,
                                         data = cbps.out$data) 
summary(model.elirreg.negshockmajor.comp)
p.cbps.irreg.comp <- interplot(model.elirreg.negshockmajor.comp, var1="jureform.lag", var2="e_van_comp.lag",
                               hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect",
                                                              title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) #No interaction


##comp subset, no 0s or 70s
dataset.matching.complete.b <- subset(dataset.matching.complete, dataset.matching.complete$e_van_comp.lag > 0 &
                                        dataset.matching.complete$e_van_comp.lag < 70)

cbps.out <- CBPS(jureform.lag~ democracy.duration.lag + #oppaut.lag + 
                   e_peaveduc +
                   e_van_comp.lag  + 
                   polity2.lag + #polcomp.lag + parcomp.lag +
                   loggpdpc.lag + 
                   e_miurbani +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 +transitional
                 + exec.respectcon.lag  +  altinfo.lag + 
                   + leg.constraints.lag  
                 , data=dataset.matching.complete.b)
summary(cbps.out)
balance(cbps.out)

model.elirreg.negshockmajor.comp <- lmer(v2elirreg.inv~jureform.lag +
                                           e_van_comp.lag + jureform.lag*e_van_comp.lag   + elexec  + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                         , weights=cbps.out$weights,
                                         data = cbps.out$data) 
summary(model.elirreg.negshockmajor.comp)
p.cbps <- interplot(model.elirreg.negshockmajor.comp, var1="jureform.lag", var2="e_van_comp.lag",
                    hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect",
                                                   title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 





##parcomp.lag

myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jureform.lag", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "parcomp.lag")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]

dataset.matching$parcomp.lag2 <- dataset.matching$parcomp.lag
dataset.matching$parcomp.lag2[vdem.nodems$parcomp.lag == -66 |
                                vdem.nodems$parcomp.lag == -77 |
                                vdem.nodems$parcomp.lag == -88 ] <- 0

dataset.matching.complete <- na.omit(dataset.matching)


cbps.out <- CBPS(jureform.lag~ democracy.duration.lag + oppaut.lag +
                   e_peaveduc +
                   #e_van_comp.lag  + 
                   polity2.lag + #polcomp.lag 
                   + parcomp.lag2 +
                   loggpdpc.lag + 
                   e_miurbani +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 +transitional
                 + exec.respectcon.lag  +  altinfo.lag + 
                   + leg.constraints.lag, data=dataset.matching.complete)
summary(cbps.out)
balance(cbps.out)


model.elirreg.negshockmajor.parcomp <- lmer(v2elirreg.inv~jureform.lag +
                                              parcomp.lag2 + jureform.lag*parcomp.lag2 + elexec  + 
                                              #e_polity2 +
                                              e_migdppcln + 
                                              #e_miurbani + 
                                              v2eldommon + 
                                              log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                            , weights=cbps.out$weights,
                                            data = cbps.out$data) 
summary(model.elirreg.negshockmajor.parcomp)
p.cbps.irreg.parcomp <- interplot(model.elirreg.negshockmajor.parcomp, var1="jureform.lag", var2="parcomp.lag2",
                                  hist=TRUE) + theme_bw() + labs(x="Lagged 'Polity'PARCOMP' score", y="Marginal effect",
                                                                 title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 


###

model.elintim.negshockmajor.parcomp <- lmer(v2elintim.inv~jureform.lag +
                                              parcomp.lag2 + jureform.lag*parcomp.lag2 + elexec  + 
                                              #e_polity2 +
                                              e_migdppcln + 
                                              #e_miurbani + 
                                              v2eldommon + 
                                              log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                            , weights=cbps.out$weights,
                                            data = cbps.out$data) 
summary(model.elintim.negshockmajor.parcomp)
p.cbps.intim.parcomp <- interplot(model.elintim.negshockmajor.parcomp, var1="jureform.lag", var2="parcomp.lag2",
                                  hist=TRUE) + theme_bw() + labs(x="Lagged 'Polity'PARCOMP' score", y="Marginal effect",
                                                                 title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 







##Opposition oversight
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jureform.lag", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "opposition.oversight.lag", "years.since.election")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)

cbps.out <- CBPS(jureform.lag~ democracy.duration.lag + oppaut.lag +
                   e_peaveduc +
                   #e_van_comp.lag  + 
                   #polcomp.lag2 + #polcomp.lag 
                   + polity2.lag +
                   loggpdpc.lag + 
                   e_miurbani +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 +transitional
                 + exec.respectcon.lag  +  altinfo.lag + 
                   + opposition.oversight.lag , data=dataset.matching.complete)
summary(cbps.out)
qchisq(.95, df=12+2)  #J-statistic should be less than the .95 quantile of the chi-squared dist,
#with df = #of parameters + # of moment conditions (two)
balance(cbps.out)
stargazer(balance(cbps.out), type="html", 
          out="C:/Users/Cole/Dropbox/Judicial independence project/cbps balance oversight.html")



model.elirreg.negshockmajor.oversight <- lmer(v2elirreg.inv~jureform.lag +
                                                opposition.oversight.lag + jureform.lag*opposition.oversight.lag + elexec  + 
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
                                              + opposition.oversight.lag +
                                                #  e_miurbani + 
                                                v2eldommon + 
                                                log(e_mipopula) + years.since.election + (1 | COWcode)# + transitional + altinfo.lag
                                              , weights=cbps.out$weights,
                                              data = cbps.out$data) 
summary(model.elirreg.negshockmajor.oversight)
p.cbps.irreg.oppover <- interplot(model.elirreg.negshockmajor.oversight, var1="jureform.lag", var2="opposition.oversight.lag",
                                  hist=TRUE) + theme_bw() + labs(x="Opposition oversight", y="Marginal effect",
                                                                 title="Marginal Effect of Negative JI Shock \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 

stargazer(model.elirreg.negshockmajor.ssdiff, type="html", 
          out="C:/Users/Cole/Dropbox/Judicial independence project/cbps outcome oversight irreg.html")

png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/cbps marginal effects oversight.png", height=5,
    width=7, units="in", res=300)
p.cbps.irreg.oppover
dev.off()

###

model.elintim.negshockmajor.oversight <- lmer(v2elintim.inv~jureform.lag +
                                                opposition.oversight.lag + jureform.lag*opposition.oversight.lag + elexec  + 
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
                                              + opposition.oversight.lag +
                                                #  e_miurbani + 
                                                v2eldommon + 
                                                log(e_mipopula) + years.since.election + (1 | COWcode)# + transitional + altinfo.lag
                                              , weights=cbps.out$weights,
                                              data = cbps.out$data) 
summary(model.elintim.negshockmajor.oversight)
p.cbps.intim.oppover <- interplot(model.elintim.negshockmajor.oversight, var1="jureform.lag", var2="opposition.oversight.lag",
                                  hist=TRUE) + theme_bw() + labs(x="Opposition oversight", y="Marginal effect",
                                                                 title="Marginal Effect of Negative JI Shock \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 

stargazer(model.elintim.negshockmajor.oversight, type="html", 
          out="C:/Users/Cole/Dropbox/Judicial independence project/cbps outcome oversight intim.html")

png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/cbps marginal effects oversight.png", height=5,
    width=7, units="in", res=300)
p.cbps.intim.oppover
dev.off()




##ss.diff.lag
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jureform.lag", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "ss.diff.lag", "years.since.election")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)
dataset.matching.complete$ss.diff.lag.inv <- 100 - dataset.matching.complete$ss.diff.lag

cbps.out <- CBPS(jureform.lag~ democracy.duration.lag + oppaut.lag +
                   e_peaveduc +
                   #e_van_comp.lag  + 
                   #polcomp.lag2 + #polcomp.lag 
                   + polity2.lag +
                   loggpdpc.lag + 
                   e_miurbani +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 +transitional
                 + exec.respectcon.lag  +  altinfo.lag + 
                   + ss.diff.lag.inv , data=dataset.matching.complete)
summary(cbps.out)
qchisq(.95, df=12+2)  #J-statistic should be less than the .95 quantile of the chi-squared dist,
#with df = #of parameters + # of moment conditions (two)
balance(cbps.out)
stargazer(balance(cbps.out), type="html", 
          out="C:/Users/Cole/Dropbox/Judicial independence project/cbps balance seatshare.html")



model.elirreg.negshockmajor.ssdiff <- lmer(v2elirreg.inv~jureform.lag +
                                             ss.diff.lag.inv + jureform.lag*ss.diff.lag.inv + elexec  + 
                                             #  e_polity2 +
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
                                           + 
                                             #  e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) + years.since.election + (1 | COWcode)# + transitional + altinfo.lag
                                           , weights=cbps.out$weights,
                                           data = cbps.out$data) 
summary(model.elirreg.negshockmajor.ssdiff)
p.cbps.irreg.ss <- interplot(model.elirreg.negshockmajor.ssdiff, var1="jureform.lag", var2="ss.diff.lag.inv",
                             hist=TRUE) + theme_bw() + labs(x="Difference in lower-chamber seat-shares, first and second parties", y="Marginal effect",
                                                            title="Marginal Effect of Negative JI Shock \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 

stargazer(model.elirreg.negshockmajor.ssdiff, type="html", 
          out="C:/Users/Cole/Dropbox/Judicial independence project/cbps outcome seatshare irreg.html")

png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/cbps marginal effects ss irreg.png", height=5,
    width=7, units="in", res=300)
p.cbps.irreg.ss
dev.off()


###

model.elintim.negshockmajor.ssdiff <- lmer(v2elintim.inv~jureform.lag +
                                             ss.diff.lag.inv + jureform.lag*ss.diff.lag.inv + elexec  + 
                                             #  e_polity2 +
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
                                           + 
                                             #  e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) + years.since.election + (1 | COWcode)# + transitional + altinfo.lag
                                           , weights=cbps.out$weights,
                                           data = cbps.out$data) 
summary(model.elintim.negshockmajor.ssdiff)
p.cbps.intim.ss <- interplot(model.elintim.negshockmajor.ssdiff, var1="jureform.lag", var2="ss.diff.lag.inv",
                             hist=TRUE) + theme_bw() + labs(x="Difference in lower-chamber seat-shares, first and second parties", y="Marginal effect",
                                                            title="Marginal Effect of Negative JI Shock \non Intimidation") +
  geom_hline(yintercept=0, linetype=2) 

stargazer(model.elintim.negshockmajor.ssdiff, type="html", 
          out="C:/Users/Cole/Dropbox/Judicial independence project/cbps outcome seatshare intim.html")

png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/cbps marginal effects ss irreg.png", height=5,
    width=7, units="in", res=300)
p.cbps.intim.ss
dev.off()

#####
#Entropy balancing

###Entropy balancing using posreform.lag as treatment (i.e. and increase in JI)
library(ebal)

myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "posreform.lag", "e_migdppcln", "loggpdpc.lag",
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

ebal.test <- ebalance(Treatment=dataset.matching.complete$posreform.lag, X = ebal.covariates[c(-15, -16)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==0,],2,mean)
library(stargazer)
stargazer(cbind(t.means, c.means.bal, c.means), type="html", 
          out="C:/Users/Cole/Dropbox/Judicial independence project/ebal adjusted means full data.html")



dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$posreform.lag == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$posreform.lag == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
elirreg.negshockmajor.ssdiff.base <- lmer(v2elirreg.inv~posreform.lag +
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


mm.elirreg.negshockmajor.ssdiff.all <- lmer(v2elirreg.inv~posreform.lag +
                                              ss.diff.lag.inv + posreform.lag*ss.diff.lag.inv   + elexec + 
                                              # e_polity2 +
                                              e_migdppcln + 
                                              #e_miurbani + 
                                              v2eldommon + 
                                              log(e_mipopula) #+ transitional + altinfo.lag 
                                            + (1 | COWcode), REML=FALSE, 
                                            weights=ebal.test.w,
                                            data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.ssdiff.all)
p2 <- interplot(mm.elirreg.negshockmajor.ssdiff.all, var1="posreform.lag", var2="ss.diff.lag.inv",
                hist=TRUE) + theme_bw() + labs(x="Prior election seat-share margin (inverse)", y="Marginal effect" , 
                                               title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2)   #No effect for fraud


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

elintim.negshockmajor.ssdiff.base <- lmer(v2elintim.inv~posreform.lag +
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


mm.elintim.negshockmajor.ssdiff.all <- lmer(v2elintim.inv~posreform.lag +
                                              ss.diff.lag.inv + posreform.lag*ss.diff.lag.inv   + elexec + 
                                              # e_polity2 +
                                              e_migdppcln + 
                                              #e_miurbani + 
                                              v2eldommon + 
                                              log(e_mipopula) #+ transitional + altinfo.lag 
                                            + (1 | COWcode), REML=FALSE, 
                                            weights=ebal.test.w,
                                            data = dataset.matching.complete.w)
summary(mm.elintim.negshockmajor.ssdiff.all)
p2.intim <- interplot(mm.elintim.negshockmajor.ssdiff.all, var1="posreform.lag", var2="ss.diff.lag.inv",
                      hist=TRUE) + theme_bw() + labs(x="Prior election seat-share margin (inverse)", y="Marginal effect" , 
                                                     title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2)  #Boost in JI reduces intimidation in less comp areas


png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/ebal seat share intim.png", height=5,
    width=7, units="in", res=300)
p2.intim
dev.off()

lrtest(elintim.negshockmajor.ssdiff.base, mm.elintim.negshockmajor.ssdiff.all)



##Core civil society


myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "posreform.lag", "e_migdppcln", "loggpdpc.lag",
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

ebal.test <- ebalance(Treatment=dataset.matching.complete$posreform.lag, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$posreform.lag == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$posreform.lag == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
elirreg.negshockmajor.civil.base <- lmer(v2elirreg.inv~posreform.lag +
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


mm.elirreg.negshockmajor.civil.all <- lmer(v2elirreg.inv~posreform.lag +
                                             core.civil.society.lag + posreform.lag*core.civil.society.lag   + elexec + 
                                             # e_polity2 +
                                             e_migdppcln + 
                                             #e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) #+ transitional + altinfo.lag 
                                           + (1 | COWcode), REML=FALSE, 
                                           weights=ebal.test.w,
                                           data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.civil.all)
p3 <- interplot(mm.elirreg.negshockmajor.civil.all, var1="posreform.lag", var2="core.civil.society.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged civil society index", y="Marginal effect" , 
                                               title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) #In low CS openness areas, JI boost reduces fraud


png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/ebal core civil society irreg.png", height=5,
    width=7, units="in", res=300)
p3
dev.off()

lrtest(elirreg.negshockmajor.civil.base, mm.elirreg.negshockmajor.civil.all)


###

elintim.negshockmajor.civil.base <- lmer(v2elintim.inv~posreform.lag +
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


mm.elintim.negshockmajor.civil.all <- lmer(v2elintim.inv~posreform.lag +
                                             core.civil.society.lag + posreform.lag*core.civil.society.lag   + elexec + 
                                             # e_polity2 +
                                             e_migdppcln + 
                                             #e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) #+ transitional + altinfo.lag 
                                           + (1 | COWcode), REML=FALSE, 
                                           weights=ebal.test.w,
                                           data = dataset.matching.complete.w)
summary(mm.elintim.negshockmajor.civil.all)
p3.intim <- interplot(mm.elintim.negshockmajor.civil.all, var1="posreform.lag", var2="core.civil.society.lag",
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
            "country_name", "posreform.lag", "e_migdppcln", "loggpdpc.lag",
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

ebal.test <- ebalance(Treatment=dataset.matching.complete$posreform.lag, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$posreform.lag == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$posreform.lag == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
elirreg.negshockmajor.oversight.base <- lmer(v2elirreg.inv~posreform.lag +
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


mm.elirreg.negshockmajor.oversight <- lmer(v2elirreg.inv~posreform.lag +
                                             opposition.oversight.lag + posreform.lag*opposition.oversight.lag   + elexec + 
                                             # e_polity2 +
                                             e_migdppcln + 
                                             #e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) #+ transitional + altinfo.lag 
                                           + (1 | COWcode), REML=FALSE, 
                                           weights=ebal.test.w,
                                           data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.oversight)
p4 <- interplot(mm.elirreg.negshockmajor.oversight, var1="posreform.lag", var2="opposition.oversight.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged opposition oversight", y="Marginal effect" , 
                                               title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2)   #When opp oversight is low, a positive reform reduces fraud

png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/ebal oversight.png", height=5,
    width=7, units="in", res=300)
p4
dev.off()

lrtest(elirreg.negshockmajor.oversight.base, mm.elirreg.negshockmajor.oversight)

###

elintim.negshockmajor.oversight.base <- lmer(v2elintim.inv~posreform.lag +
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


mm.elintim.negshockmajor.oversight <- lmer(v2elintim.inv~posreform.lag +
                                             opposition.oversight.lag + posreform.lag*opposition.oversight.lag   + elexec + 
                                             # e_polity2 +
                                             e_migdppcln + 
                                             #e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) #+ transitional + altinfo.lag 
                                           + (1 | COWcode), REML=FALSE, 
                                           weights=ebal.test.w,
                                           data = dataset.matching.complete.w)
summary(mm.elintim.negshockmajor.oversight)
p4.intim <- interplot(mm.elintim.negshockmajor.oversight, var1="posreform.lag", var2="opposition.oversight.lag",
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
            "country_name", "posreform.lag", "e_migdppcln", "loggpdpc.lag",
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

ebal.test <- ebalance(Treatment=dataset.matching.complete$posreform.lag, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$posreform.lag == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$posreform.lag == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
ols.elirreg.negshockmajor.comp.all <- lm(v2elirreg.inv~posreform.lag +
                                           polcomp.lag2 + posreform.lag*polcomp.lag2 + elexec + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) 
                                         #+ transitional + altinfo.lag
                                         , weights=ebal.test.w,
                                         data = dataset.matching.complete.w)
summary(ols.elirreg.negshockmajor.comp.all)
p1 <- interplot(ols.elirreg.negshockmajor.comp.all, var1="posreform.lag", var2="polcomp.lag2",
                hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect") + geom_hline(yintercept=0, linetype=2) 


mm.elirreg.negshockmajor.polcomp <- lmer(v2elirreg.inv~posreform.lag +
                                           polcomp.lag2 + posreform.lag*polcomp.lag2   + elexec + 
                                           # e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) #+ transitional + altinfo.lag 
                                         + (1 | COWcode), REML=FALSE, 
                                         weights=ebal.test.w,
                                         data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.polcomp)
p.polcomp <- interplot(mm.elirreg.negshockmajor.polcomp, var1="posreform.lag", var2="polcomp.lag2",
                       hist=TRUE) + theme_bw() + labs(x="Lagged political competition", y="Marginal effect" , 
                                                      title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) 



### State ownership
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "posreform.lag", "e_migdppcln", "loggpdpc.lag",
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

ebal.test <- ebalance(Treatment=dataset.matching.complete$posreform.lag, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$posreform.lag == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$posreform.lag == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
ols.elirreg.negshockmajor.comp.all <- lm(v2elirreg.inv~posreform.lag +
                                           state.ownership.lag + posreform.lag*state.ownership.lag + elexec + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) 
                                         #+ transitional + altinfo.lag
                                         , weights=ebal.test.w,
                                         data = dataset.matching.complete.w)
summary(ols.elirreg.negshockmajor.comp.all)
p1 <- interplot(ols.elirreg.negshockmajor.comp.all, var1="posreform.lag", var2="state.ownership.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect") + geom_hline(yintercept=0, linetype=2) 


mm.elirreg.negshockmajor.stateown <- lmer(v2elirreg.inv~posreform.lag +
                                            state.ownership.lag + posreform.lag*state.ownership.lag   + elexec + 
                                            # e_polity2 +
                                            e_migdppcln + 
                                            #e_miurbani + 
                                            v2eldommon + 
                                            log(e_mipopula) #+ transitional + altinfo.lag 
                                          + (1 | COWcode), REML=FALSE, 
                                          weights=ebal.test.w,
                                          data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.stateown)
p.stateowned <- interplot(mm.elirreg.negshockmajor.stateown, var1="posreform.lag", var2="state.ownership.lag",
                          hist=TRUE) + theme_bw() + labs(x="State ownership of the economy (lagged)", y="Marginal effect" , 
                                                         title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) 


###daigonal accountability
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "posreform.lag", "e_migdppcln", "loggpdpc.lag",
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

ebal.test <- ebalance(Treatment=dataset.matching.complete$posreform.lag, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$posreform.lag == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$posreform.lag == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
ols.elirreg.negshockmajor.comp.all <- lm(v2elirreg.inv~posreform.lag +
                                           diagacc.lag + posreform.lag*diagacc.lag + elexec + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) 
                                         #+ transitional + altinfo.lag
                                         , weights=ebal.test.w,
                                         data = dataset.matching.complete.w)
summary(ols.elirreg.negshockmajor.comp.all)
p1 <- interplot(ols.elirreg.negshockmajor.comp.all, var1="posreform.lag", var2="diagacc.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect") + geom_hline(yintercept=0, linetype=2) 


mm.elirreg.negshockmajor.diag <- lmer(v2elirreg.inv~posreform.lag +
                                        diagacc.lag + posreform.lag*diagacc.lag   + elexec + 
                                        # e_polity2 +
                                        e_migdppcln + 
                                        #e_miurbani + 
                                        v2eldommon + 
                                        log(e_mipopula) #+ transitional + altinfo.lag 
                                      + (1 | COWcode), REML=FALSE, 
                                      weights=ebal.test.w,
                                      data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.diag)
p.diag <- interplot(mm.elirreg.negshockmajor.diag, var1="posreform.lag", var2="diagacc.lag",
                    hist=TRUE) + theme_bw() + labs(x="Diagonal accountability (lagged)", y="Marginal effect" , 
                                                   title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) 


  ###

mm.elintim.negshockmajor.diag <- lmer(v2elintim.inv~posreform.lag +
                                        diagacc.lag + posreform.lag*diagacc.lag   + elexec + 
                                        # e_polity2 +
                                        e_migdppcln + 
                                        #e_miurbani + 
                                        v2eldommon + 
                                        log(e_mipopula) #+ transitional + altinfo.lag 
                                      + (1 | COWcode), REML=FALSE, 
                                      weights=ebal.test.w,
                                      data = dataset.matching.complete.w)
summary(mm.elintim.negshockmajor.diag)
p.diag.intim <- interplot(mm.elintim.negshockmajor.diag, var1="posreform.lag", var2="diagacc.lag",
                    hist=TRUE) + theme_bw() + labs(x="Diagonal accountability (lagged)", y="Marginal effect" , 
                                                   title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) 




###parreg2

myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "posreform.lag", "e_migdppcln", "loggpdpc.lag",
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

ebal.test <- ebalance(Treatment=dataset.matching.complete$posreform.lag, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$posreform.lag == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$posreform.lag == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
ols.elirreg.negshockmajor.comp.all <- lm(v2elirreg.inv~posreform.lag +
                                           parreg2.lag + posreform.lag*parreg2.lag + elexec + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) 
                                         #+ transitional + altinfo.lag
                                         , weights=ebal.test.w,
                                         data = dataset.matching.complete.w)
summary(ols.elirreg.negshockmajor.comp.all)
p1 <- interplot(ols.elirreg.negshockmajor.comp.all, var1="posreform.lag", var2="opposition.oversight.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect") + geom_hline(yintercept=0, linetype=2) 


mm.elirreg.negshockmajor.comp.all <- lmer(v2elirreg.inv~posreform.lag +
                                            parreg2.lag + posreform.lag*parreg2.lag   + elexec + 
                                            # e_polity2 +
                                            e_migdppcln + 
                                            #e_miurbani + 
                                            v2eldommon + 
                                            log(e_mipopula) #+ transitional + altinfo.lag 
                                          + (1 | COWcode), REML=FALSE, 
                                          weights=ebal.test.w,
                                          data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.comp.all)
p2 <- interplot(mm.elirreg.negshockmajor.comp.all, var1="posreform.lag", var2="parreg2.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect" , 
                                               title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) 


###parcomp
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "v2elintim.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "posreform.lag", "e_migdppcln", "loggpdpc.lag",
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

ebal.test <- ebalance(Treatment=dataset.matching.complete$posreform.lag, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$posreform.lag==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$posreform.lag == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$posreform.lag == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
ols.elirreg.negshockmajor.comp.all <- lm(v2elirreg.inv~posreform.lag +
                                           parcomp2.lag + posreform.lag*parcomp2.lag + elexec + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) 
                                         #+ transitional + altinfo.lag
                                         , weights=ebal.test.w,
                                         data = dataset.matching.complete.w)
summary(ols.elirreg.negshockmajor.comp.all)
p1 <- interplot(ols.elirreg.negshockmajor.comp.all, var1="posreform.lag", var2="opposition.oversight.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect") + geom_hline(yintercept=0, linetype=2) 


mm.elirreg.negshockmajor.comp.all <- lmer(v2elirreg.inv~posreform.lag +
                                            parcomp2.lag + posreform.lag*parcomp2.lag   + elexec + 
                                            # e_polity2 +
                                            e_migdppcln + 
                                            #e_miurbani + 
                                            v2eldommon + 
                                            log(e_mipopula) #+ transitional + altinfo.lag 
                                          + (1 | COWcode), REML=FALSE, 
                                          weights=ebal.test.w,
                                          data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.comp.all)
p2 <- interplot(mm.elirreg.negshockmajor.comp.all, var1="posreform.lag", var2="parcomp2.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect" , 
                                               title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) 


###Appendix table and plots
library(stargazer)
stargazer(mm.elirreg.negshockmajor.polcomp, mm.elirreg.negshockmajor.diag, mm.elirreg.negshockmajor.stateown, 
          type="html", out="C:/Users/Cole/Dropbox/Judicial independence project/ebal appendix models.html")


png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/marginal effects diagonal accountability.png", height=5,
    width=7, units="in", res=300)
p.diag
dev.off()

png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/marginal effects state ownership.png", height=5,
    width=7, units="in", res=300)
p.stateowned
dev.off()

png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/marginal effects polcomp.png", height=5,
    width=7, units="in", res=300)
p.polcomp
dev.off()

###LJI and vdem only
####Adding LJI to vdem.nodems

judicial <- read.csv("C:/Users/Cole/Dropbox/TJ + elections/dataset + coding stuff/LJI-estimates-20140422.csv")
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


####Plot of lji.2lag and posreform.lag
p.levels <- qplot(lji.2lag, jitter(posreform.lag), data=vdem.nodems, xlab="Latent judicial independence (lagged)",
                  ylab="Major negative JI shock (jittered)", main="Judicial independence levels prior to negative shocks") +
  theme_bw()

png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/lji vs jindnegshock.png", height=5,
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
  labs(x="Opposition oversight (lagged)", y="Marginal effect", title="Marginal effect of latent judicial independence on intentional voting irregularities") + 
  theme_bw() + geom_hline(yintercept=0, linetype=2)

png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/lji x oppoversight.png", height=5,
    width=7, units="in", res=300)
p.lji.opp
dev.off()

stargazer(mm.elirreg.lji.opp.base, mm.elirreg.lji.opp, type="html", 
          out="C:/Users/Cole/Dropbox/Judicial independence project/table lji oversight.html")

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


####LJI by seat share, no balancing because not a binary treatment
vdem.nodems$ss.diff.lag.inv <- 100 - vdem.nodems$ss.diff.lag

mm.elirreg.lji.comp <- lmer(v2elirreg.inv~lji.lag +
                              ss.diff.lag.inv + lji.lag*ss.diff.lag.inv   + elexec +
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
interplot(mm.elirreg.lji.comp, var1="lji.lag", var2="ss.diff.lag.inv", hist=TRUE)