###Analysis script
###Additive index of lagged HC and LC independence using VDEM measure
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


vdem.nodems$ji.addindex <- vdem.nodems$hc.ind.lag + vdem.nodems$lc.ind.lag

###Un-processed data models
##Irregularities first, followed by govt intimidation


##Core civil society
mm.elirreg.base.raw <- lmer(v2elirreg.inv~ji.addindex + v2xcs_ccsi +
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

mm.elirreg.ccsi.raw <- lmer(v2elirreg.inv~ji.addindex + v2xcs_ccsi + 
                              ji.addindex:v2xcs_ccsi +
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
interplot(mm.elirreg.ccsi.raw, var1="ji.addindex", var2="v2xcs_ccsi") #Independent CC + JI = less irreg

    ####

mm.elintim.base.raw <- lmer(v2elintim.inv~ji.addindex + v2xcs_ccsi +
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

mm.elintim.ccsi.raw <- lmer(v2elintim.inv~ji.addindex + v2xcs_ccsi + 
                              ji.addindex:v2xcs_ccsi +
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
interplot(mm.elintim.ccsi.raw, var1="ji.addindex", var2="v2xcs_ccsi") #When CC independence is high, more JI -> less intimidation






##Engaged society
mm.elirreg.base.raw <- lmer(v2elirreg.inv~ji.addindex + engaged.society.lag +
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

mm.elirreg.engaged.raw <- lmer(v2elirreg.inv~ji.addindex + engaged.society.lag + 
                                 ji.addindex:engaged.society.lag +
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
interplot(mm.elirreg.engaged.raw, var1="ji.addindex", var2="engaged.society.lag") #When engaged society is low, more independence = less irregularities

   ###

mm.elintim.base.raw <- lmer(v2elintim.inv~ji.addindex + engaged.society.lag +
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

mm.elintim.engaged.raw <- lmer(v2elintim.inv~ji.addindex + engaged.society.lag + 
                                 ji.addindex:engaged.society.lag +
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
interplot(mm.elintim.engaged.raw, var1="ji.addindex", var2="engaged.society.lag") #No interaction



##State owernship of economy
mm.elirreg.base.raw <- lmer(v2elirreg.inv~ji.addindex + v2clstown +
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

mm.elirreg.stown.raw <- lmer(v2elirreg.inv~ji.addindex + v2clstown + 
                               ji.addindex:v2clstown +
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
interplot(mm.elirreg.stown.raw, var1="ji.addindex", var2="v2clstown")  #High private enterprise + JI = less irreg (sort of)

   ###

mm.elintim.base.raw <- lmer(v2elintim.inv~ji.addindex + v2clstown +
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

mm.elintim.stown.raw <- lmer(v2elintim.inv~ji.addindex + v2clstown + 
                               ji.addindex:v2clstown +
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
interplot(mm.elintim.stown.raw, var1="ji.addindex", var2="v2clstown")  #Low state ownership + JI = less violence (sort of)




##Opposition oversight
mm.elirreg.base.raw <- lmer(v2elirreg.inv~ji.addindex + opposition.oversight.lag +
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

mm.elirreg.oppart.raw <- lmer(v2elirreg.inv~ji.addindex + opposition.oversight.lag + 
                                ji.addindex:opposition.oversight.lag +
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
interplot(mm.elirreg.oppart.raw, var1="ji.addindex", var2="opposition.oversight.lag")  #No interaction

  ###
mm.elintim.base.raw <- lmer(v2elintim.inv~ji.addindex + opposition.oversight.lag +
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

mm.elintim.oppart.raw <- lmer(v2elintim.inv~ji.addindex + opposition.oversight.lag + 
                                ji.addindex:opposition.oversight.lag +
                                elexec + #e_peaveduc+ 
                                e_polity2 +
                                e_migdppcln + 
                                e_miurbani + 
                                v2eldommon + 
                                log(e_mipopula) 
                              + transitional + altinfo.lag
                              
                              + (1 | COWcode), REML=FALSE,
                              data = vdem.nodems)
summary(mm.elintim.oppart.raw)

lrtest(mm.elintim.base.raw, mm.elintim.oppart.raw)
interplot(mm.elintim.oppart.raw, var1="ji.addindex", var2="opposition.oversight.lag")  #More oversight + JI = reduced violence



##Comp
mm.elirreg.base.raw <- lmer(v2elirreg.inv~ji.addindex + e_van_comp.lag +
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

mm.elirreg.comp.raw <- lmer(v2elirreg.inv~ji.addindex + e_van_comp.lag + 
                              ji.addindex:e_van_comp.lag +
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
interplot(mm.elirreg.comp.raw, var1="ji.addindex", var2="e_van_comp.lag") #No interaction


   ####

mm.elintim.base.raw <- lmer(v2elintim.inv~ji.addindex + e_van_comp.lag +
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

mm.elintim.comp.raw <- lmer(v2elintim.inv~ji.addindex + e_van_comp.lag + 
                              ji.addindex:e_van_comp.lag +
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
interplot(mm.elintim.comp.raw, var1="ji.addindex", var2="e_van_comp.lag") #No interaction




##regtrans
mm.elirreg.base.raw <- lmer(v2elirreg.inv~ji.addindex + regtrans2.lag +
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

mm.elirreg.regtrans.raw <- lmer(v2elirreg.inv~ji.addindex + regtrans2.lag + 
                                  ji.addindex:regtrans2.lag +
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
interplot(mm.elirreg.regtrans.raw, var1="ji.addindex", var2="regtrans2.lag") #No interaction

   ###

mm.elintim.base.raw <- lmer(v2elintim.inv~ji.addindex + regtrans2.lag +
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

mm.elintim.regtrans.raw <- lmer(v2elintim.inv~ji.addindex + regtrans2.lag + 
                                  ji.addindex:regtrans2.lag +
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
interplot(mm.elintim.regtrans.raw, var1="ji.addindex", var2="regtrans2.lag") #No interaction relationship



##parcomp2

mm.elirreg.base.raw <- lmer(v2elirreg.inv~ji.addindex + parcomp2.lag +
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

mm.elirreg.parcomp.raw <- lmer(v2elirreg.inv~ji.addindex + parcomp2.lag + 
                                 ji.addindex:parcomp2.lag +
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
interplot(mm.elirreg.parcomp.raw, "ji.addindex", "parcomp2.lag")  #Higher parcomp + JI = reduced irreg

####

mm.elintim.base.raw <- lmer(v2elintim.inv~ji.addindex + parcomp2.lag +
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

mm.elintim.parcomp.raw <- lmer(v2elintim.inv~ji.addindex + parcomp2.lag + 
                                 ji.addindex:parcomp2.lag +
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
interplot(mm.elintim.parcomp.raw, "ji.addindex", "parcomp2.lag")  #Unconvincing plot, but expected relationship

##parreg

mm.elirreg.base.raw <- lmer(v2elirreg.inv~ji.addindex + as.factor(parreg2.lag) +
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

mm.elirreg.parreg.raw <- lmer(v2elirreg.inv~ji.addindex + as.factor(parreg2.lag) + 
                                ji.addindex:(as.factor(parreg2.lag)) +
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
interplot(mm.elirreg.parreg.raw, "ji.addindex", "parreg2.lag")