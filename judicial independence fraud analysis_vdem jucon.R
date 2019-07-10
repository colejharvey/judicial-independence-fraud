###Analysis script
###Vdem jucon measure (balancing)
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


###Un-processed data models
##Irregularities first, followed by govt intimidation


##Core civil society
mm.elirreg.base.raw <- lmer(v2elirreg.inv~vdem.jucon.lag + v2xcs_ccsi +
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

mm.elirreg.ccsi.raw <- lmer(v2elirreg.inv~vdem.jucon.lag + v2xcs_ccsi + 
                              vdem.jucon.lag:v2xcs_ccsi +
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
interplot(mm.elirreg.ccsi.raw, var1="vdem.jucon.lag", var2="v2xcs_ccsi")  #No interaction

####

mm.elintim.base.raw <- lmer(v2elintim.inv~vdem.jucon.lag + v2xcs_ccsi +
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

mm.elintim.ccsi.raw <- lmer(v2elintim.inv~vdem.jucon.lag + v2xcs_ccsi + 
                              vdem.jucon.lag:v2xcs_ccsi +
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
interplot(mm.elintim.ccsi.raw, var1="vdem.jucon.lag", var2="v2xcs_ccsi") #When CC independence is high, more JI = less intim








##Lagged lower house seat second place share 
mm.elirreg.base.raw <- lmer(v2elirreg.inv~vdem.jucon.lag + lowchamb.second.seatshare.lag +
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

mm.elirreg.ss.raw <- lmer(v2elirreg.inv~vdem.jucon.lag + lowchamb.second.seatshare.lag + 
                            vdem.jucon.lag:lowchamb.second.seatshare.lag +
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

  ###

mm.elintim.base.raw <- lmer(v2elintim.inv~vdem.jucon.lag + lowchamb.second.seatshare.lag +
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

mm.elintim.ss.raw <- lmer(v2elintim.inv~vdem.jucon.lag + lowchamb.second.seatshare.lag + 
                            vdem.jucon.lag:lowchamb.second.seatshare.lag +
                            elexec + #e_peaveduc+ 
                            e_polity2 +
                            e_migdppcln + 
                            e_miurbani + 
                            v2eldommon + 
                            log(e_mipopula) 
                          + transitional + altinfo.lag
                          
                          + (1 | COWcode), REML=FALSE,
                          data = vdem.nodems)
summary(mm.elintim.ss.raw)

lrtest(mm.elintim.base.raw, mm.elintim.ss.raw)

##Engaged society
mm.elirreg.base.raw <- lmer(v2elirreg.inv~vdem.jucon.lag + engaged.society.lag +
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

mm.elirreg.engaged.raw <- lmer(v2elirreg.inv~vdem.jucon.lag + engaged.society.lag + 
                                 vdem.jucon.lag:engaged.society.lag +
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
interplot(mm.elirreg.engaged.raw, var1="vdem.jucon.lag", var2="engaged.society.lag") #No interaction effect

###

mm.elintim.base.raw <- lmer(v2elintim.inv~vdem.jucon.lag + engaged.society.lag +
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

mm.elintim.engaged.raw <- lmer(v2elintim.inv~vdem.jucon.lag + engaged.society.lag + 
                                 vdem.jucon.lag:engaged.society.lag +
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
interplot(mm.elintim.engaged.raw, var1="vdem.jucon.lag", var2="engaged.society.lag") #No convincing interaction



##State owernship of economy
mm.elirreg.base.raw <- lmer(v2elirreg.inv~vdem.jucon.lag + v2clstown +
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

mm.elirreg.stown.raw <- lmer(v2elirreg.inv~vdem.jucon.lag + v2clstown + 
                               vdem.jucon.lag:v2clstown +
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
interplot(mm.elirreg.stown.raw, var1="vdem.jucon.lag", var2="v2clstown")  #No interaction

###

mm.elintim.base.raw <- lmer(v2elintim.inv~vdem.jucon.lag + v2clstown +
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

mm.elintim.stown.raw <- lmer(v2elintim.inv~vdem.jucon.lag + v2clstown + 
                               vdem.jucon.lag:v2clstown +
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
interplot(mm.elintim.stown.raw, var1="vdem.jucon.lag", var2="v2clstown")  #High state ownership + JI = less violence




##Opposition oversight
mm.elirreg.base.raw <- lmer(v2elirreg.inv~vdem.jucon.lag + opposition.oversight.lag +
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

mm.elirreg.oppart.raw <- lmer(v2elirreg.inv~vdem.jucon.lag + opposition.oversight.lag + 
                                vdem.jucon.lag:opposition.oversight.lag +
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
interplot(mm.elirreg.oppart.raw, var1="vdem.jucon.lag", var2="opposition.oversight.lag")  #No interaction





##Comp
mm.elirreg.base.raw <- lmer(v2elirreg.inv~vdem.jucon.lag + e_van_comp.lag +
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

mm.elirreg.comp.raw <- lmer(v2elirreg.inv~vdem.jucon.lag + e_van_comp.lag + 
                              vdem.jucon.lag:e_van_comp.lag +
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
interplot(mm.elirreg.comp.raw, var1="vdem.jucon.lag", var2="e_van_comp.lag") #No interaction


####

mm.elintim.base.raw <- lmer(v2elintim.inv~vdem.jucon.lag + e_van_comp.lag +
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

mm.elintim.comp.raw <- lmer(v2elintim.inv~vdem.jucon.lag + e_van_comp.lag + 
                              vdem.jucon.lag:e_van_comp.lag +
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
interplot(mm.elintim.comp.raw, var1="vdem.jucon.lag", var2="e_van_comp.lag") #No interaction




##regtrans
mm.elirreg.base.raw <- lmer(v2elirreg.inv~vdem.jucon.lag + regtrans2.lag +
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

mm.elirreg.regtrans.raw <- lmer(v2elirreg.inv~vdem.jucon.lag + regtrans2.lag + 
                                  vdem.jucon.lag:regtrans2.lag +
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
interplot(mm.elirreg.regtrans.raw, var1="vdem.jucon.lag", var2="regtrans2.lag") #No interaction

###

mm.elintim.base.raw <- lmer(v2elintim.inv~vdem.jucon.lag + regtrans2.lag +
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

mm.elintim.regtrans.raw <- lmer(v2elintim.inv~vdem.jucon.lag + regtrans2.lag + 
                                  vdem.jucon.lag:regtrans2.lag +
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
interplot(mm.elintim.regtrans.raw, var1="vdem.jucon.lag", var2="regtrans2.lag") #No effect



##parcomp2

mm.elirreg.base.raw <- lmer(v2elirreg.inv~vdem.jucon.lag + parcomp2.lag +
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

mm.elirreg.parcomp.raw <- lmer(v2elirreg.inv~vdem.jucon.lag + parcomp2.lag + 
                                 vdem.jucon.lag:parcomp2.lag +
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
interplot(mm.elirreg.parcomp.raw, "vdem.jucon.lag", "parcomp2.lag")  #No interaction

####

mm.elintim.base.raw <- lmer(v2elintim.inv~vdem.jucon.lag + parcomp2.lag +
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

mm.elintim.parcomp.raw <- lmer(v2elintim.inv~vdem.jucon.lag + parcomp2.lag + 
                                 vdem.jucon.lag:parcomp2.lag +
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
interplot(mm.elintim.parcomp.raw, "vdem.jucon.lag", "parcomp2.lag")  #Higher parcomp + JI = less intim

##parreg

mm.elirreg.base.raw <- lmer(v2elirreg.inv~vdem.jucon.lag + as.factor(parreg2.lag) +
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

mm.elirreg.parreg.raw <- lmer(v2elirreg.inv~vdem.jucon.lag + as.factor(parreg2.lag) + 
                                vdem.jucon.lag:(as.factor(parreg2.lag)) +
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
interplot(mm.elirreg.parreg.raw, var2="vdem.jucon.lag", var1="parreg2.lag")

