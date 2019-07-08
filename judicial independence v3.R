library(lme4)
library(lmtest)
library(sandwich)
library(MASS)
library(ggplot2)
library(interplot)
library(MatchIt)
library(MatchingFrontier)
library(Zelig)

rm(list = setdiff(ls(), lsf.str())) #Remove all except functions

####Loading and merging data####
vdem <- read.csv("C:/Users/Cole/Documents/Grad school/Research topics/V-Dem/VDem7.1/Country_Year_V-Dem_other_CSV_v7.1/V-Dem-DS-CY+Others-v7.1.csv")

vdem.short <- subset(vdem, vdem$year >= 1943)
rm(vdem)


vdem.short$elexec <- 0
vdem.short$elexec[vdem.short$v2eltype_6 == 1 
                  | vdem.short$v2eltype_7 == 1] <- 1


vdem.short$v2elirreg.inv <- vdem.short$v2elirreg * -1
vdem.short$v2elintim.inv <- vdem.short$v2elintim * -1
vdem.short$v2elvotbuy.inv <- vdem.short$v2elvotbuy * -1

vdem.short$transitional <- NA

vdem.short$transitional[vdem.short$e_nelda_1_ex == "yes" | vdem.short$e_nelda_1_leg == "yes"] <- 1
vdem.short$transitional[vdem.short$e_nelda_1_ex == "no" | vdem.short$e_nelda_1_leg == "no" |
                          (vdem.short$e_nelda_1_ex == "" & vdem.short$e_nelda_1_leg == "") ] <- 0






myvars <- c("COWcode", "year", "transitional", "v2elvotbuy.inv", "v2elirreg.inv", "v2elintim.inv", "v2juncind", 
            "v2juhcind", "v2psoppaut", "e_van_comp", "elexec", "e_peaveduc", 
            "e_polity2", "e_migdppcln", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "elexec", "e_multiparty_elections", "v2jureform_ord", "v2jupurge", "v2jupurge_ord",
            "v2jupack", "v2jupack_ord", "e_democracy_duration", "e_regtrans", "v2exrescon", "v2xlg_legcon",
            "v2x_freexp_thick", "v2x_freexp", "v2xme_altinf", "e_polcomp", "e_parcomp",
            "v2x_frassoc_thick",  "v2xcs_ccsi", "v2xps_party", "v2x_civlib", "v2x_clpol", "v2x_diagacc", "v2ellocumul", "v2ellocons",
             "v2elparlel", "v2elloeldm", "v2ellovtlg", "v2ellovtsm", "v2ellostlg",
            "v2elloseat", "v2ellostsl", "v2ellostsm","v2ellostss", "v2eltvrig", "v2psbars", "v2lginvstp",
             "v2lgoppart", "v2dlengage", "v2clstown", "v2csantimv", "v2mecenefm", "v2mecenefi",
            "v2mecrit", "v2mebias", "e_parreg")  #v2csanmvch has subcodes (anti-system movement type)
vdem.small <- vdem.short[myvars]


vdem.small$jind.negshock <- NA
vdem.small$jind.negshock <- 0
vdem.small$jind.negshock[vdem.small$v2jureform_ord == 1 
                         # | vdem.small$v2jupurge == 0 
                         | vdem.small$v2jupurge_ord == 1 |
                           #  vdem.small$v2jupack == 0 | 
                           vdem.small$v2jupack_ord == 1] <- 1

vdem.small$jind.negshockmajor <- NA
vdem.small$jind.negshockmajor[vdem.small$v2jureform_ord == 1 | vdem.small$v2jureform_ord == 2 &  
                                vdem.small$v2jupurge_ord >= 1 &
                                vdem.small$v2jupack_ord >= 1 ] <- 0
vdem.small$jind.negshockmajor[vdem.small$v2jureform_ord == 0     #The judiciary's ability to control arbitrary power was reduced via institutional reform
                              | vdem.small$v2jupurge_ord < 1 |       # There was a massive, arbitrary purge of the judiciary
                                vdem.small$v2jupack_ord < 1 ] <- 1  #There was a massive, politically motivated increase in the number of judgeships
#across the entire judiciary

vdem.small$purge_major <- NA
vdem.small$purge_major[vdem.small$v2jupurge_ord == 3 | vdem.small$v2jupurge_ord == 4] <- 0
vdem.small$purge_major[vdem.small$v2jupurge_ord == 0 ] <- 1

vdem.small$pack_major <- NA
vdem.small$pack_major[vdem.small$v2jupack_ord > 0 ] <- 0
vdem.small$pack_major[vdem.small$v2jupack_ord == 0 ] <- 1

vdem.small$reform_negative <- NA
vdem.small$reform_negative[vdem.small$v2jureform_ord > 0] <- 0
vdem.small$reform_negative[vdem.small$v2jureform_ord == 0 ] <- 1

vdem.small$reform_positive <- NA
vdem.small$reform_positive[vdem.small$v2jureform_ord <= 1] <- 0
vdem.small$reform_positive[vdem.small$v2jureform_ord == 2 ] <- 1

write.csv(vdem.small, "C:/Users/Cole/Documents/Grad school/Research topics/V-Dem/vdem-small-2018-post1943.csv")

###Reading in vdem.small
vdem.small <- read.csv("C:/Users/Cole/Documents/Grad school/Research topics/V-Dem/vdem-small-2018-post1943.csv")


###Getting lagged data

vdem.small$jureform.lag <- NA
vdem.small$hc.ind.2lag <- NA
vdem.small$lc.ind.2lag <- NA
vdem.small$jupurge.lag <- NA
vdem.small$jupack.lag <- NA
vdem.small$jind.negshock.lag <- NA
vdem.small$jind.negshockmajor.lag <- NA
vdem.small$pack_major.lag <- NA
vdem.small$purge_major.lag <- NA
vdem.small$negreform.lag <- NA
vdem.small$posreform.lag <- NA

i <- 1

for(i in 1:nrow(vdem.small)){
  tryCatch({
    cow <- as.numeric(vdem.small$COWcode[i])
    group <- subset(vdem.small, vdem.small$COWcode == cow)
    current.year <- as.numeric(vdem.small$year[i])
    lag.year <- current.year - 1
    #if (group$year == loopyear) mergeddata$lji[i] <- group.year$LJI else mergeddata$lji <- NA
    group.year <- subset(group, group$year == lag.year)
    group.year2 <- subset(group, group$year == lag.year - 1)
    vdem.small$jureform.lag[i] <- group.year$v2jureform_ord
    vdem.small$hc.ind.2lag[i] <- group.year2$v2juncind
    vdem.small$lc.ind.2lag[i] <- group.year2$v2juhcind
    vdem.small$jupurge.lag[i] <- group.year$v2jupurge
    vdem.small$jupack.lag[i] <- group.year$v2jupack
    vdem.small$jind.negshock.lag[i] <- group.year$jind.negshock
    vdem.small$jind.negshockmajor.lag[i] <- group.year$jind.negshockmajor
    vdem.small$pack_major.lag[i] <- group.year$pack_major
    vdem.small$purge_major.lag[i] <- group.year$purge_major
    vdem.small$negreform.lag[i] <- group.year$reform_negative
    vdem.small$posreform.lag[i] <- group.year$reform_positive
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}   


vdem.small$hc.ind.lag <- NA
vdem.small$lc.ind.lag <- NA
vdem.small$polity2.lag <- NA
vdem.small$e_van_comp.lag <- NA
vdem.small$oppaut.lag <- NA
vdem.small$loggpdpc.lag <- NA
vdem.small$democracy.duration.lag <- NA
vdem.small$exec.respectcon.lag <- NA
vdem.small$leg.constraints.lag <- NA
vdem.small$freeexpressthick.lag <- NA
vdem.small$freeexpress.lag <- NA
vdem.small$altinfo.lag <- NA
vdem.small$parcomp.lag <- NA
vdem.small$polcomp.lag <- NA
vdem.small$regtrans.lag <- NA
vdem.small$parreg.lag <- NA


vdem.small$frassoc_thick.lag <- NA
vdem.small$core.civil.society.lag <- NA
vdem.small$party.institutionalization.lag <- NA
vdem.small$personal.civlib.lag <- NA
vdem.small$political.civlib.lag <- NA
vdem.small$diagacc.lag <- NA
vdem.small$electoralsystem.lag <- NA
vdem.small$districtmag.lag <- NA

vdem.small$leg.investigates.lag <- NA
vdem.small$opposition.oversight.lag <- NA
vdem.small$engaged.society.lag <- NA
vdem.small$state.ownership.lag <- NA
vdem.small$antisystem.lag <- NA
vdem.small$govcensorship.lag <- NA
vdem.small$internetcenorship.lag <- NA
vdem.small$critical.media.lag <- NA
vdem.small$media.bias.lag <- NA


i <- 1

for(i in 1:nrow(vdem.small)){
  tryCatch({
    cow <- as.numeric(vdem.small$COWcode[i])
    group <- subset(vdem.small, vdem.small$COWcode == cow)
    current.year <- as.numeric(vdem.small$year[i])
    lag.year <- current.year - 1
    #if (group$year == loopyear) mergeddata$lji[i] <- group.year$LJI else mergeddata$lji <- NA
    group.year <- subset(group, group$year == lag.year)
    vdem.small$polity2.lag[i] <- group.year$e_polity2
    vdem.small$lc.ind.lag[i] <- group.year$v2juncind
    vdem.small$hc.ind.lag[i] <- group.year$v2juhcind
    vdem.small$e_van_comp.lag[i] <- group.year$e_van_comp
    vdem.small$oppaut.lag[i] <- group.year$v2psoppaut
    vdem.small$loggpdpc.lag[i] <- as.numeric(group.year$e_migdppcln)
    vdem.small$democracy.duration.lag[i] <- as.numeric(group.year$e_democracy_duration)
    vdem.small$exec.respectcon.lag[i] <- group.year$v2exrescon
    vdem.small$leg.constraints.lag[i] <- group.year$v2xlg_legcon
    vdem.small$freeexpressthick.lag[i] <- group.year$v2x_freexp_thick
    vdem.small$freeexpress.lag[i] <- group.year$v2x_freexp
    vdem.small$altinfo.lag[i] <- group.year$v2xme_altinf
    vdem.small$parcomp.lag[i] <- group.year$e_parcomp 
    vdem.small$polcomp.lag[i] <- group.year$e_polcomp
    
    vdem.small$frassoc_thick.lag[i] <- group.year$v2x_frassoc_thick
    vdem.small$core.civil.society.lag[i]  <- group.year$v2xcs_ccsi 
    vdem.small$party.institutionalization.lag[i]  <- group.year$v2xps_party
    vdem.small$personal.civlib.lag[i] <- group.year$v2x_civlib
    vdem.small$political.civlib.lag[i] <- group.year$v2x_clpol
    vdem.small$diagacc.lag[i] <- group.year$v2x_diagacc
    vdem.small$electoralsystem.lag[i] <- group.year$v2elparlel
    vdem.small$districtmag.lag[i] <- group.year$v2elloeldm
    
    vdem.small$leg.investigates.lag[i] <- group.year$v2lginvstp
    vdem.small$opposition.oversight.lag[i] <- group.year$v2lgoppart
    vdem.small$engaged.society.lag[i] <- group.year$v2dlengage
    vdem.small$state.ownership.lag[i] <- group.year$v2clstown
    vdem.small$antisystem.lag[i] <- group.year$v2csantimv
    vdem.small$govcensorship.lag[i] <- group.year$v2mecenefm
    vdem.small$internetcenorship.lag[i] <- group.year$v2mecenefi
    vdem.small$critical.media.lag[i] <- group.year$v2mecrit
    vdem.small$media.bias.lag[i] <- group.year$v2mebias
    vdem.small$regtrans.lag[i] <- group.year$e_regtrans
    vdem.small$parreg.lag[i] <- group.year$e_parreg 
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}   

###Code for getting prior election results
vdem.small$lowchamb.largest.voteshare.lag <- NA
vdem.small$lowchamb.second.voteshare.lag <- NA
vdem.small$lowchamb.largest.seats.lag <- NA
vdem.small$lowchamb.seats.lag <- NA
vdem.small$lowchamb.seatshare.largest.lag <- NA
vdem.small$lowchamb.second.seats.lag <- NA
vdem.small$lowchamb.second.seatshare.lag <- NA
vdem.small$lowchamb.turnover.lag <- NA


###Getting largest party seat share in prior election
i <- 1

for(i in 1:nrow(vdem.small)){
  tryCatch({
    cow <- as.numeric(vdem.small$COWcode[i])
    group <- subset(vdem.small, vdem.small$COWcode == cow)
    current.year <- as.numeric(vdem.small$year[i])
    lag.year <- current.year - 1
    #if (group$year == loopyear) mergeddata$lji[i] <- group.year$LJI else mergeddata$lji <- NA
    group.year <- subset(group, group$year == lag.year)
    prior.elections <- subset(group, is.na(group$v2elirreg.inv) == FALSE & group$year < current.year)
    if (nrow(prior.elections) < 1) vdem.small$lowchamb.seatshare.largest.lag[i] <- NA 
    if (nrow(prior.elections) < 1) next
    else
      mostrecent.election <- subset(prior.elections, prior.elections$year == max(prior.elections$year))
    if (is.na(mostrecent.election$v2elirreg.inv) == TRUE) vdem.small$lowchamb.seatshare.largest.lag[i] <- NA
    else vdem.small$lowchamb.seatshare.largest.lag[i] <- mostrecent.election$v2ellostsl
  })
}


###Getting second largest party seats share in prior election
vdem.small$years.since.election <- NA
i <- 1

for(i in 1:nrow(vdem.small)){
  tryCatch({
    cow <- as.numeric(vdem.small$COWcode[i])
    group <- subset(vdem.small, vdem.small$COWcode == cow)
    current.year <- as.numeric(vdem.small$year[i])
    lag.year <- current.year - 1
    #if (group$year == loopyear) mergeddata$lji[i] <- group.year$LJI else mergeddata$lji <- NA
    group.year <- subset(group, group$year == lag.year)
    prior.elections <- subset(group, is.na(group$v2elirreg.inv) == FALSE & group$year < current.year)
    if (nrow(prior.elections) < 1) vdem.small$lowchamb.second.seatshare.lag[i] <- NA 
    if (nrow(prior.elections) < 1) next
    else
      mostrecent.election <- subset(prior.elections, prior.elections$year == max(prior.elections$year))
      vdem.small$years.since.election[i] <- current.year - mostrecent.election$year
      if (is.na(mostrecent.election$v2elirreg.inv) == TRUE) vdem.small$lowchamb.second.seatshare.lag[i] <- NA
      else vdem.small$lowchamb.second.seatshare.lag[i] <- mostrecent.election$v2ellostss
  })
}
    
    
###Non-working code for any prior turnovers
vdem.small$prior.turnover.any <- NA
i <- 1

for(i in 1:nrow(vdem.small)){
  tryCatch({
    cow <- as.numeric(vdem.small$COWcode[i])
    group <- subset(vdem.small, vdem.small$COWcode == cow)
    current.year <- as.numeric(vdem.small$year[i])
    group.year <- subset(group, group$year < current.year)
    max.turnover <- max(group.year$v2eltvrig)
    if (max.turnover > 0) vdem.small$prior.turnover.any[i] <- 1
    else vdem.small$prior.turnover.any[i] <- 0
  })
}
####

write.csv(vdem.small, "C:/Users/Cole/Documents/Grad school/Research topics/V-Dem/vdem-small-2018-post1943.csv")

vdem.nodems <- subset(vdem.small, vdem.small$e_polity2 < 8)
vdem.nodems <- subset(vdem.nodems, vdem.nodems$e_multiparty_elections == 1) #Only multiparty elections (stricter)
rm(vdem.small)

###Writing vdem.nodems
write.csv(vdem.nodems, "C:/Users/Cole/Documents/Grad school/Research topics/V-Dem/vdem-2018-no-dems-post1945-polity-sept2018-condensed.csv")

#####Loading dataset####
vdem.nodems <- read.csv("C:/Users/Cole/Documents/Grad school/Research topics/V-Dem/vdem-2018-no-dems-post1945-polity-sept2018-condensed.csv")
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


###Un-processed data models
 ##Freedom of association

mm.elirreg.base.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + v2x_frassoc_thick +
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

mm.elirreg.frassoc.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + v2x_frassoc_thick + 
                              jind.negshockmajor.lag:v2x_frassoc_thick +
                              elexec + #e_peaveduc+ 
                              e_polity2 +
                              e_migdppcln + 
                              e_miurbani + 
                              v2eldommon + 
                              log(e_mipopula) 
                            + transitional + altinfo.lag
                            
                            + (1 | COWcode), REML=FALSE,
                            data = vdem.nodems)
summary(mm.elirreg.frassoc.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.frassoc.raw)

 ##Core civil society
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + v2xcs_ccsi +
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

mm.elirreg.ccsi.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + v2xcs_ccsi + 
                                 jind.negshockmajor.lag:v2xcs_ccsi +
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


  ##Party institutionalization
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + party.institutionalization.lag +
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

mm.elirreg.partyinst.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + party.institutionalization.lag + 
                              jind.negshockmajor.lag:party.institutionalization.lag +
                              elexec + #e_peaveduc+ 
                              e_polity2 +
                              e_migdppcln + 
                              e_miurbani + 
                              v2eldommon + 
                              log(e_mipopula) 
                            + transitional + altinfo.lag
                            
                            + (1 | COWcode), REML=FALSE,
                            data = vdem.nodems)
summary(mm.elirreg.partyinst.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.partyinst.raw)
interplot(mm.elirreg.partyinst.raw, var1="jind.negshockmajor.lag", var2="party.institutionalization.lag")


   ##Political civil liberties
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + v2x_clpol +
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

mm.elirreg.polcl.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + v2x_clpol + 
                                   jind.negshockmajor.lag:v2x_clpol +
                                   elexec + #e_peaveduc+ 
                                   e_polity2 +
                                   e_migdppcln + 
                                   e_miurbani + 
                                   v2eldommon + 
                                   log(e_mipopula) 
                                 + transitional + altinfo.lag
                                 
                                 + (1 | COWcode), REML=FALSE,
                                 data = vdem.nodems)
summary(mm.elirreg.polcl.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.polcl.raw)

  ##Diagonal accountability
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + diagacc.lag +
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

mm.elirreg.diag.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + diagacc.lag + 
                               jind.negshockmajor.lag:diagacc.lag +
                               elexec + #e_peaveduc+ 
                               e_polity2 +
                               e_migdppcln + 
                               e_miurbani + 
                               v2eldommon + 
                               log(e_mipopula) 
                             + transitional + altinfo.lag
                             
                             + (1 | COWcode), REML=FALSE,
                             data = vdem.nodems)
summary(mm.elirreg.diag.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.diag.raw)

  ##Lagged lower house seat second place share 
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + lowchamb.second.seatshare.lag +
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

mm.elirreg.ss.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + lowchamb.second.seatshare.lag + 
                              jind.negshockmajor.lag:lowchamb.second.seatshare.lag +
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

  ##Engaged society
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + engaged.society.lag +
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

mm.elirreg.engaged.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + engaged.society.lag + 
                            jind.negshockmajor.lag:engaged.society.lag +
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
interplot(mm.elirreg.engaged.raw, var1="jind.negshockmajor.lag", var2="engaged.society.lag")

  ##State owernship of economy
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + v2clstown +
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

mm.elirreg.stown.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + v2clstown + 
                                 jind.negshockmajor.lag:v2clstown +
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
interplot(mm.elirreg.stown.raw, var1="jind.negshockmajor.lag", var2="v2clstown")

  ##Anti-system movements
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + antisystem.lag +
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

mm.elirreg.antisys.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + antisystem.lag + 
                               jind.negshockmajor.lag:antisystem.lag +
                               elexec + #e_peaveduc+ 
                               e_polity2 +
                               e_migdppcln + 
                               e_miurbani + 
                               v2eldommon + 
                               log(e_mipopula) 
                             + transitional + altinfo.lag
                             
                             + (1 | COWcode), REML=FALSE,
                             data = vdem.nodems)
summary(mm.elirreg.antisys.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.antisys.raw)
interplot(mm.elirreg.antisys.raw, var1="jind.negshockmajor.lag", var2="antisystem.lag")


 ##Opposition oversight
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + opposition.oversight.lag +
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

mm.elirreg.oppart.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + opposition.oversight.lag + 
                                 jind.negshockmajor.lag:opposition.oversight.lag +
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
interplot(mm.elirreg.oppart.raw, var1="jind.negshockmajor.lag", var2="opposition.oversight.lag")

 ##Opposition autonomy
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + oppaut.lag +
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

mm.elirreg.oppaut.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + oppaut.lag + 
                                jind.negshockmajor.lag:oppaut.lag +
                                elexec + #e_peaveduc+ 
                                e_polity2 +
                                e_migdppcln + 
                                e_miurbani + 
                                v2eldommon + 
                                log(e_mipopula) 
                              + transitional + altinfo.lag
                              
                              + (1 | COWcode), REML=FALSE,
                              data = vdem.nodems)
summary(mm.elirreg.oppaut.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.oppaut.raw)
interplot(mm.elirreg.oppaut.raw, var1="jind.negshockmajor.lag", var2="oppaut.lag")


 ##LC turnover in prior election
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + lowchamb.turnover.lag +
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

mm.elirreg.lcturn.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + lowchamb.turnover.lag + 
                                jind.negshockmajor.lag:lowchamb.turnover.lag +
                                elexec + #e_peaveduc+ 
                                e_polity2 +
                                e_migdppcln + 
                                e_miurbani + 
                                v2eldommon + 
                                log(e_mipopula) 
                              + transitional + altinfo.lag
                              
                              + (1 | COWcode), REML=FALSE,
                              data = vdem.nodems)
summary(mm.elirreg.lcturn.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.lcturn.raw)


  ##Comp
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + e_van_comp.lag +
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

mm.elirreg.comp.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + e_van_comp.lag + 
                                jind.negshockmajor.lag:e_van_comp.lag +
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


##regtrans
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + regtrans.lag +
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

mm.elirreg.regtrans.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + regtrans.lag + 
                              jind.negshockmajor.lag:regtrans.lag +
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


 ##parcomp

mm.elirreg.base.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + parcomp.lag +
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

mm.elirreg.parcomp.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + parcomp.lag + 
                                  jind.negshockmajor.lag:parcomp.lag +
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
interplot(mm.elirreg.parcomp.raw, "jind.negshockmajor.lag", "parcomp.lag")


##parreg

mm.elirreg.base.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + parreg.lag +
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

mm.elirreg.parreg.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + parreg.lag + 
                                 jind.negshockmajor.lag:parreg.lag +
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
interplot(mm.elirreg.parreg.raw, "jind.negshockmajor.lag", "parreg.lag")

 ##polcomp

mm.elirreg.base.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + polcomp.lag +
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

mm.elirreg.polcomp.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + polcomp.lag + 
                                jind.negshockmajor.lag:polcomp.lag +
                                elexec + #e_peaveduc+ 
                                e_polity2 +
                                e_migdppcln + 
                                e_miurbani + 
                                v2eldommon + 
                                log(e_mipopula) 
                              + transitional + altinfo.lag
                              
                              + (1 | COWcode), REML=FALSE,
                              data = vdem.nodems)
summary(mm.elirreg.polcomp.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.polcomp.raw)
interplot(mm.elirreg.polcomp.raw, "jind.negshockmajor.lag", "polcomp.lag")

##Seat-share difference
mm.elirreg.base.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + ss.diff.lag +
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

mm.elirreg.ssdiff.raw <- lmer(v2elirreg.inv~jind.negshockmajor.lag + ss.diff.lag + 
                                 jind.negshockmajor.lag:ss.diff.lag +
                                 elexec + #e_peaveduc+ 
                                 e_polity2 +
                                 e_migdppcln + 
                                 e_miurbani + 
                                 v2eldommon + 
                                 log(e_mipopula) 
                               + transitional + altinfo.lag
                               
                               + (1 | COWcode), REML=FALSE,
                               data = vdem.nodems)
summary(mm.elirreg.ssdiff.raw)

lrtest(mm.elirreg.base.raw, mm.elirreg.ssdiff.raw)

#######
#CBPS section
####
library(CBPS)

myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jind.negshockmajor.lag", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "state.ownership.lag", "core.civil.society.lag", "v2x_clpol",
            "diagacc.lag", "engaged.society.lag", "antisystem.lag", "v2psbars", "frassoc_thick.lag")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)

###Finding scores/weights 

cbps.out <- CBPS(jind.negshockmajor.lag~ democracy.duration.lag + oppaut.lag +
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
cbps.out <- CBPS(jind.negshockmajor.lag~ democracy.duration.lag + oppaut.lag +
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

model.elirreg.negshockmajor.frassoc <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                              frassoc_thick.lag + jind.negshockmajor.lag*frassoc_thick.lag + elexec  + 
                                                #e_polity2 +
                                                e_migdppcln + 
                                                #e_miurbani + 
                                                v2eldommon + 
                                                log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                              , weights=cbps.out$weights,
                                              data = cbps.out$data) 
summary(model.elirreg.negshockmajor.frassoc)
p.cbps <- interplot(model.elirreg.negshockmajor.frassoc, var1="jind.negshockmajor.lag", var2="frassoc_thick.lag",
                    hist=TRUE) + theme_bw() + labs(x="Lagged core civil society index", y="Marginal effect",
                                                   title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2)   #Significant positive

   #v2psbars Barriers to parties   
cbps.out <- CBPS(jind.negshockmajor.lag~ democracy.duration.lag + oppaut.lag +
                   e_peaveduc +
                   #e_van_comp.lag  + 
                   polity2.lag + #polcomp.lag + parcomp.lag +
                   loggpdpc.lag + 
                   e_miurbani +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 +transitional
                 + exec.respectcon.lag  +  altinfo.lag + 
                   + leg.constraints.lag + v2psbars, data=dataset.matching.complete)
summary(cbps.out)
balance(cbps.out)

model.elirreg.negshockmajor.partybars <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                                v2psbars + jind.negshockmajor.lag*v2psbars + elexec  + 
                                          #e_polity2 +
                                          e_migdppcln + 
                                          #e_miurbani + 
                                          v2eldommon + 
                                          log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                        , weights=cbps.out$weights,
                                        data = cbps.out$data) 
summary(model.elirreg.negshockmajor.partybars)
p.cbps <- interplot(model.elirreg.negshockmajor.partybars, var1="jind.negshockmajor.lag", var2="v2psbars",
                    hist=TRUE) + theme_bw() + labs(x="Lagged core civil society index", y="Marginal effect",
                                                   title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 

   #state.ownership.lag
cbps.out <- CBPS(jind.negshockmajor.lag~ democracy.duration.lag + oppaut.lag +
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


model.elirreg.negshockmajor.own <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                          state.ownership.lag + jind.negshockmajor.lag*state.ownership.lag + 
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
p.cbps <- interplot(model.elirreg.negshockmajor.own, var1="jind.negshockmajor.lag", var2="state.ownership.lag",
                    hist=TRUE) + theme_bw() + labs(x="Lagged state ownership", y="Marginal effect",
                                                   title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 


   #core.civil.society.lag
cbps.out <- CBPS(jind.negshockmajor.lag~ democracy.duration.lag + oppaut.lag + 
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


model.elirreg.negshockmajor.ccs <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                          core.civil.society.lag + jind.negshockmajor.lag*core.civil.society.lag + 
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
p.cbps <- interplot(model.elirreg.negshockmajor.ccs, var1="jind.negshockmajor.lag", var2="core.civil.society.lag",
                    hist=TRUE) + theme_bw() + labs(x="Lagged core civil society index", y="Marginal effect",
                                                   title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 

  
   #v2x_clpol
cbps.out <- CBPS(jind.negshockmajor.lag~ democracy.duration.lag + oppaut.lag +
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


model.elirreg.negshockmajor.polcl <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                         v2x_clpol + jind.negshockmajor.lag*v2x_clpol + elexec  + 
                                         #e_polity2 +
                                         e_migdppcln + 
                                         #e_miurbani + 
                                         v2eldommon + 
                                         log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                       , weights=cbps.out$weights,
                                       data = cbps.out$data) 
summary(model.elirreg.negshockmajor.polcl)
p.cbps <- interplot(model.elirreg.negshockmajor.polcl, var1="jind.negshockmajor.lag", var2="v2x_clpol",
                    hist=TRUE) + theme_bw() + labs(x="Political civil liberties", y="Marginal effect",
                                                   title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 



   #diagacc.lag
cbps.out <- CBPS(jind.negshockmajor.lag~ democracy.duration.lag + oppaut.lag + 
                   e_peaveduc +
                  # e_van_comp.lag  + 
                   polity2.lag + #polcomp.lag + parcomp.lag +
                   loggpdpc.lag + 
                   e_miurbani +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 +transitional
                 + exec.respectcon.lag  +  altinfo.lag + 
                   + leg.constraints.lag + diagacc.lag 
                 , data=dataset.matching.complete)
summary(cbps.out)
balance(cbps.out)


model.elirreg.negshockmajor.diag <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                           diagacc.lag + jind.negshockmajor.lag*diagacc.lag + 
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
summary(model.elirreg.negshockmajor.diag)
p.cbps <- interplot(model.elirreg.negshockmajor.diag, var1="jind.negshockmajor.lag", var2="diagacc.lag",
                    hist=TRUE) + theme_bw() + labs(x="Diagonal accountability lagged", y="Marginal effect",
                                                   title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 

   #engaged.society.lag
cbps.out <- CBPS(jind.negshockmajor.lag~ democracy.duration.lag + oppaut.lag + 
                   e_peaveduc +
                  # e_van_comp.lag  + 
                   polity2.lag + #polcomp.lag + parcomp.lag +
                   loggpdpc.lag + 
                   e_miurbani +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 +transitional
                 + exec.respectcon.lag  +  altinfo.lag + 
                   + leg.constraints.lag + engaged.society.lag 
                 , data=dataset.matching.complete)
summary(cbps.out)
balance(cbps.out)


model.elirreg.negshockmajor.engsoc <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                           engaged.society.lag + jind.negshockmajor.lag*engaged.society.lag + elexec  + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                         , weights=cbps.out$weights,
                                         data = cbps.out$data) 
summary(model.elirreg.negshockmajor.engsoc)
p.cbps <- interplot(model.elirreg.negshockmajor.engsoc, var1="jind.negshockmajor.lag", var2="engaged.society.lag",
                    hist=TRUE) + theme_bw() + labs(x="Engaged society lagged", y="Marginal effect",
                                                   title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 

   #antisystem.lag
cbps.out <- CBPS(jind.negshockmajor.lag~ democracy.duration.lag + oppaut.lag + 
                   e_peaveduc +
                 #  e_van_comp.lag  + 
                   polity2.lag + #polcomp.lag + parcomp.lag +
                   loggpdpc.lag + 
                   e_miurbani +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 +transitional
                 + exec.respectcon.lag  +  altinfo.lag + 
                   + leg.constraints.lag + antisystem.lag 
                 , data=dataset.matching.complete)
summary(cbps.out)
balance(cbps.out)


model.elirreg.negshockmajor.antisys <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                             antisystem.lag + jind.negshockmajor.lag*antisystem.lag + elexec  + 
                                             #e_polity2 +
                                             e_migdppcln + 
                                             #e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                           , weights=cbps.out$weights,
                                           data = cbps.out$data) 
summary(model.elirreg.negshockmajor.antisys)
p.cbps <- interplot(model.elirreg.negshockmajor.antisys, var1="jind.negshockmajor.lag", var2="antisystem.lag",
                    hist=TRUE) + theme_bw() + labs(x="Anti-system movements lagged", y="Marginal effect",
                                                   title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 

  

   #oppaut.lag
cbps.out <- CBPS(jind.negshockmajor.lag~ democracy.duration.lag + oppaut.lag + e_peaveduc +
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


model.elirreg.negshockmajor.oppaut <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                              oppaut.lag + jind.negshockmajor.lag*oppaut.lag 
                                            + elexec  + 
                                              #e_polity2 +
                                              e_migdppcln + 
                                              #e_miurbani + 
                                              v2eldommon + 
                                              log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                            , weights=cbps.out$weights,
                                            data = cbps.out$data) 
summary(model.elirreg.negshockmajor.oppaut)
p.cbps <- interplot(model.elirreg.negshockmajor.oppaut, var1="jind.negshockmajor.lag", var2="oppaut.lag",
                    hist=TRUE) + theme_bw() + labs(x="Opposition autonomy lagged", y="Marginal effect",
                                                   title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2)  


#opposition.oversight.lag
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jind.negshockmajor.lag", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "opposition.oversight.lag")  #, "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)

cbps.out <- CBPS(jind.negshockmajor.lag~ democracy.duration.lag + #oppaut.lag + 
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


model.elirreg.negshockmajor.oppover <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                              opposition.oversight.lag + jind.negshockmajor.lag*opposition.oversight.lag + 
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
p.cbps <- interplot(model.elirreg.negshockmajor.oppover, var1="jind.negshockmajor.lag", var2="opposition.oversight.lag",
                    hist=TRUE) + theme_bw() + labs(x="Opposition oversight lagged", y="Marginal effect",
                                                   title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2)    


  #leg.investigates.lag
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jind.negshockmajor.lag", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "leg.investigates.lag")  #, "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)

cbps.out <- CBPS(jind.negshockmajor.lag~ democracy.duration.lag + oppaut.lag + 
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

model.elirreg.negshockmajor.leginvest <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                                leg.investigates.lag + jind.negshockmajor.lag*leg.investigates.lag   + elexec  + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                         , weights=cbps.out$weights,
                                         data = cbps.out$data) 
summary(model.elirreg.negshockmajor.leginvest)
p.cbps <- interplot(model.elirreg.negshockmajor.leginvest, var1="jind.negshockmajor.lag", var2="leg.investigates.lag",
                    hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect",
                                                   title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 



#e_van_comp
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "e_van_comp.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jind.negshockmajor.lag", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "e_van_comp.lag")  #, "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)

cbps.out <- CBPS(jind.negshockmajor.lag~ democracy.duration.lag + #oppaut.lag + 
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

model.elirreg.negshockmajor.comp <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                           e_van_comp.lag + jind.negshockmajor.lag*e_van_comp.lag   + elexec  + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                         , weights=cbps.out$weights,
                                         data = cbps.out$data) 
summary(model.elirreg.negshockmajor.comp)
p.cbps <- interplot(model.elirreg.negshockmajor.comp, var1="jind.negshockmajor.lag", var2="e_van_comp.lag",
                    hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect",
                                                   title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 


##comp subset, no 0s or 70s
dataset.matching.complete.b <- subset(dataset.matching.complete, dataset.matching.complete$e_van_comp.lag > 0 &
                                        dataset.matching.complete$e_van_comp.lag < 70)

cbps.out <- CBPS(jind.negshockmajor.lag~ democracy.duration.lag + #oppaut.lag + 
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

model.elirreg.negshockmajor.comp <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                           e_van_comp.lag + jind.negshockmajor.lag*e_van_comp.lag   + elexec  + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                         , weights=cbps.out$weights,
                                         data = cbps.out$data) 
summary(model.elirreg.negshockmajor.comp)
p.cbps <- interplot(model.elirreg.negshockmajor.comp, var1="jind.negshockmajor.lag", var2="e_van_comp.lag",
                    hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect",
                                                   title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 


    #party.institutionalization.lag
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "e_van_comp.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jind.negshockmajor.lag", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "party.institutionalization.lag")  #, "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)


cbps.out <- CBPS(jind.negshockmajor.lag~ democracy.duration.lag + #oppaut.lag +
                   e_peaveduc +
                  # e_van_comp.lag  + 
                   polity2.lag + #polcomp.lag + parcomp.lag +
                   loggpdpc.lag + 
                   e_miurbani +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 +transitional
                 + exec.respectcon.lag  +  altinfo.lag + 
                   + leg.constraints.lag + party.institutionalization.lag 
                 , data=dataset.matching.complete)
summary(cbps.out)
balance(cbps.out)



model.elirreg.negshockmajor.pi <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                         party.institutionalization.lag + jind.negshockmajor.lag*party.institutionalization.lag + elexec  + 
                                         #e_polity2 +
                                         e_migdppcln + 
                                         #e_miurbani + 
                                         v2eldommon + 
                                         log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                       , weights=cbps.out$weights,
                                       data = cbps.out$data) 
summary(model.elirreg.negshockmajor.pi)
p.cbps <- interplot(model.elirreg.negshockmajor.pi, var1="jind.negshockmajor.lag", var2="party.institutionalization.lag",
                    hist=TRUE) + theme_bw() + labs(x="Lagged core civil society index", y="Marginal effect",
                                                   title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 


  ##polity2.lag

myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jind.negshockmajor.lag", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)

###Finding scores/weights 

cbps.out <- CBPS(jind.negshockmajor.lag~ democracy.duration.lag + oppaut.lag +
                   e_peaveduc +
                   #e_van_comp.lag  + 
                   polity2.lag + #polcomp.lag + parcomp.lag +
                   loggpdpc.lag + 
                   e_miurbani +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 +transitional
                 + exec.respectcon.lag  +  altinfo.lag + 
                   + leg.constraints.lag, data=dataset.matching.complete)
summary(cbps.out)
balance(cbps.out)


model.elirreg.negshockmajor.polity <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                         polity2.lag + jind.negshockmajor.lag*polity2.lag + elexec  + 
                                         #e_polity2 +
                                         e_migdppcln + 
                                         #e_miurbani + 
                                         v2eldommon + 
                                         log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                       , weights=cbps.out$weights,
                                       data = cbps.out$data) 
summary(model.elirreg.negshockmajor.polity)
p.cbps <- interplot(model.elirreg.negshockmajor.polity, var1="jind.negshockmajor.lag", var2="polity2.lag",
                    hist=TRUE) + theme_bw() + labs(x="Lagged Polity score", y="Marginal effect",
                                                   title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 


  ##parcomp.lag

myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jind.negshockmajor.lag", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "parcomp.lag")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]

dataset.matching$parcomp.lag2 <- dataset.matching$parcomp.lag
dataset.matching$parcomp.lag2[vdem.nodems$parcomp.lag == -66 |
                                vdem.nodems$parcomp.lag == -77 |
                                vdem.nodems$parcomp.lag == -88 ] <- 0

dataset.matching.complete <- na.omit(dataset.matching)


cbps.out <- CBPS(jind.negshockmajor.lag~ democracy.duration.lag + oppaut.lag +
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


model.elirreg.negshockmajor.parcomp <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                             parcomp.lag2 + jind.negshockmajor.lag*parcomp.lag2 + elexec  + 
                                             #e_polity2 +
                                             e_migdppcln + 
                                             #e_miurbani + 
                                             v2eldommon + 
                                             log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                           , weights=cbps.out$weights,
                                           data = cbps.out$data) 
summary(model.elirreg.negshockmajor.parcomp)
p.cbps <- interplot(model.elirreg.negshockmajor.parcomp, var1="jind.negshockmajor.lag", var2="parcomp.lag2",
                    hist=TRUE) + theme_bw() + labs(x="Lagged Polity score", y="Marginal effect",
                                                   title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 

##parreg

myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jind.negshockmajor.lag", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "parreg.lag")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]

dataset.matching$parreg.lag2 <- dataset.matching$parreg.lag
dataset.matching$parreg.lag2[vdem.nodems$parreg.lag == -66 |
                                vdem.nodems$parreg.lag == -77 |
                                vdem.nodems$parreg.lag == -88 ] <- 0

dataset.matching.complete <- na.omit(dataset.matching)


cbps.out <- CBPS(jind.negshockmajor.lag~ democracy.duration.lag + oppaut.lag +
                   e_peaveduc +
                   #e_van_comp.lag  + 
                   polity2.lag + #polcomp.lag 
                   + parreg.lag2 +
                   loggpdpc.lag + 
                   e_miurbani +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 +transitional
                 + exec.respectcon.lag  +  altinfo.lag + 
                   + leg.constraints.lag, data=dataset.matching.complete)
summary(cbps.out)
balance(cbps.out)


model.elirreg.negshockmajor.parreg <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                              parreg.lag2 + jind.negshockmajor.lag*parreg.lag2 + elexec  + 
                                              #e_polity2 +
                                              e_migdppcln + 
                                              #e_miurbani + 
                                              v2eldommon + 
                                              log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                            , weights=cbps.out$weights,
                                            data = cbps.out$data) 
summary(model.elirreg.negshockmajor.parreg)
p.cbps <- interplot(model.elirreg.negshockmajor.parreg, var1="jind.negshockmajor.lag", var2="parreg.lag2",
                    hist=TRUE) + theme_bw() + labs(x="Lagged Polity score", y="Marginal effect",
                                                   title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 


##polcomp
##parreg

myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jind.negshockmajor.lag", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "polcomp.lag")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]

dataset.matching$polcomp.lag2 <- dataset.matching$polcomp.lag
dataset.matching$polcomp.lag2[vdem.nodems$polcomp.lag == -66 |
                               vdem.nodems$polcomp.lag == -77 |
                               vdem.nodems$polcomp.lag == -88 ] <- 0

dataset.matching.complete <- na.omit(dataset.matching)


cbps.out <- CBPS(jind.negshockmajor.lag~ democracy.duration.lag + oppaut.lag +
                   e_peaveduc +
                   #e_van_comp.lag  + 
                   polcomp.lag2 + #polcomp.lag 
                   + polity2.lag +
                   loggpdpc.lag + 
                   e_miurbani +
                   + hc.ind.2lag 
                 +  lc.ind.2lag
                 +transitional
                 + exec.respectcon.lag  +  altinfo.lag + 
                   + leg.constraints.lag, data=dataset.matching.complete)
summary(cbps.out)
balance(cbps.out)


model.elirreg.negshockmajor.polcomp <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                              polcomp.lag2 + jind.negshockmajor.lag*polcomp.lag2 + elexec  + 
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
                                            + 
                                              #  e_miurbani + 
                                              v2eldommon + 
                                              log(e_mipopula) + (1 | COWcode)# + transitional + altinfo.lag
                                           , weights=cbps.out$weights,
                                           data = cbps.out$data) 
summary(model.elirreg.negshockmajor.polcomp)
p.cbps <- interplot(model.elirreg.negshockmajor.polcomp, var1="jind.negshockmajor.lag", var2="polcomp.lag2",
                    hist=TRUE) + theme_bw() + labs(x="Lagged Polity score", y="Marginal effect",
                                                   title="Marginal Effect of Latent Judicial Independence \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 


##Opposition oversight
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jind.negshockmajor.lag", "e_migdppcln", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "opposition.oversight.lag", "years.since.election")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)

cbps.out <- CBPS(jind.negshockmajor.lag~ democracy.duration.lag + oppaut.lag +
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



model.elirreg.negshockmajor.oversight <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                             opposition.oversight.lag + jind.negshockmajor.lag*opposition.oversight.lag + elexec  + 
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
p.cbps <- interplot(model.elirreg.negshockmajor.oversight, var1="jind.negshockmajor.lag", var2="opposition.oversight.lag",
                    hist=TRUE) + theme_bw() + labs(x="Opposition oversight", y="Marginal effect",
                                                   title="Marginal Effect of Negative JI Shock \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 

stargazer(model.elirreg.negshockmajor.ssdiff, type="html", 
          out="C:/Users/Cole/Dropbox/Judicial independence project/cbps outcome oversight.html")

png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/cbps marginal effects oversight.png", height=5,
    width=7, units="in", res=300)
p.cbps
dev.off()



##ss.diff.lag
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jind.negshockmajor.lag", "loggpdpc.lag",
            "democracy.duration.lag", "exec.respectcon.lag", "leg.constraints.lag", 
            "altinfo.lag", "ss.diff.lag", "years.since.election")  # "e_van_comp.lag  , "party.institutionalization.lag" ....... "parcomp.lag", "polcomp.lag"

dataset.matching <- vdem.nodems[myvars]
dataset.matching.complete <- na.omit(dataset.matching)
dataset.matching.complete$ss.diff.lag.inv <- 100 - dataset.matching.complete$ss.diff.lag

cbps.out <- CBPS(jind.negshockmajor.lag~ democracy.duration.lag + oppaut.lag +
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



model.elirreg.negshockmajor.ssdiff <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                              ss.diff.lag.inv + jind.negshockmajor.lag*ss.diff.lag.inv + elexec  + 
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
p.cbps <- interplot(model.elirreg.negshockmajor.ssdiff, var1="jind.negshockmajor.lag", var2="ss.diff.lag.inv",
                    hist=TRUE) + theme_bw() + labs(x="Difference in lower-chamber seat-shares, first and second parties", y="Marginal effect",
                                                   title="Marginal Effect of Negative JI Shock \non Voting Irregularities") +
  geom_hline(yintercept=0, linetype=2) 

stargazer(model.elirreg.negshockmajor.ssdiff, type="html", 
          out="C:/Users/Cole/Dropbox/Judicial independence project/cbps outcome seatshare.html")

png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/cbps marginal effects ss.png", height=5,
    width=7, units="in", res=300)
p.cbps
dev.off()

#####
#Entropy balancing

###Entropy balancing--functioning now
library(ebal)

myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jind.negshockmajor.lag", "e_migdppcln", "loggpdpc.lag",
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

ebal.test <- ebalance(Treatment=dataset.matching.complete$jind.negshockmajor.lag, X = ebal.covariates[c(-15, -16)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==0,],2,mean)
library(stargazer)
stargazer(cbind(t.means, c.means.bal, c.means), type="html", 
          out="C:/Users/Cole/Dropbox/Judicial independence project/ebal adjusted means full data.html")



dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$jind.negshockmajor.lag == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$jind.negshockmajor.lag == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
elirreg.negshockmajor.ssdiff.base <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
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


mm.elirreg.negshockmajor.ssdiff.all <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                            ss.diff.lag.inv + jind.negshockmajor.lag*ss.diff.lag.inv   + elexec + 
                                            # e_polity2 +
                                            e_migdppcln + 
                                            #e_miurbani + 
                                            v2eldommon + 
                                            log(e_mipopula) #+ transitional + altinfo.lag 
                                          + (1 | COWcode), REML=FALSE, 
                                          weights=ebal.test.w,
                                          data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.ssdiff.all)
p2 <- interplot(mm.elirreg.negshockmajor.ssdiff.all, var1="jind.negshockmajor.lag", var2="ss.diff.lag.inv",
                hist=TRUE) + theme_bw() + labs(x="Prior election seat-share margin (inverse)", y="Marginal effect" , 
                                               title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) 


png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/ebal seat share.png", height=5,
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
 

##Core civil society


myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jind.negshockmajor.lag", "e_migdppcln", "loggpdpc.lag",
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

ebal.test <- ebalance(Treatment=dataset.matching.complete$jind.negshockmajor.lag, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$jind.negshockmajor.lag == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$jind.negshockmajor.lag == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
elirreg.negshockmajor.civil.base <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
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


mm.elirreg.negshockmajor.civil.all <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                            core.civil.society.lag + jind.negshockmajor.lag*core.civil.society.lag   + elexec + 
                                            # e_polity2 +
                                            e_migdppcln + 
                                            #e_miurbani + 
                                            v2eldommon + 
                                            log(e_mipopula) #+ transitional + altinfo.lag 
                                          + (1 | COWcode), REML=FALSE, 
                                          weights=ebal.test.w,
                                          data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.civil.all)
p3 <- interplot(mm.elirreg.negshockmajor.civil.all, var1="jind.negshockmajor.lag", var2="core.civil.society.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged civil society index", y="Marginal effect" , 
                                               title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) 


png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/ebal core civil society.png", height=5,
    width=7, units="in", res=300)
p3
dev.off()

lrtest(elirreg.negshockmajor.civil.base, mm.elirreg.negshockmajor.civil.all)



### Legislative opposition oversight
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jind.negshockmajor.lag", "e_migdppcln", "loggpdpc.lag",
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

ebal.test <- ebalance(Treatment=dataset.matching.complete$jind.negshockmajor.lag, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$jind.negshockmajor.lag == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$jind.negshockmajor.lag == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
elirreg.negshockmajor.oversight.base <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
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


mm.elirreg.negshockmajor.oversight <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                            opposition.oversight.lag + jind.negshockmajor.lag*opposition.oversight.lag   + elexec + 
                                            # e_polity2 +
                                            e_migdppcln + 
                                            #e_miurbani + 
                                            v2eldommon + 
                                            log(e_mipopula) #+ transitional + altinfo.lag 
                                          + (1 | COWcode), REML=FALSE, 
                                          weights=ebal.test.w,
                                          data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.oversight)
p4 <- interplot(mm.elirreg.negshockmajor.oversight, var1="jind.negshockmajor.lag", var2="opposition.oversight.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged opposition oversight", y="Marginal effect" , 
                                               title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) 

png("C:/Users/Cole/Dropbox/Judicial independence project/R plots/ebal oversight.png", height=5,
    width=7, units="in", res=300)
p4
dev.off()

lrtest(elirreg.negshockmajor.oversight.base, mm.elirreg.negshockmajor.oversight)


###Main results table 
stargazer(elirreg.negshockmajor.ssdiff.base, mm.elirreg.negshockmajor.ssdiff.all, elirreg.negshockmajor.oversight.base, mm.elirreg.negshockmajor.oversight, elirreg.negshockmajor.civil.base,
          mm.elirreg.negshockmajor.civil.all,
          type="html", out="C:/Users/Cole/Dropbox/Judicial independence project/ebal results.html")


##polcomp


myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jind.negshockmajor.lag", "e_migdppcln", "loggpdpc.lag",
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

ebal.test <- ebalance(Treatment=dataset.matching.complete$jind.negshockmajor.lag, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$jind.negshockmajor.lag == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$jind.negshockmajor.lag == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
ols.elirreg.negshockmajor.comp.all <- lm(v2elirreg.inv~jind.negshockmajor.lag +
                                           polcomp.lag2 + jind.negshockmajor.lag*polcomp.lag2 + elexec + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) 
                                         #+ transitional + altinfo.lag
                                         , weights=ebal.test.w,
                                         data = dataset.matching.complete.w)
summary(ols.elirreg.negshockmajor.comp.all)
p1 <- interplot(ols.elirreg.negshockmajor.comp.all, var1="jind.negshockmajor.lag", var2="polcomp.lag2",
                hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect") + geom_hline(yintercept=0, linetype=2) 


mm.elirreg.negshockmajor.polcomp <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                            polcomp.lag2 + jind.negshockmajor.lag*polcomp.lag2   + elexec + 
                                            # e_polity2 +
                                            e_migdppcln + 
                                            #e_miurbani + 
                                            v2eldommon + 
                                            log(e_mipopula) #+ transitional + altinfo.lag 
                                          + (1 | COWcode), REML=FALSE, 
                                          weights=ebal.test.w,
                                          data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.polcomp)
p.polcomp <- interplot(mm.elirreg.negshockmajor.polcomp, var1="jind.negshockmajor.lag", var2="polcomp.lag2",
                hist=TRUE) + theme_bw() + labs(x="Lagged political competition", y="Marginal effect" , 
                                               title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) 





### State ownership
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jind.negshockmajor.lag", "e_migdppcln", "loggpdpc.lag",
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

ebal.test <- ebalance(Treatment=dataset.matching.complete$jind.negshockmajor.lag, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$jind.negshockmajor.lag == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$jind.negshockmajor.lag == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
ols.elirreg.negshockmajor.comp.all <- lm(v2elirreg.inv~jind.negshockmajor.lag +
                                           state.ownership.lag + jind.negshockmajor.lag*state.ownership.lag + elexec + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) 
                                         #+ transitional + altinfo.lag
                                         , weights=ebal.test.w,
                                         data = dataset.matching.complete.w)
summary(ols.elirreg.negshockmajor.comp.all)
p1 <- interplot(ols.elirreg.negshockmajor.comp.all, var1="jind.negshockmajor.lag", var2="opposition.oversight.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect") + geom_hline(yintercept=0, linetype=2) 


mm.elirreg.negshockmajor.stateown <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                            state.ownership.lag + jind.negshockmajor.lag*state.ownership.lag   + elexec + 
                                            # e_polity2 +
                                            e_migdppcln + 
                                            #e_miurbani + 
                                            v2eldommon + 
                                            log(e_mipopula) #+ transitional + altinfo.lag 
                                          + (1 | COWcode), REML=FALSE, 
                                          weights=ebal.test.w,
                                          data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.stateown)
p.stateowned <- interplot(mm.elirreg.negshockmajor.stateown, var1="jind.negshockmajor.lag", var2="state.ownership.lag",
                hist=TRUE) + theme_bw() + labs(x="State ownership of the economy (lagged)", y="Marginal effect" , 
                                               title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) 


###daigonal accountability
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jind.negshockmajor.lag", "e_migdppcln", "loggpdpc.lag",
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

ebal.test <- ebalance(Treatment=dataset.matching.complete$jind.negshockmajor.lag, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$jind.negshockmajor.lag == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$jind.negshockmajor.lag == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
ols.elirreg.negshockmajor.comp.all <- lm(v2elirreg.inv~jind.negshockmajor.lag +
                                           diagacc.lag + jind.negshockmajor.lag*diagacc.lag + elexec + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) 
                                         #+ transitional + altinfo.lag
                                         , weights=ebal.test.w,
                                         data = dataset.matching.complete.w)
summary(ols.elirreg.negshockmajor.comp.all)
p1 <- interplot(ols.elirreg.negshockmajor.comp.all, var1="jind.negshockmajor.lag", var2="opposition.oversight.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect") + geom_hline(yintercept=0, linetype=2) 


mm.elirreg.negshockmajor.diag <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                            diagacc.lag + jind.negshockmajor.lag*diagacc.lag   + elexec + 
                                            # e_polity2 +
                                            e_migdppcln + 
                                            #e_miurbani + 
                                            v2eldommon + 
                                            log(e_mipopula) #+ transitional + altinfo.lag 
                                          + (1 | COWcode), REML=FALSE, 
                                          weights=ebal.test.w,
                                          data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.diag)
p.diag <- interplot(mm.elirreg.negshockmajor.diag, var1="jind.negshockmajor.lag", var2="diagacc.lag",
                hist=TRUE) + theme_bw() + labs(x="Diagonal accountability (lagged)", y="Marginal effect" , 
                                               title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) 



###parreg2

myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jind.negshockmajor.lag", "e_migdppcln", "loggpdpc.lag",
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

ebal.test <- ebalance(Treatment=dataset.matching.complete$jind.negshockmajor.lag, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$jind.negshockmajor.lag == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$jind.negshockmajor.lag == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
ols.elirreg.negshockmajor.comp.all <- lm(v2elirreg.inv~jind.negshockmajor.lag +
                                           parreg2.lag + jind.negshockmajor.lag*parreg2.lag + elexec + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) 
                                         #+ transitional + altinfo.lag
                                         , weights=ebal.test.w,
                                         data = dataset.matching.complete.w)
summary(ols.elirreg.negshockmajor.comp.all)
p1 <- interplot(ols.elirreg.negshockmajor.comp.all, var1="jind.negshockmajor.lag", var2="opposition.oversight.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect") + geom_hline(yintercept=0, linetype=2) 


mm.elirreg.negshockmajor.comp.all <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                            parreg2.lag + jind.negshockmajor.lag*parreg2.lag   + elexec + 
                                            # e_polity2 +
                                            e_migdppcln + 
                                            #e_miurbani + 
                                            v2eldommon + 
                                            log(e_mipopula) #+ transitional + altinfo.lag 
                                          + (1 | COWcode), REML=FALSE, 
                                          weights=ebal.test.w,
                                          data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.comp.all)
p2 <- interplot(mm.elirreg.negshockmajor.comp.all, var1="jind.negshockmajor.lag", var2="parreg2.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect" , 
                                               title="Marginal effect of negative JI shock on intentional \nvoting irregularities") +
  geom_hline(yintercept=0, linetype=2) 


###parcomp
myvars <- c("COWcode", "year", "transitional", "v2elirreg.inv", "lc.ind.2lag", 
            "hc.ind.2lag", "oppaut.lag", "elexec", "e_peaveduc",  
            "polity2.lag", "e_polity2", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "jind.negshockmajor.lag", "e_migdppcln", "loggpdpc.lag",
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

ebal.test <- ebalance(Treatment=dataset.matching.complete$jind.negshockmajor.lag, X = ebal.covariates[c(-14, -15)])



# means in treatment group data
t.means <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==1,],2,mean)
# means in reweighted control group data
c.means.bal <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==0,],2,weighted.mean,w=ebal.test$w)
# means in raw data control group data
c.means <- apply(ebal.covariates[dataset.matching.complete$jind.negshockmajor.lag==0,],2,mean)


dataset.matching.complete.controls <- subset(dataset.matching.complete, dataset.matching.complete$jind.negshockmajor.lag == 0)
dataset.matching.complete.controls <- data.frame(cbind(dataset.matching.complete.controls, ebal.test$w))

dataset.matching.complete.treat <- subset(dataset.matching.complete, dataset.matching.complete$jind.negshockmajor.lag == 1)
dataset.matching.complete.treat$ebal.test.w <- 1

dataset.matching.complete.w <- data.frame(rbind(dataset.matching.complete.treat, dataset.matching.complete.controls))


###regression
ols.elirreg.negshockmajor.comp.all <- lm(v2elirreg.inv~jind.negshockmajor.lag +
                                           parcomp2.lag + jind.negshockmajor.lag*parcomp2.lag + elexec + 
                                           #e_polity2 +
                                           e_migdppcln + 
                                           #e_miurbani + 
                                           v2eldommon + 
                                           log(e_mipopula) 
                                         #+ transitional + altinfo.lag
                                         , weights=ebal.test.w,
                                         data = dataset.matching.complete.w)
summary(ols.elirreg.negshockmajor.comp.all)
p1 <- interplot(ols.elirreg.negshockmajor.comp.all, var1="jind.negshockmajor.lag", var2="opposition.oversight.lag",
                hist=TRUE) + theme_bw() + labs(x="Lagged electoral competition", y="Marginal effect") + geom_hline(yintercept=0, linetype=2) 


mm.elirreg.negshockmajor.comp.all <- lmer(v2elirreg.inv~jind.negshockmajor.lag +
                                            parcomp2.lag + jind.negshockmajor.lag*parcomp2.lag   + elexec + 
                                            # e_polity2 +
                                            e_migdppcln + 
                                            #e_miurbani + 
                                            v2eldommon + 
                                            log(e_mipopula) #+ transitional + altinfo.lag 
                                          + (1 | COWcode), REML=FALSE, 
                                          weights=ebal.test.w,
                                          data = dataset.matching.complete.w)
summary(mm.elirreg.negshockmajor.comp.all)
p2 <- interplot(mm.elirreg.negshockmajor.comp.all, var1="jind.negshockmajor.lag", var2="parcomp2.lag",
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


####Plot of lji.2lag and jind.negshockmajor.lag
p.levels <- qplot(lji.2lag, jitter(jind.negshockmajor.lag), data=vdem.nodems, xlab="Latent judicial independence (lagged)",
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



