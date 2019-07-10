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
vdem.short$v2elintim.inv <- vdem.short$v2elintim * -1    #Election-related violence by the govt/ruling party
vdem.short$v2elvotbuy.inv <- vdem.short$v2elvotbuy * -1
vdem.short$v2elpeace.inv <- vdem.short$v2elpeace * -1    #Election related violence NOT by the govt/ruling party

vdem.short$transitional <- NA

vdem.short$transitional[vdem.short$e_nelda_1_ex == "yes" | vdem.short$e_nelda_1_leg == "yes"] <- 1
vdem.short$transitional[vdem.short$e_nelda_1_ex == "no" | vdem.short$e_nelda_1_leg == "no" |
                          (vdem.short$e_nelda_1_ex == "" & vdem.short$e_nelda_1_leg == "") ] <- 0






myvars <- c("COWcode", "year", "transitional", "v2elvotbuy.inv", "v2elirreg.inv", "v2elintim.inv", "v2elpeace.inv", "v2juncind", 
            "v2juhcind", "v2psoppaut", "e_van_comp", "elexec", "e_peaveduc", 
            "e_polity2", "e_migdppcln", "e_miurbani", "v2elmulpar", "v2eldommon", "e_mipopula", 
            "country_name", "elexec", "e_multiparty_elections", "v2jureform", "v2jureform_ord", "v2jupurge", "v2jupurge_ord",
            "v2jupack", "v2jupack_ord", "v2x_jucon", "e_democracy_duration", "e_regtrans", "v2exrescon", "v2xlg_legcon",
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


vdem.small$dejure.jind.negshockmajor <- NA
vdem.small$dejure.jind.negshockmajor[vdem.small$v2jureform_ord == 1 | vdem.small$v2jureform_ord == 2] <- 0
vdem.small$dejure.jind.negshockmajor[vdem.small$v2jureform_ord == 0] <- 1  #There was a massive, politically motivated increase in the number of judgeships
#across the entire judiciary


vdem.small$defacto.jind.negshockmajor <- NA
vdem.small$defacto.jind.negshockmajor[vdem.small$v2jupurge_ord >= 1 &
                                vdem.small$v2jupack_ord >= 1 ] <- 0
vdem.small$defacto.jind.negshockmajor[vdem.small$v2jupurge_ord < 1 |       # There was a massive, arbitrary purge of the judiciary
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

write.csv(vdem.small, "C:/Users/Cole/Documents/Research projects/Judicial independence and fraud/vdem-small-2018-post1943.csv")

###Reading in vdem.small
vdem.small <- read.csv("C:/Users/Cole/Documents/Research projects/Judicial independence and fraud/vdem-small-2018-post1943.csv")


###Getting lagged data

vdem.small$jureform.lag <- NA
vdem.small$hc.ind.lag <- NA
vdem.small$lc.ind.lag <- NA
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
vdem.small$vdem.jucon.lag <- NA

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
    vdem.small$hc.ind.lag[i] <- group.year$v2juhcind
    vdem.small$lc.ind.lag[i] <- group.year$v2juncind
    vdem.small$hc.ind.2lag[i] <- group.year2$v2juhcind
    vdem.small$lc.ind.2lag[i] <- group.year2$v2juncind
    vdem.small$jupurge.lag[i] <- group.year$v2jupurge
    vdem.small$jupack.lag[i] <- group.year$v2jupack
    vdem.small$jind.negshock.lag[i] <- group.year$jind.negshock
    vdem.small$jind.negshockmajor.lag[i] <- group.year$jind.negshockmajor
    vdem.small$pack_major.lag[i] <- group.year$pack_major
    vdem.small$purge_major.lag[i] <- group.year$purge_major
    vdem.small$negreform.lag[i] <- group.year$reform_negative
    vdem.small$posreform.lag[i] <- group.year$reform_positive
    vdem.small$vdem.jucon.lag[i] <- group.year$v2x_jucon
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

write.csv(vdem.small, "C:/Users/Cole/Documents/Research projects/Judicial independence and fraud/vdem-small-2018-post1943.csv")

vdem.nodems <- subset(vdem.small, vdem.small$e_polity2 < 8)
vdem.nodems <- subset(vdem.nodems, vdem.nodems$e_multiparty_elections == 1) #Only multiparty elections (stricter)
rm(vdem.small)

###Writing vdem.nodems
write.csv(vdem.nodems, "C:/Users/Cole/Documents/Research projects/Judicial independence and fraud/vdem-2018-no-dems-post1945-polity-sept2018-condensed.csv")

