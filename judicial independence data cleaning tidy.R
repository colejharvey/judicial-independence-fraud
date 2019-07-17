###Loading and wrangling data for judicial independence and fraud
###Tidyverse version
###July 2019

library(tidyverse)
rm(list = setdiff(ls(), lsf.str())) #Remove all except functions

###Loading full dataset

vdem <- read.csv("C:/Users/Cole/Documents/Research topics literature/V-Dem/VDem7.1/Country_Year_V-Dem_other_CSV_v7.1/V-Dem-DS-CY+Others-v7.1.csv")

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
rm(vdem.short)

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

write.csv(vdem.small, "C:/Users/Cole/Documents/Research projects/Judicial independence and fraud/vdem-small-2018-post1943-tidy.csv")

###Reading in vdem.small
vdem.small <- read.csv("C:/Users/Cole/Documents/Research projects/Judicial independence and fraud/vdem-small-2018-post1943-tidy.csv")

###Getting second largest party seats share in prior election and years.since.election
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

###Most recent election year

vdem.small$most.recent.election <- vdem.small$year - vdem.small$years.since.election

##Lagging positive reform using pipe
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(reform_positive.lag = lag(reform_positive))   

##Lagging electoral manipulation DVs
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(v2elirreg.inv.lag = lag(v2elirreg.inv))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(v2elintim.inv.lag = lag(v2elintim.inv))   


###Getting posreform for any year between two elections
vdem.small$country_election_period <- paste(vdem.small$country_name, as.character(vdem.small$most.recent.election), sep="_")

test <- data.frame(tapply(X = vdem.small$reform_positive, INDEX = vdem.small$country_election_period, sum))
test[,2] <- rownames(test) #Get rownames as factor

names(test)[names(test)=="V2"] <- "country_election_period"  ##Rename v2 to the be the same as country_election_period
names(test)[names(test)=="tapply.X...vdem.small.reform_positive..INDEX...vdem.small.country_election_period.."] <- "reform.positive.electionperiod"  ##Rename other variable to be reform.positive.electionperiod


##Convert to binary so that =1 if there was a reform in the period.
test$reform.positive.electionperiod.binary <- NA
i <- 1
for (i in 1:nrow(test)){
  if (is.na(test$reform.positive.electionperiod[i]) == TRUE) {
    print("Skipping NA") 
    next}
  
  if (test$reform.positive.electionperiod[i] > 0) test$reform.positive.electionperiod.binary[i] <- 1 
  if (test$reform.positive.electionperiod[i] == 0) test$reform.positive.electionperiod.binary[i] <- 0
}

##Use mutating join (either with base R or with tidyverse) to combine the tapply result to the vdem.small frame
vdem.small <- merge(vdem.small, test)



##Getting selection-model covariates that are for the first year of the election period
vdem.small$hc.ind.cycle <- NA
vdem.small$lc.ind.cycle <- NA
vdem.small$polity2.cycle <- NA
vdem.small$e_van_comp.cycle <- NA
vdem.small$oppaut.cycle <- NA
vdem.small$loggpdpc.cycle <- NA
vdem.small$democracy.duration.cycle <- NA
vdem.small$exec.respectcon.cycle <- NA
vdem.small$leg.constraints.cycle <- NA
vdem.small$altinfo.cycle <- NA
vdem.small$parcomp.cycle <- NA
vdem.small$polcomp.cycle <- NA
vdem.small$core.civil.society.cycle <- NA
vdem.small$opposition.oversight.cycle <- NA
vdem.small$engaged.society.cycle <- NA
vdem.small$urban.cycle <- NA
vdem.small$education.cycle <- NA
vdem.small$transitional.cycle <- NA

###Find first year in election cycle
vdem.small <- vdem.small %>% group_by(country_election_period) %>% mutate(first.year.cycle = min(year))

###Save selection model covariates
vars <- c("v2juhcind",
         "v2juncind",
         "e_polity2",
         "e_van_comp",
         "v2psoppaut",
          "e_migdppcln",
          "e_democracy_duration",
          "v2exrescon",
          "v2xlg_legcon",
         "v2xme_altinf",
          "e_parcomp",
         "e_polcomp",
          "v2xcs_ccsi",
         "v2lgoppart",
          "v2dlengage",
          "e_miurbani",
         "e_peaveduc",
         "transitional",
         "country_election_period")

###Create a dataset of only the first years in each election cycle

data.firstyears <- vdem.small %>% group_by(country_election_period) %>% filter(year == first.year.cycle) %>% 
   select(vars)

###Rename the first-year variables so they can be joined to vdem.small
data.firstyears <- rename(data.firstyears, hc.ind.cycle = v2juhcind,
                          lc.ind.cycle = v2juncind,
                          polity2.cycle = e_polity2,
                          e_van_comp.cycle =  e_van_comp,
                          oppaut.cycle = v2psoppaut,
                          loggpdpc.cycle = e_migdppcln,
                          democracy.duration.cycle = e_democracy_duration,
                          exec.respectcon.cycle = v2exrescon,
                          leg.constraints.cycle = v2xlg_legcon,
                          altinfo.cycle = v2xme_altinf,
                          parcomp.cycle = e_parcomp,
                          polcomp.cycle = e_polcomp,
                          core.civil.society.cycle = v2xcs_ccsi,
                          opposition.oversight.cycle = v2lgoppart,
                          engaged.society.cycle = v2dlengage,
                          urban.cycle = e_miurbani,
                          education.cycle = e_peaveduc,
                          transitional.cycle = transitional)

###Join with vdem.small
vdem.small <- vdem.small %>% 
  left_join(data.firstyears, by = "country_election_period")

###Write vdem.small

write.csv(vdem.small, "C:/Users/Cole/Documents/Research topics literature/V-Dem/vdem-small-2018-post1943-tidy.csv")

###Make vdem.nodems

vdem.nodems <- subset(vdem.small, vdem.small$e_polity2 < 8)
vdem.nodems <- subset(vdem.nodems, vdem.nodems$e_multiparty_elections == 1) #Only multiparty elections (stricter)
rm(vdem.small)

###Writing vdem.nodems
write.csv(vdem.nodems, "C:/Users/Cole/Documents/Research topics literature/V-Dem/vdem-2018-no-dems-post1945-polity-sept2018-condensed-tidy.csv")
