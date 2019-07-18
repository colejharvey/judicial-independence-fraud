###Loading and wrangling data for judicial independence and fraud
###Tidyverse version
###July 2019

rm(list = setdiff(ls(), lsf.str())) #Remove all except functions

library(tidyverse)

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

###Getting previous.election and years.since.election
i <- 1
vdem.small$prior.election <- NA
vdem.small$next.election <- NA
for(i in 1:nrow(vdem.small)){
  tryCatch({
    cow <- as.numeric(vdem.small$COWcode[i])
    group <- subset(vdem.small, vdem.small$COWcode == cow)
    current.year <- as.numeric(vdem.small$year[i])
    lag.year <- current.year - 1
    #if (group$year == loopyear) mergeddata$lji[i] <- group.year$LJI else mergeddata$lji <- NA
    #group.year <- subset(group, group$year == lag.year)
    prior.elections <- subset(group, is.na(group$v2elirreg.inv) == FALSE & group$year < current.year)
    if (nrow(prior.elections) >= 1)  vdem.small$prior.election[i] <- max(prior.elections$year)
    
    next.elections <- subset(group, is.na(group$v2elirreg.inv) == FALSE & group$year >= current.year)
    if (nrow(next.elections) >= 1)  vdem.small$next.election[i] <- min(next.elections$year)
    
  })
}

vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(years.since.election = year - prior.election)

##Lagging positive reform using pipe
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(reform_positive.lag = lag(reform_positive))   


###Creating a factor for each election period (the period between election A and B is named after B
vdem.small$country_election_period <- paste(vdem.small$country_name, as.character(vdem.small$next.election), sep="_")



##Lagging electoral manipulation DVs and other election-specific vars
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(v2elirreg.inv.lag = lag(v2elirreg.inv))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(v2elintim.inv.lag = lag(v2elintim.inv))   
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(v2eldommon.lag = lag(v2eldommon)) 
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(elexec.lag = lag(elexec))   
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(polity2.lag = lag(e_polity2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(opposition.oversight.lag = lag(v2lgoppart))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(loggpdpc.lag = lag(e_migdppcln))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(civil.society.lag = lag(v2xcs_ccsi))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(lc.ind.lag = lag(v2juncind, 1))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(hc.ind.lag = lag(v2juhcind, 1))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(jureform.lag = lag(v2jureform_ord, 1))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(altinfo.lag = lag(v2xme_altinf, 1))




##Lagging selection model variables by 2 years (so that they can predict the 1-year lag positive reform)
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(lc.ind.2lag = lag(v2juncind, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(hc.ind.2lag = lag(v2juhcind, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(oppaut.2lag = lag(v2psoppaut, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(polity.2lag = lag(e_polity2, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(civil.society.2lag = lag(v2xcs_ccsi, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(opp.oversight.2lag = lag(v2lgoppart, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(urban.2lag = lag(e_miurbani, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(loggpdpc.2lag = lag(e_migdppcln, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(democracy.duration.2lag = lag(e_democracy_duration, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(exec.respectcon.2lag = lag(v2dlengage, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(altinfo.2lag = lag(v2xme_altinf, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(education.2lag = lag(e_peaveduc, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(leg.constraints.2lag = lag(v2xlg_legcon, 2))







###Getting posreform for any year between two elections

test <- data.frame(tapply(X = vdem.small$reform_positive.lag, INDEX = vdem.small$country_election_period, sum))
test[,2] <- rownames(test) #Get rownames as factor

names(test)[names(test)=="V2"] <- "country_election_period"  ##Rename v2 to the be the same as country_election_period
names(test)[names(test)=="tapply.X...vdem.small.reform_positive.lag..INDEX...vdem.small.country_election_period.."] <- "reform.positive.electionperiod"  ##Rename other variable to be reform.positive.electionperiod


##Convert to binary so that =1 if there was a reform in the period.

test <- test %>% mutate(reform.positive.electionperiod.binary = if_else(reform.positive.electionperiod > 0, 1, 0))



##Use mutating join (either with base R or with tidyverse) to combine the tapply result to the vdem.small frame
vdem.small <- merge(vdem.small, test)



##Getting selection-model covariates that are for the first year of the election period

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

write.csv(vdem.small, "./vdem-small-2018-post1943-tidy.csv")

###Make vdem.nodems

vdem.nodems <- subset(vdem.small, vdem.small$e_polity2 < 8)
vdem.nodems <- subset(vdem.nodems, vdem.nodems$e_multiparty_elections == 1) #Only multiparty elections (stricter)
rm(vdem.small)

###Writing vdem.nodems
write.csv(vdem.nodems, "./vdem-2018-no-dems-post1945-polity-sept2018-condensed-tidy.csv")

###Next check to get synatx of models right
###Binary EV captures reforms that took place *before* the upcoming election (including in the year of the prior election)
###So use .cycle variables for selection models, reform.binary as is, and then the un-lagged DVs
###But then some of the election variables need to lag the DV (polity, oppart, civil society and gdp)

vdem.small2 <- read.csv("./vdem-small-2018-post1943-tidy.csv")

vdem.small2 <- vdem.small2 %>% dplyr::select(v2elirreg.inv, reform.positive.electionperiod.binary, reform_positive,
                                          elexec, exec.respectcon.cycle, v2exrescon,
                                         polity2.lag,  polity2.cycle,
                                         loggpdpc.lag, 
                                        year, country_election_period, COWcode)
vdem.small2 <- vdem.small2 %>% dplyr::arrange(COWcode, year)
