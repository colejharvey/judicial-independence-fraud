###Loading and wrangling data for judicial independence and fraud
###Tidyverse version
###July 2019

rm(list = setdiff(ls(), lsf.str())) #Remove all except functions

library(tidyverse)

##Note: seat-share code is missing for some reason, data can be merged into newer datasets from july2020 file if needed


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
            "v2x_freexp_thick", "v2x_freexp", "v2xme_altinf", "e_polcomp", "e_parcomp", "e_exconst", "e_democ", "e_autoc", 
            "v2x_frassoc_thick",  "v2xcs_ccsi", "v2xps_party", "v2x_civlib", "v2x_clpol", "v2x_diagacc", "v2ellocumul", "v2ellocons",
            "v2elparlel", "v2elloeldm", "v2ellovtlg", "v2ellovtsm", "v2ellostlg",
            "v2elloseat", "v2ellostsl", "v2ellostsm","v2ellostss", "v2eltvrig", "v2psbars", "v2lginvstp",
            "v2lgoppart", "v2dlengage", "v2clstown", "v2csantimv", "v2mecenefm", "v2mecenefi",
            "v2mecrit", "v2mebias", "e_parreg", "e_ellonmpl", "e_ellonmpe", "v2x_mpi", "v2x_api",
            "v2x_frassoc_thick", "v2x_suffr", "v2xel_frefair", "v2x_elecoff", "v2x_freexp_thick",
            "e_fh_status", "e_democ", "e_polcomp", "v2elintmon",
            "e_uds_mean", "e_uds_median", "e_chga_demo", "e_h_polcon3")  #v2csanmvch has subcodes (anti-system movement type)
vdem.small <- vdem.short[myvars]
rm(vdem.short)

##Adjusting polity score to remove the values for xconst, which involve constraints (including judicial constraints) on the exec
vdem.small$xconst.value <- NA
vdem.small$xconst.value[vdem.small$e_exconst == 4] <- -1
vdem.small$xconst.value[vdem.small$e_exconst == 5] <- -2
vdem.small$xconst.value[vdem.small$e_exconst == 6] <- -3
vdem.small$xconst.value[vdem.small$e_exconst == 7] <- -4

vdem.small$xconst.value[vdem.small$e_exconst == 1] <- 3
vdem.small$xconst.value[vdem.small$e_exconst == 2] <- 2
vdem.small$xconst.value[vdem.small$e_exconst == 3] <- 1

vdem.small <- vdem.small %>% mutate(polity2.adj = e_polity2 + xconst.value)

##Adjusting V-Dem mpi and api variables to remove el_frefair component
vdem.small <- vdem.small %>% mutate(elcompindex.m = ifelse(v2xel_frefair == 0, 0, v2x_mpi / v2xel_frefair))
vdem.small <- vdem.small %>% mutate(elcompindex.a = v2x_api - (0.25*v2xel_frefair))


#Negshock variables

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

write.csv(vdem.small, "C:/Users/Cole/Documents/Research projects/Judicial independence and fraud/vdem-small-2018-post1943-tidy2.csv")

###Reading in vdem.small
vdem.small <- read.csv("C:/Users/Cole/Documents/Research projects/Judicial independence and fraud/vdem-small-2018-post1943-tidy2.csv")


###Adding DPI variables
library(countrycode)
dpi <- read.csv("C:/Users/Cole/Documents/Research topics literature/Database DPI2017/DPI2017_basefile_Jan2018.csv")
#dpi <- dpi %>% dplyr::select(1:100)

dpi <- dpi %>% mutate(COWcode = countrycode(ifs, origin = "wb", destination = "cown"))
dpi <- dpi %>% mutate(COWcode = ifelse(ifs == "CSK", 315, COWcode))
dpi <- dpi %>% mutate(COWcode = ifelse(ifs == "DDR", 265, COWcode))
dpi <- dpi %>% mutate(COWcode = ifelse(ifs == "ROM", 260, COWcode))
dpi <- dpi %>% mutate(COWcode = ifelse(ifs == "SUN", NA, COWcode))  ##Unclear
dpi <- dpi %>% mutate(COWcode = ifelse(ifs == "TMP", 860, COWcode))
dpi <- dpi %>% mutate(COWcode = ifelse(ifs == "YMD", NA, COWcode)) ##Unclear
dpi <- dpi %>% mutate(COWcode = ifelse(ifs == "YSR", NA, COWcode)) ##Unclear
dpi <- dpi %>% mutate(COWcode = ifelse(ifs == "ZAR", 490, COWcode))
dpi <- dpi %>% mutate(COWcode = ifelse(ifs == "0", NA, COWcode))

vdem.small <- vdem.small %>% left_join(dpi, by = c("COWcode", "year"))
vdem.small <- vdem.small %>% filter(is.na(COWcode) == F)


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
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(reform_positive.2lag = lag(reform_positive, 2))   

vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(reform_negative.lag = lag(reform_negative))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(reform_negative.2lag = lag(reform_negative, 2))   


##Laggin VDEM measure of judicial independence overall (jucon) by one year
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(jucon.lag = lag(v2x_jucon))   

###Creating a factor for each election period (the period between election A and B is named after B
vdem.small$country_election_period <- paste(vdem.small$country_name, as.character(vdem.small$next.election), sep="_")



##Lagging electoral manipulation DVs and other election-specific vars
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(v2elirreg.inv.lag = lag(v2elirreg.inv))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(v2elintim.inv.lag = lag(v2elintim.inv))   
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(v2eldommon.lag = lag(v2eldommon)) 
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(elexec.lag = lag(elexec))   
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(polity2.lag = lag(e_polity2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(polity2.adj.lag = lag(polity2.adj))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(opposition.oversight.lag = lag(v2lgoppart))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(loggpdpc.lag = lag(e_migdppcln))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(civil.society.lag = lag(v2xcs_ccsi))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(lc.ind.lag = lag(v2juncind, 1))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(hc.ind.lag = lag(v2juhcind, 1))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(jureform.lag = lag(v2jureform_ord, 1))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(altinfo.lag = lag(v2xme_altinf, 1))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(govseat.frac.lag = lag(govseat.frac, 1))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(clpol.lag = lag(v2x_clpol, 1))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(elcompindex.m.lag = lag(elcompindex.m, 1))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(elcompindex.a.lag = lag(elcompindex.a, 1))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(uds.mean.lag = lag(e_uds_mean, 1))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(uds.median.lag = lag(e_uds_median, 1))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(h_polcon3.lag = lag(e_h_polcon3, 1))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(e_chga_demo.lag = lag(e_chga_demo, 1))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(polcomp.lag = lag(e_polcomp, 1))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(parcomp.lag = lag(e_parcomp, 1))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(parreg.lag = lag(e_parreg, 1))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(purge.lag = lag(v2jupurge, 1))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(pack.lag = lag(v2jupack, 1))


##Lagging selection model variables by 2 years (so that they can predict the 1-year lag positive reform)
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(lc.ind.2lag = lag(v2juncind, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(hc.ind.2lag = lag(v2juhcind, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(oppaut.2lag = lag(v2psoppaut, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(polity.2lag = lag(e_polity2, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(polity2.adj.2lag = lag(polity2.adj, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(civil.society.2lag = lag(v2xcs_ccsi, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(opp.oversight.2lag = lag(v2lgoppart, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(urban.2lag = lag(e_miurbani, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(loggpdpc.2lag = lag(e_migdppcln, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(democracy.duration.2lag = lag(e_democracy_duration, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(exec.respectcon.2lag = lag(v2dlengage, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(altinfo.2lag = lag(v2xme_altinf, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(education.2lag = lag(e_peaveduc, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(leg.constraints.2lag = lag(v2xlg_legcon, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(govseat.frac.2lag = lag(govseat.frac, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(clpol.2lag = lag(v2x_clpol, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(elcompindex.m.2lag = lag(elcompindex.m, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(elcompindex.a.2lag = lag(elcompindex.a, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(uds.mean.2lag = lag(e_uds_mean, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(uds.median.2lag = lag(e_uds_median, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(h_polcon3.2lag = lag(e_h_polcon3, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(parcomp.2lag = lag(e_parcomp, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(parreg.2lag = lag(e_parreg, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(purge.2lag = lag(v2jupurge, 2))
vdem.small <- vdem.small %>% group_by(COWcode) %>% mutate(pack.2lag = lag(v2jupack, 2))






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
        # "e_ellonmpl",
        # "e_ellonmpe")

###Create a dataset of only the first years in each election cycle

data.firstyears <- vdem.small %>% group_by(country_election_period) %>% filter(year == first.year.cycle) %>% 
  dplyr::select(vars)

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

write.csv(vdem.small, "C:/Users/Cole/Documents/Research projects/Judicial independence and fraud/vdem-small-2018-post1943-tidy2.csv")

vdem.small <- read.csv("C:/Users/Cole/Documents/Research projects/Judicial independence and fraud/vdem-small-2018-post1943-tidy2.csv")

###Read in Melton and ginsburg
mg_data <- haven::read_dta("C:/Users/Cole/Desktop/MeltonGinsburg2014 - Replication Data/Stata/MeltonGinsburg2014_DJJI.dta")
mg_data <- mg_data %>% filter(year >= 1943)

mg_data <- mg_data %>% group_by(cowcode) %>% mutate(systid.lag = lag(systid))
mg_data <- mg_data %>%  mutate(djji_int = djji_sel*djji_rcon*djji_rem)

mg_data <- mg_data %>% group_by(cowcode) %>% mutate(djji_int.lag = lag(djji_int))
mg_data <- mg_data %>% group_by(cowcode) %>% mutate(djji_ji.lag = lag(djji_ji))
mg_data <- mg_data %>% group_by(cowcode) %>% mutate(djji_term.lag = lag(djji_term))
mg_data <- mg_data %>% group_by(cowcode) %>% mutate(djji_sel.lag = lag(djji_sel))
mg_data <- mg_data %>% group_by(cowcode) %>% mutate(djji_rcon.lag = lag(djji_rcon))
mg_data <- mg_data %>% group_by(cowcode) %>% mutate(djji_rem.lag = lag(djji_rem))
mg_data <- mg_data %>% group_by(cowcode) %>% mutate(djji_sal.lag = lag(djji_sal))

mg_data <- mg_data %>% rename(COWcode = cowcode)

vdem.small <- vdem.small %>% left_join(mg_data, by = c("COWcode", "year"))


###LJI


judicial <- read.csv("./LJI-estimates-20140422.csv")
vdem.small$lji <- NA
vdem.small$lji.lag <- NA
vdem.small$lji.2lag <- NA
i <- 1

for(i in 1:nrow(vdem.small)){
  tryCatch({
    cow <- as.numeric(vdem.small$COWcode[i])
    group <- subset(judicial, judicial$ccode == cow)
    loopyear <- as.numeric(vdem.small$year[i])
    lag.year <- loopyear - 1
    lag.year2 <- lag.year - 1
    #if (group$year == loopyear) mergeddata$lji[i] <- group.year$LJI else mergeddata$lji <- NA
    group.year <- subset(group, group$year == loopyear)
    vdem.small$lji[i] <- group.year$LJI
    group.year <- subset(group, group$year == lag.year)
    vdem.small$lji.lag[i] <- group.year$LJI
    group.year <- subset(group, group$year == lag.year2)
    vdem.small$lji.2lag[i] <- group.year$LJI
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}




###Make vdem.nodems

vdem.nodems <- subset(vdem.small, vdem.small$e_polity2 < 8) 
vdem.nodems <- subset(vdem.nodems, vdem.nodems$e_multiparty_elections == 1) #Only multiparty elections (stricter)

vdem.nodems.polcomp <- vdem.small %>% filter(e_polcomp < 8)
vdem.nodems.polcomp <- subset(vdem.nodems.polcomp, vdem.nodems.polcomp$e_multiparty_elections == 1) #Only multiparty elections (stricter)

vdem.nodems.cheibub <- subset(vdem.small, vdem.small$e_chga_demo == 0) 
vdem.nodems.cheibub <- subset(vdem.nodems.cheibub, vdem.nodems.cheibub$e_multiparty_elections == 1) #Only multiparty elections (stricter)

vdem.nodems.parcomp <- vdem.small %>% filter(e_parcomp < 5)
vdem.nodems.parcomp <- subset(vdem.nodems.parcomp, vdem.nodems.parcomp$e_multiparty_elections == 1) #Only multiparty elections (stricter)

vdem.nodems.parreg <- vdem.small %>% filter(e_parreg < 5)
vdem.nodems.parreg <- subset(vdem.nodems.parreg, vdem.nodems.parreg$e_multiparty_elections == 1) #Only multiparty elections (stricter)


rm(vdem.small)



###Writing vdem.nodems

###

###DO NOT OVERWRITE
###WRITE TO A NEW FILE INSTEAD
write.csv(vdem.nodems, "./vdem-2018-no-dems-post1945-polity-oct2020-condensed-tidy.csv")

write.csv(vdem.nodems.polcomp, "./vdem-2018-no-dems-polcomp-post1945-polity-oct2020-condensed-tidy.csv")

write.csv(vdem.nodems.parcomp, "./vdem-2018-no-dems-parcomp-post1945-oct2020-condensed-tidy.csv")

write.csv(vdem.nodems.parreg, "./vdem-2018-no-dems-parreg-post1945-oct2020-condensed-tidy.csv")

write.csv(vdem.nodems.cheibub, "./vdem-2018-no-dems-cheibub-post1945-oct2020-condensed-tidy.csv")


###Make vdem with dems for robustness checks

vdem.withdems <- subset(vdem.small, vdem.small$e_polity2 <= 9) #To update withdems, don't subset here at all
vdem.withdems <- subset(vdem.withdems, vdem.withdems$e_multiparty_elections == 1) 

###DO NOT OVERWRITE, WRITE TO NEW FILE EACH TIME FOR VERSION CONTROL
write.csv(vdem.withdems, "./vdem-2018-with-dems-post1945-polity-oct2020-condensed-tidy.csv")


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


###Creating an election period variable
###Restricted to data after 1989

vdem.nodems.elections.89 <- subset(vdem.nodems.elections, vdem.nodems.elections$year >= 1989 )

election.period <- matrix(1, ncol = 1, nrow = 1) #This is okay because the first ob, Afghanistan, has 1 election only
for(i in unique(vdem.nodems.elections.89$COWcode)){
  group <- subset(vdem.nodems.elections.89, vdem.nodems.elections.89$COWcode == i)
  group$election.period <- seq(1:nrow(group))
  election.period <- c(election.period, group$election.period)
}
election.period <- as.matrix(election.period)
election.period <- election.period[2:520 ,]

vdem.nodems.elections.89 <- cbind(vdem.nodems.elections.89, election.period)



###Getting election-year observations with values for effective number of parties by seats
vdem.small <- read.csv("C:/Users/Cole/Documents/Research projects/Judicial independence and fraud/vdem-small-2018-post1943-tidy2.csv")



vdem.ss <- vdem.small %>% dplyr::select(e_ellonmpl, year, COWcode, country_election_period) %>% filter(is.na(e_ellonmpl) == F)

vdem.ss <- vdem.ss %>% group_by(COWcode) %>% mutate(seat.share.lag = NA)

vdem.short2 <- vdem.small %>% left_join(vdem.ss, by = "country_election_period")

vdem.ss2 <- vdem.short2 %>% dplyr::select(seat.share.lag,
                                          e_ellonmpl.y, v2elirreg.inv, year.x, COWcode.x, country_election_period)

vdem.ss2.short <- vdem.ss2 %>% filter(is.na(v2elirreg.inv) == F)
#vdem.ss2.short <- vdem.ss2.short %>% mutate(seat.share.lag = seat.share.lag.x)

for (i in 1:nrow(vdem.ss2.short)){
  if(is.na(vdem.ss2.short$seat.share.lag[i])==TRUE){
    container <- vdem.ss2.short %>% filter(COWcode.x == vdem.ss2.short$COWcode.x[i])
    min.year <- min(container$year.x)
    country <- vdem.ss2.short$COWcode.x[i]
    prev.election.year <- min.year - 1
    row <- vdem.ss2.short %>% filter(COWcode.x == country & year.x == min.year)
    vdem.ss2.short$seat.share.lag[i] <- row$e_ellonmpl.y
  }
} 

#The above code searches by year, the below searches by the immediately prior row

for (i in 2:nrow(vdem.ss2.short)){
  if(is.na(vdem.ss2.short$seat.share.lag[i])==TRUE){
    country <- vdem.ss2.short$COWcode.x[i]
    row <- vdem.ss2.short[(i-1),]
    if(row$COWcode.x == country){
      vdem.ss2.short$seat.share.lag[i] <- row$e_ellonmpl.y
      
    }
    else{next}
  }
} 





vdem.ss2.short2 <- vdem.ss2.short %>% dplyr::select(1,4,5)
vdem.ss2.short2 <- vdem.ss2.short2 %>% rename(COWcode = COWcode.x)
vdem.ss2.short2 <- vdem.ss2.short2 %>% rename(year = year.x)



vdem.small <- vdem.small %>% left_join(vdem.ss2.short2, by = c("COWcode", "year"))
