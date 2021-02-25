require("deSolve")
library("ggplot2")
library("dplyr")
library("reshape2")
require(gridExtra)
library(ggpubr)
library(bsplus)
library(deSolve)
library(DT)
library(highcharter)
library(lubridate)
library(pushbar)
library(readxl)
library(reshape2)
library(scales)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinyhelper)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(XLConnect)
library(stringr)
library(RColorBrewer)
# library("comoOdeCpp")

#read data from excel file
setwd("C:/covid19/covid_age")
load("data_CoMo.RData")
file_path <- paste0(getwd(),"/Template_CoMoCOVID-19App_v17_all_interventions.xlsx")  
country_name<-"United Kingdom of Great Britain"
fit_mat <- read.table("fit_mat.txt",header = T)


# Cases
dta <- read_excel(file_path, sheet = "Cases")
names(dta) <- c("date", "cases", "deaths","rep_cases","rep_deaths")

cases_rv <- dta %>%
  mutate(date = as.Date(date), cumulative_death = cumsum(deaths)) %>%
  as.data.frame()

# Severity/Mortality
dta <- read_excel(file_path, sheet = "Severity-Mortality") 
names(dta) <- c("age_category",	"ifr",	"ihr")

mort_sever_rv <- dta %>%
  mutate(ihr = ihr/100) %>% # starting unit should be % - scaling to a value between 0 and 1
  mutate(ifr = ifr/max(ifr))  # scaling to a value between 0 and 1

# Population
dta <- read_excel(file_path, sheet = "Population")
names(dta) <- c("age_category",	"pop",	"birth",	"death")

population_rv <- dta %>%
  transmute(age_category, pop, birth, death)

# Parameters
param <- bind_rows(read_excel(file_path, sheet = "Parameters"),
                   read_excel(file_path, sheet = "Country Area Param"),
                   read_excel(file_path, sheet = "Virus Param"),
                   read_excel(file_path, sheet = "Hospitalisation Param"),
                   read_excel(file_path, sheet = "Interventions Param"),
                   read_excel(file_path, sheet = "Interventions")) %>%
  mutate(Value_Date = as.Date(Value_Date))


# START Bridge ----
popstruc <- population_rv %>% 
  select(age_category, pop) %>% 
  rename(agefloor = age_category) %>%
  as.data.frame()

popbirth <- population_rv %>% 
  select(age_category, birth) %>% 
  as.data.frame() # unit should be per person per day

mort <- population_rv %>% 
  pull(death) # unit should be per person per day

ihr <- mort_sever_rv %>% 
  select(age_category, ihr) %>% 
  as.data.frame()

ifr <- mort_sever_rv %>% 
  select(age_category, ifr) %>% 
  as.data.frame()


#########    POP AGEING
# per year ageing matrix
A<-length(popstruc[,2])
dd<-seq(1:A)/seq(1:A)
ageing <- t(diff(diag(dd),lag=1)/(5*365.25))
ageing<-cbind(ageing,0*seq(1:A)) # no ageing from last compartment

#
# pop<-population$country==country_name
# pp<-population$pop[pop]
###  CONTACT MATRICES
c_home <- contact_home[[country_name]] %>% as.matrix()
c_school <- contact_school[[country_name]] %>% as.matrix()
c_work <- contact_work[[country_name]] %>% as.matrix()
c_other <- contact_other[[country_name]] %>% as.matrix()
nce <-A-length(c_home[1,])

contact_home<-matrix(0,nrow=A,ncol=A)
contact_school<-matrix(0,nrow=A,ncol=A)
contact_work<-matrix(0,nrow=A,ncol=A)
contact_other<-matrix(0,nrow=A,ncol=A)

for (i in 1:(A-nce)){
  for (j in 1:(A-nce)){
    contact_home[i,j]<-c_home[i,j]
    contact_school[i,j]<-c_school[i,j]
    contact_work[i,j]<-c_work[i,j]
    contact_other[i,j]<-c_other[i,j]
  }
}

for (i in (A+1-nce):A){
  for (j in 1:(A-nce)){
    contact_home[i,j]<-c_home[(A-nce),j]
    contact_school[i,j]<-c_school[(A-nce),j]
    contact_work[i,j]<-c_work[(A-nce),j]
    contact_other[i,j]<-c_other[(A-nce),j]
  }
}
for (i in 1:(A-nce)){
  for (j in (A+1-nce):A){
    contact_home[i,j]<-c_home[i,(A-nce)]
    contact_school[i,j]<-c_school[i,(A-nce)]
    contact_work[i,j]<-c_work[i,(A-nce)]
    contact_other[i,j]<-c_other[i,(A-nce)]
  }
}
for (i in (A+1-nce):A){
  for (j in (A+1-nce):A){
    contact_home[i,j]<-c_home[(A-nce),(A-nce)]
    contact_school[i,j]<-c_school[(A-nce),(A-nce)]
    contact_work[i,j]<-c_work[(A-nce),(A-nce)]
    contact_other[i,j]<-c_other[(A-nce),(A-nce)]
  }
}



#########   INITIALISE SIMULATION/INTERVENTION START TIMES
startdate <- param$Value_Date[param$Parameter == "date_range_simul_start"]
stopdate <- param$Value_Date[param$Parameter == "date_range_simul_end"]
startdate <- startdate[1]
stopdate <- stopdate[1]


day_start <- as.numeric(startdate-startdate)
day_stop <- as.numeric(stopdate-startdate)
times <- seq(day_start, day_stop)

tin<-as.numeric(startdate-as.Date("2020-01-01"))/365.25
initP<-sum(popstruc[,2])       # population size 
ageindcase<-20                 # age of index case (years)
aci <- floor((ageindcase/5)+1) # age class of index case


#############   DEFINE PARAMETERS
parameters <- c(
  
  ###  Transmission instrinsic
  p = param$Value[param$Parameter=="p"][1],
  rho = param$Value[param$Parameter=="rho"][1],
  omega = param$Value[param$Parameter=="omega"][1],
  gamma = param$Value[param$Parameter=="gamma"][1],
  nui = param$Value[param$Parameter=="nui"][1],
  report = param$Value[param$Parameter=="report"][1],
  reportc = param$Value[param$Parameter=="reportc"][1],
  reporth = param$Value[param$Parameter=="reporth"][1],
  beds_available = param$Value[param$Parameter=="beds_available"][1],
  icu_beds_available = param$Value[param$Parameter=="icu_beds_available"][1],
  ventilators_available = param$Value[param$Parameter=="ventilators_available"][1],
  give = 95,
  pdeath_h = mean( param$Value[param$Parameter=="pdeath_h"],na.rm=T),
  pdeath_ho = mean( param$Value[param$Parameter=="pdeath_ho"],na.rm=T),
  pdeath_hc = mean( param$Value[param$Parameter=="pdeath_hc"],na.rm=T),
  pdeath_hco = mean( param$Value[param$Parameter=="pdeath_hco"],na.rm=T),
  pdeath_icu = mean( param$Value[param$Parameter=="pdeath_icu"],na.rm=T),
  pdeath_icuo = mean( param$Value[param$Parameter=="pdeath_icuo"],na.rm=T),
  pdeath_icuc = mean( param$Value[param$Parameter=="pdeath_icuc"],na.rm=T),
  pdeath_icuco = mean( param$Value[param$Parameter=="pdeath_icuco"],na.rm=T),
  pdeath_vent = mean( param$Value[param$Parameter=="pdeath_vent"],na.rm=T),
  pdeath_ventc = mean( param$Value[param$Parameter=="pdeath_ventc"],na.rm=T),
  ihr_scaling = param$Value[param$Parameter=="ihr_scaling"][1],
  nus = param$Value[param$Parameter=="nus"][1],
  nusc = param$Value[param$Parameter=="nus"][1], # nusc = nus
  nu_icu = param$Value[param$Parameter=="nu_icu"][1],
  nu_icuc = param$Value[param$Parameter=="nu_icu"][1],  # nu_icuc = nu_icu
  nu_vent = param$Value[param$Parameter=="nu_vent"][1],
  nu_ventc = param$Value[param$Parameter=="nu_vent"][1], # nu_ventc = nu_vent
  rhos = param$Value[param$Parameter=="rhos"][1],
  amp = param$Value[param$Parameter=="amp"][1],
  phi = param$Value[param$Parameter=="phi"][1],
  pclin = param$Value[param$Parameter=="pclin"][1],
  prob_icu = param$Value[param$Parameter=="prob_icu"][1],
  prob_vent = param$Value[param$Parameter=="prob_vent"][1],
  propo2 = param$Value[param$Parameter=="propo2"][1],
  dexo2 = mean( param$Value[param$Parameter=="dexo2"],na.rm=T),
  dexo2c = mean( param$Value[param$Parameter=="dexo2c"],na.rm=T),
  dexv = mean( param$Value[param$Parameter=="dexvc"],na.rm=T),
  dexvc = mean( param$Value[param$Parameter=="dexvc"],na.rm=T),
  vent_dex = mean(param$Value[param$Parameter=="vent_dex"],na.rm=T),
  prob_icu_v = mean(param$Value[param$Parameter=="prob_icu_v"],na.rm=T),
  prob_icu_vr = mean(param$Value[param$Parameter=="prob_icu_vr"],na.rm=T),
  prob_icu_r = mean(param$Value[param$Parameter=="prob_icu_r"],na.rm=T),
  prob_v_v = mean(param$Value[param$Parameter=="prob_v_v"],na.rm=T),
  prob_v_vr = mean(param$Value[param$Parameter=="prob_v_vr"],na.rm=T),
  prob_v_r = mean(param$Value[param$Parameter=="prob_v_r"],na.rm=T),
  pclin_v = mean(param$Value[param$Parameter=="pclin_v"],na.rm=T),
  pclin_vr = mean(param$Value[param$Parameter=="pclin_vr"],na.rm=T),
  pclin_r = mean(param$Value[param$Parameter=="pclin_r"],na.rm=T),
  sigmaEV = mean(param$Value[param$Parameter=="sigmaEV"],na.rm=T),
  sigmaEVR = mean(param$Value[param$Parameter=="sigmaEVR"],na.rm=T),
  sigmaER = mean(param$Value[param$Parameter=="sigmaER"],na.rm=T),
  sigmaR = mean(param$Value[param$Parameter=="sigmaR"],na.rm=T),
  vac_dur = mean(param$Value[param$Parameter=="vac_dur"],na.rm=T),
  vac_dur_r = mean(param$Value[param$Parameter=="vac_dur_r"],na.rm=T),
  report_natdeathI = mean(param$Value[param$Parameter=="report_natdeathI"],na.rm=T),
  report_natdeathCL = mean(param$Value[param$Parameter=="report_natdeathCL"],na.rm=T),
  pre = mean(param$Value[param$Parameter=="pre"],na.rm=T),
  report_v = param$Value[param$Parameter=="report_v"][1],
  report_cv = param$Value[param$Parameter=="report_cv"][1],
  report_vr = param$Value[param$Parameter=="report_vr"][1],
  report_cvr = param$Value[param$Parameter=="report_cvr"][1],
  report_r = param$Value[param$Parameter=="report_r"][1],
  report_cr = param$Value[param$Parameter=="report_cr"][1],
  reporth_ICU = param$Value[param$Parameter=="reporth_ICU"][1],
  report_death_HC = param$Value[param$Parameter=="report_death_HC"][1],
  pdeath_vent_hc = mean( param$Value[param$Parameter=="pdeath_vent_hc"],na.rm=T),
  pdeath_icu_hc = mean( param$Value[param$Parameter=="pdeath_icu_hc"],na.rm=T),
  pdeath_icu_hco = mean( param$Value[param$Parameter=="pdeath_icu_hco"],na.rm=T),
  reporth_g = param$Value[param$Parameter=="reporth_g"][1],
  seroneg = param$Value[param$Parameter=="seroneg"][1],
  sample_size = param$Value[param$Parameter=="sample_size"][1],
  
  ###  INTERVENTIONS
  # self isolation
  selfis_eff = mean(param$Value[param$Parameter=="selfis_eff"],na.rm=T),
  # social distancing
  dist_eff = mean(param$Value[param$Parameter=="dist_eff"],na.rm=T),
  # hand washing
  hand_eff = mean(param$Value[param$Parameter=="hand_eff"],na.rm=T),
  # mask wearing
  mask_eff = mean(param$Value[param$Parameter=="mask_eff"],na.rm=T),
  # working at home
  work_eff = mean(param$Value[param$Parameter=="work_eff"],na.rm=T),
  w2h = mean(param$Value[param$Parameter=="w2h"],na.rm=T),
  # school closures
  school_eff = mean(param$Value[param$Parameter=="school_eff"],na.rm=T),
  s2h = mean(param$Value[param$Parameter=="s2h"],na.rm=T),
  # cocooning the elderly
  cocoon_eff = mean(param$Value[param$Parameter=="cocoon_eff"],na.rm=T),
  age_cocoon = mean(param$Value[param$Parameter=="age_cocoon"],na.rm=T),
  # vaccination campaign
  # vaccine_on = as.numeric(param$Value_Date[param$Parameter=="date_vaccine_on"] - startdate),
  vaccine_eff = mean(param$Value[param$Parameter=="vaccine_eff"],na.rm=T),
  vaccine_eff_r = mean(param$Value[param$Parameter=="vaccine_eff_r"],na.rm=T),
  age_vaccine_min = mean(param$Value[param$Parameter=="age_vaccine_min"],na.rm=T),
  age_vaccine_max = mean(param$Value[param$Parameter=="age_vaccine_max"],na.rm=T),
  # vaccine_cov = param$Value[param$Parameter=="vaccine_cov"],
  vac_campaign = mean(param$Value[param$Parameter=="vac_campaign"],na.rm=T),
  # travel ban
  mean_imports = mean(param$Value[param$Parameter=="mean_imports"],na.rm=T),
  # screening
  screen_test_sens = mean(param$Value[param$Parameter=="screen_test_sens"],na.rm=T),
  # screen_contacts = mean(param$Value[param$Parameter=="screen_contacts"],na.rm=T),
  screen_overdispersion = mean(param$Value[param$Parameter=="screen_overdispersion"],na.rm=T),
  # voluntary home quarantine
  quarantine_days = mean(param$Value[param$Parameter=="quarantine_days"],na.rm=T),
  quarantine_effort = mean(param$Value[param$Parameter=="quarantine_effort"],na.rm=T),
  quarantine_eff_home = mean(param$Value[param$Parameter=="quarantine_eff_home"],na.rm=T),
  quarantine_eff_other = mean(param$Value[param$Parameter=="quarantine_eff_other"],na.rm=T),
  # mass testing
  age_testing_min = mean(param$Value[param$Parameter=="age_testing_min"],na.rm=T),
  age_testing_max = mean(param$Value[param$Parameter=="age_testing_max"],na.rm=T),
  mass_test_sens = mean(param$Value[param$Parameter=="mass_test_sens"],na.rm=T),
  isolation_days = mean(param$Value[param$Parameter=="isolation_days"],na.rm=T),
  
  ###  Initialisation
  init = param$Value[param$Parameter=="init"][1],
  
  ### Others
  household_size = param$Value[param$Parameter=="household_size"][1],
  noise = param$Value[param$Parameter=="noise"][1],
  iterations = param$Value[param$Parameter=="iterations"][1],
  confidence = param$Value[param$Parameter=="confidence"][1]
)
ihr[,2]<- parameters["ihr_scaling"]*ihr[,2]   
parameters["ifr_correction_young"]<-1
parameters["ifr_correction_old"]<-1
# ifr[1:12,2]<-ifr[1:12,2]/ifr_correction_young
# ihr$ihr[15:21]<-ihr$ihr[15:21]*ifr_correction_old

# Scale parameters to percentages/ rates
parameters["rho"]<-parameters["rho"]/100
parameters["omega"]<-(1/(parameters["omega"]*365))
parameters["gamma"]<-1/parameters["gamma"]
parameters["nui"]<-1/parameters["nui"]
parameters["report"]<-parameters["report"]/100
parameters["reportc"]<-parameters["reportc"]/100
parameters["report_v"]<-parameters["report_v"]/100
parameters["report_cv"]<-parameters["report_cv"]/100
parameters["report_vr"]<-parameters["report_vr"]/100
parameters["report_cvr"]<-parameters["report_cvr"]/100
parameters["report_r"]<-parameters["report_r"]/100
parameters["report_cr"]<-parameters["report_cr"]/100
parameters["reporth"]<-parameters["reporth"]/100
parameters["nus"]<-1/parameters["nus"]
parameters["rhos"]<-parameters["rhos"]/100
parameters["amp"]<-parameters["amp"]/100
parameters["selfis_eff"]<-parameters["selfis_eff"]/100
parameters["dist_eff"]<-parameters["dist_eff"]/100
parameters["hand_eff"]<-parameters["hand_eff"]/100
parameters["mask_eff"]<-parameters["mask_eff"]/100
parameters["work_eff"]<-parameters["work_eff"]/100
parameters["w2h"]<-parameters["w2h"]/100
parameters["school_eff"]<-parameters["school_eff"]/100
parameters["s2h"]<-parameters["s2h"]/100
parameters["cocoon_eff"]<-parameters["cocoon_eff"]/100
parameters["age_cocoon"]<-floor((parameters["age_cocoon"]/5)+1)
parameters["vaccine_eff"]<-parameters["vaccine_eff"]/100
parameters["vaccine_eff_r"]<-parameters["vaccine_eff_r"]/100
age_vaccine_min<-(parameters["age_vaccine_min"])
age_vaccine_max<-(parameters["age_vaccine_max"])
# parameters["vaccine_cov"]<-parameters["vaccine_cov"]/100
# parameters["vac_campaign"]<-parameters["vac_campaign"]*7
parameters["screen_test_sens"]<-parameters["screen_test_sens"]/100
parameters["quarantine_days"]<-parameters["quarantine_days"]
parameters["quarantine_effort"]<-1/parameters["quarantine_effort"]
parameters["quarantine_eff_home"]<-parameters["quarantine_eff_home"]/-100
parameters["quarantine_eff_other"]<-parameters["quarantine_eff_other"]/100
parameters["give"]<-parameters["give"]/100
parameters["pdeath_h"]<-parameters["pdeath_h"]/100
parameters["pdeath_ho"]<-parameters["pdeath_ho"]/100
parameters["pdeath_hc"]<-parameters["pdeath_hc"]/100
parameters["pdeath_hco"]<-parameters["pdeath_hco"]/100
parameters["pdeath_icu"]<-parameters["pdeath_icu"]/100
parameters["pdeath_icuo"]<-parameters["pdeath_icuo"]/100
parameters["pdeath_icuc"]<-parameters["pdeath_icuc"]/100
parameters["pdeath_icuco"]<-parameters["pdeath_icuco"]/100
parameters["pdeath_vent"]<-parameters["pdeath_vent"]/100
parameters["pdeath_ventc"]<-parameters["pdeath_ventc"]/100
parameters["nusc"]<-1/parameters["nusc"]
parameters["nu_icu"]<-1/parameters["nu_icu"]
parameters["nu_icuc"]<-1/parameters["nu_icuc"]
parameters["nu_vent"]<-1/parameters["nu_vent"]
parameters["nu_ventc"]<-1/parameters["nu_ventc"]
parameters["pclin"]<-parameters["pclin"]/100
parameters["prob_icu"]<-parameters["prob_icu"]/100
parameters["prob_vent"]<-parameters["prob_vent"]/100
iterations<-parameters["iterations"]
noise<-parameters["noise"]
confidence<-parameters["confidence"]/100
parameters["mass_test_sens"]<-parameters["mass_test_sens"]/100
age_testing_min<-(parameters["age_testing_min"])
age_testing_max<-(parameters["age_testing_max"])
parameters["isolation_days"]<-parameters["isolation_days"]
parameters["propo2"]<-parameters["propo2"]/100
parameters["dexo2"]<-parameters["dexo2"]/100
parameters["dexo2c"]<-parameters["dexo2c"]/100
parameters["dexv"]<-parameters["dexv"]/100
parameters["dexvc"]<-parameters["dexvc"]/100
parameters["vent_dex"]<-parameters["vent_dex"]/100
parameters["prob_icu_v"]<-parameters["prob_icu_v"]/100
parameters["prob_icu_vr"]<-parameters["prob_icu_vr"]/100
parameters["prob_icu_r"]<-parameters["prob_icu_r"]/100
parameters["prob_v_v"]<-parameters["prob_v_v"]/100
parameters["prob_v_r"]<-parameters["prob_v_r"]/100
parameters["prob_v_vr"]<-parameters["prob_v_vr"]/100
parameters["pclin_v"]<-parameters["pclin_v"]/100
parameters["pclin_vr"]<-parameters["pclin_vr"]/100
parameters["pclin_r"]<-parameters["pclin_r"]/100
parameters["sigmaEV"]<-parameters["sigmaEV"]/100
parameters["sigmaER"]<-parameters["sigmaER"]/100
parameters["sigmaEVR"]<-parameters["sigmaEVR"]/100
parameters["sigmaR"]<-parameters["sigmaR"]/100
parameters["vac_dur"]<-1/parameters["vac_dur"]/100
parameters["vac_dur_r"]<-1/parameters["vac_dur_r"]/100
parameters["report_natdeathI"]<-parameters["report_natdeathI"]/100
parameters["report_natdeathCL"]<-parameters["report_natdeathCL"]/100
parameters["report_death_HC"]<-parameters["report_death_HC"]/100
parameters["reporth_ICU"]<-parameters["reporth_ICU"]/100
parameters["pre"]<-parameters["pre"]/100
parameters["pdeath_vent_hc"]<-parameters["pdeath_vent_hc"]/100
parameters["pdeath_icu_hc"]<-parameters["pdeath_icu_hc"]/100
parameters["pdeath_icu_hco"]<-parameters["pdeath_icu_hco"]/100
parameters["reporth_g"]<-parameters["reporth_g"]/100
parameters["seroneg"]<-(1/parameters["seroneg"])


parameters_noise <- c("p", "rho", "omega", "gamma", "nui", "ihr_scaling","nus", "nu_icu","nu_vent",
                      "rhos", "selfis_eff", "dist_eff", "hand_eff", "mask_eff", "work_eff", 
                      "w2h", "s2h", "cocoon_eff", "mean_imports", "screen_overdispersion", 
                      "quarantine_effort", "quarantine_eff_home", "quarantine_eff_other")

# parameters_fit <- c("p", "ihr_scaling","ifr_correction_young","ifr_correction_old","init")
parameters_fit <- rownames(fit_mat)
###########################################################################
# Define the indices for each variable
Sindex<-1:A
Eindex<-(A+1):(2*A)
Iindex<-(2*A+1):(3*A)
Rindex<-(3*A+1):(4*A)
Xindex<-(4*A+1):(5*A)
Hindex<-(5*A+1):(6*A)
HCindex<-(6*A+1):(7*A)
Cindex<-(7*A+1):(8*A)
CMindex<-(8*A+1):(9*A)
Vindex<-(9*A+1):(10*A)
QSindex<-(10*A+1):(11*A)
QEindex<-(11*A+1):(12*A)
QIindex<-(12*A+1):(13*A)
QRindex<-(13*A+1):(14*A)
CLindex<-(14*A+1):(15*A)
QCindex<-(15*A+1):(16*A)
ICUindex<-(16*A+1):(17*A)
ICUCindex<-(17*A+1):(18*A)
ICUCVindex<-(18*A+1):(19*A)
Ventindex<-(19*A+1):(20*A)
VentCindex<-(20*A+1):(21*A)
CMCindex<-(21*A+1):(22*A)
Zindex<-(22*A+1):(23*A)
EVindex<-(23*A+1):(24*A)
ERindex<-(24*A+1):(25*A)
EVRindex<-(25*A+1):(26*A)
VRindex<-(26*A+1):(27*A)
QVindex<-(27*A+1):(28*A)
QEVindex<-(28*A+1):(29*A)
QEVRindex<-(29*A+1):(30*A)
QERindex<-(30*A+1):(31*A)
QVRindex<-(31*A+1):(32*A)
HCICUindex<-(32*A+1):(33*A)
HCVindex<-(33*A+1):(34*A)
Abindex<-(34*A+1):(35*A)

###########################################################################
# MODEL INITIAL CONDITIONS
initI<-0*popstruc[,2]  # Infected and symptomatic
initE<-0*popstruc[,2]  # Incubating
# initE[aci]<-1          # place random index case in E compartment
initE[aci]<-parameters["init"]     # place random index case in E compartment
initR<-parameters["pre"]*popstruc[,2]  # Immune
initX<-0*popstruc[,2]  # Isolated 
initV<-0*popstruc[,2]  # Vaccinated 
initQS<-0*popstruc[,2] # quarantined S 
initQE<-0*popstruc[,2] # quarantined E  
initQI<-0*popstruc[,2] # quarantined I  
initQR<-0*popstruc[,2] # quarantined R  
initH<-0*popstruc[,2]  # hospitalised 
initHC<-0*popstruc[,2] # hospital critical 
initC<-0*popstruc[,2]  # Cumulative cases (true)
initCM<-0*popstruc[,2] # Cumulative deaths (true)
initCL<-0*popstruc[,2] # symptomatic cases
initQC<-0*popstruc[,2] # quarantined C 
initICU<-0*popstruc[,2]   # icu
initICUC<-0*popstruc[,2]  # icu critical
initICUCV<-0*popstruc[,2] # icu critical
initVent<-0*popstruc[,2]  # icu vent
initVentC<-0*popstruc[,2] # icu vent crit
initCMC<-0*popstruc[,2]   # Cumulative deaths - overload (true)
initZ<-0*popstruc[,2]     # testing - quarantined (true)
initEV<-0*popstruc[,2]    # vaccinated exposed
initER<-0*popstruc[,2]    # recovered exposed
initEVR<-0*popstruc[,2]   # recovered and vaccinated exposed
initVR<-0*popstruc[,2]    # recovered and vaccinated
initQV<-0*popstruc[,2]    # quarantined and vaccinated
initQEV<-0*popstruc[,2]   # quarantined, exposed and vaccinated
initQEVR<-0*popstruc[,2]  # quarantined, exposed, recovered and vaccinated
initQER<-0*popstruc[,2]   # quarantined, exposed and recovered
initQVR<-0*popstruc[,2]   # quarantined, recovered and vaccinated
initHCICU<-0*popstruc[,2] # icu not seeking
initHCV<-0*popstruc[,2]   # ventilator not seeking
initAb<-0*popstruc[,2]   # ventilator not seeking

initS<-popstruc[,2]-initE-initI-initCL-initR-initX-initZ-initV-initH-initHC-initICU-initICUC-initICUCV-initVent-initVentC-
  initQS-initQE-initQI-initQR-initQC-initEV-initER-initEVR-initVR-initQV-initQEV-initQEVR-initQER-initQVR-
  initHCICU-initHCV # Susceptible (non-immune)


inp <- read_excel(file_path, sheet = "Interventions") %>%
  filter(! is.na(Intervention))
# Test if listed interventions are valid
valid_interventions_v17 <- c("Dexamethasone", "Handwashing", "International Travel Ban",
                             "Mask Wearing", "Mass Testing", "School Closures", "Self-isolation if Symptomatic",
                             "(*Self-isolation) Household Isolation", "(*Self-isolation) Screening", "Shielding the Elderly",
                             "Social Distancing", "Vaccination", "Working at Home")
if(all(inp$Intervention %in% valid_interventions_v17)) message("Okay, all interventions are valid.")
if(!all(inp$Intervention %in% valid_interventions_v17)) stop("Stop, some interventions are not valid.")
# complte the age_groups column
inp$`Age Groups`[is.na(inp$`Age Groups`)] <- "1-21"
vec<- inp$`Age Groups`


parse_age_group <- function(vec) {
  regx_str <- "^(([0]*([1-9]|1[0-9]|2[0-1]))[\\,\\:])*([0]*([1-9]|1[0-9]|2[0-1]))$"
  output <- rep(FALSE, 21)
  
  vec <- str_replace_all(vec, "-", ":")
  vec <- str_replace_all(vec, ";", ",")
  vec <- str_replace_all(vec, "[:space:]", "")
  vec <- str_replace_all(vec, "[:alpha:]", "")
  if (!str_detect(vec, regx_str)) {
    return(output)
  }
  vec <- paste0("c(", vec, ")")
  vec2 <- eval(parse(text = vec))
  output[vec2] <- TRUE
  return(output)
}

inp$Target<-rep(0,length(inp$Value))
age_group_vectors <- list()
for (i in 1:length(vec)){
  pp<-parse_age_group(vec[i])
  age_group_vectors[[i]] <- pp
  inp$Target[i]<-i
}

inputs<-function(inp, run){
  # cap intervention start and end dates with simulation end date
  inp$`Date Start` = pmin(stopdate, inp$`Date Start`)
  inp$`Date End` = pmin(stopdate, inp$`Date End`)
  inp <- inp %>% arrange(`Date Start`)
  # print(inp)
  tv<-which(inp$`Apply to`==run)

  si<-intersect(which(inp$Intervention=="Self-isolation if Symptomatic"),tv)
  scr<-intersect(which(inp$Intervention=="(*Self-isolation) Screening"),tv)
  sd<-intersect(which(inp$Intervention=="Social Distancing"),tv)
  hw<-intersect(which(inp$Intervention=="Handwashing"),tv)
  msk<-intersect(which(inp$Intervention=="Mask Wearing"),tv)
  wah<-intersect(which(inp$Intervention=="Working at Home"),tv)
  sc<-intersect(which(inp$Intervention=="School Closures"),tv)
  scp<-intersect(which(inp$Intervention=="Partial School Closures"),tv)
  # scc<-intersect(which(inp$Intervention=="School Group Code"),tv)
  cte<-intersect(which(inp$Intervention=="Shielding the Elderly"),tv)
  q<-intersect(which(inp$Intervention=="(*Self-isolation) Household Isolation"),tv)
  tb<-intersect(which(inp$Intervention=="International Travel Ban"),tv)
  vc<-intersect(which(inp$Intervention=="Vaccination"),tv)
  vcp<-intersect(which(inp$Intervention=="Partial Vaccination"),tv)
  mt<-intersect(which(inp$Intervention=="Mass Testing"),tv)
  dx<-intersect(which(inp$Intervention=="Dexamethasone"),tv)
  
  v<-(format(as.POSIXct(inp$`Date Start`,format='%Y/%m/%d %H:%M:%S'),format="%d/%m/%y"))
  v2<-as.Date(v,format="%d/%m/%y")
  inp$`Date Start`<-v2
  
  v<-(format(as.POSIXct(inp$`Date End`,format='%Y/%m/%d %H:%M:%S'),format="%d/%m/%y"))
  v2<-as.Date(v,format="%d/%m/%y")
  inp$`Date End`<-v2
  
  ##  self isolation
  f<-c()
  si_vector<-c()
  isolation<-c()
  if (length(si)>=1){
    for (i in 1:length(si)){
      f<-c(f,as.numeric(inp$`Date Start`[si[i]]-startdate),as.numeric(inp$`Date End`[si[i]]-startdate))
      # print(f)
      if(i==1){
        if (inp$`Date Start`[si[i]]>startdate){
          si_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[si[i]],(f[i+1]-f[i])*20))
          isolation<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          si_vector<-c(rep(inp$`Value`[si[i]],(f[i+1])*20))
          isolation<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          si_vector<-c(si_vector,rep(inp$`Value`[si[i]],20))
          isolation<-c(isolation,rep(1,20))
        }else{
          si_vector<-c(si_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          isolation<-c(isolation,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        si_vector<-c(si_vector,rep(inp$`Value`[si[i]],(f[i*2]-f[i*2-1])*20))
        isolation<-c(isolation,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(si) && f[i*2]<tail(times,1)){
       si_vector<-c(si_vector,rep(0,(tail(times,1)-f[i*2])*20))
       isolation<-c(isolation,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    si_vector<-rep(0,tail(times,1)*20)
    isolation<-rep(0,tail(times,1)*20)
  }
  ## social distancing
  f<-c()
  sd_vector<-c()
  distancing<-c()
  if (length(sd)>=1){
    for (i in 1:length(sd)){
      
      f<-c(f,as.numeric(inp$`Date Start`[sd[i]]-startdate),as.numeric(inp$`Date End`[sd[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[sd[i]]>startdate){
          sd_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[sd[i]],(f[i+1]-f[i])*20))
          distancing<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          sd_vector<-c(rep(inp$`Value`[sd[i]],(f[i+1])*20))
          distancing<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          sd_vector<-c(sd_vector,rep(inp$`Value`[sd[i]],20))
          distancing<-c(distancing,rep(1,20))
        }else{
          sd_vector<-c(sd_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          distancing<-c(distancing,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        sd_vector<-c(sd_vector,rep(inp$`Value`[sd[i]],(f[i*2]-f[i*2-1])*20))
        distancing<-c(distancing,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(sd)&& f[i*2]<tail(times,1)){
        sd_vector<-c(sd_vector,rep(0,(tail(times,1)-f[i*2])*20))
        distancing<-c(distancing,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    sd_vector<-rep(0,tail(times,1)*20)
    distancing<-rep(0,tail(times,1)*20)
  }
  ## screening
  f<-c()
  scr_vector<-c()
  screen<-c()
  if (length(scr)>=1){
    for (i in 1:length(scr)){
      f<-c(f,as.numeric(inp$`Date Start`[scr[i]]-startdate),as.numeric(inp$`Date End`[scr[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[scr[i]]>startdate){
          scr_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[scr[i]],(f[i+1]-f[i])*20))
          screen<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          scr_vector<-c(rep(inp$`Value`[scr[i]],(f[i+1])*20))
          screen<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          scr_vector<-c(scr_vector,rep(inp$`Value`[scr[i]],20))
          screen<-c(screen,rep(1,20))
        }else{
          scr_vector<-c(scr_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          screen<-c(screen,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        scr_vector<-c(scr_vector,rep(inp$`Value`[scr[i]],(f[i*2]-f[i*2-1])*20))
        screen<-c(screen,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(scr)&& f[i*2]<tail(times,1)){
        scr_vector<-c(scr_vector,rep(0,(tail(times,1)-f[i*2])*20))
        screen<-c(screen,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    scr_vector<-rep(0,tail(times,1)*20)
    screen<-rep(0,tail(times,1)*20)
  }
  ## handwashing
  f<-c()
  hw_vector<-c()
  handwash<-c()
  if (length(hw)>=1){
    for (i in 1:length(hw)){
      
      f<-c(f,as.numeric(inp$`Date Start`[hw[i]]-startdate),as.numeric(inp$`Date End`[hw[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[hw[i]]>startdate){
          hw_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[hw[i]],(f[i+1]-f[i])*20))
          handwash<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          hw_vector<-c(rep(inp$`Value`[hw[i]],(f[i+1])*20))
          handwash<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          hw_vector<-c(hw_vector,rep(inp$`Value`[hw[i]],20))
          handwash<-c(handwash,rep(1,20))
        }else{
          hw_vector<-c(hw_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          handwash<-c(handwash,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        hw_vector<-c(hw_vector,rep(inp$`Value`[hw[i]],(f[i*2]-f[i*2-1])*20))
        handwash<-c(handwash,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(hw)&& f[i*2]<tail(times,1)){
        hw_vector<-c(hw_vector,rep(0,(tail(times,1)-f[i*2])*20))
        handwash<-c(handwash,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    hw_vector<-rep(0,tail(times,1)*20)
    handwash<-rep(0,tail(times,1)*20)
  }
  ## masking
  f<-c()
  msk_vector<-c()
  masking<-c()
  if (length(msk)>=1){
    for (i in 1:length(msk)){
      
      f<-c(f,as.numeric(inp$`Date Start`[msk[i]]-startdate),as.numeric(inp$`Date End`[msk[i]]-startdate))
      if(i==1){
        if (inp$`Date Start`[msk[i]]>startdate){
          msk_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[msk[i]],(f[i+1]-f[i])*20))
          masking<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          msk_vector<-c(rep(inp$`Value`[msk[i]],(f[i+1])*20))
          masking<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          msk_vector<-c(msk_vector,rep(inp$`Value`[msk[i]],20))
          masking<-c(masking,rep(1,20))
        }else{
          msk_vector<-c(msk_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          masking<-c(masking,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        msk_vector<-c(msk_vector,rep(inp$`Value`[msk[i]],(f[i*2]-f[i*2-1])*20))
        masking<-c(masking,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(msk)&& f[i*2]<tail(times,1)){
        msk_vector<-c(msk_vector,rep(0,(tail(times,1)-f[i*2])*20))
        masking<-c(masking,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    msk_vector<-rep(0,tail(times,1)*20)
    masking<-rep(0,tail(times,1)*20)
  }
  ## dexamethasone
  f<-c()
  dex<-c()
  if (length(dx)>=1){
    for (i in 1:length(dx)){
      f<-c(f,as.numeric(inp$`Date Start`[dx[i]]-startdate),as.numeric(inp$`Date End`[dx[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[dx[i]]>startdate){
          dex<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          dex<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          dex<-c(dex,rep(1,20))
        }else{
          dex<-c(dex,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        dex<-c(dex,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(dx)&& f[i*2]<tail(times,1)){
        dex<-c(dex,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    dex<-rep(0,tail(times,1)*20)
  }
  ## working at home
  f<-c()
  wah_vector<-c()
  workhome<-c()
  if (length(wah)>=1){
    for (i in 1:length(wah)){
      
      f<-c(f,as.numeric(inp$`Date Start`[wah[i]]-startdate),as.numeric(inp$`Date End`[wah[i]]-startdate))
      if(i==1){
        if (inp$`Date Start`[wah[i]]>startdate){
          wah_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[wah[i]],(f[i+1]-f[i])*20))
          workhome<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          wah_vector<-c(rep(inp$`Value`[wah[i]],(f[i+1])*20))
          workhome<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          wah_vector<-c(wah_vector,rep(inp$`Value`[wah[i]],20))
          workhome<-c(workhome,rep(1,20))
        }else{
          wah_vector<-c(wah_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          workhome<-c(workhome,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        wah_vector<-c(wah_vector,rep(inp$`Value`[wah[i]],(f[i*2]-f[i*2-1])*20))
        workhome<-c(workhome,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(wah)&& f[i*2]<tail(times,1)){
        wah_vector<-c(wah_vector,rep(0,(tail(times,1)-f[i*2])*20))
        workhome<-c(workhome,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    wah_vector<-rep(0,tail(times,1)*20)
    workhome<-rep(0,tail(times,1)*20)
  }
  ## school closure
  f<-c()
  sc_vector<-c()
  schoolclose<-c()
  if (length(sc)>=1){
    for (i in 1:length(sc)){
      f<-c(f,as.numeric(inp$`Date Start`[sc[i]]-startdate),as.numeric(inp$`Date End`[sc[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[sc[i]]>startdate){
          sc_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[sc[i]],(f[i+1]-f[i])*20))
          schoolclose<-c(rep(0,f[i]*20),rep(inp$Target[sc[i]],(f[i+1]-f[i])*20))
        }
        else{
          sc_vector<-c(rep(inp$`Value`[sc[i]],(f[i+1])*20))
          schoolclose<-c(rep(inp$Target[sc[i]],(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          sc_vector<-c(sc_vector,rep(inp$`Value`[sc[i]],20))
          schoolclose<-c(schoolclose,rep(inp$Target[sc[i]],20))
        }else{
          sc_vector<-c(sc_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          schoolclose<-c(schoolclose,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        sc_vector<-c(sc_vector,rep(inp$`Value`[sc[i]],(f[i*2]-f[i*2-1])*20))
        schoolclose<-c(schoolclose,rep(inp$Target[sc[i]],(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(sc)&& f[i*2]<tail(times,1)){
        sc_vector<-c(sc_vector,rep(0,(tail(times,1)-f[i*2])*20))
        schoolclose<-c(schoolclose,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    sc_vector<-rep(0,tail(times,1)*20)
    schoolclose<-rep(0,tail(times,1)*20)
  }
  schoolclose[is.na(schoolclose)]<-1
  ## partial school closure
  f<-c()
  scp_vector<-c()
  schoolclosepartial<-c()
  if (length(scp)>=1){
    for (i in 1:length(scp)){
      f<-c(f,as.numeric(inp$`Date Start`[sc[i]]-startdate),as.numeric(inp$`Date End`[scp[i]]-startdate))
      if(i==1){
        if (inp$`Date Start`[scp[i]]>startdate){
          scp_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[scp[i]],(f[i+1]-f[i])*20))
          schoolclosepartial<-c(rep(0,f[i]*20),rep(inp$Target[scp[i]],(f[i+1]-f[i])*20))
        }
        else{
          scp_vector<-c(rep(inp$`Value`[scp[i]],(f[i+1])*20))
          schoolclosepartial<-c(rep(inp$Target[scp[i]],(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          scp_vector<-c(scp_vector,rep(inp$`Value`[scp[i]],20))
          schoolclosepartial<-c(schoolclosepartial,rep(inp$Target[scp[i]],20))
        }else{
          scp_vector<-c(scp_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          schoolclosepartial<-c(schoolclosepartial,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        scp_vector<-c(scp_vector,rep(inp$`Value`[scp[i]],(f[i*2]-f[i*2-1])*20))
        schoolclosepartial<-c(schoolclosepartial,rep(inp$Target[scp[i]],(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(scp)&& f[i*2]<tail(times,1)){
        scp_vector<-c(scp_vector,rep(0,(tail(times,1)-f[i*2])*20))
        schoolclosepartial<-c(schoolclosepartial,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    scp_vector<-rep(0,tail(times,1)*20)
    schoolclosepartial<-rep(0,tail(times,1)*20)
  }
  schoolclosepartial[is.na(schoolclosepartial)]<-1
  ## cocooning the elderly
  f<-c()
  cte_vector<-c()
  cocoon<-c()
  if (length(cte)>=1){
    for (i in 1:length(cte)){
      
      f<-c(f,as.numeric(inp$`Date Start`[cte[i]]-startdate),as.numeric(inp$`Date End`[cte[i]]-startdate))
      if(i==1){
        if (inp$`Date Start`[cte[i]]>startdate){
          cte_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[cte[i]],(f[i+1]-f[i])*20))
          cocoon<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          cte_vector<-c(rep(inp$`Value`[cte[i]],(f[i+1])*20))
          cocoon<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          cte_vector<-c(cte_vector,rep(inp$`Value`[cte[i]],20))
          cocoon<-c(cocoon,rep(1,20))
        }else{
          cte_vector<-c(cte_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          cocoon<-c(cocoon,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        cte_vector<-c(cte_vector,rep(inp$`Value`[cte[i]],(f[i*2]-f[i*2-1])*20))
        cocoon<-c(cocoon,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(cte)&& f[i*2]<tail(times,1)){
        cte_vector<-c(cte_vector,rep(0,(tail(times,1)-f[i*2])*20))
        cocoon<-c(cocoon,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    cte_vector<-rep(0,tail(times,1)*20)
    cocoon<-rep(0,tail(times,1)*20)
  }
  ## quarantine
  f<-c()
  q_vector<-c()
  quarantine<-c()
  if (length(q)>=1){
    for (i in 1:length(q)){
      
      f<-c(f,as.numeric(inp$`Date Start`[q[i]]-startdate),as.numeric(inp$`Date End`[q[i]]-startdate))
      if(i==1){
        if (inp$`Date Start`[q[i]]>startdate){
          q_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[q[i]],(f[i+1]-f[i])*20))
          quarantine<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          q_vector<-c(rep(inp$`Value`[q[i]],(f[i+1])*20))
          quarantine<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          q_vector<-c(q_vector,rep(inp$`Value`[q[i]],20))
          quarantine<-c(quarantine,rep(1,20))
        }else{
          q_vector<-c(q_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          quarantine<-c(quarantine,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        q_vector<-c(q_vector,rep(inp$`Value`[q[i]],(f[i*2]-f[i*2-1])*20))
        quarantine<-c(quarantine,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(q)&& f[i*2]<tail(times,1)){
        q_vector<-c(q_vector,rep(0,(tail(times,1)-f[i*2])*20))
        quarantine<-c(quarantine,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    q_vector<-rep(0,tail(times,1)*20)
    quarantine<-rep(0,tail(times,1)*20)
  }
  ## travel ban
  f<-c()
  tb_vector<-c()
  travelban<-c()
  if (length(tb)>=1){
    for (i in 1:length(tb)){
      f<-c(f,as.numeric(inp$`Date Start`[tb[i]]-startdate),as.numeric(inp$`Date End`[tb[i]]-startdate))
      if(i==1){
        if (inp$`Date Start`[tb[i]]>startdate){
          tb_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[tb[i]],(f[i+1]-f[i])*20))
          travelban<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          tb_vector<-c(rep(inp$`Value`[tb[i]],(f[i+1])*20))
          travelban<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          tb_vector<-c(tb_vector,rep(inp$`Value`[tb[i]],20))
          travelban<-c(travelban,rep(1,20))
        }else{
          tb_vector<-c(tb_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          travelban<-c(travelban,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        tb_vector<-c(tb_vector,rep(inp$`Value`[tb[i]],(f[i*2]-f[i*2-1])*20))
        travelban<-c(travelban,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(tb)&& f[i*2]<tail(times,1)){
        tb_vector<-c(tb_vector,rep(0,(tail(times,1)-f[i*2])*20))
        travelban<-c(travelban,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    tb_vector<-rep(0,tail(times,1)*20)
    travelban<-rep(0,tail(times,1)*20)
  }
  ## mass testing
  f<-c()
  mt_vector<-c()
  masstesting<-c()
  testage<-c()
  if (length(mt)>=1){
    for (i in 1:length(mt)){
      f<-c(f,as.numeric(inp$`Date Start`[mt[i]]-startdate),as.numeric(inp$`Date End`[mt[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[mt[i]]>startdate){
          mt_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[mt[i]],(f[i+1]-f[i])*20))
          masstesting<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
          testage<-c(rep(0,f[i]*20),rep(inp$`Target`[mt[i]],(f[i+1]-f[i])*20))
        }
        else{
          mt_vector<-c(rep(inp$`Value`[mt[i]],(f[i+1])*20))
          masstesting<-c(rep(1,(f[i+1])*20))
          testage<-c(rep(inp$`Target`[mt[i]],(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          mt_vector<-c(mt_vector,rep(inp$`Value`[mt[i]],20))
          masstesting<-c(masstesting,rep(1,20))
          testage<-c(testage,rep(inp$`Target`[mt[i]],20))
        }else{
          mt_vector<-c(mt_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          masstesting<-c(masstesting,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          testage<-c(testage,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        mt_vector<-c(mt_vector,rep(inp$`Value`[mt[i]],(f[i*2]-f[i*2-1])*20))
        masstesting<-c(masstesting,rep(1,(f[i*2]-f[i*2-1])*20))
        testage<-c(testage,rep(inp$`Target`[mt[i]],(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(mt)&& f[i*2]<tail(times,1)){
        mt_vector<-c(mt_vector,rep(0,(tail(times,1)-f[i*2])*20))
        masstesting<-c(masstesting,rep(0,(tail(times,1)-f[i*2])*20))
        testage<-c(testage,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    mt_vector<-rep(0,tail(times,1)*20)
    masstesting<-rep(0,tail(times,1)*20)
    testage<-rep(0,tail(times,1)*20)
  }
  ## vaccine
  f<-c()
  vc_vector<-c()
  vaccine<-c()
  vaccineage<-c()
  if (length(vc)>=1){
    for (i in 1:length(vc)){
      f<-c(f,as.numeric(inp$`Date Start`[vc[i]]-startdate),as.numeric(inp$`Date End`[vc[i]]-startdate))
      if(i==1){
        if (inp$`Date Start`[vc[i]]>startdate){
          vc_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[vc[i]],(f[i+1]-f[i])*20))
          vaccine<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
          vaccineage<-c(rep(0,f[i]*20),rep(inp$`Target`[vc[i]],(f[i+1]-f[i])*20))
        }
        else{
          vc_vector<-c(rep(inp$`Value`[vc[i]],(f[i+1])*20))
          vaccine<-c(rep(1,(f[i+1])*20))
          vaccineage<-c(rep(inp$`Target`[vc[i]],(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          vc_vector<-c(vc_vector,rep(inp$`Value`[vc[i]],20))
          vaccine<-c(vaccine,rep(1,20))
          vaccineage<-c(vaccineage,rep(inp$`Target`[vc[i]],20))
        }else{
          vc_vector<-c(vc_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          vaccine<-c(vaccine,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          vaccineage<-c(vaccineage,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        vc_vector<-c(vc_vector,rep(inp$`Value`[vc[i]],(f[i*2]-f[i*2-1])*20))
        vaccine<-c(vaccine,rep(1,(f[i*2]-f[i*2-1])*20))
        vaccineage<-c(vaccineage,rep(inp$`Target`[vc[i]],(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(vc)&& f[i*2]<tail(times,1)){
        vc_vector<-c(vc_vector,rep(0,(tail(times,1)-f[i*2])*20))
        vaccine<-c(vaccine,rep(0,(tail(times,1)-f[i*2])*20))
        vaccineage<-c(vaccineage,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    vc_vector<-rep(0,tail(times,1)*20)
    vaccine<-rep(0,tail(times,1)*20)
    vaccineage<-rep(0,tail(times,1)*20)
  }
  ## vaccine partial
  f<-c()
  vcp_vector<-c()
  vaccinep<-c()
  vaccineagepartial<-c()
  if (length(vcp)>=1){
    for (i in 1:length(vcp)){
      f<-c(f,as.numeric(inp$`Date Start`[vcp[i]]-startdate),as.numeric(inp$`Date End`[vcp[i]]-startdate))
      if(i==1){
        if (inp$`Date Start`[vcp[i]]>startdate){
          vcp_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[vcp[i]],(f[i+1]-f[i])*20))
          vaccinep<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
          vaccineagepartial<-c(rep(0,f[i]*20),rep(inp$`Target`[vcp[i]],(f[i+1]-f[i])*20))
        }
        else{
          vcp_vector<-c(rep(inp$`Value`[vcp[i]],(f[i+1])*20))
          vaccinep<-c(rep(1,(f[i+1])*20))
          vaccineagepartial<-c(rep(inp$`Target`[vcp[i]],(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          vcp_vector<-c(vcp_vector,rep(inp$`Value`[vcp[i]],20))
          vaccinep<-c(vaccinep,rep(1,20))
          vaccineagepartial<-c(vaccineagepartial,rep(inp$`Target`[vcp[i]],20))
        }else{
          vcp_vector<-c(vcp_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          vaccinep<-c(vaccinep,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          vaccineagepartial<-c(vaccineagepartial,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        vcp_vector<-c(vcp_vector,rep(inp$`Value`[vcp[i]],(f[i*2]-f[i*2-1])*20))
        vaccinep<-c(vaccinep,rep(1,(f[i*2]-f[i*2-1])*20))
        vaccineagepartial<-c(vaccineagepartial,rep(inp$`Target`[vcp[i]],(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(vcp)&& f[i*2]<tail(times,1)){
        vcp_vector<-c(vcp_vector,rep(0,(tail(times,1)-f[i*2])*20))
        vaccinep<-c(vaccinep,rep(0,(tail(times,1)-f[i*2])*20))
        vaccineagepartial<-c(vaccineagepartial,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    vcp_vector<-rep(0,tail(times,1)*20)
    vaccinep<-rep(0,tail(times,1)*20)
    vaccineagepartial<-rep(0,tail(times,1)*20)
  }
 
  return(list(si_vector=si_vector,sd_vector=sd_vector,scr_vector=scr_vector,hw_vector=hw_vector,msk_vector=msk_vector,
              wah_vector=wah_vector,sc_vector=sc_vector,scp_vector=scp_vector,tb_vector=tb_vector,mt_vector=mt_vector*1000,
              cte_vector=cte_vector,q_vector=q_vector,vc_vector=vc_vector,vcp_vector=vcp_vector,isolation=isolation,
              screen=screen,cocoon=cocoon,schoolclose=schoolclose,schoolclosepartial=schoolclosepartial,
              workhome=workhome,handwash=handwash,masking=masking,
              quarantine=quarantine,vaccine=vaccine,vaccinep=vaccinep,travelban=travelban,distancing=distancing,
              masstesting=masstesting,testage=testage,vaccineage=vaccineage,vaccineagepartial=vaccineagepartial,dex=dex))
}
vectors0<-inputs(inp,'Baseline (Calibration)')
vectors<-inputs(inp,'Hypothetical Scenario')


f <- c(1,(1+parameters["give"])/2,(1-parameters["give"])/2,0)
KH<-parameters["beds_available"]
KICU<- parameters["icu_beds_available"]+parameters["ventilators_available"]
Kvent<- parameters["ventilators_available"]
x.H <- c(0,(1+parameters["give"])*KH/2,(3-parameters["give"])*KH/2,2*KH)
x.ICU <- c(0,(1+parameters["give"])*KICU/2,(3-parameters["give"])*KICU/2,2*KICU)
x.Vent <- c(0,(1+parameters["give"])*Kvent/2,(3-parameters["give"])*Kvent/2,2*Kvent)
fH <- splinefun(x.H, f, method = "hyman")
fICU <- splinefun(x.ICU, f, method = "hyman")
fVent<- splinefun(x.Vent, f, method = "hyman")


# set up a function to solve the equations
covid<-function(t, Y, parameters,input) 
{
  with(as.list(c(Y, parameters)),
       {
         S <- Y[Sindex]
         E <- Y[Eindex]
         I <- Y[Iindex]
         R <- Y[Rindex]
         X <- Y[Xindex]
         Z <- Y[Zindex]
         H <- Y[Hindex]
         HC <- Y[HCindex]
         C <- Y[Cindex]
         CM <- Y[CMindex]
         V <- Y[Vindex]
         QS <- Y[QSindex]
         QE <- Y[QEindex]
         QI <- Y[QIindex]
         QR <- Y[QRindex]
         CL <- Y[CLindex]
         QC <- Y[QCindex]
         ICU <- Y[ICUindex]
         ICUC <- Y[ICUCindex]
         ICUCV <-Y[ICUCVindex]
         Vent <- Y[Ventindex]
         VentC <- Y[VentCindex]
         CMC <- Y[CMCindex]
         EV <- Y[EVindex]
         ER <- Y[ERindex]
         EVR <- Y[EVRindex]
         VR <- Y[VRindex]
         QV <- Y[QVindex]
         QEV <- Y[QEVindex]
         QER <- Y[QERindex]
         QEVR <- Y[QEVRindex]
         QVR <- Y[QVRindex]
         HCICU <- Y[HCICUindex]
         HCV <- Y[HCVindex]
         Ab <- Y[Abindex]
         
         P <- (S+E+I+R+X+Z+V+H+HC+ICU+ICUC+ICUCV+Vent+VentC+EV+ER+EVR+VR+HCICU+HCV+
                 QS+QE+QI+QR+CL+QC+QEV+QV+QER+QEVR+QVR)
         Q <- (sum(QS)+sum(QE)+sum(QI)+sum(QC)+sum(QR)+sum(QV)+sum(QER)+sum(QEVR)+sum(QEV)+sum(QVR))/sum(P)
         # print(HCICUindex)
         # print(sum(ER))
         # print(t)
         # print(paste("EVR",sum(QE)))
         # print(paste("ER",sum(QR)))
         # print(paste("QER",sum(QER)))
         # print(paste("QEVR",sum(QEVR)))
         # print(paste("QV",sum(QV)))
         # print(paste("QC",sum(QC)))
         # print(paste("QI",sum(QI)))
         # print(paste("QEV",sum(QV)))
         # print(paste("QE",sum(QC)))
         # print(paste("QR",sum(QR)))
         # health system performance
         critH<-min(1-fH(sum(H)+sum(ICUC)+sum(ICUCV)),1)
         crit<-min(1-fICU(sum(ICU)+sum(Vent)+sum(VentC)),1)
         critV<-min(1-fVent(sum(Vent)),1)
         
         # interventions
         isolation<-input$isolation[t*20+1]
         distancing<-input$distancing[t*20+1]
         handwash<-input$handwash[t*20+1]
         masking<-input$masking[t*20+1]
         workhome<-input$workhome[t*20+1]
         schoolclose<-input$schoolclose[t*20+1]
         schoolclosep<-input$schoolclosepartial[t*20+1]
         cocoon<-input$cocoon[t*20+1]
         vaccine<-input$vaccine[t*20+1]
         travelban<-input$travelban[t*20+1]
         screen<-input$screen[t*20+1]
         quarantine<-input$quarantine[t*20+1]
         masstesting<-input$masstesting[t*20+1]
         dexamethasone<-input$dex[t*20+1]
         
         screen_eff<-0
         selfis<-0
         school<-1
         dist<-1
         hand<-0
         mask<-0
         vaccinate<-0
         trvban_eff<-0
         quarantine_rate<-0
         tests_per_day<-0
         
         selfis_cov<-(input$si_vector[t*20+1])/100
         screen_contacts<-(input$scr_vector[t*20+1])/10
         school_eff<-(input$sc_vector[t*20+1])/100
         school_effp<-(input$scp_vector[t*20+1])/100
         dist_cov<-(input$sd_vector[t*20+1])/100
         hand_cov<-(input$hw_vector[t*20+1])/100
         mask_cov<-(input$msk_vector[t*20+1])/100
         cocoon<-(input$cte_vector[t*20+1])/100
         work_cov<-(input$wah_vector[t*20+1])/100
         travelban_eff<-(input$tb_vector[t*20+1])/100
         vaccine_cov<-(input$vc_vector[t*20+1])/100
         vaccine_covp<-(input$vcp_vector[t*20+1])/100
         quarantine_cov<-(input$q_vector[t*20+1])/100
         tests_per_day<-(input$mt_vector[t*20+1])
         vaccineage<-input$vaccineage[t*20+1]
         vaccineagep<-input$vaccineagepartial[t*20+1]
         testage<-input$testage[t*20+1]
         
         # if (vaccine && !vaccinep){
         #   age_vaccine_vector<-as.numeric(age_group_vectors[[vaccineage]])
         #   vac_rate<-(-log(1-vaccine_cov)/vac_campaign)
         #   vaccinate<-vac_rate
         # }
         # if(vaccine && vaccinep){
         #   age_vaccine_vector<-max(as.numeric(age_group_vectors[[vaccineage]])as.numeric(age_group_vectors[[vaccineagepartial]])
         #   vac_rate<-(-log(1-vaccine_cov)/vac_campaign)
         #   vaccinate<-vac_rate
         # }
         if (vaccine){
           age_vaccine_vector<-as.numeric(age_group_vectors[[vaccineage]])
           vac_rate<-(-log(1-vaccine_cov)/vac_campaign)
           vaccinate<-vac_rate
         }
         else{age_vaccine_vector<-rep(0,A)}
         # print(vaccinate*age_vaccine_vector)
         if (masstesting){
           age_testing_vector<-as.numeric(age_group_vectors[[testage]])
         }else{age_testing_vector<-rep(0,A)}
         if (workhome){
           work<-work_cov*work_eff
         }else{work<-1}
         if (isolation){
           selfis<-selfis_cov
           if(screen){
             screen_eff<-min(sum(report*I+reportc*(CL)+H+ICU+Vent+reporth*(HC+ICUC+ICUCV+VentC+HCICU+HCV))*screen_contacts*(screen_overdispersion*I/P)*screen_test_sens/P,1) 
           }
         }
         if (schoolclose>=1 && schoolclosep>=1){
           school<-school_eff
           school2<-school_effp
           # print(dim(schoolclose))
           schoolclose2<-as.numeric(age_group_vectors[[schoolclose]])
           schoolclose2p<-as.numeric(age_group_vectors[[schoolclosep]])
           schoolclose3<-pmax(school*schoolclose2,school2*schoolclose2p)
           schoolclose4<-pmax((1-school)*schoolclose2,(1-school2)*schoolclose2p)
         }
         if (schoolclose>=1 && schoolclosep==0){
           school<-school_eff
           schoolclose2<-as.numeric(age_group_vectors[[schoolclose]])
           schoolclose3<-school*schoolclose2
           schoolclose4<-(1-school)*schoolclose2
         }
         if (schoolclose==0 && schoolclosep>=1){
           school<-school_effp
           schoolclose2<-as.numeric(age_group_vectors[[schoolclosep]])
           schoolclose3<-school*schoolclose2
           schoolclose4<-(1-school)*schoolclose2
         }
         if (schoolclose==0 && schoolclosep==0){
           schoolclose3<-rep(0,A);schoolclose4<-rep(0,A)
         }
         # print(schoolclose3)
         # print(schoolclose)
         if(distancing){
           dist<-dist_cov*dist_eff
         }
         if(handwash){
           hand<-hand_eff*hand_cov
         }
         if(masking){
           mask<-mask_eff*mask_cov
         }
         if(travelban){
           trvban_eff<-travelban_eff
         }
         if(quarantine){
           rate_q<-min((min(sum((CL+H+ICU+Vent+HC+ICUC+ICUCV+VentC+HCV+HCICU))*(household_size-1)/sum(P),1)*quarantine_effort),quarantine_cov/2)
           quarantine_rate<-rate_q/(1+exp(-10*(quarantine_cov/2-Q)))
         }
         if(dexamethasone){
           prob_v<-prob_vent*vent_dex
           dexo2<-parameters["dexo2"];dexo2c<-parameters["dexo2c"];dexv<-parameters["dexv"];dexvc<-parameters["dexvc"];
         }else{
           dexo2<-1;dexo2c<-1;dexv<-1;dexvc<-1;prob_v<-prob_vent;
         }
         # print(paste(dexo2,dexo2c,dexv,dexvc,prob_v))
         # print(paste("quarantine_rate",quarantine_rate))
         # print(quarantine_rate*sum(R))
         # print(quarantine_rate*ER)
         # 
         # print(sum(vaccinate*age_vaccine_vector*R))
         # print((1/quarantine_days)*sum(QR))
         
         # testing rates
         propI<-sum(I)/sum(P)
         propC<-sum(CL)/sum(P)
         propE<-sum(E)/sum(P)
         propEV<-sum(EV)/sum(P)
         propER<-sum(ER)/sum(P)
         propEVR<-sum(EVR)/sum(P)
         propHC<-sum(HC)/sum(P)
         propHCICU<-sum(HCICU)/sum(P)
         propHCV<-sum(HCV)/sum(P)
         testE<-tests_per_day*propE
         testEV<-tests_per_day*propEV
         testER<-tests_per_day*propER
         testEVR<-tests_per_day*propEVR
         testI<-tests_per_day*propI
         testC<-tests_per_day*propC
         testHC<-tests_per_day*propHC
         testHCICU<-tests_per_day*propHCICU
         testHCV<-tests_per_day*propHCV
         
         if(sum(I)>1){
           ratetestI<-mass_test_sens*testI/sum(I)
           # print(paste('rateI: ',ratetestI))
         }else{ratetestI<-0}
         if(sum(CL)>1){
           ratetestC<-mass_test_sens*testC/sum(CL)
           # print(paste('rateC: ',ratetestC))
         }else{ratetestC<-0}
         # print(sum(E))
         if(sum(E)>1){
           ratetestE<-mass_test_sens*testE/sum(E)
         }else{ratetestE<-0}
         if(sum(EV)>1){
           ratetestEV<-mass_test_sens*testEV/sum(EV)
           # print(paste('rateEV: ',ratetestEV))
         }else{ratetestEV<-0}
         if(sum(ER)>1){
           ratetestER<-mass_test_sens*testER/sum(ER)
           # print(paste('rateER: ',ratetestER))
         }else{ratetestER<-0}
         if(sum(EVR)>1){
           ratetestEVR<-mass_test_sens*testEVR/sum(EVR)
         }else{ratetestEVR<-0}
         if(sum(HC)>1){
           ratetestHC<-mass_test_sens*testHC/sum(HC)
         }else{ratetestHC<-0}
         if(sum(HCICU)>1){
           ratetestHCICU<-mass_test_sens*testHCICU/sum(HCICU)
         }else{ratetestHCICU<-0}
         if(sum(HCV)>1){
           ratetestHCV<-mass_test_sens*testHCV/sum(HCV)
         }else{ratetestHCV<-0}
         
         # print(mass_test_sens)
         # print(ratetestI*sum(I) + ratetestC*sum(CL) - (1/isolation_days)*sum(Z) )
         # print(propC)
         # print(testI)
         # print(testC)
         # 
         # cocooning the elderly
         cocoon_mat<-matrix((1-cocoon_eff),nrow = length(popstruc$pop),ncol = length(popstruc$pop))
         cocoon_mat[1:(age_cocoon-1),1:(age_cocoon-1)]<-1
         
         # contact matrices
         cts<-(contact_home+distancing*(1-dist)*contact_other+(1-distancing)*contact_other+
               +(1-schoolclose3)*contact_school
               +schoolclose4* contact_school
               +schoolclose3*contact_home*s2h
               # +sweep(contact_school, MARGIN=2, (1-schoolclose3), `*`) # school on
               # +sweep(contact_school, MARGIN=2, schoolclose4, `*`)     # school close
               # +sweep(contact_home*s2h, MARGIN=2, schoolclose3, `*`)   # inflating contacts at home when school closes
               +(1-workhome)*contact_work  # normal work
               +workhome*(1-work)*contact_work # people not working from home when homework is active
               +contact_home*workhome*work*w2h # inflating contacts at home when working from home
         )
         print(sum(rowSums(cts)))
         
         # print(paste("time",t,"  cts: ",length(cts)))
         # Final transmission related parameters
         contacts <- (1-cocoon)*cts+cocoon*cts*cocoon_mat+cocoon*(1+schoolclose3*(1-school_eff)+workhome*(1-work_eff))*contact_home*(1-cocoon_mat)
         seas <- 1+amp*cos(2*3.14*(t-(phi*365.25/12))/365.25)
         importation <- mean_imports*(1-trvban_eff)
         HH<-H+ICU+Vent+ICUC+ICUCV+VentC
         HHC<-HC+HCICU+HCV
         lam <- (1-max(hand,mask))*p*seas*(contacts%*%((rho*E+(I+CL+importation)+(1-selfis_eff)*(X+HHC)+rhos*(HH))/P))+
           (1-max(hand,mask))*p*seas*(1-quarantine*quarantine_eff_other)*(contact_other%*%((rho*QE+QI+QC+QEV+QEVR+QER)/P))
         # contacts under home quarantine
         lamq<-(1-max(hand,mask))*p*seas*((1-quarantine_eff_home)*contact_home%*%(((1-selfis_eff)*(X+HHC+rho*QE+QI+QC++QEV+QEVR+QER))/P))+
           (1-max(hand,mask))*p*seas*(1-quarantine_eff_other)*(contact_other%*%((rho*E+(I+CL+importation)+(1-selfis_eff)*(X+HHC+rho*QE+QI+QC++QEV+QEVR+QER)+rhos*(HH))/P))
         # lamq<-0
         # print(paste("lamq",lamq))
         # print(paste("lamq",(1-vaccine_eff_r)*lamq*sum(QVR) ))
         # print(paste("quarantine evr",quarantine_rate*sum(EVR) ))
         
         # birth/death
         b1<-sum(popbirth[,2]*popstruc[,2])
         birth<-0*popbirth[,2]
         birth[1]<-b1
         
         # ODE system
         dSdt <- -S*lam-vaccinate*age_vaccine_vector*S+omega*R+vac_dur*V-
           quarantine_rate*S +(1/quarantine_days)*QS+ageing%*%S-mort*S+birth
         dEdt <- S*lam-gamma*E+ageing%*%E- vaccinate*age_vaccine_vector*E - mort*E -
           quarantine_rate*E+(1/quarantine_days)*QE
         dIdt <- gamma*(1-pclin)*(1-age_testing_vector*ratetestE)*(1-screen_eff)*(1-ihr[,2])*E+
           gamma*(1-pclin_v)*(1-age_testing_vector*ratetestEV)*(1-screen_eff)*(1-sigmaEV*ihr[,2])*EV+
           gamma*(1-pclin_vr)*(1-age_testing_vector*ratetestEVR)*(1-screen_eff)*(1-sigmaEVR*ihr[,2])*EVR+
           gamma*(1-pclin_r)*(1-age_testing_vector*ratetestER)*(1-screen_eff)*(1-sigmaER*ihr[,2])*ER-
           vaccinate*age_vaccine_vector*I - nui*I+ageing%*%I-mort*I + 
           (1/quarantine_days)*QI - quarantine_rate*I - ratetestI*age_testing_vector*I
         dCLdt<- gamma*pclin*(1-age_testing_vector*ratetestE)*(1-selfis)*(1-ihr[,2])*(1-quarantine_rate)*E+
           gamma*pclin_v*(1-age_testing_vector*ratetestEV)*(1-selfis)*(1-sigmaEV*ihr[,2])*(1-quarantine_rate)*EV+
           gamma*pclin_vr*(1-age_testing_vector*ratetestEVR)*(1-selfis)*(1-sigmaEVR*ihr[,2])*(1-quarantine_rate)*EVR+
           gamma*pclin_r*(1-age_testing_vector*ratetestER)*(1-selfis)*(1-sigmaER*ihr[,2])*(1-quarantine_rate)*ER-
           nui*CL+ageing%*%CL-mort*CL  + (1/quarantine_days)*QC - ratetestC*age_testing_vector*CL
         dRdt <- vac_dur_r*VR-omega*R-vaccinate*age_vaccine_vector*R-lam*sigmaR*R - quarantine_rate*R+
           nui*I+nui*X+nui*CL+ageing%*%R-mort*R + (1/isolation_days)*Z+(1/quarantine_days)*QR+ 
           nus*propo2*(1-dexo2*pdeath_ho)*ifr[,2]*H+nus*(1-propo2)*(1-pdeath_h)*ifr[,2]*H+
           nusc*propo2*(1-pdeath_hco)*ifr[,2]*HC+nusc*(1-propo2)*(1-pdeath_hc)*ifr[,2]*HC+  
           nusc*propo2*(1-pdeath_icu_hco)*ifr[,2]*HCICU+nusc*(1-propo2)*(1-pdeath_icu_hc)*ifr[,2]*HCICU+
           nu_icu*propo2*(1-dexo2*pdeath_icuo)*ifr[,2]*ICU+nu_icu*(1-propo2)*(1-pdeath_icu)*ifr[,2]*ICU+
           nu_icuc*propo2*(1-dexo2c*pdeath_icuco)*ifr[,2]*ICUC+nu_icuc*(1-propo2)*(1-pdeath_icuc)*ifr[,2]*ICUC+
           nu_vent*(1-dexv*pdeath_vent)*ifr[,2]*Vent+
           nu_ventc*(1-pdeath_vent_hc)*ifr[,2]*HCV+
           nu_ventc*(1-dexvc*pdeath_ventc)*ifr[,2]*VentC+nu_ventc*(1-dexvc*pdeath_ventc)*ifr[,2]*ICUCV 
         dXdt <- gamma*selfis*(1-age_testing_vector*ratetestE)*pclin*(1-ihr[,2])*E+
           gamma*(1-pclin)*(1-age_testing_vector*ratetestE)*screen_eff*(1-ihr[,2])*E+
           gamma*selfis*(1-age_testing_vector*ratetestEV)*pclin_v*(1-sigmaEV*ihr[,2])*EV+
           gamma*(1-pclin_v)*(1-age_testing_vector*ratetestEV)*screen_eff*(1-sigmaEV*ihr[,2])*EV+
           gamma*selfis*(1-age_testing_vector*ratetestEVR)*pclin_v*(1-sigmaEVR*ihr[,2])*EVR+
           gamma*(1-pclin_vr)*(1-age_testing_vector*ratetestEVR)*screen_eff*(1-sigmaEVR*ihr[,2])*EVR+
           gamma*selfis*(1-age_testing_vector*ratetestER)*pclin_r*(1-sigmaER*ihr[,2])*ER+
           gamma*(1-pclin_r)*(1-age_testing_vector*ratetestER)*screen_eff*(1-sigmaER*ihr[,2])*ER+
           -nui*X+ageing%*%X-mort*X 
         dVdt <- vaccinate*age_vaccine_vector*S + omega*VR - (1-vaccine_eff)*lam*V - vac_dur*V + ageing%*%V-mort*V - quarantine_rate*V
         dEVdt<- (1-vaccine_eff)*lam*V - gamma*EV + ageing%*%EV - mort*EV - quarantine_rate*EV +(1/quarantine_days)*QEV
         dERdt<- lam*sigmaR*R - gamma*ER + ageing%*%ER - mort*ER - quarantine_rate*ER +(1/quarantine_days)*QER
         dVRdt <- vaccinate*age_vaccine_vector*E + vaccinate*age_vaccine_vector*I + vaccinate*age_vaccine_vector*R -
           (1-vaccine_eff_r)*lam*VR - vac_dur_r*VR + ageing%*%VR - mort*VR - omega*VR - quarantine_rate*VR + (1/quarantine_days)*QVR
         dEVRdt<- (1-vaccine_eff_r)*lam*VR - gamma*EVR + ageing%*%EVR-mort*EVR - quarantine_rate*EVR +
           (1/quarantine_days)*QEVR
         
         
         dQSdt <- quarantine_rate*S + ageing%*%QS-mort*QS - (1/quarantine_days)*QS - lamq*QS
         dQEdt <- quarantine_rate*E - gamma*QE + ageing%*%QE-mort*QE - (1/quarantine_days)*QE + lamq*QS 
         dQIdt <- quarantine_rate*I + gamma*(1-ihr[,2])*(1-pclin)*QE+
           gamma*(1-sigmaEV*ihr[,2])*(1-pclin_v)*QEV+
           gamma*(1-sigmaER*ihr[,2])*(1-pclin_r)*QER+           
           gamma*(1-sigmaEVR*ihr[,2])*(1-pclin_vr)*QEVR-
           nui*QI+ageing%*%QI-mort*QI - (1/quarantine_days)*QI
         dQCdt <- gamma*pclin*(1-selfis)*(1-age_testing_vector*ratetestE)*(1-ihr[,2])*quarantine_rate*E+
           gamma*pclin_v*(1-age_testing_vector*ratetestEV)*(1-selfis)*(1-sigmaEV*ihr[,2])*quarantine_rate*EV+
           gamma*pclin_vr*(1-age_testing_vector*ratetestEVR)*(1-selfis)*(1-sigmaEVR*ihr[,2])*quarantine_rate*EVR+
           gamma*pclin_r*(1-age_testing_vector*ratetestER)*(1-selfis)*(1-sigmaER*ihr[,2])*quarantine_rate*ER+
           gamma*(1-ihr[,2])*pclin*QE + 
           gamma*(1-sigmaEV*ihr[,2])*pclin_v*QEV + 
           gamma*(1-sigmaER*ihr[,2])*pclin_r*QER + 
           gamma*(1-sigmaEVR*ihr[,2])*pclin_vr*QEVR -
           nui*QC+ageing%*%QC-mort*QC - (1/quarantine_days)*QC
         dQRdt <- quarantine_rate*R + nui*QI + nui*QC + ageing%*%QR-mort*QR - (1/quarantine_days)*QR + vac_dur_r*QVR
         dQVdt <- quarantine_rate*V + ageing%*%QV-mort*QV - (1/quarantine_days)*QV - (1-vaccine_eff)*lamq*QV + omega*QVR 
         dQEVdt <- quarantine_rate*EV - gamma*QEV + ageing%*%QEV-mort*QEV - (1/quarantine_days)*QEV + (1-vaccine_eff)*lamq*QV 
         dQERdt <- quarantine_rate*ER - gamma*QER + ageing%*%QER-mort*QER - (1/quarantine_days)*QER + sigmaR*lamq*QR 
         dQVRdt <- quarantine_rate*VR - (1-vaccine_eff_r)*lam*QVR - vac_dur_r*QVR - omega*QVR + ageing%*%QVR - mort*QVR 
         dQEVRdt <- quarantine_rate*EVR - gamma*QEVR +ageing%*%QEVR-mort*QEVR -
           (1/quarantine_days)*QEVR +(1-vaccine_eff_r)*lamq*QVR 
            
         
         dHdt <- gamma*ihr[,2]*(1-prob_icu)*(1-critH)*reporth*E+ 
           gamma*sigmaEV*ihr[,2]*(1-prob_icu_v)*(1-critH)*reporth*EV + 
           gamma*sigmaEVR*ihr[,2]*(1-prob_icu_vr)*(1-critH)*reporth*EVR + 
           gamma*sigmaER*ihr[,2]*(1-prob_icu_r)*(1-critH)*reporth*ER + 
           gamma*ihr[,2]*(1-prob_icu)*(1-critH)*reporth*QE +
           gamma*sigmaEV*ihr[,2]*(1-prob_icu_v)*(1-critH)*reporth*QEV + 
           gamma*sigmaEVR*ihr[,2]*(1-prob_icu_vr)*(1-critH)*reporth*QEVR + 
           gamma*sigmaER*ihr[,2]*(1-prob_icu_r)*(1-critH)*reporth*QER - 
           nus*H + ageing%*%H-mort*H 
         dHCdt <- gamma*ihr[,2]*(1-prob_icu)*(1-reporth)*E+gamma*ihr[,2]*(1-prob_icu)*critH*reporth*E + 
           gamma*sigmaEV*ihr[,2]*(1-prob_icu_v)*(1-reporth)*EV+gamma*sigmaEV*ihr[,2]*(1-prob_icu_v)*critH*reporth*EV+
           gamma*sigmaEVR*ihr[,2]*(1-prob_icu_vr)*(1-reporth)*EVR+gamma*sigmaEVR*ihr[,2]*(1-prob_icu_vr)*critH*reporth*EVR+
           gamma*sigmaER*ihr[,2]*(1-prob_icu_r)*(1-reporth)*ER+gamma*sigmaER*ihr[,2]*(1-prob_icu_r)*critH*reporth*ER +
           gamma*ihr[,2]*(1-prob_icu)*(1-reporth)*QE+gamma*ihr[,2]*(1-prob_icu)*critH*reporth*QE+
           gamma*sigmaEV*ihr[,2]*(1-prob_icu_v)*(1-reporth)*QEV+gamma*sigmaEV*ihr[,2]*(1-prob_icu_v)*critH*reporth*QEV+
           gamma*sigmaEVR*ihr[,2]*(1-prob_icu_vr)*(1-reporth)*QEVR+gamma*sigmaEVR*ihr[,2]*(1-prob_icu_vr)*critH*reporth*QEVR+
           gamma*sigmaER*ihr[,2]*(1-prob_icu_r)*(1-reporth)*QER+gamma*sigmaER*ihr[,2]*(1-prob_icu_r)*critH*reporth*QER - 
           nusc*HC + ageing%*%HC-mort*HC - ratetestHC*age_testing_vector*HC
         dHCICUdt <- gamma*(1-reporth_ICU)*ihr[,2]*prob_icu*(1-prob_v)*E+
           gamma*(1-reporth_ICU)*sigmaEV*ihr[,2]*prob_icu_v*(1-prob_v_v)*EV+
           gamma*(1-reporth_ICU)*sigmaEVR*ihr[,2]*prob_icu_vr*(1-prob_v_vr)*EVR+
           gamma*(1-reporth_ICU)*sigmaER*ihr[,2]*prob_icu_r*(1-prob_v_r)*ER+
           gamma*(1-reporth_ICU)*ihr[,2]*prob_icu*(1-prob_v)*QE+
           gamma*(1-reporth_ICU)*sigmaEV*ihr[,2]*prob_icu_v*(1-prob_v_v)*QEV+
           gamma*(1-reporth_ICU)*sigmaEVR*ihr[,2]*prob_icu_vr*(1-prob_v_vr)*QEVR+
           gamma*(1-reporth_ICU)*sigmaER*ihr[,2]*prob_icu_r*(1-prob_v_r)*QER-
           nusc*HCICU + ageing%*%HCICU-mort*HCICU - ratetestHCICU*age_testing_vector*HCICU
         dHCVdt <- gamma*(1-reporth_ICU)*ihr[,2]*prob_icu*prob_v*E+
           gamma*(1-reporth_ICU)*sigmaEV*ihr[,2]*prob_icu_v*prob_v_v*EV+
           gamma*(1-reporth_ICU)*sigmaEVR*ihr[,2]*prob_icu_vr*prob_v_vr*EVR+
           gamma*(1-reporth_ICU)*sigmaER*ihr[,2]*prob_icu_r*prob_v_r*ER+
           gamma*(1-reporth_ICU)*ihr[,2]*prob_icu*prob_v*QE+
           gamma*(1-reporth_ICU)*sigmaEV*ihr[,2]*prob_icu_v*prob_v_v*QEV+
           gamma*(1-reporth_ICU)*sigmaEVR*ihr[,2]*prob_icu_vr*prob_v_vr*QEVR+
           gamma*(1-reporth_ICU)*sigmaER*ihr[,2]*prob_icu_r*prob_v_r*QER-
           nu_ventc*HCV + ageing%*%HCV-mort*HCV - ratetestHCV*age_testing_vector*HCV 
         dICUdt <- gamma*reporth_ICU*ihr[,2]*prob_icu*(1-crit)*(1-prob_v)*E+ 
           gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*(1-crit)*(1-prob_v_v)*EV+
           gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*(1-crit)*(1-prob_v_vr)*EVR+
           gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*(1-crit)*(1-prob_v_r)*ER+
           gamma*reporth_ICU*ihr[,2]*prob_icu*(1-crit)*(1-prob_v)*QE+ 
           gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*(1-crit)*(1-prob_v_v)*QEV+
           gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*(1-crit)*(1-prob_v_vr)*QEVR+
           gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*(1-crit)*(1-prob_v_r)*QER - 
           nu_icu*ICU +ageing%*%ICU - mort*ICU + (1-crit)*ICUC*1/2
         dICUCdt <- gamma*reporth_ICU*ihr[,2]*prob_icu*crit*(1-prob_v)*E+
           gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*crit*(1-prob_v_v)*EV+
           gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*crit*(1-prob_v_vr)*EVR+
           gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*crit*(1-prob_v_r)*ER+
           gamma*reporth_ICU*ihr[,2]*prob_icu*crit*(1-prob_v)*QE+
           gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*crit*(1-prob_v_v)*QEV+
           gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*crit*(1-prob_v_vr)*QEVR+
           gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*crit*(1-prob_v_r)*QER - 
           nu_icuc*ICUC -(1-crit)*ICUC*1/2 +ageing%*%ICUC - mort*ICUC 
         dICUCVdt <- gamma*reporth_ICU*ihr[,2]*prob_icu*prob_v*crit*E+
           gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*prob_v_v*crit*EV+
           gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*prob_v_vr*crit*EVR+
           gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*prob_v_r*crit*ER+
           gamma*reporth_ICU*ihr[,2]*prob_icu*prob_v*crit*QE+
           gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*prob_v_v*crit*QEV+
           gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*prob_v_vr*crit*QEVR+
           gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*prob_v_r*crit*QER -
           nu_ventc*ICUCV +ageing%*%ICUCV - mort*ICUCV - (1-critV)*ICUCV*1/2
         dVentdt <- gamma*reporth_ICU*ihr[,2]*prob_icu*(1-crit)*(1-critV)*prob_v*E+
           gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*(1-crit)*(1-critV)*prob_v_v*EV+
           gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*(1-crit)*(1-critV)*prob_v_vr*EVR+
           gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*(1-crit)*(1-critV)*prob_v_r*ER+
           gamma*reporth_ICU*ihr[,2]*prob_icu*(1-crit)*(1-critV)*prob_v*QE+
           gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*(1-crit)*(1-critV)*prob_v_v*QEV+
           gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*(1-crit)*(1-critV)*prob_v_vr*QEVR+
           gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*(1-crit)*(1-critV)*prob_v_r*QER +
           (1-critV)*VentC*1/2 +(1-critV)*ICUCV*1/2 - nu_vent*Vent +ageing%*%Vent - mort*Vent 
         dVentCdt <- gamma*reporth_ICU*ihr[,2]*prob_icu*prob_v*(1-crit)*critV*E+
           gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*prob_v_v*(1-crit)*critV*EV+
           gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*prob_v_vr*(1-crit)*critV*EVR+
           gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*prob_v_r*(1-crit)*critV*ER+
           gamma*reporth_ICU*ihr[,2]*prob_icu*prob_v*(1-crit)*critV*QE+
           gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*prob_v_v*(1-crit)*critV*QEV+
           gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*prob_v_vr*(1-crit)*critV*QEVR+
           gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*prob_v_r*(1-crit)*critV*QER - 
           (1-critV)*VentC*1/2 -nu_ventc*VentC +ageing%*%VentC - mort*VentC 
         
         # print(paste(t,dexo2,propo2))
         # print(sum(nus*propo2*dexo2*pdeath_ho*ifr[,2]*H))
         dCdt <- report*gamma*(1-age_testing_vector*ratetestE)*(1-pclin)*(1-ihr[,2])*(E+QE)+reportc*gamma*pclin*(1-age_testing_vector*ratetestE)*(1-ihr[,2])*(E+QE)+
           gamma*ihr[,2]*(1-critH)*(1-prob_icu)*(E+QE)+gamma*ihr[,2]*critH*reporth*(1-prob_icu)*(E+QE)+
           gamma*ihr[,2]*prob_icu*(E+QE)+ratetestI*age_testing_vector*I+ratetestC*age_testing_vector*CL+gamma*age_testing_vector*ratetestE*(1-ihr[,2])*E
         dCMdt<- nus*propo2*dexo2*pdeath_ho*ifr[,2]*H+nus*(1-propo2)*pdeath_h*ifr[,2]*H+
           nusc*report_death_HC*propo2*pdeath_hco*ifr[,2]*HC+nusc*report_death_HC*(1-propo2)*pdeath_hc*ifr[,2]*HC+
           nu_icu*propo2*dexo2*pdeath_icuo*ifr[,2]*ICU+nu_icu*(1-propo2)*pdeath_icu*ifr[,2]*ICU+
           nu_icuc*propo2*dexo2c*pdeath_icuco*ifr[,2]*ICUC+nu_icuc*(1-propo2)*pdeath_icuc*ifr[,2]*ICUC+
           nu_vent*dexv*pdeath_vent*ifr[,2]*Vent+nu_ventc*dexvc*pdeath_ventc*ifr[,2]*VentC +
           nu_ventc*dexvc*pdeath_ventc*ifr[,2]*ICUCV+ nu_ventc*report_death_HC*pdeath_vent_hc*ifr[,2]*HCV+
           nusc*report_death_HC*propo2*pdeath_icu_hco*ifr[,2]*HCICU+
           nusc*report_death_HC*(1-propo2)*pdeath_icu_hc*ifr[,2]*HCICU +
           mort*H + mort*ICU + mort*ICUC + mort*ICUCV + mort*Vent + mort*VentC + mort*Z + 
           mort*report_death_HC*HC +mort*report_death_HC*HCICU + mort*report_death_HC*HCV +
           report_natdeathI*mort*I + report_natdeathI*mort*QI+ report_natdeathI*mort*E+
           report_natdeathI*mort*QE + report_natdeathI*mort*EV+ report_natdeathI*mort*EVR+
           report_natdeathI*mort*ER + report_natdeathI*mort*QEV+
           report_natdeathI*mort*QEVR + report_natdeathI*mort*QER+
           report_natdeathCL*mort*CL + report_natdeathCL*mort*QC + report_natdeathCL*mort*X
         dCMCdt <- nusc*propo2*pdeath_hco*ifr[,2]*HC+nusc*(1-propo2)*pdeath_hc*ifr[,2]*HC+
           nu_icuc*propo2*dexo2c*pdeath_icuco*ifr[,2]*ICUC+nu_icuc*(1-propo2)*pdeath_icuc*ifr[,2]*ICUC+
           nu_ventc*dexvc*pdeath_ventc*ifr[,2]*VentC+nu_ventc*dexvc*pdeath_ventc*ifr[,2]*ICUCV+
           mort*HC + mort*ICUC + mort*VentC + mort*ICUCV 
         
         dZdt <- gamma*ratetestE*age_testing_vector*(1-ihr[,2])*E+
           ratetestI*age_testing_vector*I+
           ratetestC*age_testing_vector*CL+
           gamma*(1-ihr[,2])*ratetestEV*age_testing_vector*EV+
           gamma*(1-ihr[,2])*ratetestEVR*age_testing_vector*EVR+
           gamma*(1-ihr[,2])*ratetestER*age_testing_vector*ER+
           ratetestHC*age_testing_vector*HC+
           ratetestHCICU*age_testing_vector*HCICU+
           ratetestHCV*age_testing_vector*HCV-
           (1/isolation_days)*Z-mort*Z
         
         dAbdt <- nui*I+nui*X+nui*CL+ 
           nus*propo2*(1-dexo2*pdeath_ho)*ifr[,2]*H+nus*(1-propo2)*(1-pdeath_h)*ifr[,2]*H+
           nusc*propo2*(1-pdeath_hco)*ifr[,2]*HC+nusc*(1-propo2)*(1-pdeath_hc)*ifr[,2]*HC+  
           nusc*propo2*(1-pdeath_icu_hco)*ifr[,2]*HCICU+nusc*(1-propo2)*(1-pdeath_icu_hc)*ifr[,2]*HCICU+
           nu_ventc*(1-pdeath_vent_hc)*ifr[,2]*HCV+
           nu_icu*propo2*(1-dexo2*pdeath_icuo)*ifr[,2]*ICU+nu_icu*(1-propo2)*(1-pdeath_icu)*ifr[,2]*ICU+
           nu_icuc*propo2*(1-dexo2c*pdeath_icuco)*ifr[,2]*ICUC+nu_icuc*(1-propo2)*(1-pdeath_icuc)*ifr[,2]*ICUC+
           nu_vent*(1-dexv*pdeath_vent)*ifr[,2]*Vent+
           nu_ventc*(1-dexvc*pdeath_ventc)*ifr[,2]*VentC+
           nu_ventc*(1-dexvc*pdeath_ventc)*ifr[,2]*ICUCV - 
           seroneg*Ab - mort*Ab + ageing%*%Ab
         
         # print(paste("QEVR",sum(QEVR)))
         
         # return the rate of change
         list(c(S=dSdt,dEdt,dIdt,dRdt,dXdt,dHdt,dHCdt,dCdt,dCMdt,dVdt,dQSdt,dQEdt,dQIdt,dQRdt,dCLdt,dQCdt,dICUdt,dICUCdt,dICUCVdt,
                dVentdt,dVentCdt,dCMCdt,dZdt,dEVdt,dERdt,dEVRdt,dVRdt,dQVdt,dQEVdt,dQEVRdt,dQERdt,dQVRdt,dHCICUdt,dHCVdt,dAbdt))
       }
  ) 
}

###########    RUN BASELINE MODEL - start time for interventions is set to day 1e5, i.e. interventions are always off
Y<-c(initS,initE,initI,initR,initX,initH,initHC,initC,initCM,initV, initQS, initQE, initQI, initQR, initCL, initQC, initICU, 
     initICUC, initICUCV, initVent, initVentC, initCMC,initZ, initEV, initER, initEVR, initVR, 
     initQV,initQEV,initQEVR,initQER,initQVR,initHCICU,initHCV,initAb) # initial conditions for the main solution vector
out0 <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = parameters, input=vectors0)
tail(rowSums(out0[,(CMindex+1)]),1)                       # cumulative mortality
# tail(rowSums(out0[,(Sindex+1)]),1)/sum(popstruc[,2])      # cumulative mortality
# tail(rowSums(out0[,(Vindex+1)]),1)/sum(popstruc[,2])      # cumulative mortality
# tail(rowSums(out0[,(Rindex+1)]),1)/sum(popstruc[,2])      # cumulative mortality
# sum(rowSums(out0[,(Iindex+1)]))       # cumulative mortality
# sum(rowSums(out0[,(Ventindex+1)]))    # cumulative mortality
# sum(rowSums(out0[,(HCVindex+1)]))     # cumulative mortality
# sum(rowSums(out0[,(HCICUindex+1)]))   # cumulative mortality
# sum(rowSums(out0[,(HCindex+1)]))      # cumulative mortality

# sum(rowSums(parameters["nus"]*parameters["propo2"]*parameters["pdeath_ho"]*ifr[,2]*out0[,(Hindex+1)]))      # cumulative mortality
# sum(rowSums(parameters["nus"]*(1-parameters["propo2"])*parameters["pdeath_h"]*ifr[,2]*out0[,(Hindex+1)]))      # cumulative mortality
# sum(rowSums(parameters["nusc"]*parameters["propo2"]*parameters["pdeath_hco"]*ifr[,2]*out0[,(HCindex+1)]))      # cumulative mortality
# sum(rowSums(parameters["nusc"]*(1-parameters["propo2"])*parameters["pdeath_hc"]*ifr[,2]*out0[,(HCindex+1)]))      # cumulative mortality
# sum(rowSums(parameters["nu_icu"]*parameters["propo2"]*parameters["pdeath_icuo"]*ifr[,2]*out0[,(ICUindex+1)]))      # cumulative mortality
# sum(rowSums(parameters["nu_icu"]*(1-parameters["propo2"])*parameters["pdeath_icu"]*ifr[,2]*out0[,(ICUindex+1)]))      # cumulative mortality
# sum(rowSums(parameters["nu_icuc"]*parameters["propo2"]*parameters["pdeath_icuco"]*ifr[,2]*out0[,(ICUCindex+1)]))      # cumulative mortality
# sum(rowSums(parameters["nu_icuc"]*(1-parameters["propo2"])*parameters["pdeath_icuc"]*ifr[,2]*out0[,(ICUCindex+1)]))      # cumulative mortality
# sum(rowSums(parameters["nusc"]*parameters["propo2"]*parameters["pdeath_icu_hco"]*ifr[,2]*out0[,(HCICUindex+1)]))      # cumulative mortality
# sum(rowSums(parameters["nusc"]*(1-parameters["propo2"])*parameters["pdeath_icu_hc"]*ifr[,2]*out0[,(HCICUindex+1)]))      # cumulative mortality
# sum(rowSums(parameters["nu_vent"]*parameters["pdeath_vent"]*ifr[,2]*out0[,(Ventindex+1)]))      # cumulative mortality
# sum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*ifr[,2]*out0[,(VentCindex+1)]))      # cumulative mortality
# sum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*ifr[,2]*out0[,(ICUCVindex+1)]))      # cumulative mortality
# sum(rowSums(parameters["nu_ventc"]*parameters["pdeath_vent_hc"]*ifr[,2]*out0[,(HCVindex+1)]))      # cumulative mortality
# 

# #############################   PLOT TIME VARYING INTERVENTIONS     #####################################
# library(plyr)
# interv_timeseries<-ldply(vectors[c("wah_vector","tb_vector","si_vector","sc_vector","hw_vector","msk_vector")], data.frame)
# interv_timeseries<-cbind(interv_timeseries,rep(1:(length(vectors$si_vector)),6))
# colnames(interv_timeseries)<-c("Intervention","Coverage","Time")
# 
# m4 <- interv_timeseries %>%
#   # convert state to factor and reverse order of levels
#   mutate(Intervention=factor(Intervention,levels=rev(sort(unique(Intervention))))) %>%
#   # create a new variable from count
#   mutate(covfactor=cut(Coverage,breaks=seq(0,100,by=5),
#                          labels=c("0-5","5-10","10-15","15-20","20-25","25-30",
#                                   "30-35","35-40","40-45","45-50","50-55","55-60","60-65",
#                                   "65-70","70-75","75-80","80-85","85-90","90-95","95-100"))) %>%
#   # change level order
#   mutate(covfactor=factor(as.character(covfactor),levels=rev(levels(covfactor))))
# levels(m4$Intervention)<-c("Work from home","Interntional Travel Ban","Self isolation","Screening","Mask wearing","Handwashing")
#   
# cols<-c("black",brewer.pal(11,"Spectral"))
# textcol <- "grey40"
# p <- ggplot(m4,aes(x=Time,y=Intervention,fill=covfactor))+
#   geom_tile( height = 0.9)+
#   guides(fill=guide_legend(title="Coverage (%)"))+
#   labs(x="",y="",title="")+
#   scale_y_discrete(expand=c(0,0))+
#   scale_fill_brewer(palette="Spectral")+
#   # coord_fixed()+
#   theme_grey(base_size=10)+
#   theme(legend.position="right",legend.direction="vertical",
#         legend.title=element_text(colour=textcol),
#         legend.margin=margin(grid::unit(0,"cm")),
#         legend.text=element_text(colour=textcol,size=7,face="bold"),
#         legend.key.height=grid::unit(0.8,"cm"),
#         legend.key.width=grid::unit(0.2,"cm"),
#         axis.text.x=element_text(size=10,colour=textcol),
#         axis.text.y=element_text(vjust=0.2,colour=textcol),
#         axis.ticks=element_line(size=0.4),
#         plot.background=element_blank(),
#         panel.border=element_blank(),
#         plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
#         plot.title=element_text(colour=textcol,hjust=0,size=14,face="bold"))+
#   theme_minimal()
# p
# ################################################################################################


process_ode_outcome <- function(out, iterations,intv_vector){
  out_min<-out$min
  out_max<-out$max
  out_mean<-out$mean
  
  critH<-c()
  crit<-c()
  critV<-c()
  
  for (i in 1:length(times)){
    critH[i]<-min(1-fH((sum(out_mean[i,(Hindex+1)]))+sum(out_mean[i,(ICUCindex+1)])+sum(out_mean[i,(ICUCVindex+1)])),1)
    crit[i]<-min(1-fICU((sum(out_mean[i,(ICUindex+1)]))+(sum(out_mean[i,(Ventindex+1)]))+(sum(out_mean[i,(VentCindex+1)]))))
    critV[i]<-min(1-fVent((sum(out_mean[i,(Ventindex+1)]))),1)
  }
  
  # total population
  pop1<-out_mean[,(Sindex+1)]+out_mean[,(Eindex+1)]+out_mean[,(Iindex+1)]+out_mean[,(CLindex+1)]+out_mean[,(Rindex+1)]+
    out_mean[,(Xindex+1)]+out_mean[,(Vindex+1)]+out_mean[,(Zindex+1)]+out_mean[,(EVindex+1)]+out_mean[,(ERindex+1)]+out_mean[,(EVRindex+1)]+
    out_mean[,(QSindex+1)]+out_mean[,(QEindex+1)]+out_mean[,(QIindex+1)]+out_mean[,(QCindex+1)]+out_mean[,(QRindex+1)]+
    out_mean[,(QVindex+1)]+out_mean[,(QEVindex+1)]+out_mean[,(QERindex+1)]+out_mean[,(QVRindex+1)]+out_mean[,(QEVRindex+1)]+
    out_mean[,(Hindex+1)]+out_mean[,(HCindex+1)]+out_mean[,(ICUindex+1)]+out_mean[,(ICUCindex+1)]+out_mean[,(ICUCVindex+1)]+
    out_mean[,(Ventindex+1)]+out_mean[,(VentCindex+1)]+out_mean[,(HCICUindex+1)]+out_mean[,(HCVindex+1)]
  tpop1<-rowSums(pop1)
  

  ##########################    AB prevalence
  ab_age<-out_mean[,(Abindex+1)]
  ab_all_ages<-rowSums(out_mean[,(Abindex+1)])
  
  ##########################    CALCULATE MORTALITY 
  dexo2_hist <- rep(0,length(times))
  dexo2c_hist <- rep(0,length(times))
  dexv_hist <- rep(0,length(times))
  dexvc_hist <- rep(0,length(times))
  for (tt in times) {
    if(tt < max(times)){
      if(intv_vector$dex[tt*20+1]) {
        dexo2_hist[tt+1] <- parameters["dexo2"]
        dexo2c_hist[tt+1] <- parameters["dexo2c"]
        dexv_hist[tt+1] <- parameters["dexv"]
        dexvc_hist[tt+1] <- parameters["dexvc"]
      } else {
        dexo2_hist[tt+1] <- 1
        dexo2c_hist[tt+1] <- 1
        dexv_hist[tt+1] <- 1
        dexvc_hist[tt+1] <- 1
      }
    } else {
      dexo2_hist[tt+1] <- dexo2_hist[tt]
      dexo2c_hist[tt+1] <- dexo2c_hist[tt]
      dexv_hist[tt+1] <- dexv_hist[tt]
      dexvc_hist[tt+1] <- dexvc_hist[tt]
    }
  }
  
  cinc_mort_1 <- cumsum(rowSums(parameters["nus"]*parameters["propo2"]*parameters["pdeath_ho"]*dexo2_hist*(out_mean[,(Hindex+1)]%*%ifr[,2])))
  cinc_mort_2 <- cumsum(rowSums(parameters["nus"]*(1-parameters["propo2"])*parameters["pdeath_h"]*(out_mean[,(Hindex+1)]%*%ifr[,2])))
  
  cinc_mort_3 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*parameters["propo2"]*parameters["pdeath_hco"]*(out_mean[,(HCindex+1)]%*%ifr[,2])))
  cinc_mort_4 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*(1-parameters["propo2"])*parameters["pdeath_hc"]*(out_mean[,(HCindex+1)]%*%ifr[,2])))
 
  cinc_mort_5 <- cumsum(rowSums(parameters["nu_icu"]*parameters["propo2"]*parameters["pdeath_icuo"]*dexo2_hist*(out_mean[,(ICUindex+1)]%*%ifr[,2])))
  cinc_mort_6 <- cumsum(rowSums(parameters["nu_icu"]*(1-parameters["propo2"])*parameters["pdeath_icu"]*(out_mean[,(ICUindex+1)]%*%ifr[,2])))
  cinc_mort_7 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["propo2"]*parameters["pdeath_icuco"]*dexo2c_hist*(out_mean[,(ICUCindex+1)]%*%ifr[,2])))
  cinc_mort_8 <- cumsum(rowSums(parameters["nu_icuc"]*(1-parameters["propo2"])*parameters["pdeath_icuc"]*(out_mean[,(ICUCindex+1)]%*%ifr[,2])))
  
  cinc_mort_9 <- cumsum(rowSums(parameters["nu_vent"]*parameters["pdeath_vent"]*dexv_hist*(out_mean[,(Ventindex+1)]%*%ifr[,2])))
  cinc_mort_10 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_mean[,(VentCindex+1)]%*%ifr[,2])))
  cinc_mort_11 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_mean[,(ICUCVindex+1)]%*%ifr[,2])))
  
  cinc_mort_12 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*parameters["propo2"]*parameters["pdeath_icu_hco"]*(out_mean[,(HCICUindex+1)]%*%ifr[,2])))
  cinc_mort_13 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*(1-parameters["propo2"])*parameters["pdeath_icu_hc"]*(out_mean[,(HCICUindex+1)]%*%ifr[,2])))
  cinc_mort_14 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["report_death_HC"]*parameters["pdeath_vent_hc"]*(out_mean[,(HCVindex+1)]%*%ifr[,2])))
  
  cinc_mort_121 <- cumsum(rowSums(parameters["nusc"]*parameters["propo2"]*parameters["pdeath_icu_hco"]*(out_mean[,(HCICUindex+1)]%*%ifr[,2])))
  cinc_mort_131 <- cumsum(rowSums(parameters["nusc"]*(1-parameters["propo2"])*parameters["pdeath_icu_hc"]*(out_mean[,(HCICUindex+1)]%*%ifr[,2])))
  cinc_mort_141 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_vent_hc"]*(out_mean[,(HCVindex+1)]%*%ifr[,2])))
  
  
  cinc_mort_H1 <- cinc_mort_1 + cinc_mort_2
  cinc_mort_HC1 <- cinc_mort_3 + cinc_mort_4 + cinc_mort_12 + cinc_mort_13 + cinc_mort_14
  cinc_mort_ICU1 <- cinc_mort_5 + cinc_mort_6
  cinc_mort_ICUC1 <- cinc_mort_7 + cinc_mort_8
  cinc_mort_Vent1 <- cinc_mort_9
  cinc_mort_VentC1 <- cinc_mort_10
  cinc_mort_ICUCV1 <- cinc_mort_11
  
  # all deaths due to covid19 disease - reported + unreported
  cinc_mort_all<-cinc_mort_1+cinc_mort_2+cinc_mort_3+cinc_mort_4+cinc_mort_5+cinc_mort_6+
    cinc_mort_7+cinc_mort_8+cinc_mort_9+cinc_mort_10+cinc_mort_11+cinc_mort_121+cinc_mort_131+cinc_mort_141
  
  base_mort_H1 <- cumsum(rowSums(out_mean[,(Hindex+1)]%*%mort))
  base_mort_HC1 <- cumsum(rowSums(parameters["report_death_HC"]*out_mean[,(HCindex+1)]%*%mort))
  base_mort_ICU1 <- cumsum(rowSums(out_mean[,(ICUindex+1)]%*%mort))
  base_mort_ICUC1 <- cumsum(rowSums(out_mean[,(ICUCindex+1)]%*%mort))
  base_mort_ICUCV1 <- cumsum(rowSums(out_mean[,(ICUCVindex+1)]%*%mort))
  base_mort_Vent1 <- cumsum(rowSums(out_mean[,(Ventindex+1)]%*%mort))
  base_mort_VentC1 <- cumsum(rowSums(out_mean[,(VentCindex+1)]%*%mort))
  base_mort_Z1 <- cumsum(rowSums(out_mean[,(Zindex+1)]%*%mort))
  base_mort_HCICU1 <- cumsum(rowSums(parameters["report_death_HC"]*out_mean[,(HCICUindex+1)]%*%mort))
  base_mort_HCV1 <- cumsum(rowSums(parameters["report_death_HC"]*out_mean[,(HCVindex+1)]%*%mort))
  
  base_mort_V1 <- cumsum(rowSums(out_mean[,(Vindex+1)]%*%mort))
  base_mort_S1 <- cumsum(rowSums(out_mean[,(Sindex+1)]%*%mort))
  base_mort_QS1 <- cumsum(rowSums(out_mean[,(QSindex+1)]%*%mort))
  base_mort_QR1 <- cumsum(rowSums(out_mean[,(QRindex+1)]%*%mort))
  base_mort_R1 <- cumsum(rowSums(out_mean[,(Rindex+1)]%*%mort))
  base_mort_QVR1 <- cumsum(rowSums(out_mean[,(QVRindex+1)]%*%mort))
  base_mort_VR1 <- cumsum(rowSums(out_mean[,(VRindex+1)]%*%mort))
  base_mort_QV1 <- cumsum(rowSums(out_mean[,(QVindex+1)]%*%mort))
  
  base_mort_E1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(Eindex+1)]%*%mort))
  base_mort_I1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(Iindex+1)]%*%mort))
  base_mort_CL1 <- cumsum(rowSums(parameters["report_natdeathCL"]*out_mean[,(CLindex+1)]%*%mort))
  base_mort_X1 <- cumsum(rowSums(parameters["report_natdeathCL"]*out_mean[,(Xindex+1)]%*%mort))
  base_mort_QE1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(QEindex+1)]%*%mort))
  base_mort_QI1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(QIindex+1)]%*%mort))
  base_mort_QC1 <- cumsum(rowSums(parameters["report_natdeathCL"]*out_mean[,(QCindex+1)]%*%mort))
  base_mort_ER1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(ERindex+1)]%*%mort))
  base_mort_EV1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(EVindex+1)]%*%mort))
  base_mort_EVR1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(EVRindex+1)]%*%mort))
  base_mort_QEV1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(QEVindex+1)]%*%mort))
  base_mort_QER1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(QERindex+1)]%*%mort))
  base_mort_QEVR1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(QEVRindex+1)]%*%mort))

  
  base_mort_HC11 <- cumsum(rowSums(out_mean[,(HCindex+1)]%*%mort))
  base_mort_HCICU11 <- cumsum(rowSums(out_mean[,(HCICUindex+1)]%*%mort))
  base_mort_HCV11 <- cumsum(rowSums(out_mean[,(HCVindex+1)]%*%mort))
  base_mort_E11 <- cumsum(rowSums(out_mean[,(Eindex+1)]%*%mort))
  base_mort_I11 <- cumsum(rowSums(out_mean[,(Iindex+1)]%*%mort))
  base_mort_CL11 <- cumsum(rowSums(out_mean[,(CLindex+1)]%*%mort))
  base_mort_X11 <- cumsum(rowSums(out_mean[,(Xindex+1)]%*%mort))
  base_mort_QE11 <- cumsum(rowSums(out_mean[,(QEindex+1)]%*%mort))
  base_mort_QI11 <- cumsum(rowSums(out_mean[,(QIindex+1)]%*%mort))
  base_mort_QC11 <- cumsum(rowSums(out_mean[,(QCindex+1)]%*%mort))
  base_mort_ER11 <- cumsum(rowSums(out_mean[,(ERindex+1)]%*%mort))
  base_mort_EV11 <- cumsum(rowSums(out_mean[,(EVindex+1)]%*%mort))
  base_mort_EVR11 <- cumsum(rowSums(out_mean[,(EVRindex+1)]%*%mort))
  base_mort_QEV11 <- cumsum(rowSums(out_mean[,(QEVindex+1)]%*%mort))
  base_mort_QER11 <- cumsum(rowSums(out_mean[,(QERindex+1)]%*%mort))
  base_mort_QEVR11 <- cumsum(rowSums(out_mean[,(QEVRindex+1)]%*%mort))
  
  # all deaths of infected with sars-cov-2 virus - reported + unreported
  nat_deaths_inf <- round(base_mort_E11 + base_mort_I11 + base_mort_CL11 + base_mort_X11 + 
                                           base_mort_ER11 + base_mort_EV11+  base_mort_EVR11+   
                                           base_mort_QE11 + base_mort_QI11 + base_mort_QC11 +  
                                           base_mort_QEV11 + base_mort_QER11 + base_mort_QEVR11 + base_mort_Z1+
                                           base_mort_H1+base_mort_HC11+base_mort_ICU1+base_mort_ICUC1+base_mort_ICUCV1+
                                           base_mort_Vent1+base_mort_VentC1+base_mort_HCICU11+base_mort_HCV11)
  
  # Export in a cohesive format ----
  results <- list()
  results$time <- startdate + times  # dates
  results$N <- tpop1
  
  ## Ab
  results$ab_all_ages<-ab_all_ages
  results$ab<-ab_age
  
  # Rt/ FOI
  dailyinc1<-out$mean_cases         # daily incidence
  results$Rt <- out$mean_Rt
  results$pct_total_pop_infected <- out$mean_infections
  results$doubling_time <- round(log(2)*7 / (log(dailyinc1[2+7] / dailyinc1[2])), 2)  # (Baseline only) to double the number of infections at inception
  results$daily_incidence <- round(dailyinc1)  # daily incidence (Reported)
  results$daily_total_cases <- round(out$mean_daily_infection) # daily incidence (Reported + Unreported)  # daily incidence (Reported + Unreported)
  
  # Hospital requirements
  previcureq1<-rowSums(out_mean[,(Hindex+1)])+ rowSums(out_mean[,(ICUCindex+1)])+rowSums(out_mean[,(ICUCVindex+1)]) # surge beds occupancy
  previcureq21<-rowSums(out_mean[,(ICUindex+1)])+rowSums(out_mean[,(VentCindex+1)])   # icu beds occupancy
  previcureq31<-rowSums(out_mean[,(Ventindex+1)])   # ventilator occupancy
  overloadH1<-rowSums(out_mean[,(HCindex+1)])       # requirement for beds
  overloadICU1<-rowSums(out_mean[,(ICUCindex+1)])+rowSums(out_mean[,(HCICUindex+1)])   # requirement for icu beds
  overloadICUV1<-rowSums(out_mean[,(ICUCVindex+1)]) # requirement for ventilators
  overloadVent1<-rowSums(out_mean[,(VentCindex+1)])+rowSums(out_mean[,(HCVindex+1)]) # requirement for ventilators
  
  results$required_beds <- round(previcureq1)  # required beds
  results$saturation <- parameters["beds_available"]  # saturation
  results$hospital_surge_beds <- round(previcureq1)
  results$icu_beds <- round(previcureq21)
  results$ventilators <- round(previcureq31)
  results$normal_bed_requirement <- round(rowSums(out_mean[,(Hindex+1)])+overloadH1)   #real required beds. previcureq1 above is the occupancy
  results$icu_bed_requirement <- round(rowSums(out_mean[,(ICUindex+1)])+overloadICU1)
  results$icu_ventilator_requirement <- round(rowSums(out_mean[,(Ventindex+1)])+overloadICUV1+overloadVent1)
  
  ### MORTALITY
  results$cum_mortality <- round(rowSums(out_mean[,(CMindex+1)]))       # cumulative mortality
  results$deaths_from_covid<-last(cinc_mort_all)
  results$deaths_with_covid<-last(nat_deaths_inf)
  results$death_natural_non_exposed <- round(base_mort_S1+base_mort_V1+base_mort_QS1)
  results$death_natural_exposed <- round(base_mort_E1 + base_mort_I1 + base_mort_CL1 + base_mort_X1 + 
                                           base_mort_R1+ base_mort_ER1 + base_mort_EV1+  base_mort_EVR1+   
                                           base_mort_QE1 + base_mort_QI1 + base_mort_QC1 + base_mort_QR1 + 
                                           base_mort_QEV1 + base_mort_QER1 + base_mort_QEVR1 + base_mort_QVR1 +
                                           base_mort_H1+base_mort_HC1+base_mort_ICU1+base_mort_ICUC1+base_mort_ICUCV1+
                                           base_mort_Vent1+base_mort_VentC1+base_mort_HCICU1+base_mort_HCV1)
  results$death_treated_hospital <- round(cinc_mort_H1)
  results$death_treated_icu <- round(cinc_mort_ICU1)
  results$death_treated_ventilator <- round(cinc_mort_Vent1)
  results$death_untreated_hospital <- round(cinc_mort_HC1)
  results$death_untreated_icu <- round(cinc_mort_ICUC1)
  results$death_untreated_ventilator <- round(cinc_mort_VentC1)+round(cinc_mort_ICUCV1)
  results$attributable_deaths <- results$death_treated_hospital + results$death_treated_icu + results$death_treated_ventilator +
    results$death_untreated_hospital + results$death_untreated_icu + results$death_untreated_ventilator
  results$attributable_deaths_end <- last(results$attributable_deaths)
  results$total_deaths <- results$attributable_deaths + results$death_natural_non_exposed + results$death_natural_exposed
  results$total_deaths_end <- last(results$total_deaths)
  results$total_reported_deaths_end <- last(results$cum_mortality)
  
  
  ## AGE DEPENDENT MORTALITY
  cinc_mort_H1 <- parameters["nus"]*parameters["propo2"]*parameters["pdeath_ho"]*dexo2_hist*(out$mean[,(Hindex+1)])+
    parameters["nus"]*(1-parameters["propo2"])*parameters["pdeath_h"]*(out$mean[,(Hindex+1)])
  cinc_mort_HC1 <- parameters["nusc"]*parameters["report_death_HC"]*parameters["propo2"]*parameters["pdeath_hco"]*(out$mean[,(HCindex+1)])+
    parameters["nusc"]*parameters["report_death_HC"]*(1-parameters["propo2"])*parameters["pdeath_hc"]*(out$mean[,(HCindex+1)])
  cinc_mort_ICU1 <- parameters["nu_icu"]*parameters["propo2"]*parameters["pdeath_icuo"]*dexo2_hist*(out$mean[,(ICUindex+1)])+
    parameters["nu_icu"]*(1-parameters["propo2"])*parameters["pdeath_icu"]*(out$mean[,(ICUindex+1)])
  cinc_mort_ICUC1 <- parameters["nu_icuc"]*parameters["propo2"]*parameters["pdeath_icuco"]*dexo2c_hist*(out$mean[,(ICUCindex+1)] )+
    parameters["nu_icuc"]*(1-parameters["propo2"])*parameters["pdeath_icuc"]*(out$mean[,(ICUCindex+1)] )
  cinc_mort_Vent1  <- parameters["nu_vent"]*parameters["pdeath_vent"]*dexv_hist*(out$mean[,(Ventindex+1)] )
  cinc_mort_VentC1 <- parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out$mean[,(VentCindex+1)] )
  cinc_mort_ICUCV1 <- parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out$mean[,(ICUCVindex+1)] )
  cinc_mort_HCICU1 <- parameters["nusc"]*parameters["report_death_HC"]*parameters["propo2"]*parameters["pdeath_icu_hco"]*(out$mean[,(HCICUindex+1)] )+
    parameters["nusc"]*parameters["report_death_HC"]*(1-parameters["propo2"])*parameters["pdeath_icu_hc"]*(out$mean[,(HCICUindex+1)] )
  cinc_mort_HCV1 <- parameters["nu_ventc"]*parameters["report_death_HC"]*parameters["pdeath_vent_hc"]*(out$mean[,(HCVindex+1)] )
  
  totage1<-as.data.frame(cinc_mort_H1+cinc_mort_HC1+cinc_mort_ICU1+cinc_mort_ICUC1+
                           cinc_mort_Vent1+cinc_mort_VentC1+cinc_mort_ICUCV1+cinc_mort_HCICU1+cinc_mort_HCV1)
  
  basemort_H1<-(out$mean[,(Hindex+1)])
  basemort_HC1<-parameters["report_death_HC"]*(out$mean[,(HCindex+1)])
  basemort_ICU1<-(out$mean[,(ICUindex+1)])
  basemort_ICUC1<-(out$mean[,(ICUCindex+1)])
  basemort_ICUCV1<-(out$mean[,(ICUCVindex+1)])
  basemort_Vent1<-(out$mean[,(Ventindex+1)])
  basemort_VentC1<-(out$mean[,(VentCindex+1)])
  basemort_HCICU1<-parameters["report_death_HC"]*(out$mean[,(HCICUindex+1)])
  basemort_HCV1<-parameters["report_death_HC"]*(out$mean[,(HCVindex+1)])
  basemort_I<-parameters["report_natdeathI"]*(out$mean[,(Iindex+1)])
  basemort_QI<-parameters["report_natdeathI"]*(out$mean[,(QIindex+1)])
  basemort_E<-parameters["report_natdeathI"]*(out$mean[,(Eindex+1)])
  basemort_QE<-parameters["report_natdeathI"]*(out$mean[,(QEindex+1)])
  basemort_EV<-parameters["report_natdeathI"]*(out$mean[,(EVindex+1)])
  basemort_EVR<-parameters["report_natdeathI"]*(out$mean[,(EVRindex+1)])
  basemort_ER<-parameters["report_natdeathI"]*(out$mean[,(ERindex+1)])
  basemort_QEV<-parameters["report_natdeathI"]*(out$mean[,(QEVindex+1)])
  basemort_QEVR<-parameters["report_natdeathI"]*(out$mean[,(QEVRindex+1)])
  basemort_QER<-parameters["report_natdeathI"]*(out$mean[,(QERindex+1)])
  basemort_CL<-parameters["report_natdeathCL"]*(out$mean[,(CLindex+1)])
  basemort_QC<-parameters["report_natdeathCL"]*(out$mean[,(QCindex+1)])
  basemort_X<-parameters["report_natdeathCL"]*(out$mean[,(Xindex+1)])
  
  totbase1<-as.data.frame(basemort_H1+basemort_HC1+basemort_ICU1+basemort_ICUC1+basemort_ICUCV1+
                            basemort_Vent1+basemort_VentC1+basemort_HCICU1+basemort_HCV1+ 
                            basemort_I+basemort_QI+basemort_E+basemort_QE+basemort_EV+basemort_EVR+
                            basemort_ER+basemort_QEV+basemort_QEVR+basemort_QER+basemort_CL+basemort_QC+basemort_X)
  
  tc<-c()
  for (i in 1:dim(cinc_mort_H1)[1]) {
    for (j in 1:dim(cinc_mort_H1)[2]) {
      # print(totage1[i,j]*ifr[j,2]+totbase1[i,j]*mort[j])
      tc<-rbind(tc,c(i, j, totage1[i,j]*ifr[j,2]+totbase1[i,j]*mort[j]))
    }
  }
  tc<-as.data.frame(tc)
  colnames(tc)<-c("Day","Age","value")

  results$tc <- tc %>%
    mutate(Date = startdate + Day,
           age_cat = case_when(
             Age >=  1 & Age <= 6   ~ "<= 30 y.o.",
             Age >  6 & Age <= 8    ~ "30-40 y.o.",
             Age >  8 & Age <= 10    ~ "40-50 y.o.",
             Age >  10 & Age <= 12    ~ "50-60 y.o.",
             Age >  12 & Age <= 14    ~ "60-70 y.o.",
             Age >=  15  ~ ">= 70 y.o.")) %>%
    mutate(age_cat = factor(age_cat, levels = rev(c("<= 30 y.o.", "30-40 y.o.",
                                                    "40-50 y.o.", "50-60 y.o.", "60-70 y.o.", ">= 70 y.o."))))
  

  mortality_lag <- data.frame(Age = popstruc$agefloor)
  if(nrow(out_mean) >= 30)  mortality_lag <- bind_cols(mortality_lag,
                                                       data.frame(day30 = out_mean[30,CMindex+1]/out_mean[30,Cindex+1]) %>%
                                                         mutate(day30 = ifelse(is.infinite(day30), 0, day30)))
  if(nrow(out_mean) >= 60)  mortality_lag <- bind_cols(mortality_lag,
                                                       data.frame(day60 = out_mean[60,CMindex+1]/out_mean[60,Cindex+1]) %>%
                                                         mutate(day60 = ifelse(is.infinite(day60), 0, day60)))
  if(nrow(out_mean) >= 90)  mortality_lag <- bind_cols(mortality_lag,
                                                       data.frame(day90 = out_mean[90,CMindex+1]/out_mean[90,Cindex+1]) %>%
                                                         mutate(day90 = ifelse(is.infinite(day90), 0, day90))) 
  if(nrow(out_mean) >= 120)  mortality_lag <- bind_cols(mortality_lag,
                                                        data.frame(day120 = out_mean[120,CMindex+1]/out_mean[120,Cindex+1]) %>%
                                                          mutate(day120 = ifelse(is.infinite(day120), 0, day120)))

  results$mortality_lag <- mortality_lag

  
  if(iterations>1){
    
    cinc_mort_1 <- cumsum(rowSums(parameters["nus"]*parameters["propo2"]*parameters["pdeath_ho"]*dexo2_hist*(out_min[,(Hindex+1)]%*%ifr[,2])))
    cinc_mort_2 <- cumsum(rowSums(parameters["nus"]*(1-parameters["propo2"])*parameters["pdeath_h"]*(out_min[,(Hindex+1)]%*%ifr[,2])))
    cinc_mort_3 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*parameters["propo2"]*parameters["pdeath_hco"]*(out_min[,(HCindex+1)]%*%ifr[,2])))
    cinc_mort_4 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*(1-parameters["propo2"])*parameters["pdeath_hc"]*(out_min[,(HCindex+1)]%*%ifr[,2])))
    cinc_mort_5 <- cumsum(rowSums(parameters["nu_icu"]*parameters["propo2"]*parameters["pdeath_icuo"]*dexo2_hist*(out_min[,(ICUindex+1)]%*%ifr[,2])))
    cinc_mort_6 <- cumsum(rowSums(parameters["nu_icu"]*(1-parameters["propo2"])*parameters["pdeath_icu"]*(out_min[,(ICUindex+1)]%*%ifr[,2])))
    cinc_mort_7 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["propo2"]*parameters["pdeath_icuco"]*dexo2c_hist*(out_min[,(ICUCindex+1)]%*%ifr[,2])))
    cinc_mort_8 <- cumsum(rowSums(parameters["nu_icuc"]*(1-parameters["propo2"])*parameters["pdeath_icuc"]*(out_min[,(ICUCindex+1)]%*%ifr[,2])))
    cinc_mort_9 <- cumsum(rowSums(parameters["nu_vent"]*parameters["pdeath_vent"]*dexv_hist*(out_min[,(Ventindex+1)]%*%ifr[,2])))
    cinc_mort_10 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_min[,(VentCindex+1)]%*%ifr[,2])))
    cinc_mort_11 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_min[,(ICUCVindex+1)]%*%ifr[,2])))
    cinc_mort_121 <- cumsum(rowSums(parameters["nusc"]*parameters["propo2"]*parameters["pdeath_icu_hco"]*(out_min[,(HCICUindex+1)]%*%ifr[,2])))
    cinc_mort_131 <- cumsum(rowSums(parameters["nusc"]*(1-parameters["propo2"])*parameters["pdeath_icu_hc"]*(out_min[,(HCICUindex+1)]%*%ifr[,2])))
    cinc_mort_141 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_vent_hc"]*(out_min[,(HCVindex+1)]%*%ifr[,2])))
    
    cinc_mort_all_min<-cinc_mort_1+cinc_mort_2+cinc_mort_3+cinc_mort_4+cinc_mort_5+cinc_mort_6+
      cinc_mort_7+cinc_mort_8+cinc_mort_9+cinc_mort_10+cinc_mort_11+cinc_mort_121+cinc_mort_131+cinc_mort_141
    
    cinc_mort_1 <- cumsum(rowSums(parameters["nus"]*parameters["propo2"]*parameters["pdeath_ho"]*dexo2_hist*(out_max[,(Hindex+1)]%*%ifr[,2])))
    cinc_mort_2 <- cumsum(rowSums(parameters["nus"]*(1-parameters["propo2"])*parameters["pdeath_h"]*(out_max[,(Hindex+1)]%*%ifr[,2])))
    cinc_mort_3 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*parameters["propo2"]*parameters["pdeath_hco"]*(out_max[,(HCindex+1)]%*%ifr[,2])))
    cinc_mort_4 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*(1-parameters["propo2"])*parameters["pdeath_hc"]*(out_max[,(HCindex+1)]%*%ifr[,2])))
    cinc_mort_5 <- cumsum(rowSums(parameters["nu_icu"]*parameters["propo2"]*parameters["pdeath_icuo"]*dexo2_hist*(out_max[,(ICUindex+1)]%*%ifr[,2])))
    cinc_mort_6 <- cumsum(rowSums(parameters["nu_icu"]*(1-parameters["propo2"])*parameters["pdeath_icu"]*(out_max[,(ICUindex+1)]%*%ifr[,2])))
    cinc_mort_7 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["propo2"]*parameters["pdeath_icuco"]*dexo2c_hist*(out_max[,(ICUCindex+1)]%*%ifr[,2])))
    cinc_mort_8 <- cumsum(rowSums(parameters["nu_icuc"]*(1-parameters["propo2"])*parameters["pdeath_icuc"]*(out_max[,(ICUCindex+1)]%*%ifr[,2])))
    cinc_mort_9 <- cumsum(rowSums(parameters["nu_vent"]*parameters["pdeath_vent"]*dexv_hist*(out_max[,(Ventindex+1)]%*%ifr[,2])))
    cinc_mort_10 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_max[,(VentCindex+1)]%*%ifr[,2])))
    cinc_mort_11 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_max[,(ICUCVindex+1)]%*%ifr[,2])))
    cinc_mort_121 <- cumsum(rowSums(parameters["nusc"]*parameters["propo2"]*parameters["pdeath_icu_hco"]*(out_max[,(HCICUindex+1)]%*%ifr[,2])))
    cinc_mort_131 <- cumsum(rowSums(parameters["nusc"]*(1-parameters["propo2"])*parameters["pdeath_icu_hc"]*(out_max[,(HCICUindex+1)]%*%ifr[,2])))
    cinc_mort_141 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_vent_hc"]*(out_max[,(HCVindex+1)]%*%ifr[,2])))
    
    cinc_mort_all_max<-cinc_mort_1+cinc_mort_2+cinc_mort_3+cinc_mort_4+cinc_mort_5+cinc_mort_6+
      cinc_mort_7+cinc_mort_8+cinc_mort_9+cinc_mort_10+cinc_mort_11+cinc_mort_121+cinc_mort_131+cinc_mort_141
    
    base_mort_H1 <- cumsum(rowSums(out_min[,(Hindex+1)]%*%mort))
    base_mort_ICU1 <- cumsum(rowSums(out_min[,(ICUindex+1)]%*%mort))
    base_mort_ICUC1 <- cumsum(rowSums(out_min[,(ICUCindex+1)]%*%mort))
    base_mort_ICUCV1 <- cumsum(rowSums(out_min[,(ICUCVindex+1)]%*%mort))
    base_mort_Vent1 <- cumsum(rowSums(out_min[,(Ventindex+1)]%*%mort))
    base_mort_VentC1 <- cumsum(rowSums(out_min[,(VentCindex+1)]%*%mort))
    base_mort_Z1 <- cumsum(rowSums(out_min[,(Zindex+1)]%*%mort))
    base_mort_HC11 <- cumsum(rowSums(out_min[,(HCindex+1)]%*%mort))
    base_mort_HCICU11 <- cumsum(rowSums(out_min[,(HCICUindex+1)]%*%mort))
    base_mort_HCV11 <- cumsum(rowSums(out_min[,(HCVindex+1)]%*%mort))
    base_mort_E11 <- cumsum(rowSums(out_min[,(Eindex+1)]%*%mort))
    base_mort_I11 <- cumsum(rowSums(out_min[,(Iindex+1)]%*%mort))
    base_mort_CL11 <- cumsum(rowSums(out_min[,(CLindex+1)]%*%mort))
    base_mort_X11 <- cumsum(rowSums(out_min[,(Xindex+1)]%*%mort))
    base_mort_QE11 <- cumsum(rowSums(out_min[,(QEindex+1)]%*%mort))
    base_mort_QI11 <- cumsum(rowSums(out_min[,(QIindex+1)]%*%mort))
    base_mort_QC11 <- cumsum(rowSums(out_min[,(QCindex+1)]%*%mort))
    base_mort_ER11 <- cumsum(rowSums(out_min[,(ERindex+1)]%*%mort))
    base_mort_EV11 <- cumsum(rowSums(out_min[,(EVindex+1)]%*%mort))
    base_mort_EVR11 <- cumsum(rowSums(out_min[,(EVRindex+1)]%*%mort))
    base_mort_QEV11 <- cumsum(rowSums(out_min[,(QEVindex+1)]%*%mort))
    base_mort_QER11 <- cumsum(rowSums(out_min[,(QERindex+1)]%*%mort))
    base_mort_QEVR11 <- cumsum(rowSums(out_min[,(QEVRindex+1)]%*%mort))
    
    nat_deaths_inf_min <- last(round(base_mort_E11 + base_mort_I11 + base_mort_CL11 + base_mort_X11 + 
                              base_mort_ER11 + base_mort_EV11+  base_mort_EVR11+   
                              base_mort_QE11 + base_mort_QI11 + base_mort_QC11 +  
                              base_mort_QEV11 + base_mort_QER11 + base_mort_QEVR11 + base_mort_Z1+
                              base_mort_H1+base_mort_HC11+base_mort_ICU1+base_mort_ICUC1+base_mort_ICUCV1+
                              base_mort_Vent1+base_mort_VentC1+base_mort_HCICU11+base_mort_HCV11))
    
    base_mort_H1 <- cumsum(rowSums(out_max[,(Hindex+1)]%*%mort))
    base_mort_ICU1 <- cumsum(rowSums(out_max[,(ICUindex+1)]%*%mort))
    base_mort_ICUC1 <- cumsum(rowSums(out_max[,(ICUCindex+1)]%*%mort))
    base_mort_ICUCV1 <- cumsum(rowSums(out_max[,(ICUCVindex+1)]%*%mort))
    base_mort_Vent1 <- cumsum(rowSums(out_max[,(Ventindex+1)]%*%mort))
    base_mort_VentC1 <- cumsum(rowSums(out_max[,(VentCindex+1)]%*%mort))
    base_mort_Z1 <- cumsum(rowSums(out_max[,(Zindex+1)]%*%mort))
    base_mort_HC11 <- cumsum(rowSums(out_max[,(HCindex+1)]%*%mort))
    base_mort_HCICU11 <- cumsum(rowSums(out_max[,(HCICUindex+1)]%*%mort))
    base_mort_HCV11 <- cumsum(rowSums(out_max[,(HCVindex+1)]%*%mort))
    base_mort_E11 <- cumsum(rowSums(out_max[,(Eindex+1)]%*%mort))
    base_mort_I11 <- cumsum(rowSums(out_max[,(Iindex+1)]%*%mort))
    base_mort_CL11 <- cumsum(rowSums(out_max[,(CLindex+1)]%*%mort))
    base_mort_X11 <- cumsum(rowSums(out_max[,(Xindex+1)]%*%mort))
    base_mort_QE11 <- cumsum(rowSums(out_max[,(QEindex+1)]%*%mort))
    base_mort_QI11 <- cumsum(rowSums(out_max[,(QIindex+1)]%*%mort))
    base_mort_QC11 <- cumsum(rowSums(out_max[,(QCindex+1)]%*%mort))
    base_mort_ER11 <- cumsum(rowSums(out_max[,(ERindex+1)]%*%mort))
    base_mort_EV11 <- cumsum(rowSums(out_max[,(EVindex+1)]%*%mort))
    base_mort_EVR11 <- cumsum(rowSums(out_max[,(EVRindex+1)]%*%mort))
    base_mort_QEV11 <- cumsum(rowSums(out_max[,(QEVindex+1)]%*%mort))
    base_mort_QER11 <- cumsum(rowSums(out_max[,(QERindex+1)]%*%mort))
    base_mort_QEVR11 <- cumsum(rowSums(out_max[,(QEVRindex+1)]%*%mort))
    
    nat_deaths_inf_max<- last(round(base_mort_E11 + base_mort_I11 + base_mort_CL11 + base_mort_X11 + 
                                       base_mort_ER11 + base_mort_EV11+  base_mort_EVR11+   
                                       base_mort_QE11 + base_mort_QI11 + base_mort_QC11 +  
                                       base_mort_QEV11 + base_mort_QER11 + base_mort_QEVR11 + base_mort_Z1+
                                       base_mort_H1+base_mort_HC11+base_mort_ICU1+base_mort_ICUC1+base_mort_ICUCV1+
                                       base_mort_Vent1+base_mort_VentC1+base_mort_HCICU11+base_mort_HCV11))
    
    
    previcureq1_max<-rowSums(out_max[,(Hindex+1)])+ rowSums(out_max[,(ICUCindex+1)])+rowSums(out_max[,(ICUCVindex+1)]) # surge beds occupancy
    previcureq21_max<-rowSums(out_max[,(ICUindex+1)])+rowSums(out_max[,(VentCindex+1)])   # icu beds occupancy
    previcureq31_max<-rowSums(out_max[,(Ventindex+1)])   # ventilator occupancy
    cmortality1_max<-rowSums(out_max[,(CMindex+1)])      # cumulative mortality
    overloadH1_max<-rowSums(out_max[,(HCindex+1)])       # requirement for beds
    overloadICU1_max<-rowSums(out_max[,(ICUCindex+1)])+ rowSums(out_max[,(HCICUindex+1)])  # requirement for icu beds
    overloadICUV1_max<-rowSums(out_max[,(ICUCVindex+1)])+ rowSums(out_max[,(HCVindex+1)])  # requirement for ventilators
    overloadVent1_max<-rowSums(out_max[,(VentCindex+1)]) # requirement for ventilators
    ccases1_max<-rowSums(out_max[,(Cindex+1)])           # cumulative cases
    reqsurge1_max<-rowSums(out_max[,(Hindex+1)])+overloadH1  # surge beds total requirements
    reqicu1_max<-rowSums(out_max[,(ICUindex+1)])+overloadICU1 # ICU beds total requirements
    reqvent1_max<-rowSums(out_max[,(Ventindex+1)])+overloadICUV1+overloadVent1 # ventilator beds total requirements
    
    previcureq1_min<-rowSums(out_min[,(Hindex+1)])+rowSums(out_min[,(ICUCindex+1)])+rowSums(out_min[,(ICUCVindex+1)]) # surge beds occupancy
    previcureq21_min<-rowSums(out_min[,(ICUindex+1)])+rowSums(out_min[,(VentCindex+1)])   # icu beds occupancy
    previcureq31_min<-rowSums(out_min[,(Ventindex+1)])   # ventilator occupancy
    cmortality1_min<-rowSums(out_min[,(CMindex+1)])      # cumulative mortality
    overloadH1_min<-rowSums(out_min[,(HCindex+1)])       # requirement for beds
    overloadICU1_min<-rowSums(out_min[,(ICUCindex+1)])   # requirement for icu beds
    overloadICUV1_min<-rowSums(out_min[,(ICUCVindex+1)]) # requirement for ventilators
    overloadVent1_min<-rowSums(out_min[,(VentCindex+1)]) # requirement for ventilators
    ccases1_min<-rowSums(out_min[,(Cindex+1)])           # cumulative cases
    reqsurge1_min<-rowSums(out_min[,(Hindex+1)])+overloadH1  # surge beds total requirements
    reqicu1_min<-rowSums(out_min[,(ICUindex+1)])+overloadICU1 # ICU beds total requirements
    reqvent1_min<-rowSums(out_min[,(Ventindex+1)])+overloadICUV1+overloadVent1 # ventilator beds total requirements
    
    results$Rt_max <- out$max_Rt
    results$Rt_min <- out$min_Rt
    
    results$daily_incidence_max <- out$max_cases
    results$daily_incidence_min <- out$min_cases  
    
    results$deaths_from_covid_min<-last(cinc_mort_all_min)
    results$deaths_from_covid_max<-last(cinc_mort_all_max)
      
    results$deaths_with_covid_min<-last(nat_deaths_inf_min)
    results$deaths_with_covid_max<-last(nat_deaths_inf_max)
    
    results$daily_total_cases_max <- out$max_daily_infection
    results$daily_total_cases_min <- out$min_daily_infection
    
    results$total_reported_deaths_end_min <- last(cmortality1_min)
    results$total_reported_deaths_end_max <- last(cmortality1_max)
    
    results$pct_total_pop_infected_min <- out$min_infections  # proportion of the  population that has been infected at the end of the simulation
    results$pct_total_pop_infected_max <- out$max_infections  # proportion of the  population that has been infected at the end of the simulation
  }
  return(results)
}



multi_runs<-function(Y,times,parameters,input,iterations,noise,confidence,fit,fit_mat){
  results <- list()
  aux<-array(0, dim=c(length(times),35*A+1,iterations))
  results$mean<-matrix(0,nrow = length(times),ncol = 35*A+1)
  results$min<-matrix(0,nrow = length(times),ncol = 35*A+1)
  results$max<-matrix(0,nrow = length(times),ncol = 35*A+1)
  results$mean_cases<-matrix(0,nrow = length(times),ncol = 35*A+1)
  results$min_cases<-matrix(0,nrow = length(times),ncol = 35*A+1)
  results$max_cases<-matrix(0,nrow = length(times),ncol = 35*A+1)
  results$mean_cum_cases<-matrix(0,nrow = length(times),ncol = 1)
  results$min_cum_cases<-matrix(0,nrow = length(times),ncol = 1)
  results$max_cum_cases<-matrix(0,nrow = length(times),ncol = 1)
  results$mean_daily_infection<-matrix(0,nrow = length(times),ncol = 1)
  results$min_daily_infection<-matrix(0,nrow = length(times),ncol = 1)
  results$max_daily_infection<-matrix(0,nrow = length(times),ncol = 1)
  results$mean_ab<-matrix(0,nrow = length(times),ncol = 1)
  results$min_ab<-matrix(0,nrow = length(times),ncol = 1)
  results$max_ab<-matrix(0,nrow = length(times),ncol = 1)
  cases<-matrix(0, nrow=length(times),ncol=iterations)
  cum_cases<-matrix(0, nrow=length(times),ncol=iterations)
  day_infections<-matrix(0, nrow=length(times),ncol=iterations)
  Rt_aux<-matrix(0, nrow=length(times),ncol=iterations)
  infections<-matrix(0, nrow=iterations,ncol=1)
  Rt <- NULL
  print(iterations)
  
  param_vector<-parameters
  if(iterations>1){
    for (i in 1:iterations){
      print(i)
      param_vector[parameters_noise]<-parameters[parameters_noise]+rnorm(length(parameters_noise),mean=0,sd=noise*abs(parameters[parameters_noise]))
      if(fit){
        param_vector[parameters_fit]<-fit_mat[,floor(runif(1)*(length(fit_mat[1,])-1))+1]
        initE[aci]<-round(sum(popstruc[,2])/param_vector["init"])    
        initS<-popstruc[,2]-initE-initI-initCL-initR-initX-initZ-initV-initH-initHC-initICU-initICUC-initICUCV-initVent-initVentC-
          initQS-initQE-initQI-initQR-initQC-initEV-initER-initEVR-initVR-initQV-initQEV-initQEVR-initQER-initQVR-
          initHCICU-initHCV 
        Y<-c(initS,initE,initI,initR,initX,initH,initHC,initC,initCM,initV, initQS, initQE, initQI, initQR, initCL, initQC, initICU, 
          initICUC, initICUCV, initVent, initVentC, initCMC,initZ, initEV, initER, initEVR, initVR, 
          initQV,initQEV,initQEVR,initQER,initQVR,initHCICU,initHCV,initAb)
      }
      out0 <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = param_vector, input=input)
      aux[,,i]<-out0
      
      critH<-c()
      crit<-c()
      critV<-c()
      for (ii in 1:length(times)){
        critH[ii]<-min(1-fH((sum(out0[ii,(Hindex+1)]))+sum(out0[ii,(ICUCindex+1)])+sum(out0[ii,(ICUCVindex+1)])),1)
        crit[ii]<-min(1-fICU((sum(out0[ii,(ICUindex+1)]))+(sum(out0[ii,(Ventindex+1)]))+(sum(out0[ii,(VentCindex+1)]))),1)
        critV[ii]<-min(1-fVent((sum(out0[ii,(Ventindex+1)]))),1)
      }
      
      # daily incidence
      incidence<-param_vector["report"]*param_vector["gamma"]*(1-param_vector["pclin"])*out0[,(Eindex+1)]%*%(1-ihr[,2])+
        param_vector["reportc"]*param_vector["gamma"]*param_vector["pclin"]*out0[,(Eindex+1)]%*%(1-ihr[,2])+
        param_vector["report"]*param_vector["gamma"]*(1-param_vector["pclin"])*out0[,(QEindex+1)]%*%(1-ihr[,2])+
        param_vector["reportc"]*param_vector["gamma"]*param_vector["pclin"]*out0[,(QEindex+1)]%*%(1-ihr[,2])+
        param_vector["report_v"]*param_vector["gamma"]*(1-param_vector["pclin_v"])*out0[,(EVindex+1)]%*%(1-param_vector["sigmaEV"]*ihr[,2])+
        param_vector["report_cv"]*param_vector["gamma"]*param_vector["pclin_v"]*out0[,(EVindex+1)]%*%(1-param_vector["sigmaEV"]*ihr[,2])+
        param_vector["report_vr"]*param_vector["gamma"]*(1-param_vector["pclin_vr"])*out0[,(EVRindex+1)]%*%(1-param_vector["sigmaEVR"]*ihr[,2])+
        param_vector["report_cvr"]*param_vector["gamma"]*param_vector["pclin_vr"]*out0[,(EVRindex+1)]%*%(1-param_vector["sigmaEVR"]*ihr[,2])+
        param_vector["report_r"]*param_vector["gamma"]*(1-param_vector["pclin_r"])*out0[,(ERindex+1)]%*%(1-param_vector["sigmaER"]*ihr[,2])+
        param_vector["report_cr"]*param_vector["gamma"]*param_vector["pclin_r"]*out0[,(ERindex+1)]%*%(1-param_vector["sigmaER"]*ihr[,2])
      
      incidenceh<- param_vector["gamma"]*out0[,(Eindex+1)]%*%ihr[,2]*(1-critH)*(1-param_vector["prob_icu"])*param_vector["reporth"]+
        param_vector["gamma"]*out0[,(Eindex+1)]%*%ihr[,2]*(1-critH)*(1-param_vector["prob_icu"])*(1-param_vector["reporth"])*param_vector["reporth_g"]+
        param_vector["gamma"]*out0[,(QEindex+1)]%*%ihr[,2]*(1-critH)*(1-param_vector["prob_icu"])*param_vector["reporth"]+
        param_vector["gamma"]*out0[,(QEindex+1)]%*%ihr[,2]*(1-critH)*(1-param_vector["prob_icu"])*(1-param_vector["reporth"])*param_vector["reporth_g"]+
        param_vector["gamma"]*param_vector["sigmaEV"]*out0[,(EVindex+1)]%*%ihr[,2]*(1-critH)*(1-param_vector["prob_icu_v"])*param_vector["reporth"]+
        param_vector["gamma"]*param_vector["sigmaEVR"]*out0[,(EVRindex+1)]%*%ihr[,2]*(1-critH)*(1-param_vector["prob_icu_vr"])*param_vector["reporth"]+
        param_vector["gamma"]*param_vector["sigmaER"]*out0[,(ERindex+1)]%*%ihr[,2]*(1-critH)*(1-param_vector["prob_icu_r"])*param_vector["reporth"]+
        param_vector["gamma"]*out0[,(Eindex+1)]%*%ihr[,2]*critH*param_vector["reporth_g"]*(1-param_vector["prob_icu"])+
        param_vector["gamma"]*out0[,(QEindex+1)]%*%ihr[,2]*critH*param_vector["reporth_g"]*(1-param_vector["prob_icu"])+
        param_vector["gamma"]*param_vector["sigmaEV"]*out0[,(EVindex+1)]%*%ihr[,2]*critH*param_vector["reporth_g"]*(1-param_vector["prob_icu_v"])+
        param_vector["gamma"]*param_vector["sigmaEVR"]*out0[,(EVRindex+1)]%*%ihr[,2]*critH*param_vector["reporth_g"]*(1-param_vector["prob_icu_vr"])+
        param_vector["gamma"]*param_vector["sigmaER"]*out0[,(ERindex+1)]%*%ihr[,2]*critH*param_vector["reporth_g"]*(1-param_vector["prob_icu_r"])+
        #ICU
        param_vector["gamma"]*out0[,(Eindex+1)]%*%ihr[,2]*param_vector["prob_icu"]*(1-crit)*param_vector["reporth_ICU"]+
        param_vector["gamma"]*out0[,(QEindex+1)]%*%ihr[,2]*param_vector["prob_icu"]*(1-crit)*param_vector["reporth_ICU"]+
        param_vector["gamma"]*out0[,(Eindex+1)]%*%ihr[,2]*param_vector["prob_icu"]*crit*param_vector["reporth_ICU"]*param_vector["reporth_g"]+
        param_vector["gamma"]*out0[,(QEindex+1)]%*%ihr[,2]*param_vector["prob_icu"]*crit*param_vector["reporth_ICU"]*param_vector["reporth_g"]+
        param_vector["gamma"]*param_vector["sigmaEV"]*out0[,(EVindex+1)]%*%ihr[,2]*(1-crit)*param_vector["prob_icu_v"]*param_vector["reporth_ICU"]+
        param_vector["gamma"]*param_vector["sigmaEVR"]*out0[,(EVRindex+1)]%*%ihr[,2]*(1-crit)*param_vector["prob_icu_vr"]*param_vector["reporth_ICU"]+
        param_vector["gamma"]*param_vector["sigmaER"]*out0[,(ERindex+1)]%*%ihr[,2]*(1-crit)*param_vector["prob_icu_r"]*param_vector["reporth_ICU"]+
        param_vector["gamma"]*param_vector["sigmaEV"]*out0[,(EVindex+1)]%*%ihr[,2]*crit*param_vector["prob_icu_v"]*param_vector["reporth_ICU"]*param_vector["reporth_g"]+
        param_vector["gamma"]*param_vector["sigmaEVR"]*out0[,(EVRindex+1)]%*%ihr[,2]*crit*param_vector["prob_icu_vr"]*param_vector["reporth_ICU"]*param_vector["reporth_g"]+
        param_vector["gamma"]*param_vector["sigmaER"]*out0[,(ERindex+1)]%*%ihr[,2]*crit*param_vector["prob_icu_r"]*param_vector["reporth_ICU"]*param_vector["reporth_g"]+
        param_vector["gamma"]*out0[,(Eindex+1)]%*%ihr[,2]*param_vector["prob_icu"]*(1-param_vector["reporth_ICU"])*param_vector["reporth_g"]+
        param_vector["gamma"]*out0[,(QEindex+1)]%*%ihr[,2]*param_vector["prob_icu"]*(1-param_vector["reporth_ICU"])*param_vector["reporth_g"]+
        param_vector["gamma"]*param_vector["sigmaEV"]*out0[,(EVindex+1)]%*%ihr[,2]*param_vector["prob_icu_v"]*(1-param_vector["reporth_ICU"])*param_vector["reporth_g"]+
        param_vector["gamma"]*param_vector["sigmaEVR"]*out0[,(EVRindex+1)]%*%ihr[,2]*param_vector["prob_icu_vr"]*(1-param_vector["reporth_ICU"])*param_vector["reporth_g"]+
        param_vector["gamma"]*param_vector["sigmaER"]*out0[,(ERindex+1)]%*%ihr[,2]*param_vector["prob_icu_r"]*(1-param_vector["reporth_ICU"])*param_vector["reporth_g"]
      
      
      cases[,i]<-(rowSums(incidence)+rowSums(incidenceh))           # daily incidence cases
      cum_cases[,i]<-colSums(incidence)+colSums(incidenceh)         # cumulative incidence cases
      day_infections[,i]<- round(rowSums(param_vector["gamma"]*out0[,(Eindex+1)]+
                                         param_vector["gamma"]*out0[,(QEindex+1)]+
                                         param_vector["gamma"]*out0[,(EVindex+1)]+
                                         param_vector["gamma"]*out0[,(EVRindex+1)]+
                                         param_vector["gamma"]*out0[,(ERindex+1)]))

      # daily infections
      infections[i] <- round(100*last(cumsum(day_infections[,i]))/sum(popstruc[,2]), 1)  # proportion of the  population that has been infected at the end of the simulation
      for (w in (ceiling(1/param_vector["nui"])+1):length(times)){
        Rt_aux[w,i]<-cumsum(sum(param_vector["gamma"]*out0[w,(Eindex+1)]))/cumsum(sum(param_vector["gamma"]*out0[(w-1/param_vector["nui"]),(Eindex+1)]))
        if(Rt_aux[w,i] >= 7) {Rt_aux[w,i]  <- NA}
      }
    } 
    qq <- quantile(infections, c(confidence, 0.5, (1-confidence)))
    results$mean_infections<-qq[2]
    results$min_infections<-qq[1]
    results$max_infections<-qq[3]
    
    for(i in 1:length(out0[,1])){
      qq <- quantile(cases[i,], c(confidence, 0.5, (1-confidence)))
      results$mean_cases[i]<-qq[2]
      results$min_cases[i]<-qq[1]
      results$max_cases[i]<-qq[3]
      
      qq <- quantile(cum_cases[i,], c(confidence, 0.5, (1-confidence)))
      results$mean_cum_cases[i]<-qq[2]
      results$min_cum_cases[i]<-qq[1]
      results$max_cum_cases[i]<-qq[3]
      
      qq <- quantile(day_infections[i,], c(confidence, 0.5, (1-confidence)))
      results$mean_daily_infection[i]<-qq[2]
      results$min_daily_infection[i]<-qq[1]
      results$max_daily_infection[i]<-qq[3]
      
      qq <- quantile(Rt_aux[i,], c(confidence, 0.5, (1-confidence)),na.rm = T)
      results$mean_Rt[i]<-qq[2]
      results$min_Rt[i]<-qq[1]
      results$max_Rt[i]<-qq[3]
      
      for (j in 1:length(out0[1,])){
        qq <- quantile(aux[i,j,], c(confidence, 0.5, (1-confidence)))
        results$mean[i,j]<-qq[2]
        results$min[i,j]<-qq[1]
        results$max[i,j]<-qq[3]
      }
    }
  }else{
    results$mean <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = parameters, input=input)
  }
  return(results)
}
out0<-multi_runs(Y, times, parameters, vectors0, iterations, noise, confidence,0,fit_mat)
out0$min_infections
out0$max_infections

plot(times,rowSums(out0$mean[,Iindex+1]),type = 'l')
polygon(c(times, rev(times)), c(rowSums(out0$max[,Iindex+1]), rev(rowSums(out0$min[,Iindex+1]))),
        col=rgb(0, 0, 0,0.25), border = NA)

plot(times,out0$mean_Rt,type = 'l')
polygon(c(times, rev(times)), c(out0$max_Rt, rev(out0$min_Rt)),
        col=rgb(0, 0, 0,0.25), border = NA)

simul_baseline <- process_ode_outcome(out0,iterations,vectors0)
# # write.csv(simul_baseline, paste0(hilo,"_baseline_",gsub(":|-","",Sys.time()),".csv"))
# 
#future interventions
#extend travel ban, quarantine, hand washing, cocooning the elderly until 1st July
out <-multi_runs(Y, times, parameters, vectors, iterations, noise, confidence,0,fit_mat)
simul_interventions <- process_ode_outcome(out,iterations,vectors)
# write.csv(simul_interventions, paste0(hilo,"_futureIntv_",gsub(":|-","",Sys.time()),".csv"))


#############    PLOTTING
# Fitting tab
# fitting the intervention lines to the data to account for any historical interventions
time<-as.Date(out0$mean[,1]+startdate)
par(mfrow=c(1,2))
# set up the axis limits
xmin<-min(as.Date(cases_rv[,1]))
xmax<-max(as.Date(cases_rv[,1]))
ymax<-max(cases_rv[,2],na.rm = T)
xtick<-seq(xmin, xmax, by=7)
plot(time,rowSums(simul_interventions$daily_incidence),type='l',lwd=3,
     main="New Reported Cases", xlab="Date", ylab="Cases per day",
     xlim=c(xmin,xmax),  ylim=c(0,ymax), col='blue',xaxt="n")
axis(side=1, labels = FALSE)
text(x=xtick,  y=-250, labels = format(xtick,"%b-%d"), srt = 0, xpd = TRUE)
points(as.Date(cases_rv[,1]),cases_rv[,2],pch=19,col='red')

# reset the maximum to the cumulative mortality
ymax<-max(cases_rv[,3],na.rm = T)
plot(time,simul_interventions$cum_mortality,type='l',lwd=3,
     main="Cumulative Mortality", xlab="Date", ylab="Total deaths",
     xlim=c(xmin,xmax), ylim=c(0,ymax), col='blue',xaxt="n")
text(x=xtick,  y=-100, labels = format(xtick,"%b-%d"), srt = 0, xpd = TRUE)
points(as.Date(cases_rv[,1]),cases_rv[,3],pch=19,col='red')


### Predictions tab
par(mfrow=c(1,2))
### Cases at baseline and intervention
ymax<-max(c(cases_rv[,2],rowSums(simul_baseline$daily_incidence),rowSums(simul_interventions$daily_incidence)),na.rm=T)
plot(time,rowSums(simul_baseline$daily_incidence),type='l',lwd=3,col='blue',
     main="Baseline", xlab="Date", ylab="New cases per day",ylim=c(0,ymax))
points(as.Date(cases_rv[,1]),cases_rv[,2],pch=19,col='red')
plot(time,rowSums(simul_interventions$daily_incidence),type='l',lwd=3,col='blue',
     main="Intervention", xlab="Date", ylab="New cases per day",ylim=c(0,ymax))
points(as.Date(cases_rv[,1]),cases_rv[,2],pch=19,col='red')



# # # Hospital prevalences stratified by H,ICU and Vent
ymax<-max(c((simul_baseline$hospital_surge_beds+simul_baseline$icu_beds+simul_baseline$ventilators),(simul_interventions$hospital_surge_beds+simul_interventions$icu_beds+simul_interventions$ventilators)))
time<-as.Date(out0$mean[,1]+startdate)
coul=c("#047883", "#24A9E2","#051A46")
DM<-as.data.frame(cbind(time,simul_baseline$hospital_surge_beds,simul_baseline$icu_beds,simul_baseline$ventilators))
colnames(DM)<-c("Time","Hospital surge beds","ICU beds","Ventilators")
DM$Time<-as.Date(DM$Time,origin = "1970-01-01")
DMF<-melt(DM, id.vars="Time",measure.vars = c("Hospital surge beds","ICU beds","Ventilators"))
d0<-ggplot(DMF, aes(x = Time, y = value,fill=variable)) +
  geom_area()+
  scale_fill_manual(values=coul)

DM<-as.data.frame(cbind(time,simul_interventions$hospital_surge_beds,simul_interventions$icu_beds,simul_interventions$ventilators))
colnames(DM)<-c("Time","Hospital surge beds","ICU beds","Ventilators")
DM$Time<-as.Date(DM$Time, origin = "1970-01-01")
DMF<-melt(DM, id.vars="Time",measure.vars = c("Hospital surge beds","ICU beds","Ventilators"))
d1<-ggplot(DMF, aes(x = Time, y = value,fill=variable)) +
  geom_area()+
  scale_fill_manual(values=coul)

grid.arrange(d0+ylab("Number of Patients")+
               ggtitle("Baseline")+
               ylim(0, ymax)+
               geom_hline(yintercept=(parameters["beds_available"]+parameters["icu_beds_available"]+parameters["ventilators_available"]), linetype="dashed", color = "#047883")+
               geom_hline(yintercept=(parameters["icu_beds_available"]+parameters["ventilators_available"]), linetype="dashed", color = "#24A9E2")+
               geom_hline(yintercept=parameters["ventilators_available"], linetype="dashed", color = "#051A46")+
               theme_bw(),
             d1+ylab("Number of Patients")+
               ggtitle("Intervention")+
               ylim(0, ymax)+
               geom_hline(yintercept=(parameters["beds_available"]+parameters["icu_beds_available"]+parameters["ventilators_available"]), linetype="dashed", color = "#047883")+
               geom_hline(yintercept=(parameters["icu_beds_available"]+parameters["ventilators_available"]), linetype="dashed", color = "#24A9E2")+
               geom_hline(yintercept=parameters["ventilators_available"], linetype="dashed", color = "#051A46")+
               theme_bw(),
             nrow = 1)


# # # Cumulative mortality at baseline and intervention stratified by hospital status
ymax<-max(rowSums(cbind(simul_baseline$death_treated_hospital,
              simul_baseline$death_treated_icu,
              simul_baseline$death_treated_ventilator,
              simul_baseline$death_untreated_hospital,
              simul_baseline$death_untreated_icu,
              simul_baseline$death_untreated_ventilator)), rowSums(cbind(simul_interventions$death_treated_hospital,
                                                              simul_interventions$death_treated_icu,
                                                              simul_interventions$death_treated_ventilator,
                                                              simul_interventions$death_untreated_hospital,
                                                              simul_interventions$death_untreated_icu,
                                                              simul_interventions$death_untreated_ventilator)))
time<-as.Date(out$mean[,1]+startdate)
coul=c("#047883", "#24A9E2","#051A46","#E68029", "#D63304","#D1D604")
DM0<-as.data.frame(cbind(time,
                         simul_baseline$death_treated_hospital,
                         simul_baseline$death_treated_icu,
                         simul_baseline$death_treated_ventilator,
                         simul_baseline$death_untreated_hospital,
                         simul_baseline$death_untreated_icu,
                         simul_baseline$death_untreated_ventilator))
colnames(DM0)<-c("Time", "Treated: Hospital","Treated: ICU","Treated: Ventilator","Untreated: Hospital","Untreated: ICU","Untreated: Ventilator")
DM0$Time<-as.Date(DM0$Time, origin = "1970-01-01")
DMF0<-melt(DM0, id.vars="Time",measure.vars = c("Treated: Hospital","Treated: ICU","Treated: Ventilator","Untreated: Hospital","Untreated: ICU","Untreated: Ventilator"))
m0<-ggplot(DMF0, aes(x = Time, y = value,fill=variable)) +
  geom_area()

DM<-as.data.frame(cbind(time, 
                        simul_interventions$death_treated_hospital,
                        simul_interventions$death_treated_icu,
                        simul_interventions$death_treated_ventilator,
                        simul_interventions$death_untreated_hospital,
                        simul_interventions$death_untreated_icu,
                        simul_interventions$death_untreated_ventilator))
colnames(DM)<-c("Time","Treated: Hospital","Treated: ICU","Treated: Ventilator","Untreated: Hospital","Untreated: ICU","Untreated: Ventilator")
DM$Time<-as.Date(DM$Time, origin = "1970-01-01")
DMF<-melt(DM, id.vars="Time",measure.vars = c("Treated: Hospital","Treated: ICU","Treated: Ventilator","Untreated: Hospital","Untreated: ICU","Untreated: Ventilator"))
m1<-ggplot(DMF, aes(x = Time, y = value,fill=variable)) +
  geom_area()
grid.arrange(m0+ylab("Cumulatice mortality")+
               ggtitle("Baseline")+
               ylim(0, ymax),
             m1+ylab("Cumulatice mortality")+
               ggtitle("Intervention")+
               ylim(0, ymax),
             nrow = 1)



# Estimated basic reproduction number, R_t
par(mfrow=c(1,2))
ymax<-max(c(simul_baseline$Rt[!is.na(simul_baseline$Rt)],simul_interventions$Rt[!is.na(simul_interventions$Rt)]))
plot(time,simul_baseline$Rt,type='l',lwd=3,col='black',
     main="Baseline", xlab="Date", ylab="Reproduction number",ylim=c(0,ymax))
lines(time,simul_baseline$Rt/simul_baseline$Rt,lwd=2,col='grey')
plot(time,simul_interventions$Rt,type='l',lwd=3,col='black',
     main="Intervention", xlab="Date", ylab="Reproduction number",ylim=c(0,ymax))
lines(time,simul_interventions$Rt/simul_interventions$Rt,lwd=2,col='grey')


dexo2_hist <- rep(0,length(times))
dexo2c_hist <- rep(0,length(times))
dexv_hist <- rep(0,length(times))
dexvc_hist <- rep(0,length(times))
for (tt in times) {
  if(tt < max(times)){
    if(vectors$dex[tt*20+1]) {
      dexo2_hist[tt+1] <- parameters["dexo2"]
      dexo2c_hist[tt+1] <- parameters["dexo2c"]
      dexv_hist[tt+1] <- parameters["dexv"]
      dexvc_hist[tt+1] <- parameters["dexvc"]
    } else {
      dexo2_hist[tt+1] <- 1
      dexo2c_hist[tt+1] <- 1
      dexv_hist[tt+1] <- 1
      dexvc_hist[tt+1] <- 1
    }
  } else {
    dexo2_hist[tt+1] <- dexo2_hist[tt]
    dexo2c_hist[tt+1] <- dexo2c_hist[tt]
    dexv_hist[tt+1] <- dexv_hist[tt]
    dexvc_hist[tt+1] <- dexvc_hist[tt]
  }
}

## AGE DEPENDENT MORTALITY
cinc_mort_H1 <- parameters["nus"]*parameters["propo2"]*parameters["pdeath_ho"]*dexo2_hist*(out$mean[,(Hindex+1)])+
  parameters["nus"]*(1-parameters["propo2"])*parameters["pdeath_h"]*(out$mean[,(Hindex+1)])
cinc_mort_HC1 <- parameters["nusc"]*parameters["report_death_HC"]*parameters["propo2"]*parameters["pdeath_hco"]*(out$mean[,(HCindex+1)])+
  parameters["nusc"]*parameters["report_death_HC"]*(1-parameters["propo2"])*parameters["pdeath_hc"]*(out$mean[,(HCindex+1)])
cinc_mort_ICU1 <- parameters["nu_icu"]*parameters["propo2"]*parameters["pdeath_icuo"]*dexo2_hist*(out$mean[,(ICUindex+1)])+
  parameters["nu_icu"]*(1-parameters["propo2"])*parameters["pdeath_icu"]*(out$mean[,(ICUindex+1)])
cinc_mort_ICUC1 <- parameters["nu_icuc"]*parameters["propo2"]*parameters["pdeath_icuco"]*dexo2c_hist*(out$mean[,(ICUCindex+1)] )+
  parameters["nu_icuc"]*(1-parameters["propo2"])*parameters["pdeath_icuc"]*(out$mean[,(ICUCindex+1)] )
cinc_mort_Vent1  <- parameters["nu_vent"]*parameters["pdeath_vent"]*dexv_hist*(out$mean[,(Ventindex+1)] )
cinc_mort_VentC1 <- parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out$mean[,(VentCindex+1)] )
cinc_mort_ICUCV1 <- parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out$mean[,(ICUCVindex+1)] )
cinc_mort_HCICU1 <- parameters["nusc"]*parameters["report_death_HC"]*parameters["propo2"]*parameters["pdeath_icu_hco"]*(out$mean[,(HCICUindex+1)] )+
  parameters["nusc"]*parameters["report_death_HC"]*(1-parameters["propo2"])*parameters["pdeath_icu_hc"]*(out$mean[,(HCICUindex+1)] )
cinc_mort_HCV1 <- parameters["nu_ventc"]*parameters["report_death_HC"]*parameters["pdeath_vent_hc"]*(out$mean[,(HCVindex+1)] )

totage1<-as.data.frame(cinc_mort_H1+cinc_mort_HC1+cinc_mort_ICU1+cinc_mort_ICUC1+
                         cinc_mort_Vent1+cinc_mort_VentC1+cinc_mort_ICUCV1+cinc_mort_HCICU1+cinc_mort_HCV1)

basemort_H1<-(out$mean[,(Hindex+1)])
basemort_HC1<-parameters["report_death_HC"]*(out$mean[,(HCindex+1)])
basemort_ICU1<-(out$mean[,(ICUindex+1)])
basemort_ICUC1<-(out$mean[,(ICUCindex+1)])
basemort_ICUCV1<-(out$mean[,(ICUCVindex+1)])
basemort_Vent1<-(out$mean[,(Ventindex+1)])
basemort_VentC1<-(out$mean[,(VentCindex+1)])
basemort_HCICU1<-parameters["report_death_HC"]*(out$mean[,(HCICUindex+1)])
basemort_HCV1<-parameters["report_death_HC"]*(out$mean[,(HCVindex+1)])
basemort_I<-parameters["report_natdeathI"]*(out$mean[,(Iindex+1)])
basemort_QI<-parameters["report_natdeathI"]*(out$mean[,(QIindex+1)])
basemort_E<-parameters["report_natdeathI"]*(out$mean[,(Eindex+1)])
basemort_QE<-parameters["report_natdeathI"]*(out$mean[,(QEindex+1)])
basemort_EV<-parameters["report_natdeathI"]*(out$mean[,(EVindex+1)])
basemort_EVR<-parameters["report_natdeathI"]*(out$mean[,(EVRindex+1)])
basemort_ER<-parameters["report_natdeathI"]*(out$mean[,(ERindex+1)])
basemort_QEV<-parameters["report_natdeathI"]*(out$mean[,(QEVindex+1)])
basemort_QEVR<-parameters["report_natdeathI"]*(out$mean[,(QEVRindex+1)])
basemort_QER<-parameters["report_natdeathI"]*(out$mean[,(QERindex+1)])
basemort_CL<-parameters["report_natdeathCL"]*(out$mean[,(CLindex+1)])
basemort_QC<-parameters["report_natdeathCL"]*(out$mean[,(QCindex+1)])
basemort_X<-parameters["report_natdeathCL"]*(out$mean[,(Xindex+1)])

totbase1<-as.data.frame(basemort_H1+basemort_HC1+basemort_ICU1+basemort_ICUC1+basemort_ICUCV1+
                          basemort_Vent1+basemort_VentC1+basemort_HCICU1+basemort_HCV1+ 
                          basemort_I+basemort_QI+basemort_E+basemort_QE+basemort_EV+basemort_EVR+
                          basemort_ER+basemort_QEV+basemort_QEVR+basemort_QER+basemort_CL+basemort_QC+basemort_X)

tc<-c()
for (i in 1:dim(cinc_mort_H1)[1]) {
  for (j in 1:dim(cinc_mort_H1)[2]) {
    # print(totage1[i,j]*ifr[j,2]+totbase1[i,j]*mort[j])
    tc<-rbind(tc,c(i, j, totage1[i,j]*ifr[j,2]+totbase1[i,j]*mort[j]))
  }
}
tc<-as.data.frame(tc)
colnames(tc)<-c("Day","Age","value")
tc$Age<-as.factor(tc$Age)
p6<-ggplot(data=tc, aes(x=Day,y=value,fill=Age))+
  geom_bar(stat = "identity",position="fill", width=1)+
  ylab("Proportion of deaths")
p6

ee<-matrix(0,nrow = A, ncol = dim(cinc_mort_H1)[1])
for(i in 1:A){
  ww<-which(tc$Age==i)
  ee[i,]<-cumsum(tc$value[ww])
}
sum(ee[,516])
simul_interventions$total_reported_deaths_end


####### PLOT Abs 
samp.sizes<-round(rnorm(length(times),parameters["sample_size"],parameters["sample_size"]/5))
ab0<-as.data.frame(matrix(0,nrow=length(times)*1000,ncol=2))
colnames(ab0)<-c("Time","Ab")
ab0$Time<-rep(times,1000)
aux<-c()
for (i in 1:1000) {
  num.inf.samp <- rbinom(length(times), size = samp.sizes, prob = (simul_baseline$ab_all_ages/simul_baseline$N))
  aux<-c(aux,num.inf.samp/samp.sizes)
}
ab0$Ab<-aux
ggplot(ab0,aes(x=Time,y=Ab,group=Time))+geom_boxplot()

samp.sizes<-round(rnorm(length(times),parameters["sample_size"],parameters["sample_size"]/5))
ab<-as.data.frame(matrix(0,nrow=length(times)*1000,ncol=2))
colnames(ab)<-c("Time","Ab")
ab$Time<-rep(times,1000)
aux<-c()
for (i in 1:1000) {
  num.inf.samp <- rbinom(length(times), size = samp.sizes, prob = (simul_interventions$ab_all_ages/simul_interventions$N))
  aux<-c(aux,num.inf.samp/samp.sizes)
}
se<-0.5
sp<-0.9
ab$Ab<-se*aux+(1-sp)*(1-aux)
ggplot(ab,aes(x=Time,y=Ab,group=Time))+geom_boxplot()


##########################################################################################################################
######   SUMMARY METRICS  ################################################################################################
########################################################################################################################

infected0<-tail((rowSums(out0$mean[,(Rindex+1)])),1)/sum(popstruc[,2])
infected0
infected1<-tail((rowSums(out$mean[,(Rindex+1)])),1)/sum(popstruc[,2])
infected1





