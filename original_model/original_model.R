###########################################################################
## AGE-DEPENDANT SEIRS MODEL WITH 5-YEAR AGE CLASSES USING UN DEMOG DATA ##
###########################################################################

require("deSolve")
library("ggplot2")
library("dplyr")
library("reshape2")
require(gridExtra)
library(ggpubr)


setwd("C:/covid19/covid_age")

#########  INCIDENCE DATA
incdata_X<-read.csv("UKcovidcases5.csv")
incdata_X[,1]<-as.Date(incdata_X[,1],"%d/%m/%y")

#########  DEMOGRAPHIC DATA
# population structure in 2020
popstruc <- read.csv("pop_structure_X.csv",header=TRUE)
# convert from 1000s to total numbers
popstruc[,2]<-popstruc[,2]*1000

A<-length(popstruc[,2])

# births by age of mother
popbirth <- read.csv("pop_birth_X.csv",header=TRUE)
# convert from 1000s per 5 year period to per person per day
popbirth[,2]<-1000*popbirth[,2]/(5*(popstruc[,2])*365.25)

#natural mortality per person per year
popmort<-read.csv("pop_mort_X.csv",header=TRUE)
# convert from 1000s per 5 year period to per person per day
popmort[,2]<-1000*popmort[,2]/(5*popstruc[,2]*365.25)
mort<- popmort[,2]

##########   CONTACT DATA
c_home <- as.matrix(read.csv("contact_home_X.csv",header=FALSE))
c_school <- as.matrix(read.csv("contact_school_X.csv",header=FALSE))
c_work <- as.matrix(read.csv("contact_work_X.csv",header=FALSE))
c_other <- as.matrix(read.csv("contact_other_X.csv",header=FALSE))

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

# average contacts per day from POLYMOD matrices 
c<-sum((contact_home+contact_other+contact_school+contact_work)%*%(popstruc[,2]/sum(popstruc[,2])))


#########    POP AGEING
# per year ageing matrix
dd<-seq(1:A)/seq(1:A)
ageing <- t(diff(diag(dd),lag=1)/(5*365.25))
ageing<-cbind(ageing,0*seq(1:A)) # no ageing from last compartment


#########   INITIALISE SIMULATION/INTERVENTION START TIMES
startdate<-as.Date("2020-02-15") 
# stopdate<-Sys.Date() # today
stopdate<-as.Date("2021-03-05")
# stopdate<-as.Date("2020-03-18")
day_start <- as.numeric(startdate-startdate)
day_stop <- as.numeric(stopdate-startdate)
times <- seq(day_start, day_stop)

tin<-as.numeric(startdate-as.Date("2020-01-01"))/365.25
initP<-sum(popstruc[,2])       # population size 
ageindcase<-20                 # age of index case (years)
aci <- floor((ageindcase/5)+1) # age class of index case


######    THESE ARE JUST VARIABLE DEFINITIONS - PLEASE DO NOT CHANGE   #################################
######     GO TO LINE 622 TO Choose Interventions' start dates         #################################

# date to start the self isolation intervention
date_selfis_on<-as.Date("2021-12-15")
# date to start the social distancing intervention
#date_dist_on<-as.Date("2020-03-17")
date_dist_on<-as.Date("2021-12-15")
# date to start the handwashing intervention
# date_hand_on<-as.Date("2020-02-01")
date_hand_on<-as.Date("2021-12-31")
# date to start the working from home
#date_work_on<-as.Date("2020-03-19")
date_work_on<-as.Date("2021-12-15")
# date to start the school closure
#date_school_on<-as.Date("2020-03-23")
date_school_on<-as.Date("2021-12-15")
# date to start cocooning the elderly
date_cocoon_on<-as.Date("2021-12-14")
# date to start vaccinating
date_vaccine_on<-as.Date("2021-12-31")
# date to start international travel ban
date_travelban_on<-as.Date("2021-12-31")
# date to start screening
date_screen_on<-as.Date("2021-12-21")
# date to start voluntary quarantine
#date_quarantine_on<-as.Date("2020-03-15")
date_quarantine_on<-as.Date("2021-12-19")
# date to start lockdown low 
date_lockdown_low_on<-as.Date("2021-12-31")
# date to start lockdown mid 
# date_lockdown_mid_on<-as.Date("2020-03-23")
date_lockdown_mid_on<-as.Date("2021-12-18")
# date to start lockdown high 
date_lockdown_high_on<-as.Date("2021-12-20")

###########################################################################
# include scaleup time to intervention coverage about 2 weeks
############################################################################

runif(length(times), 0.75, 1.25)


#############   DEFINE PARAMETERS
parameters <- c(
  # Transmission instrinsic
  p=0.042,           # probabilty of infection given a contact 
  rho = 25,          # relative infectiousness of incubation phase (%) min 0 max 100 step 0.5 
  omega=200,         # average duration of immunity (years) min 0.5 max 100 step 0.5 
  gamma=3.5,         # average incubation period (days) min 1 max 7 step 0.5 
  nui=4.5,           # average duration of symptomatic infection period (days) min 1 max 7 step 0.5
  report=0,          # percentage of all asymptomatic infections that are reported (%) min 0 max 100 step 1
  reportc=0,         # percentage of all symptomatic infections that are reported (%) min 0 max 100 step 1
  reporth=95,        # percentage of all infections requiring hospitalisation that are actually admitted to hospital (%) min 0 max 100 step 1
  beds_available = sum(popstruc[,2])*2.54/1000,#80000, # maximum number of hospital beds - numeric 
  icu_beds_available = sum(popstruc[,2])*6.6/10000,#8000, # maximum number of hospital beds - numeric 
  ventilators_available = 10000, # maximum number of ventilators - numeric
  give = 65 ,        # system capacity stressor  
  pdeath_h = 30,     # probability of dying when hospitalised 
  pdeath_hc = 35,    # probability of dying when denied hospitalisation 
  pdeath_icu = 50,   # probability of dying when admitted to ICU 
  pdeath_icuc = 75,  # probability of dying when admission to ICU denied 
  pdeath_vent = 75,  # probability of dying when ventilated 
  pdeath_ventc = 85, # probability of dying when ventilator denied 
  ihr_scaling = 1,   # scaling factor for infection hospitalisation rate
  nus = 10,          # duration of non-fatal hospitalised infection (days) min 1 max 20 step 0.5
  nusc = 10,         # duration of non-fatal denied hospitalisation infection (days) min 1 max 20 step 0.5
  nu_icu = 10,       # duration of non-fatal icu infection (days) min 1 max 20 step 0.5
  nu_icuc = 10,      # duration of non-fatal denied icu infection (days) min 1 max 20 step 0.5
  nu_vent = 10,      # duration of non-fatal ventilated infection (days) min 1 max 20 step 0.5
  nu_ventc = 10,     # duration of non-fatal denied ventilation infection (days) min 1 max 20 step 0.5
  rhos= 5,           # relative level of contacts from severely ill patients (%) min 0 max 100 step 1
  amp=80,            # relative amplitude of seasonal forcing (%) min 0 max 100 step 1
  phi=12,            # month of peak in seasonal forcing
  pclin=15,          # probability upon infection of developing clinical symptoms
  prob_icu = 70,     # probability upon hospitalisation of requiring icu admission   
  prob_vent = 80,    # probability upon admission to the UCI of requiring a ventilator
  # INTERVENTIONS
  # self isolation
  selfis_on=as.numeric(date_selfis_on-startdate),
  selfis_dur=16,    # duration of self-isolation protocol (weeks) min 1 max 52 step 1
  selfis_cov=50,    # coverage of self isolation (%) min 0 max 100 step 1
  selfis_eff=50,    # adherence to self isolation (%) min 0 max 100 step 1
  # social distancing
  dist_on=as.numeric(date_dist_on-startdate),
  dist_dur=12,      # duration of social distancing protocol (weeks) min 1 max 52 step 1
  dist_cov=50,      # coverage of social distancing (%) min 0 max 100 step 1
  dist_eff=100,     # adherence to social distancing (%) min 0 max 100 step 1
  # hand washing
  hand_on=as.numeric(date_hand_on-startdate),
  hand_dur=12,      # duration of increased hand hygiene protocol (weeks) min 1 max 52 step 1
  hand_eff=5,       # efficacy of hand hygiene  (%) min 0 max 100 step 1
  # working at home
  work_on=as.numeric(date_work_on-startdate),
  work_dur=12,      # duration of working from home protocol (weeks) min 1 max 52 step 1
  work_cov=50,      # coverage of working from home (%) min 0 max 100 step 1
  work_eff=85,      # efficacy of working from home (%) min 0 max 100 step 1
  w2h = 10,         # work contacts that get attibuted to home when working from home (%) min 0 max 100 step 1
  # school closures
  school_on=as.numeric(date_school_on-startdate),
  school_dur=12,    # duration of school closure (weeks) min 1 max 52 step 1
  school_eff=90,    # efficacy of school closure (%) min 0 max 100 step 1
  s2h = 20,         # school contacts that get attibuted to home when school closes (%) min 0 max 100 step 1
  # cocooning the elderly
  cocoon_on = as.numeric(date_cocoon_on-startdate), 
  cocoon_dur=16,    # duration of elderly cocoon protocol (weeks) min 1 max 52 step 1
  cocoon_eff=35,    # efficacy of elderly cocoon (%) min 0 max 100 step 1
  cocoon_cov=75,    # coverage of elderly cocoon (%) min 0 max 100 step 1
  age_cocoon=70,    # minimum age for elderly cocoon min 0 max 100 step 5
  # vaccination
  vaccine_on= as.numeric(date_vaccine_on-startdate),
  vaccine_eff=90,   # vaccine efficacy (%)- min 0 max 100 step 1
  vaccine_cov=0,    # vaccine coverage (%)- min 0 max 100 step 1
  vac_campaign = 4, # Number of weeks it takes to reach maximum coverage - min 1 max 8 step 1
  # imported cases 
  mean_imports = 0,           # user defined - mean number of infectious migrants per day (number) - min 0 max 500 step 1
  travelban_on= as.numeric(date_travelban_on-startdate),
  travelban_dur = 16,         # duration of internation travel restrictions (weeks) - min 1 max 52 step 1
  travelban_eff=50,           # travel restriction efficacy (%) - min 0 max 100 step 1
  # screening - increases the rate of isolation of infectious people in the model
  screen_on = as.numeric(date_screen_on-startdate), 
  screen_dur = 12,            # duration of intensified screening (week) - min 1 max 52 step 1
  screen_cov = 90,            # sensitivity of screening test min 25 max 100 step 1
  screen_overdispersion = 4,  # overdispersion of cases around index case. If  1 likelihood same as general population min 1 max 5 step 0.2 
  screen_contacts = 4,        # number of contacts screened per index case min 1 max 10 step 1
  # quarantine - This is the bi-product of increasing testing of suspected cases with a certain false positivity rate and voluntary home quarantining of people sharing a house with an infectious case
  quarantine_on = as.numeric(date_quarantine_on-startdate),
  quarantine_cov = 70,        # coverage of quarantine (%)- min 0 max 100 step 1
  quarantine_dur = 24,        # duration of quarantine (weeks) - min 1 max 52 step 1
  quarantine_days = 14,       # days in isolation for average person (days)  - min 5 max 21 step 1
  quarantine_effort = 2,      # days to implement maximum quarantine coverage - min 1 max 5
  quarantine_eff_home = 50,   # increase in the number of contacts at home when quarantined (%) - min 0 max 100 step 52
  quarantine_eff_other = 90,  # reduction in the number of other contacts when quarantined (%) - min 0 max 100 step 52
  # lockdown
  lockdown_low_on=as.numeric(date_lockdown_low_on-startdate),
  lockdown_low_dur = 16,
  lockdown_mid_on=as.numeric(date_lockdown_mid_on-startdate),
  lockdown_mid_dur = 16,
  lockdown_high_on=as.numeric(date_lockdown_high_on-startdate),
  lockdown_high_dur = 16,
  # mean household size
  household_size = 2          # mean household size (number) - min 1 max 10 step 1 
)



# Scale parameters to percentages/ rates
parameters["rho"]<-parameters["rho"]/100
parameters["omega"]<-(1/(parameters["omega"]*365))
parameters["gamma"]<-1/parameters["gamma"]
parameters["nui"]<-1/parameters["nui"]
parameters["report"]<-parameters["report"]/100
parameters["reportc"]<-parameters["reportc"]/100
parameters["reporth"]<-parameters["reporth"]/100
parameters["nus"]<-1/parameters["nus"]
parameters["rhos"]<-parameters["rhos"]/100
parameters["amp"]<-parameters["amp"]/100
parameters["selfis_dur"]<-parameters["selfis_dur"]*7
parameters["selfis_cov"]<-parameters["selfis_cov"]/100
parameters["selfis_eff"]<-parameters["selfis_eff"]/100
parameters["dist_dur"]<-parameters["dist_dur"]*7
parameters["dist_cov"]<-parameters["dist_cov"]/100
parameters["dist_eff"]<-parameters["dist_eff"]/100
parameters["hand_dur"]<-parameters["hand_dur"]*7
parameters["hand_eff"]<-parameters["hand_eff"]/100
parameters["work_dur"]<-parameters["work_dur"]*7
parameters["work_cov"]<-parameters["work_cov"]/100
parameters["work_eff"]<-parameters["work_eff"]/100
parameters["w2h"]<-parameters["w2h"]/100
parameters["school_dur"]<-parameters["school_dur"]*7
parameters["schoolcov"]<-parameters["schoolcov"]/100
parameters["school_eff"]<-parameters["school_eff"]/100
parameters["s2h"]<-parameters["s2h"]/100
parameters["cocoon_dur"]<-parameters["cocoon_dur"]*7
parameters["cocoon_cov"]<-parameters["cocoon_cov"]/100
parameters["cocoon_eff"]<-parameters["cocoon_eff"]/100
parameters["age_cocoon"]<-floor((parameters["age_cocoon"]/5)+1)
parameters["travelban_eff"]<-parameters["travelban_eff"]/100
parameters["vaccine_eff"]<-parameters["vaccine_eff"]/100
parameters["vaccine_cov"]<-parameters["vaccine_cov"]/100
parameters["vac_campaign"]<-parameters["vac_campaign"]*7
parameters["travelban_dur"]<-parameters["travelban_dur"]*7
parameters["screen_dur"]<-parameters["screen_dur"]*7
parameters["screen_cov"]<-parameters["screen_cov"]/100
parameters["quarantine_cov"]<-parameters["quarantine_cov"]/100
parameters["quarantine_dur"]<-parameters["quarantine_dur"]*7
parameters["quarantine_days"]<-parameters["quarantine_days"]
parameters["quarantine_effort"]<-1/parameters["quarantine_effort"]
parameters["quarantine_eff_home"]<-parameters["quarantine_eff_home"]/-100
parameters["quarantine_eff_other"]<-parameters["quarantine_eff_other"]/100
parameters["give"]<-parameters["give"]/100
parameters["pdeath_h"]<-parameters["pdeath_h"]/100
parameters["pdeath_hc"]<-parameters["pdeath_hc"]/100
parameters["pdeath_icu"]<-parameters["pdeath_icu"]/100
parameters["pdeath_icuc"]<-parameters["pdeath_icuc"]/100
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
parameters["lockdown_low_dur"]<-parameters["lockdown_low_dur"]*7
parameters["lockdown_mid_dur"]<-parameters["lockdown_mid_dur"]*7
parameters["lockdown_high_dur"]<-parameters["lockdown_high_dur"]*7

beds <- 2.54 # beds per 1000 population
parameters2<-parameters

#########    SEVERITY AND MORTALITY
# age dependent hosp and mort
ihr <- read.csv("covidagehosp_X.csv",header=TRUE) # hospitalisation rate given infection
ifr <- read.csv("covidagefrpi_X.csv",header=TRUE) # fatality rate given infection
ihr<- parameters["ihr_scaling"]*ihr/100   # csv data is in percentages
ifr_original<-ifr/100   # csv data is in percentages
for (i in 1:A){
  ifr[i,2]=ifr[i,2]/max(ifr[,2])    # transform ifr into a normalised age profile (highest value turns into 1)
}
ifr[1:14,2]<-ifr[1:14,2]/2
ihr$severe[15:21]<-ihr$severe[15:21]*1.75

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
Ventindex<-(18*A+1):(19*A)
VentCindex<-(19*A+1):(20*A)
CMCindex<-(20*A+1):(21*A)

###########################################################################
# MODEL INITIAL CONDITIONS
initI<-0*popstruc[,2]  # Infected and symptomatic
initE<-0*popstruc[,2]  # Incubating
initE[aci]<-1          # place random index case in E compartment
initR<-0*popstruc[,2]  # Immune
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
initVent<-0*popstruc[,2]  # icu vent
initVentC<-0*popstruc[,2] # icu vent crit
initCMC<-0*popstruc[,2]   # Cumulative deaths (true)

initS<-popstruc[,2]-initE-initI-initR-initX-initV-initH-initHC-initQS-initQE-initQI-initQR-initCL-initQC-initICU-initICUC-initVent-initVentC  # Susceptible (non-immune)


# set up a function to solve the equations
covid<-function(t, Y, parameters) 
{
  with(as.list(c(Y, parameters)),
       {
         # write.table(t,"times.txt",row.names = F,col.names = F,append = T)
         S <- Y[Sindex]
         E <- Y[Eindex]
         I <- Y[Iindex]
         R <- Y[Rindex]
         X <- Y[Xindex]
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
         Vent <- Y[Ventindex]
         VentC <- Y[VentCindex]
         CMC <- Y[CMCindex]
         
         P <- (S+E+I+R+X+V+H+HC+QS+QE+QI+QR+CL+QC+ICU+ICUC+Vent+VentC)
         # print(sum(P))
         
         # health system performance
         f <- c(1,(1+give)/2,(1-give)/2,0)
         KH<-beds_available
         KICU<- icu_beds_available
         Kvent<- ventilators_available
         x.H <- c(0,(1+give)*KH/2,(3-give)*KH/2,2*KH)
         x.ICU <- c(0,(1+give)*KICU/2,(3-give)*KICU/2,2*KICU)
         x.Vent <- c(0,(1+give)*Kvent/2,(3-give)*Kvent/2,2*Kvent)
         fH <- splinefun(x.H, f, method = "hyman")
         fICU <- splinefun(x.ICU, f, method = "hyman")
         fVent<- splinefun(x.Vent, f, method = "hyman")
         critH<-min(1-fH(sum(H))+(1-reporth),1)
         crit<-min(1-fICU(sum(ICU)+sum(Vent)),1)
         critV<-min(1-fVent(sum(Vent)),1)
         # print(fH(sum(H)))
         
         # interventions
         isolation<-(t>=selfis_on)*(t<=selfis_on+selfis_dur)
         distancing<-(t>=dist_on)*(t<=(dist_on+dist_dur))
         handwash<-(t>=hand_on)*(t<=(hand_on+hand_dur))
         workhome<-(t>=work_on)*(t<=(work_on+work_dur))
         schoolclose<-(t>=school_on)*(t<=(school_on+school_dur))
         cocoon<-(t>=cocoon_on)*(t<=(cocoon_on+cocoon_dur))*cocoon_cov
         vaccine<-(t>=(vaccine_on))*(t<=vaccine_on+vac_campaign)
         travelban<-(t>=travelban_on)*(t<=(travelban_on+travelban_dur))
         screen<-(t>=screen_on)*(t<=(screen_on+screen_dur))
         quarantine<-(t>=quarantine_on)*(t<=(quarantine_on+quarantine_dur))
         lockdown_low<-(t>=lockdown_low_on)*(t<=(lockdown_low_on+lockdown_low_dur))
         lockdown_mid<-(t>=lockdown_mid_on)*(t<=(lockdown_mid_on+lockdown_mid_dur))
         lockdown_high<-(t>=lockdown_high_on)*(t<=(lockdown_high_on+lockdown_high_dur))

         
         screen_eff<-0
         selfis<-0
         school<-1
         dist<-1
         hand<-0
         vaccinate<-0
         trvban_eff<-0
         quarantine_rate<-0
         

        
         
         if (lockdown_low || lockdown_mid || lockdown_high){
           if(lockdown_low){
             selfis<-0.5
             dist<-0.25
             school<-0
             trvban_eff<-0
             quarantine_rate<-0
             work<-0
             cocoon<-0.95
             hand<-0.05
             vaccinate<-0
           }
           if(lockdown_mid){
             selfis<-0.5
             dist<-0.35
             school<-0.85
             trvban_eff<-0
             quarantine_rate<-0.05
             work<-0.5
             cocoon<-0.95
             hand<-0.05
             vaccinate<-0
           }
           if(lockdown_high){
             selfis<-0.95
             dist<-0.95
             school<-0.85
             trvban_eff<-0.95
             quarantine_rate<-0.9
             work<-0.75
             cocoon<-0.95
             hand<-0.075
             vaccinate<-0
           }
         }
         else{
           if (workhome){
             work<-work_cov*work_eff
           }else{work<-1}
           if (isolation){
             selfis<-selfis_cov
             # if(screen){selfis<-min(selfis_cov/(1-screen_sens),1)}
             if(screen){
               screen_eff<-min((report*I+reportc*(CL)+H+ICU+Vent+reporth*HC+ICUC+VentC)*screen_contacts*(screen_overdispersion*I/P)*screen_cov/P,1) 
             }
           }
           if (schoolclose){
             school<-school_eff
           }
           if(distancing){
             dist<-dist_cov*dist_eff
           }
           if(handwash){
             hand<-hand_eff
           }
           if(vaccine){
             vac_rate <- (-log(1-vaccine_cov)/vac_campaign)
             vaccinate <- vac_rate
           }
           if(travelban){
             trvban_eff<-travelban_eff
           }
           if(quarantine){
             quarantine_rate<-min(((I+CL+H+ICU+Vent+HC+ICUC+VentC)*household_size/P),1)*quarantine_cov*quarantine_effort
           }
         }
         
         
         # print(quarantine_rate)
         # cocooning the elderly
         cocoon_mat<-matrix((1-cocoon_eff),nrow = length(popstruc$pop),ncol = length(popstruc$pop))
         cocoon_mat[1:(age_cocoon-1),1:(age_cocoon-1)]<-1
         
         # contact matrices
         cts<-(contact_home+distancing*(1-dist)*contact_other+(1-distancing)*contact_other
               +(1-schoolclose)*contact_school # school on
               +schoolclose*(1-school)*contact_school # school close
               +schoolclose*contact_home*school*s2h # inflating contacts at home when school closes
               +(1-workhome)*contact_work  # normal work
               +workhome*(1-work)*contact_work # people not working from home when homework is active
               +contact_home*workhome*work*w2h # inflating contacts at home when working from home
         )
         
         # Final transmission related parameters
         contacts <- (1-cocoon)*cts+cocoon*cts*cocoon_mat+cocoon*(1+school*(1-school_eff)+work*(1-work_eff))*contact_home*(1-cocoon_mat)
         seas <- 1+amp*cos(2*3.14*(t-(phi*365.25/12))/365.25)
         importation <- mean_imports*(1-trvban_eff)
         HH<-H+ICU+Vent
         HHC<-HC+ICUC+VentC
         lam <- (1-hand)*p*seas*(contacts%*%((rho*E+(I+CL+importation)+(1-selfis_eff)*(X+HHC)+rhos*(HH))/P))
         # contacts under home quarantine
         lamq<-(1-hand)*p*seas*((1-quarantine_eff_home)*contact_home%*%(((1-selfis_eff)*(X+HHC))/P))+(1-hand)*p*seas*(1-quarantine_eff_other)*(contact_other%*%((rho*E+(I+CL+importation)+(1-selfis_eff)*(X+HHC)+rhos*(HH))/P))
         
         # birth/death
         b1<-sum(popbirth[,2]*popstruc[,2])
         birth<-0*popbirth[,2]
         birth[1]<-b1
         
         # ODE system
         dSdt <- -S*lam-S*vaccinate+omega*R+ageing%*%S-mort*S+birth-quarantine_rate*S +(1/quarantine_days)*QS
         dEdt <- S*lam-gamma*E+ageing%*%E-mort*E + (1-vaccine_eff)*lam*V-quarantine_rate*E+(1/quarantine_days)*QE
         dIdt <- gamma*(1-pclin)*(1-screen_eff)*(1-ihr[,2])*E-nui*I+ageing%*%I-mort*I + (1/quarantine_days)*QI - quarantine_rate*I
         dCLdt<- gamma*pclin*(1-selfis)*(1-ihr[,2])*E-nui*CL+ageing%*%CL-mort*CL + (1/quarantine_days)*QC
         dRdt <- nui*I-omega*R+nui*X+nui*CL+ageing%*%R-mort*R + (1/quarantine_days)*QR + nus*(1-pdeath_h*ifr[,2])*H + (1-pdeath_icu*ifr[,2])*nu_icu*ICU + (1-pdeath_icuc*ifr[,2])*nu_icuc*ICUC + (1-pdeath_hc*ifr[,2])*nusc*HC + (1-pdeath_vent*ifr[,2])*nu_vent*Vent+ (1-pdeath_ventc*ifr[,2])*nu_ventc*VentC
         dXdt <- gamma*selfis*pclin*(1-ihr[,2])*E+gamma*(1-pclin)*screen_eff*(1-ihr[,2])*E-nui*X+ageing%*%X-mort*X 
         dVdt <- vaccinate*S -(1-vaccine_eff)*lam*V +ageing%*%V - mort*V
         
         dQSdt <- quarantine_rate*S+ ageing%*%QS-mort*QS - (1/quarantine_days)*QS - lamq*QS
         dQEdt <- quarantine_rate*E - gamma*QE + ageing%*%QE-mort*QE - (1/quarantine_days)*QE + lamq*QS 
         dQIdt <- quarantine_rate*I + gamma*(1-ihr[,2])*(1-pclin)*QE-nui*QI+ageing%*%QI-mort*QI - (1/quarantine_days)*QI
         dQCdt <- gamma*(1-ihr[,2])*pclin*QE-nui*QC+ageing%*%QC-mort*QC - (1/quarantine_days)*QC
         dQRdt <- nui*QI+nui*QC+ageing%*%QR-mort*QR - (1/quarantine_days)*QR
         
         dHdt <- gamma*ihr[,2]*(1-prob_icu)*(1-critH)*E + gamma*ihr[,2]*(1-prob_icu)*(1-critH)*QE - nus*H + ageing%*%H-mort*H  # all pdeath have to be lower than
         dHCdt <- gamma*ihr[,2]*(1-prob_icu)*critH*E + gamma*ihr[,2]*(1-prob_icu)*critH*QE - nusc*HC + ageing%*%HC-mort*HC 
         dICUdt <- gamma*ihr[,2]*prob_icu*(1-crit)*(1-prob_vent)*E + gamma*ihr[,2]*prob_icu*(1-crit)*(1-prob_vent)*QE - nu_icu*ICU +ageing%*%ICU - mort*ICU 
         dICUCdt <- gamma*ihr[,2]*prob_icu*crit*(1-prob_vent)*E + gamma*ihr[,2]*prob_icu*crit*(1-prob_vent)*QE - nu_icuc*ICUC +ageing%*%ICUC - mort*ICUC 
         dVentdt <- gamma*ihr[,2]*prob_icu*(1-crit)*(1-critV)*prob_vent*E + gamma*ihr[,2]*prob_icu*(1-crit)*(1-critV)*prob_vent*QE - nu_vent*Vent +ageing%*%Vent - mort*Vent 
         dVentCdt <- gamma*ihr[,2]*prob_icu*prob_vent*(1-crit)*critV*E +gamma*ihr[,2]*prob_icu*prob_vent*crit*E+
           gamma*ihr[,2]*prob_icu*prob_vent*(1-crit)*critV*QE+ gamma*ihr[,2]*prob_icu*prob_vent*crit*QE - nu_ventc*VentC +ageing%*%VentC - mort*VentC 
         
         dCdt <- report*gamma*(1-pclin)*(1-ihr[,2])*(E+QE)+reportc*gamma*pclin*(1-ihr[,2])*(E+QE)+ 
           gamma*ihr[,2]*(1-critH)*(1-prob_icu)*(E+QE)+gamma*ihr[,2]*critH*reporth*(1-prob_icu)*(E+QE)+
           gamma*ihr[,2]*prob_icu*(E+QE)
         dCMdt<- nus*pdeath_h*ifr[,2]*H + nusc*pdeath_hc*ifr[,2]*HC + nu_icu*pdeath_icu*ifr[,2]*ICU + nu_icuc*pdeath_icuc*ifr[,2]*ICUC +  nu_vent*pdeath_vent*ifr[,2]*Vent + nu_ventc*pdeath_ventc*ifr[,2]*VentC + 
           mort*H + mort*HC + mort*ICU + mort*ICUC + mort*Vent + mort*VentC + mort*I + mort*X
         dCMCdt <- nusc*pdeath_hc*ifr[,2]*HC+nu_icuc*pdeath_icuc*ifr[,2]*ICUC + nu_ventc*pdeath_ventc*ifr[,2]*VentC + 
           mort*HC + mort*ICUC + mort*VentC

         # return the rate of change
         list(c(dSdt,dEdt,dIdt,dRdt,dXdt,dHdt,dHCdt,dCdt,dCMdt,dVdt,dQSdt,dQEdt,dQIdt,dQRdt,dCLdt,dQCdt,dICUdt,dICUCdt,dVentdt,dVentCdt,dCMCdt))
       }
  ) 
}

###########    RUN BASELINE MODEL - start time for interventions is set to day 1e5, i.e. interventions are always off

Y<-c(initS,initE,initI,initR,initX,initH,initHC,initC,initCM,initV, initQS, initQE, initQI, initQR, initCL, initQC, initICU, initICUC, initVent, initVentC, initCMC) # initial conditions for the main solution vector
out0 <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = parameters)


############     CALCULATE HOSPITAL STRESS
f <- c(1,(1+parameters["give"])/2,(1-parameters["give"])/2,0) 
KH<-parameters["beds_available"]
KICU<- parameters["icu_beds_available"]
Kvent<- parameters["ventilators_available"]
x.H <- c(0,(1+parameters["give"])*KH/2,(3-parameters["give"])*KH/2,2*KH) 
x.ICU <- c(0,(1+parameters["give"])*KICU/2,(3-parameters["give"])*KICU/2,2*KICU) 
x.Vent <- c(0,(1+parameters["give"])*Kvent/2,(3-parameters["give"])*Kvent/2,2*Kvent) 
fH <- splinefun(x.H, f, method = "hyman") 
fICU <- splinefun(x.ICU, f, method = "hyman") 
fVent<- splinefun(x.Vent, f, method = "hyman") 
critH<-c()
crit<-c()
critV<-c()
for (i in 1:length(time)){
  critH[i]<-min(1-fH(sum(out0[i,(Hindex+1)]))+parameters["reporth"],1)
  crit[i]<-min(1-fICU((sum(out0[i,(ICUindex+1)]))+(sum(out0[i,(Ventindex+1)]))+(sum(out0[i,(VentCindex+1)]))))
  critV[i]<-min(1-fVent((sum(out0[i,(Ventindex+1)]))),1)
}

# total population
pop0<-out0[,(Sindex+1)]+out0[,(Eindex+1)]+out0[,(Iindex+1)]+out0[,(CLindex+1)]+out0[,(Rindex+1)]+out0[,(Xindex+1)]+out0[,(Vindex+1)]+
  out0[,(QSindex+1)]+out0[,(QEindex+1)]+out0[,(QIindex+1)]+out0[,(QCindex+1)]+out0[,(QRindex+1)]+
  out0[,(Hindex+1)]+out0[,(HCindex+1)]+out0[,(ICUindex+1)]+out0[,(ICUCindex+1)]+out0[,(Ventindex+1)]+out0[,(VentCindex+1)]
tpop0<-rowSums(pop0)
time<-as.Date(out0[,1]+startdate)

# daily incidence
inc0 <- parameters["report"]*parameters["gamma"]*(1-parameters["pclin"])*out0[,(Eindex+1)]%*%(1-ihr[,2])+
  parameters["reportc"]*parameters["gamma"]*parameters["pclin"]*out0[,(Eindex+1)]%*%(1-ihr[,2])+
  parameters["report"]*parameters["gamma"]*(1-parameters["pclin"])*out0[,(QEindex+1)]%*%(1-ihr[,2])+
  parameters["reportc"]*parameters["gamma"]*parameters["pclin"]*out0[,(QEindex+1)]%*%(1-ihr[,2])

inc0h<- parameters["gamma"]*out0[,(Eindex+1)]%*%ihr[,2]*(1-critH)*(1-parameters["prob_icu"])+
  parameters["gamma"]*out0[,(QEindex+1)]%*%ihr[,2]*(1-critH)*(1-parameters["prob_icu"])+
  parameters["gamma"]*out0[,(Eindex+1)]%*%ihr[,2]*critH*parameters["reporth"]*(1-parameters["prob_icu"])+
  parameters["gamma"]*out0[,(QEindex+1)]%*%ihr[,2]*critH*parameters["reporth"]*(1-parameters["prob_icu"])+
  parameters["gamma"]*out0[,(Eindex+1)]%*%ihr[,2]*parameters["prob_icu"]+
  parameters["gamma"]*out0[,(QEindex+1)]%*%ihr[,2]*parameters["prob_icu"]
  

dailyinc0<-rowSums(inc0)+rowSums(inc0h)
previcureq0<-rowSums(out0[,(Hindex+1)])     # requirement for beds
previcureq20<-rowSums(out0[,(ICUindex+1)])  # requirement for icu
previcureq30<-rowSums(out0[,(Ventindex+1)]) # requirement for vent
cmortality0<-rowSums(out0[,(CMindex+1)])    # cumulative mortality
overloadH0<-rowSums(out0[,(HCindex+1)])     # requirement for beds
overloadICU0<-rowSums(out0[,(ICUCindex+1)]) # requirement for beds
overloadVent0<-rowSums(out0[,(VentCindex+1)]) # requirement for beds


#########    RUN MODEL WITH INTERVENTION
# date to start the self isolation intervention
date_selfis_on<-as.Date("2020-03-15")
# date to start the social distancing intervention
date_dist_on<-as.Date("2020-03-22")
# date to start the handwashing intervention
date_hand_on<-as.Date("2020-03-05")
# date to start the working from home
date_work_on<-as.Date("2020-03-15")
# date to start the school closure
date_school_on<-as.Date("2020-03-20")
# date to start cocooning the elderly
date_cocoon_on<-as.Date("2020-03-22")
# date to start vaccinating
date_vaccine_on<-as.Date("2020-12-31")
# date to start international travel ban
date_travelban_on<-as.Date("2020-12-31")
# date to start screening
date_screen_on<-as.Date("2020-12-21")
# date to start voluntary quarantine
date_quarantine_on<-as.Date("2020-12-19")
# date to start lockdown low
date_lockdown_low_on<-as.Date("2020-12-24")
# date to start lockdown mid
date_lockdown_mid_on<-as.Date("2020-03-24")
# date to start lockdown high
date_lockdown_high_on<-as.Date("2020-12-25")

parameters2["work_on"]<-as.numeric(date_work_on-startdate)
parameters2["travelban_on"]<-as.numeric(date_travelban_on-startdate)
parameters2["school_on"]<-as.numeric(date_school_on-startdate)
parameters2["cocoon_on"]<-as.numeric(date_cocoon_on-startdate)
parameters2["dist_on"]<-as.numeric(date_dist_on-startdate)
parameters2["selfis_on"]<-as.numeric(date_selfis_on-startdate)
parameters2["travelban_on"]<-as.numeric(date_travelban_on-startdate)
parameters2["screen_on"]<-as.numeric(date_screen_on-startdate)
parameters2["quarantine_on"]<-as.numeric(date_quarantine_on-startdate)
parameters2["lockdown_low_on"]<-as.numeric(date_lockdown_low_on-startdate)
parameters2["lockdown_mid_on"]<-as.numeric(date_lockdown_mid_on-startdate)
parameters2["lockdown_high_on"]<-as.numeric(date_lockdown_high_on-startdate)

Y<-c(initS,initE,initI,initR,initX,initH,initHC,initC,initCM,initV, initQS, initQE, initQI, initQR, initCL, initQC, initICU, initICUC, initVent, initVentC, initCMC) # initial conditions for the main solution vector
out <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = parameters2)

############     CALCULATE HOSPITAL STRESS
f <- c(1,(1+parameters["give"])/2,(1-parameters["give"])/2,0) 
KH<-parameters["beds_available"]
KICU<- parameters["icu_beds_available"]
Kvent<- parameters["ventilators_available"]
x.H <- c(0,(1+parameters["give"])*KH/2,(3-parameters["give"])*KH/2,2*KH) 
x.ICU <- c(0,(1+parameters["give"])*KICU/2,(3-parameters["give"])*KICU/2,2*KICU) 
x.Vent <- c(0,(1+parameters["give"])*Kvent/2,(3-parameters["give"])*Kvent/2,2*Kvent) 
fH <- splinefun(x.H, f, method = "hyman") 
fICU <- splinefun(x.ICU, f, method = "hyman") 
fVent<- splinefun(x.Vent, f, method = "hyman") 
for (i in 1:length(time)){
  critH[i]<-min(1-fH(sum(out[i,(Hindex+1)]))+parameters["reporth"],1)
  crit[i]<-min(1-fICU((sum(out[i,(ICUindex+1)]))+(sum(out[i,(Ventindex+1)]))+(sum(out[i,(VentCindex+1)]))))
  critV[i]<-min(1-fVent((sum(out[i,(Ventindex+1)]))),1)
}

# total population
pop1<-out[,(Sindex+1)]+out[,(Eindex+1)]+out[,(Iindex+1)]+out[,(CLindex+1)]+out[,(Rindex+1)]+out[,(Xindex+1)]+out[,(Vindex+1)]+
  out[,(QSindex+1)]+out[,(QEindex+1)]+out[,(QIindex+1)]+out[,(QCindex+1)]+out[,(QRindex+1)]+
  out[,(Hindex+1)]+out[,(HCindex+1)]+out[,(ICUindex+1)]+out[,(ICUCindex+1)]+out[,(Ventindex+1)]+out[,(VentCindex+1)] 
tpop1<-rowSums(pop1)
time<-as.Date(out[,1]+startdate)
# daily incidence
inc1 <- parameters["report"]*parameters["gamma"]*(1-parameters["pclin"])*out[,(Eindex+1)]%*%(1-ihr[,2])+
  parameters["reportc"]*parameters["gamma"]*parameters["pclin"]*out[,(Eindex+1)]%*%(1-ihr[,2])+
  parameters["report"]*parameters["gamma"]*(1-parameters["pclin"])*out[,(QEindex+1)]%*%(1-ihr[,2])+
  parameters["reportc"]*parameters["gamma"]*parameters["pclin"]*out[,(QEindex+1)]%*%(1-ihr[,2])

inc1h<- parameters["gamma"]*out[,(Eindex+1)]%*%ihr[,2]*(1-critH)*(1-parameters["prob_icu"])+
  parameters["gamma"]*out[,(QEindex+1)]%*%ihr[,2]*(1-critH)*(1-parameters["prob_icu"])+
  parameters["gamma"]*out[,(Eindex+1)]%*%ihr[,2]*critH*parameters["reporth"]*(1-parameters["prob_icu"])+
  parameters["gamma"]*out[,(QEindex+1)]%*%ihr[,2]*critH*parameters["reporth"]*(1-parameters["prob_icu"])+
  parameters["gamma"]*out[,(Eindex+1)]%*%ihr[,2]*parameters["prob_icu"]+
  parameters["gamma"]*out[,(QEindex+1)]%*%ihr[,2]*parameters["prob_icu"]

dailyinc1<-rowSums(inc1)+rowSums(inc1h)      # daily incidence
cuminc1<-colSums(inc1)+colSums(inc1h)        # cumulative incidence
previcureq1<-rowSums(out[,(Hindex+1)])       # requirement for beds
previcureq21<-rowSums(out[,(ICUindex+1)])    # requirement for icu
previcureq31<-rowSums(out[,(Ventindex+1)])   # requirement for icu
cmortality1<-rowSums(out[,(CMindex+1)])      # cumulative mortality
overloadH1<-rowSums(out[,(HCindex+1)])       # requirement for beds
overloadICU1<-rowSums(out[,(ICUCindex+1)])   # requirement for beds
overloadVent1<-rowSums(out[,(VentCindex+1)]) # requirement for beds
ccases1<-rowSums(out[,(Cindex+1)])           # cumulative cases

inc_overloadH1<-((parameters["gamma"]*(1-parameters["prob_icu"])*out[,(Eindex+1)]))
inc_overloadICU1<-((parameters["gamma"]*parameters["prob_icu"]*(1-parameters["prob_vent"])*out[,(Eindex+1)]))
for (i in 1:length(time)) {
  inc_overloadH1[i,]<-inc_overloadH1[i,]*critH[i]*ihr[,2]
  inc_overloadICU1[i,]<-inc_overloadICU1[i,]*crit[i]*ihr[,2]
}
inc_overloadH1<-cumsum(rowSums(inc_overloadH1))
inc_overloadICU1<-cumsum(rowSums(inc_overloadICU1))


MORTDF<-as.data.frame(cbind(out[30,CMindex+1]/out[30,Cindex+1],out[60,CMindex+1]/out[60,Cindex+1],out[90,CMindex+1]/out[90,Cindex+1],out[120,CMindex+1]/out[120,Cindex+1]))
MORTDF<-cbind(popstruc$agefloor,MORTDF)
colnames(MORTDF)<-c("Age","day30","day60","day90","day120")
MORTDF$day30[is.infinite(MORTDF$day30)]<-0
MORTDF$day60[is.infinite(MORTDF$day60)]<-0
MORTDF$day90[is.infinite(MORTDF$day90)]<-0
MORTDF$day120[is.infinite(MORTDF$day120)]<-0
MORT1<-melt(MORTDF, id.vars="Age",measure.vars = c("day30","day60","day90","day120"))



##########################    CALCULATE MORTALITY 
pdeath_hc<-parameters["pdeath_hc"]
prob_icu<-parameters["prob_icu"]
prob_vent<-parameters["prob_vent"]
pdeath_icuc<-parameters["pdeath_icuc"]
pdeath_ventc<-parameters["pdeath_ventc"]


cinc_mort_H1 <- cumsum(rowSums(parameters["nus"]*parameters["pdeath_h"]*(out[,(Hindex+1)]%*%ifr[,2])+ out[,(Hindex+1)]%*%mort))
cinc_mort_HC1 <- cumsum(rowSums(parameters["nusc"]*parameters["pdeath_hc"]*(out[,(HCindex+1)]%*%ifr[,2]) + out[,(HCindex+1)]%*%mort))
cinc_mort_ICU1 <- cumsum(rowSums(parameters["nu_icu"]*parameters["pdeath_icu"]*out[,(ICUindex+1)]%*%ifr[,2] + out[,(ICUindex+1)]%*%mort))
cinc_mort_ICUC1 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["pdeath_icuc"]*out[,(ICUCindex+1)]%*%ifr[,2] + out[,(ICUCindex+1)]%*%mort))
cinc_mort_Vent1 <- cumsum(rowSums(parameters["nu_vent"]*parameters["pdeath_vent"]*out[,(Ventindex+1)]%*%ifr[,2] + out[,(Ventindex+1)]%*%mort))
cinc_mort_VentC1 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*out[,(VentCindex+1)]%*%ifr[,2] + out[,(VentCindex+1)]%*%mort))

cinc_mort_H0  <- cumsum(rowSums(parameters["nus"]*parameters["pdeath_h"]*(out0[,(Hindex+1)]%*%ifr[,2])+ out0[,(Hindex+1)]%*%mort))
cinc_mort_HC0 <- cumsum(rowSums(parameters["nusc"]*parameters["pdeath_hc"]*(out0[,(HCindex+1)]%*%ifr[,2]) + out0[,(HCindex+1)]%*%mort))
cinc_mort_ICU0 <- cumsum(rowSums(parameters["nu_icu"]*parameters["pdeath_icu"]*out0[,(ICUindex+1)]%*%ifr[,2] + out0[,(ICUindex+1)]%*%mort))
cinc_mort_ICUC0 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["pdeath_icuc"]*out0[,(ICUCindex+1)]%*%ifr[,2] + out0[,(ICUCindex+1)]%*%mort))
cinc_mort_Vent0 <- cumsum(rowSums(parameters["nu_vent"]*parameters["pdeath_vent"]*out0[,(Ventindex+1)]%*%ifr[,2] + out0[,(Ventindex+1)]%*%mort))
cinc_mort_VentC0 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*out0[,(VentCindex+1)]%*%ifr[,2] + out0[,(VentCindex+1)]%*%mort))


MORTDF<-as.data.frame(cbind(out0[30,CMindex+1]/out0[30,Cindex+1],out0[60,CMindex+1]/out0[60,Cindex+1],out0[90,CMindex+1]/out0[90,Cindex+1],out0[120,CMindex+1]/out0[120,Cindex+1]))
MORTDF<-cbind(popstruc$agefloor,MORTDF)
colnames(MORTDF)<-c("Age","day30","day60","day90","day120")
MORTDF$day30[is.infinite(MORTDF$day30)]<-0
MORTDF$day60[is.infinite(MORTDF$day60)]<-0
MORTDF$day90[is.infinite(MORTDF$day90)]<-0
MORTDF$day120[is.infinite(MORTDF$day120)]<-0
MORT0<-melt(MORTDF, id.vars="Age",measure.vars = c("day30","day60","day90","day120"))


##########################   CALCULATE Rt  
Rt0<-c()
Rt1<-c()
for (i in (1/parameters["nui"]+1):length(time)){
  Rt0[i]<-cumsum(sum(parameters["gamma"]*out0[i,(Eindex+1)]))/cumsum(sum(parameters["gamma"]*out0[(i-1/parameters["nui"]),(Eindex+1)]))
  Rt1[i]<-cumsum(sum(parameters["gamma"]*out[i,(Eindex+1)]))/cumsum(sum(parameters["gamma"]*out[(i-1/parameters["nui"]),(Eindex+1)]))
}



#############    PLOTTING

# Fitting tab
# fitting the intervention lines to the data to account for any historical interventions
par(mfrow=c(1,2))
# set up the axis limits
xmin<-min(as.Date(incdata_X[,1]))
xmax<-max(as.Date(incdata_X[,1]))
ymax<-max(incdata_X[,2])
xtick<-seq(xmin, xmax, by=7)
plot(time,dailyinc1,type='l',lwd=3,
     main="New Reported Cases", xlab="Date", ylab="Cases per day",
     xlim=c(xmin,xmax),  ylim=c(0,ymax), col='blue',xaxt="n")
axis(side=1, labels = FALSE)
text(x=xtick,  y=-250, labels = format(xtick,"%b-%d"), srt = 0, xpd = TRUE)
points(as.Date(incdata_X[,1]),incdata_X[,2],pch=19,col='red')

# reset the maximum to the cumulative mortality 
ymax<-max(incdata_X[,3])
plot(time,cmortality1,type='l',lwd=3,
     main="Cumulative Mortality", xlab="Date", ylab="Total deaths",
     xlim=c(xmin,xmax), ylim=c(0,ymax), col='blue',xaxt="n")
text(x=xtick,  y=-100, labels = format(xtick,"%b-%d"), srt = 0, xpd = TRUE)
points(as.Date(incdata_X[,1]),incdata_X[,3],pch=19,col='red')


# # Predictions tab
par(mfrow=c(1,2))
# Cases at baseline and intervention
ymax<-max(c(incdata_X[,2],dailyinc0,dailyinc1))
plot(time,dailyinc0,type='l',lwd=3,col='blue',
     main="Baseline", xlab="Date", ylab="New cases per day",ylim=c(0,ymax))
points(as.Date(incdata_X[,1]),incdata_X[,2],pch=19,col='red')
plot(time,dailyinc1,type='l',lwd=3,col='blue',
     main="Intervention", xlab="Date", ylab="New cases per day",ylim=c(0,ymax))
points(as.Date(incdata_X[,1]),incdata_X[,2],pch=19,col='red')



# Hospital prevalences stratified by H,ICU and Vent
ymax<-max(c((previcureq0+previcureq20+previcureq30),(previcureq1+previcureq21+previcureq31)))
time<-as.Date(out[,"time"]+startdate)
coul=c("#047883", "#24A9E2","#051A46")
DM<-as.data.frame(cbind(time,previcureq0,previcureq20,previcureq30))
colnames(DM)<-c("Time","Hospital surge beds","ICU beds","Ventilators")
DM$Time<-as.Date(DM$Time,origin = "1970-01-01")
DMF<-melt(DM, id.vars="Time",measure.vars = c("Hospital surge beds","ICU beds","Ventilators"))
d0<-ggplot(DMF, aes(x = Time, y = value,fill=variable)) +
  geom_area()+
  scale_fill_manual(values=coul)


DM<-as.data.frame(cbind(time,previcureq1,previcureq21,previcureq31))
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


# Cumulative mortality at baseline and intervention stratified by hospital status
ymax<-max(c((cinc_mort_H0+cinc_mort_ICU0+cinc_mort_Vent0+cinc_mort_HC0+cinc_mort_ICUC0+cinc_mort_VentC0),(cinc_mort_H1+cinc_mort_ICU1+cinc_mort_Vent1+cinc_mort_HC1+cinc_mort_ICUC1+cinc_mort_VentC1)))
time<-as.Date(out[,"time"]+startdate)
coul=c("#047883", "#24A9E2","#051A46","#E68029", "#D63304","#D1D604")
DM<-as.data.frame(cbind(time,cinc_mort_H0,cinc_mort_ICU0,cinc_mort_Vent0,cinc_mort_HC0,cinc_mort_ICUC0,cinc_mort_VentC0))
colnames(DM)<-c("Time","Treated: Hospital","Treated: ICU","Treated: Ventilator","Untreated: Hospital","Untreated: ICU","Untreated: Ventilator")
DM$Time<-as.Date(DM$Time, origin = "1970-01-01")
DMF<-melt(DM, id.vars="Time",measure.vars = c("Treated: Hospital","Treated: ICU","Treated: Ventilator","Untreated: Hospital","Untreated: ICU","Untreated: Ventilator"))
m0<-ggplot(DMF, aes(x = Time, y = value,fill=variable)) +
  geom_area()+
  scale_fill_manual(values=coul)

DM<-as.data.frame(cbind(time,cinc_mort_H1,cinc_mort_ICU1,cinc_mort_Vent1,cinc_mort_HC1,cinc_mort_ICUC1,cinc_mort_VentC1))
colnames(DM)<-c("Time","Treated: Hospital","Treated: ICU","Treated: Ventilator","Untreated: Hospital","Untreated: ICU","Untreated: Ventilator")
DM$Time<-as.Date(DM$Time, origin = "1970-01-01")
DMF<-melt(DM, id.vars="Time",measure.vars = c("Treated: Hospital","Treated: ICU","Treated: Ventilator","Untreated: Hospital","Untreated: ICU","Untreated: Ventilator"))
m1<-ggplot(DMF, aes(x = Time, y = value,fill=variable)) +
  geom_area()+
  scale_fill_manual(values=coul)

grid.arrange(m0+ylab("Cumulatice mortality")+
               ggtitle("Baseline")+
               ylim(0, ymax),
             m1+ylab("Cumulatice mortality")+
               ggtitle("Intervention")+
               ylim(0, ymax),
             nrow = 1)



# Estimated basic reproduction number, R_t
par(mfrow=c(1,2))
ymax<-max(c(Rt0[!is.na(Rt0)],Rt1[!is.na(Rt1)]))
plot(time[2:length(time)],Rt0,type='l',lwd=3,col='black',
     main="Baseline", xlab="Date", ylab="Reproduction number",ylim=c(0,ymax))
lines(time[2:length(time)],Rt0/Rt0,lwd=2,col='grey')
plot(time[2:length(time)],Rt1,type='l',lwd=3,col='black',
     main="Intervention", xlab="Date", ylab="Reproduction number",ylim=c(0,ymax))
lines(time[2:length(time)],Rt1/Rt1,lwd=2,col='grey')


# Predicted ifr 
ymax=max(c(MORT0$value,MORT1$value))
gm<-ggplot(data=MORT1,aes(x=Age,y=value,fill=variable))+
  geom_line(data=MORT1,aes(x=Age,y=value,colour=variable),lwd=1.5)+ylim(0,ymax)+ylab("Mortality")
gm0<-ggplot(data=MORT0,aes(x=Age,y=value,fill=variable))+
  geom_line(data=MORT0,aes(x=Age,y=value,colour=variable),lwd=1.5)+ylim(0,ymax)+ylab("Mortality")
# 
grid.arrange(gm+theme_classic(),
             gm0+theme_classic(),
             nrow=1)


# output doubling time over time first 7 days
dd<-7
doub0<-log(2)*dd/(log(dailyinc0[2+dd]/dailyinc0[2]))
doub0

# plot(times, out[])
infected0<-tail((rowSums(out0[,(Rindex+1)])),1)/sum(popstruc[,2])
infected0
infected1<-tail((rowSums(out[,(Rindex+1)])),1)/sum(popstruc[,2])
infected1

# tpop1
# tpop0

# PCR
time_of_measurement<-40:49
# general population
(rowSums(out[time_of_measurement,Iindex+1])+rowSums(out[time_of_measurement,CLindex+1])+rowSums(out[time_of_measurement,QIindex+1])+
    rowSums(out[time_of_measurement,QCindex+1]))/sum(popstruc[,2])
# every infection including hospital infections
(rowSums(out[time_of_measurement,Iindex+1])+rowSums(out[time_of_measurement,CLindex+1])+rowSums(out[time_of_measurement,Hindex+1])+
    rowSums(out[time_of_measurement,ICUindex+1])+rowSums(out[time_of_measurement,Ventindex+1])+rowSums(out[time_of_measurement,HCindex+1])+
    rowSums(out[time_of_measurement,ICUCindex+1])+rowSums(out[time_of_measurement,VentCindex+1])+rowSums(out[time_of_measurement,QIindex+1])+
    rowSums(out[time_of_measurement,QCindex+1]))/sum(popstruc[,2])

# SEROLOGY
tail((rowSums(out[,(Rindex+1)])),1)/sum(popstruc[,2])


# IHR
sum(ihr$severe*popstruc[,2]/sum(popstruc[,2]))

# IFR
m30<-out0[30,CMindex+1]/(out0[30,Cindex+1])
m30[is.infinite(m30)]<-0
m60<-out0[60,CMindex+1]/out0[60,Cindex+1]
m60[is.infinite(m60)]<-0
m90<-out0[90,CMindex+1]/out0[90,Cindex+1]
m90[is.infinite(m90)]<-0
m120<-out0[120,CMindex+1]/out0[120,Cindex+1]
m120[is.infinite(m120)]<-0

ifr30<-sum(m30*popstruc[,2]/sum(popstruc[,2]))
ifr60<-sum(m60*popstruc[,2]/sum(popstruc[,2]))
ifr90<-sum(m90*popstruc[,2]/sum(popstruc[,2]))
ifr120<-sum(m120*popstruc[,2]/sum(popstruc[,2]))
cbind(ifr30,ifr60,ifr90,ifr120)*100


PMORTDF0<-as.data.frame(cbind(out0[30,CMindex+1]/sum(out0[30,CMindex+1]),out0[60,CMindex+1]/sum(out0[60,CMindex+1]),
                    out0[90,CMindex+1]/sum(out0[90,CMindex+1]),out0[120,CMindex+1]/sum(out0[120,CMindex+1])))
PMORTDF<-as.data.frame(cbind(out[30,CMindex+1]/sum(out[30,CMindex+1]),out[60,CMindex+1]/sum(out[60,CMindex+1]),
                    out[90,CMindex+1]/sum(out[90,CMindex+1]),out[120,CMindex+1]/sum(out[120,CMindex+1])))
sum(PMORTDF0$V4[15:21])
sum(PMORTDF$V4[15:21])


