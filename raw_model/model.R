# v9
###########################################################################
## AGE-DEPENDANT SEIRS MODEL WITH 5-YEAR AGE CLASSES USING UN DEMOG DATA ##
###########################################################################


require("deSolve")
setwd("C:/Users/ricardo/Dropbox/covid19/covid_age")

#########  INCIDENCE DATA
incdata_X<-read.csv("UKcovidcases1.csv")

#########  DEMOGRAPHIC DATA
# population structure in 2020
popstruc <- read.csv("pop_structure_X.csv",header=TRUE)
# convert from 1000s to total numbers
popstruc[,2]<-popstruc[,2]*1000

A<-length(popstruc[,2])

# births by age of mother
popbirth <- read.csv("pop_birth_X.csv",header=TRUE)
# convert from 1000s per 5 year period to per person per day
popbirth[,2]<-1000*popbirth[,2]/(5*popstruc[,2]*365.25)

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


#########    SEVERITY AND MORTALITY
# age dependent hosp and mort
csr <- read.csv("covidagesevere_X.csv",header=TRUE) # of confirmed cases requiring ICU
cfr <- read.csv("covidagemort_X.csv",header=TRUE) # of confirmed cases who die
# Olivier: csr-cfr >= 0 for all age classes - if not model won't work
# do somthing here cfr[i]=min(csr[i],cfr[i])
for (i in 1:A){
  cfr[i,2]=min(csr[i,2],cfr[i,2])
}

#########    POP AGEING
# per year ageing matrix
dd<-seq(1:A)/seq(1:A)
ageing <- t(diff(diag(dd),lag=1)/(5*365.25))
ageing<-cbind(ageing,0*seq(1:A)) # no ageing from last compartment


#########   INITIALISE SIMULATION/INTERVENTION START TIMES
startdate<-as.Date("2020-01-31") 
# stopdate<-Sys.Date() # today
stopdate<-as.Date("2020-12-31")
# stopdate<-as.Date("2020-03-18")
day_start <- as.numeric(startdate-startdate)
day_stop <- as.numeric(stopdate-startdate)
times <- seq(day_start, day_stop)

tin<-as.numeric(startdate-as.Date("2020-01-01"))/365.25
initP<-sum(popstruc[,2]) # population size 
ageindcase<-20 # age of index case (years)
aci <- floor((ageindcase/5)+1) # age class of index case

# date to start the self isolation intervention
date_selfis_on<-as.Date("2020-03-10")
# date to start the social distancing intervention
date_dist_on<-as.Date("2020-03-17")
# date to start the handwashing intervention
date_hand_on<-as.Date("2020-02-01")
# date to start the working from home
date_work_on<-as.Date("2020-03-19")
# date to start the school closure
date_school_on<-as.Date("2020-03-23")
# date to start cocooning the elderly
date_cocoon_on<-as.Date("2020-03-14")
# date to start vaccinating
date_vaccine_on<-as.Date("2020-08-14")
# date to start international travel ban
date_travelban_on<-as.Date("2020-08-25")
# date to start screening
date_screen_on<-as.Date("2020-08-25")
# date to start voluntary quarantine
date_quarantine_on<-as.Date("2020-03-25")

###########################################################################
# include scaleup time to intervention coverage about 2 weeks
############################################################################


#############   DEFINE PARAMETERS
parameters <- c(
  # Transmission instrinsic
  p=0.053,        # probabilty of infection given a contact 
  rho = 12.5,     # relative infectiousness of incubation phase (%) min 0 max 100 step 0.5 
  omega=100,      # average duration of immunity (years) min 0.5 max 100 step 0.5 
  gamma=4.5,      # average incubation period (days) min 1 max 7 step 0.5 
  nui=5.5,        # average duration of symptomatic infection period (days) min 1 max 7 step 0.5
  report=12.5,    # percentage of all infections that are reported (%) min 0 max 100 step 1
  ratem=7,        # time to death for fatal infection (days) min 1 max 20 step 0.5
  nus = 7,        # duration of non-fatal severe infection (days) min 1 max 20 step 0.5
  rhos= 10,       # relative level of conatcts from severely ill patients (%) min 0 max 100 step 1
  amp=50,         # relative amplitude of seasonal forcing (%) min 0 max 100 step 1
  phi=1,          # month of peak in seasonal forcing
  # INTERVENTIONS
  # self isolation
  selfis_on=as.numeric(date_selfis_on-startdate),
  selfis_dur=36,  # duration of self-isolation protocol (weeks) min 1 max 52 step 1
  selfis_cov=50,  # coverage of self isolation (%) min 0 max 100 step 1
  selfis_eff=65,  # adherence to self isolation (%) min 0 max 100 step 1
  # social distancing
  dist_on=as.numeric(date_dist_on-startdate),
  dist_dur=26,    # duration of social distancing protocol (weeks) min 1 max 52 step 1
  dist_cov=50,    # coverage of social distancing (%) min 0 max 100 step 1
  dist_eff=100,   # adherence to social distancing (%) min 0 max 100 step 1
  # hand washing
  hand_on=as.numeric(date_hand_on-startdate),
  hand_dur=30,    # duration of increased hand hygiene protocol (weeks) min 1 max 52 step 1
  hand_eff=5,     # efficacy of hand hygiene  (%) min 0 max 100 step 1
  # working at home
  work_on=as.numeric(date_work_on-startdate),
  work_dur=12,    # duration of working from home protocol (weeks) min 1 max 52 step 1
  work_cov=50,    # coverage of working from home (%) min 0 max 100 step 1
  work_eff=85,    # efficacy of working from home (%) min 0 max 100 step 1
  w2h = 10,       # work contacts that get attibuted to home when working from home (%) min 0 max 100 step 1
  # school closures
  school_on=as.numeric(date_school_on-startdate),
  school_dur=16,  # duration of school closure (weeks) min 1 max 52 step 1
  school_eff=85,  # efficacy of school closure (%) min 0 max 100 step 1
  s2h = 20,       # school contacts that get attibuted to home when school closes (%) min 0 max 100 step 1
  # cocooning the elderly
  cocoon_on = as.numeric(date_cocoon_on-startdate), 
  cocoon_dur=16,  # duration of elderly cocoon protocol (weeks) min 1 max 52 step 1
  cocoon_eff=95,  # efficacy of elderly cocoon (%) min 0 max 100 step 1
  cocoon_cov=90,  # coverage of elderly cocoon (%) min 0 max 100 step 1
  age_cocoon=70,  # minimum age for elderly cocoon min 0 max 100 step 5
  # vaccination
  vaccine_on= as.numeric(date_vaccine_on-startdate),
  vaccine_eff=90,   # vaccine efficacy (%)- min 0 max 100 step 1
  vaccine_cov=0,    # vaccine coverage (%)- min 0 max 100 step 1
  vac_campaign = 4, # Number of weeks it takes to reach maximum coverage - min 1 max 8 step 1
  # imported cases 
  mean_imports = 0,   # user defined - mean number of infectious migrants per day (number) - min 0 max 500 step 1
  travelban_on= as.numeric(date_travelban_on-startdate),
  travelban_dur = 16, # duration of internation travel restrictions (weeks) - min 1 max 52 step 1
  travelban_eff=50,   # travel restriction efficacy (%) - min 0 max 100 step 1
  # screening - increases the rate of isolation of infectious people in the model
  screen_on = as.numeric(date_screen_on-startdate), 
  screen_dur = 32,    # duration of intensified screening (week) - min 1 max 52 step 1
  screen_cov = 90,    # intensified screening coverage (%) - min 0 max 100 step 1
  # quarantine - This is the bi-product of increasing testing of suspected cases with a certain false positivity rate and voluntary home quarantining of people sharing a house with an infectious case
  quarantine_on = as.numeric(date_quarantine_on-startdate),
  quarantine_cov = 70,          # coverage of quarantine (%)- min 0 max 100 step 1
  quarantine_dur = 24,          # duration of quarantine (weeks) - min 1 max 52 step 1
  quarantine_days = 14,         # days in isolation for average person (days)  - min 5 max 21 step 1
  quarantine_eff_home = -100,   # reduction in the number of contacts at home when quarantined (%) - min 0 max 100 step 52
  quarantine_eff_other = 20,    # reduction in the number of other contacts when quarantined (%) - min 0 max 100 step 52
  # mean household size
  household_size = 2            # mean household size (number) - min 1 max 10 step 1 
  
)

# Scale parameters to percentages/ rates
parameters["rho"]<-parameters["rho"]/100
parameters["omega"]<-(1/(parameters["omega"]*365))
parameters["gamma"]<-1/parameters["gamma"]
parameters["nui"]<-1/parameters["nui"]
parameters["report"]<-parameters["report"]/100
parameters["ratem"]<-1/parameters["ratem"]
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
parameters["quarantine_eff_home"]<-parameters["quarantine_eff_home"]/100
parameters["quarantine_eff_other"]<-parameters["quarantine_eff_other"]/100

beds <- 2.54 # beds per 1000 population

###########################################################################
# Define the indices for each variable
Sindex<-1:A
Eindex<-(A+1):(2*A)
Iindex<-(2*A+1):(3*A)
Rindex<-(3*A+1):(4*A)
Xindex<-(4*A+1):(5*A)
Hindex<-(5*A+1):(6*A)
Mindex<-(6*A+1):(7*A)
Cindex<-(7*A+1):(8*A)
CMindex<-(8*A+1):(9*A)
Vindex<-(9*A+1):(10*A)
QSindex<-(10*A+1):(11*A)
QEindex<-(11*A+1):(12*A)
QIindex<-(12*A+1):(13*A)
QRindex<-(13*A+1):(14*A)

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
initM<-0*popstruc[,2]  # died 
initC<-0*popstruc[,2]  # Cumulative cases (true)
initCM<-0*popstruc[,2] # Cumulative deaths (true)

initS<-popstruc[,2]-initE-initI-initR-initX-initV-initH-initM-initQS-initQE-initQI-initQR  # Susceptible (non-immune)


# set up a function to solve the equations
covid<-function(t, Y, parameters) 
{
  with(as.list(c(Y, parameters)),
       {
         S <- Y[Sindex]
         E <- Y[Eindex]
         I <- Y[Iindex]
         R <- Y[Rindex]
         X <- Y[Xindex]
         H <- Y[Hindex]
         M <- Y[Mindex]
         C <- Y[Cindex]
         CM <- Y[CMindex]
         V <- Y[Vindex]
         QS <- Y[QSindex]
         QE <- Y[QEindex]
         QI <- Y[QIindex]
         QR <- Y[QRindex]
         P <- (S+E+I+R+X+V+H+M+QS+QE+QI+QR)
         
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
         
         if (workhome){
           work<-work_cov*work_eff
         }else{work<-1}
         if (isolation){
           if(screen){selfis<-min(selfis_cov/screen_cov,1)}
           else{selfis<-selfis_cov}
         }else{selfis<-0}
         if (schoolclose){
           school<-school_eff
         }else{school<-1}
         if(distancing){
           dist<-dist_cov*dist_eff
         }else{dist<-1}
         if(handwash){
           hand<-hand_eff
         }else{hand<-0}
         if(vaccine){
           vac_rate <- (-log(1-vaccine_cov)/vac_campaign)
           vaccinate <- vac_rate
         }else{vaccinate<-0}
         if(travelban){
           trvban_eff<-travelban_eff
         }else{trvban_eff<-0}
         if(quarantine){
           quarantine_rate<-(I*household_size/P)*quarantine_cov
         }else{quarantine_rate<-0}
         
         # cocooning the elderly
         cocoon_mat<-matrix(1,nrow = length(popstruc$pop),ncol = length(popstruc$pop))
         cocoon_mat[(age_cocoon-1):length(popstruc$pop),(age_cocoon-1):length(popstruc$pop)]<-(1-cocoon_eff)
         
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
         seas <- 1+amp*cos(2*pi*(t+tin-(phi*365.25/12))/365.25)
         importation <- mean_imports*(1-trvban_eff)
         lam <- (1-hand)*p*seas*(contacts%*%((rho*E+(I+importation)+(1-selfis_eff)*X+rhos*(H+M))/P))
         # contacts under home quarantine
         lamq<-(1-hand)*p*seas*(contact_home%*%(((1-selfis_eff)*X)/P))+(1-hand)*p*seas*(1-quarantine_eff_other)*(contact_other%*%((rho*E+(I+importation)+(1-selfis_eff)*X+rhos*(H+M))/P))
         
         # birth/death
         b1<-sum(popbirth[,2]*popstruc[,2])
         birth<-0*popbirth[,2]
         birth[1]<-b1
         
         # ODE system
         dSdt <- -S*lam-S*vaccinate+omega*R+ageing%*%S-mort*S+birth-quarantine_rate*S +(1/quarantine_days)*QS
         dEdt <- S*lam-gamma*E+ageing%*%E-mort*E + (1-vaccine_eff)*lam*V-quarantine_rate*E+(1/quarantine_days)*QE
         dIdt <- gamma*(1-selfis)*(1-csr[,2])*E-nui*I+ageing%*%I-mort*I + (1/quarantine_days)*QI
         dRdt <- nui*I-omega*R+nui*X+ageing%*%R-mort*R+nus*H + (1/quarantine_days)*QR
         dXdt <- gamma*selfis*(1-csr[,2])*E-nui*X+ageing%*%X-mort*X 
         dVdt <- vaccinate*S -(1-vaccine_eff)*lam*V +ageing%*%V - mort*V
         dQSdt <- quarantine_rate*S+ageing%*%QS-mort*QS - (1/quarantine_days)*QS - lamq*QS
         dQEdt <- quarantine_rate*E- gamma*QE +ageing%*%QE-mort*QE - (1/quarantine_days)*QE +lamq*QS 
         dQIdt <- gamma*(1-csr[,2])*QE-nui*QI+ageing%*%QI-mort*QI - (1/quarantine_days)*QI
         dQRdt <- nui*QI+ageing%*%QR-mort*QR - (1/quarantine_days)*QR
         
         dHdt <- ageing%*%H-mort*H +gamma*(csr[,2]-cfr[,2])*E-nus*H + gamma*(csr[,2]-cfr[,2])*QE
         dMdt <- ageing%*%M+gamma*cfr[,2]*E-ratem*M-mort*M +gamma*cfr[,2]*QE
         
         dCdt <- report*gamma*(E+QE)
         dCMdt <- ratem*M+mort*M
         
         # return the rate of change
         list(c(dSdt,dEdt,dIdt,dRdt,dXdt,dHdt,dMdt,dCdt,dCMdt,dVdt,dQSdt,dQEdt,dQIdt,dQRdt))
       }
  ) 
}


#########    RUN MODEL WITH INTERVENTION
Y<-c(initS,initI,initE,initR,initX,initV,initH,initM,initC,initCM, initQS, initQR, initQI, initQE) # initial conditions for the main solution vector
out <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = parameters)

# total population
pop1<-out[,(Sindex+1)]+out[,(Eindex+1)]+out[,(Iindex+1)]+out[,(Rindex+1)]+out[,(Xindex+1)]+out[,(Vindex+1)]+
  out[,(QSindex+1)]+out[,(QEindex+1)]+out[,(QIindex+1)]+out[,(QRindex+1)]
tpop1<-rowSums(pop1)
time<-as.Date(out[,1]+startdate)
# daily incidence
inc1 <- parameters["report"]*parameters["gamma"]*out[,(Eindex+1)]+parameters["report"]*parameters["gamma"]*out[,(QEindex+1)]
dailyinc1<-rowSums(inc1)
previcureq1<-rowSums(out[,(Hindex+1)]) # requirement for ICU
cmortality1<-rowSums(out[,(CMindex+1)]) # cumulative mortality


###########    RUN BASELINE MODEL - start time for interventions is set to day 1e5, i.e. interventions are always off
parameters["selfis_on"]<-10e5
parameters["dist_on"]<-10e5
parameters["hand_on"]<-10e5
parameters["work_on"]<-10e5
parameters["school_on"]<-10e5
parameters["cocoon_on"]<-10e5
parameters["vaccine_on"]<-10e5
parameters["travelban_on"]<-10e5
parameters["screen_on"]<-10e5
parameters["quarantine_on"]<-10e5

Y<-c(initS,initI,initE,initR,initX,initV,initH,initM,initC,initCM, initQS, initQR, initQI, initQE) # initial conditions for the main solution vector
out <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = parameters)
# total population
pop0<-out[,(Sindex+1)]+out[,(Eindex+1)]+out[,(Iindex+1)]+out[,(Rindex+1)]+out[,(Xindex+1)]+out[,(Vindex+1)]+
  out[,(QSindex+1)]+out[,(QEindex+1)]+out[,(QIindex+1)]+out[,(QRindex+1)]
tpop0<-rowSums(pop0)
time<-as.Date(out[,1]+startdate)
# daily incidence
inc0 <- parameters["report"]*parameters["gamma"]*out[,(Eindex+1)]+parameters["report"]*parameters["gamma"]*out[,(QEindex+1)]
dailyinc0<-rowSums(inc0)
previcureq0<-rowSums(out[,(Hindex+1)]) # requirement for ICU
cmortality0<-rowSums(out[,(CMindex+1)]) # cumulative mortality


#############    PLOTTING
time<-as.Date(out[,"time"]+startdate)
plot(time,cmortality0,type='l',lwd=3,col='grey',
     main="Predicted Cumulative Mortality", xlab="Date", ylab="Deaths")
lines(time,cmortality1,type='l',lwd=3)
legend(as.Date(stopdate-0.35*day_stop), 0.3*max(cmortality0), legend=c("Baseline", "Intervention"),
       col=c("grey", "black"), lwd=c(3,3))

par(mfrow=c(1,1))
time<-as.Date(out[,"time"]+startdate)
plot(time,previcureq0,type='l',lwd=3,col='grey',
     main="Predicted ICU requirement", xlab="Date", ylab="Beds")
lines(time,previcureq1,type='l',lwd=3)
lines(time,tpop0*2.54/1000,lwd=3,col='pink')
lines(time,tpop1*2.54/1000,lwd=3,col='red')
legend(as.Date(stopdate-0.35*day_stop), 0.9*max(previcureq0), legend=c("Baseline", "Intervention"),
       col=c("grey", "black"), lwd=c(3,3))

par(mfrow=c(1,1))
time<-as.Date(out[,"time"]+startdate)
plot(time,dailyinc0,type='l',lwd=3,col='grey',
     main="Predicted Confirmed Cases", xlab="Date", ylab="Cases",ylim=c(0,max(dailyinc0)))
lines(time,dailyinc1,type='l',lwd=3)
points(as.Date(incdata_X[,1]),incdata_X[,2],pch=19,col='blue')
legend(as.Date(stopdate-0.35*day_stop), 0.9*max(dailyinc0), legend=c("Baseline", "Intervention"),
       col=c("grey", "black"), lwd=c(3,3))

# output doubling time over time first 7 days
dd<-7
doub0<-log(2)*dd/(log(dailyinc0[2+dd]/dailyinc0[2]))
doub0


# write.csv(cbind(dailyinc0,dailyinc1),file = "baseline_intervention_UK.csv")