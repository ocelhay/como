# model.R ----

# Contact ----
c_home <- contact_home[[input$country_contact]] %>% as.matrix()
c_school <- contact_school[[input$country_contact]] %>% as.matrix()
c_work <- contact_work[[input$country_contact]] %>% as.matrix()
c_other <- contact_other[[input$country_contact]] %>% as.matrix()

nce <- (A-length(c_home[1, ]))

# Filling in 4 higher age groups 75-80, 80-85, 85-90, 95-100, 100+
contact_home <- matrix(0, nrow=A, ncol=A)
contact_school <- matrix(0, nrow=A, ncol=A)
contact_work <- matrix(0, nrow=A, ncol=A)
contact_other <- matrix(0, nrow=A, ncol=A)

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


startdate <- input$date_range[1]
stopdate <- input$date_range[2]

# What to choose?
tin <- as.numeric(startdate - as.Date("2020-01-01")) / 365.25

# Index case
ageindcase <- 20 # age of index case (years)
aci <- floor((ageindcase / 5) + 2) # age class of index case

times <- seq(0, as.numeric(stopdate - startdate))


# Model Parameters, parameters vector ----
parameters <- c(
  p = input$p,
  rho = input$rho,
  omega = input$omega,
  gamma = input$gamma,
  nui = input$nui,
  report = input$report,
  ratem = input$ratem,
  nus = input$nus,
  rhos = input$rhos,
  amp = input$amp,
  phi = which(month.name == input$phi),
  
  # INTERVENTIONS
  # self isolation
  selfis_on = as.numeric(input$date_selfis_on - startdate),
  selfis_dur = input$selfis_dur,
  selfis_cov = input$selfis_cov,
  selfis_eff = input$selfis_eff,
  
  # social distancing
  dist_on = as.numeric(input$date_dist_on - startdate),
  dist_dur = input$dist_dur,
  dist_cov = input$dist_cov,
  dist_eff = input$dist_eff,
  
  # hand washing
  hand_on = as.numeric(input$date_hand_on - startdate),
  hand_dur = input$hand_dur,
  hand_eff = input$hand_eff,
  
  # working at home
  work_on = as.numeric(input$date_work_on - startdate),
  work_dur = input$work_dur,
  work_eff = input$work_eff,
  work_cov = input$work_cov,
  w2h = input$w2h,
  
  # school closures
  school_on = as.numeric(input$date_school_on - startdate),
  school_dur = input$school_dur,
  school_eff = input$school_eff,
  s2h = input$s2h,
  
  # cocooning the elderly
  cocoon_on = as.numeric(input$date_cocoon_on - startdate),
  cocoon_dur = input$cocoon_dur,
  cocoon_eff = input$cocoon_eff,
  cocoon_cov = input$cocoon_cov,
  age_cocoon = input$age_cocoon,
  
  # travel ban
  travelban_on = as.numeric(input$date_travelban_on - startdate),
  travelban_dur = input$travelban_dur,
  travelban_eff = input$travelban_eff,
  mean_imports = input$mean_imports,
  
  # screening
  screen_on = as.numeric(input$date_screen_on - startdate),
  screen_dur = input$screen_dur,
  screen_cov = input$screen_cov,
  
  # vaccination campaign
  vaccine_on = as.numeric(input$date_vaccine_on - startdate),
  vaccine_eff = input$vaccine_eff,
  vaccine_cov = input$vaccine_cov,
  vac_campaign = input$vac_campaign,
  
  # voluntary home quarantine
  quarantine_on = as.numeric(input$date_quarantine_on - startdate),
  quarantine_cov = input$quarantine_cov,
  quarantine_dur = input$quarantine_dur,
  quarantine_days = input$quarantine_days,
  quarantine_eff_home = input$quarantine_eff_home,
  quarantine_eff_other = input$quarantine_eff_other,
  household_size = input$household_size
)

# Scale parameters tp pct/rates ----
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
parameters["school_eff"]<-parameters["school_eff"]/100
parameters["s2h"]<-parameters["s2h"]/100

parameters["cocoon_dur"]<-parameters["cocoon_dur"]*7
parameters["cocoon_cov"]<-parameters["cocoon_cov"]/100
parameters["cocoon_eff"]<-parameters["cocoon_eff"]/100
parameters["age_cocoon"]<-floor((parameters["age_cocoon"]/5)+1)

parameters["vaccine_eff"]<-parameters["vaccine_eff"]/100
parameters["vaccine_cov"]<-parameters["vaccine_cov"]/100
parameters["vac_campaign"]<-parameters["vac_campaign"]*7

parameters["travelban_eff"]<-parameters["travelban_eff"]/100
parameters["travelban_dur"]<-parameters["travelban_dur"]*7

parameters["screen_dur"]<-parameters["screen_dur"]*7
parameters["screen_cov"]<-parameters["screen_cov"]/100

parameters["quarantine_cov"]<-parameters["quarantine_cov"]/100
parameters["quarantine_dur"]<-parameters["quarantine_dur"]*7
parameters["quarantine_days"]<-parameters["quarantine_days"]*7
parameters["quarantine_eff_home"]<-parameters["quarantine_eff_home"]/-100  # minus sign not a typo!
parameters["quarantine_eff_other"]<-parameters["quarantine_eff_other"]/100

# Define the indices for each variable ----
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

# Model initial conditions ----
initI<-0*population_rv$data$pop# Infected and symptomatic
initE<-0*population_rv$data$pop # Incubating
initE[aci]<-1 # place random index case in E compartment
initR<-0*population_rv$data$pop # Immune
initX<-0*population_rv$data$pop # Isolated 
initV<-0*population_rv$data$pop # Vaccinated 
initQS<-0*population_rv$data$pop # quarantined S 
initQE<-0*population_rv$data$pop # quarantined E  
initQI<-0*population_rv$data$pop # quarantined I  
initQR<-0*population_rv$data$pop # quarantined R  
initH<-0*population_rv$data$pop # hospitalised 
initM<-0*population_rv$data$pop # died 
initC<-0*population_rv$data$pop # Cumulative cases (true)
initCM<-0*population_rv$data$pop # Cumulative deaths (true)

initS<-population_rv$data$pop-initE-initI-initR-initX-initV-initH-initM-initQS-initQE-initQI-initQR  # Susceptible (non-immune)

Y<-c(initS,initI,initE,initR,initX,initV,initH,initM,initC,initCM, initQS, initQR, initQI, initQE) # initial conditions for the main solution vector




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
         isolation<-(t>=selfis_on)*(t<=(selfis_on+selfis_dur))
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
         cocoon_mat <- matrix(1,nrow = A, ncol = A)
         cocoon_mat[(age_cocoon-1):A,(age_cocoon-1):A]<-(1-cocoon_eff)
         
         cts<-(contact_home+distancing*(1-dist)*contact_other+(1-distancing)*contact_other
               + (1-schoolclose)*contact_school # school on
               + schoolclose*(1-school)*contact_school # school close
               + schoolclose*contact_home*school*s2h # inflating contacts at home when school closes
               + (1-workhome)*contact_work  # normal work
               + workhome*(1-work)*contact_work # people not working from home when homework is active
               + contact_home*workhome*work*w2h # inflating contacts at home when working from home
         )
         
         
         # Final transmission related parameters
         contacts <- (1-cocoon)*cts+cocoon*cts*cocoon_mat+cocoon*(1+school*(1-school_eff)+work*(1-work_eff))*contact_home*(1-cocoon_mat)
         seas <- 1+amp*cos(2*pi*(t+tin-(phi*365.25/12))/365.25)
         importation <- mean_imports*(1-trvban_eff)
         lam <- (1-hand)*p*seas*(contacts%*%((rho*E+(I+importation)+(1-selfis_eff)*X+rhos*(H+M))/P))
         # contacts under home quarantine
         lamq<-(1-hand)*p*seas*((1-quarantine_eff_home)*contact_home%*%(((1-selfis_eff)*X)/P))+(1-hand)*p*seas*(1-quarantine_eff_other)*(contact_other%*%((rho*E+(I+importation)+(1-selfis_eff)*X+rhos*(H+M))/P))
         
         
         birth <- c(sum(population_rv$data$birth * population_rv$data$pop), rep(0, A - 1))
         mort <- population_rv$data$death
         

         # ODE system
         dSdt <- -S*lam-S*vaccinate+omega*R+ageing%*%S-mort*S+birth-quarantine_rate*S +(1/quarantine_days)*QS
         dEdt <- S*lam-gamma*E+ageing%*%E-mort*E + (1-vaccine_eff)*lam*V-quarantine_rate*E+(1/quarantine_days)*QE
         dIdt <- gamma*(1-selfis)*(1-mort_sever$data$severity)*E-nui*I+ageing%*%I-mort*I + (1/quarantine_days)*QI
         dRdt <- nui*I-omega*R+nui*X+ageing%*%R-mort*R+nus*H + (1/quarantine_days)*QR
         dXdt <- gamma*selfis*(1-mort_sever$data$severity)*E-nui*X+ageing%*%X-mort*X 
         dVdt <- vaccinate*S -(1-vaccine_eff)*lam*V +ageing%*%V - mort*V
         dQSdt <- quarantine_rate*S+ageing%*%QS-mort*QS - (1/quarantine_days)*QS - lamq*QS
         dQEdt <- quarantine_rate*E- gamma*QE +ageing%*%QE-mort*QE - (1/quarantine_days)*QE +lamq*QS 
         dQIdt <- gamma*(1-mort_sever$data$severity)*QE-nui*QI+ageing%*%QI-mort*QI - (1/quarantine_days)*QI
         dQRdt <- nui*QI+ageing%*%QR-mort*QR - (1/quarantine_days)*QR
         dHdt <- ageing%*%H-mort*H +gamma*(mort_sever$data$severity-mort_sever$data$mortality)*E-nus*H + gamma*(mort_sever$data$severity-mort_sever$data$mortality)*QE
         dMdt <- ageing%*%M+gamma*mort_sever$data$mortality*E-ratem*M-mort*M +gamma*mort_sever$data$mortality*QE
         dCdt <- report*gamma*(E+QE)
         dCMdt <- ratem*M+mort*M
         
         # return the rate of change
         list(c(dSdt,dEdt,dIdt,dRdt,dXdt,dHdt,dMdt,dCdt,dCMdt,dVdt,dQSdt,dQEdt,dQIdt,dQRdt))
       }
  ) 
}



process_ode_outcome <- function(out){
  
  nb_beds <- NULL
  if(input$country_beds != "-- Own Value ---")  nb_beds <- beds$beds[beds$country == input$country_beds]
  if(input$country_beds == "-- Own Value ---")  nb_beds <- input$beds
  
  results <- list()
  results$pop0<-out[,(Sindex+1)]+out[,(Eindex+1)]+out[,(Iindex+1)]+out[,(Rindex+1)]+out[,(Xindex+1)]+out[,(Vindex+1)]+
    out[,(QSindex+1)]+out[,(QEindex+1)]+out[,(QIindex+1)]+out[,(QRindex+1)]
  results$tpop0<-rowSums(results$pop0)
  results$pct_total_pop_infected <- 100*sum(tail(out,1)[Cindex+1])/(tail(results$tpop0,1)*parameters["report"])
  results$saturation <- (nb_beds * results$tpop0)/1000
  
  results$time <- as.Date(out[, 1] + startdate)
  # daily incidence
  results$inc0 <- parameters["report"]*parameters["gamma"]*out[,(Eindex+1)]+parameters["report"]*parameters["gamma"]*out[,(QEindex+1)]
  results$dailyinc0 <- rowSums(results$inc0)  # Reported Cases
  results$daily_total_cases <- (1 / parameters["report"]) * rowSums(results$inc0)  # Reported + Suspected
  results$previcureq0 <- rowSums(out[,(Hindex+1)]) # requirement for ICU
  results$cmortality0 <- rowSums(out[,(CMindex+1)]) # cumulative mortality
  
  return(results)
}