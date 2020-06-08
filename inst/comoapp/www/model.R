# START Bridge ----
popstruc <- population_rv$data %>% 
  select(age_category, pop) %>% 
  rename(agefloor = age_category) %>% 
  as.data.frame()

popbirth <- population_rv$data %>% 
  select(age_category, birth) %>% 
  as.data.frame() # unit should be per person per day

mort <- population_rv$data %>% 
  pull(death) # unit should be per person per day

ihr <- mort_sever_rv$data %>% 
  select(age_category, ihr) %>% 
  as.data.frame()

ifr <- mort_sever_rv$data %>% 
  select(age_category, ifr) %>% 
  as.data.frame()

country_name <- input$country_contact
# END Bridge ----

# START Placeholder for covidage_v13.6.R code (DO NOT EDIT) ----
# per year ageing matrix
A<-length(popstruc[,2])
dd<-seq(1:A)/seq(1:A)
ageing <- t(diff(diag(dd),lag=1)/(5*365.25))
ageing<-cbind(ageing,0*seq(1:A)) # no ageing from last compartment
# END Placeholder ----

# START Placeholder for covidage_v13.6.R code (DO NOT EDIT) ----
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
# END Placeholder ----

# START Bridge ----
startdate <- input$date_range[1]
stopdate <- input$date_range[2]
# END Bridge ----

# START Placeholder for covidage_v13.6.R code (DO NOT EDIT) ----
day_start <- as.numeric(startdate-startdate)
day_stop <- as.numeric(stopdate-startdate)
times <- seq(day_start, day_stop)

tin<-as.numeric(startdate-as.Date("2020-01-01"))/365.25
initP<-sum(popstruc[,2])       # population size 
ageindcase<-20                 # age of index case (years)
aci <- floor((ageindcase/5)+1) # age class of index case
# END Placeholder ----

# START Bridge ----
parameters <- c(
  p = input$p,
  rho = input$rho,
  omega = input$omega,
  gamma = input$gamma,
  nui = input$nui,
  report = input$report,
  reportc = input$reportc,
  reporth = input$reporth,
  beds_available = input$beds_available,
  icu_beds_available = input$icu_beds_available,
  ventilators_available = input$ventilators_available,
  give = 95,
  pdeath_h = input$pdeath_h,
  pdeath_hc = input$pdeath_hc,
  pdeath_icu = input$pdeath_icu,
  pdeath_icuc = input$pdeath_icuc,
  pdeath_vent = input$pdeath_vent,
  pdeath_ventc = input$pdeath_ventc,
  ihr_scaling = input$ihr_scaling,
  nus = input$nus,
  nusc = input$nus, # nusc = nus
  nu_icu = input$nu_icu,
  nu_icuc = input$nu_icu, # nu_icuc = nu_icu
  nu_vent = input$nu_vent,
  nu_ventc = input$nu_vent, # nu_ventc = nu_vent
  rhos = input$rhos,
  amp = input$amp,
  phi = which(month.name == input$phi),
  pclin = input$pclin,
  prob_icu = input$prob_icu,
  prob_vent = input$prob_vent,
  
  # INTERVENTIONS
  # self isolation
  selfis_eff = input$selfis_eff,
  # social distancing
  dist_eff = input$dist_eff,
  # hand washing
  hand_eff = input$hand_eff,
  # working at home
  work_eff = input$work_eff,
  w2h = input$w2h,
  # school closures
  school_eff = input$school_eff,
  s2h = input$s2h,
  # cocooning the elderly
  cocoon_eff = input$cocoon_eff,
  age_cocoon = input$age_cocoon,
  # vaccination campaign
  vaccine_eff = input$vaccine_eff,
  vac_campaign = input$vac_campaign,
  # travel ban
  mean_imports = input$mean_imports,
  # screening
  screen_test_sens = input$screen_test_sens,
  screen_overdispersion = input$screen_overdispersion,
  screen_contacts = input$screen_contacts,
  
  # voluntary home quarantine
  quarantine_days = input$quarantine_days,
  quarantine_effort = input$quarantine_effort,
  quarantine_eff_home = input$quarantine_eff_home,
  quarantine_eff_other = input$quarantine_eff_other,
  
  household_size = input$household_size
)

ihr[,2] <- parameters["ihr_scaling"]*ihr[,2]
# END Bridge ----

# START Placeholder for covidage_v13.6.R code (DO NOT EDIT) ----
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
parameters["selfis_eff"]<-parameters["selfis_eff"]/100
parameters["dist_eff"]<-parameters["dist_eff"]/100
parameters["hand_eff"]<-parameters["hand_eff"]/100
parameters["work_eff"]<-parameters["work_eff"]/100
parameters["w2h"]<-parameters["w2h"]/100
parameters["school_eff"]<-parameters["school_eff"]/100
parameters["s2h"]<-parameters["s2h"]/100
parameters["cocoon_eff"]<-parameters["cocoon_eff"]/100
parameters["age_cocoon"]<-floor((parameters["age_cocoon"]/5)+1)
parameters["vaccine_eff"]<-parameters["vaccine_eff"]/100
# parameters["vaccine_cov"]<-parameters["vaccine_cov"]/100
# parameters["vac_campaign"]<-parameters["vac_campaign"]*7
parameters["screen_test_sens"]<-parameters["screen_test_sens"]/100
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
parameters2<-parameters


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
initICUCV<-0*popstruc[,2]  # icu critical
initVent<-0*popstruc[,2]  # icu vent
initVentC<-0*popstruc[,2] # icu vent crit
initCMC<-0*popstruc[,2]   # Cumulative deaths (true)
initS<-popstruc[,2]-initE-initI-initR-initX-initV-initH-initHC-initQS-initQE-initQI-initQR-initCL-initQC-initICU-initICUC-initICUCV-initVent-initVentC  # Susceptible (non-immune)
# END Placeholder ----


# START Bridge ----
inp <- bind_rows(interventions$baseline_mat %>% mutate(`Apply to` = "Baseline (Calibration)"),
                 interventions$future_mat %>% mutate(`Apply to` = "Hypothetical Scenario")) %>%
  rename(Intervention = intervention, `Date Start` = date_start, `Date End` = date_end, `Value` = value)
# END Bridge ----

# START Placeholder for covidage_v13.12.R code (DO NOT EDIT) ----
inputs<-function(inp, run){
  # cap intervention end dates with simulation end date
  inp$`Date End` = pmin(stopdate, inp$`Date End`)
  
  tb<-which(inp$`Apply to`==run)
  
  si<-intersect(which(inp$Intervention=="Self-isolation if Symptomatic"),tb)
  scr<-intersect(which(inp$Intervention=="Screening (when S.I.)"),tb)
  sd<-intersect(which(inp$Intervention=="Social Distancing"),tb)
  hw<-intersect(which(inp$Intervention=="Handwashing"),tb)
  wah<-intersect(which(inp$Intervention=="Working at Home"),tb)
  sc<-intersect(which(inp$Intervention=="School Closures"),tb)
  cte<-intersect(which(inp$Intervention=="Shielding the Elderly"),tb)
  q<-intersect(which(inp$Intervention=="Household Isolation (when S.I.)"),tb)
  tb<-intersect(which(inp$Intervention=="International Travel Ban"),tb)
  vc<-intersect(which(inp$Intervention=="Vaccination"),tb)
  
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
        si_vector<-c(si_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        si_vector<-c(si_vector,rep(inp$`Value`[si[i]],(f[i*2]-f[i*2-1])*20))
        isolation<-c(isolation,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
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
        sd_vector<-c(sd_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        sd_vector<-c(sd_vector,rep(inp$`Value`[sd[i]],(f[i*2]-f[i*2-1])*20))
        distancing<-c(distancing,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
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
        scr_vector<-c(scr_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        scr_vector<-c(scr_vector,rep(inp$`Value`[scr[i]],(f[i*2]-f[i*2-1])*20))
        screen<-c(screen,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
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
        hw_vector<-c(hw_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        hw_vector<-c(hw_vector,rep(inp$`Value`[hw[i]],(f[i*2]-f[i*2-1])*20))
        handwash<-c(handwash,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
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
        wah_vector<-c(wah_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        wah_vector<-c(wah_vector,rep(inp$`Value`[wah[i]],(f[i*2]-f[i*2-1])*20))
        workhome<-c(workhome,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
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
          schoolclose<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          sc_vector<-c(rep(inp$`Value`[sc[i]],(f[i+1])*20))
          schoolclose<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        sc_vector<-c(sc_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        sc_vector<-c(sc_vector,rep(inp$`Value`[sc[i]],(f[i*2]-f[i*2-1])*20))
        schoolclose<-c(schoolclose,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        schoolclose<-c(schoolclose,rep(1,(f[i*2]-f[i*2-1])*20))
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
        cte_vector<-c(cte_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        cte_vector<-c(cte_vector,rep(inp$`Value`[cte[i]],(f[i*2]-f[i*2-1])*20))
        cocoon<-c(cocoon,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
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
        q_vector<-c(q_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        q_vector<-c(q_vector,rep(inp$`Value`[q[i]],(f[i*2]-f[i*2-1])*20))
        quarantine<-c(quarantine,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
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
        tb_vector<-c(tb_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        tb_vector<-c(tb_vector,rep(inp$`Value`[tb[i]],(f[i*2]-f[i*2-1])*20))
        travelban<-c(travelban,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
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
  ## vaccine
  f<-c()
  vc_vector<-c()
  vaccine<-c()
  if (length(vc)>=1){
    for (i in 1:length(vc)){
      
      f<-c(f,as.numeric(inp$`Date Start`[vc[i]]-startdate),as.numeric(inp$`Date End`[vc[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[vc[i]]>startdate){
          vc_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[vc[i]],(f[i+1]-f[i])*20))
          vaccine<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          vc_vector<-c(rep(inp$`Value`[vc[i]],(f[i+1])*20))
          vaccine<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        vc_vector<-c(vc_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        vc_vector<-c(vc_vector,rep(inp$`Value`[vc[i]],(f[i*2]-f[i*2-1])*20))
        vaccine<-c(vaccine,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        vaccine<-c(vaccine,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(vc)&& f[i*2]<tail(times,1)){
        vc_vector<-c(vc_vector,rep(0,(tail(times,1)-f[i*2])*20))
        vaccine<-c(vaccine,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    vc_vector<-rep(0,tail(times,1)*20)
    vaccine<-rep(0,tail(times,1)*20)
  }
  
  return(list(si_vector=si_vector,sd_vector=sd_vector,scr_vector=scr_vector,hw_vector=hw_vector,wah_vector=wah_vector,
              sc_vector=sc_vector,tb_vector=tb_vector,cte_vector=cte_vector,q_vector=q_vector,vc_vector=vc_vector,isolation=isolation,
              screen=screen,cocoon=cocoon,schoolclose=schoolclose,workhome=workhome,handwash=handwash,
              quarantine=quarantine,vaccine=vaccine,travelban=travelban,distancing=distancing))
}
vectors<<-inputs(inp,'Hypothetical Scenario')
vectors0<<-inputs(inp,'Baseline (Calibration)')

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
         P <- (S+E+I+R+X+V+H+HC+QS+QE+QI+QR+CL+QC+ICU+ICUC+ICUCV+Vent+VentC)
         Q <- (sum(QS)+sum(QE)+sum(QI)+sum(QC)+sum(QR))/sum(P)
         
         # health system performance
         critH<-min(1-fH(sum(H)+sum(ICUC)+sum(ICUCV)),1)
         crit<-min(1-fICU(sum(ICU)+sum(Vent)+sum(VentC)),1)
         critV<-min(1-fVent(sum(Vent)),1)
         
         # interventions
         isolation<-input$isolation[t*20+1]
         distancing<-input$distancing[t*20+1]
         handwash<-input$handwash[t*20+1]
         workhome<-input$workhome[t*20+1]
         schoolclose<-input$schoolclose[t*20+1]
         cocoon<-input$cocoon[t*20+1]
         vaccine<-input$vaccine[t*20+1]
         travelban<-input$travelban[t*20+1]
         screen<-input$screen[t*20+1]
         quarantine<-input$quarantine[t*20+1]
         lockdown_low<-input$lockdown_low[t*20+1]
         lockdown_mid<-input$lockdown_mid[t*20+1]
         lockdown_high<-input$lockdown_high[t*20+1]
         
         screen_eff<-0
         selfis<-0
         school<-1
         dist<-1
         hand<-0
         vaccinate<-0
         trvban_eff<-0
         quarantine_rate<-0
         
         selfis_cov<-(input$si_vector[t*20+1])/100
         screen_contacts<-(input$scr_vector[t*20+1])/10
         school_eff<-(input$sc_vector[t*20+1])/100
         dist_cov<-(input$sd_vector[t*20+1])/100
         hand_cov<-(input$hw_vector[t*20+1])/100
         cocoon<-(input$cte_vector[t*20+1])/100
         work_cov<-(input$wah_vector[t*20+1])/100
         travelban_eff<-(input$tb_vector[t*20+1])/100
         vaccine_cov<-(input$vc_vector[t*20+1])/100
         quarantine_cov<-(input$q_vector[t*20+1])/100
         
         if (workhome){
           work<-work_cov*work_eff
         }else{work<-1}
         if (isolation){
           selfis<-selfis_cov
           if(screen){
             screen_eff<-min(sum(report*I+reportc*(CL)+H+ICU+Vent+reporth*(HC+ICUC+ICUCV+VentC))*screen_contacts*(screen_overdispersion*I/P)*screen_test_sens/P,1) 
           }
         }
         if (schoolclose){
           school<-school_eff
         }
         if(distancing){
           dist<-dist_cov*dist_eff
         }
         if(handwash){
           hand<-hand_eff*hand_cov
         }
         if(vaccine){
           vac_rate <- (-log(1-vaccine_cov)/vac_campaign)
           vaccinate <- vac_rate
         }
         if(travelban){
           trvban_eff<-travelban_eff
         }
         if(quarantine){
           rate_q<-min((min(sum((I+CL+H+ICU+Vent+HC+ICUC+ICUCV+VentC))*(household_size-1)/sum(P),1)*quarantine_effort),quarantine_cov/2)
           quarantine_rate<-rate_q/(1+exp(-10*(quarantine_cov/2-Q)))
         }
         
         
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
         contacts <- (1-cocoon)*cts+cocoon*cts*cocoon_mat+cocoon*(1+schoolclose*(1-school_eff)+workhome*(1-work_eff))*contact_home*(1-cocoon_mat)
         seas <- 1+amp*cos(2*3.14*(t-(phi*365.25/12))/365.25)
         importation <- mean_imports*(1-trvban_eff)
         HH<-H+ICU+Vent+ICUC+ICUCV+VentC
         HHC<-HC
         lam <- (1-hand)*p*seas*(contacts%*%((rho*E+(I+CL+importation)+(1-selfis_eff)*(X+HHC)+rhos*(HH))/P))+
           (1-hand)*p*seas*(1-quarantine*quarantine_eff_other)*(contact_other%*%((rho*QE+QI+QC)/P))
         # contacts under home quarantine
         lamq<-(1-hand)*p*seas*((1-quarantine_eff_home)*contact_home%*%(((1-selfis_eff)*(X+HHC+rho*QE+QI+QC))/P))+
           (1-hand)*p*seas*(1-quarantine_eff_other)*(contact_other%*%((rho*E+(I+CL+importation)+(1-selfis_eff)*(X+HHC+rho*QE+QI+QC)+rhos*(HH))/P))
         
         # birth/death
         b1<-sum(popbirth[,2]*popstruc[,2])
         birth<-0*popbirth[,2]
         birth[1]<-b1
         
         # ODE system
         dSdt <- -S*lam-S*vaccinate+omega*R+ageing%*%S-mort*S+birth-quarantine_rate*S +(1/quarantine_days)*QS
         dEdt <- S*lam-gamma*E+ageing%*%E-mort*E + (1-vaccine_eff)*lam*V-quarantine_rate*E+(1/quarantine_days)*QE
         dIdt <- gamma*(1-pclin)*(1-screen_eff)*(1-ihr[,2])*E-nui*I+ageing%*%I-mort*I + (1/quarantine_days)*QI - quarantine_rate*I
         dCLdt<- gamma*pclin*(1-selfis)*(1-ihr[,2])*(1-quarantine_rate)*E-nui*CL+ageing%*%CL-mort*CL + (1/quarantine_days)*QC
         dRdt <- nui*I-omega*R+nui*X+nui*CL+ageing%*%R-mort*R + (1/quarantine_days)*QR + nus*(1-pdeath_h*ifr[,2])*H + (1-pdeath_icu*ifr[,2])*nu_icu*ICU + (1-pdeath_icuc*ifr[,2])*nu_icuc*ICUC + (1-pdeath_ventc*ifr[,2])*nu_ventc*ICUCV + (1-pdeath_hc*ifr[,2])*nusc*HC + (1-pdeath_vent*ifr[,2])*nu_vent*Vent+ (1-pdeath_ventc*ifr[,2])*nu_ventc*VentC
         dXdt <- gamma*selfis*pclin*(1-ihr[,2])*E+gamma*(1-pclin)*screen_eff*(1-ihr[,2])*E-nui*X+ageing%*%X-mort*X 
         dVdt <- vaccinate*S -(1-vaccine_eff)*lam*V +ageing%*%V - mort*V
         
         dQSdt <- quarantine_rate*S+ ageing%*%QS-mort*QS - (1/quarantine_days)*QS - lamq*QS
         dQEdt <- quarantine_rate*E - gamma*QE + ageing%*%QE-mort*QE - (1/quarantine_days)*QE + lamq*QS 
         dQIdt <- quarantine_rate*I + gamma*(1-ihr[,2])*(1-pclin)*QE-nui*QI+ageing%*%QI-mort*QI - (1/quarantine_days)*QI
         dQCdt <- gamma*pclin*(1-selfis)*(1-ihr[,2])*quarantine_rate*E+gamma*(1-ihr[,2])*pclin*QE-nui*QC+ageing%*%QC-mort*QC - (1/quarantine_days)*QC
         dQRdt <- nui*QI+nui*QC+ageing%*%QR-mort*QR - (1/quarantine_days)*QR
         
         dHdt <- gamma*ihr[,2]*(1-prob_icu)*(1-critH)*reporth*E + gamma*ihr[,2]*(1-prob_icu)*(1-critH)*QE - nus*H + ageing%*%H-mort*H  
         dHCdt <- gamma*ihr[,2]*(1-prob_icu)*(1-critH)*(1-reporth)*E+gamma*ihr[,2]*(1-prob_icu)*critH*E + gamma*ihr[,2]*(1-prob_icu)*critH*QE - nusc*HC + ageing%*%HC-mort*HC 
         dICUdt <- gamma*ihr[,2]*prob_icu*(1-crit)*(1-prob_vent)*E + gamma*ihr[,2]*prob_icu*(1-crit)*(1-prob_vent)*QE - nu_icu*ICU +ageing%*%ICU - mort*ICU +(1-crit)*ICUC*1/2
         dICUCdt <- gamma*ihr[,2]*prob_icu*crit*(1-prob_vent)*E + gamma*ihr[,2]*prob_icu*crit*(1-prob_vent)*QE - 
           nu_icuc*ICUC -(1-crit)*ICUC*1/2 +ageing%*%ICUC - mort*ICUC 
         dICUCVdt <- gamma*ihr[,2]*prob_icu*prob_vent*crit*E +gamma*ihr[,2]*prob_icu*prob_vent*crit*QE -nu_ventc*ICUCV +ageing%*%ICUCV - mort*ICUCV - (1-critV)*ICUCV*1/2
         dVentdt <- gamma*ihr[,2]*prob_icu*(1-crit)*(1-critV)*prob_vent*E + gamma*ihr[,2]*prob_icu*(1-crit)*(1-critV)*prob_vent*QE +(1-critV)*VentC*1/2 +(1-critV)*ICUCV*1/2 -nu_vent*Vent +ageing%*%Vent - mort*Vent 
         dVentCdt <- gamma*ihr[,2]*prob_icu*prob_vent*(1-crit)*critV*E +gamma*ihr[,2]*prob_icu*prob_vent*(1-crit)*critV*QE - 
           (1-critV)*VentC*1/2 -nu_ventc*VentC +ageing%*%VentC - mort*VentC 
         
         dCdt <- report*gamma*(1-pclin)*(1-ihr[,2])*(E+QE)+reportc*gamma*pclin*(1-ihr[,2])*(E+QE)+ 
           gamma*ihr[,2]*(1-critH)*(1-prob_icu)*(E+QE)+gamma*ihr[,2]*critH*reporth*(1-prob_icu)*(E+QE)+
           gamma*ihr[,2]*prob_icu*(E+QE)
         dCMCdt <- nusc*pdeath_hc*ifr[,2]*HC+nu_icuc*pdeath_icuc*ifr[,2]*ICUC + nu_ventc*pdeath_ventc*ifr[,2]*VentC + nu_ventc*pdeath_ventc*ifr[,2]*ICUCV+
           mort*H + mort*HC + mort*ICU + mort*ICUC + mort*ICUCV + mort*Vent + mort*VentC 
         dCMCdt <- nusc*pdeath_hc*ifr[,2]*HC+nu_icuc*pdeath_icuc*ifr[,2]*ICUC + nu_ventc*pdeath_ventc*ifr[,2]*VentC + nu_ventc*pdeath_ventc*ifr[,2]*ICUCV
         mort*HC + mort*ICUC + mort*VentC + mort*ICUCV 
         
         # return the rate of change
         list(c(dSdt,dEdt,dIdt,dRdt,dXdt,dHdt,dHCdt,dCdt,dCMdt,dVdt,dQSdt,dQEdt,dQIdt,dQRdt,dCLdt,dQCdt,dICUdt,dICUCdt,dICUCVdt,dVentdt,dVentCdt,dCMCdt))
       }
  ) 
}

###########    RUN BASELINE MODEL - start time for interventions is set to day 1e5, i.e. interventions are always off

Y<-c(initS,initE,initI,initR,initX,initH,initHC,initC,initCM,initV, initQS, initQE, initQI, initQR, initCL, initQC, initICU, initICUC, initICUCV, initVent, initVentC, initCMC) # initial conditions for the main solution vector
# END Placeholder ----

# START Placeholder for covidage_v13.6.R code (DO NOT EDIT) ----
process_ode_outcome <- function(out){
  critH<-c()
  crit<-c()
  critV<-c()
  
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
  for (i in 1:length(times)){
    critH[i]<-min(1-fH((sum(out[i,(Hindex+1)]))+sum(out[i,(ICUCindex+1)])+sum(out[i,(ICUCVindex+1)])),1)
    crit[i]<-min(1-fICU((sum(out[i,(ICUindex+1)]))+(sum(out[i,(Ventindex+1)]))+(sum(out[i,(VentCindex+1)]))))
    critV[i]<-min(1-fVent((sum(out[i,(Ventindex+1)]))),1)
  }
  
  # total population
  pop1<-out[,(Sindex+1)]+out[,(Eindex+1)]+out[,(Iindex+1)]+out[,(CLindex+1)]+out[,(Rindex+1)]+out[,(Xindex+1)]+out[,(Vindex+1)]+
    out[,(QSindex+1)]+out[,(QEindex+1)]+out[,(QIindex+1)]+out[,(QCindex+1)]+out[,(QRindex+1)]+
    out[,(Hindex+1)]+out[,(HCindex+1)]+out[,(ICUindex+1)]+out[,(ICUCindex+1)]+out[,(ICUCVindex+1)]+out[,(Ventindex+1)]+out[,(VentCindex+1)]
  tpop1<-rowSums(pop1)
  time<-as.Date(out[,1]+startdate)
  # daily incidence
  inc1 <- parameters["report"]*parameters["gamma"]*(1-parameters["pclin"])*out[,(Eindex+1)]%*%(1-ihr[,2])+
    parameters["reportc"]*parameters["gamma"]*parameters["pclin"]*out[,(Eindex+1)]%*%(1-ihr[,2])+
    parameters["report"]*parameters["gamma"]*(1-parameters["pclin"])*out[,(QEindex+1)]%*%(1-ihr[,2])+
    parameters["reportc"]*parameters["gamma"]*parameters["pclin"]*out[,(QEindex+1)]%*%(1-ihr[,2])
  
  inc1h<- parameters["gamma"]*out[,(Eindex+1)]%*%ihr[,2]*(1-critH)*(1-parameters["prob_icu"])*parameters["reporth"]+
    parameters["gamma"]*out[,(Eindex+1)]%*%ihr[,2]*(1-critH)*(1-parameters["prob_icu"])*(1-parameters["reporth"])+
    parameters["gamma"]*out[,(QEindex+1)]%*%ihr[,2]*(1-critH)*(1-parameters["prob_icu"])+
    parameters["gamma"]*out[,(Eindex+1)]%*%ihr[,2]*critH*parameters["reporth"]*(1-parameters["prob_icu"])+
    parameters["gamma"]*out[,(QEindex+1)]%*%ihr[,2]*critH*parameters["reporth"]*(1-parameters["prob_icu"])+
    parameters["gamma"]*out[,(Eindex+1)]%*%ihr[,2]*parameters["prob_icu"]+
    parameters["gamma"]*out[,(QEindex+1)]%*%ihr[,2]*parameters["prob_icu"]
  
  dailyinc1<-rowSums(inc1)+rowSums(inc1h)      # daily incidence
  cuminc1<-colSums(inc1)+colSums(inc1h)        # cumulative incidence
  previcureq1<-rowSums(out[,(Hindex+1)])+ rowSums(out[,(ICUCindex+1)])+rowSums(out[,(ICUCVindex+1)]) # surge beds occupancy
  previcureq21<-rowSums(out[,(ICUindex+1)])+rowSums(out[,(VentCindex+1)])   # icu beds occupancy
  previcureq31<-rowSums(out[,(Ventindex+1)])   # ventilator occupancy
  cmortality1<-rowSums(out[,(CMindex+1)])      # cumulative mortality
  overloadH1<-rowSums(out[,(HCindex+1)])       # requirement for beds
  overloadICU1<-rowSums(out[,(ICUCindex+1)])   # requirement for icu beds
  overloadICUV1<-rowSums(out[,(ICUCVindex+1)]) # requirement for ventilators
  overloadVent1<-rowSums(out[,(VentCindex+1)]) # requirement for ventilators
  ccases1<-rowSums(out[,(Cindex+1)])           # cumulative cases
  reqsurge1<-rowSums(out[,(Hindex+1)])+overloadH1
  reqicu1<-rowSums(out[,(ICUindex+1)])+overloadICU1
  reqvent1<-rowSums(out[,(Ventindex+1)])+overloadICUV1+overloadVent1
  
  inc_overloadH1<-((parameters["gamma"]*(1-parameters["prob_icu"])*out[,(Eindex+1)]))
  inc_overloadICU1<-((parameters["gamma"]*parameters["prob_icu"]*(1-parameters["prob_vent"])*out[,(Eindex+1)]))
  for (i in 1:length(times)) {
    inc_overloadH1[i,]<-inc_overloadH1[i,]*critH[i]*ihr[,2]
    inc_overloadICU1[i,]<-inc_overloadICU1[i,]*crit[i]*ihr[,2]
  }
  inc_overloadH1<-cumsum(rowSums(inc_overloadH1))
  inc_overloadICU1<-cumsum(rowSums(inc_overloadICU1))
  
  ##########################    CALCULATE MORTALITY 
  pdeath_hc<-parameters["pdeath_hc"]
  prob_icu<-parameters["prob_icu"]
  prob_vent<-parameters["prob_vent"]
  pdeath_icuc<-parameters["pdeath_icuc"]
  pdeath_ventc<-parameters["pdeath_ventc"]
  
  
  cinc_mort_H1 <- cumsum(rowSums(parameters["nus"]*parameters["pdeath_h"]*(out[,(Hindex+1)]%*%ifr[,2])))
  cinc_mort_HC1 <- cumsum(rowSums(parameters["nusc"]*parameters["pdeath_hc"]*(out[,(HCindex+1)]%*%ifr[,2])))
  cinc_mort_ICU1 <- cumsum(rowSums(parameters["nu_icu"]*parameters["pdeath_icu"]*out[,(ICUindex+1)]%*%ifr[,2]))
  cinc_mort_ICUC1 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["pdeath_icuc"]*out[,(ICUCindex+1)]%*%ifr[,2]))
  cinc_mort_ICUCV1 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*out[,(ICUCVindex+1)]%*%ifr[,2]))
  cinc_mort_Vent1 <- cumsum(rowSums(parameters["nu_vent"]*parameters["pdeath_vent"]*out[,(Ventindex+1)]%*%ifr[,2]))
  cinc_mort_VentC1 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*out[,(VentCindex+1)]%*%ifr[,2]))
  base_mort_H1 <- cumsum(rowSums(out[,(Hindex+1)]%*%mort))
  base_mort_HC1 <- cumsum(rowSums(out[,(HCindex+1)]%*%mort))
  base_mort_ICU1 <- cumsum(rowSums(out[,(ICUindex+1)]%*%mort))
  base_mort_ICUC1 <- cumsum(rowSums(out[,(ICUCindex+1)]%*%mort))
  base_mort_ICUCV1 <- cumsum(rowSums(out[,(ICUCVindex+1)]%*%mort))
  base_mort_Vent1 <- cumsum(rowSums(out[,(Ventindex+1)]%*%mort))
  base_mort_VentC1 <- cumsum(rowSums(out[,(VentCindex+1)]%*%mort))
  base_mort_S1 <- cumsum(rowSums(out[,(Sindex+1)]%*%mort))
  base_mort_E1 <- cumsum(rowSums(out[,(Eindex+1)]%*%mort))
  base_mort_I1 <- cumsum(rowSums(out[,(Iindex+1)]%*%mort))
  base_mort_CL1 <- cumsum(rowSums(out[,(CLindex+1)]%*%mort))
  base_mort_X1 <- cumsum(rowSums(out[,(Xindex+1)]%*%mort))
  base_mort_QS1 <- cumsum(rowSums(out[,(QSindex+1)]%*%mort))
  base_mort_QE1 <- cumsum(rowSums(out[,(QEindex+1)]%*%mort))
  base_mort_QI1 <- cumsum(rowSums(out[,(QIindex+1)]%*%mort))
  base_mort_QC1 <- cumsum(rowSums(out[,(QCindex+1)]%*%mort))
  base_mort_QR1 <- cumsum(rowSums(out[,(QRindex+1)]%*%mort))
  base_mort_R1 <- cumsum(rowSums(out[,(Rindex+1)]%*%mort))
  
  Rt <- NULL
  for (i in (ceiling(1/parameters["nui"])+1):length(times)){
    Rt[i]<-cumsum(sum(parameters["gamma"]*out[i,(Eindex+1)]))/cumsum(sum(parameters["gamma"]*out[(i-1/parameters["nui"]),(Eindex+1)]))
    if(Rt[i] >= 7) {Rt[i] <- NA}
  }
  
  # Export in a cohesive format ----
  results <- list()
  results$time <- startdate + times  # dates
  results$Rt <- Rt
  results$cum_mortality <- round(cmortality1)  # cumulative mortality
  results$pct_total_pop_infected <- round(100 * tail(cumsum(rowSums(parameters["gamma"]*out[,(Eindex+1)])),1)/sum(popstruc[,2]), 1)  # proportion of the  population that has been infected at the end of the simulation
  results$doubling_time <- round(log(2)*7 / (log(dailyinc1[2+7] / dailyinc1[2])), 2)  # (Baseline only) to double the number of infections at inception
  results$required_beds <- round(previcureq1)  # required beds
  results$saturation <- parameters["beds_available"]  # saturation
  results$daily_incidence <- round(dailyinc1)  # daily incidence (Reported)
  results$daily_total_cases <- round(rowSums(parameters["gamma"]*out[,(Eindex+1)]+parameters["gamma"]*out[,(QEindex+1)])) # daily incidence (Reported + Unreported)  # daily incidence (Reported + Unreported)
  results$hospital_surge_beds <- round(previcureq1)
  results$icu_beds <- round(previcureq21)
  results$ventilators <- round(previcureq31)
  results$normal_bed_requirement <- round(reqsurge1) #real required beds. previcureq1 above is the occupancy
  results$icu_bed_requirement <- round(reqicu1)
  results$icu_ventilator_requirement <- round(reqvent1)
  
  results$death_natural_non_exposed <- round(base_mort_S1)
  results$death_natural_exposed <- round(base_mort_E1 + base_mort_I1 + base_mort_CL1 + base_mort_X1 + base_mort_QS1 + 
                                           base_mort_QE1 + base_mort_QI1 + base_mort_QC1 + base_mort_QR1 + base_mort_R1+
                                           base_mort_H1+base_mort_HC1+base_mort_ICU1+base_mort_ICUC1+base_mort_ICUCV1+
                                           base_mort_Vent1+base_mort_VentC1)
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
  results$base_mort_H <- base_mort_H1
  results$base_mort_HC <- base_mort_HC1
  results$base_mort_ICU <- base_mort_ICU1
  results$base_mort_ICUC <- base_mort_ICUC1
  results$base_mort_ICUCV <- base_mort_ICUCV1
  results$base_mort_Vent <- base_mort_Vent1
  results$base_mort_VentC <- base_mort_VentC1
  results$base_mort_S <- base_mort_S1
  results$base_mort_E <- base_mort_E1
  results$base_mort_I <- base_mort_I1
  results$base_mort_CL <- base_mort_CL1
  results$base_mort_X <- base_mort_X1
  results$base_mort_QS <- base_mort_QS1
  results$base_mort_QE <- base_mort_QE1
  results$base_mort_QI <- base_mort_QI1
  results$base_mort_QC <- base_mort_QC1
  results$base_mort_QR <- base_mort_QR1
  results$base_mort_R <- base_mort_R1
  
  ## AGE DEPENDENT MORTALITY
  cinc_mort_H1 <- parameters["nus"]*parameters["pdeath_h"]*(out[,(Hindex+1)])
  cinc_mort_HC1 <- parameters["nusc"]*parameters["pdeath_hc"]*(out[,(HCindex+1)])
  cinc_mort_ICU1 <- parameters["nu_icu"]*parameters["pdeath_icu"]*out[,(ICUindex+1)]
  cinc_mort_ICUC1 <- parameters["nu_icuc"]*parameters["pdeath_icuc"]*out[,(ICUCindex+1)] 
  cinc_mort_ICUCV1 <- parameters["nu_ventc"]*parameters["pdeath_ventc"]*out[,(ICUCVindex+1)]
  cinc_mort_Vent1 <- parameters["nu_vent"]*parameters["pdeath_vent"]*out[,(Ventindex+1)] 
  cinc_mort_VentC1 <- parameters["nu_ventc"]*parameters["pdeath_ventc"]*out[,(VentCindex+1)] 
  totage1<-as.data.frame(cinc_mort_H1+cinc_mort_HC1+cinc_mort_ICU1+cinc_mort_ICUC1+cinc_mort_ICUCV1+cinc_mort_Vent1+cinc_mort_VentC1)
  basemort_H1<-(out[,(Hindex+1)])
  basemort_HC1<-(out[,(HCindex+1)])
  basemort_ICU1<-(out[,(ICUindex+1)])
  basemort_ICUC1<-(out[,(ICUCindex+1)])
  basemort_ICUCV1<-(out[,(ICUCVindex+1)])
  basemort_Vent1<-(out[,(Ventindex+1)])
  basemort_VentC1<-(out[,(VentCindex+1)])
  totbase1<-as.data.frame(basemort_H1+basemort_HC1+basemort_ICU1+basemort_ICUC1+basemort_ICUCV1+basemort_Vent1+basemort_VentC1)
  tc<-c()
  
  for (i in 1:dim(cinc_mort_H1)[1]) {
    for (j in 1:dim(cinc_mort_H1)[2]) {
      tc<-rbind(tc,c(i, j, totage1[i,j]*ifr[j,2]+totbase1[i,j]*mort[j])) 
    }
  }
  tc<-as.data.frame(tc)
  colnames(tc)<-c("Day","Age","value")
  
  results$tc <- tc %>%
    mutate(Date = startdate + Day,
           age_cat = case_when(
             Age >=  1 & Age <= 6   ~ "≤ 30 y.o.",
             Age >  6 & Age <= 8    ~ "30-40 y.o.",
             Age >  8 & Age <= 10    ~ "40-50 y.o.",
             Age >  10 & Age <= 12    ~ "50-60 y.o.",
             Age >  12 & Age <= 14    ~ "60-70 y.o.",
             Age >=  15  ~ "≥ 70 y.o.")) %>%
    mutate(age_cat = factor(age_cat, levels = rev(c("≤ 30 y.o.", "30-40 y.o.",
                                                    "40-50 y.o.", "50-60 y.o.", "60-70 y.o.", "≥ 70 y.o."))))
  
  mortality_lag <- data.frame(Age = popstruc$agefloor)
  if(nrow(out) >= 30)  mortality_lag <- bind_cols(mortality_lag, 
                                                  data.frame(day30 = out[30,CMindex+1]/out[30,Cindex+1]) %>%
                                                    mutate(day30 = ifelse(is.infinite(day30), 0, day30)) %>%
                                                    rename(`Day 30` = day30))
  if(nrow(out) >= 60)  mortality_lag <- bind_cols(mortality_lag, 
                                                  data.frame(day60 = out[60,CMindex+1]/out[60,Cindex+1]) %>%
                                                    mutate(day60 = ifelse(is.infinite(day60), 0, day60)) %>%
                                                    rename(`Day 60` = day60))
  if(nrow(out) >= 90)  mortality_lag <- bind_cols(mortality_lag, 
                                                  data.frame(day90 = out[90,CMindex+1]/out[90,Cindex+1]) %>%
                                                    mutate(day90 = ifelse(is.infinite(day90), 0, day90)) %>%
                                                    rename(`Day 90` = day90))
  if(nrow(out) >= 120)  mortality_lag <- bind_cols(mortality_lag, 
                                                   data.frame(day120 = out[120,CMindex+1]/out[120,Cindex+1]) %>%
                                                     mutate(day120 = ifelse(is.infinite(day120), 0, day120)) %>%
                                                     rename(`Day 120` = day120))
  
  results$mortality_lag <- mortality_lag
  
  
  shiny_results <<- results
  return(results)
}
# END Placeholder ----