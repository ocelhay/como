#' @ copyright
#' Authors:
#'   Ricardo Aguas
#'   Adam Mahdi
#'   Rima Shretta
#'   Peter Horby
#'   Martin Landray
#'   Lisa J White
#' 
#' This work is protected under the @Attribution-NonCommercial 4.0 International intellectual property license. 
#' You are free to:
#'   Share - copy and redistribute the material in any medium or format
#' Adapt - remix, transform, and build upon the material Under the following terms:
#'   Attribution - You must give appropriate credit to the authors, and indicate if any changes were made. You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
#' NonCommercial - You may not use the material for commercial purposes.
#' ShareAlike - If you remix, transform, or build upon the material, you must distribute your contributions under the same license as the original.


library(readxl)
library(XLConnect)

#read data from excel file
# setwd("C:/dexamethasone")  ## set wd to the location of the .Rdata file
load("data_CoMo.RData")
country_name<-"United Kingdom"

wpop<-read.table("worldpop.txt")
pop<-population$country==country_name   # UK population
pp<-population$pop[pop]

att_v<-seq(0.05,0.15,by=0.005)  # attack rate vector
acc_v<-seq(1,1,by=0.05)         # acess tp treatment vector 

ihr<-unlist(read.table("ihr.txt"))  # infection fatality ratio
hfr<-unlist(read.table("hfr.txt"))  # hospitalisation fatality ratio

ll<-unlist(read.table("lifelost.txt"))  # years life lost when dying at different ages


#### DEXAMETHASONE clinical efficacy
eff_icu<-0.82      
eff_vent<-0.64
eff_ventc<-1
eff_icuc<-1


pi_v<-seq(0.2,0.9,by=0.01)     # probability of requiring O2
pv_v1<-seq(0.05,0.6,by=0.01)   # probability of requiring ventilation if not on Dexamethasone
pv_v2<-seq(0.75,1,by=0.025)    # probability of requiring ventilation if given Dexamethasone

icv_v1<-seq(-2,3,by=1)         # extra days spent in icu if surviving mechanical ventilation
icv_v2<-seq(3,8,by=1)          # extra days spent in surge bed if surviving mechanical ventilation
icicu_v<-seq(2,7,by=1)         # extra days spent in surge bed if surviving O2 treatment

c_v<-seq(1,2,by=0.05)          # increase in likelihood of dying if not receiving the appropriate treatment



#### INITIALISE OUTCOMES
samples<-1e6
EFFECT<-rep(0,samples)
LYG<-rep(0,samples)
LYGLS<-rep(0,samples)
IC<-rep(0,samples)
ICLS<-rep(0,samples)
ICLYG<-rep(0,samples)
diff_o2<-rep(0,samples)
diff_v<-rep(0,samples)
H<-rep(0,samples)
D<-rep(0,samples)


#### PARAMETER SAMPLING
for (i in 1:samples){
  att_rate<-att_v[round(runif(1, 1, length(att_v)))]
  access<-acc_v[round(runif(1, 1, length(acc_v)))]
  prob_icu<-pi_v[round(runif(1, 1, length(pi_v)))]
  prob_vent1<-pv_v1[round(runif(1, 1, length(pv_v1)))]
  prob_vent2<-prob_vent1*pv_v2[round(runif(1, 1, length(pv_v2)))]
  c<-c_v[round(runif(1, 1, length(c_v)))]
  ic_vent1<-icv_v1[round(runif(1, 1, length(icv_v1)))]
  ic_vent2<-icv_v2[round(runif(1, 1, length(icv_v2)))]
  ic_icu<-icicu_v[round(runif(1, 1, length(icicu_v)))]
  
  pdeath_h<-0.3
  pdeath_hc<-pdeath_h*c
  pdeath_icu<-0.4
  pdeath_icuc<-pdeath_icu*c
  pdeath_vent<-0.66
  pdeath_ventc<-pdeath_vent*c
  
  ###    without Dexamethasone
  dh<-ihr*pp*att_rate*(1-prob_icu)*access*hfr*pdeath_h                            # deaths h
  dhc<-ihr*pp*att_rate*(1-prob_icu)*(1-access)*hfr*pdeath_hc                      # deaths hc
  di<-ihr*pp*att_rate*prob_icu*(1-prob_vent1)*access*hfr*pdeath_icu                # deaths icu 
  dic<-ihr*pp*att_rate*prob_icu*(1-prob_vent1)*(1-access)*hfr*pdeath_icuc          # deaths icuc
  dicv<-ihr*pp*att_rate*prob_icu*prob_vent1*(1-access)*hfr*pdeath_ventc            # deaths icucv
  dv<-ihr*pp*att_rate*prob_icu*prob_vent1*access*access*hfr*pdeath_vent            # deaths vent
  dvc<-ihr*pp*att_rate*prob_icu*prob_vent1*access*(1-access)*hfr*pdeath_ventc      # deaths ventc
  total<-(dh+dhc+di+dic+dv+dvc)
  ttv<-sum(dv)
  ttic<-sum(di+dvc+dic)
  
  ### with Dexamethasone
  dhw<-ihr*pp*att_rate*(1-prob_icu)*access*hfr*pdeath_h                                    # deaths h
  dhcw<-ihr*pp*att_rate*(1-prob_icu)*(1-access)*hfr*pdeath_hc                              # deaths hc
  diw<-ihr*pp*att_rate*prob_icu*(1-prob_vent2)*access*hfr*pdeath_icu*eff_icu                # deaths icu 
  dicw<-ihr*pp*att_rate*prob_icu*(1-prob_vent2)*(1-access)*hfr*pdeath_icuc*eff_icuc         # deaths icuc
  dicvw<-ihr*pp*att_rate*prob_icu*prob_vent2*(1-access)*hfr*pdeath_ventc*eff_ventc          # deaths icucv
  dvw<-ihr*pp*att_rate*prob_icu*prob_vent2*access*access*hfr*pdeath_vent*eff_vent           # deaths vent
  dvcw<-ihr*pp*att_rate*prob_icu*prob_vent2*access*(1-access)*hfr*pdeath_ventc*eff_ventc    # deaths ventc
  totalw<-(dhw+dhcw+diw+dicw+dvw+dvcw)
  ttwv<-sum(dvw)
  ttwic<-sum(diw+dvcw+dicw)
  
  ### Hospitalizations
  H[i]<-sum(ihr*pp*att_rate)
  
  ### DEATHS
  D[i]<-sum(total)
  
  ### EFFECT
  EFFECT[i]<-sum(total)-sum(totalw)
  
  ## LIFE YEARS GAINED
  LYG[i]<-sum(total*ll)-sum(totalw*ll)
  
  # LIFE YEARS GAINED PER LIFE SAVED
  LYGLS[i]<-LYG[i]/EFFECT[i]
  
  # Incremental cost
  IC[i]<-(ttv-ttwv)*(ic_vent1*4520+ic_vent2*1356)+(ttic-ttwic)*(ic_icu*1356)
  
  # INCREMENTAL COST PER LIFE SAVED
  ICLS[i]<-IC[i]/EFFECT[i]
  
  # INCREMENTAL COST PER LIFE YEAR GAINED
  ICLYG[i]<-IC[i]/LYG[i]
  
  diff_v[i]<-(ttv-ttwv)
  diff_o2[i]<-(ttic-ttwic)
  # rr[i]<-sum(total)-sum(totalw)/sum(total)
}

### Hospitalizations
quantile(H,0.05)
quantile(H,0.5)
quantile(H,0.95)

### DEATHS
quantile(D,0.05)
quantile(D,0.5)
quantile(D,0.95)

# LIVES SAVED
hist(EFFECT)
quantile(EFFECT,0.05)
quantile(EFFECT,0.5)
quantile(EFFECT,0.95)

# LIFE YEARS GAINED
hist(LYG)
quantile(LYG,0.05)
quantile(LYG,0.5)
quantile(LYG,0.95)

# LIFE YEARS GAINED PER LIFE SAVED
hist(LYGLS)
quantile(LYGLS,0.05)
quantile(LYGLS,0.5)
quantile(LYGLS,0.95)

# INCREMENTAL COST
hist(IC)
quantile(IC,0.05)
quantile(IC,0.95)
quantile(IC,0.5)

# INCREMENTAL COST PER LIFE SAVED 
hist(ICLS)
quantile(ICLS,0.5)
quantile(ICLS,0.05)
quantile(ICLS,0.95)

# INCREMENTAL COST PER LIFE YEAR GAINED 
quantile(ICLYG,0.5)
quantile(ICLYG,0.05)
quantile(ICLYG,0.95)

# # OXIGEN LIVES SAVED
# hist(diff_o2)
# quantile(diff_o2,0.5)
# # VENTILATOR LIVES SAVED
# hist(diff_v)
# quantile(diff_v,0.5)

