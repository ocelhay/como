# getting elements from dashboard ----
# run just before the model; L40 of fun_multi_runs.R
popbirth_col2 <- popbirth[, 2]
popstruc_col2 <-popstruc[, 2]
ifr_col2 <- ifr[, 2]
ihr_col2 <- ihr[, 2]

save(Y, 
     times, 
     parameters_dup, 
     input, 
     A, 
     contact_home, 
     contact_school, 
     contact_work, 
     contact_other, 
     popbirth_col2,
     popstruc_col2,
     ageing,
     ifr_col2,
     ihr_col2,
     mort,
     age_group_vectors,
     file = "/Users/olivier/Desktop/debug_arguments_ode.Rdata")

# run just after the model; L59 of fun_multi_runs.R
save(mat_ode, 
     file = "/Users/olivier/Desktop/debug_outputs_ode.Rdata")

# getting elements from covidage script ----

## code replacements in covidage script ----

# instead of:
# load("data_CoMo.RData")
# do:
load(file = "./inst/comoapp/www/data/cases.Rda")
load(file = "./inst/comoapp/www/data/contacts.Rda")
load(file = "./inst/comoapp/www/data/demog.Rda")
load(file = "./inst/comoapp/www/data/mort_sever_default.Rda")

# instead of:
# file_path <- paste0(getwd(),"/Template_CoMoCOVID-19App_v19_12 Sept-TL.xlsx")
# do:
file_path <- "/Users/olivier/Documents/Projets/CoMo/como/misc/debug_2021-09-12/Template_CoMoCOVID-19App_v19_12 Sept-TL.xlsx"

# skip: 
# fit_mat <- read.table("fit_mat.txt",header = T)

# instead of:
# names(dta) <- c("date", "cases", "deaths","serology","rep_deaths")
# do:
names(dta) <- c("date", "cases", "deaths","serology")

# ~L411 skip:
# parameters_fit <- rownames(fit_mat)

## run just before model ----

# make sure to run all elements of the script until ~L1838
save(Y, 
     times, 
     parameters, 
     vectors0, 
     A, 
     contact_home, 
     contact_school, 
     contact_work, 
     contact_other, 
     # popbirth_col2,
     # popstruc_col2,
     ageing,
     # ifr_col2,
     # ihr_col2,
     mort,
     age_group_vectors,
     file = "/Users/olivier/Desktop/script_debug_arguments_ode.Rdata")

out0 <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = parameters, input=vectors0)
save(s_mat_ode = out0, 
     file = "/Users/olivier/Desktop/script_debug_outputs_ode.Rdata")


# comparing elements from dashboard and from scripts ----
rm(list = ls())

## comparing outputs ----
load("/Users/olivier/Desktop/script_debug_outputs_ode.Rdata")
s_mat_ode <- out0
load("/Users/olivier/Desktop/debug_outputs_ode.Rdata")

# different outputs: 
all_equal(mat_ode[1:494, ] |> as_tibble(),
          s_mat_ode[1:494, ] |> as_tibble())

sum(mat_ode[, 400])
sum(s_mat_ode[, 400])

sum(mat_ode[, 200])
sum(s_mat_ode[, 200])

# comparing ab_all_ages and N
A <- 21
Sindex<-1:A
SRindex<-(A+1):(2*A) # v19
Eindex<-(2*A+1):(3*A)
Iindex<-(3*A+1):(4*A)
Rindex<-(4*A+1):(5*A)
Xindex<-(5*A+1):(6*A)
Hindex<-(6*A+1):(7*A)
HCindex<-(7*A+1):(8*A)
Cindex<-(8*A+1):(9*A)
CMindex<-(9*A+1):(10*A)
Vindex<-(10*A+1):(11*A)
QSindex<-(11*A+1):(12*A)
QSRindex<-(12*A+1):(13*A) # v19
QEindex<-(13*A+1):(14*A)
QIindex<-(14*A+1):(15*A)
QRindex<-(15*A+1):(16*A)
CLindex<-(16*A+1):(17*A)
QCindex<-(17*A+1):(18*A)
ICUindex<-(18*A+1):(19*A)
ICUCindex<-(19*A+1):(20*A)
ICUCVindex<-(20*A+1):(21*A)
Ventindex<-(21*A+1):(22*A)
VentCindex<-(22*A+1):(23*A)
CMCindex<-(23*A+1):(24*A)
Zindex<-(24*A+1):(25*A)
EVindex<-(25*A+1):(26*A)
ERindex<-(26*A+1):(27*A)
EVRindex<-(27*A+1):(28*A)
VRindex<-(28*A+1):(29*A)
QVindex<-(29*A+1):(30*A)
QEVindex<-(30*A+1):(31*A)
QEVRindex<-(31*A+1):(32*A)
QERindex<-(32*A+1):(33*A)
QVRindex<-(33*A+1):(34*A)
HCICUindex<-(34*A+1):(35*A)
HCVindex<-(35*A+1):(36*A)
Abindex<-(36*A+1):(37*A)

pop1<-s_mat_ode[,(Sindex+1)]+s_mat_ode[,(Eindex+1)]+s_mat_ode[,(Iindex+1)]+s_mat_ode[,(CLindex+1)]+s_mat_ode[,(Rindex+1)]+
        s_mat_ode[,(Xindex+1)]+s_mat_ode[,(Vindex+1)]+s_mat_ode[,(Zindex+1)]+s_mat_ode[,(EVindex+1)]+s_mat_ode[,(ERindex+1)]+s_mat_ode[,(EVRindex+1)]+
        s_mat_ode[,(QSindex+1)]+s_mat_ode[,(QEindex+1)]+s_mat_ode[,(QIindex+1)]+s_mat_ode[,(QCindex+1)]+s_mat_ode[,(QRindex+1)]+
        s_mat_ode[,(QVindex+1)]+s_mat_ode[,(QEVindex+1)]+s_mat_ode[,(QERindex+1)]+s_mat_ode[,(QVRindex+1)]+s_mat_ode[,(QEVRindex+1)]+
        s_mat_ode[,(Hindex+1)]+s_mat_ode[,(HCindex+1)]+s_mat_ode[,(ICUindex+1)]+s_mat_ode[,(ICUCindex+1)]+s_mat_ode[,(ICUCVindex+1)]+
        s_mat_ode[,(Ventindex+1)]+s_mat_ode[,(VentCindex+1)]+s_mat_ode[,(HCICUindex+1)]+s_mat_ode[,(HCVindex+1)]
tpop1<-rowSums(pop1)
ab_all_ages<-rowSums(s_mat_ode[,(Abindex+1)])
ab_all_ages/tpop1

# it seems that the problem should also exist on L2806 of the script
# model outputs look comparable even if significantly different

## comparing arguments ode ----
rm(list = ls())
load("/Users/olivier/Desktop/script_debug_arguments_ode.Rdata")
s_parameters <- parameters
s_input <- vectors0

load("/Users/olivier/Desktop/debug_arguments_ode.Rdata")
parameters <- parameters_dup
input <- input

### comparing parameters ----
names(parameters)
names(s_parameters)

common_parameters <- intersect(names(parameters), names(s_parameters))

which(! parameters[common_parameters] == s_parameters[common_parameters])

parameters["pclin_v"] - s_parameters["pclin_v"]
parameters["pclin_vr"] - s_parameters["pclin_vr"]

# those where at first different due to rounding, shouldn't have a large impact

## comparing input vectors ----
for (vec in intersect(names(input), names(s_input))) {
        print(vec)
        print(all(input[[vec]] == s_input[[vec]]))
}

all(input$si_vector == s_input$si_vector)

# Issue at hand:
# The issue is that prob is a (up to 5) in certains cases in: 

num.inf.samp <- rbinom(length(results$time), size = samp.sizes, 
                       prob = (results$med$ab_all_ages / results$med$N))

# this is to investigate:
# ab_all_ages<-rowSums(out_mat[,(Abindex+1)])
