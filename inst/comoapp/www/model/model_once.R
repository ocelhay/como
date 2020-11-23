# Load Data.
load(file = "./www/data/cases.Rda")
load(file = "./www/data/contacts.Rda")
load(file = "./www/data/demog.Rda")
load(file = "./www/data/mort_sever_default.Rda")

# Choices for dropdowns.
countries_cases <- sort(unique(cases$country))
entities_tests <- c("_", sort(unique(tests$entity)))
countries_contact <- names(contact_home)
countries_demographic <- sort(unique(population$country))

valid_interventions_v17 <- c("Dexamethasone", "Handwashing", "International Travel Ban",
                             "Mask Wearing", "Mass Testing", "School Closures", "Self-isolation if Symptomatic",
                             "(*Self-isolation) Household Isolation", "(*Self-isolation) Screening", "Shielding the Elderly",
                             "Social Distancing", "Vaccination", "Working at Home")

all_interventions <- c("_", valid_interventions_v17)

vec_age_categories <- c("1 = 0-5 y.o.", "2 = 5-10 y.o.", "3 = 10-15 y.o.", 
                        "4 = 15-20 y.o.", "5 = 20-25 y.o.", "6 = 25-30 y.o.",
                        "7 = 30-35 y.o.", "8 = 35-40 y.o.", "9 = 40-45 y.o.",
                        "10 = 45-50 y.o.", "11 = 50-55 y.o.", "12 = 55-60 y.o.",
                        "13 = 60-65 y.o.", "14 = 65-70 y.o.", "15 = 70-75 y.o.",
                        "16 = 75-80 y.o.", "17 = 80-85 y.o.", "18 = 85-90 y.o.",
                        "19 = 90-95 y.o.", "20 = 95-100 y.o.", "21 = 100+ y.o.")

real_interventions <- setdiff(all_interventions, c("_", "(*Self-isolation) Household Isolation", "(*Self-isolation) Screening"))

# Default values for interventions.
nb_interventions_max <- 100
new_intervention_value <- "_"
new_daterange_value <- c(as.Date("2020-02-10"), as.Date("2020-06-30"))
new_coverage_value <- 0

# highchart export options.
hc_export_items <- c("downloadPNG", "downloadCSV", "downloadXLS")

# Model elements that are independants of inputs ----
A <- 21
# per year ageing matrix
ageing <- t(diff(diag(rep(1, A)), lag = 1) / (5 * 365.25))
ageing <- cbind(ageing, 0 * seq(1:A)) # no ageing from last compartment

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

# assign index case to an age class
ageindcase <- 20
aci <- floor((ageindcase / 5) + 1)

# those should have noise added when user decide to
# generate_uncertainty.md contains a table listing these parameters
parameters_noise <- c("p", "rho", "omega", "gamma", "nui", "ihr_scaling","nus", "nu_icu","nu_vent",
                      "rhos", "selfis_eff", "dist_eff", "hand_eff", "mask_eff", "work_eff", 
                      "w2h", "s2h", "cocoon_eff", "mean_imports", "screen_overdispersion", 
                      "quarantine_effort", "quarantine_eff_home", "quarantine_eff_other")

source("./www/model/fun_validation_interventions.R")
source("./www/model/fun_inputs.R")
source("./www/model/fun_multi_runs.R")
source("./www/model/fun_process_ode_outcome.R")
source("./www/model/fun_conf_interval.R")
source("./www/model/fun_check_parameters_list_for_na.R")
source("./www/model/fun_parse_age_group.R")