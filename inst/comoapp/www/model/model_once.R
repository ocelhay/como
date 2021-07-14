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

valid_interventions <- c("Dexamethasone", "Handwashing", "International Travel Ban",
                             "Mask Wearing", "Mass Testing", "School Closures", "Self-isolation if Symptomatic",
                             "(*Self-isolation) Household Isolation", "(*Self-isolation) Screening", "Shielding the Elderly",
                             "Social Distancing", "Vaccination", "Working at Home", "Partial School Closures")

all_interventions <- c("_", valid_interventions)

real_interventions <- setdiff(valid_interventions, c("(*Self-isolation) Household Isolation", "(*Self-isolation) Screening"))

real_rr_interventions <- c("Transmissibility", "Lethality", "Breakthrough infection probability")
rr_interventions <- c("_", real_rr_interventions)


vec_age_categories <- c("1 = 0-5 y.o.", "2 = 5-10 y.o.", "3 = 10-15 y.o.", 
                        "4 = 15-20 y.o.", "5 = 20-25 y.o.", "6 = 25-30 y.o.",
                        "7 = 30-35 y.o.", "8 = 35-40 y.o.", "9 = 40-45 y.o.",
                        "10 = 45-50 y.o.", "11 = 50-55 y.o.", "12 = 55-60 y.o.",
                        "13 = 60-65 y.o.", "14 = 65-70 y.o.", "15 = 70-75 y.o.",
                        "16 = 75-80 y.o.", "17 = 80-85 y.o.", "18 = 85-90 y.o.",
                        "19 = 90-95 y.o.", "20 = 95-100 y.o.", "21 = 100+ y.o.")

# Default values for interventions.
nb_interventions_max <- 200
new_daterange_value <- c(as.Date("2020-02-10"), as.Date("2020-06-30"))

# highchart export options.
hc_export_items <- c("downloadPNG", "downloadCSV", "downloadXLS")

# Model elements that are independants of inputs ----
A <- 21
# per year ageing matrix
ageing <- t(diff(diag(rep(1, A)), lag = 1) / (5 * 365.25))
ageing <- cbind(ageing, 0 * seq(1:A)) # no ageing from last compartment

# Define the indices for each variable
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
source("./www/model/fun_process_ode_outcome_compute.R")
source("./www/model/fun_conf_interval.R")
source("./www/model/fun_check_parameters_list_for_na.R")
source("./www/model/fun_parse_age_group.R")