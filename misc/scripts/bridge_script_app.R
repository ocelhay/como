# set debug_app to TRUE or FALSE
# run covidage_v16.8,R until line 2242
# i.e. simul_baseline <- process_ode_outcome(out0,iterations,vectors0)

debug_app <- FALSE
results_scripts <- simul_baseline

# identical with results_app:
results_scripts$death_treated_ventilator
results_scripts$attributable_deaths
results_scripts$N
results_scripts$ab_all_ages
results_scripts$saturation


# different with results_app
results_scripts$tc

# check tc components
# identicals:
ifr
mort
totage 
# Browse[1]> sum(totage1)
# [1] 873056.5
totbase1
# Browse[1]> sum(totbase1)
# [1] 13759387

# Browse[1]> dim(tc)
# [1] 10836     3
# Browse[1]> sum(tc$value)
# [1] 346191.6

debug_app <- TRUE
