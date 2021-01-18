Data with simulation results is provided in .csv format with the variables in the table below.


`*_`: either for baseline or hypothetical scenario.
Each variable ends with `_min`, `_med` or `_max`. If there is only one model run (no generated uncertainty), these values are identical. Otherwise these are the lower limit, median and upper limit for the model credible interval, as per the "Generate Uncertainty" section.


| Variable                            | Description                                                                                                                                                          |
|-------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| *_predicted_reported                | Daily reported incidence (i.e. newly reported cases on each day). "Predicted Reported" line on the graph.                                                            |
| *_predicted_reported_and_unreported | Sum of daily reported and unreported incidence. "Predicted Reported + Unreported" line on the graph.                                                                 |
| *_normal_bed_occupancy              | Patients who occupy normal inpatient beds (up to their bed capacity) + Patients who need ICU care, but do not get it because ICU beds are fully occupied at time t.  |
| *_icu_bed_occupancy                 | Patients who occupy ICU beds with ventilators + Patients who need ventilators, but do not get them because ventilator capacity is exhausted  at time t.              |
| *_icu_ventilator_occupancy          | Patients who occupy ICU beds with ventilators (maximum number possible is the ventilator capacity) at time t.                                                        |
| *_normal_bed_requirement            | Patients who require normal inpatient beds                                                                                                                           |
| *_icu_bed_requirement               | Patients who require ICU care, but don't need ventialtor                                                                                                             |
| *_icu_ventilator_requirement        | Patients who require ICU care with ventilator                                                                                                                        |
| *_death_natural_non_exposed         | Cumulative non-COVID related deaths in the suscepitable population (S compartment) up to time t                                                                      |
| *_death_natural_exposed             | Cumulative non-COVID related deaths in the exposed population up to time t                                                                                           |
| *_death_treated_hospital            | Cumulative COVID-19 deaths in the hospitals (only from normal beds) up to time t                                                                                     |
| *_death_treated_icu                 | Cumulative COVID-19 deaths in the ICUs up to time t                                                                                                                  |
| *_death_treated_ventilator          | Cumulative COVID-19 deaths while on ventilator up to time t                                                                                                          |
| *_death_untreated_hospital          | Cumulative COVID-19 deaths in patients who should have been in the hospital on inpatient beds up to time t                                                           |
| *_death_untreated_icu               | Cumulative COVID-19 deaths in patients who require ICU bed, but placed on normal beds up to time t                                                                   |
| *_death_untreated_ventilator        | Cumulative COVID-19 deaths in patients who require ventilators, but only get ICU beds up to time t                                                                   |
| *_cum_mortality                     | Cumulative COVID-19 deaths + natural deaths from hospitalized, ICU, ventilator (and their overflown) compartments up to time t                                       |
| input_cases                         | Observed daily incidence (based on user input or from European Centre for Disease Prevention and Control)                                                            |
| input_deaths                        | Observed daily deaths (based on user input or from European Centre for Disease Prevention and Control)                                                               |
| input_cumulative_death              | Observed cumulative deaths (based on user input or from European Centre for Disease Prevention and Control)                                                          |
| *_si_vector                         | Coverage over time (self-isolation)                                                                                                                                  |
| *_sd_vector                         | Coverage over time (social distancing)                                                                                                                               |
| *_scr_vector                        | Coverage over time (screening)                                                                                                                                       |
| *_hw_vector                         | Coverage/efficacy over time (handwashing)                                                                                                                            |
| *_msk_vector                        | Mask Use                                                                                                                                                             |
| *_wah_vector                        | Coverage over time (working at home)                                                                                                                                 |
| *_sc_vector                         | Coverage over time (school closure)                                                                                                                                  |
| *_tb_vector                         | Coverage over time (travel ban)                                                                                                                                      |
| *_mt_vector                         | Mass Testing                                                                                                                                                         |
| *_cte_vector                        | Coverage over time (shielding the elderly)                                                                                                                           |
| *_q_vector                          | Coverage over time (household isolation)                                                                                                                             |
| *_vc_vector                         | Coverage over time (vaccination)                                                                                                                                     |