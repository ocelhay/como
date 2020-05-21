[Download this table in Excel format.](https://github.com/ocelhay/como/blob/master/Results_Legend_CoMoCOVID-19App.xlsx)

`*_`: either for baseline or hypothetical scenario.


| Variable                        | Description                                                                                                                                                          |
|------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| *_daily_incidence                  | Daily reported incidence (i.e. newly reported cases on each day). "Predicted Reported" line on the graph.                                                            |
| *_daily_total_cases                | Sum of daily reported and unreported incidence. "Predicted Reported + Unreported" line on the graph.                                                                 |
| *_normal_bed_occupancy             | Patients who occupy normal inpatient beds (up to their bed capacity) + Patients who need ICU care, but do not get it because ICU beds are fully occupied at time t.  |
| *_icu_bed_occupancy                | Patients who occupy ICU beds with ventilators + Patients who need ventilators, but do not get them because ventilator capacity is exhausted  at time t.              |
| *_icu_ventilator_occupancy         | Patients who occupy ICU beds with ventilators (maximum number possible is the ventilator capacity) at time t.                                                        |
| *_death_natural_non_exposed        | Cumulative non-COVID related deaths in the suscepitable population (S compartment) up to time t                                                                      |
| *_death_natural_exposed            | Cumulative non-COVID related deaths in the exposed population (E, I, CL, X, QS, QE, QI, QC, QR, and R compartments) up to time t                                     |
| *_death_treated_hospital           | Cumulative COVID-19 deaths in the hospitals (only from normal beds) up to time t                                                                                     |
| *_death_treated_icu                | Cumulative COVID-19 deaths in the ICUs up to time t                                                                                                                  |
| *_death_treated_ventilator         | Cumulative COVID-19 deaths while on ventilator up to time t                                                                                                          |
| *_death_untreated_hospital         | Cumulative COVID-19 deaths in patients who should have been in the hospital on inpatient beds up to time t                                                           |
| *_death_untreated_icu              | Cumulative COVID-19 deaths in patients who require ICU bed, but placed on normal beds up to time t                                                                   |
| *_death_untreated_ventilator       | Cumulative COVID-19 deaths in patients who require ventilators, but only get ICU beds up to time t                                                                   |
| *_death_untreated_ventilator_surge | Cumulative COVID-19 deaths in patients who ventilators, but placed on normal beds up to time t                                                                       |
| *_cum_mortality                    | Cumulative COVID-19 deaths + natural deaths from hospitalized, ICU, ventilator (and their overflown) compartments up to time t                                       |
| input_cases                        | Observed daily incidence (based on user input or from European Centre for Disease Prevention and Control)                                                            |
| input_deaths                       | Observed daily deaths (based on user input or from European Centre for Disease Prevention and Control)                                                               |
| input_cumulative_death             | Observed cumulative deaths (based on user input or from European Centre for Disease Prevention and Control)                                                          |
| *_si_vector                        | Coverage over time (self-isolation)                                                                                                                                  |
| *_sd_vector                        | Coverage over time (social distancing)                                                                                                                               |
| *_scr_vector                       | Coverage over time (screening)                                                                                                                                       |
| *_hw_vector                        | Coverage/efficacy over time (handwashing)                                                                                                                            |
| *_wah_vector                       | Coverage over time (working at home)                                                                                                                                 |
| *_sc_vector                        | Coverage over time (school closure)                                                                                                                                  |
| *_tb_vector                        | Coverage over time (travel ban)                                                                                                                                      |
| *_cte_vector                       | Coverage over time (shielding the elderly)                                                                                                                           |
| *_q_vector                         | Coverage over time (household isolation)                                                                                                                             |
| *_vc_vector                        | Coverage over time (vaccination)                                                                                                                                     |