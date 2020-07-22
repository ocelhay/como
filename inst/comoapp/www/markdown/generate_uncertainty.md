
**Choosing a "Number of model runs" greater than 1 will generate uncertainty as described below. If you decide to do so, we recommend to go for a minimum of 10 runs. Simulation running time can amount to 1 second per run and 30 seconds to aggregate results (irrespective of the number of runs).**


Users of the CoMo Collaborative COVID-19 model need to comprehend and effectively communicate the multiple uncertainties involved in making quantitative projections about evolving outbreaks.  To assist in this "propagation of uncertainty", the model development team has created a way for users to specify a level of parametric (Gaussian) "white noise"" to **all of the model parameters listed below**.  If implemented by the user, this means that the chosen point value for each of these parameters will become the midpoint for a range defined by the standard deviation selected on the **Noise** slider bar.  With each modeling run, these parameters are then sampled from the resulting Gaussian distribution with mean given by the chosen parameter value.

**Confidence** sets the degree of uncertainty the user would like to have reflected in the model prediction, expressed over all the model runs. Specifically, this defines the quantile of model predictions for each time point that comprise the chosen the credible intervals. Thus, if confidence is set at 5%, the upper limit for the model credible interval will be one that excludes the highest 5% values of the model predictions for each data point; since the same restriction applies to the lowest 5% of values, this choice corresponds to a 90% credible interval.  Similarly, if the user choses 25%, then only the middle 50% of values is shown in the model output.


| Category                        	| Description                                                      	| Parameter             	|
|---------------------------------	|------------------------------------------------------------------	|-----------------------	|
| General                         	| Probability of infection given contact: (0 to 0.2)               	| p                     	|
| Country/Area                    	| Mean number of infectious migrants per day:                      	| mean_imports          	|
| Virus                           	| Relative infectiousness of incubation phase:                     	| rho                   	|
| Virus                           	| Average incubation period: (1 to 7 days)                         	| gamma                 	|
| Virus                           	| Average duration of symptomatic infection period: (1 to 7 days)  	| nui                   	|
| Virus                           	| Average duration of immunity: (0.5 to 150)                       	| omega                 	|
| Hospitalisation                 	| Relative percentage of regular daily contacts when hospitalised: 	| rhos                  	|
| Hospitalisation                 	| Scaling factor for infection hospitalisation rate: (0.5 to 4)    	| ihr_scaling           	|
| Hospitalisation                 	| Duration of hospitalised infection: (1 to 30)                    	| nus                   	|
| Hospitalisation                 	| Duration of ICU infection: (1 to 30)                             	| nu_icu                	|
| Hospitalisation                 	| Duration of ventilated infection: (1 to 30)                      	| nu_vent               	|
| Self-isolation if Symptomatic   	| Adherence:                                                       	| selfis_eff            	|
| Screening (when S.I.)           	| Overdispersion: (1, 2, 3, 4 or 5)                                	| screen_overdispersion 	|
| Household Isolation (when S.I.) 	| Days to implement maximum quarantine coverage: (1 to 5)          	| quarantine_effort     	|
| Household Isolation (when S.I.) 	| Decrease in the number of other contacts when quarantined:       	| quarantine_eff_other  	|
| Household Isolation (when S.I.) 	| Increase in the number of contacts at home when quarantined:     	| quarantine_eff_home   	|
| Social Distancing               	| Adherence:                                                       	| dist_eff              	|
| Handwashing                     	| Efficacy: (0-25%)                                                	| hand_eff              	|
| Working at Home                 	| Efficacy:                                                        	| work_eff              	|
| Working at Home                 	| Home contacts inflation due to working from home:                	| w2h                   	|
| School Closures                 	| Efficacy:                                                        	| school_eff            	|
| School Closures                 	| Home contacts inflation due to school closure:                   	| s2h                   	|
| Shielding the Elderly           	| Efficacy:                                                        	| cocoon_eff            	|

</br>