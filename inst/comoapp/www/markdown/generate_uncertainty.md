If you decide to perform several model runs, **we recommend to go for a minimum of 10 runs.**


### Running time

When performimng several model runs, simulation time can amount to 1 second per run and half a minute to aggregate results.


### Noise

To provide a measure of model prediction uncertainty, we impose Gaussian white noise to the following model parameters:

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
For each model run we then have a set of these parameters each sampled from a Gaussian distribution with mean given by the parameter value set in the data template or app, and a standard deviation given by parameter noise. The remaining model parameters do not change across model runs

### Confidence

This sets the degree of uncertainty one wants to have reflected in the model prediction. Given all the model runs, this defined the quantile of model predictions for each time point that will define the credible intervals. Thus, if confidence is set at 95%, the upper limit for the model credible interval will be one that excludes the highest 5% values of the model predictions for each data point.