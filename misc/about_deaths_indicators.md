
In the box: `total_covid_deaths` for the end date.


with the following definition:

`total_covid_deaths` = `deaths_from_covid` (= `cinc_mort_all`) + `deaths_with_covid` (= `nat_deaths_inf`)

with:

# all deaths due to covid19 disease - reported + unreported
    `cinc_mort_all<-cinc_mort_1+cinc_mort_2+cinc_mort_3+cinc_mort_4+cinc_mort_5+cinc_mort_6+
      cinc_mort_7+cinc_mort_8+cinc_mort_9+cinc_mort_10+cinc_mort_11+cinc_mort_121+cinc_mort_131+cinc_mort_141`
      

# all deaths of infected with sars-cov-2 virus - reported + unreported
    nat_deaths_inf <- round(base_mort_E11 + base_mort_I11 + base_mort_CL11 + base_mort_X11 + 
                              base_mort_ER11 + base_mort_EV11+  base_mort_EVR11+   
                              base_mort_QE11 + base_mort_QI11 + base_mort_QC11 +  
                              base_mort_QEV11 + base_mort_QER11 + base_mort_QEVR11 + base_mort_Z1+
                              base_mort_H1+base_mort_HC11+base_mort_ICU1+base_mort_ICUC1+base_mort_ICUCV1+
                              base_mort_Vent1+base_mort_VentC1+base_mort_HCICU11+base_mort_HCV11)


In the plot "Baseline Cumlative Deaths": `cum_mortality`

`cum_mortality = round(rowSums(out_mat[,(CMindex+1)]))`