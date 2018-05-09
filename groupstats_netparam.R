library("tidyr", lib.loc="~/R/win-library/3.2")
library("ggplot2", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")

#statistics on network parameters
EO_all_network_filter<-na.omit(EO_all_network)
#(age, sec, SPM, ZVT no group differences: five non native speakers within AP...)

#run all t.tests for one frequency band (EO)
lapply(EO_all_network[,c(141:218,231:238)], 
       function(x) t.test(x ~ EO_all_network$AP.0.RP.1.AP., var.equal = TRUE))
##-->threshold T3 sometimes sign or nearly (Path lengt, Small world)


lapply(EO_alpha_network[,c(140:218,231:238)], 
       function(x) t.test(x ~ EO_all_network$AP.0.RP.1.AP., var.equal = TRUE,na.rm=TRUE))
#reduced small worldness T4,(T5)
#L_T4 slightly reduced (.14), E_T4 slightly higher (.14)
#Crand_T3,C_T7 slightly lower (.12,.17)

lapply(EO_beta_network[,c(140:218,231:238)], 
       function(x) t.test(x ~ EO_all_network$AP.0.RP.1.AP., var.equal = TRUE))
#Lrand_T7 higher (.04),Lrand_T4,L_T5 slightly higher (.14),
#Crand_T09 and T10 lower (.016), T7,T8,T5 tendency (.14)
#CErand_T7 lower (.0538), Erand_T7 (.0538)

lapply(EO_delta_network[,c(140:218,231:238)], 
       function(x) t.test(x ~ EO_all_network$AP.0.RP.1.AP., var.equal = TRUE))
#lower E_T7,T6,T5 (T4,T3),T2,T1 (.009-.05)
#Lrand_T1,T2 lower (.03-.01)
#L_T1-T10 higher (.13-.01)
#Crand T2 lower (.04),C_T2 (.10)
#CE_T1-10 reduced (.05-.10), CErand_T1,T2 (.01,.05)

lapply(EO_gamma_network[,c(140:218,231:238)], 
       function(x) t.test(x ~ EO_all_network$AP.0.RP.1.AP., var.equal = TRUE))
#nothing

lapply(EO_theta_network[,c(140:218,231:238)], 
       function(x) t.test(x ~ EO_all_network$AP.0.RP.1.AP., var.equal = TRUE))
#L_T6-T10 slightly lower (.19-.08)
#C_T2 higher (0.01)


###################
#EC

lapply(EC_all_network[,c(140:218,231:238)], 
       function(x) t.test(x ~ EC_all_network$AP.0.RP.1.AP., var.equal = TRUE))
#E_T5 higher (.09), _T3,_T4 (.13-.16)
#L_ T6 lower (.18), _T5 (.08), _T4(.10), _T3 (.13)
#Lrand_T4 lower (.09), _T3 (.12)
#Crand_T4,T3 higher (.14), _T2 lower (.047)
#SW_T6-T10 lower (.10-.07)
#CE_T5 higher(.09), T4,T3 (.13-.16)

lapply(EC_alpha_network[,c(140:218,231:238)], 
       function(x) t.test(x ~ EC_alpha_network$AP.0.RP.1.AP., var.equal = TRUE))
#Crand_T2 lower(.09), C_T6 lower (.09)
#SW_T6,T7,T9 lower (.15-.19)

lapply(EC_beta_network[,c(140:218,231:238)], 
       function(x) t.test(x ~ EC_beta_network$AP.0.RP.1.AP., var.equal = TRUE))
#C_T2, Crand_T8 higher (.15)
#SW_T8 reduced (.11)

lapply(EC_delta_network[,c(140:218,231:238)], 
       function(x) t.test(x ~ EC_delta_network$AP.0.RP.1.AP., var.equal = TRUE)) 
#Lrand_T10 higher (0.009), T9 (.04), T8 (.13), T7 (.014), T6 (.18), T5(.02), T1 (.04)
#L_T10,T9 higher (.02), T8 (.06), T1 (.10)
#CErand_T10,T9,T7,T5 lower (.009-.03), T1 (.7), T8 (.13)
#CE_T10,T9 lower (.04)
#Erand_T10,T9,T7,T5 lower (.01,.03), T1 (.7), T8(.13)
#E_T10,T9 (.04), t8 (.11)

lapply(EC_gamma_network[,c(140:218,231:238)], 
       function(x) t.test(x ~ EC_gamma_network$AP.0.RP.1.AP., var.equal = TRUE)) 
#SW_T4 (.03),, SW_T3(.07) SW_T6 (.15)

lapply(EC_theta_network[,c(140:218,231:238)], 
       function(x) t.test(x ~ EC_theta_network$AP.0.RP.1.AP., var.equal = TRUE)) 
#nothing


############filter excluded participants e.g.
#(sex, age, SPM,ZVT no group differences)


german_alpha_EC<-filter(EC_alpha_network,
                        EC_alpha_network$exclude.0.no.1.elec.2.med.3.exp.==0)
##electrodes interpolated, medication, bad experimants behaviour (total 5)
#basically does not change effects (only reduces a bit the alpha level.

#exclude asian AP
german_delta_EC<-filter(EC_delta_network,
                        EC_delta_network$german_native==1)
#basically does not change effects (only changes a bit the alpha level, more towards higher significance!)

german_beta_EO<-filter(EO_beta_network,
                       EO_beta_network$german_native==1)

german_delta_EO<-filter(EO_delta_network,
                        EO_delta_network$german_native==1)