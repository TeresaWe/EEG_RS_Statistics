#matching and group differences demographics

#all participants
t.test(ZVT_T~APdef, data=EO_beta_network)
t.test(SPM_C~APdef, data=EO_beta_network)
t.test(age~APdef, data=EO_beta_network)
t.test(APdef~age, data=EO_beta_network)
t.test(tonal_PR~APdef, data=EO_beta_network)
t.test(comp_PR~APdef, data=EO_beta_network)
t.test(ryth_PR~APdef, data=EO_beta_network)
sum(EO_beta_network$Group..0.RP..1.AP..2.ASD..3.NM.==1) #--> 31 AP
sum(EO_beta_network$Group..0.RP..1.AP..2.ASD..3.NM.==0) #--> 33 RP

#after exclusion (5)
t.test(ZVT_T~APdef, data=german_beta_EO)
t.test(SPM_C~APdef, data=german_beta_EO)
t.test(age~APdef, data=german_beta_EO)
t.test(APdef~age, data=german_beta_EO)
t.test(tonal_PR~APdef, data=german_beta_EO)
t.test(comp_PR~APdef, data=german_beta_EO)
t.test(ryth_PR~APdef, data=german_beta_EO)
sum(german_beta_EO$Group..0.RP..1.AP..2.ASD..3.NM.==1) #--> 26 AP
sum(german_beta_EO$Group..0.RP..1.AP..2.ASD..3.NM.==0) #--> 33 RP

levels(allresults$main_instrument)
#[1] "Akkordeon"   "Bratsche"    "E-Bass"      "Fagott"      "Gesang"      "Gitarre"    
#[7] "Klarinette"  "Klavier"     "Oboe"        "Posaune"     "Querfloete"  "Schlagzeug" 
#[13] "Trompete"    "Violine"     "Violoncello" "Waldhorn"

levels(allresults$main_instrument)<-c("accordion","viola","bass","bassoon","voice","guitar","clarinet",
                                      "piano","oboe","trombone","flute","drums","trumpet","violine","violoncello","french horn")

AP<-subset(allresults, allresults$AP.0.RP.1.AP.==1) 
colors<-rainbow(16)
colors_AP<-c(colors[2:5], colors[8:9],colors[13:15])
plot(AP$main_instrument)# plot isntruments
instruments_AP<-summary(AP$main_instrument)
instruments_AP<-c(instruments_AP[2:5], instruments_AP[8:9],instruments_AP[13:15])
pie(instruments_AP, main="AP instruments",col=colors_AP)
RP<-subset(allresults, allresults$AP.0.RP.1.AP.==0)
colors_RP<-c(colors[1:3], colors[5:12],colors[14:16])
instruments_RP<-summary(RP$main_instrument)
instruments_RP<-c(instruments_RP[1:3], instruments_RP[5:12],instruments_RP[14:16])
pie(instruments_RP, main="RP instruments",col=colors_RP)
plot(RP$main_instrument) #plot instruments

levels(allresults$main_instrument)<-c("1","2","3","4","5","6","7",
                                      "8","9","10","11","12","13","14","15","16")

allresults$main_instrument<-as.numeric(as.character(allresults$main_instrument))
