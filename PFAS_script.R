#Libraries Needed to Run Code in R
library(tidyverse)
library(scales)
library(lubridate)
library(title)
library(moderndive)

#Code to Access the Data 
All_Data_pfas <- read.csv("~/Westfieldfinalproject/pfas/All_Data_pfas.csv")

########################################################################################################################

#This code is used to produce plots of average reportable concentrations for PFAS compounds in the influents, effluents, filter cakes, and major industries discharging pfas contaminated water at two
#wastewater treatment facilities owned by the Narragansett Bay Commission. Any input into
#the ALL_Data_pfas excel sheet that contains reportable concentrations will be reflected in the output of this code. 
#If any data is added to ALL_Data_pfas.xls, this script can be run to quickly generate new results of average reportable PFAS compound concentrations at each wastewater facility.

###########################################################################################################################
#FIELDS POINT wastewater treatment facility

#filtering for fields point influent and effluent reportable avgerage concentrations. Ignoring "u" and "j" qualifiers because the data are non-detect and non-reportable

FP_INFLUENT<-All_Data_pfas %>% filter(Description=="FP Influent") %>% filter(Qualifier==" ") %>%    
  group_by(Description,Compound,Plant)%>% 
  summarize(textResult= mean(textResult))#average concentrations of PFAS compounds in Fields point inlfuent
FP_EFFLUENT<-All_Data_pfas %>% filter(Description=="FP Final Effluent") %>% filter(Qualifier==" ") %>% 
  group_by(Description,Compound,Plant) %>% 
  summarize(textResult=mean(textResult))#average concentrations of PFAS compounds in Fields point Effluent


#joining PFAS compounds in FP influent and Effluent that had average reportable concentrations
FP_INF_EFF<-rbind(FP_EFFLUENT,FP_INFLUENT)

#plot of Avg. Reportable Conc.(ng/L) of PFAS compounds at Fields Point from Oct. 2020 to May 2021 
ggplot(FP_INF_EFF,mapping=aes(Description,textResult,fill=Description))+geom_col()+
  facet_wrap(~Compound)+scale_x_discrete(limits=c("FP Influent","FP Final Effluent"))+
    ylab("Avg. Concenration (ng/L)")+xlab(" ")+theme(legend.position = "none")+theme(axis.text.x = element_text(angle = -90))


# model to check to see if avg conc. is a function of INFULENT and EFFLUENT
model_FP<-lm(textResult~Description, data=FP_INF_EFF)
#pulls P_value
summaryFP<-get_regression_summaries(model_FP)%>% pull(p_value)

#######################################################################
#BUCKLIN POINT wastewater treatment facility

#most of the code below has the same format except this time we are filtering for Bucklin Point instead of Fields Point
#filtering for Bucklin Point influent and effluent reportable avg concentrations.Ignoring "u" and "j" qualifiers because the data are non-detect and non-reportable
BP_INFLUENT<-All_Data_pfas %>% filter(Description=="BP Influent") %>% filter(Qualifier==" ") %>%    
  group_by(Description,Compound,Plant) %>% 
  summarize(textResult= mean(textResult))
BP_EFFLUENT<-All_Data_pfas %>% filter(Description=="BP Final Effluent") %>% filter(Qualifier==" ") %>% 
  group_by(Description,Compound,Plant) %>% 
  summarize(textResult=mean(textResult))

#joining BP influent and Effluent that had average reportable concentrations
BP_INF_EFF<-rbind(BP_EFFLUENT,BP_INFLUENT)

#plot of Avg. Reportable Conc.(ng/L) of PFAS compounds at Bucklin Point from Oct. 2020 to May 2021  
ggplot(BP_INF_EFF,mapping=aes(Description,textResult,fill=Description))+geom_col()+
  facet_wrap(~Compound)+scale_x_discrete(limits=c("BP Influent","BP Final Effluent"))+
  ylab("Avg. Concenration (ng/L)")+xlab(" ")+theme(legend.position = "none")+theme(axis.text.x = element_text(angle = -90))

#check to see if avg conc. is a function of BP INFULENT and BP EFFLUENT
model_BP<-lm(textResult~Description,data=BP_INF_EFF)
#Pull P_value
summaryBP<-get_regression_summaries(model_BP)%>% pull(p_value)

################################################################################
#FILTER CAKE DATA for both FIELDS POINT and BUCKLIN POINT wastewater treatment facilities
#NOTE: only a few compounds had reportable concentration in each facilities' filtercakes, thus they were combined for visual clarity
#NOTE: units for pfas in solids are reported in (ug/kg) which is why they needed their own plot


#Not a lot of PFAS compounds found in filter cake so they are joined together to make one plot
#filtering for FP filtercake and BP filtercake average reportable PFAS compound concentrations
FP_FCAK<-All_Data_pfas %>% filter(Description=="FP Filter Cake") %>% filter(Qualifier==" ") %>%    
  group_by(Description,Compound,Plant) %>% 
  summarize(textResult= mean(textResult))
BP_FCAK<-All_Data_pfas %>% filter(Description=="BP Filter Cake") %>% filter(Qualifier==" ") %>% 
  group_by(Description,Compound,Plant) %>% 
  summarize(textResult=mean(textResult))

#joining FP filtercake and BP filter cake average PFAS compound concentrations into one dataframe
FP_BP_FCAK<-rbind(FP_FCAK,BP_FCAK)
#Plot of Avg. Reportable Conc.(ug/Kg) of PFAS compounds in Fields Point and Bucklin Point Filter Cake from October 2020 to May 2021     
ggplot(FP_BP_FCAK,mapping=aes(Description,textResult,fill=Description))+geom_col()+
  facet_wrap(~Compound)+scale_x_discrete(limits=c("FP Filter Cake","BP Filter Cake"))+
  ylab("Avg. Concenration (ug/Kg)")+xlab(" ")+theme(legend.position = "none")+theme(axis.text.x = element_text(angle = -90))

#model to see if  average concentration is dependent on the type of PFAS compound
model_fcak<-lm(textResult~Compound,data=FP_BP_FCAK)
#pulling the P-value
summaryFCAK<-get_regression_summaries(model_fcak)%>% pull(p_value)


############################################################################################
# FP significant industrial user (SIU) DATA

#filtering data by FP SIU and average reportable PFAS Compound concentrations
FP_SIU<-All_Data_pfas %>% filter(Sampling.point.type=="SIU") %>% filter(Qualifier==" ") %>% filter(Plant=="FP") %>%    
  group_by(Point.location,Compound,Plant) %>%  
  summarize(textResult= mean(textResult))

#plot of Avg. Reportable Conc.(ng/L) of PFAS compounds found in Fields Point Industrial Users
ggplot(FP_SIU,mapping=aes(Compound,textResult,color=Compound))+geom_point()+facet_wrap(~Point.location)+theme(axis.text.x = element_text(angle = -90))+theme(legend.position = "none")+
  labs(title="Avg. Reportable Conc.(ng/L) of PFAS compounds found in Fields Point Industrial Users")+
  ylab("Avg. Conc. (ng/L)")+xlab("Compound")+ theme(text = element_text(size = 8)) 

#model to determine if there is a difference between PFAS concentrations and Industry 
model_FPSIU<- lm(textResult~Point.location,data=FP_SIU)
#pull p-value
summaryFPSIU<-get_regression_summaries(model_FPSIU) %>% pull(p_value)

#################################################################################################
#BP significant industrial user (SIU) DATA

#filtering data by BP SIU and reportable average concentrations
BP_SIU<-All_Data_pfas %>% filter(Sampling.point.type=="SIU") %>% filter(Qualifier==" ") %>% filter(Plant=="BP") %>%     
  group_by(Point.location,Compound,Plant) %>% 
  summarize(textResult= mean(textResult))

#plot of Avg. Reportable Conc.(ng/L) of PFAS compounds found in Bucklin Point Industrial Users
ggplot(BP_SIU,mapping=aes(Compound,textResult,color=Compound))+geom_point()+facet_wrap(~Point.location)+theme(axis.text.x = element_text(angle = -90))+theme(legend.position = "none")+
  labs(title="Avg. Reportable Conc.(ng/L) of PFAS compounds found in Bucklin Point Industrial Users")+
  ylab("Avg. Conc. (ng/L)")+xlab("Compound")+theme(text = element_text(size = 8)) 

#model to determine if there is a difference between PFAS concentrations and INdustry 
model_BPSIU<- lm(textResult~Point.location,data=BP_SIU)
#pull p-value
summaryBPSIU<-get_regression_summaries(model_BPSIU) %>% pull(p_value)
