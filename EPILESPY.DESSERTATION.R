
# Library  used for the Analysis.
install.packages("RODBC")
install.packages("tcltk")
library(sjPlot)
library(gridExtra)
library(tidyverse)
library(plyr)
library(ggplot2)
library(dplyr)
library(ztable)
library(scales)
library(tidyr)
library(RColor)

#Code to Connect DB2 server to R studio.
setwd("/shared/0895 - Educational attainment of children with epilepsy in Wales/Meghna/")
source("sail_login.r")




################################# Key stage data ##########################################

# To Perform Logistic regression for children with epilepsy for Key Stage 2.

# Importing dataset from DB2 into R, to carry out statistical Analysis.
Keystage2 <- sqlQuery(channel, "select CSI1,Quintile,School_year,GNDR_CD,DRUG_NAME from  Sailw0895v.epi_Keysatge_2")
summary(Keystage2)

# code to check the data type of the variables in the dataset.
str(Keystage2)


# Preparing the table for Logistic regression.
#Converting drug_name from String to numeric.
Keystage2 <- Keystage2 %>% mutate(DRUG_CODE=ifelse(DRUG_NAME=="LOMOTRIGINE",26,
          ifelse(DRUG_NAME=="LEVETRICETAM",2,
          ifelse(DRUG_NAME=="VALPROIC ACID",3,
          ifelse(DRUG_NAME=="ARBIL",4,
          ifelse(DRUG_NAME=="TOPIRAMATE",5,
          ifelse(DRUG_NAME=="SODIUM VALPROATE",6,
          ifelse(DRUG_NAME=="ZONISAMIDE",7,
          ifelse(DRUG_NAME=="ETHOSUXIMIDE",8,
          ifelse(DRUG_NAME=="PENTRAN",9,
          ifelse(DRUG_NAME=="RUFINAMIDE",10,
          ifelse(DRUG_NAME=="LACOSAMIDE",11,
          ifelse(DRUG_NAME=="VIGABATRIN",12,
          ifelse(DRUG_NAME=="GABAPENTINE",13,
          ifelse(DRUG_NAME=="PHENYTOIN",14,
          ifelse(DRUG_NAME=="OXCARBAZEPINE",15,
          ifelse(DRUG_NAME=="DIAZEPAM",16,
          ifelse(DRUG_NAME=="PHENOBARBITAL",17,
          ifelse(DRUG_NAME=="PIRACETAM",18,
          ifelse(DRUG_NAME=="ESLICARBAZEPINE_ACETATE",19,
          ifelse(DRUG_NAME=="GABITRIL",20,
          ifelse(DRUG_NAME=="RETIGABINE",21,
          ifelse(DRUG_NAME=="PREGABALINE",22,
          ifelse(DRUG_NAME=="STIRIPENTOL",23,
          ifelse(DRUG_NAME=="EMESIDE",24,
          ifelse(DRUG_NAME=="PRIMIDONE",25,
          ifelse(DRUG_NAME=="NO_DRUG",1,NA)))))))))))))))))))))))))))

                                                                       
# converting numeric to factor(nominal encoding)
Keystage2$DRUG_CODE <- as.factor(Keystage2$DRUG_CODE)

#encoding school Year
Keystage2$SCHOOL_YEAR <- as.factor(Keystage2$SCHOOL_YEAR)

#encoding gender 
Keystage2$GNDR_CD <- as.factor(Keystage2$GNDR_CD)

#Binary encoding the output variable CSI ( core subject indicators)
Keystage2 <- Keystage2 %>% mutate(CSI1=ifelse(CSI1=="Y",1,0))
Keystage2$CSI1 <- as.factor(Keystage2$CSI1)

#ordinal encoding.
Keystage2$QUINTILE <- as.factor(Keystage2$QUINTILE)

#verifying the data type conversion.
str(Keystage2)

# Running logistic regression for Key Stage 2
logis_EP_Key2 <- glm(CSI1~ QUINTILE + SCHOOL_YEAR +GNDR_CD + DRUG_CODE ,data=Keystage2, family=binomial(link="logit"),
          na.action=na.exclude)
# summary report after running logistic regression for Key Stage 2.Analysing the statistical Variables.
summary(logis_EP_Key2)




# To Perform Logistic regression for children with epilepsy for Key Stage 3.

# Importing dataset from DB2 into R, to carry out statistical Analysis.
Keystage3 <- sqlQuery(channel, "select CSI1,Quintile,School_year,GNDR_CD,dRUG_NAME from  Sailw0895v.epi_Keysatge_3")
summary(Keystage3)

# Preparing the table for Logistic regression.
#Converting drug_name from String to numeric.
Keystage3 <- Keystage3%>% mutate(DRUG_CODE=ifelse(DRUG_NAME=="LOMOTRIGINE",26,
                          ifelse(DRUG_NAME=="LEVETRICETAM",2,
                          ifelse(DRUG_NAME=="VALPROIC ACID",3,
                          ifelse(DRUG_NAME=="ARBIL",4,
                          ifelse(DRUG_NAME=="TOPIRAMATE",5,
                          ifelse(DRUG_NAME=="SODIUM VALPROATE",6,
                          ifelse(DRUG_NAME=="ZONISAMIDE",7,
                          ifelse(DRUG_NAME=="ETHOSUXIMIDE",8,
                          ifelse(DRUG_NAME=="PENTRAN",9,
                          ifelse(DRUG_NAME=="RUFINAMIDE",10,
                          ifelse(DRUG_NAME=="LACOSAMIDE",11,
                          ifelse(DRUG_NAME=="VIGABATRIN",12,
                          ifelse(DRUG_NAME=="GABAPENTINE",13,
                          ifelse(DRUG_NAME=="PHENYTOIN",14,
                          ifelse(DRUG_NAME=="OXCARBAZEPINE",15,
                          ifelse(DRUG_NAME=="DIAZEPAM",16,
                          ifelse(DRUG_NAME=="PHENOBARBITAL",17,
                          ifelse(DRUG_NAME=="PIRACETAM",18,
                          ifelse(DRUG_NAME=="ESLICARBAZEPINE_ACETATE",19,
                          ifelse(DRUG_NAME=="GABITRIL",20,
                          ifelse(DRUG_NAME=="RETIGABINE",21,
                          ifelse(DRUG_NAME=="PREGABALINE",22,
                          ifelse(DRUG_NAME=="STIRIPENTOL",23,
                          ifelse(DRUG_NAME=="EMESIDE",24,
                          ifelse(DRUG_NAME=="PRIMIDONE",25,
                          ifelse(DRUG_NAME=="NO_DRUG",1,NA)))))))))))))))))))))))))))                                                                                    


# converting numeric to factor(nominal encoding)
Keystage3$DRUG_CODE <- as.factor(Keystage3$DRUG_CODE)

#Binary encoding the output variable CSI ( core subject indicators)
Keystage3 <- Keystage3 %>% mutate(CSI1=ifelse(CSI1=="Y",1,0))
Keystage3$CSI1 <- as.factor(Keystage3$CSI1)

#encoding school Year
Keystage3$SCHOOL_YEAR <- as.factor(Keystage3$SCHOOL_YEAR)


#encoding gender 
Keystage3$GNDR_CD <- as.factor(Keystage3$GNDR_CD)

#ordinal encoding 
Keystage3$QUINTILE <- as.factor(Keystage3$QUINTILE)

# to determine data type of exach variable 
str(Keystage3)

# Running logistic regression for Key Stage 3
logis_EP_Key3 <- glm(CSI1~ QUINTILE + SCHOOL_YEAR +GNDR_CD+DRUG_CODE , data=Keystage3, family=binomial)

# summary report after running logistic regression for Key Stage 3.Analysing the statistical Variables.
summary(logis_EP_Key3)




# To Perform Logistic regression for children with epilepsy for Key Stage 4.

# Importing dataset from DB2 into R, to carry out statistical Analysis.
Keystage4 <- sqlQuery(channel, "select  CSI1,Keystage,Quintile,School_year,drug_nAME,GNDR_CD from  Sailw0895v.epi_Keysatge_4")

#Preparing the table for Logistic regression.
#Converting drug_name from String to numeric.
Keystage4 <- Keystage4%>% mutate(DRUG_CODE=ifelse(DRUG_NAME=="LOMOTRIGINE",26,
                                ifelse(DRUG_NAME=="LEVETRICETAM",2,
                                ifelse(DRUG_NAME=="VALPROIC ACID",3,
                                ifelse(DRUG_NAME=="ARBIL",4,
                                ifelse(DRUG_NAME=="TOPIRAMATE",5,
                                ifelse(DRUG_NAME=="SODIUM VALPROATE",6,
                                ifelse(DRUG_NAME=="ZONISAMIDE",7,
                                ifelse(DRUG_NAME=="ETHOSUXIMIDE",8,
                                ifelse(DRUG_NAME=="PENTRAN",9,
                                ifelse(DRUG_NAME=="RUFINAMIDE",10,
                                ifelse(DRUG_NAME=="LACOSAMIDE",11,
                                ifelse(DRUG_NAME=="GABAPENTINE",13,
                                ifelse(DRUG_NAME=="PHENYTOIN",14,
                                ifelse(DRUG_NAME=="OXCARBAZEPINE",15,
                                ifelse(DRUG_NAME=="DIAZEPAM",16,
                                ifelse(DRUG_NAME=="PHENOBARBITAL",17,
                                ifelse(DRUG_NAME=="PIRACETAM",18,
                                ifelse(DRUG_NAME=="ESLICARBAZEPINE_ACETATE",19,
                                ifelse(DRUG_NAME=="GABITRIL",20,
                                ifelse(DRUG_NAME=="RETIGABINE",21,
                                ifelse(DRUG_NAME=="PREGABALINE",22,
                                ifelse(DRUG_NAME=="STIRIPENTOL",23,
                                ifelse(DRUG_NAME=="EMESIDE",24,
                                ifelse(DRUG_NAME=="PRIMIDONE",25,
                                ifelse(DRUG_NAME=="NO_DRUG",1,NA))))))))))))))))))))))))))                                                                                  


## converting numeric to factor(nominal encoding)
Keystage4$DRUG_NAME <- as.factor(Keystage4$DRUG_NAME)                                                                                                                                          

#Binary encoding the output variable CSI ( core subject indicators)
Keystage4 <- Keystage4 %>% mutate(CSI1=ifelse(CSI1=="Y",1,0))
Keystage4$CSI1 <- as.factor(Keystage4$CSI1)

#encoding school Year
Keystage4$SCHOOL_YEAR <- as.factor(Keystage4$SCHOOL_YEAR)

#encoding gender 
Keystage4$GNDR_CD <- as.factor(Keystage4$GNDR_CD)

# encoding Quintile
Keystage4$QUINTILE <- as.factor(Keystage4$QUINTILE)

# to determine data type of exach variable
str(Keystage4)

# Running logistic regression for Key Stage 4
logis_EP_Key4 <- glm(CSI1~ QUINTILE + SCHOOL_YEAR +GNDR_CD+DRUG_NAME , data=Keystage4, family=binomial)

# summary report after running logistic regression for Key Stage 3.Analysing the statistical Variables.
summary(logis_EP_Key4)



# plots for Children with Epilepsy 
################### Function for Plots #####################

plot_res_stage <- function(stage_num) {
  
#1) Plot Quintile vs Pass % - Line graph
  
  ##get pass count for each Quintile
  K_Quintile <- stage_num%>% group_by(QUINTILE,CSI1)%>%
    dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
  ##get total count per Quintile
  K_Quintile_1<- stage_num%>% group_by(QUINTILE)%>% dplyr::summarise(total=n())
  ##calculate proportion
  K_Quintile$Pass_Percent <- K_Quintile$pass_count/K_Quintile_1$total
  ##plot Quintile vs Pass %
  print(ggplot(data=K_Quintile,aes(x=QUINTILE,y=Pass_Percent,group=1))+
          geom_line(aes(group=1),colour="#9933FF")+geom_point(aes(group=1),colour="#9933FF")+
          labs(title="Keystage2- Quintile vs Pass Percentage")+
          scale_y_continuous(labels=scales::percent)+
          theme(legend.position="none",axis.text=element_text(face="bold")))  
  
  
#2) Plot Quintile vs Pass % with respect to gender - Line graph
  
  ##get pass count for each Gender per each Quintile
  K_Quintile <- stage_num%>% group_by(GNDR_CD,QUINTILE,CSI1)%>%
    dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
  ##get total count for each Gender per each Quintile
  K_Quintile_1<- stage_num%>% group_by(GNDR_CD,QUINTILE)%>% dplyr::summarise(total=n())
  ##calculate proportion
  K_Quintile$Pass_Percent <- K_Quintile$pass_count/K_Quintile_1$total
  ##plot Quintile vs Pass % for both genders
  print(ggplot(data=K_Quintile,aes(x=QUINTILE,y=Pass_Percent,colour=as.factor(GNDR_CD),group=GNDR_CD))+
          geom_line()+geom_point()+
          labs(title="Keystage2- Quintile vs Pass Percentage")+
          scale_y_continuous(labels=scales::percent)+
          scale_colour_discrete(name="Gender",labels=c("Male","Female"))+
          theme(axis.text=element_text(face="bold")))
  
  
#3) Bar Plot- Pass ratio with respect to gender
  ##get pass count for gender
  K_Gender <- stage_num%>% group_by(GNDR_CD,CSI1)%>%
    dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
  ##get total count per gender
  K_Gender_1<- stage_num%>% group_by(GNDR_CD)%>% dplyr::summarise(total=n())
  ##calculate proportion
  K_Gender$Pass_Percent <- K_Gender$pass_count/K_Gender_1$total
  ##plot gender vs Pass %
  print(ggplot(data=K_Gender, aes(x=GNDR_CD,y=Pass_Percent))+
          geom_bar(stat='identity',width=0.3,fill="#9933FF") + 
          theme_classic() +
          labs(x="Gender",title="Keystage2- Gender vs Pass Percentage")+
          scale_x_discrete(limit=c("1","2"),labels=c("Male","Female"))+
          scale_y_continuous(labels=scales::percent)+
          scale_fill_discrete(labels=c("Fail","Pass"))+
          theme(legend.position="none",axis.text=element_text(face="bold")))
  
  
#4) Plot School_Year vs Pass %
  ##get pass count for each School_Year
  K_Schoolyear <- stage_num%>% group_by(SCHOOL_YEAR,CSI1)%>%
    dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
  ##get total count for School_Year
  K_Schoolyear_1<- stage_num%>% group_by(SCHOOL_YEAR)%>% dplyr::summarise(total=n())
  ##calculate proportion of pass
  K_Schoolyear$Pass_Percent <- K_Schoolyear$pass_count/K_Schoolyear_1$total
  ##plot School_Year vs Pass %
  print(ggplot(data=K_Schoolyear,aes(x=SCHOOL_YEAR,y=Pass_Percent,group=1,colour="green"))+
          geom_line(aes(group=1),colour="#9933FF")+geom_point(aes(group=1),colour="#9933FF")+
          labs(title="Keystage2- School_Year vs Pass Percentage")+
          scale_y_continuous(labels=scales::percent)+
          theme(legend.position="none",axis.text=element_text(face="bold")))
  
  #5) Plot School_Year vs Pass %
  ##get pass count for each Quintile per School_Year
  K_Schoolyear <- stage_num%>% group_by(SCHOOL_YEAR,QUINTILE,CSI1,.drop=FALSE)%>%
    dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
  ##get total count for each Quintile per School_Year
  K_Schoolyear_1<- stage_num%>% group_by(SCHOOL_YEAR,QUINTILE,.drop=FALSE)%>% dplyr::summarise(total=n())
  ##calculate proportion of pass
  K_Schoolyear$Pass_Percent <- K_Schoolyear$pass_count/K_Schoolyear_1$total
  ##plot School_Year vs Pass %
  print(ggplot(data=K_Schoolyear,aes(x=SCHOOL_YEAR,y=Pass_Percent,group=QUINTILE,colour=QUINTILE))+
          geom_line()+geom_point()+labs(title="Keystage2- School_Year vs Pass Percentage")+
          scale_y_continuous(labels=scales::percent)+
          theme(axis.text=element_text(face="bold")))
  
  
#6)Plot Drug name vs Pass %
  ##get pass count for each Drug
  K_Drug_Name <- stage_num%>% group_by(DRUG_NAME,CSI1,.drop=FALSE)%>%
    dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
  ##get total count for Drug
  K_Drug_Name_1<- stage_num%>% group_by(DRUG_NAME,.drop=FALSE)%>% dplyr::summarise(total=n())
  ##calculate proportion of pass
  K_Drug_Name$Pass_Percent <- K_Drug_Name$pass_count/K_Drug_Name_1$total
  ##plot Drug name vs Pass %
  print(ggplot(data=K_Drug_Name,aes(y=reorder(DRUG_NAME,-Pass_Percent),x=Pass_Percent))+
          geom_bar(stat='identity',width=0.5,fill="#9933FF")+labs(y="DRUG_CODE",title="Keystage2- Drug_Name vs Pass Percentage")+
          scale_x_continuous(labels=scales::percent)+
          theme(legend.position="none",axis.text=element_text(face="bold")))
  
} 

#generate plots for key stage 2,3 & 4
plot_res_stage(Keystage2)
plot_res_stage(Keystage3)
plot_res_stage(Keystage4)





############################# case_ control data ########################
# Comparison of Exposed and Non exposed data for each key Stage.
#case control- key stage 2

#Importing data from DB2 into R, for statistical Analysis.
Case_control_key2 <- sqlQuery(channel, "select * FROM Sailw0895v.control_case_Key2 ")
# To explore data to understand statistical variable 
summary(Case_control_key2)

# To determine data type for each variable.
str(Case_control_key2)

#Binary encoding the out put Variable and Status variable ( children with Epilepsy(exposed group,
#Children without Epilepsy Non-Exposed group))
Case_control_key2 <- Case_control_key2 %>% mutate(CSI1=ifelse(CSI1=="Y",1,0))
Case_control_key2<- Case_control_key2 %>% mutate(STATUS=ifelse(STATUS=="EPILEPSY",1,0))

# encoding variables to perform logistic regression
Case_control_key2$CSI1 <- as.factor(Case_control_key2$CSI1)
Case_control_key2$STATUS <- as.factor(Case_control_key2$STATUS)
Case_control_key2$KEYSTAGE <- as.factor(Case_control_key2$KEYSTAGE)
Case_control_key2$QUINTILE <- as.factor(Case_control_key2$QUINTILE)
Case_control_key2$SCHOOL_YEAR <- as.factor(Case_control_key2$SCHOOL_YEAR)

# Running logistic regression for Key Stage 2
logis_key2 <- glm(CSI1 ~ STATUS+ QUINTILE + SCHOOL_YEAR +GNDR_CD, data=
                Case_control_key2, family=binomial)
# summary report after running logistic regression for Key Stage 2.Analysing the statistical Variables.
summary(logis_key2)



# case_ control --  key stage 3
#Importing data from DB2 into R, for statistical Analysis.
Case_control_key3 <- sqlQuery(channel, "select * FROM Sailw0895v.control_case_Key3 ")

# To explore data to understand statistical variabl
summary(Case_control_key3)

# To determine data type for each variable.
str(Case_control_key3)

#Binary encoding the out put Variable and Status variable ( children with Epilepsy(exposed group,
#Children without Epilepsy Non-Exposed group))
Case_control_key3 <- Case_control_key3 %>% mutate(CSI1=ifelse(CSI1=="Y",1,0))
Case_control_key3<- Case_control_key3 %>% mutate(STATUS=ifelse(STATUS=="EPILEPSY",1,0))

# encoding variables to perform logistic regression
Case_control_key3$CSI1 <- as.factor(Case_control_key3$CSI1)
Case_control_key3$STATUS <- as.factor(Case_control_key3$STATUS)
Case_control_key3$KEYSTAGE <- as.factor(Case_control_key3$KEYSTAGE)
Case_control_key3$QUINTILE <- as.factor(Case_control_key3$QUINTILE)
Case_control_key3$SCHOOL_YEAR <- as.factor(Case_control_key3$SCHOOL_YEAR)

# Running logistic regression for Key Stage 3
logis_key3 <- glm(CSI1 ~ STATUS+ QUINTILE + SCHOOL_YEAR +GNDR_CD, data=
                    Case_control_key3, family=binomial)

#summary report after running logistic regression for Key Stage 3.Analysing the statistical Variables.
summary(logis_key3)




# case_ control --  key stage 4
#Importing data from DB2 into R, for statistical Analysis.
Case_control_key4 <- sqlQuery(channel, "select * FROM Sailw0895v.control_case_Key4 ")

# To explore data to understand statistical variable 
summary(Case_control_key4)


# To determine data type for each variable.
str(Case_control_key4)

#Binary encoding the out put Variable and Status variable ( children with Epilepsy(exposed group,
#Children without Epilepsy Non-Exposed group))
Case_control_key4 <- Case_control_key4 %>% mutate(CSI1=ifelse(CSI1=="Y",1,0))
Case_control_key4<- Case_control_key4 %>% mutate(STATUS=ifelse(STATUS=="EPILEPSY",1,0))

#encoding variables to perform logistic regression
Case_control_key4$CSI1 <- as.factor(Case_control_key4$CSI1)
Case_control_key4$STATUS <- as.factor(Case_control_key4$STATUS)
Case_control_key4$KEYSTAGE <- as.factor(Case_control_key4$KEYSTAGE)
Case_control_key4$QUINTILE <- as.factor(Case_control_key4$QUINTILE)
Case_control_key4$SCHOOL_YEAR <- as.factor(Case_control_key4$SCHOOL_YEAR)

## Running logistic regression for Key Stage 3
logis_key4 <- glm(CSI1 ~ STATUS+ QUINTILE + SCHOOL_YEAR +GNDR_CD, data=
                    Case_control_key4, family=binomial)

# To explore data to understand statistical variable 
summary(logis_key4)




############# Function for case-control plots ############
plot_case_stage <- function(stage_num) {
  
#1) Plot Quintile vs Pass % 
  ##get pass count for each Quintile
  K_Quintile <- stage_num%>% group_by(STATUS,QUINTILE,CSI1)%>%
    dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
  ##get total count per Quintile
  K_Quintile_1<- stage_num%>% group_by(STATUS,QUINTILE)%>% dplyr::summarise(total=n())
  ##calculate proportion
  K_Quintile$Pass_Percent <- K_Quintile$pass_count/K_Quintile_1$total
  ##plot Quintile vs Pass % for Epilepsy & Non-epilepsy
  print(ggplot(data=K_Quintile,aes(x=QUINTILE,y=Pass_Percent,colour=as.factor(STATUS),group=STATUS))+
          geom_line()+geom_point()+
          labs(title="Keystage4 Case control- Quintile vs Pass Percentage")+
          scale_y_continuous(labels=scales::percent)+
          scale_colour_discrete(name="Epilepsy",labels=c("No","Yes")))
  
  
#2) Bar Plot- Pass ratio with respect to gender
  ##get pass count for gender
  K_Status <- stage_num%>% group_by(GNDR_CD,STATUS,CSI1,.drop=FALSE)%>%
    dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
  ##get total count per gender
  K_Status_1<- stage_num%>% group_by(GNDR_CD,STATUS,.drop=FALSE)%>% dplyr::summarise(total=n())
  ##calculate proportion
  K_Status$Pass_Percent <- K_Status$pass_count/K_Status_1$total
  ##plot gender vs Pass %
  print(ggplot(data=K_Status, aes(x=GNDR_CD,y=Pass_Percent,fill=STATUS))+
          geom_bar(stat='identity',width=0.3,position="fill") + 
          theme_classic() +
          labs(y="Percentage-Pass",x="Gender",title="Keystage4 Case control-Gender vs Pass Percentage")+
          scale_x_discrete(limit=c("1","2"),labels=c("Male","Female"))+
          scale_y_continuous(labels=scales::percent)+
          scale_fill_discrete(name="Epilepsy",labels=c("No","Yes")))

  
#3) Plot School year vs Pass % 
  ##get pass count for each School year
  K_Schoolyear <- stage_num%>% group_by(STATUS,SCHOOL_YEAR,CSI1,.drop=FALSE)%>%
    dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
  ##get total count per School year
  K_Schoolyear_1<- stage_num%>% group_by(STATUS,SCHOOL_YEAR,.drop=FALSE)%>% dplyr::summarise(total=n())
  ##calculate proportion
  K_Schoolyear$Pass_Percent <- K_Schoolyear$pass_count/K_Schoolyear_1$total
  ##plot School year vs Pass % for epilepsy & Non- epilepsy
  print(ggplot(data=K_Schoolyear,aes(x=SCHOOL_YEAR,y=Pass_Percent,colour=as.factor(STATUS),group=STATUS))+
          geom_line()+geom_point()+
        labs(title="Keystage Case control- School year vs Pass Percentage")+
          scale_y_continuous(labels=scales::percent)+ 
          scale_colour_discrete(name="Epilepsy",labels=c("No","Yes")))
  
} 

#generate plots for case-control- keystage 2,3 & 4
plot_case_stage(Case_control_key2)
plot_case_stage(Case_control_key3)
plot_case_stage(Case_control_key4)


