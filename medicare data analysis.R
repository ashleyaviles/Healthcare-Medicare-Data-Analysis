# Data Prep and Regressions
# Date: 12/5/2021


## Set Working Directory
setwd("/Users/ashleyaviles/Documents/ Healthcare Analytics/medicare data")


#### Clear Workspace and Load Packages ####
# Clear
rm(list=ls())

# Install new packages
pckg = rownames(installed.packages())
if(!"data.table" %in% pckg){
  install.packages("data.table")
}

# Load packages
library(data.table)

#### Read in Data ####
## Claims Data
ip_clms = read.csv("DE1_0_2008_to_2010_Inpatient_Claims_Sample_1.csv")
## Convert to data.table
ip_clms = as.data.table(ip_clms)
## Order data and set key
setkey(ip_clms,DESYNPUF_ID)

### member data ###
members_2010 = read.csv("DE1_0_2010_Beneficiary_Summary_File_Sample_1.csv")
members_2010 = as.data.table(members_2010)
setkey(members_2010,DESYNPUF_ID)


#### Calculate Spending Variables ####
## Total Cost for medicare - healthcare expenditure####
ip_clms$MEDICARE_PAY <- ip_clms$CLM_PMT_AMT+(ip_clms$CLM_PASS_THRU_PER_DIEM_AMT*ip_clms$CLM_UTLZTN_DAY_CNT)
## Total Cost for memeber - personal cost####
ip_clms$PERSON_COST<- ip_clms$NCH_BENE_IP_DDCTBL_AMT+ip_clms$NCH_BENE_PTA_COINSRNC_LBLTY_AM+
  ip_clms$NCH_BENE_BLOOD_DDCTBL_LBLTY_AM

##Recode chronic conditions & sex###
members_2010$SP_ALZHDMTA[members_2010$SP_ALZHDMTA == 2] <- 0
members_2010$SP_CHF[members_2010$SP_CHF == 2] <- 0
members_2010$SP_CHRNKIDN[members_2010$SP_CHRNKIDN == 2] <- 0
members_2010$SP_CNCR[members_2010$SP_CNCR == 2] <- 0
members_2010$SP_COPD[members_2010$SP_COPD == 2] <- 0
members_2010$SP_DEPRESSN[members_2010$SP_DEPRESSN == 2] <- 0
members_2010$SP_DIABETES[members_2010$SP_DIABETES == 2] <- 0
members_2010$SP_ISCHMCHT[members_2010$SP_ISCHMCHT == 2] <- 0
members_2010$SP_OSTEOPRS[members_2010$SP_OSTEOPRS == 2] <- 0
members_2010$SP_RA_OA[members_2010$SP_RA_OA == 2] <- 0
members_2010$SP_STRKETIA[members_2010$SP_STRKETIA == 2] <- 0
members_2010$BENE_SEX_IDENT_CD[members_2010$BENE_SEX_IDENT_CD == 2] <- 0

###create race dummy variables ###
members_2010$WHITE_MEM<- ifelse(members_2010$BENE_RACE_CD == 1, 1, 0)
members_2010$BLACK_MEM<- ifelse(members_2010$BENE_RACE_CD == 2, 1, 0)
members_2010$OTHER_MEM<- ifelse(members_2010$BENE_RACE_CD == 3, 1, 0)
members_2010$HISPANIC_MEM<- ifelse(members_2010$BENE_RACE_CD == 5, 1, 0)

## make count variable for number of comorbities###
members_2010$CHRONIC_CNT <- members_2010$SP_ALZHDMTA+members_2010$SP_CHF+members_2010$SP_CHRNKIDN+members_2010$SP_CNCR+members_2010$SP_COPD+members_2010$SP_DEPRESSN+
  members_2010$SP_DIABETES+members_2010$SP_ISCHMCHT+members_2010$SP_OSTEOPRS+members_2010$SP_RA_OA+members_2010$SP_STRKETIA

members_2010$CHRONIC_CNT2<- ifelse(members_2010$CHRONIC_CNT == 2, 1, 0)
members_2010$CHRONIC_CNT3<- ifelse(members_2010$CHRONIC_CNT == 3, 1, 0)
members_2010$CHRONIC_CNT4PLUS <- ifelse(members_2010$CHRONIC_CNT >=4, 1, 0)

### person with Diabetes and extra comorbities ###
### we want to know the number of chronic conditions without depression for indivuals w depres ###
members_2010$CHRONIC_CNT_WO_DEPRESS <- ifelse(members_2010$SP_DEPRESSN == 1, members_2010$SP_ALZHDMTA+members_2010$SP_CHF+members_2010$SP_CHRNKIDN+members_2010$SP_CNCR+members_2010$SP_COPD+members_2010$SP_DIABETES+members_2010$SP_ISCHMCHT+members_2010$SP_OSTEOPRS+members_2010$SP_RA_OA+members_2010$SP_STRKETIA, members_2010$CHRONIC_CNT_W0_DEPRESS == 0)
### we want to know the total number of chronic conditions for people with depression ##
members_2010$CHRONIC_CNT_W_DEPRESS <- ifelse(members_2010$SP_DEPRESSN == 1, members_2010$SP_ALZHDMTA+members_2010$SP_CHF+members_2010$SP_CHRNKIDN+members_2010$SP_CNCR+members_2010$SP_COPD+members_2010$SP_DEPRESSN+members_2010$SP_DIABETES+members_2010$SP_ISCHMCHT+members_2010$SP_OSTEOPRS+members_2010$SP_RA_OA+members_2010$SP_STRKETIA, members_2010$CHRONIC_CNT_W_DEPRESS == 0)

## we want to know the number of individuals with depression and chrnonic conditions #####
members_2010$CHRONIC_CNT_W_DEPRESS_YN <- ifelse(members_2010$CHRONIC_CNT_W0_DEPRESS >=1, 1, 0)

members_2010$CHRONIC_CNT_W_DEPRESS_YN[is.na(members_2010$CHRONIC_CNT_W_DEPRESS_YN)] <- 0

members_2010$CHRONIC_CNT_W_DEPRESS[is.na(members_2010$CHRONIC_CNT_W_DEPRESS)] <- 0
members_2010$CHRONIC_CNT_W0_DEPRESS[is.na(members_2010$CHRONIC_CNT_W0_DEPRESS)] <- 0


#### Remove unwanted variables #####
within(ip_clms, rm(HCPCS_CD_45,HCPCS_CD_44,HCPCS_CD_43,HCPCS_CD_42,HCPCS_CD_41,HCPCS_CD_40,HCPCS_CD_39,HCPCS_CD_38,HCPCS_CD_37,HCPCS_CD_36,
                   HCPCS_CD_35,HCPCS_CD_34,HCPCS_CD_33,HCPCS_CD_32,HCPCS_CD_31,HCPCS_CD_30,HCPCS_CD_29,HCPCS_CD_28,HCPCS_CD_27,HCPCS_CD_26,HCPCS_CD_25,
                   HCPCS_CD_24,HCPCS_CD_23,HCPCS_CD_22,HCPCS_CD_21,HCPCS_CD_20,HCPCS_CD_19,HCPCS_CD_18,HCPCS_CD_17,HCPCS_CD_16,HCPCS_CD_15,HCPCS_CD_14,
                   HCPCS_CD_13,HCPCS_CD_12,HCPCS_CD_11,HCPCS_CD_10,HCPCS_CD_9,HCPCS_CD_8,HCPCS_CD_7,HCPCS_CD_6,HCPCS_CD_5,HCPCS_CD_4,HCPCS_CD_3,HCPCS_CD_2,
                   HCPCS_CD_1))


#### Merge Membership and Claims Files w added spending varibles created above ####
full_medicare_data_3 = merge(members_2010,ip_clms,by="DESYNPUF_ID",all.x=FALSE)

##make a spending variable only for those treated with depression ####
full_medicare_data_3$Medicare_pay_DEPRESS <- ifelse(full_medicare_data_3$SP_DEPRESSN == 1, full_medicare_data_3$MEDICARE_PAY, full_medicare_data_3$Medicare_pay_DEPRESS == 0)
full_medicare_data_3$Person_Cost_DEPRESS <- ifelse(full_medicare_data_3$SP_DEPRESSN == 1, full_medicare_data_3$PERSON_COST, full_medicare_data_3$Person_Cost_DEPRESS == 0)

## spending for only those with depression and multplie comorbities ###
full_medicare_data_3$Medicare_pay_DEPRESS_N_CRHN <- ifelse(full_medicare_data_3$CHRONIC_CNT_W_DEPRESS_YN == 1, full_medicare_data_3$MEDICARE_PAY, full_medicare_data_3$Medicare_pay_DEPRESS_N_CRHN == 0)
full_medicare_data_3$Person_Cost_DEPRESS_N_CRHN <- ifelse(full_medicare_data_3$CHRONIC_CNT_W_DEPRESS_YN == 1, full_medicare_data_3$PERSON_COST, full_medicare_data_3$Person_Cost_DEPRESS_N_CRHN == 0)

##getting rid of NAs##
full_medicare_data_3$Medicare_pay_DEPRESS[is.na(full_medicare_data_3$Medicare_pay_DEPRESS)] <- 0
full_medicare_data_3$Person_Cost_DEPRESS[is.na(full_medicare_data_3$Person_Cost_DEPRESS)] <- 0
full_medicare_data_3$Medicare_pay_DEPRESS_N_CRHN[is.na(full_medicare_data_3$Medicare_pay_DEPRESS_N_CRHN)] <- 0
full_medicare_data_3$Person_Cost_DEPRESS_N_CRHN [is.na(full_medicare_data_3$Person_Cost_DEPRESS_N_CRHN)] <- 0

##exporting data###
#write_csv(full_medicare_data_3, path = "full_medicare_data_3.1.csv")

library(jtools)
install.packages('remotes')
remotes::install_github('hughjonesd/huxtable')
##Regressions##

##Predict medicare spending##
depression_medicarepay_regress <-lm(MEDICARE_PAY ~ SP_DEPRESSN, data = full_medicare_data_3)
summ(depression_medicarepay_regress)

##Predict medicare spending##
depression_medicarepay_regresswithDEPRESS <-lm(Medicare_pay_DEPRESS ~ SP_DEPRESSN, data = full_medicare_data_3)
summ(depression_medicarepay_regresswithDEPRESS)

##Predict member spending spending##
depression_personcost_regress <-lm(PERSON_COST ~ SP_DEPRESSN, data = full_medicare_data_3)
summ(depression_personcost_regress)

##spedning with chronic condistions and depression##
depressNchronic_medicarepay_regress <-lm(MEDICARE_PAY ~ CHRONIC_CNT_W_DEPRESS_YN, data = full_medicare_data_3)
summ(depressNchronic_medicarepay_regress)

depressNchronic_personcost_regress <-lm(PERSON_COST ~ CHRONIC_CNT_W_DEPRESS_YN, data = full_medicare_data_3)
summ(depressNchronic_personcost_regress)

##spedning with chronic condistions and depression with only depress spending variables##
depressNchronic_medicarepay_regressWDEPRESS <-lm(Medicare_pay_DEPRESS ~ CHRONIC_CNT_W_DEPRESS_YN, data = full_medicare_data_3)
summ(depressNchronic_medicarepay_regressWDEPRESS )

depressNchronic_personcost_regressWDEPRESS <-lm(Person_Cost_DEPRESS ~ CHRONIC_CNT_W_DEPRESS_YN, data = full_medicare_data_3)
summ(depressNchronic_personcost_regressWDEPRESS)

##depression regression with utlization ###
depress_utlization <- lm(CLM_UTLZTN_DAY_CNT ~ SP_DEPRESSN, data = full_medicare_data_3)
summ(depress_utlization)

depressNCHRONIC_utlization <- lm(CLM_UTLZTN_DAY_CNT ~ CHRONIC_CNT_W_DEPRESS_YN, data = full_medicare_data_3)
summ(depressNCHRONIC_utlization)


##Predict member spending spending with only depsress person cost##
depression_personcost_regresswithDEPRESSPAY <-lm(Person_Cost_DEPRESS ~ SP_DEPRESSN, data = full_medicare_data_3)
summ(depression_personcost_regresswithDEPRESSPAY)


##which factors influence medicare pay the most ###
MedicarePay_Multivarite <- lm(MEDICARE_PAY ~ SP_DEPRESSN+SP_ALZHDMTA+SP_CHF+SP_CHRNKIDN+SP_CNCR+SP_COPD+SP_DIABETES+SP_ISCHMCHT+
                             SP_OSTEOPRS+SP_RA_OA+SP_STRKETIA+BENE_SEX_IDENT_CD+WHITE_MEM+BLACK_MEM+OTHER_MEM+HISPANIC_MEM+
                             CHRONIC_CNT2+CHRONIC_CNT3+CHRONIC_CNT4PLUS, data=full_medicare_data_3)
summ(MedicarePay_Multivarite)
summary(MedicarePay_Multivarite)

PersonCost_Multivarite <- lm(PERSON_COST ~ SP_DEPRESSN+SP_ALZHDMTA+SP_CHF+SP_CHRNKIDN+SP_CNCR+SP_COPD+SP_DIABETES+SP_ISCHMCHT+
                                SP_OSTEOPRS+SP_RA_OA+SP_STRKETIA+BENE_SEX_IDENT_CD+WHITE_MEM+BLACK_MEM+OTHER_MEM+HISPANIC_MEM+
                                CHRONIC_CNT2+CHRONIC_CNT3+CHRONIC_CNT4PLUS, data=full_medicare_data_3)
summ(PersonCost_Multivarite)
summary(PersonCost_Multivarite)

MedicarePay_CHRN_Multivarite <- lm(MEDICARE_PAY ~ SP_DEPRESSN+SP_ALZHDMTA+SP_CHF+SP_CHRNKIDN+SP_CNCR+SP_COPD+SP_DIABETES+SP_ISCHMCHT+
                                SP_OSTEOPRS+SP_RA_OA+SP_STRKETIA, data=full_medicare_data_3)
summ(MedicarePay_CHRN_Multivarite)

MedicarePay_DEMOG_Multivarite <- lm(MEDICARE_PAY ~ BENE_SEX_IDENT_CD+WHITE_MEM+BLACK_MEM+OTHER_MEM+HISPANIC_MEM, data=full_medicare_data_3)
summ(MedicarePay_DEMOG_Multivarite)
summary(MedicarePay_DEMOG_Multivarite)

MedicarePay_CHRNCNT_Multivarite <- lm(MEDICARE_PAY ~ CHRONIC_CNT2+CHRONIC_CNT3+CHRONIC_CNT4PLUS, data=full_medicare_data_3)
summ(MedicarePay_CHRNCNT_Multivarite)
summary(MedicarePay_CHRNCNT_Multivarite)

## Predict Total Spending
# Create Formula
##formula = PERSON_ALLOWED ~ ADG01+ADG02+ADG03+ADG04+ADG05+ADG06+ADG07+ADG08+ADG09+ 
#  ADG10+ADG11+ADG12+ADG13+ADG14+ADG15+ADG16+ADG17+ADG18+ADG19+ADG20+
#  ADG21+ADG22+ADG23+ADG24+ADG25+ADG26+ADG27+ADG28+ADG29+ADG30+
#  ADG31+ADG30+ADG32+ADG31+ADG32+ADG33+ADG34+
#  AGE_CAT+REGION+FEMALE

# Run Regression and Summarize Results
##res_total = lm(formula,data=full_data)
## summary(res_total)

##relationship
##library(ggplot2)

###ggplot(full_data, aes(ADG25, PERSON_COSTSHR)) + geom_smooth(method = "lm") + geom_point() + ggtitle("linear fit")

## the interaction of having mental health related ADGs and 1 vs 8 MDC

## Predict Total Spending - Using Logs
full_data[,log_ALLOWED:=log(PERSON_ALLOWED+.001)]
# Create Formula
formula = log_ALLOWED ~ ADG01+ADG02+ADG03+ADG04+ADG05+ADG06+ADG07+ADG08+ADG09+ 
  ADG10+ADG11+ADG12+ADG13+ADG14+ADG15+ADG16+ADG17+ADG18+ADG19+ADG20+
  ADG21+ADG22+ADG23+ADG24+ADG25+ADG26+ADG27+ADG28+ADG29+ADG30+
  ADG31+ADG30+ADG32+ADG31+ADG32+ADG33+ADG34+
  AGE_CAT+REGION+FEMALE

# Run Regression and Summarize Results
res_log = lm(formula,data=full_data)
summary(res_log)

## relationship



## Predict Significant Health Care Use
full_data[,large_Expense:=0]
full_data[PERSON_ALLOWED>1000,large_Expense:=1]
# Create Formula
formula = large_Expense ~ ADG01+ADG02+ADG03+ADG04+ADG05+ADG06+ADG07+ADG08+ADG09+ 
  ADG10+ADG11+ADG12+ADG13+ADG14+ADG15+ADG16+ADG17+ADG18+ADG19+ADG20+
  ADG21+ADG22+ADG23+ADG24+ADG25+ADG26+ADG27+ADG28+ADG29+ADG30+
  ADG31+ADG30+ADG32+ADG31+ADG32+ADG33+ADG34+
  AGE_CAT+REGION+FEMALE

# Run a Linear Regression Model 
res_linear = lm(formula,data=full_data)
summary(res_linear)

### relationship


# Run a Probit Regression Model 
res_probit = glm(formula,data=full_data,family=binomial(link="probit"))
summary(res_probit)

# Run a Logit Regression Model
res_logit = glm(formula,data=full_data,family=binomial(link="logit"))
summary(res_logit)
