# **Research Misconduct_Iran**
        
## *Rscript*

#############################################################################################

# Omitting NA values within data frames:
smq_r <- na.omit(Codedresults_smq_r2)

ppq <- na.omit(Codedresults_ppq)

Gender_unitype <- table(SMQ_R$Demographic1, SMQ_R$Demographic3)

# First contingency table:
Degree_unitype <- table(SMQ_R$Demographic8, SMQ_R$Demographic3)

# Tables:
Unitype_Gender_table <- table(Codedresults_smq_r2$Gender, Codedresults_smq_r2$Unitype)

Unitype_hindex_table <- table(Codedresults_smq_r2$Hindex, Codedresults_smq_r2$Unitype)

Gender_table <- table(Codedresults_smq_r2$Gender)

Degrees_table <- table(Codedresults_smq_r2$Academ_degree)

Unitype_table <- table(Codedresults_smq_r2$Unitype)

Clinical_exp_table <- table(Codedresults_smq_r2$Clinical_practice)

Unitype_chancellery_table <- table(Codedresults_smq_r2$Vicechanc_exp,Codedresults_smq_r2$Unitype)

unitype_conference_table <- table(Codedresults_smq_r2$No_conference, Codedresults_smq_r2$Unitype)

awareness_misconduct_table <- table(Codedresults_smq_r2$`Awareness of misconduct 3`)

mean_age_unitype_table <- table(mean_age_unitype1, mean_age_unitype2, mean_age_unitype3)

# Frequencies(proportions):
prop.table(Degrees_table)

prop.table(Gender_table)

prop.table(Gender_table)

prop.table(Clinical_exp_table)

# Means of  interest:
mean_age_total <- mean(Codedresults_smq_r2$Age, na.rm = TRUE)

mean_age_unitype1 <- mean(Codedresults_smq_r2$Age[Codedresults_smq_r2$Unitype==1], na.rm = TRUE)

mean_age_unitype2 <- mean(Codedresults_smq_r2$Age[Codedresults_smq_r2$Unitype==2], na.rm = TRUE)

mean_age_unitype3 <- mean(Codedresults_smq_r2$Age[Codedresults_smq_r2$Unitype==3], na.rm = TRUE)

mean_tenure_unitype1 <- mean(Codedresults_smq_r2$Tenure_exp[Codedresults_smq_r2$Unitype==1], na.rm = TRUE)

mean_tenure_unitype2 <- mean(Codedresults_smq_r2$Tenure_exp[Codedresults_smq_r2$Unitype==2], na.rm = TRUE)

mean_tenure_unitype3 <- mean(Codedresults_smq_r2$Tenure_exp[Codedresults_smq_r2$Unitype==3], na.rm = TRUE)

mean_tenure_total <- mean(Codedresults_smq_r2$Tenure_exp, na.rm = TRUE)

mean_hindex_total <- mean(Codedresults_smq_r2$Hindex)

mean_hindex_uni1 <- mean(Codedresults_smq_r2$Hindex[Codedresults_smq_r2$Unitype==1], na.rm = TRUE)

mean_hindex_uni2 <- mean(Codedresults_smq_r2$Hindex[Codedresults_smq_r2$Unitype==2], na.rm = TRuE)

mean_hindex_uni3 <- mean(Codedresults_smq_r2$Hindex[Codedresults_smq_r2$Unitype==3], na.rm = TRUE)

mean_hindex_total <- mean(smq_r$Hindex)

mean_chancellery_uni1 <- mean(smq_r$Vicechanc_exp[smq_r$Unitype==1])

mean_chancellery_uni2 <- mean(smq_r$Vicechanc_exp[smq_r$Unitype==2])

mean_chancellery_uni3 <- mean(smq_r$Vicechanc_exp[smq_r$Unitype==3])

mean_chancellery_total <- mean(smq_r$Vicechanc_exp)

mean_conference_uni1 <- mean(smq_r$No_conference[smq_r$Unitype==1])

mean_conference_uni2 <- mean(smq_r$No_conference[smq_r$Unitype==2])

mean_conference_uni3 <- mean(smq_r$No_conference[smq_r$Unitype==3])

mean_conference_total <- mean(smq_r$No_conference)

# Medians and quantiles of interest:
median_chancellery_total <- median(Codedresults_smq_r2$Vicechanc_exp , na.rm = TRUE)

median_conference_total <- median(Codedresults_smq_r2$No_conference, na.rm = TRUE)

median_tenure_total <- median(Codedresults_smq_r2$Tenure_exp, na.rm = TRUE)

median_conference_unitype1 <- median(unitype_conference_table[,1])

median_conference_unitype2 <- median(unitype_conference_table[,2])

median_conference_unitype3 <- median(unitype_conference_table[,3])

median_chancellery_unitype1 <- median(Unitype_chancellery_table[,1])

median_chancellery_unitype2 <- median(Unitype_chancellery_table[,2])

median_chancellery_unitype3 <- median(Unitype_chancellery_table[,3])

quantile(CodedresultsTenureCat$Understanding)

quantile(CodedresultsTenureCat$Perception1)

quantile(CodedresultsTenureCat$Perception2)

quantile(CodedresultsTenureCat$Perception3)

quantile(CodedresultsTenureCat$Perception4)

quantile(CodedresultsTenureCat$Perception5)

quantile(CodedresultsTenureCat$Perception6)

quantile(CodedresultsTenureCat$Perception7)

quantile(CodedresultsTenureCat$Perception8)

quantile(CodedresultsTenureCat$Perception9)

quantile(CodedresultsTenureCat$Perception10)

quantile(CodedresultsTenureCat$Perception11)

quantile(CodedresultsTenureCat$Perception12)

quantile(CodedresultsTenureCat$Prevalence1)

quantile(CodedresultsTenureCat$Prevalence2)

quantile(CodedresultsTenureCat$Prevalence3)

quantile(CodedresultsTenureCat$Prevalence4)

quantile(CodedresultsTenureCat$Prevalence5)

quantile(CodedresultsTenureCat$Prevalence6)

quantile(CodedresultsTenureCat$Prevalence7)

quantile(CodedresultsTenureCat$Prevalence8)

quantile(CodedresultsTenureCat$Reporting1)

quantile(CodedresultsTenureCat$Reporting2)

quantile(CodedresultsTenureCat$Reporting3)

quantile(CodedresultsTenureCat$Beliefs1)

quantile(CodedresultsTenureCat$Beliefs2)

quantile(CodedresultsTenureCat$Beliefs3)

quantile(CodedresultsTenureCat$Beliefs4)

quantile(CodedresultsTenureCat$Beliefs5)

quantile(CodedresultsTenureCat$Behavioral1)

quantile(CodedresultsTenureCat$Behavioral2)

quantile(CodedresultsTenureCat$Behavioral3)

quantile(CodedresultsTenureCat$Behavioral4)

quantile(CodedresultsTenureCat$Behavioral5)

quantile(CodedresultsTenureCat$Behavioral6)

quantile(CodedresultsTenureCat$Behavioral7)

quantile(CodedresultsTenureCat$Behavioral8)

quantile(CodedresultsTenureCat$Behavioral1)

quantile(CodedresultsTenureCat$PP1)

quantile(CodedresultsTenureCat$PP2)

quantile(CodedresultsTenureCat$PP3)

quantile(CodedresultsTenureCat$PP4)

quantile(CodedresultsTenureCat$PP5)

quantile(CodedresultsTenureCat$PP6)

quantile(CodedresultsTenureCat$PP7)

quantile(CodedresultsTenureCat$PP8)

quantile(CodedresultsTenureCat$PP9)

quantile(CodedresultsTenureCat$PP10)

quantile(CodedresultsTenureCat$PP11)

quantile(CodedresultsTenureCat$PP12)

quantile(CodedresultsTenureCat$PP13)

quantile(CodedresultsTenureCat$PP14)

# to be able to calculate the corrected estimator version of standard deviation in R, we are going to 
# multiply whatever result that R gives us (uncorrected estimator) by sqrt((N-1)/N), with N being the 
# length of our vector (sample size). This is easily done by defining a function for the new and corrected sd:
sd_corrected <- function(x, na.rm = TRUE){
      newsd <-  sd(x) * sqrt((length(x)-1)/length(x))
      return(newsd)
}

sd_hindex <- sd_corrected(smq_r$Hindex)

sd_age_uni1 <- sd_corrected(smq_r$Age[smq_r$Unitype==1])

sd_age_uni2 <- sd_corrected(smq_r$Age[smq_r$Unitype==2])

sd_age_uni3 <- sd_corrected((smq_r$Age[smq_r$Unitype==3]))

sd_age_total <- sd_corrected(smq_r$Age)

sd_hindex_uni1 <- sd_corrected(smq_r$Hindex[smq_r$Unitype==1])

sd_hindex_uni2 <- sd_corrected(smq_r$Hindex[smq_r$Unitype==2])

sd_hindex_uni3 <- sd_corrected(smq_r$Hindex[smq_r$Unitype==3])

sd_hindex_total <- sd_corrected(smq_r$Hindex)

sd_tenure_uni1 <- sd_corrected(smq_r$Tenure_exp[smq_r$Unitype==1])

sd_tenure_uni2 <- sd_corrected(smq_r$Tenure_exp[smq_r$Unitype==2])

sd_tenure_uni3 <- sd_corrected(smq_r$Tenure_exp[smq_r$Unitype==3])

sd_tenure_total <- sd_corrected(smq_r$Tenure_exp)

sd_chancellery_uni1 <- sd_corrected((smq_r$Vicechanc_exp[smq_r$Unitype==1]))

sd_chancellery_uni2 <- sd_corrected((smq_r$Vicechanc_exp[smq_r$Unitype==2]))

sd_chancellery_uni3 <- sd_corrected((smq_r$Vicechanc_exp[smq_r$Unitype==3]))

sd_chancellery_total <- sd_corrected(smq_r$Vicechanc_exp)

sd_conference_uni1 <- sd_corrected(smq_r$No_conference[smq_r$Unitype==1])

sd_conference_uni2 <- sd_corrected(smq_r$No_conference[smq_r$Unitype==2])

sd_conference_uni3 <- sd_corrected(smq_r$No_conference[smq_r$Unitype==3])

sd_conference_total <- sd_corrected(smq_r$No_conference)

# Ranges for desired variables:
range(Codedresults_smq_r2$Hindex, na.rm = TRUE)

range(Codedresults_smq_r2$Age, na.rm = TRUE)

range(Codedresults_smq_r2$Tenure_exp, na.rm = TRUE)

range(smq_r$Vicechanc_exp)

range(smq_r$No_conference)

## Summary tables for demographic data and the results of questionnaires:
library(broom)
library(tidyr)
library(dplyr)
pkgs <- c("dplyr", "tidyr", "broom")
install.packages(pkgs)
saplly(pkgs, require, character.only = T)

sumstat <- Codedresults_smq_r2 %>%
        select('Gender(%)' = Gender, 
               'Age(%)' = Age) 
        
write.table(Unitype_Gender_table, file='unitype_gender.txt', sep = ",", quote = FALSE)

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(Codedresults_smq_r2, row.vars = "Unitype", col.vars = "Gender", type = "f" )
crosstab(Codedresults_smq_r2, row.vars = "Unitype", col.vars = c("Gender", "Age", ""))

## the sort = "dec" is used to sort the data in a descending order (from bottom to top).
library(questionr)
questionr::freq(Codedresults_smq_r2$RM_perception, cum = TRUE, sort = "dec", total = TRUE)

questionr::freq(Codedresults_smq_r2$`Perception
                + of workplace environment1`, cum = TRUE, sort = "dec", total = TRUE)

questionr:: freq(Codedresults_smq_r2$`Perception
                 + of workplace environment2`, cum = TRUE, total = TRUE)

questionr::freq(Codedresults_smq_r2$`Perception
                + of workplace environment3`, cum = TRUE, total = TRUE)

questionr::freq(Codedresults_smq_r2$`Perception
                + of workplace environment4`, cum = TRUE, total = TRUE)

questionr::freq(Codedresults_smq_r2$`Perception
                + of workplace environment5`, cum = TRUE, total = TRUE)

questionr:: freq(Codedresults_smq_r2$`Perception
                 + of workplace environment6`, cum = TRUE, total = TRUE)

questionr::freq(Codedresults_smq_r2$`Perception
                + of workplace environment7`, cum = TRUE, total = TRUE)

questionr::freq(Codedresults_smq_r2$`Perception
                + of workplace environment8`, cum = TRUE, total = TRUE)

questionr::freq(Codedresults_smq_r2$`Perception
                + of workplace environment9`, cum = TRUE, total = TRUE)

questionr::freq(Codedresults_smq_r2$`Perception
                + of workplace environment10`, cum = TRUE, total = TRUE)

questionr::freq(Codedresults_smq_r2$`Perception
                + of workplace environment11`, cum = TRUE, total = TRUE)

questionr:: freq(Codedresults_smq_r2$`Perception
                 + of workplace environment12`, cum = TRUE, total = TRUE)

questionr::freq(Codedresults_smq_r2$`Prevalence
                + of scientific misconduct1`, cum = TRUE, total = TRUE)

questionr::freq(Codedresults_smq_r2$`Prevalence
                + of scientific misconduct2`, cum = TRUE, total = TRUE)

questionr::freq(Codedresults_smq_r2$`Prevalence
                + of scientific misconduct3`, cum = TRUE, total = TRUE)

questionr::freq(Codedresults_smq_r2$`Prevalence
                + of scientific misconduct4`, cum = TRUE, total = TRUE)

questionr::freq(Codedresults_smq_r2$`Prevalence
                + of scientific misconduct5`, cum = TRUE, total = TRUE)

questionr::freq(Codedresults_smq_r2$`Prevalence
                + of scientific misconduct6`, cum = TRUE, total = TRUE)

questionr::freq(Codedresults_smq_r2$`Prevalence
                + of scientific misconduct7`, cum = TRUE, total = TRUE)

questionr::freq(Codedresults_smq_r2$`Prevalence
                + of scientific misconduct8`, cum = TRUE, total = TRUE)

questionr::freq(Codedresults_smq_r2$`Awareness of misconduct 1`, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_smq_r2$`Awareness of misconduct 2`, cum = TRUE, total = TRUE)


questionr::freq(first_hand_knowledge_allocated$`Awareness of misconduct 3`, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_smq_r2$`reporting research misconduct1`, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_smq_r2$`reporting research misconduct2`, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_smq_r2$`reporting research misconduct3`, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_smq_r2$`beliefs about research misconduct1`, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_smq_r2$`beliefs about research misconduct2`, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_smq_r2$`beliefs about research misconduct3`, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_smq_r2$`beliefs about research misconduct4`, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_smq_r2$`beliefs about research misconduct5`, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_smq_r2$`Behavioral Influences1`, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_smq_r2$`Behavioral Influences2`, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_smq_r2$`Behavioral Influences3`, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_smq_r2$`Behavioral Influences4`, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_smq_r2$`Behavioral Influences5`, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_smq_r2$`Behavioral Influences6`, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_smq_r2$`Behavioral Influences7`, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_smq_r2$`Behavioral Influences8`, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_ppq$Publicationpressure1, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_ppq$Publicationpressure2, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_ppq$Publicationpressure3, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_ppq$Publicationpressure4, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_ppq$Publicationpressure5, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_ppq$Publicationpressure6, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_ppq$Publicationpressure7, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_ppq$Publicationpressure8, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_ppq$Publicationpressure9, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_ppq$Publicationpressure10, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_ppq$Publicationpressure11, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_ppq$Publicationpressure12, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_ppq$Publicationpressure13, cum = TRUE, total = TRUE)


questionr::freq(Codedresults_ppq$Publicationpressure14, cum = TRUE, total = TRUE)


apply(Codedresults_smq_r2, 2, mean)

apply(Codedresults_smq_r2, 2, sd_corrected, na.rm = TRUE)

apply(Codedresults_ppq, 2, mean)

apply(Codedresults_ppq, 2, sd_corrected, na.rm = TRUE)

## Adding new columns(variables) as per need:
CodedresultsTenureCat <- Codedresults %>%
        mutate(TenureCat = case_when(Tenure<=5~1,
                                     Tenure>5 & Tenure<=10~2,
                                     Tenure>10 & Tenure<=15~3,
                                     Tenure>15~4))
CodedresultsTenureCat <- CodedresultsTenureCat %>%
        mutate(PressExtFund = case_when(Behavioral1==1~1,
                                        Behavioral1==2~1,
                                        Behavioral1==3~2))

CodedresultsTenureCat <- CodedresultsTenureCat %>%
        mutate(GettingCaughtLow = case_when(Perception5==1~1,
                                            Perception5==2~2,
                                            Perception5==3~2,
                                            Perception5==4~2))
CodedresultsTenureCat <- CodedresultsTenureCat %>%
        mutate(PenaltySeverityLow = case_when(Perception4==1~1,
                                              Perception4==2~2,
                                              Perception4==3~2,
                                              Perception4==4~2))
CodedresultsTenureCat <- CodedresultsTenureCat %>%
        mutate(PrevalenceViolationNever = case_when(Prevalence4==1~1,
                                              Prevalence4==2~2,
                                              Prevalence4==3~2,
                                              Prevalence4==4~2))
CodedresultsTenureCat <- CodedresultsTenureCat %>%
        mutate(OrganizationEffectHigh = case_when(Perception12==1~1,
                                                  Perception12==2~1,
                                                  Perception12==3~2,
                                                  Perception12==4~2))

CodedresultsTenureCat <- CodedresultsTenureCat %>%
        mutate(HigherQualityNopp = case_when(PP1==1~1,
                                             PP1==2~1,
                                             PP1==3~1,
                                             PP1==4~2,
                                             PP1==5~2))
CodedresultsTenureCat <- CodedresultsTenureCat %>%
        mutate(StimulatingppHigh = case_when(PP4==1~1,
                                             PP4==2~1,
                                             PP4==3~1,
                                             PP4==4~2,
                                             PP4==5~2))

## Contingency tables, chi-square test and summarized info
## Tenure and plagiarism:
table_tenure_plagiarism <- table(CodedresultsTenureCat$TenureCat, CodedresultsTenureCat$Prevalence1)
chisq.test(table_tenure_plagiarism)
group_by(CodedresultsTenureCat, TenureCat) %>% summarise(count=n(),
                                                         mean=mean(Prevalence1, na.rm = T),
                                                         sd=sd(Prevalence1, na.rm = T))

## unitype and plagiarism:
table_unitype_plagiarism <- table(CodedresultsTenureCat$Unitype, CodedresultsTenureCat$Prevalence1)
chisq.test(table_unitype_plagiarism)                                 
group_by(CodedresultsTenureCat, Unitype) %>% summarize(count=n(),
                                                       mean=mean(Prevalence1, na.rm = T),
                                                       sd=sd(Prevalence1, na.rm = T))
## tenure and first hand knowledge of misconduct:
table_tenure_firsthandknowledge <- table(CodedresultsTenureCat$TenureCat, CodedresultsTenureCat$Awareness3)
chisq.test(table_tenure_firsthandknowledge) 
group_by(CodedresultsTenureCat, TenureCat) %>% summarize(count=n(),
                                                         mean=mean(Awareness3, na.rm = T),
                                                         sd=sd(Awareness3, na.rm = T))
## Unitype and first hand knowledge of misconduct:
table_unitype_firsthandknowledge <- table(CodedresultsTenureCat$Unitype, CodedresultsTenureCat$Awareness3)
chisq.test(table_unitype_firsthandknowledge)
group_by(CodedresultsTenureCat, Unitype) %>% summarize(count=n(),
                                                       mean=mean(Awareness3, na.rm = T),
                                                       sd=sd(Awareness3, na.rm = T))
## Unitype and pressure for external funding:
table_unitype_externalfunding <- table(CodedresultsTenureCat$Unitype, CodedresultsTenureCat$Behavioral2)
chisq.test(table_unitype_externalfunding)
group_by(CodedresultsTenureCat, Unitype) %>% summarize(count=n(),
                                                       mean=mean(Behavioral2, na.rm = T),
                                                       sd=sd(Behavioral2, na.rm = T))
## Tenure and pressure for external funding:
table_tenure_externalfunding <- table(CodedresultsTenureCat$TenureCat, CodedresultsTenureCat$Behavioral2)
chisq.test(table_tenure_externalfunding)
group_by(CodedresultsTenureCat, TenureCat) %>% summarize(count=n(),
                                                         mean=mean(Behavioral2, na.rm = T),
                                                         sd=sd(Behavioral2, na.rm = T))
## Tenure and effectiveness of institutional policy in reducing misconduct:
table_tenure_effectivenessofpolicy <- table(CodedresultsTenureCat$TenureCat, CodedresultsTenureCat$Perception12)
chisq.test(table_tenure_effectivenessofpolicy)
group_by(CodedresultsTenureCat, TenureCat) %>% summarize(count=n(),
                                                         mean=mean(Perception12, na.rm = T),
                                                         sd=sd(Perception12, na.rm = T))
## Unitype and and effectiveness of institutional policy in reducing misconduct:
table_unitype_effectivenessofpolicy <- table(CodedresultsTenureCat$Unitype, CodedresultsTenureCat$Perception12)
chisq.test(table_unitype_effectivenessofpolicy)
group_by(CodedresultsTenureCat, Unitype) %>% summarize(count=n(),
                                                       mean=mean(Perception12, na.rm = T),
                                                       sd=sd(Perception12, na.rm = T))
## First hand knowledge and effectiveness of institutional policy in reducing misconduct:
table_firsthandknowledge_effectivenessofpolicy <- table(CodedresultsTenureCat$Awareness3, CodedresultsTenureCat$Perception12)
chisq.test(table_firsthandknowledge_effectivenessofpolicy)
group_by(CodedresultsTenureCat, Awareness3) %>% summarize(count=n(),
                                                          mean=mean(Perception12, na.rm = T),
                                                          sd=sd(Perception12, na.rm = T))
## Unitype and chances of getting caught:
table_unitype_gettingcaught <- table(CodedresultsTenureCat$Unitype, CodedresultsTenureCat$Perception5)
chisq.test(table_unitype_gettingcaught)
group_by(CodedresultsTenureCat, Unitype) %>% summarize(count=n(),
                                                       mean=mean(Perception5, na.rm = T),
                                                       sd=sd(Perception5, na.rm = T))
## Tenure and chances of getting caught:
table_tenure_gettingcaught <- table(CodedresultsTenureCat$TenureCat, CodedresultsTenureCat$Perception5)
chisq.test(table_tenure_gettingcaught)
group_by(CodedresultsTenureCat, TenureCat) %>% summarize(count=n(),
                                                         mean=mean(Perception5, na.rm = T),
                                                         sd=sd(Perception5, na.rm = T))
## Unitype and penalty severity:
table_unitype_penaltyseverity <- table(CodedresultsTenureCat$Unitype, CodedresultsTenureCat$Perception4)
chisq.test(table_unitype_penaltyseverity)
group_by(CodedresultsTenureCat, Unitype) %>% summarize(count=n(),
                                                       mean=mean(Perception4, na.rm = T),
                                                       sd=sd(Perception4, na.rm =T))
## Unitype and penalty severity (low):
table_unitype_penaltyseverity_low <- table(CodedresultsTenureCat$Unitype, CodedresultsTenureCat$PenaltySeverityLow)
chisq.test(table_unitype_penaltyseverity_low)
group_by(CodedresultsTenureCat, Unitype) %>% summarize(count=n(),
                                                       mean=mean(PenaltySeverityLow, na.rm=T),
                                                       sd=sd(PenaltySeverityLow, na.rm=T))
## Tenure and penalty severity:
table_tenure_penaltyseverity <- table(CodedresultsTenureCat$TenureCat, CodedresultsTenureCat$Perception4)
chisq.test(table_tenure_penaltyseverity)
group_by(CodedresultsTenureCat, TenureCat) %>% summarize(count=n(),
                                                         mean=mean(Perception4, na.rm = T),
                                                         sd=sd(Perception4, na.rm = T))
## Tenure and penalty severity (low):
table_tenure_penaltyseverity_low <- table(CodedresultsTenureCat$TenureCat, CodedresultsTenureCat$PenaltySeverityLow)
chisq.test(table_tenure_penaltyseverity_low)
group_by(CodedresultsTenureCat, TenureCat) %>% summarize(count=n(),
                                                      mean=mean(PenaltySeverityLow, na.rm = T),
                                                      sd=sd(PenaltySeverityLow, na.rm = T))
## First-hand knowledge and violation prevalence:
table_firsthandknowledge_violationprevalence <- table(CodedresultsTenureCat$Awareness3, CodedresultsTenureCat$Prevalence4)
chisq.test(table_firsthandknowledge_violationprevalence)
group_by(CodedresultsTenureCat, Awareness3) %>% summarize(count=n(),
                                                          mean=mean(Prevalence4, na.rm = T),
                                                          sd=sd(Prevalence4, na.rm = T))
## Organization effectiveness and coordinator reporting:
table_organizationaleffectiveness_coordinatorreport <- table(CodedresultsTenureCat$OrganizationEffectHigh, CodedresultsTenureCat$Reporting1)
chisq.test(table_organizationaleffectiveness_coordinatorreport)
group_by(CodedresultsTenureCat, OrganizationEffectHigh) %>% summarize(count=n(),
                                                                      mean=mean(Reporting1, na.rm = T),
                                                                      sd=sd(Reporting1, na.rm = T))
## First-hand knowledge and coordinator reporting:
table_firsthandknowledge_coordinatorreport <- table(CodedresultsTenureCat$Awareness3, CodedresultsTenureCat$Reporting1)
chisq.test(table_firsthandknowledge_coordinatorreport)
group_by(CodedresultsTenureCat, Awareness3) %>% summarize(count=n(),
                                                          mean=mean(Reporting1, na.rm = T),
                                                          sd=sd(Reporting1, na.rm = T))
## Tenure and higher quality of scientific productions if no publication pressure existed:
table_tenure_higherqualitynopp <- table(CodedresultsTenureCat$TenureCat, CodedresultsTenureCat$PP1)
chisq.test(table_tenure_higherqualitynopp)
group_by(CodedresultsTenureCat, TenureCat) %>% summarize(count=n(),
                                                         mean=mean(PP1, na.rm = T),
                                                         sd=sd(PP1, na.rm = T))
## Unitype and higher quality of scientific productions if no publication pressure existed:
table_unitype_higherqualitynopp <- table(CodedresultsTenureCat$Unitype, CodedresultsTenureCat$PP1)
chisq.test(table_unitype_higherqualitynopp)
group_by(CodedresultsTenureCat, Unitype) %>% summarize(count=n(),
                                                       mean=mean(PP1, na.rm = T),
                                                       sd=sd(PP1, na.rm = T))
## Unitype and higher quality of scientific productions if no publication pressure existed (high and very high):
table_unitype_higherqualitynopp_high <- table(CodedresultsTenureCat$Unitype, CodedresultsTenureCat$HigherQualityNopp)
chisq.test(table_unitype_higherqualitynopp_high)

## First-hand knowledge and higher quality of scientific productions if no publication pressure existed:
table_firsthandknowledge_higherqualitynopp <- table(CodedresultsTenureCat$Awareness3, CodedresultsTenureCat$PP1)
chisq.test(table_firsthandknowledge_higherqualitynopp)

## First-hand knowledge and higher quality of scientific productions if no publication pressure existed (high and very high):
table_firsthandknowledge_higherqualitynopp_high <- table(CodedresultsTenureCat$Awareness3, CodedresultsTenureCat$HigherQualityNopp)
chisq.test(table_firsthandknowledge_higherqualitynopp_high)

## Tenure and publication pressure deemed as stimulating:
table_tenure_stimulatingpp <- table(CodedresultsTenureCat$TenureCat, CodedresultsTenureCat$PP4)
chisq.test(table_tenure_stimulatingpp)
group_by(CodedresultsTenureCat, TenureCat) %>% summarize(count=n(),
                                                         mean=mean(PP4, na.rm = T),
                                                         sd=sd(PP4, na.rm = T))
## Unitype and and publication pressure deemed as stimulating:
table_unitype_stimulatingpp <- table(CodedresultsTenureCat$Unitype, CodedresultsTenureCat$PP4)
chisq.test(table_unitype_stimulatingpp)

## Unitype and and publication pressure deemed as stimulating (high and very high):
table_unitype_stimulatingpp_high <- table(CodedresultsTenureCat$Unitype, CodedresultsTenureCat$StimulatingppHigh)
chisq.test(table_unitype_stimulatingpp_high)

## First-hand knowledge and publication pressure deemed as stimulating:
table_firsthandknowledge_stimulatingpp <- table(CodedresultsTenureCat$Awareness3, CodedresultsTenureCat$PP4)
chisq.test(table_firsthandknowledge_stimulatingpp)
group_by(CodedresultsTenureCat, Awareness3) %>% summarize(count=n(),
                                                          mean=mean(PP4, na.rm = T),
                                                          sd=sd(PP4, na.rm = T))
## Effectiveness of institutional policy and publication pressure deemed as stimulating:
table_effectivenessofpolicy_stimulatingpp <- table(CodedresultsTenureCat$OrganizationEffectHigh, CodedresultsTenureCat$PP4)
chisq.test(table_effectivenessofpolicy_stimulatingpp)

## Effectiveness of institutional policy and publication pressure deemed as stimulating (high and very high):
table_effectivenessofpolicy_stimulatingpp_high <- table(CodedresultsTenureCat$OrganizationEffectHigh, CodedresultsTenureCat$StimulatingppHigh)
chisq.test(table_effectivenessofpolicy_stimulatingpp_high)

## First-hand knowledge and chances of getting caught:
table_firsthandknowledge_gettingcaught <- table(CodedresultsTenureCat$Awareness3, CodedresultsTenureCat$Perception5)
chisq.test(table_firsthandknowledge_gettingcaught)
group_by(CodedresultsTenureCat, Awareness3) %>% summarize(count=n(),
                                                          mean=mean(Perception5, na.rm = T),
                                                          sd=sd(Perception5, na.rm = T))
## Getting caught and higher quality of scientific productions if no publication pressure existed:
table_gettingcaught_higherqualitynopp <- table(CodedresultsTenureCat$Perception5, CodedresultsTenureCat$PP1)
chisq.test(table_gettingcaught_higherqualitynopp)

## Getting caught and higher quality of scientific productions if no publication pressure existed (high and very high):
table_gettingcaught_higherqualitynopp_high <- table(CodedresultsTenureCat$Perception5, CodedresultsTenureCat$HigherQualityNopp)
chisq.test(table_gettingcaught_higherqualitynopp_high)
