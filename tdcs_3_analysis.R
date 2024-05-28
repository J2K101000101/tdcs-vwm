
# get ready----

# R version 4.1.3 (2022-03-10)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS 13.5.1
set.seed(1234321)
library(pacman) # pacman_0.5.1    
pacman::p_load( # workflow and plotting 
                rempsyc, # rempsyc_0.0.9
                tidyverse, # tidyverse_1.3.2
                readr, # readr_2.1.4 
                readxl, # readxl_1.4.1 
                dplyr, # dplyr_1.1.2 
                ggpubr, # ggpubr_0.4.0 
                introdataviz,#introdataviz_0.0.0.9003
                apaTables, # apaTables_2.0.8
                psychReport, # psychReport_3.0.1   
                gmodels, # gmodels_2.18.1.1
                # statistics 
                BayesFactor, # BayesFactor_0.9.12-4.3
                rstatix, # rstatix_0.7.0
                effectsize, # 0.8.2 
                afex, # 1.3-0 
                # for analysis_questionnaires.R 
                rcompanion, # 2.4.15 
                lsr # 0.5.2 
                )

workspace = getwd()
myICSFolder = paste(workspace,'cleaned','outputICS',sep = '/')
outputFolder = paste(workspace, "output", sep = "/")

setwd(workspace)
options(scipen=999) 
outputFolder = paste(workspace, "output", sep = "/")
demographicsInfo <- read_csv('demographics.csv')
demographics <- demographicsInfo %>% 
  select(ID, Age, Gender,NI, TT,C)

# fun <- function(x) {
#   formatC(x, format = "f", digits = 3)
# }
# modify rstatix:: get_summary_stats to fix the nice_table() & save_as_docx() round number with three decimal problem (-0.015 --> -0.01)
body(get_summary_stats)[[10]] <- substitute(results <- switch(type, common = common_summary(data), robust = robust_summary(data), 
                                                              five_number = five_number_summary(data), mean_sd = mean_sd(data), 
                                                              mean_se = mean_se(data), mean_ci = mean_ci(data), median_iqr = median_iqr(data), 
                                                              median_mad = median_mad(data), quantile = quantile_summary(data, 
                                                                                                                         probs), mean = mean_(data), median = median_(data), min = min_(data), 
                                                              max = max_(data), full_summary(data)) %>% dplyr::mutate_if(is.numeric, 
                                                                                                                         round, digits = 8)) # increase the decimal spaces for this function
modified_get_summary_stats <- get_summary_stats
body(get_summary_stats)[[3]] <- substitute(
  if (is_grouped_df(data)) {
    results <- data %>% doo(modified_get_summary_stats, ..., type = type, 
        show = show)
    return(results)
})

# read files #### 
readFiles <- list.files(path = outputFolder, pattern="*.csv", full.names = TRUE)
final <- lapply(readFiles, read.csv, header = FALSE, sep=',')
colList <-sub(outputFolder, '', readFiles)
colList2 <-sub('/', '', colList)
COL<- sub('*.csv','',colList2)
ROW<-c(1:48)+100
TOTAL<-c()
for (i in 1:length(final)){
  tmp <- final[[i]][["V1"]]
  TOTAL <- cbind(TOTAL, tmp)
}
colnames(TOTAL) <- COL
rownames(TOTAL) <- ROW
included <- as.data.frame(TOTAL)
includedDemographics <- demographics[(demographics$ID %in% row.names(included)),]
includedTable <- data.frame(age = includedDemographics$Age,sex = includedDemographics$Gender,NI = includedDemographics$NI,TT = includedDemographics$TT,C = includedDemographics$C,row.names = includedDemographics$ID)
finalTable <- merge(included,includedTable,by="row.names")


# report baseline (sham) & condtions 
# Table (Appendix)
sham <- finalTable %>% select (Row.names,capacity_Sham_2,capacity_Sham_4,capacity_Sham_6, precision_Sham_2,precision_Sham_4,precision_Sham_6)
shamSummary <- sham %>%  get_summary_stats(type = 'full')
shamSummary$lb <- shamSummary$mean-shamSummary$ci
shamSummary$ub <- shamSummary$mean+shamSummary$ci
PPC <- finalTable %>% select (Row.names,capacity_PPC_2,capacity_PPC_4,capacity_PPC_6, precision_PPC_2,precision_PPC_4,precision_PPC_6)
ppcSummary <- PPC %>%  get_summary_stats(type = 'full')
ppcSummary$lb <- ppcSummary$mean-ppcSummary$ci
ppcSummary$ub <- ppcSummary$mean+ppcSummary$ci
DLPFC <- finalTable %>% select (Row.names,capacity_DLPFC_2,capacity_DLPFC_4,capacity_DLPFC_6, precision_DLPFC_2,precision_DLPFC_4,precision_DLPFC_6)
dlpfcSummary <- DLPFC %>%  get_summary_stats(type = 'full')
dlpfcSummary$lb <- dlpfcSummary$mean-dlpfcSummary$ci
dlpfcSummary$ub <- dlpfcSummary$mean+dlpfcSummary$ci
baseline <- cbind(
  shamSummary %>% select(mean,sd,lb,ub),
  ppcSummary %>% select(mean,sd,lb,ub),
  dlpfcSummary %>% select(mean,sd,lb,ub)
)
baseline <- cbind(c('Set size 2', 'Set size 4','Set size 6','Set size 2', 'Set size 4','Set size 6'),baseline)
colnames(baseline) <- c('Variable','Sham.M','Sham.SD','Sham.LB','Sham.UB','PPC.M','PPC.SD','PPC.LB','PPC.UB','DLPFC.M','DLPFC.SD','DLPFC.LB','DLPFC.UB')
nice_posteriorSummary <- nice_table(baseline,
                                    title = c("Table A", "Descriptive Statistics of Performance After Each Stimulation"),
                                    footnote = c("Capacity ranges from 0 to the set size; precision ranges from 0 to ∞. M = mean; SD = standard deviation; CI = 95% confidence interval of the mean value"),
                                    separate.header = TRUE)
save_as_docx(nice_posteriorSummary, path = paste(outputFolder,'Descriptive Statistics of Performance After Each Stimulation.docx',sep = '/'))

# demographics
demSummary <- includedDemographics %>%
  dplyr::summarise(mAge = mean(Age),
                   sdAge = sd(Age)
  ) 
sex <- includedDemographics %>%
  subset(Gender== 'Female')
nF <- nrow(sex) #31

includedDemographics$ID <- as.character(includedDemographics$ID )

# Supplementary Table 2
# head size table 
Participant <- c(1:48)
head <- cbind(Participant,finalTable %>% select(NI,TT,C))
summaryHeadSize <- head %>% 
  dplyr::summarise(mNI = mean(NI),sdNI = sd(NI),
                   mTT = mean(TT),sdTT = sd(TT),
                   mC = mean(C),sdC = sd(C)
                   )
headSizeTable <- nice_table(head,
                        title = c(paste("Table X"), "Individual Head Model"),
                        footnote = c("NI = length between nasion and inion (cm): TT = length from left tragus to right tragus (cm); C = circumference length (cm)."),
                        separate.header = TRUE)
save_as_docx(headSizeTable, path = paste(outputFolder, 'headSizeTable.docx', sep = '/'))

# Model comparision results####
# Input:  dAIC_M1_M2.csv & dBIC_M1_M2.csv; eachCondFit.csv
# Output: AIC_all.docx & BIC_all.docx (Appendix B); summaryAICBIC.docx (Table 1 in the manuscript)
source('analysis_plot_model_comparison.R')

# test for main hypotheses####
# capacity
capacityDf<- finalTable %>% 
  select (Row.names, capacity_DLPFC_2,capacity_DLPFC_4,capacity_DLPFC_6,capacity_PPC_2,capacity_PPC_4,capacity_PPC_6,capacity_Sham_2,capacity_Sham_4,capacity_Sham_6) %>%
  mutate(DLPFC_2= capacity_DLPFC_2-capacity_Sham_2,DLPFC_4= capacity_DLPFC_4-capacity_Sham_4,DLPFC_6= capacity_DLPFC_6-capacity_Sham_6,
         PPC_2= capacity_PPC_2-capacity_Sham_2,PPC_4= capacity_PPC_4-capacity_Sham_4,PPC_6= capacity_PPC_6-capacity_Sham_6)

capacityDLPFC <- as.data.frame(cbind (ID = capacityDf$Row.names,setsize = rep(c(2,4,6),each=48), stimulation = c('DLPFC'), capacity = c(capacityDf$DLPFC_2,capacityDf$DLPFC_4,capacityDf$DLPFC_6)))
capacityPPC <- as.data.frame(cbind (ID = capacityDf$Row.names,setsize = rep(c(2,4,6),each=48), stimulation = c('PPC'), capacity = c(capacityDf$PPC_2,capacityDf$PPC_4,capacityDf$PPC_6)))
capacityF <- rbind (capacityDLPFC,capacityPPC)
capacityF$capacity <-  as.numeric(capacityF$capacity)
capacityF$stimulation <- factor(capacityF$stimulation, levels = c('PPC', 'DLPFC'))
capacityF$setsize <- as.factor(capacityF$setsize)
capacityF$ID <- as.factor(capacityF$ID)

# precision
precisionDf<- finalTable %>% 
  select (Row.names, precision_DLPFC_2,precision_DLPFC_4,precision_DLPFC_6,precision_PPC_2,precision_PPC_4,precision_PPC_6,precision_Sham_2,precision_Sham_4,precision_Sham_6) %>%
  mutate(DLPFC_2= precision_DLPFC_2-precision_Sham_2,DLPFC_4= precision_DLPFC_4-precision_Sham_4,DLPFC_6= precision_DLPFC_6-precision_Sham_6,
         PPC_2= precision_PPC_2-precision_Sham_2,PPC_4= precision_PPC_4-precision_Sham_4,PPC_6= precision_PPC_6-precision_Sham_6)

precisionDLPFC <- as.data.frame(cbind (ID = precisionDf$Row.names,setsize = rep(c(2,4,6),each=48), stimulation = c('DLPFC'), precision = c(precisionDf$DLPFC_2,precisionDf$DLPFC_4,precisionDf$DLPFC_6)))
precisionPPC <- as.data.frame(cbind (ID = precisionDf$Row.names,setsize = rep(c(2,4,6),each=48), stimulation = c('PPC'), precision = c(precisionDf$PPC_2,precisionDf$PPC_4,precisionDf$PPC_6)))
precisionF <- rbind (precisionDLPFC,precisionPPC)
precisionF$precision <-  as.numeric(precisionF$precision)
precisionF$stimulation <- factor(precisionF$stimulation, levels = c('PPC', 'DLPFC'))
precisionF$setsize <- factor(precisionF$setsize)
precisionF$ID <- factor(precisionF$ID)

# Main analysis ####
# 1. descriptive stats (Table 1 in the manuscript)
# 2. plot the main resutls (Figure 2 in the manuscript)
# 3. Two-way mixed ANOVA test
# Input: capacityF & precisionF
# Output: Table_descriptive_changes.docx (Table 2 in the manuscript), main_results_figure.png (Figure 2 in the manuscript), ANOVA_stats.docx,  Summary for Posterior Distribution ANOVA.docx (Supplementary Table 3)
setwd(workspace)
source('analysis_main.R')

# compared against zero, one-sample t-tests----
# 1. compared against zero, one-sample t-tests
# 2. paired t-test on capacity at set size 6
# Input: capacityF & precisionF
# Output: t_stats.docx (Table 2 in the manuscript), Summary for Posterior Distribution t-tests.docx (Supplementary Table 5)
source('analysis_follow.R')

# More Supplementary Information
# ANOVA_stats_sensitivity.docx (Supplementary Table 4)
source('analysis_main_sensitivity.R')
# t_stats_sensitivity.docx (Supplementary Table 6)
source('analysis_follow_sensitivity.R')

# post stimulation rating ====
Ques <- read_excel('Sessions & Questionnaires.xlsx')
source('analysis_questionnaires.R')
#clear up----

p_unload(all)


