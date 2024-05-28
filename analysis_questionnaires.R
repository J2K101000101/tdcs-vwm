# guess 
guess <- Ques %>% 
  select(ID,Condition,Guess)
guess$Guess <- as.numeric(guess$Guess)
summaryGuess <- guess %>% group_by(Condition) %>%
  get_summary_stats(Guess, type = "mean_sd")
summaryGuess$mean100 <- summaryGuess$mean*100
# # A tibble: 3 × 5
# Condition variable     n  mean    sd
# <chr>     <chr>    <dbl> <dbl> <dbl>
#   1 DLPFC     Guess       48 0.812 0.394
# 2 PPC       Guess       48 0.646 0.483
# 3 Sham      Guess       48 0.521 0.505

guessSham <- guess %>% 
  subset(Condition == 'Sham')
guessSham %>% identify_outliers(Guess) 
guessSham %>% shapiro_test(Guess)
wilcox.test(guessSham$Guess,mu=0.5,alternative = "two.sided",correct = T)
wilcoxonOneSampleR(guessSham$Guess,mu=0.5)
# Wilcoxon signed rank test with continuity correction
# 
# data:  guessSham$Guess
# V = 612.5, p-value = 0.7773
# alternative hypothesis: true location is not equal to 0.5
# r 
# 0.0417 
1/ttestBF(guessSham$Guess,mu=0.5)
# [1] Null, mu=0.5 : 6.135253 ±0.06%
# 
# Against denominator:
#   Alternative, r = 0.707106781186548, mu =/= 0.5 
# ---
#   Bayes factor type: BFoneSample, JZS

guessPPC <- guess %>% 
  subset(Condition == 'PPC')
guessPPC %>% identify_outliers(Guess) 
guessPPC %>% shapiro_test(Guess)
wilcox.test(guessPPC$Guess,mu=0.5,alternative = "two.sided")
wilcoxonOneSampleR(guessPPC$Guess,mu=0.5)
ttestBF(guessPPC$Guess,mu=0.5)
# data:  guessPPC$Guess
# V = 759.5, p-value = 0.04392
# alternative hypothesis: true location is not equal to 0.5
# r 
# 0.292 
# [1] Alt., r=0.707 : 1.144146 ±0.02%
# 
# Against denominator:
#   Null, mu = 0.5 

guessDLPFC <- guess %>% 
  subset(Condition == 'DLPFC')
guessDLPFC %>% identify_outliers(Guess) 
guessDLPFC %>% shapiro_test(Guess)
wilcox.test(guessDLPFC$Guess,mu=0.5,alternative = "two.sided")
wilcoxonOneSampleR(guessDLPFC$Guess,mu=0.5)
ttestBF(guessDLPFC$Guess,mu=0.5)
# V = 955.5, p-value = 0.00001531
# alternative hypothesis: true location is not equal to 0.5
# r 
# 0.625
# [1] Alt., r=0.707 : 10739.71 ±0%
# 
# Against denominator:
#   Null, mu = 0.5 

postRating <- Ques %>% 
  select(ID,Condition,'Pain level', 'Attention level', 'Fatigue level')
summaryPost <- postRating %>% group_by(Condition) %>%
  get_summary_stats(c('Pain level', 'Attention level', 'Fatigue level'), type = "mean_sd")
# 1 DLPFC     Attention level    48  4.83 1.43 
# 2 DLPFC     Fatigue level      48  2.52 1.30 
# 3 DLPFC     Pain level         48  1.83 1.10 
# 4 PPC       Attention level    48  4.65 1.85 
# 5 PPC       Fatigue level      48  2.46 1.43 
# 6 PPC       Pain level         48  1.48 0.743
# 7 Sham      Attention level    48  5.23 1.39 
# 8 Sham      Fatigue level      48  2.40 1.40 
# 9 Sham      Pain level         48  1.33 0.724

pain <- postRating %>% select(ID,Condition,score = 'Pain level')
outlier<-pain %>%
  group_by(Condition) %>%
  identify_outliers(score)
data.frame(outlier)
normality<-pain %>%
  group_by(Condition) %>%
  shapiro_test(score)
data.frame(normality)
res<-anova_test(data=pain,dv=score,wid=ID,within=Condition) 
get_anova_table(res) 
pair<-pain %>% 
  pairwise_t_test(score~Condition,paired=TRUE, p.adjust.method = "bonferroni" ) 
data.frame(pair) 
cohensD(pain[pain$Condition == 'DLPFC',]$score,
        pain[pain$Condition == 'Sham',]$score,
        method = "paired" )
ttestBF(pain[pain$Condition == 'DLPFC',]$score,
        pain[pain$Condition == 'Sham',]$score,
        paired=TRUE)
# .y. group1 group2 n1 n2 statistic df     p p.adj p.adj.signif
# 1 score  DLPFC    PPC 48 48  2.311132 47 0.025 0.076           ns
# 2 score  DLPFC   Sham 48 48  3.065942 47 0.004 0.011            *
#   3 score    PPC   Sham 48 48  1.069078 47 0.290 0.870           ns
# 0.4425306

attention <- postRating %>% select(ID,Condition,score = 'Attention level')
attention %>%
  group_by(Condition) %>%
  identify_outliers(score)
data.frame(outlier)
attention %>%
  group_by(Condition) %>%
  shapiro_test(score)
res<-anova_test(data=attention,dv=score,wid=ID,within=Condition) 
get_anova_table(res) 
pair<-attention %>% 
  pairwise_t_test(score~Condition,paired=TRUE, p.adjust.method = "bonferroni" ) 
data.frame(pair) 
# 1 score  DLPFC    PPC 48 48  0.8366271 47 0.407 1.000           ns
# 2 score  DLPFC   Sham 48 48 -1.6152720 47 0.113 0.339           ns
# 3 score    PPC   Sham 48 48 -2.3614130 47 0.022 0.067           ns


Fatigue <- postRating %>% select(ID,Condition,score = 'Fatigue level')
outlier<-Fatigue %>%
  group_by(Condition) %>%
  identify_outliers(score)
data.frame(outlier)
normality<-Fatigue %>%
  group_by(Condition) %>%
  shapiro_test(score)
data.frame(normality)
res<-anova_test(data=Fatigue,dv=score,wid=ID,within=Condition) 
get_anova_table(res) 
pair<-Fatigue %>% 
  pairwise_t_test(score~Condition,paired=TRUE, p.adjust.method = "bonferroni" ) 
data.frame(pair) 
# 1 score  DLPFC    PPC 48 48 0.2820039 47 0.779     1           ns
# 2 score  DLPFC   Sham 48 48 0.6215315 47 0.537     1           ns
# 3 score    PPC   Sham 48 48 0.3745644 47 0.710     1           ns


# adverse effects
adverse <- Ques[10:29]

general_related<- function (df){
  DF <- as.data.frame(df[["prop.tbl"]])
  DF$x <- as.numeric(DF$x)
  DF$y <- as.numeric(DF$y)
  # general report 2 mild - 4 severe
  general <- DF %>% subset(x != 1)
  general_percent <- sum(general$Freq)
  # tDCS-related 3 possible- 5 definite
  relate <- general %>% subset(y >= 3)
  relate_percent <- sum(relate$Freq)
  output <- c(general_percent*100, relate_percent*100)
  return(output)
}

Headache <- CrossTable(adverse$Headache,adverse$`tDCS related headache`, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
Neck <- CrossTable(adverse$`Neck pain`,adverse$`tDCS related neck pain`, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
Scalp <- CrossTable(adverse$`Scalp pain`,adverse$`tDCS related scalp pain`, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
Tingling <- CrossTable(adverse$Tingling,adverse$`tDCS related tingling`, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
Itching <- CrossTable(adverse$Itching ,adverse$`tDCS related Itching`, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
Buring <- CrossTable(adverse$`Buring sensation` ,adverse$`tDCS related bruning sensation`, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
Skin <- CrossTable(adverse$`Skin redness` ,adverse$`tDCS related skin redness`, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
Sleepiness <- CrossTable(adverse$Sleepiness ,adverse$`tDCS related sleepiness`, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
Trouble <- CrossTable(adverse$`Trouble concentrating` ,adverse$`tDCS related trouble concentrating`, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
Acute <- CrossTable(adverse$`Acute mood change` ,adverse$`tDCS related acute mood change`, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)

adverseT <- rbind(general_related(Headache),
                  general_related(Neck), 
                  general_related(Scalp),
                  general_related(Tingling),
                  general_related(Itching),
                  general_related(Buring),
                  general_related(Skin),
                  general_related(Sleepiness),
                  general_related(Trouble),
                  general_related(Acute)
                  )
adverseT <- as.data.frame(adverseT)
adverseT <- cbind(
  c("Headache","Neck pain","Scalp pain", "Tingling","Itching", "Burning sensation", "Skin redness", "Sleepiness", "Trouble concentrating", "Acute mood change"),
  adverseT
)
colnames(adverseT) <- c("Symptom","General (%)","TDCS-Related (%)")
nice_adverseT <- nice_table(adverseT,
                                    title = c("Table X", "Self-Reported Adverse Effects After Stimulation"),
                                    footnote = c("A total of 144 sessions (three sessions for each participant). General: percentage of reported mild to severe symptom; TDCS-Related: percentage of reported symptom that is at least possibly related to stimulation."),
                                    separate.header = TRUE)
save_as_docx(nice_adverseT, path = paste(outputFolder,'Adverse Effect Table.docx',sep = '/'))
