# ANOVAs

# Table_descriptive_changes
summary <- capacityF %>% 
  group_by(setsize,stimulation) %>%
  get_summary_stats(capacity, type = "full")
capacityF$stimulation <- factor(capacityF$stimulation, levels = c('PPC', 'DLPFC'))

summaryPrecision <- precisionF %>% group_by(setsize,stimulation) %>%
  get_summary_stats(precision, type = "full")
summary2 <- rbind(summary,summaryPrecision)

PPC <- summary2[summary2$stimulation=='PPC',c(1,3,12,13,15)]
PPC$lb <- PPC$mean-PPC$ci
PPC$ub <- PPC$mean+PPC$ci
PPC <- PPC %>% select(-ci)
DLPFC <- summary2[summary2$stimulation=='DLPFC',c(1,3,12,13,15)]
DLPFC$lb <- DLPFC$mean-DLPFC$ci
DLPFC$ub <- DLPFC$mean+DLPFC$ci
DLPFC <- DLPFC %>% select(-ci)
summaryT <- merge(PPC,DLPFC,by=c("variable","setsize"))
colnames(summaryT) <- gsub('.x','.PPC',names(summaryT))
colnames(summaryT) <- gsub('.y','.DLPFC',names(summaryT))
# fun <- function(x) {
#   formatC(x, format = "f", digits = 3)
# }
Table1<- nice_table(summaryT,
                    title = c("Table 1", "Descriptive Statistics of Performance Changes Relative to Sham (N = 48)"),
                    footnote = c("Capacity ranges from 0 to the set size; precision ranges from 0 to ∞.PPC: Right posterior parietal cortex; DLPFC: Left dorsolateral prefrontal cortex."),
                    separate.header = TRUE)
save_as_docx(Table1, path = paste(outputFolder, 'Table_descriptive_changes.docx',sep='/'))


# main results figure: figure 2----
f <- capacityF$setsize
f <- c(0.7, 1.7,2.7)[as.integer(factor(f))]
f1 <- capacityF$setsize
f1 <- c(1.2, 2.2,3.2)[as.integer(factor(f1))]
str(capacityF)
p1 <- 
  ggplot(capacityF, aes(y=capacity, x=setsize,fill=stimulation, shape = stimulation)) +
  geom_hline(yintercept= 0, linetype='dashed', color= '#b5afaf')+
  geom_point( aes(x =f, shape = stimulation,color=stimulation), position = position_dodge(width=0.3), alpha=0.2)+
  stat_summary(fun.data = mean_cl_normal,geom='errorbar',position = position_dodge(width=0.3),width = 0.2,size = 0.5,aes(x =f)) +
  stat_summary(fun.y = mean,geom='point',position = position_dodge(width=0.3),size =3, alpha=1, aes(x =f, shape = stimulation,color=stimulation)) +
  # geom_boxplot(width=0.1, alpha=0.5,outlier.shape = NA,outlier.size =0,position = position_dodge(width=0.3))+
  geom_flat_violin(width = 1, alpha=0.5,position = position_nudge()) +
  scale_fill_manual(values = c('#57364e', '#566f1b')) +
  scale_color_manual(values = c('#57364e', '#566f1b')) +
  labs( x = 'Set size', y = expression(~bold("Capacity Relative to Sham ∆"~bolditalic(K))),size =1) + 
  coord_cartesian(ylim = c(-2.5, 2.5))+
  scale_x_discrete(expand = c(0, 1))+
  guides(fill=guide_legend(title.position="top", title.hjust = 0.5,title="Stimulation"),color=guide_legend(title.position="top", title.hjust = 0.5,title="Stimulation"),shape=guide_legend(title.position="top", title.hjust = 0.5,title="Stimulation"))+
  theme(
    legend.position = "top",
    legend.background=element_blank(),
    legend.key = element_blank(),
    legend.title=element_text(face="bold"),
    legend.text =element_text(face="bold"),
    element_line(colour = 'black',size = 1),
    panel.background=element_blank(),
    axis.line.y.left = element_line(size = 1),
    axis.line.x.bottom  = element_line(size = 1),
    axis.text = element_text(face="bold",color = 'black'),
    axis.title = element_text(face="bold"),
    text = element_text(size = 12))

p1
ggsave('tDCS_effect_on_capacity',device = "png", path = outputFolder)

str(precisionF)
require(scales)
p2 <- 
  ggplot(precisionF, aes(y=precision, x=setsize,fill=stimulation, shape = stimulation)) +
  geom_hline(yintercept= 0, linetype='dashed', color= '#b5afaf')+
  geom_point( aes(x =f, shape = stimulation,color=stimulation), position = position_dodge(width=0.3), alpha=0.2)+
  stat_summary(fun.data = mean_cl_normal,geom='errorbar',position = position_dodge(width=0.3),width = 0.2,size = 0.5,aes(x =f)) +
  stat_summary(fun.y = mean,geom='point',position = position_dodge(width=0.3),size =3, alpha=1, aes(x =f, shape = stimulation,color=stimulation)) +
  # geom_boxplot(width=0.1, alpha=0.5,outlier.shape = NA,outlier.size =0)+
  geom_flat_violin(width = 1, alpha=0.5,position = position_nudge()) +
  scale_fill_manual(values = c('#57364e', '#566f1b')) +
  scale_color_manual(values = c('#57364e', '#566f1b')) +
  labs( x = 'Set size', y = expression(~bold("Precision Relative to Sham ∆"~bolditalic(SD^-1))),size =1) + 
  coord_cartesian(ylim = c(-0.05, 0.05))+
  scale_x_discrete(expand = c(0, 1))+
  scale_y_continuous(labels = function(x) format(x, nsmall = 2)) +
  guides(fill=guide_legend(title.position="top", title.hjust = 0.5,title="Stimulation"),color=guide_legend(title.position="top", title.hjust = 0.5,title="Stimulation"),shape=guide_legend(title.position="top", title.hjust = 0.5,title="Stimulation"))+
  theme(
    legend.position = "top",
    legend.background=element_blank(),
    legend.key = element_blank(),
    legend.title=element_text(face="bold"),
    legend.text =element_text(face="bold"),
    element_line(colour = 'black',size = 1),
    panel.background=element_blank(),
    axis.line.y.left = element_line(size = 1),
    axis.line.x.bottom  = element_line(size = 1),
    axis.text = element_text(face="bold",color = 'black'),
    axis.title = element_text(face="bold"),
    text = element_text(size = 12))
 p2 
ggsave('tDCS_effect_on_precision',device = "png", path = outputFolder)

figure <- ggarrange(p1,p2, 
                    labels = c("a", "b"),
                    ncol = 2, nrow = 1, 
                    common.legend = TRUE, legend="top")+
  theme(
    legend.background=element_blank(),
    legend.key = element_blank(),
    legend.title=element_blank(),  element_line(colour = 'black',size = 1),panel.background=element_blank(),axis.text = element_text(face="bold"),axis.title = element_text(face="bold"),text = element_text(size = 12))
ggsave(paste(outputFolder,"main_results_figure.png",sep = '/'),width = 9,height = 4.5)

# Two-way ANOVA test----
# capacity----
# outfilters
capacityF %>%
  group_by(stimulation, setsize) %>%
  identify_outliers(capacity)

#Normality:If the data is normally distributed, the p-value should be greater than 0.05.Note that, if your sample size is greater than 50, the normal QQ plot is preferred because at larger sample sizes the Shapiro-Wilk test becomes very sensitive even to a minor deviation from normality.
capacityF%>%
  group_by(stimulation, setsize) %>%
  shapiro_test(capacity)

#Homogneity of variance assumption. There was homogeneity of variances, as assessed by Levene’s test (p > 0.05).
capacityF %>%
  group_by(setsize) %>%
  levene_test(capacity ~ stimulation)

str(capacityF)

# BF
bf <- anovaBF(capacity~ stimulation* setsize + ID,
              data = capacityF, whichRandom = 'ID')
plot(bf)
bfplus = bf[4]/bf[3]

samples = posterior(bfplus, iterations = 10000,progress = FALSE, columnFilter="^ID$")
pSum <- summary(samples)
temp <- as.data.frame(cbind(pSum[["statistics"]][7:12,1:2], pSum[["quantiles"]][7:12,c(1,5)]))
posteriorSummary_capacity <- cbind(temp[c(1,3,5),], temp[c(2,4,6),])

#plot the table 
bfResult = c(rev(as.vector(bf)[1:2]), as.vector(bfplus))
bfError = c(rev(bf@bayesFactor$error[1:2]),bfplus@bayesFactor$error)
bfValues <-  cbind(as.data.frame(bfResult),as.data.frame(bfError))

afmod <- afex::aov_car(capacity~ stimulation* setsize + Error(ID/stimulation* setsize) , capacityF)
result2 <- afex::nice(afmod, es="ges")
es_g <- effectsize::eta_squared(afmod,generalized = TRUE, alternative = 'two.sided')
result22 <- afex::nice(afmod, es="pes")
es_p <- effectsize::eta_squared(afmod, generalized = FALSE, alternative = 'two.sided')

tableANOVA1 <- cbind(
  ANOVA.Effect = c('ST','SS','ST x SS'),
  ANOVA.F = afmod[["anova_table"]][["F"]],
  ANOVA.df1 = afmod[["anova_table"]][["num Df"]],
  ANOVA.df2 = afmod[["anova_table"]][["den Df"]],
  result2 %>% select(c(ANOVA.p = 'p.value', ANOVA.η2g = 'ges')),
  es_g %>% select(c(ANOVA.LBg = 'CI_low', ANOVA.UBg = 'CI_high')),
  result22 %>% select(ANOVA.η2p = 'pes'),
  es_p %>% select(c(ANOVA.LBp = 'CI_low', ANOVA.UBp = 'CI_high')),
  bfValues %>% select(ANOVA.BF = bfResult,ANOVA.BFerror = bfError)
)
tableANOVA1$ANOVA.BFerror = tableANOVA1$ANOVA.BFerror*100
tableANOVA1$ANOVA.BF = 1/tableANOVA1$ANOVA.BF

#precision----
# outfilters
precisionF %>%
  group_by(stimulation, setsize) %>%
  identify_outliers(precision)

#Normality:If the data is normally distributed, the p-value should be greater than 0.05.Note that, if your sample size is greater than 50, the normal QQ plot is preferred because at larger sample sizes the Shapiro-Wilk test becomes very sensitive even to a minor deviation from normality.
precisionF%>%
  group_by(stimulation, setsize) %>%
  shapiro_test(precision)

#Homogneity of variance assumption. There was homogeneity of variances, as assessed by Levene’s test (p > 0.05).
precisionF %>%
  group_by(setsize) %>%
  levene_test(precision ~ stimulation)

str(precisionF)

# BF
bf <- anovaBF(precision~ stimulation* setsize + ID,
              data = precisionF, whichRandom = 'ID')
bf
plot(bf)
bfplus = bf[4]/bf[3]

samples = posterior(bfplus, iterations = 10000,progress = FALSE, columnFilter="^ID$")
pSum <- summary(samples)
temp <- as.data.frame(cbind(pSum[["statistics"]][7:12,1:2], pSum[["quantiles"]][7:12,c(1,5)]))
posteriorSummary_precision <- cbind(temp[c(1,3,5),], temp[c(2,4,6),])

# plot the table
bfResult = c(rev(as.vector(bf)[1:2]), as.vector(bfplus))
bfError = c(rev(bf@bayesFactor$error[1:2]),bfplus@bayesFactor$error)
bfValues <-  cbind(as.data.frame(bfResult),as.data.frame(bfError))

afex::afex_options("APA" = FALSE)
afmod <- afex::aov_car(precision ~ stimulation* setsize + Error(ID/stimulation* setsize) , precisionF)
result2 <- afex::nice(afmod, es="ges")
es_g <- effectsize::eta_squared(afmod,generalized = TRUE, alternative = 'two.sided')
result22 <- afex::nice(afmod, es="pes")
es_p <- effectsize::eta_squared(afmod, generalized = FALSE, alternative = 'two.sided')

tableANOVA2 <- cbind(
  ANOVA.Effect = c('ST','SS','ST x SS'),
  ANOVA.F = afmod[["anova_table"]][["F"]],
  ANOVA.df1 = afmod[["anova_table"]][["num Df"]],
  ANOVA.df2 = afmod[["anova_table"]][["den Df"]],
  result2 %>% select(c(ANOVA.p = 'p.value', ANOVA.η2g = 'ges')),
  es_g %>% select(c(ANOVA.LBg = 'CI_low', ANOVA.UBg = 'CI_high')),
  result22 %>% select(ANOVA.η2p = 'pes'),
  es_p %>% select(c(ANOVA.LBp = 'CI_low', ANOVA.UBp = 'CI_high')),
  bfValues %>% select(ANOVA.BF = bfResult,ANOVA.BFerror = bfError)
)
tableANOVA2$ANOVA.BFerror = tableANOVA2$ANOVA.BFerror*100
tableANOVA2$ANOVA.BF = 1/tableANOVA2$ANOVA.BF
tableANOVA <- rbind(tableANOVA1,tableANOVA2)
nice_tableANOVA <- nice_table(tableANOVA,
                              title = c("Table X", "ANOVA Statistics for Performance Changes Relative to Sham"),
                              footnote = c("N = 48. ANOVA = analysis of variance. ST = stimulation condition. SS = set size."),
                              separate.header = TRUE)
save_as_docx(nice_tableANOVA, path = paste(outputFolder,'ANOVA_stats.docx',sep = '/'))

# plot posterior distribution
posteriorSummary <- rbind(posteriorSummary_capacity,posteriorSummary_precision)
posteriorSummary <- cbind(c('2','4','6','2','4','6'),posteriorSummary)
rownames(posteriorSummary) <- NULL
colnames(posteriorSummary) <- c('Set Size','PPC.M','PPC.SD','PPC.LB','PPC.UB','DLPFC.M','DLPFC.SD','DLPFC.LB','DLPFC.UB')

nice_posteriorSummary <- nice_table(posteriorSummary,
                                    title = c("Table X", "Summary of Posterior Distribution for ANOVA Interactions"),
                                    footnote = c("M = mean; SD = standard deviation; LB = lower bound of 95% credibility interval; UB = upper bound of 95% credibility interval."),
                                    separate.header = TRUE)
save_as_docx(nice_posteriorSummary, path = paste(outputFolder,'Summary for Posterior Distribution ANOVA.docx',sep = '/'))
