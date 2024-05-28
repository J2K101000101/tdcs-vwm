# compared against zero, one-way one-sample t-tests, truncated BFttest----
# fun <- function(x) {
#   formatC(x, format = "f", digits = 3)
# }
# For set size 2, assumption of normality is violated. Run both non-parametric and t-tests (sensitivity analysis )
capacity_ppc_2 <- capacityF %>%
  filter(stimulation == 'PPC'& setsize == '2')
capacity_ppc_2 %>% identify_outliers(capacity) 
capacity_ppc_2 %>% shapiro_test(capacity)
# Not normally distributed, non-parametric 
res.stats <- wilcox_test(capacity_ppc_2, capacity~1, mu = 0, alternative = "greater", conf.level = 0.95, p.adjust.method = "bonferroni",detailed = TRUE)
res.es <- wilcox_effsize(capacity_ppc_2, capacity~1, mu = 0, alternative = "greater", ci = TRUE)
# sensitivity analysis using t-tests 
sensitivity_res.stats <- t_test(capacity_ppc_2, capacity~1 ,mu=0,alternative = "greater",conf.level = 0.95, p.adjust.method = "bonferroni",detailed = TRUE)
sensitivity_res.es <- effectsize::cohens_d(data = capacity_ppc_2, capacity ~ 1, mu=0, alternative = "greater")
# using the default prior
bf_greater <- ttestBF(capacity_ppc_2$capacity, mu=0, nullInterval =  c(0,Inf))
bf <- bf_greater[1]
ppc2_default <- cbind(res.stats[["statistic"]],res.stats[["p"]], res.es$effsize, 1/(as.vector(bf)[1]), 100*bf@bayesFactor$error)
# using the given effect size by the original study as prior
bf_greater <- ttestBF(capacity_ppc_2$capacity,mu=0, rscale = 0.293, nullInterval =  c(0,Inf))
bf <- bf_greater[1]
ppc2_informed <- cbind(res.stats[["statistic"]],res.stats[["p"]], res.es$effsize, 1/(as.vector(bf)[1]), 100*bf@bayesFactor$error)
ppc2 <- ppc2_informed

sample<- posterior(bf, iterations = 10000,progress = FALSE)
pSum <- summary(sample)
posterior_ppc2 <- c(pSum[["statistics"]][3,1:2], pSum[["quantiles"]][3,c(1,5)])


capacity_ppc_4 <- capacityF %>%
  filter(stimulation == 'PPC'& setsize == '4')
capacity_ppc_4 %>% identify_outliers(capacity) 
capacity_ppc_4 %>% shapiro_test(capacity)
res.stats <-t_test(capacity_ppc_4, capacity~1, mu = 0, alternative = "greater",conf.level = 0.95, p.adjust.method = "bonferroni",detailed = TRUE)
res.es <- effectsize::cohens_d(data = capacity_ppc_4, capacity ~ 1, mu=0, alternative = "greater")
# using the default prior
bf_greater <- ttestBF(capacity_ppc_4$capacity, mu=0, nullInterval =  c(0,Inf))
bf <- bf_greater[1]
ppc4_default <- cbind(res.stats[["statistic"]],res.stats[["p"]], res.es$Cohens_d, 1/(as.vector(bf)[1]), 100*bf@bayesFactor$error)
# using the given effect size by the original study as prior
bf_greater <- ttestBF(capacity_ppc_4$capacity,mu=0, rscale = 0.221, nullInterval =  c(0,Inf))
bf <- bf_greater[1]
ppc4_informed <- cbind(res.stats[["statistic"]],res.stats[["p"]], res.es$Cohens_d, 1/(as.vector(bf)[1]), 100*bf@bayesFactor$error)
ppc4 <- ppc4_informed

sample<- posterior(bf, iterations = 10000,progress = FALSE)
pSum <- summary(sample)
posterior_ppc4 <- c(pSum[["statistics"]][3,1:2], pSum[["quantiles"]][3,c(1,5)])


capacity_ppc_6 <- capacityF %>%
  filter(stimulation == 'PPC'& setsize == '6')
capacity_ppc_6 %>% identify_outliers(capacity) 
capacity_ppc_6 %>% shapiro_test(capacity)
# one-way t-test, greater according to Wang et al.,2019
res.stats <-t_test(capacity_ppc_6, capacity ~ 1, mu=0,alternative = "greater",conf.level = 0.95, p.adjust.method = "bonferroni",detailed = TRUE)
res.es <- effectsize::cohens_d(data = capacity_ppc_6, capacity ~ 1, mu=0, alternative = "greater")
# using the default prior
bf_greater <-  ttestBF(capacity_ppc_6$capacity,mu=0, nullInterval =  c(0,Inf))
bf <- bf_greater[1]
ppc6_default <- cbind(res.stats[["statistic"]],res.stats[["p"]], res.es$Cohens_d,  1/(as.vector(bf)[1]), 100*bf@bayesFactor$error)
# using the given effect size by the original study as prior
bf_greater <-  ttestBF(capacity_ppc_6$capacity,mu=0, rscale= 1.028, nullInterval =  c(0,Inf))
bf <- bf_greater[1]
ppc6_informed <- cbind(res.stats[["statistic"]],res.stats[["p"]], res.es$Cohens_d, 1/(as.vector(bf)[1]), 100*bf@bayesFactor$error)
ppc6 <- ppc6_informed

report_ppc_6 <- cbind(res.stats[["statistic"]],res.stats[["p"]], res.es$Cohens_d, res.es$CI_low, res.es$CI_high, 1/as.vector(bf)[1], 100*bf@bayesFactor$error)


sample<- posterior(bf, iterations = 10000,progress = FALSE)
pSum <- summary(sample)
posterior_ppc6 <- c(pSum[["statistics"]][3,1:2], pSum[["quantiles"]][3,c(1,5)])
posterior_ppc <-  as.data.frame(rbind(posterior_ppc2,posterior_ppc4,posterior_ppc6))

capacity_dlpfc_2 <- capacityF %>%
  filter(stimulation == 'DLPFC'& setsize == '2')
capacity_dlpfc_2 %>% identify_outliers(capacity) 
capacity_dlpfc_2 %>% shapiro_test(capacity)
# extreme outliers & not normally distributed, non-parametric
res.stats <- wilcox_test(capacity_dlpfc_2, capacity ~ 1, mu = 0, alternative = "two.sided",conf.level = 0.95, p.adjust.method = "bonferroni",detailed = TRUE)
res.es <- wilcox_effsize(capacity_dlpfc_2, capacity ~ 1, mu = 0, alternative = "two.sided", ci = TRUE) 
# sensitivity analysis using t-tests  
sensitivity_res.stats <- t_test(capacity_dlpfc_2, capacity ~ 1, mu=0, alternative = "two.sided",conf.level = 0.95, p.adjust.method = "bonferroni",detailed = TRUE)
sensitivity_res.es <- effectsize::cohens_d(data = capacity_dlpfc_2, capacity ~ 1, mu=0, alternative = "two.sided")
# using the default prior
bf <-  ttestBF(capacity_dlpfc_2$capacity,mu=0)
dlpfc2_default <- cbind(res.stats[["statistic"]],res.stats[["p"]], res.es$effsize, 1/as.vector(bf)[1], 100*bf@bayesFactor$error)
# using the prior as the 'mean' of effect size range [0.015-0.628]  given non-significant changes according to Wang et al. (2019)
bf <-  ttestBF(capacity_dlpfc_2$capacity,mu=0, rscale = 0.409)
dlpfc2_informed <- cbind(res.stats[["statistic"]],res.stats[["p"]], res.es$effsize, 1/as.vector(bf)[1], 100*bf@bayesFactor$error)
dlpfc2 <- dlpfc2_informed

sample<- posterior(bf, iterations = 10000,progress = FALSE)
pSum <- summary(sample)
posterior_dlpfc2 <- c(pSum[["statistics"]][3,1:2], pSum[["quantiles"]][3,c(1,5)])

capacity_dlpfc_4 <- capacityF %>%
  filter(stimulation == 'DLPFC'& setsize == '4')
capacity_dlpfc_4 %>% identify_outliers(capacity) 
capacity_dlpfc_4 %>% shapiro_test(capacity)
res.stats <-t_test(capacity_dlpfc_4, capacity ~ 1, mu=0, alternative = "two.sided",conf.level = 0.95, p.adjust.method = "bonferroni",detailed = TRUE)
res.es <- effectsize::cohens_d(data = capacity_dlpfc_4, capacity ~ 1, mu=0, alternative = "two.sided")
# using the default prior
bf <- ttestBF(capacity_dlpfc_4$capacity,mu=0)
dlpfc4_default <- cbind(res.stats[["statistic"]],res.stats[["p"]], res.es$Cohens_d, 1/as.vector(bf)[1], 100*bf@bayesFactor$error)
# using the prior as the 'mean' of effect size range [0.015-0.628]  given non-significant changes according to Wang et al. (2019)
bf<-  ttestBF(capacity_dlpfc_4$capacity,mu=0, rscale = 0.409)
dlpfc4_informed <- cbind(res.stats[["statistic"]],res.stats[["p"]], res.es$Cohens_d, 1/as.vector(bf)[1], 100*bf@bayesFactor$error)
dlpfc4 <-dlpfc4_informed

sample<- posterior(bf, iterations = 10000,progress = FALSE)
pSum <- summary(sample)
posterior_dlpfc4 <- c(pSum[["statistics"]][3,1:2], pSum[["quantiles"]][3,c(1,5)])

capacity_dlpfc_6 <- capacityF %>%
  filter(stimulation == 'DLPFC'& setsize == '6')
capacity_dlpfc_6 %>% identify_outliers(capacity) 
capacity_dlpfc_6 %>% shapiro_test(capacity)
res.stats <-t_test(capacity_dlpfc_6, capacity ~ 1, mu=0, alternative = "two.sided",conf.level = 0.95, p.adjust.method = "bonferroni",detailed = TRUE)
res.es <- effectsize::cohens_d(data = capacity_dlpfc_6, capacity ~ 1, mu=0, alternative = "two.sided")
# using the default prior
bf <- ttestBF(capacity_dlpfc_6$capacity,mu=0)
dlpfc6_default <- cbind(res.stats[["statistic"]],res.stats[["p"]], res.es$Cohens_d, 1/as.vector(bf)[1], 100*bf@bayesFactor$error)
# using the prior as the 'mean' of effect size range [0.015-0.628]  given non-significant changes according to Wang et al. (2019)
bf <-  ttestBF(capacity_dlpfc_6$capacity,mu=0, rscale = 0.409)
dlpfc6_informed <- cbind(res.stats[["statistic"]],res.stats[["p"]], res.es$Cohens_d, 1/as.vector(bf)[1], 100*bf@bayesFactor$error)
dlpfc6 <- dlpfc6_informed

sample<- posterior(bf, iterations = 10000,progress = FALSE)
pSum <- summary(sample)
posterior_dlpfc6 <- c(pSum[["statistics"]][3,1:2], pSum[["quantiles"]][3,c(1,5)])
posterior_dlpfc <-  as.data.frame(rbind(posterior_dlpfc2,posterior_dlpfc4,posterior_dlpfc6))
posterior_t <- rbind(posterior_ppc, posterior_dlpfc)

table4 <- rbind(ppc2,ppc4,ppc6,dlpfc2,dlpfc4,dlpfc6)
table4 <- as.data.frame(table4)
table4 <- cbind(
  c('PPC','PPC','PPC','DLPFC','DLPFC','DLPFC'),
  c('2','4','6','2','4','6'),
  table4)
colnames(table4) <- c('Stimulation','Set size','Statistical value','p', 'Effect size', 'BF01','Error')
t_cap_Table<- nice_table(table4, 
                         title = c("Table 2", "One-Sample t-tests for Capacity Changes Relative to Sham Against Zero"),
                         footnote = c("df = 47. Bonferroni-corrected threshold of 0.0083."),
                         separate.header = TRUE)
save_as_docx(t_cap_Table, path = paste(outputFolder,'t_stats.docx',sep = '/'))

# paired t-test at set size 6####
df_ss6_cap <- capacityF %>%
  filter(stimulation %in% c('PPC','DLPFC')& setsize == '6')
res.stats <- df_ss6_cap %>% t_test(capacity ~ stimulation,paired = TRUE, alternative = 'greater', conf.level = 0.95, p.adjust.method = "bonferroni",detailed = TRUE) 
res.es <- effectsize::cohens_d(data = df_ss6_cap, capacity ~ stimulation,paired = TRUE,alternative = 'greater')
# default
bf_greater <- ttestBF(capacity_ppc_6$capacity, capacity_dlpfc_6$capacity,nullInterval =  c(0,Inf),paired=TRUE)
bf <- bf_greater[1]
# informed
bf_greater <- ttestBF(capacity_ppc_6$capacity, capacity_dlpfc_6$capacity,rscale = 0.711, nullInterval =  c(0,Inf),paired=TRUE)
bf <- bf_greater[1]
1/(as.vector(bf)[1])
100*bf@bayesFactor$error

report_ppc_vs_dlpfc_6 <- cbind(res.stats[["statistic"]],res.stats[["p"]], res.es$Cohens_d, res.es$CI_low, res.es$CI_high, 1/as.vector(bf)[1], 100*bf@bayesFactor$error)
extra_report <- rbind(report_ppc_6, report_ppc_vs_dlpfc_6)
extra_report <- as.data.frame(extra_report)
extra_report <- cbind(c('PPC vs 0 at set size 6','PPC vs DLPFC at set size 6'),extra_report)
colnames(extra_report) <- c('Comparison','t','p', 'd', 'lb','up','BF01','Error')
extra_report_table <- nice_table(extra_report)
save_as_docx(extra_report_table, path = paste(outputFolder,'PPC compared to zero and DLPFC at set size six.docx',sep = '/'))

sample<- posterior(bf, iterations = 10000,progress = FALSE)
pSum <- summary(sample)
posterior_ppc6_dlpfc6 <- c(pSum[["statistics"]][3,1:2], pSum[["quantiles"]][3,c(1,5)])
posteriorSummary <- rbind(posterior_t, posterior_ppc6_dlpfc6)
posteriorSummary <- cbind(
  c('PPC & Set size 2 vs 0','PPC & Set size 4 vs 0','PPC & Set size 6 vs 0','DLPFC & Set size 2 vs 0','DLPFC & Set size 4 vs 0','DLPFC & Set size 6 vs 0', 'PPC & Set size 6 vs DLPFC & Set size 6'),
  posteriorSummary)
rownames(posteriorSummary) <- NULL
colnames(posteriorSummary) <- c('Comparison','M','SD','LB','UB')


nice_posteriorSummary <- nice_table(posteriorSummary,
                                    title = c("Table X", "Summary of Posterior Distribution for T-Tests"),
                                    footnote = c("M = mean; SD = standard deviation; LB = lower bound of 95% credibility interval; UB = upper bound of 95% credibility interval."),
                                    separate.header = TRUE)
save_as_docx(nice_posteriorSummary, path = paste(outputFolder,'Summary for Posterior Distribution t-tests.docx',sep = '/'))
