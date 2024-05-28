# sensitivity analysis with different priors

# capacity----
# BF with smaller prior as 1/3 of default medium
bf <- anovaBF(capacity~ stimulation* setsize + ID,
              data = capacityF, whichRandom = 'ID',
              rscaleEffects = 0.15)
plot(bf)
bfplus = bf[4]/bf[3]
bfResult = c(rev(as.vector(bf)[1:2]), as.vector(bfplus))
bfError = c(rev(bf@bayesFactor$error[1:2]),bfplus@bayesFactor$error)
bfValues <-  cbind(as.data.frame(bfResult),as.data.frame(bfError))

# BF with a larger prior, ultrawide
bf <- anovaBF(capacity~ stimulation* setsize + ID,
             data = capacityF, whichRandom = 'ID',
             rscaleEffects = 1)
plot(bf)
bfplus = bf[4]/bf[3]
bfResult = c(rev(as.vector(bf)[1:2]), as.vector(bfplus))
bfError = c(rev(bf@bayesFactor$error[1:2]),bfplus@bayesFactor$error)
bfValues2 <-  cbind(as.data.frame(bfResult),as.data.frame(bfError))


tableANOVA1 <- cbind(
  ANOVA.Effect = c('ST','SS','ST x SS'),
  bfValues %>% select(Prior_Smaller.BF01 = bfResult,Prior_Smaller.Error = bfError),
  bfValues2 %>% select(Prior_Larger.BF01 = bfResult,Prior_Larger.Error = bfError)
)
tableANOVA1$Prior_Smaller.Error = tableANOVA1$Prior_Smaller.Error*100
tableANOVA1$Prior_Larger.Error = tableANOVA1$Prior_Larger.Error*100
tableANOVA1$Prior_Smaller.BF01 = 1/tableANOVA1$Prior_Smaller.BF01
tableANOVA1$Prior_Larger.BF01 = 1/tableANOVA1$Prior_Larger.BF01


#precision----

# BF with smaller Prior as 1/3 of default medium
bf <- anovaBF(precision~ stimulation* setsize + ID,
              data = precisionF, whichRandom = 'ID',
              rscaleEffects = 0.15)
plot(bf)
bfplus = bf[4]/bf[3]
bfResult = c(rev(as.vector(bf)[1:2]), as.vector(bfplus))
bfError = c(rev(bf@bayesFactor$error[1:2]),bfplus@bayesFactor$error)
bfValues <-  cbind(as.data.frame(bfResult),as.data.frame(bfError))

# BF with a larger Prior, ultrawide
bf <- anovaBF(precision~ stimulation* setsize + ID,
              data = precisionF, whichRandom = 'ID',
              rscaleEffects = 1)
plot(bf)
bfplus = bf[4]/bf[3]
bfResult = c(rev(as.vector(bf)[1:2]), as.vector(bfplus))
bfError = c(rev(bf@bayesFactor$error[1:2]),bfplus@bayesFactor$error)
bfValues2 <-  cbind(as.data.frame(bfResult),as.data.frame(bfError))


tableANOVA2 <- cbind(
  ANOVA.Effect = c('ST','SS','ST x SS'),
  bfValues %>% select(Prior_Smaller.BF01 = bfResult,Prior_Smaller.Error = bfError),
  bfValues2 %>% select(Prior_Larger.BF01 = bfResult,Prior_Larger.Error = bfError)
)
tableANOVA2$Prior_Smaller.Error = tableANOVA2$Prior_Smaller.Error*100
tableANOVA2$Prior_Larger.Error = tableANOVA2$Prior_Larger.Error*100
tableANOVA2$Prior_Smaller.BF01 = 1/tableANOVA2$Prior_Smaller.BF01
tableANOVA2$Prior_Larger.BF01 = 1/tableANOVA2$Prior_Larger.BF01


tableANOVA <- rbind(tableANOVA1,tableANOVA2)
nice_tableANOVA <- nice_table(tableANOVA,
                              title = c("Table X", "Sensitivity of ANOVA Statistics"),
                              footnote = c("N = 48. ANOVA = analysis of variance. ST = stimulation condition. SS = set size. Smaller prior relative to the default prior = 0.15; Larger prior relative to the default prior = 1."),
                              separate.header = TRUE)
save_as_docx(nice_tableANOVA, path = paste(outputFolder,'ANOVA_stats_sensitivity.docx',sep = '/'))
