# sensitivity analysis with different priors small 0.2, medium 0.5, large 0.8
# fun <- function(x) {
#   formatC(x, format = "f", digits = 3)
# }
# For set size 2, assumption of normality is violated. Run both non-parametric and t-tests (sensitivity analysis )
capacity_ppc_2 <- capacityF %>%
  filter(stimulation == 'PPC'& setsize == '2')
# using the small prior
bf_greater <- ttestBF(capacity_ppc_2$capacity, mu=0, rscale = 0.2, nullInterval =  c(0,Inf))
bf <- bf_greater[1]
Prior.Small <- cbind(BF01 = 1/(as.vector(bf)[1]), Error = 100*bf@bayesFactor$error)
# using the medium prior
bf_greater <- ttestBF(capacity_ppc_2$capacity, mu=0, rscale = 0.5, nullInterval =  c(0,Inf))
bf <- bf_greater[1]
Prior.Medium <- cbind(BF01 = 1/(as.vector(bf)[1]), Error = 100*bf@bayesFactor$error)
# using the large prior
bf_greater <- ttestBF(capacity_ppc_2$capacity, mu=0, rscale = 0.8, nullInterval =  c(0,Inf))
bf <- bf_greater[1]
Prior.Large <- cbind(BF01 = 1/(as.vector(bf)[1]), Error = 100*bf@bayesFactor$error)
PPC_2 <- cbind(Prior.Small,Prior.Medium, Prior.Large)

capacity_ppc_4 <- capacityF %>%
  filter(stimulation == 'PPC'& setsize == '4')
# using the small prior
bf_greater <- ttestBF(capacity_ppc_4$capacity, mu=0, rscale = 0.2, nullInterval =  c(0,Inf))
bf <- bf_greater[1]
Prior.Small <- cbind(BF01 = 1/(as.vector(bf)[1]), Error = 100*bf@bayesFactor$error)
# using the medium prior
bf_greater <- ttestBF(capacity_ppc_4$capacity, mu=0, rscale = 0.5, nullInterval =  c(0,Inf))
bf <- bf_greater[1]
Prior.Medium <- cbind(BF01 = 1/(as.vector(bf)[1]), Error = 100*bf@bayesFactor$error)
# using the large prior
bf_greater <- ttestBF(capacity_ppc_4$capacity, mu=0, rscale = 0.8, nullInterval =  c(0,Inf))
bf <- bf_greater[1]
Prior.Large <- cbind(BF01 = 1/(as.vector(bf)[1]), Error = 100*bf@bayesFactor$error)
PPC_4 <- cbind(Prior.Small,Prior.Medium, Prior.Large)

capacity_ppc_6 <- capacityF %>%
  filter(stimulation == 'PPC'& setsize == '6')
# using the small prior
bf_greater <- ttestBF(capacity_ppc_6$capacity, mu=0, rscale = 0.2, nullInterval =  c(0,Inf))
bf <- bf_greater[1]
Prior.Small <- cbind(BF01 = 1/(as.vector(bf)[1]), Error = 100*bf@bayesFactor$error)
# using the medium prior
bf_greater <- ttestBF(capacity_ppc_6$capacity, mu=0, rscale = 0.5, nullInterval =  c(0,Inf))
bf <- bf_greater[1]
Prior.Medium <- cbind(BF01 = 1/(as.vector(bf)[1]), Error = 100*bf@bayesFactor$error)
# using the large prior
bf_greater <- ttestBF(capacity_ppc_6$capacity, mu=0, rscale = 0.8, nullInterval =  c(0,Inf))
bf <- bf_greater[1]
Prior.Large <- cbind(BF01 = 1/(as.vector(bf)[1]), Error = 100*bf@bayesFactor$error)
PPC_6 <- cbind(Prior.Small,Prior.Medium, Prior.Large)


capacity_dlpfc_2 <- capacityF %>%
  filter(stimulation == 'DLPFC'& setsize == '2')
# using the small prior
bf <-  ttestBF(capacity_dlpfc_2$capacity,mu=0, rscale = 0.2)
Prior.Small <- cbind(BF01 = 1/as.vector(bf)[1], Error = 100*bf@bayesFactor$error)
# using the medium prior 
bf <-  ttestBF(capacity_dlpfc_2$capacity,mu=0, rscale = 0.5)
Prior.Medium <- cbind(BF01 = 1/as.vector(bf)[1], Error = 100*bf@bayesFactor$error)
# using the large prior 
bf <-  ttestBF(capacity_dlpfc_2$capacity,mu=0, rscale = 0.8)
Prior.Large<- cbind(BF01 = 1/as.vector(bf)[1], Error = 100*bf@bayesFactor$error)
DLPFC_2 <- cbind(Prior.Small,Prior.Medium, Prior.Large)

capacity_dlpfc_4 <- capacityF %>%
  filter(stimulation == 'DLPFC'& setsize == '4')
# using the small prior
bf <-  ttestBF(capacity_dlpfc_4$capacity,mu=0, rscale = 0.2)
Prior.Small <- cbind(BF01 = 1/as.vector(bf)[1], Error = 100*bf@bayesFactor$error)
# using the medium prior 
bf <-  ttestBF(capacity_dlpfc_4$capacity,mu=0, rscale = 0.5)
Prior.Medium <- cbind(BF01 = 1/as.vector(bf)[1], Error = 100*bf@bayesFactor$error)
# using the large prior 
bf <-  ttestBF(capacity_dlpfc_4$capacity,mu=0, rscale = 0.8)
Prior.Large<- cbind(BF01 = 1/as.vector(bf)[1], Error = 100*bf@bayesFactor$error)
DLPFC_4 <- cbind(Prior.Small,Prior.Medium, Prior.Large)

capacity_dlpfc_6 <- capacityF %>%
  filter(stimulation == 'DLPFC'& setsize == '6')
# using the small prior
bf <-  ttestBF(capacity_dlpfc_6$capacity,mu=0, rscale = 0.2)
Prior.Small <- cbind(BF01 = 1/as.vector(bf)[1], Error = 100*bf@bayesFactor$error)
# using the medium prior 
bf <-  ttestBF(capacity_dlpfc_6$capacity,mu=0, rscale = 0.5)
Prior.Medium <- cbind(BF01 = 1/as.vector(bf)[1], Error = 100*bf@bayesFactor$error)
# using the large prior 
bf <-  ttestBF(capacity_dlpfc_6$capacity,mu=0, rscale = 0.8)
Prior.Large<- cbind(BF01 = 1/as.vector(bf)[1], Error = 100*bf@bayesFactor$error)
DLPFC_6 <- cbind(Prior.Small,Prior.Medium, Prior.Large)

# paired t-test at set size 6####
df_ss6_cap <- capacityF %>%
  filter(stimulation %in% c('PPC','DLPFC')& setsize == '6')
# using the small prior
bf <- ttestBF(capacity_ppc_6$capacity, capacity_dlpfc_6$capacity,rscale = 0.2, nullInterval =  c(0,Inf),paired=TRUE)
Prior.Small <- cbind(BF01 = 1/as.vector(bf)[1], Error = 100*bf@bayesFactor$error[1])
# using the medium prior
bf <- ttestBF(capacity_ppc_6$capacity, capacity_dlpfc_6$capacity,rscale = 0.5, nullInterval =  c(0,Inf),paired=TRUE)
Prior.Medium <- cbind(BF01 = 1/as.vector(bf)[1], Error = 100*bf@bayesFactor$error[1])
# using the large prior
bf <- ttestBF(capacity_ppc_6$capacity, capacity_dlpfc_6$capacity,rscale = 0.8, nullInterval =  c(0,Inf),paired=TRUE)
Prior.Large <- cbind(BF01 = 1/as.vector(bf)[1], Error = 100*bf@bayesFactor$error[1])
PPC_DLPFC_6 <- cbind(Prior.Small,Prior.Medium, Prior.Large)
  
table4 <- rbind(PPC_2,PPC_4,PPC_6,DLPFC_2,DLPFC_4,DLPFC_6,PPC_DLPFC_6)
table4 <- as.data.frame(table4)
table4 <- cbind(
  c('PPC','PPC','PPC','DLPFC','DLPFC','DLPFC','PPC vs DLPFC'),
  c('2','4','6','2','4','6','6'),
  table4)
colnames(table4) <- c('Stimulation','Set size', 'Prior_Small.BF01','Prior_Small.Error','Prior_Medium.BF01','Prior_Medium.Error','Prior_Large.BF01','Prior_Large.Error')
t_cap_Table<- nice_table(table4, 
                         title = c("Table X", "Sensitivity of T-Tests"),
                         footnote = c("Small prior = 0.2; Medium prior = 0.5; Large prior = 0.8."),
                         separate.header = TRUE)
save_as_docx(t_cap_Table, path = paste(outputFolder,'t_stats_sensitivity.docx',sep = '/'))
