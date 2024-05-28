# Model comparision results####
AIC = read.csv(paste(myICSFolder,'dAIC_M1_M2.csv',sep = '/'), row.names = 1)
BIC = read.csv(paste(myICSFolder,'dBIC_M1_M2.csv',sep = '/'),row.names = 1)
setwd(myICSFolder)
bin = 6
stimulation <- gsub('*[0-9]','\\1',rownames(AIC))
setSize <- gsub('[A-Za-z]*','\\1',rownames(AIC))
rowlabs <- cbind(stimulation,setSize)

# one table for all individuals
clabs <- paste(stimulation, 'Set size',setSize, sep = '.')
Table1 <-  t(AIC)
colnames(Table1) <-clabs
Table1 <- as.data.frame(Table1)
Participant <- c(1:48)
Table1_all <- nice_table( cbind(Participant,Table1),
                          title = c(paste("Table X"), "Differences in Relative Akaike Information Criterion Values (ΔAIC)"),
                          footnote = c("Negative values favour SMM while positive values favour SM. The lower value represents a better model fit. SMM: standard mixture model; SM: swap model."),
                          separate.header = TRUE)
save_as_docx(Table1_all, path = 'AIC_all.docx')

Table2 <-  t(BIC)
colnames(Table2) <-clabs
Table2 <- as.data.frame(Table2)
dBIC <- row.names(Table2)
Table1_all <- nice_table( cbind(Participant,Table2), 
                          title = c("Table X", "Differences in Relative Bayesian Information Criterion Values (ΔBIC)"),
                          footnote = c("Negative values favour SMM while positive values favour SM. The lower value represents a better model fit. SMM: standard mixture model; SM: swap model."),
                          separate.header = TRUE)
save_as_docx(Table1_all, path = 'BIC_all.docx')

# a summary table for both AIC & BIC and all conditions
summaryAICBIC = read.csv(paste(myICSFolder,'eachCondFit.csv',sep = '/'))
summaryAICBIC$AIC = summaryAICBIC$AIC*100
summaryAICBIC$BIC = summaryAICBIC$BIC*100
Stimulation = append(stimulation,'All')
SetSize = append(setSize,'All')
summaryAICBIC <- cbind(
  Stimulation,
  SetSize,
  summaryAICBIC[,-1]
)
colnames(summaryAICBIC) <-c('Stimulation', 'Set size', 'AIC (%)','BIC(%)')

Table1_summaryAICBIC<- nice_table(summaryAICBIC,
                                  title = c("Table X", "Summary of Model Fit"),
                                  footnote = c("AIC: Akaike information criterion; BIC: Bayesian information criterion."),
                                  separate.header = TRUE)
save_as_docx(Table1_summaryAICBIC, path = 'summaryAICBIC.docx')
