
# Get the valid three sessions each participant (n=48) : 
# 0. keep only valid extId. 
# 1. Correct human error during data collection according to the notes: ID 115 session 2 belong to 116 session 1
# 2. Check each participants and its session number and trial number
# 3. remove invalid trails, sessions and participant 
#
# required input: 
#   1. downloaded in rawData Folder from https://www.tatool-web.com/#!/analytics and unzip to input Folder
#   2. conditionList.csv in the working directory
#   3. customized function: reduceReplica.R  
#
# Output: 
#   1. ~cleaned/ort_tdcs.csv

#  Basic session information for running the analysis 
# R version 4.1.3 (2022-03-10)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Monterey 12.2.1
# Additional packages: dbplyr_2.1.1; tidyr_1.3.0; tidyverse_1.3.2; plyr_1.8.8

# get ready----

set.seed(1234321)
# install.packages('pacman')
library(pacman)
pacman::p_load(pacman,dplyr,tidyr,plyr,tidyverse)
rm(list=ls()) 
setwd('/Users/JSK/Desktop/WM_tDCS')
workspace = getwd()
rawFolder = paste(workspace, "rawData", sep = "/")
inputFolder = paste(workspace, "input", sep = "/")
# outputFolder = paste(workspace, "output", sep = "/")
cleanedFolder = paste(workspace, "cleaned", sep = "/")

# dowanloaded in rawData folder from https://www.tatool-web.com/#!/analytics and unzip to input folder
zipFiles <- list.files(path=rawFolder, pattern = "*.zip", full.names = TRUE)
llply(.data = zipFiles, .fun = unzip, exdir = inputFolder)

#read files
#warnings: incomplete final line found by readTableHeader on 'XXX'. Do not worry! This means these csv files do not end with an End Of Line (EOL) character AKA unfinished session with less than 5 lines 
readFiles <- list.files(path = inputFolder, pattern="*.csv",full.names = TRUE)

# remove some replicated data if any
source('reduceReplica.R')
readFiles <- reduceReplica(readFiles)
all <- ldply(.data = readFiles, .fun = read.csv,header = TRUE, sep=',', stringsAsFactors = FALSE) 

# remove some invalid extId if any (test and lasttest); Valid extId should be like 101
all_1 <- all[grep('^\\d',all$extId),] 
for (r in 1: nrow(all_1)){
  if (nchar(all_1$extId[r]) > 3 && nchar(all_1$extId[r]) == 0){
    print('Warning: you have invilid extId pattern:')
    print(all_1$extId[r])
  }
}

# according to the data collection note: 116's session 1 was mistakenly recorded as 115's session 2 due to technical issue, so here I correct the extId and mark the sessionId as '-1'. 
all_1$sessionId[all_1$extId == '115' & all_1$sessionId == 2] = -1
all_1$extId[all_1$extId == '115' & all_1$sessionId == -1] = '116'
if (sum(all_1$extId == '116' & all_1$sessionId == -1)>0 & sum(all_1$extId == '115' & all_1$sessionId == 2) == 0 ){print('Continue...')}else{print('Double check the change!')}

#check missing trials, valid ids, missing sessions
# add condition (session sorting first)
df <- all_1[grep('^tatoolContinuousOrientation_b\\d',all_1$executableId),] 
all_2 <- all_1[grep('^tatoolContinuousOrientation_b\\d',all_1$executableId),] 
check <- df %>% group_by(extId, sessionId)
check <- split(check,check$extId)
practiceTrials <- 30
requiredTrials <- 360
excludedToken <- c()
excludedID <- c()
sortSessionByParticipant <- c()
for (i in 1:length(check)){
  
  participant = names(check)[i]
  checkByToken <- split(check[[i]], check[[i]]$sessionToken)
  
  for (n in 1:length(checkByToken)){
    token <- names(checkByToken)[n]
    if (nrow(checkByToken[[n]]) < requiredTrials & nrow(checkByToken[[n]]) > practiceTrials){ 
      print(token)
      print("Warning: this session have more extra trials than the pratice trials, if they are not valid session, it could possibly confound your results!")
    }
    if (nrow(checkByToken[[n]]) < requiredTrials){ # deleted those with less trials than the required as imcompleted session(s)
      excludedToken <- cbind(excludedToken,token)
      df <- df %>% 
        subset(sessionToken!= token)
    }
  }
  excludedToken <-  as.character(excludedToken)
  checkByToken1 <- checkByToken[names(checkByToken)%in%excludedToken==FALSE]
  
  excludedToken1 <- c()
  for(k in 1:length(checkByToken1) ){
    N = 0
    token <- names(checkByToken1)[k]
    for (j in 1: length(checkByToken1[[k]]$reactionTime)){
      if (checkByToken1[[k]]$reactionTime[j] < 1500){#excluded data: Orientation and shape reproduction tasks: one-third of trials’ reaction time is shorter than 1500 ms
        N = N+1
      }
    }
    if (N >= length(checkByToken1[[k]]$reactionTime)/3){
      excludedToken1 <- cbind(excludedToken1,token)
      excludedID <- rbind(excludedID, participant)
    }
  }
  
  # sort sessionId
  uniqueSessionId <- unique(check[[i]]$sessionId)
  sortSession <-uniqueSessionId[order(uniqueSessionId)]
  sortSession <- as.data.frame(cbind(extId = participant, sessionId = sortSession,session = c(1,2,3) ) )
  sortSessionByParticipant <- rbind(sortSessionByParticipant, sortSession)
  
}

df1 <- merge(df,sortSessionByParticipant, by = c('extId','sessionId'))
readCond <- paste(workspace, "conditionList.csv", sep = "/")
conditionList <- read.csv(readCond)
df2 <- merge(df1,conditionList, by = c('extId','session'))
write.csv(df2, file=paste(cleanedFolder,'ort_tdcs.csv',sep = "/"))

#by setsize
by_setsize <- df2 %>% 
  select(extId,setSize,deviation,session,condition) 
by_setsize <- by_setsize %>% 
  split(by_setsize$setSize) 


# in each set size, by participant, by session
tdcs <- data.frame()
rowList <-vector() 
for (i in 1: length(by_setsize)){
  t <- by_setsize[[i]] %>% 
    group_by(extId,session) 
  t <-split(t,t$extId) 
  for (j in 1:length(t)){
    temp <- t[[j]]  %>% 
      group_by(session) 
    temp <- split(temp,temp$session)
    
    for (n in 1:length(temp)){
      extId <- unique(temp[[n]]$extId)
      setsize <- unique(temp[[n]]$setSize)
      S <- unique(temp[[n]]$session)
      C <- unique(temp[[n]]$condition)
      tdcs <- rbind(tdcs, temp[[n]]$deviation) 
      k=(j-1)*length(temp)+n
      k=(i-1)*length(temp)*length(t)+k
      rowList[k] <- paste(extId, 'session', S,'setsize',setsize,'condition',C,sep = '_')
    }
  }
}
row.names(tdcs) <-  rowList
colnames(tdcs) <- c(1:120)

#  Next: go to matlab and run script tdcs_2_fitIntoModels.m


