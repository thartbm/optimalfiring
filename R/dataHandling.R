extractMatData <- function() {
  
  library(R.matlab)
  
  # reading stuff from messy 7 year old matlab project...
  files <- c('/home/marius/exDropbox/CoSMo13/matlab/data/Experiment 1/data-exp1.mat',
             '/home/marius/exDropbox/CoSMo13/matlab/data/Experiment 2 (noTraining-noCatch)/data-exp2.mat')
  
  for (exp in c(1,2)) {
    
    data <- readMat(files[exp])[[1]]
    
    integrateResponses <- NA
    
    for (ppno in c(1:length(data))) {
      
      # extract data from single participant
      ppdata <- data[[ppno]][[1]][,,1]
      # we want the integration trials:
      integrateTrials <- ppdata$rawIntegrateTrials
      
      Ntrials <- dim(integrateTrials)[1]
      
      # create vectors for data frame columns:
      expNo <- rep(exp,Ntrials)
      participant <- rep(ppno,Ntrials)
      cue1pos <- integrateTrials[,3]
      cue2pos <- integrateTrials[,4]
      targetGuess <- integrateTrials[,5]
      targetPos <- integrateTrials[,6]
      # just for completeness we add reaction time:
      RT <- ppdata$reactionTime
      
      ppdf <- data.frame(expNo,
                         participant,
                         cue1pos,
                         cue2pos,
                         targetGuess,
                         targetPos,
                         RT)
      
      if (is.data.frame(integrateResponses)) {
        integrateResponses <- rbind(integrateResponses, ppdf)
      } else {
        integrateResponses <- ppdf
      }
      
    }
  
    write.csv(integrateResponses, file=sprintf('data/exp%02d-integration.csv',exp), row.names=FALSE, quote=FALSE )
    
    # for exp 1:
    # get the distribution learning data (stimuli & responses)
    
  }
  
  
  
}