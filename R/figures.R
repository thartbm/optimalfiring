plotIntegrationErrors <- function(exp=1, target='inline') {
  
  df <- read.csv(sprintf('data/exp%02d-integration.csv', exp), stringsAsFactors=FALSE)
  
  participants <- unique(df$participant)
  
  if (target == 'svg') {
    
    library(svglite)
    
    svglite(sprintf('doc/fig/exp%02d-integrationerrors.svg',exp), width=11, height=8.5, system_fonts=list(sans='Arial'))
    
  } else if (target == 'pdf') {
    
    cairo_pdf(sprintf('doc/fig/exp%02d-integrationerrors.pdf',exp), width=11, height=8.5, family='Arial')
    
  }
  
  
  par(mfrow=c(5,6),mar=c(4,4,0.2,0.05))
  
  for (participant in participants) {
    
    pp.idx <- which(df$participant == participant & !is.na(df$RT))
    
    # first the cue position difference (for the X-axis)
    cue1 <- df$cue1pos[pp.idx] # Gaussian?
    cue2 <- df$cue2pos[pp.idx] # exponential?
    cueDiff <- cue1 - cue2
    
    # now the error of the target estimate (for the Y-axis)
    targetTrue <- df$targetPos[pp.idx]
    targetGuess <- df$targetGuess[pp.idx]
    
    integrationError <- targetGuess - targetTrue
    
    plot(x = cueDiff, 
         y = integrationError, 
         main = '',
         xlab = 'cue difference',
         ylab = 'integration error',
         xlim = c(-.2,.2),
         ylim = c(-.2,.2),
         col = '#AAAAAA33',
         #bt=NA,
         ax = FALSE,
         asp = 1)
    
    text(-0.19,0.19,sprintf('%d', participant))
    
    axis(side=1,at=c(-.2,-.1,0,.1,.2),labels=c('-.2','-.1','0','.1','.2'))
    axis(side=2,at=c(-.2,-.1,0,.1,.2),labels=c('-.2','-.1','0','.1','.2'))
    
  }
  
  
  if (target %in% c('svg','pdf')) {
    
    dev.off()
    
  }
  
}

plotCueDistributions <- function(exp=1) {
  
  # these are exploratory plots only, so there should not be PDF or SVG output, only inline
  
  df <- read.csv(sprintf('data/exp%02d-integration.csv', exp), stringsAsFactors=FALSE)
  
  participants <- unique(df$participant)
  
  par(mfrow=c(1,2))
  
  cue1hist <- list()
  cue2hist <- list()
  
  for (participant in participants) {
    
    ppdata <- df[which(df$participant == participant & !is.na(df$RT)),]
    
    # hist1 <- hist(ppdata$cue1pos - ppdata$targetPos,breaks=seq(-.38,.38,.01),plot=FALSE)
    # print(hist1$counts)
    # print(hist1$breaks)
    cue1hist[[participant]] <- hist(ppdata$cue1pos - ppdata$targetPos,breaks=seq(-.38,.38,.01),plot=FALSE)
    # print(diff(abs(range(ppdata$cue2pos - ppdata$targetPos)) > .1))
    # print(range(ppdata$cue2pos - ppdata$targetPos))
    cue2hist[[participant]] <- hist(ppdata$cue2pos - ppdata$targetPos,breaks=seq(-.38,.38,.01),plot=FALSE)
    
  }
  
  plot.new()
  plot.window(xlim=c(-.38,.38),ylim=c(0,50))
  title(main='Gaussian gun distributions')
  
  for (participant in participants) {
    
    # cat(sprintf('participant: %d\n',participant))
    hist1 <- cue1hist[[participant]]
    # print(hist1$breaks)
    # print(hist1$counts)
    
    y <- hist1[['counts']]
    x <- (hist1[['breaks']][1:(length(hist1[['breaks']])-1)] + (diff(hist1[['breaks']])/2.))
    
    lines(x,y,col='#66666666')
    
  }
  
  axis(side=1,at=c(-.3,-.2,-.1,0,.1,.2,.3))
  axis(side=2,at=c(0,10,20,30,40,50))
  
  plot.new()
  plot.window(xlim=c(-.38,.38),ylim=c(0,150))
  title(main='Exponential gun distributions')
  
  for (participant in participants) {
    
    # cat(sprintf('participant: %d\n',participant))
    hist2 <- cue2hist[[participant]]
    # print(hist1$breaks)
    # print(hist1$counts)
    
    y <- hist2[['counts']]
    x <- (hist2[['breaks']][1:(length(hist2[['breaks']])-1)] + (diff(hist2[['breaks']])/2.))
    
    lines(x,y,col='#66666666')
    
  }
  
  axis(side=1,at=c(-.3,-.2,-.1,0,.1,.2,.3))
  axis(side=2,at=c(0,30,60,90,120,150))
  
  
}