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