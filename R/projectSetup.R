
# should only be run once


createFolders <- function() {
  
  folders <- c('data', 
               'doc', 
               'doc/fig')
  
  # data
  # doc
  # doc/fig
  
  for (folder in folders) {
    
    if (!dir.exists(folder)) {
      dir.create(folder)
    }
    
  }
  
}

downloadOSFdata <- function() {
  
  
  
}