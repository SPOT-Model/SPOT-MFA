##################################################################################
##
## Script name:   4c-DefineSummaryFunction.R
##
## Version:       1.4.1-G-0426
## 
## Author:        Dr Josh Cottom (J.Cottom@imperial.ac.uk)
##
## Organisation:  Imperial College London - Dr Costas Velis (C.Velis@imperial.ac.uk) Research Group.
##
## Last updated:  30/03/2026
##
## Parent script  0-MasterScript.R
##
## Description:   Creates a function to summarise the raw MFA results by the 
##                statistics defined in the Master Script.
##
##################################################################################

#################################################################################
##  Create a function to summarise the raw MFA results iteratively during the MFA                                              
#################################################################################
summarise <-  function(data){
  
  if(length(dim(data))==2){
    
    ## Create a temp data frame to populate the summarised results
    summary <- matrix(NA,ncol=ncol(data),nrow=length(stat.names))
    
    ##  For each of the summary statistics, summarise the raw data
    for(l in 1:length(summary.stats)){
      
      if(names(summary.stats)[l]=="Quantiles"){
        
        ##  Calculate the summary quantiles
        summary[l:nrow(summary),] <- apply(data,MARGIN = 2,quantiles)
        
      } else {
        ##  Calculate the summary statistic if not quantiles
        summary[l,]<- apply(data,MARGIN = 2,summary.stats[[l]])
      }
    }
  }
      
  if(length(dim(data))==3){
    
    x <- nrow(data)
    
    ## Create a temp data frame to populate the summarised results
    summary <- matrix(NA,ncol=ncol(data),nrow=length(stat.names)*dim(data)[1])

  ##  For each of the summary statistics, summarise the raw data
  for(l in 1:length(summary.stats)){
    
    if(names(summary.stats)[l]=="Quantiles"){
      
      ##  Calculate the row numbers for quantile data
      index <- c(1:nrow(summary))
      tmp <- !(rep(stat.names,nrow(data)) %in% stat.names[which(names(summary.stats)!="Quantiles")])
      index <- subset(index,tmp)
      
      
      ##  Calculate the summary quantiles
      summary[index,] <- apply(data,MARGIN = c(1,2),quantiles)
      
    } else {
      ##  Calculate the summary statistic if mot quantiles
      summary[seq(l,nrow(summary),length(stat.names)),]<- apply(data,MARGIN = c(1,2),summary.stats[[l]])
    }
    
  }
  

  }
  
  return(summary)
  
}


#################################################################################
##  Create a function to summarise quantiles if they exist                                              
#################################################################################

##  If quantiles need to be summarised, create a function that does this all at once
if(any(names(summary.stats)=="Quantiles")){
  
  ## Extract the quantiles to summarize over
  quants <- unname(unlist(summary.stats[which(names(summary.stats)=="Quantiles")]))
  
  ##  Add to a new quantile function
  quantiles <- function(x) {
    quantile(x,probs=quants, na.rm=T, names=F)
  }
  
}



