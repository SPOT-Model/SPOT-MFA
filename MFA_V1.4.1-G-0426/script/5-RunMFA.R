##################################################################################
##
## Script name:   5-RunMFA.R
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
## Description:   Runs the municipal level probabilistic MFA
##
##################################################################################

##  Outline that the model is solving the MFA
print("Solving probabilistic MFA")

##  Create a progress bar
progress <- txtProgressBar(min=1, 
                           max=length(uniqueID),
                           style=3)

##  Create lists to store the summarised results for each municipality in.
##  T1 = Process summaries
##  T2 = Coefficient summaries
##  T3 = Output summaries
T1 <- list()
T2 <- list()
T3 <- list()

sensitivity.results <- list()

################################################################################
##  Run the MFA for each country                                              
################################################################################

## Create a counter for municipality number regardless of country

m.all <- 1

## Loop over the countries
for(ISO3 in unique(inputs$ISO3)){

  ## Get the row numbers for municipalities in the country
  m.country <- which(inputs$ISO3==ISO3)
  
  raw.mc <- raw.mc_template(m.country)
  
  ## Store raw misc results in an array.
  raw.misc <- array(NA,dim=c(iterations,6,length(m.country)))
  
  ##############################################################################
  ##  Iterate the MFA across each municipality in the country                                          
  ##############################################################################
  
  ##  Iterate across the municipalities
  for(m in m.country){

    ##  Populate progress bar
    setTxtProgressBar(progress,m.all)
    
    ## Start a second counter to record progression through country municipalities   
    m.c <- which(m==m.country)
    
    ##  If no population, make all results equal zero 
    if(inputs$Population_2020[m]<=0){
      
      res <- list("Processes" = matrix(NA,nrow=iterations,ncol=length(processes$Process_ID)),
                  "Coefficients" = matrix(NA,nrow=iterations,ncol=length(coeffs$Coeff_ID)),
                  "Outputs" = matrix(NA,nrow=iterations,ncol=length(outputs$Output_ID))
                  )
      
      colnames(res[[1]]) <- processes$Process_ID
      colnames(res[[2]]) <- coeffs$Coeff_ID
      colnames(res[[3]]) <- outputs$Output_ID
      
    } else {
      
      ##  Run the MFA calculations returning a list of (1) list of MFA results
      ##  (processes, coefficients & outputs); and (2) a list of sampled inputs
      ##  for the MFA
      res <- MFA.municipal(m)
      MFA.inputs <- res[[2]]
      res <- res[[1]]
    
    }
    
    ## Store the raw municipal level process results in an array
    raw.mc[,,m.c] <- res[[1]]
    
    ## Store the raw misc results 
    raw.misc[,,m.c] <- res[[3]][,c("WP_no","people_no_col","exp_uncol_plas_ob","exp_IRS_disp_plas_ob","exp_form_disp_plas_ob","exp_ob")]

    ##  If the municipality is to have its raw results saved, add to the list
    if(inputs$Unique_ID[m] %in% munic.raw & retain_full_munic==T){
      raw.save.m[[which(munic.raw == inputs$Unique_ID[m])]] <- res
    }
    
    ##################################################################################
    ##  Run sensitivity analysis if required                                                   
    ##################################################################################
    
    if(run_sensitivity_analysis==T){
      sensitivity.results[[m]] <- sensitivity(MFA.inputs)
    }
    
    ##################################################################################
    ##  Summarise MFA raw results                                                   
    ##################################################################################
    
    for(output in 1:3){
      
      if(output==1){
        T1[[m]] <- summarise(res[[output]])
      }
      
      if(output==2){
        T2[[m]] <- summarise(res[[output]])
      }
      
      if(output==3){
        T3[[m]] <- summarise(res[[output]])
      }
    }
    
    ## Increase counter by 1
    m.all <- m.all+1 
    
  } # Close municipal loop
  
  ##############################################################################
  ##  Apply spatial dependency to results                                         
  ##############################################################################
  
  if(length(m.country)>1){
    
      ## Extract the waste generation process result and transform so municipalities are columns
      Waste_generation <- raw.mc[,11,]
    
      ## Specify a correlation matrix
      corr <- matrix(0.5,nrow = length(m.country), ncol=length(m.country))
      diag(corr) <- 1
    
      ##  Use the Iman-Conover method to induce spatial dependency across municipalities
      ##  according to the above correlation matrix and returning the new rank order for
      ##  iterations.
      rank <- cornode(Waste_generation,
                      target = corr,
                      result = FALSE,
                      outrank = TRUE
                      )
    
      ##  Reorder the iterations of each municipality on the rank matrix, and repeat
      ##  for all outputs.
      for(j in seq_len(dim(raw.mc)[2])){
        raw.mc[,j,] <- sapply(1:dim(raw.mc)[3], function(m) raw.mc[,j,m][rank[,m]])
      }
      
      for(k in seq_len(dim(raw.misc)[2])){
        raw.misc[,k,] <- sapply(1:dim(raw.misc)[3], function(m) raw.misc[,k,m][rank[,m]])
      }
  }
  
  ##  Aggregate the results across all municipalities in the country, but keeping
  ##  each iteration and add to the National raw process array.
  raw.process.nat[which(ISO3 == unique(inputs$ISO3)),,] <- t(apply(raw.mc,MARGIN = c(1,2),function(x)sum(x,na.rm=T)))
  
  raw.misc.nat[which(ISO3 == unique(inputs$ISO3)),,] <- t(apply(raw.misc,MARGIN=c(1,2),function(x)sum(x,na.rm=T)))
  

} # Close country loop

if(run_sensitivity_analysis){
names(sensitivity.results) <- inputs$Unique_ID
}

## convert the summarised results from a list to a df and add to the relevant formatted table
Results[[1]][[1]][1:(length(T1)*length(stat.names)),(length(D1)+1):ncol(Results[[1]][[1]])] <- do.call("rbind",T1)
Results[[1]][[2]][1:(length(T2)*length(stat.names)),(length(D1)+1):ncol(Results[[1]][[2]])] <- do.call("rbind",T2)
Results[[1]][[3]][1:(length(T3)*length(stat.names)),(length(D1)+1):ncol(Results[[1]][[3]])] <- do.call("rbind",T3)


rm(T1,T2,T3)
