##################################################################################
##
## Script name:   3-Predictions.R
##
## Version:       1.4.1-G-0426
## 
## Author:        Dr Josh Cottom (J.Cottom@imperial.ac.uk)
##
## Organisation:  Imperial College London - Dr Costas Velis (C.Velis@imperial.ac.uk) Research Group.
##
## Last updated:  13/04/2026
##
## Parent script  0-MasterScript.R
##
## Description:   Uses random forest quantile regression to predict SWM data. Also
##                performs adjustments to predictions.
##
##################################################################################

##################################################################################
##  Create a function to sample input PDFs
##################################################################################

## Function for inputs not based on categorical income groups
sample.pdf<- function(samples,distribution,Param1,Param2,Param3,Param4){
  
  
  if(distribution=="Beta-PERT"){
    sample.pdf <- rpert(samples,min = Param1, mode = Param2,max = Param3, shape=Param4)
  }
  if(distribution=="Triangular"){
    sample.pdf <- rtriang(samples,min = Param1, mode = Param2,max = Param3)
  }
  if(distribution=="Normal"){
    if(Param1 ==0 & Param2 == 0){sample.pdf <- rep(0,samples)} else {
      sample.pdf <- rtruncnorm(samples, a=0 , b=100, mean = Param1, sd = Param2)
    }
  }
  if(distribution=="Uniform"){
    sample.pdf <- runif(samples,min = Param1, max = Param2)
  }
  return(sample.pdf)
}

## Function for inputs based on categorical income groups

sample.pdf.cat <- function(frac_income_cat,samples,distribution,col1,col2,col3,col4,WP){
  
  if(WP==TRUE){
    
    Param1 <- unlist(inputs[which(inputs$Income_level==floor(frac_income_cat)&inputs$DEGURBA_L2_2020==inputs$DEGURBA_L2_2020[m])[1],col1])+
      (unlist(inputs[which(inputs$Income_level==ceiling(frac_income_cat)&inputs$DEGURBA_L2_2020==inputs$DEGURBA_L2_2020[m])[1],col1])-
         unlist(inputs[which(inputs$Income_level==floor(frac_income_cat)&inputs$DEGURBA_L2_2020==inputs$DEGURBA_L2_2020[m])[1],col1]))*(frac_income_cat-floor(frac_income_cat))
    
    Param2 <- unlist(inputs[which(inputs$Income_level==floor(frac_income_cat)&inputs$DEGURBA_L2_2020==inputs$DEGURBA_L2_2020[m])[1],col2])+
      (unlist(inputs[which(inputs$Income_level==ceiling(frac_income_cat)&inputs$DEGURBA_L2_2020==inputs$DEGURBA_L2_2020[m])[1],col2])-
         unlist(inputs[which(inputs$Income_level==floor(frac_income_cat)&inputs$DEGURBA_L2_2020==inputs$DEGURBA_L2_2020[m])[1],col2]))*(frac_income_cat-floor(frac_income_cat))
    
    if(distribution=="Beta-PERT" | distribution == "Triangular"){
    Param3 <- unlist(inputs[which(inputs$Income_level==floor(frac_income_cat)&inputs$DEGURBA_L2_2020==inputs$DEGURBA_L2_2020[m])[1],col3])+
      (unlist(inputs[which(inputs$Income_level==ceiling(frac_income_cat)&inputs$DEGURBA_L2_2020==inputs$DEGURBA_L2_2020[m])[1],col3])-
         unlist(inputs[which(inputs$Income_level==floor(frac_income_cat)&inputs$DEGURBA_L2_2020==inputs$DEGURBA_L2_2020[m])[1],col3]))*(frac_income_cat-floor(frac_income_cat))
    }
    
    if(distribution=="Beta-PERT"){
    Param4 <- unlist(inputs[which(inputs$Income_level==floor(frac_income_cat)&inputs$DEGURBA_L2_2020==inputs$DEGURBA_L2_2020[m])[1],col4])+
      (unlist(inputs[which(inputs$Income_level==ceiling(frac_income_cat)&inputs$DEGURBA_L2_2020==inputs$DEGURBA_L2_2020[m])[1],col4])-
         unlist(inputs[which(inputs$Income_level==floor(frac_income_cat)&inputs$DEGURBA_L2_2020==inputs$DEGURBA_L2_2020[m])[1],col4]))*(frac_income_cat-floor(frac_income_cat))
    }
    
  } else {
  
    Param1 <- unlist(inputs[which(inputs$Income_level==floor(frac_income_cat))[1],col1])+
      (unlist(inputs[which(inputs$Income_level==ceiling(frac_income_cat))[1],col1])-
         unlist(inputs[which(inputs$Income_level==floor(frac_income_cat))[1],col1]))*(frac_income_cat-floor(frac_income_cat))
    
    Param2 <- unlist(inputs[which(inputs$Income_level==floor(frac_income_cat))[1],col2])+
      (unlist(inputs[which(inputs$Income_level==ceiling(frac_income_cat))[1],col2])-
         unlist(inputs[which(inputs$Income_level==floor(frac_income_cat))[1],col2]))*(frac_income_cat-floor(frac_income_cat))
    
    if(distribution=="Beta-PERT" | distribution == "Triangular"){
      Param3 <- unlist(inputs[which(inputs$Income_level==floor(frac_income_cat))[1],col3])+
        (unlist(inputs[which(inputs$Income_level==ceiling(frac_income_cat))[1],col3])-
           unlist(inputs[which(inputs$Income_level==floor(frac_income_cat))[1],col3]))*(frac_income_cat-floor(frac_income_cat))
    }
    
    if(distribution=="Beta-PERT"){
      Param4 <- unlist(inputs[which(inputs$Income_level==floor(frac_income_cat))[1],col4])+
        (unlist(inputs[which(inputs$Income_level==ceiling(frac_income_cat))[1],col4])-
           unlist(inputs[which(inputs$Income_level==floor(frac_income_cat))[1],col4]))*(frac_income_cat-floor(frac_income_cat))
    }
  }
  
  ## Sample distribution based on above parameters
    if(distribution=="Beta-PERT"){
      sample.pdf <- rpert(samples,min = Param1, mode = Param2,max = Param3, shape=Param4)
    }
    
    if(distribution=="Triangular"){
      sample.pdf <- rtriang(samples,min = Param1, mode = Param2,max = Param3)
    }
    
    if(distribution=="Normal"){
      if(Param1 ==0 & Param2 == 0){sample.pdf <- rep(0,samples)} else {
        sample.pdf <- rtruncnorm(samples, a=0 , b=100, mean = Param1, sd = Param2)
      }
    }
    
    if(distribution=="Uniform"){
      sample.pdf <- runif(samples,min = Param1, max = Param2)
    }
    
    return(sample.pdf) 
  }


##################################################################################
##  Estimate total litter as global variable                                         
##################################################################################

## Estimate the total litter for the European data (global not by municipality)
LT <- sample.pdf(iterations,"Normal",0.81,0.15)/(sample.pdf(iterations,"Beta-PERT",70,95,97.5,4)/100)

################################################################################
##  Create functions to predict SWM data for all municipalities and adjust the 
##  predictions based on if Kernal density estimation is required, removal of 
##  outliers and rurality.
################################################################################
RF.predict <- function(process_id,
                       RF_pred,
                       percentage){
  
  ##########################################################################
  ## Progress bar
  ##########################################################################
  
  progress <- txtProgressBar(
    min = 1,
    max = length(uniqueID),
    style = 3
  )
  
  ##########################################################################
  ## Generate RF predictions
  ##########################################################################
  
  RF_pred <- predictions(
    predict(
      RF_models[[RF_pred]],
      testing_data,
      type = "quantiles",
      what = function(x)
        sample(x, iterations, TRUE)
    )
  )
  
  ##########################################################################
  ## Storage object
  ##########################################################################
  
  adjusted <- vector("list", length(uniqueID))
  
  ##########################################################################
  ## For each municipality
  ##########################################################################
  
  for(m in 1:length(uniqueID)){
    
    setTxtProgressBar(progress, m)
    
    ######################################################################
    ## Extract predictions
    ######################################################################
    
    x <- as.numeric(RF_pred[m,])
    
    ######################################################################
    ## Constant values
    ######################################################################
    
    if(length(unique(x)) == 1){
      
      s <- rep(x[1], iterations)
      
    } else {
      
      ####################################################################
      ## Calculate statistics for outlier identification
      ####################################################################
      
      Q1 <- quantile(x,0.25,na.rm = TRUE)
      
      Q3 <- quantile(x,0.75,na.rm = TRUE)
      
      IQR_val <- Q3 - Q1
      
      low <- max(0,Q1 - 1.5 * IQR_val) ## limited to zero as minimum
      up  <- Q3 + 1.5 * IQR_val
      
      ####################################################################
      ## NON-PERCENTAGE VARIABLES
      ####################################################################
      
      if(percentage == FALSE){
        
        ##################################################################
        ## Remove outliers
        ##################################################################
        
        clean <- x[
          x >= low &
            x <= up
        ]
        
        ##################################################################
        ## No variation after cleaning
        ##################################################################
        
        if(length(unique(clean)) ==1){
          
          s <- rep(clean[1], iterations)
          
        } else {
          
          ################################################################
          ## KDE
          ################################################################
          
          den <- density(
            clean,
            kernel = "gaussian",
            bw = "nrd0",
            from = min(clean),
            to = max(clean),
            n = 128
          )
          
          ################################################################
          ## Initial vectorised sample
          ################################################################
          
          s <- sample(
            den$x,
            iterations,
            replace = TRUE,
            prob = den$y
          ) +
            rnorm(iterations, 0, den$bw)
          
          ################################################################
          ## Keep only plausible values
          ################################################################
          
          s <- s[
            is.finite(s) &
            s >= low &
            s <= up
          ]
          
          ################################################################
          ## Vectorised replenishment
          ################################################################
          
          counter <- 0
          
          while(length(s) < iterations &&
                counter < 100){
            
            needed <- iterations - length(s)
            
            ## Calculate more samples than needed as some may be rejected
            batch_size <- max(needed * 2, 100)
            
            candidate <- sample(
              den$x,
              batch_size,
              TRUE,
              den$y
            ) +
              rnorm(batch_size, 0, den$bw)
            
            ## Remove samples beyond outlier limits
            candidate <- candidate[
              is.finite(candidate) &
              candidate >= low &
              candidate <= up
            ]
            
            s <- c(s, candidate)
            
            counter <- counter + 1
          }
          
          ################################################################
          ## Throw error if KDE fails
          ################################################################
          
          if(length(s) < iterations){
            
            stop(
              paste(
                "Non-percentage KDE failed for municipality",
                m,
                "process:",
                process_id
              )
            )
          }
          
          ## limit to correct sample size
          s <- s[1:iterations]
        }
      }
      
      ##########################################################################
      ## PERCENTAGE VARIABLES
      ##########################################################################
      
      if(percentage == TRUE){
        
        ########################################################################
        ## tC1 special handling
        ########################################################################
        
        if(process_id == "tC1" | 
           process_id== "C0"){
          
          ## remove outliers
          s <- x[x >= low & x <= up]
          
          ## resample
          while(length(s) < iterations){
            
            s <- c(s, sample(x, 1, TRUE))
            
            s <- s[s >= low & s <= up]
          }
        }
        
        ########################################################################
        ## tC2iii special handling
        ########################################################################
        
        if(process_id == "tC2iii" &&
           !(inputs$ISO3[m] %in% Incin_countries)){
          
          s <- rep(0, iterations)
        }
        
        ####################################################################
        ## tC3 special handling
        ####################################################################
        
        if(process_id == "tC3"){
          
          mode_x <- as.numeric(
            names(which.max(table(x)))
          )
          
          if(median(x) == mode_x){
            
            s <- rep(mode_x, iterations)
            
          }
        }
        
        ########################################################################
        ## C0, tC2i, tC2ii
        ########################################################################
        
        if(process_id == "tC2i" |
           process_id == "tC2ii") {
          
          ########################################################################
          ## Boundary probabilities
          ########################################################################
          
          p0   <- mean(x == 0)
          p100 <- mean(x == 100)
          
          p_mid <- 1 - p0 - p100
          
          ########################################################################
          ## Sample groups
          ########################################################################
          
          group <- sample(
            c(1,2,3),
            size = iterations,
            replace = TRUE,
            prob = c(p0, p_mid, p100)
          )
          
          ########################################################################
          ## Output vector
          ########################################################################
          
          s <- rep(NA_real_, iterations)
          
          s[group == 1] <- 0
          s[group == 3] <- 100
          
          ########################################################################
          ## Number of interior values required
          ########################################################################
          
          k <- sum(group == 2)
          
          ########################################################################
          ## Interior processing
          ########################################################################
          
          if(k > 0){
            
            ######################################################################
            ## Interior values only
            ######################################################################
            
            unbounded <- x[
              x > 0 &
                x < 100
            ]
            
            ######################################################################
            ## Sparse support:
            ## empirical resampling
            ######################################################################
            
            if(length(unique(unbounded)) < 5){
              
              ps <- sample(
                unbounded,
                size = k,
                replace = TRUE
              )
              
            } else {
              
              ####################################################################
              ## Logit transform
              ####################################################################
              
              transformed <- log(
                unbounded /
                  (100 - unbounded)
              )
              
              transformed <- transformed[
                is.finite(transformed)
              ]
              
              ####################################################################
              ## KDE on logit scale
              ####################################################################
              
              den <- density(
                transformed,
                kernel = "gaussian",
                bw = "nrd0",
                n = 128
              )
              
              ####################################################################
              ## Vectorised rejection sampling
              ####################################################################
              
              ps <- numeric(0)
              
              counter <- 0
              
              while(length(ps) < k &&
                    counter < 100){
                
                needed <- k - length(ps)
                
                batch_size <- max(
                  needed * 2,
                  100
                )
                
                ##################################################################
                ## Sample KDE
                ##################################################################
                
                y <- sample(
                  den$x,
                  batch_size,
                  TRUE,
                  den$y
                ) +
                  rnorm(batch_size, 0, den$bw)
                
                ##################################################################
                ## Back-transform
                ##################################################################
                
                candidate <- 100 /
                  (1 + exp(-y))
                
                ##################################################################
                ## Keep valid percentages
                ##################################################################
                
                candidate <- candidate[
                  is.finite(candidate) &
                    candidate >= 0 &
                    candidate <= 100
                ]
                
                ps <- c(ps, candidate)
                
                counter <- counter + 1
              }
              
              ps <- ps[1:k]
            }
            
            ######################################################################
            ## Insert interior values
            ######################################################################
            
            s[group == 2] <- ps
          }
        }
        
      } ## closes percentage == TRUE
      
    } ## closes constant/non-constant
    
    ######################################################################
    ## Round and store
    ######################################################################
    
    adjusted[[m]] <- round(s, 2)
    
  } ## closes municipality loop
  
  ##########################################################################
  ## Convert to matrix
  ##########################################################################
  
  adjusted <- do.call("rbind", adjusted)
  
  return(adjusted)
  
} ## closes function

        

##################################################################################
##  Apply RF prediction function to each primary input                                                         
##################################################################################
##  Create a progress bar
progress <- txtProgressBar(min=1, 
                           max=length(uniqueID),
                           style=3)

RF_predictions <- list()

cat("\n")
print("Predicting waste generation rates - 1 of 7")
RF_predictions[["Waste_gen_rate"]] <- RF.predict(process_id = "tP1",
                                      RF_pred = "Waste_gen_rate",
                                      percentage = F)
cat("\n")
print("Predicting plastic in MSW - 2 of 7")
RF_predictions[["Plastic_MSW"]] <- RF.predict(process_id = "C0",
                                   RF_pred = "Plastic_MSW",
                                   percentage = T)
cat("\n")
print("Predicting collection coverage - 3 of 7")
RF_predictions[["Col_cov"]] <- RF.predict(process_id = "tC1",
                               RF_pred = "Col_cov",
                               percentage = T)

cat("\n")
print("Predicting formal dry recycling - 4 of 7")
RF_predictions[["Form_dry_recy"]] <- RF.predict(process_id = "tC2i",
                                     RF_pred = "Form_dry_recy",
                                     percentage = T)

cat("\n")
print("Predicting other recovery - 5 of 7")
RF_predictions[["Other_recv"]] <- RF.predict(process_id = "tC2ii",
                                  RF_pred = "Other_recv",
                                  percentage = T)
cat("\n")
print("Predicting incineration - 6 of 7")
RF_predictions[["Incin"]] <- RF.predict(process_id = "tC2iii",
                             RF_pred = "Incin",
                             percentage = T)
cat("\n")
print("Predicting controlled disposal - 7 of 7")
RF_predictions[["Cont_disp"]] <- RF.predict(process_id = "tC3",
                                RF_pred = "Cont_disp",
                                percentage = T)


