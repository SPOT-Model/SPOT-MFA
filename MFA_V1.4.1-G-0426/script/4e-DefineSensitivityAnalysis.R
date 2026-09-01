##################################################################################
##
## Script name:   4e-DefineSensitivityAnalysis.R
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
## Description:   Creates a function to run the sensitivity analysis
##
##################################################################################

##################################################################################
##  Define the sensitivity analysis function                                              
##################################################################################

sensitivity <- function(MFA.inputs){
  
  df <-   do.call("cbind",MFA.inputs)

  ## Split the inputs by those with uncertainty and those as constants.
  with_var <- as.vector(which(apply(df,2,var)>0))
  
  df_var <- df[,with_var]
  df_no.var <- df[,-with_var]
  
  for(i in 1:ncol(df_no.var)){
    assign(colnames(df_no.var)[i],as.vector(df_no.var[1,colnames(df_no.var)[i]]))
  }
  
  
  ##  Split the inputs into two separate tables
  A <- df_var[1:(iterations/2),]
  B <- df_var[((iterations/2)+1):iterations,]
  
  ##  Define the model over which to calculate the sensitivity
  ##  (e.g., the MFA reporting back emissions)
    MFA.model <- function(df_var){
        
      for(i in 1:ncol(df_var)){
        
        assign(colnames(df_var)[i],as.vector(df_var[,colnames(df_var)[i]]))
      }
      
      ##  Apply rural corrections
      tP1pc <- tP1pc*(1-(inputs$Rural_share_2020[m]*(1-RC_tP1pc)))
      C0 <- C0*(1-(inputs$Rural_share_2020[m]*(1-RC_C0)))
      tC1 <- tC1*(1-(inputs$Rural_share_2020[m]*(1-RC_tC1)))
      tC2i <- tC2i*(1-(inputs$Rural_share_2020[m]*(1-RC_tC2i)))
      tC2ii <- tC2ii*(1-(inputs$Rural_share_2020[m]*(1-RC_tC2ii)))
      tC2iii <- tC2iii*(1-(inputs$Rural_share_2020[m]*(1-RC_tC2iii)))
      tC3 <- tC3*(1-(inputs$Rural_share_2020[m]*(1-RC_tC3)))
      
      
      ##  If the Country is China, replace incineration estimates with actual data on incineration
      if(inputs$Country[m]=="China"){
        tC2iii <- rep(China_incin$Incinerated[which(China_incin$Unique_ID==inputs$Unique_ID[m])],nrow(df_var))
      }
      
      
      ## Sum each recovery or treatment option and normalise to 100%
      tmp <- which((tC2i+tC2ii+tC2iii)>100)
      
      for(j in 1:length(tmp)){
        
        tmp2 <- c(tC2i[tmp[j]], tC2ii[tmp[j]], tC2iii[tmp[j]])
        tC2i[tmp[j]] <- (tmp2[1]/(sum(tmp2)))*100
        tC2ii[tmp[j]] <- (tmp2[2]/(sum(tmp2)))*100
        tC2iii[tmp[j]] <- (tmp2[3]/(sum(tmp2)))*100
      }
      
      rm(tmp,tmp2)
      
      ## Calculate the percentage of collected waste going to treatment or recovery
      ## and round to 2 decimal places
      tC2 <- round(tC2i+tC2ii+tC2iii,2)
      
    ##  Calculate mass collected by informal sector
      P14 <- (WP/100 * Pop * WPp * WPdw * WPmw)/1000
    
    ##  Calculate the uncollected litter for the municipality
    C1 <- LT * (1+log(100/tC1)) * (1-S/100)
    
    ##  Calcualte the collection system emissions
    C3 <- C3i * (1-S/100)
    
    ## Calculate the percentage of collected waste going to treatment or recovery
    tC2 <- round(tC2i+tC2ii+tC2iii,2)
    
    ##  Define transfer coefficients with equivalence to other inputs
    C2 <- tC1
    C12 <- C0
    C13 <- C0
    C17 <- C0
    C18 <- C0
    C12a <- C0a
    C13a <- C0a
    C17a <- C0a
    C18a <- C0a
    C22a <- C0a
    C27aa <- C10
    C27ab <- C10
    C28aa <- C10
    C28ab <- C10
    
    ##  Calculate mismanagement of sorting losses
    C25aa <- (100-C2)*(1-S/100)
    C25ab <- (100-C2)*(1-S/100)
    C26aa <- (100-C2)*(1-S/100)
    C26ab <- (100-C2)*(1-S/100)
    
    
    ##################################################################################
    ##  Calculate the processes                                             
    ##################################################################################
    
    
    ##  Convert the generation from kg/cap/day to absolute tonnes/year
    tP1 <- ((tP1pc * Pop)*365)/1000
    
    ##  Calculate non-input tributary MFA processes
    tP2 <- tP1 * (tC1/100)
    tP8 <- tP1 * (1-tC1/100)
    tP4 <- tP2 * (tC2/100)
    tP4i <- tP2 * (tC2i/100)
    tP4ii <- tP2 * (tC2ii/100)
    tP4iii <- tP2 * (tC2iii/100)
    tP3 <- tP2 * (1-tC2/100)
    tP5 <- tP3 * (tC3/100)
    tP9 <- tP3 * (1-tC3/100)
    
    ##  Calculate the full MFA processes
    P15 <- tP4i
    P13 <- P14 + P15
    P12i <- tP4ii
    P12ii <- tP4iii
    P9 <- P12i + P12ii + P13
    P11 <- tP5
    P10 <- tP9
    P8 <- P10 + P11
    P7 <- P8 + P9
    P5 <- P7/(1-(C3/100))
    P6 <- P5 * (C3/100)
    P4 <- tP8 
    P3 <- P4 + P5
    P1 <- P3/(1-(C1/100))
    P2 <- P1 * (C1/100)
    P16 <- P10 * (1-(C8/100))
    P17 <- P10 * (C8/100)
    P18 <- P16 * (1-(C9/100))
    P19 <- P16 * (C9/100)
    P20 <- P4 * (1-(C10/100))
    P21 <- P4 * (C10/100)    
    P100 <- P2 + P4 + P6 + P10
    
    ##  Calculate the plastic MFA processes
    P2a <- P2 * (C11/100)
    P2aa <- P2a * (C11a/100)
    P2ab <- P2a * (1-(C11a/100))
    P6a <- P6 * (C13/100)
    P6aa <- P6a * (C13a/100)
    P6ab <- P6a * (1-(C13a/100))
    P14a <- P14 * (C15/100)
    P14aa <- P14a * (C21a/100)
    P14ab <- P14a * (1-(C21a/100))
    P15a <- P15 * (C16/100)
    P15aa <-P15a * (C22a/100)
    P15ab <- P15a * (1-(C22a/100))
    P17a <- P17 * (C17/100)
    P17aa <- P17a * (C17a/100)
    P17ab <- P17a * (1-(C17a/100))
    P19a <- P19 * (C14/100)
    P19aa <- P19a * (C14a/100)
    P19ab <- P19a * (1-(C14a/100))
    P20a <- P20 * (C18/100)
    P20aa <- P20a * (C18a/100)
    P20ab <- P20a * (1-(C18a/100))
    P21a <- P21 * (C12/100)
    P21aa <- P21a * (C12a/100)
    P21ab <- P21a * (1-(C12a/100))
    P22aa <- P14aa * (C23aa/100)
    P22ab <- P14ab * (C23ab/100)
    P23aa <- P14aa * (1-(C23aa/100))
    P23ab <- P14ab * (1-(C23ab/100))
    P24aa <- P15aa * (C24aa/100)
    P24ab <- P15ab * (C24ab/100)
    P25aa <- P15aa * (1-(C24aa/100))
    P25ab <- P15ab * (1-(C24ab/100))
    P26aa <- P22aa * (C25aa/100)
    P26ab <- P22ab * (C25ab/100)
    P27aa <- P22aa * (1-(C25aa/100))
    P27ab <- P22ab * (1-(C25ab/100))
    P28aa <- P24aa * (C26aa/100)
    P28ab <- P24ab * (C26ab/100)
    P29aa <- P24aa * (1-(C26aa/100))
    P29ab <- P24ab * (1-(C26ab/100))
    P30aa <- P26aa * (C27aa/100)
    P30ab <- P26ab * (C27ab/100)
    P31aa <- P26aa * (1-(C27aa/100))
    P31ab <- P26ab* (1-(C27ab/100))
    P32aa <- P28aa * (C28aa/100)
    P32ab <- P28ab * (C28ab/100)
    P33aa <- P28aa * (1-(C28aa/100))
    P33ab <- P28ab * (1-(C28ab/100))
    P1a <- P2a + P20a + P21a + P6a + P14a + P15a + P19a + P17a + P18*C0 + P11*C0 + P12ii*C0
    P1aa <- P1a * (C0a/100)
    P1ab <- P1a * (1-(C0a/100))
    
    ## Calculate the mass of plastic openly burned in tonnes/year
    plas_ob <- P17a + P21a + P30aa + P30ab + P32aa + P32ab
    
    ##  Calculate the gaseous emissions from open burning of plastic
    CO2_ob_plas <- CO2_EF * plas_ob/1e6 #t/y
    CO_ob_plas <- CO_EF * plas_ob/1e6 #t/y
    SO2_ob_plas <- SO2_EF * plas_ob/1e6 #t/y
    NOx_ob_plas <- NOx_EF * plas_ob/1e6 #t/y
    PM2.5_ob_plas <- PM2.5_EF * plas_ob/1e6 #t/y
    PM10_ob_plas <- PM10_EF * plas_ob/1e6 #t/y
    EC_ob_plas <- EC_EF * plas_ob/1e6 #t/y
    PCDD.Fs_TEQ_ob_plas <- PCDD.Fs_TEQ_EF * plas_ob/1e6 #g/y
    dl.PCB_TEQ_ob_plas <- dl.PCB_TEQ_EF * plas_ob/1e6 #g/y

    ##################################################################################
    ##  Calculate the output to report back on for the sensitivity analysis                                            
    ##################################################################################
    
    if(metric==1){
    ## Absolute emissions (debris + openly burned)
    result <- P2a+P6a+P19a+P20a+P31aa+P31ab+P33aa+P33ab+P17a+P21a+P30aa+P30ab+P32aa+P32ab
    }
    
    if(metric==2){
    ## Absolute emissions (debris)
    result <- P2a+P6a+P19a+P20a+P31aa+P31ab+P33aa+P33ab
    }
    
    if(metric==3){
    ## Absolute emissions (openly burned)
    #result <- P17a+P21a+P30aa+P30ab+P32aa+P32ab
    }
    
    if(metric==4){ 
    ## Per capita emissions (debris + openly burned)
    result <- (P2a+P6a+P19a+P20a+P31aa+P31ab+P33aa+P33ab+P17a+P21a+P30aa+P30ab+P32aa+P32ab)/Pop
    }
    
    if(metric==5){
    ## Per capita  emissions (debris)
    result <- (P2a+P6a+P19a+P20a+P31aa+P31ab+P33aa+P33ab)/Pop
    }
    
    if(metric==6){
    ## Per capita emissions (openly burned)
    result <- (P17a+P21a+P30aa+P30ab+P32aa+P32ab)/Pop
    }
    
    if(metric==7){
    ## SDG11.6.1
    result <- ((P11+P9-P26aa-P26ab-P28aa-P28ab)/P1)*100
    }
    
    if(metric==8){
    ## Mismanaged waste
    result <- P100
    }
    
    if(metric==9){
    ## Recycling rate
    result <- (P13/P1)*100
    }
    
    return(result)
      }
      
    ##################################################################################
    ##  Run the sensitivity analysis                                           
    ##################################################################################
    sen.result <-sobolmartinez(model=MFA.model,
                    X1 = A,
                    X2 = B)
     
    ##  Extract just the key data (e.g. Sobol Indices) so these can be saved and not take up
    ##  too much space.
     values <- rbind(t(sen.result$S[,1]),t(sen.result$T[,1]))
     
    ## Convert to a table and return
     values <- as.data.frame(cbind(matrix(c(inputs$Unique_ID[m],inputs$Unique_ID[m],"Main effect","Total effect"),ncol=2),values))
      colnames(values) <- c("Unique_ID","Effect",rownames(sen.result$S))
      rownames(values) <- NULL
     
    return(values)


}

