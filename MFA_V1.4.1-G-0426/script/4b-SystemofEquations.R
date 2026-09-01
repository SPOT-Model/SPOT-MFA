##################################################################################
##
## Script name:   4b-SystemofEquations.R
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
## Description:   Create a function to solve the MFA system of equations. 
##
##################################################################################

##################################################################################
##  Specify the inputs                                                
##################################################################################

MFA <- function(MFA.inputs){
  
  ##  Convert inputs list to objects
  for(i in 1:length(MFA.inputs)){
    assign(names(MFA.inputs)[i], MFA.inputs[[i]])
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
    tC2iii <- rep(China_incin$Incinerated[which(China_incin$Unique_ID==inputs$Unique_ID[m])],iterations)
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

##  Calcualte the colelction system emissions
C3 <- C3i * (1-S/100)

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
P1a <- P2a + P20a + P21a + P6a + P14a + P15a + P12ii*(C0/100) + P19a + P17a + P18*(C0/100) + P11*(C0/100) 
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
##  Perform error checks                                                    
##################################################################################

##  Calculate the tributary mass balance to check it equals 0 for each row
trib_mass_balance <- round(tP1 - (tP5 + tP4 + tP8 + tP9),
                           digits =3)

full_mass_balance <- round(P1 - (P2 + P20 + P21 + P6 + P18 + P19
                                + P17 + P11 + P12i + P12ii + P14 + P15),
                          digits = 3)

plastic_mass_balance <- round((P2a + P20a + P21a + P17a + P6a + P19a + P14a + P15a)
                             -(P2aa + P2ab + P20aa + P20ab + P21aa + P21ab + P6aa +P6ab 
                               + P17aa + P17ab + P19aa +P19ab + P23aa + P23ab + P27aa + P27ab
                               + P30aa + P31aa + P30ab + P31ab + P25aa + P25ab
                               + P29aa + P29ab + P32aa + P33aa + P32ab + P33ab),digits = 3)


## Create a warning if their is an error in the mass balance
if(any(trib_mass_balance != 0)){
  print("\n ERROR - Mass doesnt balance for Tributary MFA")
  stop()
}
if(any(full_mass_balance != 0)){
  print("\n ERROR - Mass doesnt balance for Full MFA")
  stop()
}
if(any(plastic_mass_balance != 0)){
  print("\n ERROR - Mass doesnt balance for Plastics MFA")
  stop()
}

##################################################################################
##  Group processes for returning                                                   
##################################################################################

MFA <- cbind(tP1,tP2,tP3,tP4i,tP4ii,tP4iii,tP4,tP5,tP8,tP9,
             P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12i,P12ii,P13,P14,P15,P16,P17,P18,P19,P20,P21,P100,
             P1a,P1aa,P1ab,P2a,P2aa, P2ab, P6a,P6aa, P6ab, P14a,P14aa,P14ab,P15a,P15aa,P15ab,P17a,P17aa,P17ab,
             P19a,P19aa,P19ab,P20a, P20aa,P20ab,P21a,P21aa,P21ab,P22aa,P22ab,P23aa,P23ab,
             P24aa,P24ab,P25aa,P25ab,P26aa,P26ab,P27aa,P27ab,P28aa,P28ab,P29aa,P29ab,
             P30aa,P30ab,P31aa,P31ab,P32aa,P32ab,P33aa,P33ab,
             CO2_ob_plas,CO_ob_plas,SO2_ob_plas,NOx_ob_plas,PM2.5_ob_plas,PM10_ob_plas,EC_ob_plas,PCDD.Fs_TEQ_ob_plas,dl.PCB_TEQ_ob_plas
)

return(MFA)


}
