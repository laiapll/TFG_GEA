## FUNCIONS:

# Seleccionar:
selecio_vars_inicial <- function(x){
  res <- x %>%
    select(idno, essround, cntry,
           gndr, yrbrn,
           starts_with("isco") & !ends_with("p"),
           emplrel, emplno,
           starts_with("occf14") & !ends_with("ie"),
           starts_with("occm14") & !ends_with("ie"))
  return(res)
}

selecio_vars_final <- function(x){
  res <- x %>%
    select(idno, essround, cntry,
           gndr, yrbrn,
           origen, desti)
  return(res)
}


# OESCH:
Oesch_class_isco88 <- function(ocupacio, emplrel, emplno) {
  #### Recode occupation variable (isco88 com 4-digit) for respondents
  isco_mainjob <- ocupacio
  isco_mainjob[is.na(isco_mainjob)] <- -9
  var_label(isco_mainjob) <- "Current occupation of respondent - isco88 4-digit"
  
  #### Recode employment status for respondents
  emplrel_r <- emplrel
  emplrel_r[is.na(emplrel_r)] <- 9
  # emplrel_r[cntry == "FR" & (essround == 1 | essround == 2)] <- -9 # Replace this command line with [SYNTAX A] or [SYNTAX C]
  # emplrel_r[cntry == "HU" & essround == 2] <- -9 # Replace this command line with [SYNTAX E]
  val_label(emplrel_r, 9) <- "Missing"
  
  emplno_r <- emplno
  emplno_r[is.na(emplno_r)] <- 0
  emplno_r[emplno_r >= 1 & emplno_r <= 9] <- 1
  emplno_r[emplno_r >= 10 & emplno_r <= 66665] <- 2
  val_labels(emplno_r) <- c("0 employees" = 0,
                            "1-9 employees" = 1,
                            "10+ employees" = 2)
  
  
  selfem_mainjob <- NA
  selfem_mainjob[emplrel_r == 1 | emplrel_r == 9] <- 1
  selfem_mainjob[emplrel_r == 2 & emplno_r == 0] <- 2
  selfem_mainjob[emplrel_r == 3] <- 2
  selfem_mainjob[emplrel_r == 2 & emplno_r == 1] <- 3
  selfem_mainjob[emplrel_r == 2 & emplno_r == 2] <- 4
  val_labels(selfem_mainjob) <- c("Not self-employed" = 1,
                                  "Self-empl without employees" = 2,
                                  "Self-empl with 1-9 employees" = 3,
                                  "Self-empl with 10 or more" = 4)
  var_label(selfem_mainjob) <- "Employment status for respondants"
  
  
  #################################################
  # Create Oesch class schema for respondents
  #################################################
  
  class16 <- rep(-9, length(ocupacio))
  
  # Large employers (1)
  
  class16[selfem_mainjob == 4] <- 1
  
  # Self-employed professionals (2)
  
  class16[(selfem_mainjob == 2 | selfem_mainjob == 3) & isco_mainjob >= 2000 & isco_mainjob <= 2229] <- 2
  class16[(selfem_mainjob == 2 | selfem_mainjob == 3) & isco_mainjob >= 2300 & isco_mainjob <= 2470] <- 2
  
  # Small business owners with employees (3)
  
  class16[selfem_mainjob == 3 & isco_mainjob >= 1000 & isco_mainjob <= 1999] <- 3
  class16[selfem_mainjob == 3 & isco_mainjob >= 3000 & isco_mainjob <= 9333] <- 3
  class16[selfem_mainjob == 3 & isco_mainjob == 2230] <- 3
  
  # Small business owners without employees (4)
  
  class16[selfem_mainjob == 2 & isco_mainjob >= 1000 & isco_mainjob <= 1999] <- 4
  class16[selfem_mainjob == 2 & isco_mainjob >= 3000 & isco_mainjob <= 9333] <- 4
  class16[selfem_mainjob == 2 & isco_mainjob == 2230] <- 4
  
  # Technical experts (5)
  
  class16[selfem_mainjob == 1 & isco_mainjob >= 2100 &  isco_mainjob <= 2213] <- 5
  
  # Technicians (6)
  
  class16[selfem_mainjob == 1 & isco_mainjob >= 3100 &  isco_mainjob <= 3152] <- 6
  class16[selfem_mainjob == 1 & isco_mainjob >= 3210 &  isco_mainjob <= 3213] <- 6
  class16[selfem_mainjob == 1 & isco_mainjob == 3434] <- 6
  
  # Skilled manual (7)
  
  class16[selfem_mainjob == 1 & isco_mainjob >= 6000 &  isco_mainjob <= 7442] <- 7
  class16[selfem_mainjob == 1 & isco_mainjob >= 8310 &  isco_mainjob <= 8312] <- 7
  class16[selfem_mainjob == 1 & isco_mainjob >= 8324 &  isco_mainjob <= 8330] <- 7
  class16[selfem_mainjob == 1 & isco_mainjob >= 8332 &  isco_mainjob <= 8340] <- 7
  
  # Low-skilled manual (8)
  
  class16[selfem_mainjob == 1 & isco_mainjob >= 8000 &  isco_mainjob <= 8300] <- 8
  class16[selfem_mainjob == 1 & isco_mainjob >= 8320 &  isco_mainjob <= 8321] <- 8
  class16[selfem_mainjob == 1 & isco_mainjob == 8331] <- 8
  class16[selfem_mainjob == 1 & isco_mainjob >= 9153 &  isco_mainjob <= 9333] <- 8
  
  # Higher-grade managers and administrators (9)
  
  class16[selfem_mainjob == 1 & isco_mainjob >= 1000 &  isco_mainjob <= 1239] <- 9
  class16[selfem_mainjob == 1 & isco_mainjob >= 2400 &  isco_mainjob <= 2429] <- 9
  class16[selfem_mainjob == 1 & isco_mainjob == 2441] <- 9
  class16[selfem_mainjob == 1 & isco_mainjob == 2470] <- 9
  
  # Lower-grade managers and administrators (10)
  
  class16[selfem_mainjob == 1 & isco_mainjob >= 1300 &  isco_mainjob <= 1319] <- 10
  class16[selfem_mainjob == 1 & isco_mainjob >= 3400 &  isco_mainjob <= 3433] <- 10
  class16[selfem_mainjob == 1 & isco_mainjob >= 3440 &  isco_mainjob <= 3450] <- 10
  
  # Skilled clerks (11)
  
  class16[selfem_mainjob == 1 & isco_mainjob >= 4000 &  isco_mainjob <= 4112] <- 11
  class16[selfem_mainjob == 1 & isco_mainjob >= 4114 &  isco_mainjob <= 4210] <- 11
  class16[selfem_mainjob == 1 & isco_mainjob >= 4212 &  isco_mainjob <= 4222] <- 11
  
  # Unskilled clerks (12)
  
  class16[selfem_mainjob == 1 & isco_mainjob == 4113] <- 12
  class16[selfem_mainjob == 1 & isco_mainjob == 4211] <- 12
  class16[selfem_mainjob == 1 & isco_mainjob == 4223] <- 12
  
  # Socio-cultural professionals (13)
  
  class16[selfem_mainjob == 1 & isco_mainjob >= 2220 &  isco_mainjob <= 2229] <- 13
  class16[selfem_mainjob == 1 & isco_mainjob >= 2300 &  isco_mainjob <= 2320] <- 13
  class16[selfem_mainjob == 1 & isco_mainjob >= 2340 &  isco_mainjob <= 2359] <- 13
  class16[selfem_mainjob == 1 & isco_mainjob >= 2430 &  isco_mainjob <= 2440] <- 13
  class16[selfem_mainjob == 1 & isco_mainjob >= 2442 &  isco_mainjob <= 2443] <- 13
  class16[selfem_mainjob == 1 & isco_mainjob == 2445] <- 13
  class16[selfem_mainjob == 1 & isco_mainjob == 2451] <- 13
  class16[selfem_mainjob == 1 & isco_mainjob == 2460] <- 13
  
  # Socio-cultural semi-professionals (14)
  
  class16[selfem_mainjob == 1 & isco_mainjob == 2230] <- 14
  class16[selfem_mainjob == 1 & isco_mainjob >= 2330 &  isco_mainjob <= 2332] <- 14
  class16[selfem_mainjob == 1 & isco_mainjob == 2444] <- 14
  class16[selfem_mainjob == 1 & isco_mainjob >= 2446 &  isco_mainjob <= 2450] <- 14
  class16[selfem_mainjob == 1 & isco_mainjob >= 2452 &  isco_mainjob <= 2455] <- 14
  class16[selfem_mainjob == 1 & isco_mainjob == 3200] <- 14
  class16[selfem_mainjob == 1 & isco_mainjob >= 3220 &  isco_mainjob <= 3224] <- 14
  class16[selfem_mainjob == 1 & isco_mainjob == 3226] <- 14
  class16[selfem_mainjob == 1 & isco_mainjob >= 3229 &  isco_mainjob <= 3340] <- 14
  class16[selfem_mainjob == 1 & isco_mainjob >= 3460 &  isco_mainjob <= 3472] <- 14
  class16[selfem_mainjob == 1 & isco_mainjob == 3480] <- 14
  
  # Skilled service (15)
  
  class16[selfem_mainjob == 1 & isco_mainjob == 3225] <- 15
  class16[selfem_mainjob == 1 & isco_mainjob >= 3227 &  isco_mainjob <= 3228] <- 15
  class16[selfem_mainjob == 1 & isco_mainjob >= 3473 &  isco_mainjob <= 3475] <- 15
  class16[selfem_mainjob == 1 & isco_mainjob >= 5000 &  isco_mainjob <= 5113] <- 15
  class16[selfem_mainjob == 1 & isco_mainjob == 5122] <- 15
  class16[selfem_mainjob == 1 & isco_mainjob >= 5131 &  isco_mainjob <= 5132] <- 15
  class16[selfem_mainjob == 1 & isco_mainjob >= 5140 &  isco_mainjob <= 5141] <- 15
  class16[selfem_mainjob == 1 & isco_mainjob == 5143] <- 15
  class16[selfem_mainjob == 1 & isco_mainjob >= 5160 &  isco_mainjob <= 5220] <- 15
  class16[selfem_mainjob == 1 & isco_mainjob == 8323] <- 15
  
  # Low-skilled service (16)
  
  class16[selfem_mainjob == 1 & isco_mainjob >= 5120 &  isco_mainjob <= 5121] <- 16
  class16[selfem_mainjob == 1 & isco_mainjob >= 5123 &  isco_mainjob <= 5130] <- 16
  class16[selfem_mainjob == 1 & isco_mainjob >= 5133 &  isco_mainjob <= 5139] <- 16
  class16[selfem_mainjob == 1 & isco_mainjob == 5142] <- 16
  class16[selfem_mainjob == 1 & isco_mainjob == 5149] <- 16
  class16[selfem_mainjob == 1 & isco_mainjob == 5230] <- 16
  class16[selfem_mainjob == 1 & isco_mainjob == 8322] <- 16
  class16[selfem_mainjob == 1 & isco_mainjob >= 9100 &  isco_mainjob <= 9152] <- 16
  
  ####################################################################################################
  # Final Oesch class position
  ####################################################################################################
  
  class16[class16 == -9] <- NA
  val_labels(class16) <- c("Large employers" = 1,
                           "Self-employed professionals" = 2,
                           "Small business owners with employees" = 3,
                           "Small business owners without employees" = 4,
                           "Technical experts" = 5,
                           "Technicians" = 6,
                           "Skilled manual" = 7,
                           "Low-skilled manual" = 8,
                           "Higher-grade managers and administrators" = 9,
                           "Lower-grade managers and administrators" = 10,
                           "Skilled clerks" = 11,
                           "Unskilled clerks" = 12,
                           "Socio-cultural professionals" = 13,
                           "Socio-cultural semi-professionals" = 14,
                           "Skilled service" = 15,
                           "Low-skilled service" = 16)
  var_label(class16) <- "Respondent's Oesch class position - 16 classes"
  
  
  class8 <- NA
  class8[class16 <= 2] <- 1
  class8[class16 == 3 | class16 == 4] <- 2
  class8[class16 == 5 | class16 == 6] <- 3
  class8[class16 == 7 | class16 == 8] <- 4
  class8[class16 == 9 | class16 == 10] <- 5
  class8[class16 == 11 | class16 == 12] <- 6
  class8[class16 == 13 | class16 == 14] <- 7
  class8[class16 == 15 | class16 == 16] <- 8
  val_labels(class8) <- c("Self-employed professionals and large employers" = 1,
                          "Small business owners" = 2,
                          "Technical (semi-)professionals" = 3,
                          "Production workers" = 4,
                          "(Associate) managers" = 5,
                          "Clerks" = 6,
                          "Socio-cultural (semi-)professionals" = 7,
                          "Service workers" = 8)
  var_label(class8) <- "Respondent's Oesch class position - 8 classes"
  
  
  class5 <- NA
  class5[class16 <= 2 | class16 == 5 | class16 == 9 | class16 == 13] <- 1
  class5[class16 == 6 | class16 == 10 | class16 == 14] <- 2
  class5[class16 == 3 | class16 == 4] <- 3
  class5[class16 == 7 | class16 == 11 | class16 == 15] <- 4
  class5[class16 == 8 | class16 == 12 | class16 == 16] <- 5
  val_labels(class5) <- c("Higher-grade service class" = 1,
                          "Lower-grade service class" = 2,
                          "Small business owners" = 3,
                          "Skilled workers" = 4,
                          "Unskilled workers" = 5)
  var_label(class5) <- "Respondent's Oesch class position - 5 classes"
  
  
  ####################################################################################################
  # Convert all labelled variables (haven_labelled class) to factors
  # To convert a specific labelled variable to a factor: class16 <- to_factor(class16, drop_unused_labels = TRUE)
  # The levels argument allows to specify what should be used as the factor levels, the labels (default), the values or the labels prefixed with values
  # Example with the labels prefixed with values: class16 <- to_factor(class16, drop_unused_labels = TRUE, levels = "p")
  ####################################################################################################
  
  #d <-  unlabelled(d, drop_unused_labels = TRUE)
  return(list(class5, selfem_mainjob))
  
  ##################################
  # End 1
  ##################################
}

Oesch_class_isco88_origen <- function(ocupacio, emplrel, emplno) {
  #### Recode occupation variable (isco88 com 4-digit) for respondents
  isco_mainjob <- ocupacio
  isco_mainjob[is.na(isco_mainjob)] <- -9
  var_label(isco_mainjob) <- "Current occupation of respondent - isco88 4-digit"
  
  #### Recode employment status for respondents
  emplrel_r <- emplrel
  emplrel_r[emplrel_r != 1 & emplrel_r != 2] <- 9
  # emplrel_r[cntry == "FR" & (essround == 1 | essround == 2)] <- -9 # Replace this command line with [SYNTAX A] or [SYNTAX C]
  # emplrel_r[cntry == "HU" & essround == 2] <- -9 # Replace this command line with [SYNTAX E]
  val_label(emplrel_r, 9) <- "Missing"
  
  emplno_r <- emplno
  emplno_r[is.na(emplno_r)] <- 0
  emplno_r[emplno_r %in% c(-1, 5, 7)] <- 0
  emplno_r[emplno_r == 11] <- 1
  emplno_r[emplno_r == 26] <- 2
  val_labels(emplno_r) <- c("0 employees" = 0,
                            "1-9 employees" = 1,
                            "10+ employees" = 2)
  
  
  selfem_mainjob <- NA
  selfem_mainjob[emplrel_r == 1 | emplrel_r == 9] <- 1
  selfem_mainjob[emplrel_r == 2 & emplno_r == 0] <- 2
  selfem_mainjob[emplrel_r == 3] <- 2
  selfem_mainjob[emplrel_r == 2 & emplno_r == 1] <- 3
  selfem_mainjob[emplrel_r == 2 & emplno_r == 2] <- 4
  val_labels(selfem_mainjob) <- c("Not self-employed" = 1,
                                  "Self-empl without employees" = 2,
                                  "Self-empl with 1-9 employees" = 3,
                                  "Self-empl with 10 or more" = 4)
  var_label(selfem_mainjob) <- "Employment status for respondants"
  
  
  #################################################
  # Create Oesch class schema for respondents
  #################################################
  
  class16 <- rep(-9, length(ocupacio))
  
  # Large employers (1)
  
  class16[selfem_mainjob == 4] <- 1
  
  # Self-employed professionals (2)
  
  class16[(selfem_mainjob == 2 | selfem_mainjob == 3) & isco_mainjob >= 2000 & isco_mainjob <= 2229] <- 2
  class16[(selfem_mainjob == 2 | selfem_mainjob == 3) & isco_mainjob >= 2300 & isco_mainjob <= 2470] <- 2
  
  # Small business owners with employees (3)
  
  class16[selfem_mainjob == 3 & isco_mainjob >= 1000 & isco_mainjob <= 1999] <- 3
  class16[selfem_mainjob == 3 & isco_mainjob >= 3000 & isco_mainjob <= 9333] <- 3
  class16[selfem_mainjob == 3 & isco_mainjob == 2230] <- 3
  
  # Small business owners without employees (4)
  
  class16[selfem_mainjob == 2 & isco_mainjob >= 1000 & isco_mainjob <= 1999] <- 4
  class16[selfem_mainjob == 2 & isco_mainjob >= 3000 & isco_mainjob <= 9333] <- 4
  class16[selfem_mainjob == 2 & isco_mainjob == 2230] <- 4
  
  # Technical experts (5)
  
  class16[selfem_mainjob == 1 & isco_mainjob >= 2100 &  isco_mainjob <= 2213] <- 5
  
  # Technicians (6)
  
  class16[selfem_mainjob == 1 & isco_mainjob >= 3100 &  isco_mainjob <= 3152] <- 6
  class16[selfem_mainjob == 1 & isco_mainjob >= 3210 &  isco_mainjob <= 3213] <- 6
  class16[selfem_mainjob == 1 & isco_mainjob == 3434] <- 6
  
  # Skilled manual (7)
  
  class16[selfem_mainjob == 1 & isco_mainjob >= 6000 &  isco_mainjob <= 7442] <- 7
  class16[selfem_mainjob == 1 & isco_mainjob >= 8310 &  isco_mainjob <= 8312] <- 7
  class16[selfem_mainjob == 1 & isco_mainjob >= 8324 &  isco_mainjob <= 8330] <- 7
  class16[selfem_mainjob == 1 & isco_mainjob >= 8332 &  isco_mainjob <= 8340] <- 7
  
  # Low-skilled manual (8)
  
  class16[selfem_mainjob == 1 & isco_mainjob >= 8000 &  isco_mainjob <= 8300] <- 8
  class16[selfem_mainjob == 1 & isco_mainjob >= 8320 &  isco_mainjob <= 8321] <- 8
  class16[selfem_mainjob == 1 & isco_mainjob == 8331] <- 8
  class16[selfem_mainjob == 1 & isco_mainjob >= 9153 &  isco_mainjob <= 9333] <- 8
  
  # Higher-grade managers and administrators (9)
  
  class16[selfem_mainjob == 1 & isco_mainjob >= 1000 &  isco_mainjob <= 1239] <- 9
  class16[selfem_mainjob == 1 & isco_mainjob >= 2400 &  isco_mainjob <= 2429] <- 9
  class16[selfem_mainjob == 1 & isco_mainjob == 2441] <- 9
  class16[selfem_mainjob == 1 & isco_mainjob == 2470] <- 9
  
  # Lower-grade managers and administrators (10)
  
  class16[selfem_mainjob == 1 & isco_mainjob >= 1300 &  isco_mainjob <= 1319] <- 10
  class16[selfem_mainjob == 1 & isco_mainjob >= 3400 &  isco_mainjob <= 3433] <- 10
  class16[selfem_mainjob == 1 & isco_mainjob >= 3440 &  isco_mainjob <= 3450] <- 10
  
  # Skilled clerks (11)
  
  class16[selfem_mainjob == 1 & isco_mainjob >= 4000 &  isco_mainjob <= 4112] <- 11
  class16[selfem_mainjob == 1 & isco_mainjob >= 4114 &  isco_mainjob <= 4210] <- 11
  class16[selfem_mainjob == 1 & isco_mainjob >= 4212 &  isco_mainjob <= 4222] <- 11
  
  # Unskilled clerks (12)
  
  class16[selfem_mainjob == 1 & isco_mainjob == 4113] <- 12
  class16[selfem_mainjob == 1 & isco_mainjob == 4211] <- 12
  class16[selfem_mainjob == 1 & isco_mainjob == 4223] <- 12
  
  # Socio-cultural professionals (13)
  
  class16[selfem_mainjob == 1 & isco_mainjob >= 2220 &  isco_mainjob <= 2229] <- 13
  class16[selfem_mainjob == 1 & isco_mainjob >= 2300 &  isco_mainjob <= 2320] <- 13
  class16[selfem_mainjob == 1 & isco_mainjob >= 2340 &  isco_mainjob <= 2359] <- 13
  class16[selfem_mainjob == 1 & isco_mainjob >= 2430 &  isco_mainjob <= 2440] <- 13
  class16[selfem_mainjob == 1 & isco_mainjob >= 2442 &  isco_mainjob <= 2443] <- 13
  class16[selfem_mainjob == 1 & isco_mainjob == 2445] <- 13
  class16[selfem_mainjob == 1 & isco_mainjob == 2451] <- 13
  class16[selfem_mainjob == 1 & isco_mainjob == 2460] <- 13
  
  # Socio-cultural semi-professionals (14)
  
  class16[selfem_mainjob == 1 & isco_mainjob == 2230] <- 14
  class16[selfem_mainjob == 1 & isco_mainjob >= 2330 &  isco_mainjob <= 2332] <- 14
  class16[selfem_mainjob == 1 & isco_mainjob == 2444] <- 14
  class16[selfem_mainjob == 1 & isco_mainjob >= 2446 &  isco_mainjob <= 2450] <- 14
  class16[selfem_mainjob == 1 & isco_mainjob >= 2452 &  isco_mainjob <= 2455] <- 14
  class16[selfem_mainjob == 1 & isco_mainjob == 3200] <- 14
  class16[selfem_mainjob == 1 & isco_mainjob >= 3220 &  isco_mainjob <= 3224] <- 14
  class16[selfem_mainjob == 1 & isco_mainjob == 3226] <- 14
  class16[selfem_mainjob == 1 & isco_mainjob >= 3229 &  isco_mainjob <= 3340] <- 14
  class16[selfem_mainjob == 1 & isco_mainjob >= 3460 &  isco_mainjob <= 3472] <- 14
  class16[selfem_mainjob == 1 & isco_mainjob == 3480] <- 14
  
  # Skilled service (15)
  
  class16[selfem_mainjob == 1 & isco_mainjob == 3225] <- 15
  class16[selfem_mainjob == 1 & isco_mainjob >= 3227 &  isco_mainjob <= 3228] <- 15
  class16[selfem_mainjob == 1 & isco_mainjob >= 3473 &  isco_mainjob <= 3475] <- 15
  class16[selfem_mainjob == 1 & isco_mainjob >= 5000 &  isco_mainjob <= 5113] <- 15
  class16[selfem_mainjob == 1 & isco_mainjob == 5122] <- 15
  class16[selfem_mainjob == 1 & isco_mainjob >= 5131 &  isco_mainjob <= 5132] <- 15
  class16[selfem_mainjob == 1 & isco_mainjob >= 5140 &  isco_mainjob <= 5141] <- 15
  class16[selfem_mainjob == 1 & isco_mainjob == 5143] <- 15
  class16[selfem_mainjob == 1 & isco_mainjob >= 5160 &  isco_mainjob <= 5220] <- 15
  class16[selfem_mainjob == 1 & isco_mainjob == 8323] <- 15
  
  # Low-skilled service (16)
  
  class16[selfem_mainjob == 1 & isco_mainjob >= 5120 &  isco_mainjob <= 5121] <- 16
  class16[selfem_mainjob == 1 & isco_mainjob >= 5123 &  isco_mainjob <= 5130] <- 16
  class16[selfem_mainjob == 1 & isco_mainjob >= 5133 &  isco_mainjob <= 5139] <- 16
  class16[selfem_mainjob == 1 & isco_mainjob == 5142] <- 16
  class16[selfem_mainjob == 1 & isco_mainjob == 5149] <- 16
  class16[selfem_mainjob == 1 & isco_mainjob == 5230] <- 16
  class16[selfem_mainjob == 1 & isco_mainjob == 8322] <- 16
  class16[selfem_mainjob == 1 & isco_mainjob >= 9100 &  isco_mainjob <= 9152] <- 16
  
  ####################################################################################################
  # Final Oesch class position
  ####################################################################################################
  
  class16[class16 == -9] <- NA
  val_labels(class16) <- c("Large employers" = 1,
                           "Self-employed professionals" = 2,
                           "Small business owners with employees" = 3,
                           "Small business owners without employees" = 4,
                           "Technical experts" = 5,
                           "Technicians" = 6,
                           "Skilled manual" = 7,
                           "Low-skilled manual" = 8,
                           "Higher-grade managers and administrators" = 9,
                           "Lower-grade managers and administrators" = 10,
                           "Skilled clerks" = 11,
                           "Unskilled clerks" = 12,
                           "Socio-cultural professionals" = 13,
                           "Socio-cultural semi-professionals" = 14,
                           "Skilled service" = 15,
                           "Low-skilled service" = 16)
  var_label(class16) <- "Respondent's Oesch class position - 16 classes"
  
  
  class8 <- NA
  class8[class16 <= 2] <- 1
  class8[class16 == 3 | class16 == 4] <- 2
  class8[class16 == 5 | class16 == 6] <- 3
  class8[class16 == 7 | class16 == 8] <- 4
  class8[class16 == 9 | class16 == 10] <- 5
  class8[class16 == 11 | class16 == 12] <- 6
  class8[class16 == 13 | class16 == 14] <- 7
  class8[class16 == 15 | class16 == 16] <- 8
  val_labels(class8) <- c("Self-employed professionals and large employers" = 1,
                          "Small business owners" = 2,
                          "Technical (semi-)professionals" = 3,
                          "Production workers" = 4,
                          "(Associate) managers" = 5,
                          "Clerks" = 6,
                          "Socio-cultural (semi-)professionals" = 7,
                          "Service workers" = 8)
  var_label(class8) <- "Respondent's Oesch class position - 8 classes"
  
  
  class5 <- NA
  class5[class16 <= 2 | class16 == 5 | class16 == 9 | class16 == 13] <- 1
  class5[class16 == 6 | class16 == 10 | class16 == 14] <- 2
  class5[class16 == 3 | class16 == 4] <- 3
  class5[class16 == 7 | class16 == 11 | class16 == 15] <- 4
  class5[class16 == 8 | class16 == 12 | class16 == 16] <- 5
  val_labels(class5) <- c("Higher-grade service class" = 1,
                          "Lower-grade service class" = 2,
                          "Small business owners" = 3,
                          "Skilled workers" = 4,
                          "Unskilled workers" = 5)
  var_label(class5) <- "Respondent's Oesch class position - 5 classes"
  
  
  ####################################################################################################
  # Convert all labelled variables (haven_labelled class) to factors
  # To convert a specific labelled variable to a factor: class16 <- to_factor(class16, drop_unused_labels = TRUE)
  # The levels argument allows to specify what should be used as the factor levels, the labels (default), the values or the labels prefixed with values
  # Example with the labels prefixed with values: class16 <- to_factor(class16, drop_unused_labels = TRUE, levels = "p")
  ####################################################################################################
  
  #d <-  unlabelled(d, drop_unused_labels = TRUE)
  return(list(class5, selfem_mainjob))
  
  ##################################
  # End 1
  ##################################
}


# Altres:
dominancia_origen <- function(pare, mare){
  pare <- pare %>% unlist %>% as.numeric
  mare <- mare %>% unlist %>% as.numeric
  oc <- data.frame(pare, mare)
  oc$mare[is.na(oc$mare)] <- 100
  oc$pare[is.na(oc$pare)] <- 100
  d <- (apply(oc, 1, min))
  d[d==100] <- NA
  return(d)
}
