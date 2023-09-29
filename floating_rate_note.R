rm(list=ls())


# leg floater -------------------------------------------------------------

value_date <- as.Date("2023-03-31")

shock_DF_tenor <- "5y"
shock_DF_bp <- 1 #1.0

shock_index_tenor <- "5y"
shock_index_bp <- 1 #1.0

notional <- (-1) * c(500000000,500000000,500000000,500000000,500000000,500000000,500000000,500000000,500000000)

schedule_leg_flt <- as.Date(c("2023-04-11","2023-10-09","2024-04-08","2024-10-07","2025-04-07","2025-10-07","2026-04-07","2026-10-07","2027-04-07"))
CF_coupon_days_leg_flt <- c(186,181,182,182,182,183,182,183,182)
schedule_fixing_leg_flt <- as.Date(c("2022-10-05","2023-04-05","2023-10-05","2024-04-04","2024-10-03","2025-04-03","2025-10-03","2026-04-01","2026-10-05"))
fixed_rates_leg_flt <- c(1.731)
names(fixed_rates_leg_flt) <- c("2022-10-05")

# discount rates (EUR1D)
discount_rates <- c(2.91797049,2.918611793,2.918838987,2.919310972,3.009349742,3.07470749,3.142735759,3.200584315,3.246087577,3.287794299,3.314641559,3.337570143,3.35310209,3.358321919,3.358192409,3.305170817,3.290520023,3.188395715,3.009312629,2.880350079,2.806382029,2.764895849,2.743897018,2.734682047,2.742164158,2.755486348,2.776054461,2.796605979,2.822141058,2.703219374,2.531881792,2.382081711)
discount_rates_tenors <- c(12,19,26,35,67,96,129,158,188,221,249,280,312,340,371,462,556,738,1104,1467,1832,2197,2562,2929,3294,3658,4023,4388,5485,7312,9139,10965)

# index rates (EU6MN) # se fosse floater to floater teriamos uma 2a index_rates
index_rates <- c(2.990604642,2.990665902,2.990702658,2.996763467,3.005324243,3.014269848,3.054626569,3.111755117,3.177901221,3.234043703,3.309259154,3.384250258,3.44875536,3.503715906,3.55792218,3.601884099,3.626736781,3.50601598,3.374023913,3.205511716,3.078327141,2.99734076,2.955428073,2.928305062,2.913279866,2.912041082,2.916364368,2.926037579,2.936121268,2.932336625,2.929069989,2.925743837,2.892498199,2.85865317,2.825225753,2.791338475,2.757899521,2.717763276,2.677735709,2.637656799,2.597528276,2.556854312,2.516766189,2.476647644,2.436399342,2.395916254,2.355675728)
index_rates_tenors <- c(3,4,5,11,18,25,34,66,95,126,157,187,220,248,279,311,339,370,735,1103,1466,1831,2196,2561,2926,3293,3657,4022,4387,4753,5121,5484,5848,6214,6579,6948,7311,7675,8040,8405,8770,9138,9502,9866,10231,10597,10962)

crr2_tenors_days <- c(91,183,366,731,1096,1827,3653,5479,7305,10958)
crr2_tenors <- c("3m","6m","1y","2y","3y","5y","10y","15y","20y","30y")

discount_basis <- 365 #360.0


### floating rate note ###

delta_floating_rate_note <- function(value_date, shock_DF_tenor, shock_DF_bp, shock_index_tenor, shock_index_bp, notional, schedule_leg_flt, CF_coupon_days_leg_flt, schedule_fixing_leg_flt, fixed_rates_leg_flt, discount_rates, discount_rates_tenors, index_rates, index_rates_tenors, crr2_tenors_days, crr2_tenors, discount_basis) {
  
  df_irs_flt <- data.frame(notional=notional,
                           payment_date=schedule_leg_flt,
                           fixing_date=schedule_fixing_leg_flt,
                           coupon_days = CF_coupon_days_leg_flt)
  
  df_irs_flt$payment_nbr_days <- as.numeric(df_irs_flt$payment_date - value_date)
  
  df_irs_flt$rate_type <- NA
  df_irs_flt[df_irs_flt$fixing_date <= value_date,]$rate_type <- "Fixed"
  df_irs_flt[df_irs_flt$fixing_date > value_date,]$rate_type <- "Variable"
  
  
  df_irs_flt$index_rate <- NA
  df_irs_flt[as.character(df_irs_flt$fixing_date) %in% names(fixed_rates_leg_flt), ]$index_rate <- fixed_rates_leg_flt / 100.0
  
  # calculate discount factors crr2 basis
  
  df_discount <- data.frame(tenor=discount_rates_tenors,
                            rate=discount_rates)
  
  df_discount_crr2 <- data.frame(tenor=crr2_tenors,
                                 tenors_days=crr2_tenors_days)
  
  df_discount_crr2$rate <- (approx(x = df_discount$tenor, y = df_discount$rate, xout = df_discount_crr2$tenors_days, method = "linear", rule = 2))$y
  # apply shock
  df_discount_crr2$rate_shocked <- df_discount_crr2$rate
  df_discount_crr2[df_discount_crr2$tenor == shock_DF_tenor,]$rate_shocked <- df_discount_crr2[df_discount_crr2$tenor == shock_DF_tenor,]$rate + shock_DF_bp/100.0
  
  
  # calculate index rates crr2 basis
  
  df_index <- data.frame(tenor=index_rates_tenors,
                         rate=index_rates)
  
  df_index_crr2 <- data.frame(tenor=crr2_tenors,
                              tenors_days=crr2_tenors_days)
  
  df_index_crr2$rate <- (approx(x = df_index$tenor, y = df_index$rate, xout = df_index_crr2$tenors_days, method = "linear", rule = 2))$y
  # apply shock
  df_index_crr2$rate_shocked <- df_index_crr2$rate
  df_index_crr2[df_index_crr2$tenor == shock_index_tenor,]$rate_shocked <- df_index_crr2[df_index_crr2$tenor == shock_index_tenor,]$rate + shock_index_bp/100.0
  
  
  
  # project index rates from crr2 basis onto Cash Flows dates and calculate Fwd rates from discount factors of index (swap) curve
  
  aux_nbr_days <- as.numeric(df_irs_flt$payment_date - value_date)
  
  ### no shock
  aux_index_rates <- (approx(x = df_index_crr2$tenors_days, y = df_index_crr2$rate, xout = aux_nbr_days, method = "linear", rule = 2))$y
  aux_index_rates <- aux_index_rates / 100.0
  # calculate fwd rates based on discount factors
  aux_DF_index <- 1.0 / exp(aux_index_rates*(df_irs_flt$payment_nbr_days/discount_basis))
  aux_DF_index_lagged <- c(NA, aux_DF_index[1:(length(aux_DF_index)-1)])
  DF_ratio <- aux_DF_index_lagged / aux_DF_index
  DF_ratio <- DF_ratio[!is.na(DF_ratio)]
  rates_Fwd_index_leg_flt_crr2 <- (DF_ratio -1) * (1 / (as.numeric(df_irs_flt[df_irs_flt$rate_type=="Variable",]$coupon_days) / 365)  ) # 360 ou 365??
  df_irs_flt[df_irs_flt$rate_type == "Variable",]$index_rate <- rates_Fwd_index_leg_flt_crr2
  # or:
  # df_irs_flt[!(as.character(df_irs_flt$fixing_date) %in% names(fixed_rates_leg_flt)), ]$index_rate <- rates_Fwd_index_leg_flt_crr2
  
  ### with shock
  df_irs_flt$index_rate_shocked <- df_irs_flt$index_rate
  aux_index_rates <- (approx(x = df_index_crr2$tenors_days, y = df_index_crr2$rate_shocked, xout = aux_nbr_days, method = "linear", rule = 2))$y
  aux_index_rates <- aux_index_rates / 100.0
  # calculate fwd rates based on discount factors
  aux_DF_index <- 1.0 / exp(aux_index_rates*(df_irs_flt$payment_nbr_days/discount_basis))
  aux_DF_index_lagged <- c(NA, aux_DF_index[1:(length(aux_DF_index)-1)])
  DF_ratio <- aux_DF_index_lagged / aux_DF_index
  DF_ratio <- DF_ratio[!is.na(DF_ratio)]
  rates_Fwd_index_leg_flt_crr2 <- (DF_ratio -1) * (1 / (as.numeric(df_irs_flt[df_irs_flt$rate_type=="Variable",]$coupon_days) / 365)  ) # 360 ou 365??
  df_irs_flt[df_irs_flt$rate_type == "Variable",]$index_rate_shocked <- rates_Fwd_index_leg_flt_crr2
  
  # calculate Cash Flows
  df_irs_flt$CF <- NA
  df_irs_flt[df_irs_flt$rate_type == "Fixed",]$CF <- df_irs_flt[df_irs_flt$rate_type == "Fixed",]$notional * df_irs_flt[df_irs_flt$rate_type == "Fixed",]$index_rate * (df_irs_flt[df_irs_flt$rate_type == "Fixed",]$coupon_days/360)
  df_irs_flt[df_irs_flt$rate_type == "Variable",]$CF <- df_irs_flt[df_irs_flt$rate_type == "Variable",]$notional * df_irs_flt[df_irs_flt$rate_type == "Variable",]$index_rate * (df_irs_flt[df_irs_flt$rate_type == "Variable",]$coupon_days/365)
  
  df_irs_flt$CF_index_shocked <- NA
  df_irs_flt[df_irs_flt$rate_type == "Fixed",]$CF_index_shocked <- df_irs_flt[df_irs_flt$rate_type == "Fixed",]$notional * df_irs_flt[df_irs_flt$rate_type == "Fixed",]$index_rate_shocked * (df_irs_flt[df_irs_flt$rate_type == "Fixed",]$coupon_days/360)
  df_irs_flt[df_irs_flt$rate_type == "Variable",]$CF_index_shocked <- df_irs_flt[df_irs_flt$rate_type == "Variable",]$notional * df_irs_flt[df_irs_flt$rate_type == "Variable",]$index_rate_shocked * (df_irs_flt[df_irs_flt$rate_type == "Variable",]$coupon_days/365)
  
  
  # project discount rates from crr2 basis onto Cash Flows dates and calculate discount factors
  df_irs_flt$DF_rate <- (approx(x = df_discount_crr2$tenors_days, y = df_discount_crr2$rate, xout = df_irs_flt$payment_nbr_days, method = "linear", rule = 2))$y
  df_irs_flt$DF_rate <- df_irs_flt$DF_rate / 100.0
  
  df_irs_flt$DF_rate_shocked <- (approx(x = df_discount_crr2$tenors_days, y = df_discount_crr2$rate_shocked, xout = df_irs_flt$payment_nbr_days, method = "linear", rule = 2))$y
  df_irs_flt$DF_rate_shocked <- df_irs_flt$DF_rate_shocked / 100.0
  
  df_irs_flt$DF <- NA
  df_irs_flt$DF <- 1.0 / exp(df_irs_flt$DF_rate*(df_irs_flt$payment_nbr_days/discount_basis))
  
  df_irs_flt$DF_shocked <- NA
  df_irs_flt$DF_shocked <- 1.0 / exp(df_irs_flt$DF_rate_shocked*(df_irs_flt$payment_nbr_days/discount_basis))
  
  df_irs_flt$NPV <- NA
  df_irs_flt$NPV <- df_irs_flt$CF * df_irs_flt$DF
  
  df_irs_flt$NPV_index_shocked <- NA
  df_irs_flt$NPV_index_shocked <- df_irs_flt$CF_index_shocked * df_irs_flt$DF
  
  df_irs_flt$NPV_DF_shocked <- NA
  df_irs_flt$NPV_DF_shocked <- df_irs_flt$CF * df_irs_flt$DF_shocked
  
  NPV_flt <- sum(df_irs_flt$NPV)
  
  NPV_flt_DF_shocked <- sum(df_irs_flt$NPV_DF_shocked)
  
  
  NPV_index_shocked_flt <- sum(df_irs_flt$NPV_index_shocked)
  delta_NPV_index_shocked_flt <- (NPV_index_shocked_flt - NPV_flt) #/ 0.0001
  delta_index_shocked_flt <- (NPV_index_shocked_flt - NPV_flt) / 0.0001
  
  delta_DF_shocked_flt <- (NPV_flt_DF_shocked - NPV_flt) #/ 0.0001
  
  deltas <- list(delta_index=delta_NPV_index_shocked_flt, delta_DF=delta_DF_shocked_flt)
  return(deltas)
  
}


delta_flt <- delta_floating_rate_note(value_date, 
                                      shock_DF_tenor, 
                                      shock_DF_bp, 
                                      shock_index_tenor, 
                                      shock_index_bp, 
                                      notional, 
                                      schedule_leg_flt, 
                                      CF_coupon_days_leg_flt, 
                                      schedule_fixing_leg_flt, 
                                      fixed_rates_leg_flt, 
                                      discount_rates, 
                                      discount_rates_tenors, 
                                      index_rates, 
                                      index_rates_tenors, 
                                      crr2_tenors_days, 
                                      crr2_tenors, 
                                      discount_basis)

# print(delta_flt$delta_index/0.0001)
# print(delta_flt$delta_DF/0.0001)

# print(delta_NPV_index_shocked_flt)
# print(delta_DF_shocked_flt)
# 
# print(delta_NPV_index_shocked_flt/0.0001)
# print(delta_DF_shocked_flt/0.0001)