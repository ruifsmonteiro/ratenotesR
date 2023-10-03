

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
