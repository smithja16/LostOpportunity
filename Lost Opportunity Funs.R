
###### UTILITY FUNCTION - estimates profit at locations from a specifed vessel departure port
## See published article for equation (Smith et al (accepted) J. Appl. Ecology)
## Splits variable costs into distance (fuel) and time (crew) costs

exp_profit_port2U <- function(sword_layer, port_start, sword_price, cost_per_km, cost_per_h,
                              trip_days, set_dur, travel_speed, km_day) {
  # sword_layer <- the data frame icnluding swordfish CPUE; a new 'profit' column is added
  # port_start <- calculated profits are relative to this numbered departure port
  # sword_price <- price (USD per fish)
  # cost_per_km <- vessel distance costs (fuel, oil; USD per km)
  # cost_per_h <- vessel time cost (crew, food; USD per h)
  # trip_days <- number of sets in a trip (i.e. trip duration)
  # set_dur <- set duration (12h - same as predicted CPUE values)
  # travel_speed <- vessel speed between sets (km/h)
  # km_day <- mean distance travelled between sets

  dist_all_ports <- ref_data[,which(colnames(ref_data)=="dist_P1"):which(colnames(ref_data)=="dist_P11")]
  dist_Pxx <- dist_all_ports[,port_start]    #distances to all cells from this departure port
  
  #Calculate profit for all cells (revenue - costs)
  exp_profit <- sword_price * sword_layer$CPUE * trip_days -  #revenue
    cost_per_km * ((dist_Pxx + apply(dist_all_ports, 1, FUN=min)) + (trip_days-1)*km_day) -  #distance costs
    cost_per_h * ((trip_days*set_dur) + ((trip_days-1)*(24-set_dur)) + (dist_Pxx + apply(dist_all_ports, 1, FUN=min))/travel_speed)  #time costs

  #the 'dist_Pxx + apply(dist_all_ports)' bit accounts for the ability to return to nearest port not just port of origin

  #add expected profit data to data frame
  if (length(names(sword_layer)[names(sword_layer)=="exp_profit_Px"]) > 0) {  #first remove any previous attachments
    sword_layer <- subset(sword_layer, select = -exp_profit_Px)
  }
  
  sword_layer <- cbind(sword_layer, exp_profit_Px=exp_profit)
  return(sword_layer)
  
}
########################################################################################

##### Function to summarise the data reported by the simulation, for subsequent plotting

summarise_utility <- function(data) {
  
  data$month <- month(data$date)
  data$day_d <- day(data$date)
  
  end_m <- 8; end_d <- 31  #end date of the LCA closure
  
  value_stats <- data.frame(season=rep(1:length(years_s), each=length(max_daysX)*length(portsX)),
                            max_days=0, port=0, tot_value=0, LH_value_LH=0, perc_value=0, 
                            mean_LH_area=0, mean_tot_area=0)
  idx <- 0
  for (ss in 1:length(years_s)) {
    saved_utilityX <- data[data$season==ss,]
    for (pp in portsX) {
      for (mm in max_daysX) {
        idx <- idx+1
        value_stats$season[idx] <- ss; value_stats$max_days[idx] <- mm; value_stats$port[idx] <- pp
        #sum of all positive utility from Port pp DURING whole season
        saved_utilityX_mm <- saved_utilityX[saved_utilityX$max_days==mm,]
        saved_utilityX_mm[is.na(saved_utilityX_mm)] <- 0  #some NAs and NaNs, due to dividing by zero (when there is no profitable area in whole EEZ)
        value_stats$tot_value[idx] <- sum(saved_utilityX_mm[,which(names(saved_utilityX_mm)==paste0("port",pp,"_tot_value"))])
        value_stats$mean_tot_area[idx] <- mean(saved_utilityX_mm[,which(names(saved_utilityX_mm)==paste0("port",pp,"_area"))])
        #sum of all positive utility from Port pp in LH area, DURING LH closure
        saved_utilityX_mm_LH <- saved_utilityX_mm[which.min(saved_utilityX_mm$date):which(saved_utilityX_mm$month==end_m & saved_utilityX_mm$day_d==end_d),]
        value_stats$LH_value_LH[idx] <- sum(saved_utilityX_mm_LH[,which(names(saved_utilityX_mm_LH)==paste0("port",pp,"_LH_value"))])
        value_stats$perc_value[idx] <- (value_stats$LH_value_LH[idx]/value_stats$tot_value[idx])*100  #percentage
        value_stats$mean_LH_area[idx] <- mean(saved_utilityX_mm_LH[,which(names(saved_utilityX_mm_LH)==paste0("port",pp,"_area"))] *
                                                saved_utilityX_mm_LH[,which(names(saved_utilityX_mm_LH)==paste0("port",pp,"_area_prop"))])  #'median' does not work here, as it is often zero as few days may have profitable areas
      }  #this calculates the percentage of total profitability (whole season) by the profitability in the LH closure (LH season only)
    }
  }
  return(value_stats)
  
}
########################################################################################
