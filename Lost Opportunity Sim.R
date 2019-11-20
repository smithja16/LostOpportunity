
############### Lost Economic Opportunity Simulation - Smith et al 2019 (UCSC, NOAA) ###############

##### This is a partial example of the full simulation reported in Smith et al (in press) J. Appl. Ecology
##### It was developed using the California drift gillnet swordfish fishery (DGN), and much of the data here relates to that fishery

## NOTE: to run this script, you need to download the swordfish CPUE rasters, the functions R script, and the two RDS data objects
## NOTE: Smith et al simulated 19 fishing seasons, but we only include 3 (1991-1993) in this example
## NOTE: Smith et al used both BRT and GAMM versions of swordfish CPUE, here we provide only the GAMM


## Load packages
library(raster)
library(lubridate)

## Specify directory location of CPUE rasters (only if you want to change it)
raster_loc <- "./CPUE_rasters"

## Load functions
source(file = "./Lost Opportunity Funs.R")

## Load data (variable swordfish price, reference data)
closures <- readRDS("./closure_dates.RDS")  #whether a closure is active (1) or inactive (0) for every day of year
variable_prices <- readRDS("./variable_prices.RDS")  #these are the daily mean swordfish prices (USD per pouns and fish)
ref_data <- readRDS("./ref_data.RDS")  
#ref_data is a summary of data relevant to each cell of the CPUE rasters (method not shown here):
#'area' is each raster cell's area (km2), as calculated using raster::area
#'EEZ' is whether each raster cell is inside (1) or outside (0) the EEZ; this applies to all 15 closures, including 'Logger' which is the LCA closure
#'effort99' is whether each raster cell is inside the 95% effort contour (don't let the '99' fool you)
#'dist_P1' etc is the distance from Port 1 to every cell (up to Port 11)


## Set up the SIMULATION
# specify dates (don't change)
start_y <- 1991; end_y <- 1993  #start years of fishing seasons to simulate (here three seasons)
date_start <- "1991-07-01"  #start of FIRST DGN fishing season
date_end <- "1992-01-31"  #end of FIRST DGN fishing season
date_string <- seq(as.Date(date_start), as.Date(date_end), by=1)
season_len <- length(date_string)  #length of each season

# create date vector
years_s <- seq(start_y, end_y, by=1)  #join fishing seasons together
date_string <- as.Date("1900-01-01")  #prime date format
for (s in 1:length(years_s)) {  #n fishing seasons (here, 1991-92, 1992-93)
  year_s <- years_s[s]
  date_seq_s <- seq(as.Date(ISOdate(year_s, month(date_start), day(date_start))),
                    as.Date(ISOdate(year_s+1, month(date_end), day(date_end))), by=1)
  date_string <- as.Date(c(date_string, date_seq_s))
}
date_string <- date_string[-1]  #delete priming date
t_days <- length(date_string)  #number of days for entire simulation

# specify simulation details
eez_eff99 <- "Eez"  #"Eez" to use area inside EEZ, or "Eff99" for that area only inside the 95% observer effort contour
portsX <- c(1:5)  #which ports to simulate vessels leaving from
max_daysX <- c(2:6)  #trip lengths to simulate (number of sets per trip)
var_price <- "y"  #"y" use variable swordfish price, "n" use constant mean price
sword_price <- 525  #the constant mean swordfish price (USD per fish, if above is "n")

## Create file to save data
saved_utility <- data.frame(day=rep(seq(1:t_days),length(max_daysX)), date=rep(date_string,length(max_daysX)),
                            max_days=rep(max_daysX, each=t_days), 
                            port1_area=0, port1_area_prop=0, port1_med_value=0, port1_tot_value=0, port1_LH_value=0,
                            port2_area=0, port2_area_prop=0, port2_med_value=0, port2_tot_value=0, port2_LH_value=0,
                            port3_area=0, port3_area_prop=0, port3_med_value=0, port3_tot_value=0, port3_LH_value=0,
                            port4_area=0, port4_area_prop=0, port4_med_value=0, port4_tot_value=0, port4_LH_value=0,
                            port5_area=0, port5_area_prop=0, port5_med_value=0, port5_tot_value=0, port5_LH_value=0,
                            season=rep(rep(1:(end_y-start_y+1), each=season_len),length(max_daysX)))

## RUN SIMULATION
idx <- 0
pb <- txtProgressBar(min=0, max=nrow(saved_utility)*length(portsX), style=3)

for (tt in 1:t_days) {  #begin looping through every date in simulated period
  
  sword_layer_t <- raster(paste0(raster_loc,"/sf_GAMM_NB_P1_",date_string[tt],".grd"))  #day t's predicted mean swordfish catch per 12h set
  
  sword_CPUE_t <- raster::extract(sword_layer_t, seq(1,sword_layer_t@ncols*sword_layer_t@nrows, 1))  #extract CPUE values for every cell
  sword_data_t <- cbind(data.frame(CPUE=sword_CPUE_t),ref_data)
  
  #find out which closures are active on this date
  which_close <- closures[closures$Day==day(date_string[tt]) & closures$Month_n==month(date_string[tt]),5:ncol(closures)]  #find which closures active on this day of year
  which_close <- which_close[,which_close==1]
  which_close <- which_close[!names(which_close) %in% c("Logger")]  #exclude LCA closure, which is dealt with separately
  which_close_n <- names(which_close)
  
  #if variable price is used, calculate mean price on this date
  if (var_price == "y") {
    month_tt <- month(date_string[tt])
    day_tt <- day(date_string[tt])
    dec_month <- month_tt + day_tt/days_in_month(month_tt)
    sword_price <- variable_prices$USD_per_fish[which.min(abs(variable_prices$month - dec_month))]  #the seasonally varying price
  }
  
  for (pp in portsX) {  #for each departure port
    
    for (mm in max_daysX) {  #and for each trip duration
      idx <- idx+1
      setTxtProgressBar(pb, idx)

      ## Run the Utility Function (this adds a variable called 'exp_profit_Px' to data frame, which is the expected profit at each cell, given departure port P)
      utility_data1_t <- exp_profit_port2U(sword_layer=sword_data_t, port_start=pp,
                                            sword_price=sword_price, cost_per_km=2.3, cost_per_h=22.25,
                                            trip_days=mm, set_dur=12,
                                            travel_speed=15, km_day=40)

      utility_data1_tX <- na.omit(utility_data1_t)

      cell_cut_t <- utility_data1_tX[,c("cell_num",which_close_n)]
      cell_cut_t <- cell_cut_t[apply(cell_cut_t[,2:ncol(cell_cut_t)], 1, FUN=max)==1,1]  #if a cell is in any of these closures (max==1), then store cell_num
      utility_data1_tX <- utility_data1_tX[!utility_data1_tX$cell_num %in% cell_cut_t,]  #keep only the cells which are OUTSIDE these closures

      if (eez_eff99 == "Eez") {
         utility_data1_tX <- utility_data1_tX[utility_data1_tX$exp_profit_Px>=0 & utility_data1_tX$EEZ==1,]  #keep only profitable cells inside EEZ
      }
      if (eez_eff99 == "Eff99") {
        utility_data1_tX <- utility_data1_tX[utility_data1_tX$exp_profit_Px>=0 & utility_data1_tX$EEZ==1,]  #keep only profitable cells inside EEZ
        utility_data1_tX <- utility_data1_tX[utility_data1_tX$exp_profit_Px>=0 & utility_data1_tX$effort99==1,]  #AND keep only profitable cells inside E95% contour
      }
      area1_tX <- sum(utility_data1_tX$area)  #total profitable area (km2)
      area1_LH_tX <- sum(utility_data1_tX$area[utility_data1_tX$Logger==1])  #profitable area inside the LCA (km2)
      prop_area1_LH_tX <- area1_LH_tX/area1_tX  #proportion of profitable area inside LCA
      med_value_tX <- median(utility_data1_tX$exp_profit_Px)
      tot_value_tX <- sum(utility_data1_tX$exp_profit_Px)  #value of total profitable area (USD)
      LH_value_tX <- sum(utility_data1_tX$exp_profit_Px[utility_data1_tX$Logger==1])  #value of LCA profitable area (USD)

      #Save Data in 'saved_utility' data frame
      header1 <- paste0("port",pp,"_area"); header2 <- paste0("port",pp,"_area_prop"); header3 <- paste0("port",pp,"_med_value")
      header4 <- paste0("port",pp,"_tot_value"); header5 <- paste0("port",pp,"_LH_value")
      saved_utility[saved_utility$day==tt & saved_utility$max_days==mm,
                    which(names(saved_utility)==header1)] <- area1_tX
      saved_utility[saved_utility$day==tt & saved_utility$max_days==mm,
                    which(names(saved_utility)==header2)] <- prop_area1_LH_tX
      saved_utility[saved_utility$day==tt & saved_utility$max_days==mm,
                    which(names(saved_utility)==header3)] <- med_value_tX
      saved_utility[saved_utility$day==tt & saved_utility$max_days==mm,
                    which(names(saved_utility)==header4)] <- tot_value_tX
      saved_utility[saved_utility$day==tt & saved_utility$max_days==mm,
                    which(names(saved_utility)==header5)] <- LH_value_tX

    }
    
  }
  
}  #end [will take 40+ mins for 3 seasons]


## SUMMARISE RESULTS
summary_data <- summarise_utility(data=saved_utility)
  

## REPORT results

# 1) Calculate mean lost economic opportunity (%; as in Table 2 of Smith et al (in press))
mean(summary_data$perc_value)  #this is the mean impact (%) from 3 seasons, 5 ports, and trip durations 2-6


# 2) Plot trajectory of total and LCA value (sum of profit in profitable area) for a specific season (as in Fig. 2 of Smith et al (in press))
#this is from dpearture port 1 (Sand Diego); black = total value, red = LCA value
season_plot <- 3  #specify season (in this example, from 1-3)
max_days_plot <- 3  #trip duration to plot (in this example, from 2-6)
saved_utilityX <- saved_utility[saved_utility$season==season_plot &
                                  saved_utility$max_days==max_days_plot,
                                c(which(names(saved_utility)=="date"),
                                  which(names(saved_utility)=="port1_tot_value"),
                                  which(names(saved_utility)=="port1_LH_value"))]
plot(saved_utilityX[,1], saved_utilityX[,2], type="l",
     ylab="Value (sum of profit in profitable area, USD)", xlab="",
     main=paste("Season =",years_s[season_plot]))
lines(saved_utilityX[,1], saved_utilityX[,3], col="red") #LH value
abline(v=as.Date(paste0(years_s[season_plot],"-08-31")), lty=2, col="blue") #end date of LCA


# 3) Lost Economic Opportunity, for 9 combinations of departure port (P) and trip duration (D) (as in Fig. 3 of Smith et al (in press))
par(mfrow=c(3,3), mar=c(3,4,2,2.5))
for (pp in c(1,3,5)) {  #the 3 departure ports to show
  for (mm in c(2,4,6)) {  #the 3 trip durations to show 
    summary_dataX <- summary_data[summary_data$max_days==mm & summary_data$port==pp,]
    barplot(summary_dataX$perc_value, names.arg=years_s, 
            main=paste0("P=",pp,"; D=",mm,"; GAMM"),
            ylab="L.E.O. (%)", xlab="", ylim=c(0,3), xlim=c(0,3.8), las=2, axisnames=F)
    axis(1,at=c(0.7,1.9,3.1), labels=years_s, tcl=-0.3, las=2)
    box()

  }
}
par(mfrow=c(1,1))

