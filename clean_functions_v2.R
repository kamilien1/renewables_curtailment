library(xlsx)
library(dplyr)
library(lubridate)
library(ggplot2)
# round two, cleanup version



# subset a gridmodel
# if the grid model exists, subset it
# if it doesn't, default to default_grid_model and print a warning message
# filename = the name of the data frame that we are 
# if default grid model is ALSO unavailable, default to JJT and print another
# warning message!
subset_gridmodel <- function(df, gridmodel, filename, default_grid_model = 'jjt'){
    library(dplyr)
    
    models <- unique(df$grid_model)
    if(!(gridmodel %in% models)){
        print(cat('WARNING: grid model \"',gridmodel,'\" not found for file \"',filename,'\". Using ', 
                  default_grid_model,' model instead. ', sep=''))
        gridmodel = default_grid_model
    }
    df_out <- subset(df, grid_model==gridmodel & complete.cases(df))
    if(dim(df_out)[1]==0){
        print(cat('DOUBLE WARNING. The backup grid model of ',gridmodel,' is not available. Switching to JJT grid model!',sep=''))
        df_out <- subset(df, grid_model=='jjt' & complete.cases(df))    
    }
    print(paste('Data file:',filename))
    print(tail(df_out))
    df_out
}



# take a 24 hour profile
# there are 'peak' and 'regular' profiles ONLY
# create a 1-year, hourly profile (8760 hours, leap year dealt with later)
create_transmission_profile <- function(transmission_schedule, peak_season = c(7,8)) {
    
    library(lubridate)
    library(dplyr)
    
    # make one year of data
    one_year <- seq(from=as.POSIXlt('2014-01-01 00:00'),to=as.POSIXlt('2014-12-31 23:00'),by='hour')
    one_year <- data.frame(one_year)
    one_year$month <- month(one_year$one_year)
    one_year$hour <- hour(one_year$one_year)
    
    # split up schedule by peak/regular seasons
    one_year$season <- ifelse(one_year$month %in% peak_season, 'peak','regular')
    
    # grow by month, hour
    output <- left_join(one_year[,c(2:4)],transmission_schedule[,c(2:4)],by=c('season','hour'))
    
    print('Transmission profile created. 24-hour season data turned into one year data.')
    print(tail(output))
    
    output
}



# Make a 1 year hourly solar profile (8760 hours)
# the output is kwh per kw
create_solar_profile <- function(solar_month, solar_day){
    
    library(lubridate)
    library(dplyr)
    
    # change the names to understand them
    names(solar_month) <- c('grid_model','month','cf_month','month_rank')
    names(solar_day) <- c('grid_model', 'hour','cf_hour','day_cf_rank','pct_daily')
    
    # merge the right columns of the table
    # use month rank
    solar_table <- merge(solar_month[,c('month','cf_month')],solar_day[,c('hour','pct_daily')])
    # subtract an hour to match up 
    solar_table$hour <- solar_table$hour-1
    
    # add kwh per kw scale
    # in create hourly profile, we will scale by iCap solar
    solar_table$kWh_kW <- with(solar_table,cf_month*pct_daily*24)
    
    # make one year of data
    one_year <- seq(from=as.POSIXlt('2014-01-01 00:00'),to=as.POSIXlt('2014-12-31 23:00'),by='hour')
    one_year <- data.frame(one_year)
    one_year$month <- month(one_year$one_year)
    one_year$hour <- hour(one_year$one_year)
    
    # join only the month, hour, and kwh per kw data
    test <- left_join(one_year[,c(2,3)],solar_table[,c('month','hour','kWh_kW')],by=c('month','hour'))
    print('Solar profile created. 24-hour and 12-month capacity factor data turned into one year data. Multiply by iCap(MW) solar to get hourly output in MW-hours.')
    print(tail(test))
    
    # return the profile
    test
    
}



# hourly profile for all years we care about
# solar, wind scaled appropriately 
create_hourly_renewables_profile <- function(wind, solar, gen_forecast,
                                             start_year='2015',end_year='2020',
                                             wind_scale_factor=1,
                                             solar_scale_factor=1,
                                             offset_wind_hours=0){
    
    #### create an hourly time series ####
    start <- as.POSIXlt(paste(start_year,"-01-01 00:00",sep=''))
    end <- as.POSIXlt(paste(end_year,"-12-31 23:00",sep=''))
    date <- seq(from=start,to=end,by='hour')
    
    library(lubridate)
    library(dplyr)
        
    # we will lengthen by at most lengthen amount
    # in a leapyear, we handle this by taking 1:length(date) 
    lengthen <- ceiling(length(date)/dim(wind)[1])
    
    print(paste('lengthening our data by ',lengthen,'years or 1 year less than that if we have a leap year.'))
    
    # get the iCap forecast data for wind
    wind_forecast <- subset(gen_forecast, type=='wind')
    
    # get the forecast data for the grid model
    solar_forecast <- subset(gen_forecast, type=='solar')
    names(solar_forecast) <- c('grid_model','year','iCap_solar','type')
    
    
    ## TODO
    ## wind offshore, solar thermal
    ## NOTE: most likely this will be an addition to wind, solar profiles 
    ## as iCap 
    
    #### create hourly profile for multiple years
    set.seed(1)
    randomize_wind <- rnorm(as.numeric(length(date)),mean=1,sd=0.015)
    wind_total <- randomize_wind*rep(wind$kWh_kW,lengthen)[1:length(date)]
    
    # offset_wind_hours: 0 to 23 hours that we offset wind by
    offset_start = offset_wind_hours
    if(offset_wind_hours>0){
        offset_wind_hours = offset_wind_hours+1
        start_offset = offset_wind_hours+1
        wind_total <- c(wind_total[start_offset:length(date)],wind_total[1:offset_wind_hours])
    }
    
    
    set.seed(2)
    randomize_solar <- rnorm(as.numeric(length(date)),mean=1,sd=0.015)
    solar_total <- randomize_solar*rep(solar$kWh_kW,lengthen)[1:length(date)]
    
    
    print(paste('solar total NA is',sum(is.na(solar_total))))

    # make a data frame of dates (start to end ymd hour)
    # total wind produced 
    # total solar produced
    # and the year
    df <- data.frame(date,wind_total,solar_total,year=year(date))
    # add the wind forecast
    df_new <- left_join(df,wind_forecast[,2:3],by="year")
    
    # add the solar forecast
    df_new <- left_join(df_new,solar_forecast[,2:3],by="year")
    
    # add coal, natgas
    coal_icap <- subset(gen_forecast,type=='coal')
    
    names(coal_icap)[3] <- "coal_iCap"
    # add coal and natgas to calculate pmin ccp/no ccp
    df_new <- left_join(df_new, coal_icap[,2:3],by="year")

    # natural gas is not always present
    # so make it 0 if it is not
    if(sum(gen_forecast$type %in% 'natgas')>0){
        print("natural gas present")
        natgas_icap <- subset(gen_forecast,type=='natgas')
        names(natgas_icap)[3] <- "natgas_iCap"
        df_new <- left_join(df_new, natgas_icap[,2:3],by="year")
        df_new$natgas_iCap[is.na(df_new$natgas_iCap)] <- 0
    }else{
        df_new$natgas_iCap <- 0
    }
    
    
    # get rid of NA values
    df_new$iCap[is.na(df_new$iCap)] <- 0
    df_new$iCap_solar[is.na(df_new$iCap_solar)] <- 0
    df_new$coal_iCap[is.na(df_new$coal_iCap)] <- 0
    
    
    # iCap, wind total, and scale factor provide the current output
    df_new$gwh_wind_onshore <- with(df_new,iCap*wind_total*wind_scale_factor/1000)
    df_new$gwh_solar <- with(df_new,iCap_solar*solar_total*solar_scale_factor/1000)
    
    
    
    # show the gwh annually produced
    printout <- df_new %>% 
        group_by(year) %>%
        summarise(twh_wind = sum(gwh_wind_onshore)/1000,
                  twh_solar = sum(gwh_solar)/1000)
    print("Summary of wind and solar twh generation by year.")
    print(printout)
    
    # return
    print(paste("Returning a data frame of all wind, solar hourly data for years",start_year,"to",end_year))
    print(tail(df_new))
    print(object.size(df_new), units= "Mb")
    df_new
    
}

# test profile is the hourly wind/solar profile from start to end year
# t_annual_sched is a 1 year, hourly schedule of capacity factor of transmission lines
# t_icap is a forecast of transmission growth, along with the % of 
# power flow IN to the grid and OUTFLOW (1 - inflow) can be calculated
add_hourly_transmission_profile <- function(test_profile, t_annual_sched, t_icap,randomize_t=T,sd_level=0.03){
    library(dplyr)
    set.seed(1)
    
    
    # lengthen annual schedule to right years
    lengthen <- ceiling(dim(test_profile)[1]/dim(t_annual_sched)[1])
    
    # add small randomness of 3% to 6% 
    randomize_transmission <- ifelse(randomize_t,rnorm(as.numeric(dim(test_profile)[1]),mean=1,sd=sd_level),1:as.numeric(dim(test_profile)[1]))
    
    # create transmission schedule for all years
    trans_hourly_allyears <- randomize_transmission*rep(t_annual_sched$trans_cap,lengthen)[1:dim(test_profile)[1]]
    
    # add it to the test profile
    test_profile$trans_profile <- trans_hourly_allyears
    # gw_flow is iCap of transmission
    # inflow_ratio is the % of power going in to the grid, 0 = all power flows out
    test_profile <- left_join(test_profile,t_icap[,c(2:4)],by='year')
    
    print("Forecasted transmission added to hourly wind/solar profile.")
    print(tail(test_profile))
    print(object.size(test_profile), units= "Mb")
    test_profile
}

add_hourly_load_profile <- function(test_profile,
                                    seasonal_hourly_load,
                                    monthly_demand_profile,
                                    annual_demand, 
                                    ccp_season, 
                                    time_period=4,
                                    wind_transmission_share = 0.1,
                                    extra_trans_capacity_renewables = 0,
                                    #extra_trans_capacity_for_wind = 0.1,
                                    solar_transmission_share = 0.1,
                                    #extra_trans_capacity_for_solar = 0.1,
                                    gw_load_base_wind_ratio = 0,
                                    gw_load_base_solar_ratio = 0,
                                    max_option = 1){
    

    library(lubridate)    
    library(dplyr)
    library(caTools)
    
    # add month, day, hour
    test_profile$month <- month(test_profile$date)
    test_profile$day <- day(test_profile$date)
    test_profile$hour <- hour(test_profile$date)
    
    # add the relative peak by month, include the season for later
    test_profile <- left_join(test_profile, monthly_demand_profile[,c('month','relative_peak','season')],by="month")
    
    # add the hourly profile scale by season, include percent load (this is a pct of the peak load 
    # in the peak season)
    test_profile <- left_join(test_profile, seasonal_hourly_load[,c('hour','pct_load','season')],by=c("hour","season"))
    
    # add max and min loads by year
    test_profile <- left_join(test_profile, annual_demand[,c('year','max_gw','min_gw')],by="year")
    
    
    
    # get capacity of transmission (50% to 100%), multiply by iCap transmission
    # multiply by the ratio of iCap flowing out
    # scale it by the amount of wind power that is allowed to be on the power line
    # at that given point in time
    # if there is extra OUTFLOW capacity unused AND we have the option of extra 
    # transmission cap set to TRUE, also let the extra wind flow out
    # in/out trans/sched same means we scale flow DOWNWARD by the 
    # inflow capacity at that hour
    # otherwise, outflow is at 100% available capacity all the time
    
#     # new wind output = wind output less wind transmitted to outside of grid
#     wind_after_transmission <- with(test_profile, gwh_wind_onshore - 
#         # scale transmission by iCap transmission, outflow ratio, and share of renewables
#         # allowed on the transmission line (0 to 100%)
#         trans_profile * gw_flow * (1-inflow_ratio) * wind_transmission_share -
#         # if there is extra unused capacity at that hour available
#         # and we choose to use it, add it 
#         # and allow extra_t_c_for_wind (0% to 100%)
#         extra_trans_capacity_renewables * (1-trans_profile) * gw_flow * (1-inflow_ratio) *
#         extra_trans_capacity_for_wind - 
#         # 0-100% of wind can be removed from baseload
#         # may need to test case for when wind base > demand
#         gw_load_base_wind_ratio * gwh_wind_onshore)
#     
#     
#     
#     # remove negatives (if greater than 0, we have negative wind power flow, 
#     # indicating too much extra transmission was available
#     wind_after_transmission <- ifelse(wind_after_transmission < 0, 0, wind_after_transmission)
#     # add to data frame
#     test_profile$wind_after_transmission <- wind_after_transmission
    
    # multi step process for wind and solar transmission
    # step 1: wind production - wind transmission (don't go larger than wind production #)
    # wind_after_transmission= gwh_wind_onshore - 
    # min(gwh_wind_onshore, trans_profile*gw_flow*(1-inflow_ratio)*wind_transmission_share)
    # do the same for solar
    # step 2: get the ratio of wind to wind+solar or solar/wind+solar 
    # get the extra amount of t capacity available for w/s (as GW in one hour)
    # extra_trans_capacity = extra_trans_capacity_renewables * (1-trans_profile) * gw_flow * (1-inflow_ratio)
    #wind_vs_ws_ratio=ifelse(wind_after_transmission==0 & solar_after_transmission ==0,0,
    #       wind_after_transmission/(wind_after_transmission+solar_after_transmission))
    # step 3: update wind after t, solar after t (part 2)
    # wind_after_transmission = wind_after_transmission - min(wind_after_transmission,
    # wind_vs_ws_ratio*extra_trans_capacity_renewables*(1-trans_profile)*gw_flow*(1-inflow_ratio))
    # step 4: update wind vs ws ratio
    

    # step 1
    # after T = renewables gen - (1-% base gen) - transmission allocation
test_profile <- mutate(test_profile,  wind_after_transmission= gwh_wind_onshore - 
               gw_load_base_wind_ratio*gwh_wind_onshore - trans_profile*gw_flow*(1-inflow_ratio)*wind_transmission_share,
               wind_after_transmission = ifelse(wind_after_transmission<0,0,wind_after_transmission))

test_profile <- mutate(test_profile,  solar_after_transmission= gwh_solar - 
                           gw_load_base_solar_ratio*gwh_solar - trans_profile*gw_flow*(1-inflow_ratio)*solar_transmission_share,
                       solar_after_transmission = ifelse(solar_after_transmission<0,0,solar_after_transmission))

    
    print("Wind after step 1, initial transmission:")
    print(test_profile %>% summarize(wind_after_t = sum(wind_after_transmission)))

    # step 2
    # following the outflow profile, how much of it in GW is alotted for wind+solar
    test_profile <- mutate(test_profile, extra_trans_capacity = extra_trans_capacity_renewables * 
                               (1-trans_profile) * gw_flow * (1-inflow_ratio))
    test_profile <- mutate(test_profile,wind_vs_ws_ratio=ifelse(wind_after_transmission==0 & solar_after_transmission ==0,0,
                                wind_after_transmission/(wind_after_transmission+solar_after_transmission)))
    
    # step 3
    # extra trans available: wind_t - extra gen, scaled by wind vs solar ratio
    test_profile <- mutate(test_profile, wind_after_transmission = wind_after_transmission - 
                                wind_vs_ws_ratio*extra_trans_capacity_renewables*extra_trans_capacity,
                           wind_after_transmission = ifelse(wind_after_transmission<0,0,wind_after_transmission))

    test_profile <- mutate(test_profile, solar_after_transmission = solar_after_transmission - 
                                 (1-wind_vs_ws_ratio)*extra_trans_capacity_renewables*extra_trans_capacity,
                           solar_after_transmission = ifelse(solar_after_transmission<0,0,solar_after_transmission))
    
    print("Wind after step 4,extra transmission:")
    print(test_profile %>% summarize(wind_after_t_extra= sum(wind_after_transmission)))

    # step 4
    # repeate step 2 to update data
    test_profile <- mutate(test_profile, extra_trans_capacity = extra_trans_capacity - wind_after_transmission - solar_after_transmission)
    test_profile <- mutate(test_profile,wind_vs_ws_ratio=ifelse(wind_after_transmission==0 & solar_after_transmission ==0,0,
                               wind_after_transmission/(wind_after_transmission+solar_after_transmission)))
    
    
    
#     # do the same for solar
#     solar_after_transmission <- with(test_profile, 
#      gwh_solar-trans_profile*gw_flow*(1-inflow_ratio) *solar_transmission_share-
#      extra_trans_capacity_renewables*(1-trans_profile)*gw_flow*(1-inflow_ratio)*extra_trans_capacity_for_wind -
#      gw_load_base_solar_ratio*gwh_solar)
#     
#     solar_after_transmission <- ifelse(solar_after_transmission < 0, 0, solar_after_transmission)
#     test_profile$solar_after_transmission <- solar_after_transmission
#     
#     
#     # use this for calculating wind + solar curtailment
#     test_profile$wind_vs_ws_ratio <- with(test_profile,ifelse(wind_after_transmission==0 & solar_after_transmission ==0,0,
#                                   wind_after_transmission/(wind_after_transmission+solar_after_transmission)))
    
    
    # this amount of transmission OUTFLOW capacity is now unavailable due to wind, solar
    # used to calculate coal/nat gas util hours in analysis
    # as the generation data is reduced by this amount
#     t_out_wind_solar_gwh <- test_profile$gwh_solar + 
#         test_profile$gwh_wind_onshore - wind_after_transmission - solar_after_transmission
#     
#     # if its a tiny number, make it be zero
#     t_out_wind_solar_gwh <- ifelse(abs(t_out_wind_solar_gwh)<0.01,0,t_out_wind_solar_gwh)
#     
#     # so that we can calculate coal utilization hours later 
#     test_profile$t_out_wind_solar_gwh <- t_out_wind_solar_gwh
    
    test_profile <- mutate(test_profile, t_out_wind_solar_gwh=gwh_solar + gwh_wind_onshore-
                               wind_after_transmission - solar_after_transmission)
    test_profile <- mutate(test_profile, t_out_wind_solar_gwh = 
                               ifelse(abs(t_out_wind_solar_gwh)<0.01,0,t_out_wind_solar_gwh))    


    # print results for sanity check
    trans_reductions <- test_profile %>% group_by(year) %>% 
        summarize(wind_before = sum(gwh_wind_onshore),
                  wind_after_trans = sum(wind_after_transmission),
                  solar_before = sum(gwh_solar),
                  solar_after_trans=sum(solar_after_transmission),
                  trans_reduction_gwh = sum(t_out_wind_solar_gwh) )
    
    print("Transmission reductions in wind and solar output, and trans capacity outflow reduction:")
    print(trans_reductions)

    # hourly demand is the demand required to be met by baseload generation
    # which is typically coal and natural gas
    # it is is current demand adjusted downward for base load solar/wind and transmission inflow 
    # relative peak for month * pct load during the hour * peak demand
    test_profile$hourly_demand_max_scale <- with(test_profile,relative_peak*pct_load*max_gw -
                                                     # "baseload" wind generation
                                                     gw_load_base_wind_ratio*gwh_wind_onshore -
                                                     # "baseload" solar generation
                                                     gw_load_base_solar_ratio*gwh_solar -
                                                     # inflow transmission
                                                     trans_profile*gw_flow*inflow_ratio)
    
    # get min load
    # need to scale the min demand value upward by value/min(load_for_season)
    min_load_for_season <- seasonal_hourly_load %>% 
        group_by(season) %>%
        summarize(min_val = min(pct_load))
    test_profile <- left_join(test_profile,data.frame(min_load_for_season),by="season")
    
    # scale by month as well
    min_peak_by_month <- min(monthly_demand_profile$relative_peak)
    
    # scale the min profile
    # current demand
    # adjusted downward for base load solar/wind and transmission inflow
    test_profile$hourly_demand_min_scale <- with(test_profile,
             (pct_load/min_val) * (relative_peak/min_peak_by_month) * min_gw -
                 gw_load_base_wind_ratio*gwh_wind_onshore -
                 gw_load_base_solar_ratio*gwh_solar - 
                 trans_profile*gw_flow*inflow_ratio)
    
    # deal with case that transmission inflow is larger than total demand
    test_profile <- mutate(test_profile, trans_curtail_min = ifelse(hourly_demand_min_scale < 0, hourly_demand_min_scale,0))
    test_profile <- mutate(test_profile, hourly_demand_min_scale = ifelse(hourly_demand_min_scale < 0,0,hourly_demand_min_scale))
    test_profile <- mutate(test_profile, trans_curtail_max = ifelse(hourly_demand_max_scale < 0, hourly_demand_max_scale,0))
    test_profile <- mutate(test_profile, hourly_demand_max_scale = ifelse(hourly_demand_max_scale < 0,0,hourly_demand_max_scale))
    
    # verify results
    
    results_printout <- test_profile %>% group_by(year) %>% 
        summarize( min_scale_min = min(hourly_demand_min_scale),
                   max_scale_min=max(hourly_demand_min_scale),
                   min_scale_max = min(hourly_demand_max_scale),
                   max_scale_max=max(hourly_demand_max_scale),
                   min_scale_total=sum(hourly_demand_min_scale),
                   max_scale_total=sum(hourly_demand_max_scale) )
    
    print("After transmisison, wind, and solar impacts on the demand profile")
    print(results_printout)
    
    # add the ccp/non-ccp dates 
    # get start and end months/days
    ccp_start_month <- with(ccp_season,month(date_start))
    ccp_start_day <- with(ccp_season,day(date_start))
    ccp_end_month <- with(ccp_season,month(date_end))
    ccp_end_day <- with(ccp_season,day(date_end))
    
    # now apply to ccp_on 
    test_profile <- mutate(test_profile,ccp_on = 
                              # in chp month and day
                       ifelse( ( (month <= ccp_end_month & day <= ccp_end_day) | 
                                    # or before end month of chp
                                    (month < ccp_end_month) |
                                    # or after chp start month
                                    (month > ccp_start_month) |
                                    # or after the start month and day
                                    (month >=ccp_start_month & day >=ccp_start_day) ) ,
                                    # use the chp value 
                                    TRUE, 
                                    # don't use the chp values
                                    FALSE) )

        
    # the way we calculate max is important
    # option 1: running max value, it looks forward by time_period hours
    # and finds the running max. This value changes hourly
    # option 2: rolling max: every time_period increment, the max value is found
    # it does not differentiate between times, it simply jumps forward by time_period hours
    # option 3: no max -- we assume grid commitment was already taken care of 
    # NOTE: if time period == 1, then option 3 is used by default
    

#     gen_peak_min = ifelse(max_option == 1 & time_period > 1, runmax(test_profile$hourly_demand_min_scale,time_period, endrule='constant',align='left'), 
#                           ifelse(max_option == 2 & time_period > 1, rolling_max(test_profile$hourly_demand_min_scale,time_period),
#                           test_profile$hourly_demand_min_scale))
#     

    # if option 1, use runmax
    gen_peak_max <- if(max_option == 1 & time_period > 1){
        runmax(test_profile$hourly_demand_max_scale,time_period, endrule='constant',align='center')
    } else {
        # if option 2, use rolling_max (slower function)
        if(max_option == 2 & time_period > 1){
            rolling_max(test_profile$hourly_demand_max_scale,time_period)
        }else{
            # timeperiod is less than 2, so let's return the current demand
            test_profile$hourly_demand_max_scale
        }
    }

    # repeat for min vals
    gen_peak_min <- if(max_option == 1 & time_period > 1){
        runmax(test_profile$hourly_demand_min_scale,time_period, endrule='constant',align='center')
    } else {
        if(max_option == 2 & time_period > 1){
            rolling_max(test_profile$hourly_demand_min_scale,time_period)
        }else{
            # timeperiod is less than 2, so let's return the current demand
            test_profile$hourly_demand_min_scale
        }
    }

    test_profile <- mutate(test_profile, 
           gen_peak_max = gen_peak_max,
           gen_peak_min = gen_peak_min )
    
    print("Now we have a good profile set up, ready for curtailment.")
    print(tail(test_profile))
    print(object.size(test_profile), units= "Mb")
    test_profile
    
}






# function to replace my rollapply function failure from above
# this gets the max value in a time window of width
# useful for unit commitment calculations
# NOTE
# TODO: turn this into something like a tapply function
# because the for loop slows down the program enough to annoy me
rolling_max <- function(x,width=4) {
    
    
    # get the length of x
    len = length(x)
    remainder = len%%width
    index <- seq(1,len-remainder,width)
    
    # step every width
    val <- {}
    for (i in index){
        val = c(val,rep(max(x[i:(i+width-1)]),width))    
    }
    
    # take care of uneven remainder
    if (remainder > 0){
        val = c(val,rep(val[len-remainder],remainder))
    }
    
    # return val
    val
}


# r_ccp_coal = 0.4
# r_ccp_ng = 0.9
# pmin_noccp_coal = 0.5
# pmin_ccp_coal = 0.85
# pmax_ccp_coal = 0.95
# pmin_noccp_ng = 0.3
# pmin_ccp_ng = 0.85
# pmax_ccp_ng = 0.95

calculate_curtailment <- function(test_profile, 
                                  spinning_reserve=1.15, 
                                  r_ccp_coal = 0.4,
                                  r_ccp_ng = 0.9,
                                  pmin_noccp_coal = 0.5,
                                  pmin_ccp_coal = 0.85,
                                  pmax_ccp_coal = 0.95,
                                  pmin_noccp_ng = 0.3,
                                  pmin_ccp_ng = 0.85,
                                  pmax_ccp_ng = 0.95){
    
    library(dplyr)
    
    # ccpon_max_scale: ccp on, max output (to scale up power plants in the winter)
    # we will add this to spinning reserve and scale upward
    test_profile <- mutate(test_profile, ccpon_max_scale=(1-pmax_ccp_coal)*(coal_iCap/(coal_iCap+natgas_iCap))*r_ccp_coal+
                               (1-pmax_ccp_ng)*(natgas_iCap/(coal_iCap+natgas_iCap))*r_ccp_ng)
    
    # ccpon_min_scale: ccp on, min output (to find pmin in winter/ccp months)
    # this is the pmin for the winter, typically worse than summer due to CHP
    test_profile <- mutate(test_profile, ccpon_min_scale=(coal_iCap/(coal_iCap+natgas_iCap))*(pmin_ccp_coal*r_ccp_coal+pmin_noccp_coal*(1-r_ccp_coal))+
                               (natgas_iCap/(coal_iCap+natgas_iCap))*(pmin_ccp_ng*r_ccp_ng+pmin_noccp_ng*(1-r_ccp_ng)))
    
    # ccpoff_min_scale: ccp off, min output (to find pmin in summer/non-ccp months)
    # this is the pmin in the summer, typically better than winter due to no CHP
    test_profile <- mutate(test_profile, ccpoff_min_scale = pmin_noccp_coal*(coal_iCap/(coal_iCap+natgas_iCap))+
                               pmin_noccp_ng*(natgas_iCap)/(coal_iCap+natgas_iCap))
    
    
    # scale by spinning reserve the min ccp
    # min power output during ccp season
    #ccpon_min_scale = ccpon_min_scale*spinning_reserve
    test_profile <- mutate(test_profile, ccpon_min_scale = ccpon_min_scale*spinning_reserve)
    # max power output during non-ccp season
    #ccpoff_min_scale = ccpoff_min_scale *spinning_reserve
    test_profile <- mutate(test_profile, ccpoff_min_scale = ccpoff_min_scale*spinning_reserve)
    
    # get min coal output levels for min load profile
    # if ccp is on
    #   get the peak value of the min profile, scale it downward by ccpon_min_scale, and scale upward 
    #   by the ccpon_max_scale (because we have spinning reserves!)
    # else: 
    #   take the peak value of the min profile, 
    #   and scale it downward by ccpoff_min_scale
    
    # for the load profile using min generation
    test_profile <- mutate(test_profile, gen_pmin_min = ifelse(ccp_on, 
                           # if chp, scale down by pmin winter, and up by pmax
                           gen_peak_min * ccpon_min_scale * (1+ccpon_max_scale),
                           # no chp, scale down by pmin non-chp
                           gen_peak_min*ccpoff_min_scale))
    
    
    # if supply > demand, we must curtail coal 
    # NOTE: gen_pmin_min is coal+natgas output AFTER transmission input
    test_profile <- mutate(test_profile, coal_curtail_pmin =
                    ifelse(hourly_demand_min_scale < gen_pmin_min, hourly_demand_min_scale - gen_pmin_min , 0) )
    
    # the amount to curtail, used in the case of solar vs wind ratio curtailment
    test_profile<- mutate(test_profile,curtail_renewables_amount_pmin = hourly_demand_min_scale - gen_pmin_min - 
                              wind_after_transmission - solar_after_transmission) 
    # if demand - gen - renewables is positive, there is no curtailment, so set it to 0
    test_profile <- mutate(test_profile, curtail_renewables_amount_pmin = 
                               ifelse(curtail_renewables_amount_pmin >0,0,curtail_renewables_amount_pmin))
    
  
    # calculate curtailment
    test_profile <- mutate(test_profile, curtail_pmin = 
                               # if demand exceeds generation AND wind, no curtailment
                               ifelse(hourly_demand_min_scale - gen_pmin_min - wind_after_transmission - solar_after_transmission > 0, 0,
                                      # if coal (AFTER accounting for transmission) is curtailed, curtail all wind
                                      ifelse(coal_curtail_pmin < 0, - wind_after_transmission,
                                             # otherwise curtail a portion
                                             wind_vs_ws_ratio*curtail_renewables_amount_pmin  ) ) )
    
    test_profile <- mutate(test_profile, curtail_solar_pmin = 
                               # if demand exceeds generation AND wind, no curtailment
                               ifelse(hourly_demand_min_scale - gen_pmin_min - wind_after_transmission - solar_after_transmission > 0, 0,
                                      # if coal (AFTER accounting for transmission) is curtailed, curtail all wind
                                      ifelse(coal_curtail_pmin < 0, - solar_after_transmission,
                                             # otherwise curtail a portion
                                             (1-wind_vs_ws_ratio)*curtail_renewables_amount_pmin  ) ) )
    
#     # calculate curtailment
#     test_profile <- mutate(test_profile, curtail_pmin = 
#                      # if demand exceeds generation AND wind, no curtailment
#                      ifelse(hourly_demand_min_scale - gen_pmin_min - wind_after_transmission - solar_after_transmission > 0, 0,
#                             # if coal (AFTER accounting for transmission) is curtailed, curtail all wind
#                               ifelse(coal_curtail_pmin < 0, - wind_after_transmission,
#                                      # otherwise curtail a portion
#                                      hourly_demand_min_scale - gen_pmin_min - wind_after_transmission  ) ) )
#     
    


    # get solar pmin

    
    # test that solar curtail+ wind curtail = curtail_pmin
    
    # calculate solar curtailment 
    
#     # gen_pmax_min 
#     # see above comments, all identical except now doing it for peak load instead
    test_profile <- mutate(test_profile,gen_pmax_min = 
                       ifelse(ccp_on,gen_peak_max*ccpon_min_scale*(1+ccpon_max_scale),
                       gen_peak_max*ccpoff_min_scale))
    

    
    # if supply > demand, we must curtail coal 
    
    test_profile <- mutate(test_profile, coal_curtail_pmax =
                               ifelse(hourly_demand_max_scale < gen_pmax_min, hourly_demand_max_scale - gen_pmax_min , 0) )
    
#     # calculate curtailment
#     test_profile <- mutate(test_profile, curtail_pmax = 
#                                # if demand exceeds generation AND wind, no curtailment
#                                ifelse(hourly_demand_max_scale - gen_pmax_min - wind_after_transmission > 0, 0,
#                                       # if coal is curtailed, curtail all wind
#                                       ifelse(coal_curtail_pmax < 0, - wind_after_transmission,
#                                              # otherwise curtail a portion
#                                              hourly_demand_max_scale - gen_pmax_min - wind_after_transmission  ) ) )
    
    # the amount to curtail, used in the case of solar vs wind ratio curtailment
    test_profile<- mutate(test_profile,curtail_renewables_amount_pmax = 
                              hourly_demand_max_scale - gen_pmax_min - wind_after_transmission - solar_after_transmission) 
    # in case this is a positive number, we will not curtail renewables
    test_profile <- mutate(test_profile, curtail_renewables_amount_pmax = 
                               ifelse(curtail_renewables_amount_pmax >0,0,curtail_renewables_amount_pmax))
    
    
    # calculate curtailment
    test_profile <- mutate(test_profile, curtail_pmax = 
                               # if demand exceeds generation AND wind, no curtailment
                               ifelse(hourly_demand_max_scale - gen_pmax_min - wind_after_transmission > 0, 0,
                                      # if coal is curtailed, curtail all wind
                                      ifelse(coal_curtail_pmax < 0, - wind_after_transmission,
                                             # otherwise curtail a portion
                                             wind_vs_ws_ratio*curtail_renewables_amount_pmax  ) ) )
    
    test_profile <- mutate(test_profile, curtail_solar_pmax = 
                               # if demand exceeds generation AND wind, no curtailment
                               ifelse(hourly_demand_max_scale - gen_pmax_min - wind_after_transmission > 0, 0,
                                      # if coal is curtailed, curtail all wind
                                      ifelse(coal_curtail_pmax < 0, - solar_after_transmission,
                                             # otherwise curtail a portion
                                             (1-wind_vs_ws_ratio)*curtail_renewables_amount_pmax  ) ) )
    
        

    # add local base generation after t_in, wind, solar
    test_profile <- mutate(test_profile, 
                           local_base_min = hourly_demand_min_scale - wind_after_transmission - solar_after_transmission -
                               curtail_pmin - curtail_solar_pmin,
                           local_base_max = hourly_demand_max_scale - wind_after_transmission - solar_after_transmission -
                               curtail_pmax - curtail_solar_pmax,
                           t_out_total = trans_profile*gw_flow*(1-inflow_ratio),
                           # total transmission out is 
                           # t_out_total (iCap transmission * schedule (0-100%) * outflow ratio) -
                           # transmission outflow of wind and solar
                           t_out_base_min = t_out_total - t_out_wind_solar_gwh,
                           t_out_base_max = t_out_total - t_out_wind_solar_gwh
                           
                           
                           # if total trans out minus wind+solar t_out is greater than remaining installed capacity (minus demand)
                           # then output is the leftover base generation available, otherwise its the total transmission out available
#                            t_out_base_min = ifelse((t_out_total-t_out_wind_solar_gwh) > (((coal_iCap+natgas_iCap)/1000-local_base_min)),
#                                                    (coal_iCap/1000+natgas_iCap/1000)-local_base_min, t_out_total-t_out_wind_solar_gwh),
#                            t_out_base_max = ifelse((t_out_total-t_out_wind_solar_gwh)>(((coal_iCap+natgas_iCap)/1000-local_base_max)),
#                                                    (coal_iCap/1000+natgas_iCap/1000)-local_base_max, t_out_total-t_out_wind_solar_gwh)
    )

    # print out results
    
    min_peak_by_month <- min(test_profile$relative_peak)
    
    printout <- test_profile %>% group_by(year) %>% 
        summarize(demand_max = round(sum(relative_peak*pct_load*max_gw),2),
                  demand_min = round(sum((pct_load/min_val) * (relative_peak/min_peak_by_month) * min_gw),2),
                  pmin_curtailed = round(sum(curtail_pmin),2),
                  pmax_curtailed = round(sum(curtail_pmax),2),
                  wind_gwh = sum(gwh_wind_onshore),
                  curtail_pct_min_profile = round(100*abs(pmin_curtailed)/wind_gwh,2),
                  curtail_pct_max_profile = round(100*abs(pmax_curtailed)/wind_gwh,2),
                  wind_vs_load_max = round(100*wind_gwh/demand_max,2),
                  wind_vs_load_min = round(100*wind_gwh/demand_min,2),
                  ufactor_max = sum(demand_max)/max(relative_peak*pct_load*max_gw),
                  ufactor_min = sum(demand_min)/max((pct_load/min_val) * (relative_peak/min_peak_by_month) * min_gw),
                  coal_curtail_pmin = sum(coal_curtail_pmin),
                  coal_curtail_pmax = sum(coal_curtail_pmax),
                  solar_curtail_pmin = sum(curtail_solar_pmin),
                  solar_curtail_pmax = sum(curtail_solar_pmax),
                  trans_curtail_min = sum(trans_curtail_min),
                  trans_curtail_max = sum(trans_curtail_max))
    


    print('data summary:')
    # NOTE...if there are any NA values, this function does not run
    # can use round_any() in plyr, however, we have issues with plyr
    # so fix plyr issue (mutate/subset/other functions don't work properly 
    # when dplyr and plyr run at the same time)

    printout <- round(printout,2)
    print(printout)
    print(object.size(test_profile), units= "Mb")
    test_profile
}






analyze_data_annually <- function(test_profile) {
    library(dplyr)


    
    printout <- test_profile %>% group_by(year) %>%
        summarise(
            
            # demand and transmission impact on demand
            demand_noT_min_twh = sum((pct_load/min_val) * (relative_peak/min(relative_peak)) * min_gw)/1000,
            demand_afterT_min_twh = sum(hourly_demand_min_scale)/1000,
            # percent of demand satisfied by transmission
            t_dem_min_pct = 100*(1-demand_afterT_min_twh/demand_noT_min_twh),
            demand_noT_max_twh = sum(relative_peak*pct_load*max_gw)/1000,
            demand_afterT_max_twh = sum(hourly_demand_max_scale)/1000,
            t_dem_max_pct = 100*(1-demand_afterT_max_twh/demand_noT_max_twh),
            
            
            
            # wind and wind transmission data
            wind_gen_twh = sum(gwh_wind_onshore)/1000,
            # wind outflow on transmission lines
            wind_after_trans_twh = sum(wind_after_transmission)/1000,
            # percent of wind leaving 
            wind_trans_pct = 100*(1-wind_after_trans_twh/wind_gen_twh),
            solar_gen_twh = sum(gwh_solar)/1000,
            solar_after_trans_twh = sum(solar_after_transmission)/1000,
            solar_trans_pct = 100*(1-solar_after_trans_twh/solar_gen_twh),
            

            
            # transmission data
            t_inflow_twh = sum(gw_flow*inflow_ratio*trans_profile)/1000,
            t_outflow_tot_twh = sum(t_out_total)/1000,
            t_out_wind_twh = sum(gwh_wind_onshore-wind_after_transmission)/1000,
            t_out_wind_pct_of_outflow = 100*t_out_wind_twh/(t_outflow_tot_twh+0.001),
            t_out_solar_twh = sum(gwh_solar-solar_after_transmission)/1000,
            t_out_solar_pct_of_outflow = 100*t_out_solar_twh/(t_outflow_tot_twh+0.001),
            t_out_base_twh = sum(t_out_base_min)/1000,
            t_out_base_pct_of_outflow = 100*t_out_base_twh/(t_outflow_tot_twh+0.0001),
            t_out_pct_of_schedule_met_min = 100*sum(t_out_base_min)/(sum(t_out_total)+0.001),
            t_out_pct_of_schedule_met_max = 100*sum(t_out_base_max)/(sum(t_out_total)+0.001),
            # transmission curtailed curtailed
            
            
            trans_curtail_min_twh = sum(trans_curtail_min)/1000,
            trans_curtail_max_twh = sum(trans_curtail_max)/1000,
            # in case there is a divide by zero error!
             trans_curtail_min_pct = 100*abs((trans_curtail_min_twh/(t_inflow_twh+.0001) )),
             trans_curtail_max_pct = 100*abs((trans_curtail_max_twh/(t_inflow_twh+.0001) )),
            
            
            # renewables curtailment data
            wind_curtailed_min_twh = sum(curtail_pmin)/1000,
            wind_curtailed_max_twh = sum(curtail_pmax)/1000,
            wind_avg_curtailed_twh = (wind_curtailed_min_twh+wind_curtailed_max_twh)/2,
            wind_pct_min_curtail = 100*abs(wind_curtailed_min_twh/wind_gen_twh),
            wind_pct_max_curtail = 100*abs(wind_curtailed_max_twh/wind_gen_twh),
            wind_pct_avg_curtail = (wind_pct_min_curtail+wind_pct_max_curtail)/2,
            solar_curtailed_min_twh = sum(curtail_solar_pmin)/1000,
            solar_curtailed_max_twh = sum(curtail_solar_pmax)/1000,
            solar_curtailed_avg_twh = (solar_curtailed_min_twh+solar_curtailed_max_twh)/2,
            # div by zero problems
            solar_pct_min_curtail = 100*abs(solar_curtailed_min_twh/solar_gen_twh+0.0001),
            solar_pct_max_curtail = 100*abs(solar_curtailed_max_twh/solar_gen_twh+0.0001),
            solar_pct_avg_curtail = (solar_pct_min_curtail+solar_pct_max_curtail)/2,
            
            # base generation data
            base_local_gen_min_twh = sum(local_base_min)/1000,
            base_local_gen_max_twh = sum(local_base_max)/1000,
            base_local_gen_avg_twh = (base_local_gen_min_twh+base_local_gen_max_twh)/2,
            base_t_gen_min_twh = sum(t_out_base_min)/1000,
            base_t_gen_max_twh = sum(t_out_base_max)/1000,
            base_t_gen_avg_twh = (base_t_gen_min_twh+base_t_gen_max_twh)/2,
            
            # base load utilization hours data
            # demand minus wind+solar output - wind+solar curtailemnt(negative number, so - -), 
            # divide by iCap
            base_util_hrs_min = sum((local_base_min+t_out_base_min))/
                ((min(coal_iCap)+min(natgas_iCap))/1e3),
            base_util_hrs_max = sum((local_base_max+t_out_base_max))/
                ((min(coal_iCap)+min(natgas_iCap))/1e3),
            base_util_hrs_avg = (base_util_hrs_min+base_util_hrs_max)/2,
            # base load while ignoring transmission
            base_util_hrs_min_noT = (demand_noT_min_twh -wind_after_trans_twh - solar_after_trans_twh-
                                         wind_curtailed_min_twh - solar_curtailed_min_twh)/
                ((min(coal_iCap)+min(natgas_iCap))/1e6),
            base_util_hrs_max_noT = (demand_noT_max_twh -wind_after_trans_twh - solar_after_trans_twh-
                                         wind_curtailed_min_twh - solar_curtailed_min_twh)/
                ((min(coal_iCap)+min(natgas_iCap))/1e6),
            base_util_hrs_avg_noT = (base_util_hrs_min_noT+base_util_hrs_max_noT)/2,
            
             # util hours 
             util_factor_min = sum((pct_load/min_val) * (relative_peak/min(relative_peak)) * min_gw)/
                 max((pct_load/min_val) * (relative_peak/min(relative_peak)) * min_gw),
             util_factor_max = sum(relative_peak*pct_load*max_gw)/
                 max(relative_peak*pct_load*max_gw)
            
            
            )
    
    printout <- round(printout,2)
    print('Annual data analysis: ')
    print(printout)
    printout
}


analyze_data_monthly <- function(test_profile,months=1:12) {
    library(dplyr)
    
    printout <- test_profile %>% subset(month %in% months) %>% group_by(year,month) %>%
        summarise(
            
            # basic data
            demand_noT_avg_gwh = ((sum((pct_load/min_val) * (relative_peak/min(relative_peak)) * min_gw))+
                                      (sum(relative_peak*pct_load*max_gw)))/2,
            demand_afterT_avg_gwh = ((sum(hourly_demand_min_scale))+
                                         (sum(hourly_demand_max_scale)))/2,
            t_demand_pct = 100*(1-demand_afterT_avg_gwh/demand_noT_avg_gwh),
            wind_gen_gwh = sum(gwh_wind_onshore),
            wind_after_trans_gwh = sum(wind_after_transmission),
            wind_trans_pct = 100*(1-wind_after_trans_gwh/wind_gen_gwh),
            solar_gen_gwh = sum(gwh_solar),
            solar_after_trans_gwh = sum(solar_after_transmission),
            solar_trans_pct = 100*(1-solar_after_trans_gwh/solar_gen_gwh),
            
            # transmission data
            t_inflow_gwh = sum(gw_flow*inflow_ratio*trans_profile),
            t_outflow_tot_gwh = sum(t_out_total),
            t_out_wind_pct_of_outflow = 100*(sum(gwh_wind_onshore-wind_after_transmission))/(t_outflow_tot_gwh+0.0001),
            t_out_solar_pct_of_outflow = 100*(sum(gwh_solar-solar_after_transmission))/(t_outflow_tot_gwh+0.0001),
            t_out_base_pct_of_outflow = 100*(sum(t_out_base_min))/(t_outflow_tot_gwh+0.0001),
            t_out_pct_of_schedule_met_avg = ((100*sum(t_out_base_min)/(sum(t_out_total)+0.0001) )+
                                                 (100*sum(t_out_base_max)/(sum(t_out_total)+0.0001) ))/2,

            # curtailment data
            wind_curtailed_avg_gwh = (sum(curtail_pmin)+sum(curtail_pmax))/2,
            wind_pct_avg_curtail = 100*abs(((sum(curtail_pmin)+sum(curtail_pmax))/2)/wind_gen_gwh),
            solar_pct_avg_curtail = 100*abs(((sum(curtail_solar_pmin)+sum(curtail_solar_pmax))/2)/solar_gen_gwh),

            # base gen stuff
            base_local_gen_avg_gwh = ((sum(local_base_min))+
                                          (sum(local_base_max)))/2,
            base_t_gen_avg_gwh = ((sum(t_out_base_min))+
                                      (sum(t_out_base_max)))/2,
            
            base_afterT_util_hrs_avg = 1e3*(((sum((local_base_min+t_out_base_min)))+
                                                 (sum((local_base_max+t_out_base_max))))/2)/(min(coal_iCap)+min(natgas_iCap))
        
            )

    printout <- round(printout,2)
    print('Annual data analysis: ')
    print(printout)
    printout
}


analyze_data_quarterly <- function(test_profile) {

    # add year+quarter
    library(zoo)
    library(dplyr)
    test_profile <- mutate(test_profile, yearqtr = as.character(as.yearqtr(date)))
    
    # printout copied from above
    printout <- test_profile %>% group_by(yearqtr) %>%
        summarise(
            
            # basic data
            demand_noT_avg_gwh = ((sum((pct_load/min_val) * (relative_peak/min(relative_peak)) * min_gw))+
                                      (sum(relative_peak*pct_load*max_gw)))/2,
            demand_afterT_avg_gwh = ((sum(hourly_demand_min_scale))+
                                         (sum(hourly_demand_max_scale)))/2,
            t_demand_pct = 100*(1-demand_afterT_avg_gwh/demand_noT_avg_gwh),
            wind_gen_gwh = sum(gwh_wind_onshore),
            wind_after_trans_gwh = sum(wind_after_transmission),
            wind_trans_pct = 100*(1-wind_after_trans_gwh/wind_gen_gwh),
            solar_gen_gwh = sum(gwh_solar),
            solar_after_trans_gwh = sum(solar_after_transmission),
            solar_trans_pct = 100*(1-solar_after_trans_gwh/solar_gen_gwh),
            
            # transmission data
            t_inflow_gwh = sum(gw_flow*inflow_ratio*trans_profile),
            t_outflow_tot_gwh = sum(t_out_total),
            t_out_wind_pct_of_outflow = 100*(sum(gwh_wind_onshore-wind_after_transmission))/(t_outflow_tot_gwh+0.0001),
            t_out_solar_pct_of_outflow = 100*(sum(gwh_solar-solar_after_transmission))/(t_outflow_tot_gwh+0.0001),
            t_out_base_pct_of_outflow = 100*(sum(t_out_base_min))/(t_outflow_tot_gwh+0.0001),
            t_out_pct_of_schedule_met_avg = ((100*sum(t_out_base_min)/(sum(t_out_total)+0.0001) )+
                                                 (100*sum(t_out_base_max)/(sum(t_out_total)+0.0001) ))/2,
            
            # curtailment data
            wind_curtailed_avg_gwh = (sum(curtail_pmin)+sum(curtail_pmax))/2,
            wind_pct_avg_curtail = 100*abs(((sum(curtail_pmin)+sum(curtail_pmax))/2)/wind_gen_gwh),
            solar_pct_avg_curtail = 100*abs(((sum(curtail_solar_pmin)+sum(curtail_solar_pmax))/2)/solar_gen_gwh),
            
            # base gen stuff
            base_local_gen_avg_gwh = ((sum(local_base_min))+
                                          (sum(local_base_max)))/2,
            base_t_gen_avg_gwh = ((sum(t_out_base_min))+
                                      (sum(t_out_base_max)))/2,
            
            base_afterT_util_hrs_avg = 1e3*(((sum((local_base_min+t_out_base_min)))+
                                                 (sum((local_base_max+t_out_base_max))))/2)/(min(coal_iCap)+min(natgas_iCap))
            
        )
 
    printout[,-1] <- round(printout[,-1],2)
    print('Annual data analysis: ')
    print(printout)
    printout
}


analyze_data_hourly <- function(test_profile){
    library(dplyr)
    
    printout <- test_profile  %>% group_by(year,hour) %>%
        summarise(
            
            # basic data
            demand_noT_avg_gwh = ((sum((pct_load/min_val) * (relative_peak/min(relative_peak)) * min_gw))+
                                      (sum(relative_peak*pct_load*max_gw)))/2,
            demand_afterT_avg_gwh = ((sum(hourly_demand_min_scale))+
                                         (sum(hourly_demand_max_scale)))/2,
            t_demand_pct = 100*(1-demand_afterT_avg_gwh/demand_noT_avg_gwh),
            wind_gen_gwh = sum(gwh_wind_onshore),
            wind_after_trans_gwh = sum(wind_after_transmission),
            wind_trans_pct = 100*(1-wind_after_trans_gwh/wind_gen_gwh),
            solar_gen_gwh = sum(gwh_solar),
            solar_after_trans_gwh = sum(solar_after_transmission),
            solar_trans_pct = 100*(1-solar_after_trans_gwh/solar_gen_gwh),
            
            # transmission data
            t_inflow_gwh = sum(gw_flow*inflow_ratio*trans_profile),
            t_outflow_tot_gwh = sum(t_out_total),
            t_out_wind_pct_of_outflow = 100*(sum(gwh_wind_onshore-wind_after_transmission))/(t_outflow_tot_gwh+0.0001),
            t_out_solar_pct_of_outflow = 100*(sum(gwh_solar-solar_after_transmission))/(t_outflow_tot_gwh+0.0001),
            t_out_base_pct_of_outflow = 100*(sum(t_out_base_min))/(t_outflow_tot_gwh+0.0001),
            t_out_pct_of_schedule_met_avg = ((100*sum(t_out_base_min)/(sum(t_out_total)+0.0001) )+
                                                 (100*sum(t_out_base_max)/(sum(t_out_total)+0.0001) ))/2,
            
            # curtailment data
            wind_curtailed_avg_gwh = (sum(curtail_pmin)+sum(curtail_pmax))/2,
            wind_pct_avg_curtail = 100*abs(((sum(curtail_pmin)+sum(curtail_pmax))/2)/wind_gen_gwh),
            solar_pct_avg_curtail = 100*abs(((sum(curtail_solar_pmin)+sum(curtail_solar_pmax))/2)/solar_gen_gwh),
            
            # base gen stuff
            base_local_gen_avg_gwh = ((sum(local_base_min))+
                                          (sum(local_base_max)))/2,
            base_t_gen_avg_gwh = ((sum(t_out_base_min))+
                                      (sum(t_out_base_max)))/2,
            
            base_afterT_util_hrs_avg = 1e3*(((sum((local_base_min+t_out_base_min)))+
                                                 (sum((local_base_max+t_out_base_max))))/2)/(min(coal_iCap)+min(natgas_iCap))
            
        )
    
    printout <- round(printout,2)
    print('Annual data analysis: ')
    print(printout)
    printout
    
    
}


seasonal_hourly_load <- read.xlsx("data/input_data_v5.xlsx",sheetName="seasonal_hourly_load",colIndex=1:4,stringsAsFactors=F)
monthly_demand_profile <- read.xlsx("data/input_data_v5.xlsx",sheetName="monthly_demand_profile",colIndex=1:5,stringsAsFactors=F)
annual_demand <- read.xlsx("data/input_data_v5.xlsx",sheetName="annual_demand",colIndex=1:6,stringsAsFactors=F)
ccp_season <- read.xlsx("data/input_data_v5.xlsx",sheetName="ccp_season",colIndex=1:3,stringsAsFactors=F)
generation_forecast <- read.xlsx("data/input_data_v5.xlsx",sheetName="generation_forecast",colIndex=1:4,stringsAsFactors=F)
wind <- read.xlsx("data/input_data_v5.xlsx",sheetName="wind",colIndex=2:3,stringsAsFactors=F)
solar_month <- read.xlsx("data/input_data_v5.xlsx",sheetName="solar_profile",colIndex = 1:4,stringsAsFactors=F)
solar_day <- read.xlsx('data/input_data_v5.xlsx',sheetName="solar_profile",colIndex=8:12,stringsAsFactors=F)
t_sched <- read.xlsx("data/input_data_v5.xlsx",sheetName='transmission_schedule',colIndex=1:4,stringsAsFactors=F)
t_icap <- read.xlsx("data/input_data_v5.xlsx",sheetName='transmission_icap',colIndex=1:4,stringsAsFactors=F)
growth_cases <- read.xlsx("data/input_data_v5.xlsx",sheetName="growth_cases",colIndex=1:3,stringsAsFactors=F)


## Step 5: bonus step!
# quick input wrapper functions

# use the <<- super assignment to globally assign these variables
run_model <- function(        peak_season = c(7,8),
                              start_year='2014',
                              end_year='2025',
                              wind_scale_factor=1,
                              solar_scale_factor=1,
                              time_period=6,
                              wind_transmission_share = 0.1,
                              solar_transmission_share = 0,
                              extra_trans_capacity_renewables = 0,
                              #extra_trans_capacity_for_wind = 0,
                              #extra_trans_capacity_for_solar = 0,
                              gw_load_base_wind_ratio = 0,
                              gw_load_base_solar_ratio = 0,
                              max_option = 1,
                              spinning_reserve=1.15,
                              r_ccp_coal = 0.4,
                              r_ccp_ng = 0.9,
                              pmin_noccp_coal = 0.5,
                              pmin_ccp_coal = 0.85,
                              pmax_ccp_coal = 0.95,
                              pmin_noccp_ng = 0.3,
                              pmin_ccp_ng = 0.85,
                              pmax_ccp_ng = 0.95,
                              offset_wind_hours=0,
                              randomize_transmission=T,
                              transmission_sd =0.03){
    
    peak_season <<- peak_season
    start_year<<-start_year
    end_year<<-end_year
    wind_scale_factor<<-wind_scale_factor
    solar_scale_factor<<-solar_scale_factor
    time_period<<-time_period
    wind_transmission_share <<- wind_transmission_share
    solar_transmission_share <<- solar_transmission_share
    extra_trans_capacity_renewables <<- extra_trans_capacity_renewables
    #extra_trans_capacity_for_wind <<- extra_trans_capacity_for_wind
    #extra_trans_capacity_for_solar <<- extra_trans_capacity_for_solar
    gw_load_base_wind_ratio <<- gw_load_base_wind_ratio
    gw_load_base_solar_ratio <<- gw_load_base_solar_ratio
    max_option <<- max_option
    spinning_reserve<<-spinning_reserve
    r_ccp_coal <<- r_ccp_coal
    r_ccp_ng <<- r_ccp_ng
    pmin_noccp_coal <<- pmin_noccp_coal
    pmin_ccp_coal <<- pmin_ccp_coal
    pmax_ccp_coal <<- pmax_ccp_coal
    pmin_noccp_ng <<- pmin_noccp_ng
    pmin_ccp_ng <<- pmin_ccp_ng
    pmax_ccp_ng <<- pmax_ccp_ng
    offset_wind_hours <<- offset_wind_hours
    randomize_transmission <<- randomize_transmission
    transmission_sd <<- transmission_sd
    
    
    library(dplyr)
    options(dplyr.width = Inf)
    
    t_annual_sched <<- create_transmission_profile(t_sched_gridmodel,peak_season=peak_season)
    solar_profile <<- create_solar_profile(solar_month_gridmodel,solar_day_gridmodel)
    model_part1 <<- create_hourly_renewables_profile(wind=wind_gridmodel,
                                                    solar=solar_profile, 
                                                    gen_forecast=generation_forecast_gridmodel,
                                                    start_year=start_year,
                                                    end_year=end_year,
                                                    wind_scale_factor= wind_scale_factor, 
                                                    solar_scale_factor= solar_scale_factor,
                                                    offset_wind_hours=offset_wind_hours)
    
    
    model_part2 <<- add_hourly_transmission_profile(test_profile=model_part1, 
                                                   t_annual_sched=t_annual_sched, 
                                                   t_icap=t_icap_gridmodel,
                                                   randomize_t=randomize_transmission,
                                                   sd_level=transmission_sd)
    

    
    model_part3 <<- add_hourly_load_profile(test_profile = model_part2,
                                           seasonal_hourly_load = seasonal_hourly_load_gridmodel,
                                           monthly_demand_profile = monthly_demand_profile_gridmodel,
                                           annual_demand = annual_demand_gridmodel,
                                           ccp_season = ccp_season_gridmodel, 
                                           time_period = time_period,
                                           wind_transmission_share = wind_transmission_share,
                                           extra_trans_capacity_renewables = extra_trans_capacity_renewables,
                                           solar_transmission_share = solar_transmission_share,
                                           gw_load_base_wind_ratio = gw_load_base_wind_ratio,
                                           gw_load_base_solar_ratio = gw_load_base_solar_ratio,
                                           max_option = max_option)
    

    
    
    model_part4 <<- calculate_curtailment(test_profile = model_part3, 
                                         spinning_reserve=spinning_reserve,
                                         r_ccp_coal = r_ccp_coal,
                                         r_ccp_ng = r_ccp_ng,
                                         pmin_noccp_coal = pmin_noccp_coal,
                                         pmin_ccp_coal = pmin_ccp_coal,
                                         pmax_ccp_coal = pmax_ccp_coal,
                                         pmin_noccp_ng = pmin_noccp_ng,
                                         pmin_ccp_ng = pmin_ccp_ng,
                                         pmax_ccp_ng = )
    
}

get_grid_model <- function(   gridmodel = 'zjk',
                              special_subset_model = 'zjk',
                              seasonal_hourly_load,
                              monthly_demand_profile,
                              annual_demand,
                              ccp_season,
                              generation_forecast,
                              wind,
                              solar_month,
                              solar_day,
                              t_sched,
                              t_icap,
                              growth_cases,
                              growth_case = 'noscale',
                              demand_scale = 1,
                              transmission_model = "jjt",
                              transmission_scale = 1,
                              annual_demand_model = "none"){
    

    if(sum((unique(annual_demand$grid_model) %in% annual_demand_model))==0){
        annual_demand_model <<- special_subset_model
    }
    
    gridmodel <<-gridmodel
    special_subset_model <<-special_subset_model
    transmission_model <<- transmission_model
    transmission_scale <<- transmission_scale
        
    seasonal_hourly_load_gridmodel <<- subset_gridmodel(seasonal_hourly_load, gridmodel, "Seasonal Hourly Load Profile", default_grid_model = special_subset_model)
    monthly_demand_profile_gridmodel <<- subset_gridmodel(monthly_demand_profile, gridmodel, "Monthly Demand Profile", default_grid_model = special_subset_model)
    # modified to allow for an input (annual_demand_model)
    annual_demand_gridmodel <<- subset_gridmodel(annual_demand, annual_demand_model, "Annual Demand", default_grid_model = special_subset_model)
    ccp_season_gridmodel <<- subset_gridmodel(ccp_season, gridmodel, "CCP Season", default_grid_model = special_subset_model)
    # SPECIAL NOTE FOR GENERATION FORECAST
    # Due to bad design, you have to put in solar, wind, coal, and natgas
    generation_forecast_gridmodel <<- subset_gridmodel(generation_forecast, gridmodel, "Generation Forecast", default_grid_model = special_subset_model)
    wind_gridmodel <<- subset_gridmodel(wind, gridmodel, "Wind Hourly Annual Profile kwh/kw", default_grid_model = special_subset_model)
    solar_month_gridmodel <<- subset_gridmodel(solar_month, gridmodel, "Solar Monthly Profile CF", default_grid_model = special_subset_model)
    solar_day_gridmodel <<- subset_gridmodel(solar_day, gridmodel, "Solar Daily Profile CF", default_grid_model = special_subset_model)
    # note this is transmission_model for the default grid model
    t_sched_gridmodel <<- subset_gridmodel(t_sched, transmission_model, "Transmission Hourly Schedule", default_grid_model = special_subset_model)
    t_sched_gridmodel <<- mutate(t_sched_gridmodel,trans_cap = trans_cap*transmission_scale)
    t_icap_gridmodel <<- subset_gridmodel(t_icap, gridmodel, "Transmission Installed Capacity Forecast", default_grid_model = special_subset_model)
    
    
    # custom additions
    growth_cases <<- subset(growth_cases,growth_model==growth_case)
    print(paste('Growth Case is',growth_case))
    print(tail(growth_cases))
    annual_demand_gridmodel <<- left_join(annual_demand_gridmodel,growth_cases[,c(1,3)],by="year")
    annual_demand_gridmodel <<- mutate(annual_demand_gridmodel, twh_load = demand_scale*twh_load*growth_scale,
                                       max_gw=max_gw*demand_scale*growth_scale, min_gw=demand_scale*min_gw*growth_scale)
    annual_demand_gridmodel <<- select(annual_demand_gridmodel,-growth_scale)
    
    
}

