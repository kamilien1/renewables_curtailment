### NOTE these values MUST be inputted as follows, or the 
# model will not work properly
# wind_transmission_share = 0
# solar_transmission_share = 0

# testing out additions of functions

add_hourly_load_profile_v2 <- function(test_profile,
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
    # get the extra amount of t capacity available for w/s
    # extra_trans_capacity = extra_trans_capacity_renewables * (1-trans_profile) * gw_flow * (1-inflow_ratio)
    #wind_vs_ws_ratio=ifelse(wind_after_transmission==0 & solar_after_transmission ==0,0,
    #       wind_after_transmission/(wind_after_transmission+solar_after_transmission))
    # step 3: update wind after t, solar after t
    # wind_after_transmission = wind_after_transmission - min(wind_after_transmission,
    # wind_vs_ws_ratio*extra_trans_capacity_renewables*(1-trans_profile)*gw_flow*(1-inflow_ratio))
    # step 4: update wind vs ws ratio
    
    
    # step 1
    # after T = renewables gen - base_allocation - transmission allocation
    
    test_profile <- mutate(test_profile,  wind_after_transmission= gwh_wind_onshore - 
                               gw_load_base_wind_ratio*gwh_wind_onshore - trans_profile*gw_flow*(1-inflow_ratio)*wind_transmission_share,
                           wind_after_transmission = ifelse(wind_after_transmission<0,0,wind_after_transmission))
    
    test_profile <- mutate(test_profile,  solar_after_transmission= gwh_solar - 
                               (1-gw_load_base_solar_ratio)*gwh_solar* trans_profile*gw_flow*(1-inflow_ratio)*solar_transmission_share,
                           solar_after_transmission = ifelse(solar_after_transmission<0,0,solar_after_transmission))
    
    
    print("Wind after step 1, initial transmission:")
    print(test_profile %>% summarize(wind_after_t = sum(wind_after_transmission)))
    
    # step 2
    # after the outflow of w/s from step 1, how much of it in GW is alotted for wind+solar
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
    
    
    # step 4
    # extra transmission capacity is updated, reduced by wind/solar sent from step 3, which is the
    # wind/solar sent from the additionally available transmission capacity (extra_trans_capacity_renewables)
#     test_profile <- mutate(test_profile, extra_trans_capacity = extra_trans_capacity - 
#                     (gwh_wind_onshore - wind_after_transmission) - (gwh_solar - solar_after_transmission))
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
    # AND transmission outflow: t outflow is treated like an addition in demand
    # relative peak for month * pct load during the hour * peak demand
    test_profile$hourly_demand_max_scale <- with(test_profile,relative_peak*pct_load*max_gw -
                                                     # "baseload" wind generation
                                                     gw_load_base_wind_ratio*gwh_wind_onshore -
                                                     # "baseload" solar generation
                                                     gw_load_base_solar_ratio*gwh_solar -
                                                     # inflow transmission
                                                     trans_profile*gw_flow*inflow_ratio +
                                                     # t outflow 
                                                     trans_profile*gw_flow*(1-inflow_ratio))
    
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
                                                     trans_profile*gw_flow*inflow_ratio +
                                                     trans_profile*gw_flow*(1-inflow_ratio))
    
    # deal with case that transmission inflow is larger than total demand
    # by tracking the negative numbers. If no trans curtailed, then the value is 0
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





calculate_curtailment_v2 <- function(test_profile, 
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
    # it is demand (demand = demand + transmission flow out) - gen_pmin_min (gen online pmin from both local demand and transmission out)
    # - transmission outflow base gen(= total outflow - wind outflow - solar outflow)
    # NOTE: this ONLY works if step 1 of wind/solar transmission out procedure has 
    # the following values:
    # wind_transmission_share = 0
    # solar_transmission_share = 0
    # 
    test_profile<- mutate(test_profile,curtail_renewables_amount_pmin = hourly_demand_min_scale - gen_pmin_min - 
                              - wind_after_transmission - solar_after_transmission ) 
    test_profile <- mutate(test_profile, curtail_renewables_amount_pmin = 
                               ifelse(curtail_renewables_amount_pmin >0,0,curtail_renewables_amount_pmin))
    
    
    
    
    # calculate curtailment of WIND
    # it is a negative number by default 
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
    test_profile<- mutate(test_profile,curtail_renewables_amount_pmax = hourly_demand_max_scale - 
                              gen_pmax_min - wind_after_transmission - solar_after_transmission) 
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
    # NOTE: in this model, local_base is ignored because it cannot be
    # calculated without assumptions
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
                           t_out_base_max = t_out_total - t_out_wind_solar_gwh,
                           # total gen from base load (local+t_out)
                           
                           # base generation is
                           # demand + transmission inflow - transmission outflow
                           # - all wind - all solar 
                           # add back in the amount of wind/solar curtailed
                           # - any coal/baseload curtailed 
                           # NOTE: curtailment is a NEGATIVE number
                           # so pay attention to the signs
                           total_base_gen_min = hourly_demand_min_scale - gwh_wind_onshore - gwh_solar -
                               curtail_pmin - curtail_solar_pmin + coal_curtail_pmin,
                           total_base_gen_max = hourly_demand_max_scale - gwh_wind_onshore - gwh_solar -
                               curtail_pmax - curtail_solar_pmax + coal_curtail_pmax
                           
                           
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



run_model_v2 <- function(     peak_season = c(7,8),
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
                              offset_wind_hours=0){
    
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
                                                    t_icap=t_icap_gridmodel)
    
    
    
    model_part3 <<- add_hourly_load_profile_v2(test_profile = model_part2,
                                            seasonal_hourly_load = seasonal_hourly_load_gridmodel,
                                            monthly_demand_profile = monthly_demand_profile_gridmodel,
                                            annual_demand = annual_demand_gridmodel,
                                            ccp_season = ccp_season_gridmodel, 
                                            time_period = time_period,
                                            wind_transmission_share = wind_transmission_share,
                                            extra_trans_capacity_renewables = extra_trans_capacity_renewables,
                                            #extra_trans_capacity_for_wind = extra_trans_capacity_for_wind,
                                            solar_transmission_share = solar_transmission_share,
                                            #extra_trans_capacity_for_solar = extra_trans_capacity_for_solar,
                                            gw_load_base_wind_ratio = gw_load_base_wind_ratio,
                                            gw_load_base_solar_ratio = gw_load_base_solar_ratio,
                                            max_option = max_option)
    
    
    
    
    model_part4 <<- calculate_curtailment_v2(test_profile = model_part3, 
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



analyze_data_annually_v2 <- function(test_profile) {
    library(dplyr)
    
    printout <- test_profile %>% group_by(year) %>%
        summarise(
            
            
            demand_noT_avg_twh = ((sum((pct_load/min_val) * (relative_peak/min(relative_peak)) * min_gw))+
                                      (sum(relative_peak*pct_load*max_gw)))/2000,
            demand_afterT_avg_twh = ((sum(hourly_demand_min_scale))+
                                         (sum(hourly_demand_max_scale)))/2000,
            
            # wind and wind transmission data
            wind_gen_twh = sum(gwh_wind_onshore)/1000,
            solar_gen_twh = sum(gwh_solar)/1000,
            # transmission data
            t_inflow_twh = sum(gw_flow*inflow_ratio*trans_profile)/1000,
            t_outflow_tot_twh = sum(t_out_total)/1000,            
            trans_curtail_avg_pct = ((100*abs(((sum(trans_curtail_min)/1000)/(t_inflow_twh+.0001) )))+
                                         (100*abs(((sum(trans_curtail_max)/1000)/(t_inflow_twh+.0001) ))))/2,
            wind_avg_curtailed_twh = ((sum(curtail_pmin)/1000)+(sum(curtail_pmax)/1000))/2,            
            wind_pct_avg_curtail = ((100*abs((sum(curtail_pmin)/1000)/wind_gen_twh))+
                                        (100*abs((sum(curtail_pmax)/1000)/wind_gen_twh)))/2,            
            solar_curtailed_avg_twh = ((sum(curtail_solar_pmin)/1000)+(sum(curtail_solar_pmax)/100))/2,
            solar_pct_avg_curtail = ((100*abs((sum(curtail_solar_pmin)/1000)/solar_gen_twh+0.0001))+
                                         (100*abs((sum(curtail_solar_pmax)/1000)/solar_gen_twh+0.0001)))/2,
            total_base_gen_avg_twh = (sum(total_base_gen_min)+sum(total_base_gen_max))/2000,
            total_base_gen_avg_util_hours = (sum(total_base_gen_min) + sum(total_base_gen_max) ) /
        (((min(coal_iCap)+min(natgas_iCap)))/500)
            
        )
    
    printout <- round(printout,2)
    print('Annual data analysis: ')
    print(printout)
    printout
}


analyze_data_monthly_v2 <- function(test_profile) {
    library(dplyr)
    
    printout <- test_profile %>% group_by(year,month) %>%
        summarise(
            
            
            demand_noT_avg_gwh = ((sum((pct_load/min_val) * (relative_peak/min(relative_peak)) * min_gw))+
                                      (sum(relative_peak*pct_load*max_gw)))/2,
            demand_afterT_avg_gwh = ((sum(hourly_demand_min_scale))+
                                         (sum(hourly_demand_max_scale)))/2,
            
            # wind and wind transmission data
            wind_gen_gwh = sum(gwh_wind_onshore)/1,
            solar_gen_gwh = sum(gwh_solar)/1,
            # transmission data
            t_inflow_gwh = sum(gw_flow*inflow_ratio*trans_profile)/1,
            t_outflow_tot_gwh = sum(t_out_total)/1,            
            trans_curtail_avg_pct = ((100*abs(((sum(trans_curtail_min)/1)/(t_inflow_gwh+.0001) )))+
                                         (100*abs(((sum(trans_curtail_max)/1)/(t_inflow_gwh+.0001) ))))/2,
            wind_avg_curtailed_gwh = ((sum(curtail_pmin)/1)+(sum(curtail_pmax)/1))/2,            
            wind_pct_avg_curtail = ((100*abs((sum(curtail_pmin)/1)/wind_gen_gwh))+
                                        (100*abs((sum(curtail_pmax)/1)/wind_gen_gwh)))/2,            
            solar_curtailed_avg_gwh = ((sum(curtail_solar_pmin)/1)+(sum(curtail_solar_pmax)/100))/2,
            solar_pct_avg_curtail = ((100*abs((sum(curtail_solar_pmin)/1)/solar_gen_gwh+0.0001))+
                                         (100*abs((sum(curtail_solar_pmax)/1)/solar_gen_gwh+0.0001)))/2,
            total_base_gen_avg_gwh = (sum(total_base_gen_min)+sum(total_base_gen_max))/2,
            total_base_gen_avg_util_hours = (sum(total_base_gen_min) + sum(total_base_gen_max) ) /
                (((min(coal_iCap)+min(natgas_iCap)))/500)
            
        )
    
    printout <- round(printout,2)
    print('Annual data analysis: ')
    print(printout)
    printout
}


analyze_data_hourly_v2 <- function(test_profile) {
    library(dplyr)
    
    printout <- test_profile %>% group_by(year,hour) %>%
        summarise(
            
            
            demand_noT_avg_gwh = ((sum((pct_load/min_val) * (relative_peak/min(relative_peak)) * min_gw))+
                                      (sum(relative_peak*pct_load*max_gw)))/2,
            demand_afterT_avg_gwh = ((sum(hourly_demand_min_scale))+
                                         (sum(hourly_demand_max_scale)))/2,
            
            # wind and wind transmission data
            wind_gen_gwh = sum(gwh_wind_onshore)/1,
            solar_gen_gwh = sum(gwh_solar)/1,
            # transmission data
            t_inflow_gwh = sum(gw_flow*inflow_ratio*trans_profile)/1,
            t_outflow_tot_gwh = sum(t_out_total)/1,            
            trans_curtail_avg_pct = ((100*abs(((sum(trans_curtail_min)/1)/(t_inflow_gwh+.0001) )))+
                                         (100*abs(((sum(trans_curtail_max)/1)/(t_inflow_gwh+.0001) ))))/2,
            wind_avg_curtailed_gwh = ((sum(curtail_pmin)/1)+(sum(curtail_pmax)/1))/2,            
            wind_pct_avg_curtail = ((100*abs((sum(curtail_pmin)/1)/wind_gen_gwh))+
                                        (100*abs((sum(curtail_pmax)/1)/wind_gen_gwh)))/2,            
            solar_curtailed_avg_gwh = ((sum(curtail_solar_pmin)/1)+(sum(curtail_solar_pmax)/100))/2,
            solar_pct_avg_curtail = ((100*abs((sum(curtail_solar_pmin)/1)/solar_gen_gwh+0.0001))+
                                         (100*abs((sum(curtail_solar_pmax)/1)/solar_gen_gwh+0.0001)))/2,
            total_base_gen_avg_gwh = (sum(total_base_gen_min)+sum(total_base_gen_max))/2,
            total_base_gen_avg_util_hours = (sum(total_base_gen_min) + sum(total_base_gen_max) ) /
                (((min(coal_iCap)+min(natgas_iCap)))/500)
            
        )
    
    printout <- round(printout,2)
    print('Annual data analysis: ')
    print(printout)
    printout
}

analyze_data_quarterly_v2 <- function(test_profile) {
    library(dplyr)
    library(zoo)
    
    test_profile <- mutate(test_profile, yearqtr = as.character(as.yearqtr(date)))
    
    printout <- test_profile %>% group_by(yearqtr) %>%
        summarise(
            
            
            demand_noT_avg_gwh = ((sum((pct_load/min_val) * (relative_peak/min(relative_peak)) * min_gw))+
                                      (sum(relative_peak*pct_load*max_gw)))/2,
            demand_afterT_avg_gwh = ((sum(hourly_demand_min_scale))+
                                         (sum(hourly_demand_max_scale)))/2,
            
            # wind and wind transmission data
            wind_gen_gwh = sum(gwh_wind_onshore)/1,
            solar_gen_gwh = sum(gwh_solar)/1,
            # transmission data
            t_inflow_gwh = sum(gw_flow*inflow_ratio*trans_profile)/1,
            t_outflow_tot_gwh = sum(t_out_total)/1,            
            trans_curtail_avg_pct = ((100*abs(((sum(trans_curtail_min)/1)/(t_inflow_gwh+.0001) )))+
                                         (100*abs(((sum(trans_curtail_max)/1)/(t_inflow_gwh+.0001) ))))/2,
            wind_avg_curtailed_gwh = ((sum(curtail_pmin)/1)+(sum(curtail_pmax)/1))/2,            
            wind_pct_avg_curtail = ((100*abs((sum(curtail_pmin)/1)/wind_gen_gwh))+
                                        (100*abs((sum(curtail_pmax)/1)/wind_gen_gwh)))/2,            
            solar_curtailed_avg_gwh = ((sum(curtail_solar_pmin)/1)+(sum(curtail_solar_pmax)/100))/2,
            solar_pct_avg_curtail = ((100*abs((sum(curtail_solar_pmin)/1)/solar_gen_gwh+0.0001))+
                                         (100*abs((sum(curtail_solar_pmax)/1)/solar_gen_gwh+0.0001)))/2,
            total_base_gen_avg_gwh = (sum(total_base_gen_min)+sum(total_base_gen_max))/2,
            total_base_gen_avg_util_hours = (sum(total_base_gen_min) + sum(total_base_gen_max) ) /
                (((min(coal_iCap)+min(natgas_iCap)))/500)
            
        )
    
    printout[,-1] <- round(printout[,-1],2)
    print('Annual data analysis: ')
    print(printout)
    printout
}
