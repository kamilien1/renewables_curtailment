# get this ready for shiny server


# this loads functions
# and reads in the source file
source("clean_functions_v2.R")
source("function_testbed.R")

# this runs the sumulation

# update this every time we want a NEW grid model
get_grid_model(gridmodel = 'zjk',
               special_subset_model = 'zjk',
               seasonal_hourly_load=seasonal_hourly_load,
               monthly_demand_profile=monthly_demand_profile,
               annual_demand=annual_demand,
               ccp_season=ccp_season,
               generation_forecast=generation_forecast,
               wind=wind,
               solar_month=solar_month,
               solar_day=solar_day,
               t_sched=t_sched,
               t_icap=t_icap,
               growth_cases,
               growth_case = 'noscale',
               demand_scale = 1,
               transmission_model = "dem_scale",
               transmission_scale = 1)

# update this every time we want a NEW simulation
run_model_v2(     peak_season = c(12,1,2,6,7,8),
                  start_year="2012",
                  end_year="2025",
                  wind_scale_factor=0.9,
                  solar_scale_factor=1,
                  time_period=12,
                  wind_transmission_share = 0,
                  solar_transmission_share = 0,                  
                  extra_trans_capacity_renewables = 1,   
                  gw_load_base_wind_ratio = 0,
                  gw_load_base_solar_ratio = 0,
                  max_option = 1,
                  spinning_reserve=1.08,
                  r_ccp_coal = 0.13,
                  r_ccp_ng = 0.9,
                  pmin_noccp_coal = 0.5,
                  pmin_ccp_coal = 0.85,
                  pmax_ccp_coal = 0.95,
                  pmin_noccp_ng = 0.3,
                  pmin_ccp_ng = 0.85,
                  pmax_ccp_ng = 0.95,
                  offset_wind_hours=0)



# this analyzes the results
annual_analysis <- analyze_data_annually_v2(test_profile=model_part4) 
monthly_analysis <- analyze_data_monthly_v2(test_profile=model_part4)
quarterly_analysis <- analyze_data_quarterly_v2(test_profile=model_part4)
hourly_analysis <- analyze_data_hourly_v2(test_profile=model_part4)


ggplot(annual_analysis, aes(x=factor(year),y=wind_pct_avg_curtail))+geom_bar(stat='identity')+
    theme(text = element_text(size=20)) +ggtitle("Annual Curtailment of Wind (Average)") +
    geom_text(aes(label=wind_pct_avg_curtail), vjust=4,colour="white")

# ggplot(annual_analysis, aes(x=factor(year),y=total_base_gen_avg_util_hours))+geom_bar(stat='identity')+
#     theme(text = element_text(size=20)) +ggtitle("Avg Util Hours") +
#     geom_text(aes(label=total_base_gen_avg_util_hours), vjust=4,size=6, colour="white")
# 



# ggplot(hourly_analysis, aes(x=factor(hour),y=wind_pct_avg_curtail,colour=factor(year)))+geom_freqpoly(aes(group=factor(year)),stat='identity',size=1)+
#     theme(text = element_text(size=20)) + ggtitle("Wind % Curtailed by Hour, all Years")
# ggplot(annual_analysis, aes(x=factor(year)))+geom_bar(stat='identity',aes(y=base_util_hrs_avg),fill='red',position='dodge')+
#     geom_bar(stat='identity',aes(y=base_util_hrs_avg_noT),fill='green',position='dodge')+ylab("Utilizations Hours")+
#     theme(text = element_text(size=20))+ggtitle("Utilization Hours before(green) and after T(red)")

# ggplot(all_annual, aes(x=factor(year),y=wind_pct_avg_curtail,colour=factor(gridmodel)))+geom_freqpoly(aes(group=factor(gridmodel)),stat='identity',size=1)+
#     theme(text = element_text(size=20)) + ggtitle("Wind % Curtailed Annually Years")





# print output results
annual_analysis <- mutate(annual_analysis, gridmodel=gridmodel, wind_transmission_share=wind_transmission_share, 
                          solar_transmission_share=solar_transmission_share,extra_trans_capacity_renewables=extra_trans_capacity_renewables,
                          spinning_reserve=spinning_reserve,time_period=time_period,
                          offset_wind_hours=offset_wind_hours,wind_scale_factor=wind_scale_factor,transmission_model=transmission_model,
                          transmission_scale=transmission_scale)

monthly_analysis <- mutate(monthly_analysis, gridmodel=gridmodel, wind_transmission_share=wind_transmission_share, 
                           solar_transmission_share=solar_transmission_share,extra_trans_capacity_renewables=extra_trans_capacity_renewables,
                           spinning_reserve=spinning_reserve,time_period=time_period,
                           offset_wind_hours=offset_wind_hours,wind_scale_factor=wind_scale_factor,transmission_model=transmission_model,
                           transmission_scale=transmission_scale)

quarterly_analysis <- mutate(quarterly_analysis,gridmodel=gridmodel,wind_transmission_share=wind_transmission_share,
                             solar_transmission_share=solar_transmission_share,spinning_reserve=spinning_reserve,time_period=time_period,
                             offset_wind_hours=offset_wind_hours,wind_scale_factor=wind_scale_factor,transmission_model=transmission_model)

hourly_analysis <- mutate(hourly_analysis,gridmodel=gridmodel,wind_transmission_share=wind_transmission_share,
                          solar_transmission_share=solar_transmission_share,spinning_reserve=spinning_reserve,time_period=time_period,
                          offset_wind_hours=offset_wind_hours,wind_scale_factor=wind_scale_factor,transmission_model=transmission_model)





all_annual <- bind_rows(all_annual,annual_analysis)
all_monthly <- bind_rows(all_monthly, monthly_analysis)
all_quarterly <- bind_rows(all_quarterly,quarterly_analysis)
all_hourly <- bind_rows(all_hourly, hourly_analysis)

###############################################
###############################################
###############################################
# use this only the first time!! then use above
all_annual <- annual_analysis
all_monthly <- monthly_analysis
all_quarterly <- quarterly_analysis
all_hourly <- hourly_analysis


# write stuff out
library(XLConnect)
rm("data/output.xlsx")
wb <- loadWorkbook("data/output.xlsx", create = T)
createSheet(wb,name="annual_analysis")
createSheet(wb,name="monthly_analysis")
createSheet(wb,name="quarterly_analysis")
createSheet(wb,name="hourly_analysis")
#write data
writeWorksheet(wb, data.frame(all_annual),sheet="annual_analysis")
writeWorksheet(wb, data.frame(all_monthly),sheet="monthly_analysis")
writeWorksheet(wb,data.frame(all_quarterly),sheet="quarterly_analysis")
writeWorksheet(wb,data.frame(all_hourly),sheet="hourly_analysis")
#save .xls file
saveWorkbook(wb)



# use these to append
wb <- loadWorkbook("data/output.xlsx", create = F)
appendWorksheet(wb, data.frame(annual_analysis), sheet = "annual_analysis")
appendWorksheet(wb, data.frame(monthly_analysis),sheet="monthly_analysis")
appendWorksheet(wb, data.frame(quarterly_analysis),sheet="quarterly_analysis")
appendWorksheet(wb, data.frame(hourly_analysis),sheet="hourly_analysis")
saveWorkbook(wb)