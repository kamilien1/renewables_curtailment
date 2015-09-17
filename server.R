library(ggplot2)
library(xtable)
#library(mvtnorm)
#library(MASS)

# taken from
# https://github.com/aeliv002/developing-data-products-project/blob/master/server.R


if(!exists("model_part1")){
    get_grid_model(gridmodel = 'zjk_olympics_light',
                   special_subset_model = 'zjk',
                   growth_case = 'noscale',
                   demand_scale = 1,
                   transmission_model = "all_high",
                   transmission_scale = 0.5)
    
    Sys.sleep(1)
    # update this every time we want a NEW simulation
    run_model_v2(     peak_season = c(12,1,2,6,7,8),
                      start_year='2012',
                      end_year='2025',
                      wind_scale_factor=0.9,
                      solar_scale_factor=1,
                      time_period=3,
                      wind_transmission_share = 0,
                      solar_transmission_share = 0,                  
                      extra_trans_capacity_renewables = 1,   
                      spinning_reserve=1.08,
                      r_ccp_coal = 0.4,
                      r_ccp_ng = 0.9,
                      offset_wind_hours=0)
    
}



# source the clean_functions file here
# initially read in all data here also

if(!exists("model_part1")){
    source('clean_functions_v2.R')
    source('function_testbed.R')
    
}

generateData <- function(mu,Sigma,size){
    generatedDF <- as.data.frame(rownames = NULL, mvrnorm(n = size, mu, Sigma))
    names(generatedDF) <- c("x1","x2")
    return(generatedDF)
    
}

shinyServer(
    function(input, output) {
        
        runSimulation <- eventReactive(
            
            # run the model here
            input$actionButton,{
                
                
                # this is where we run our simulations
                
                get_grid_model(gridmodel = input$gridmodel,
                               special_subset_model = input$special_subset_model,
                               growth_case = input$growth_case,
                               demand_scale = input$demand_scale,
                               transmission_model = input$transmission_model,
                               transmission_scale = input$transmission_scale,
                               # and now all the defaults
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
                               growth_cases)
                
                #in case we have consistent problems, Sys.sleep()
                #Sys.sleep(1)
                # update this every time we want a NEW simulation
                run_model_v2(     peak_season = c(12,1,2,6,7,8),
                                  start_year=input$date_range[1],
                                  end_year=input$date_range[2],
                                  wind_scale_factor=input$wind_scale_factor,
                                  solar_scale_factor=input$solar_scale_factor,
                                  time_period=input$time_period,
                                  wind_transmission_share = input$wind_transmission_share,
                                  solar_transmission_share = input$solar_transmission_share,                  
                                  extra_trans_capacity_renewables = input$extra_trans_capacity_renewables,   
                                  spinning_reserve=input$spinning_reserve,
                                  r_ccp_coal = input$r_ccp_coal,
                                  r_ccp_ng = input$r_ccp_ng,
                                  offset_wind_hours=input$offset_wind_hours)
                
                annual_analysis <<- analyze_data_annually_v2(test_profile=model_part4) 
                monthly_analysis <<- analyze_data_monthly_v2(test_profile=model_part4)
                quarterly_analysis <<- analyze_data_quarterly_v2(test_profile=model_part4)
                hourly_analysis <<- analyze_data_hourly_v2(test_profile=model_part4)
                
                
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
                
                
                
            }
        )
        
        saveSimulation <- eventReactive(
            
            # run the model here
            input$saveModel,{
                
                # if it doesn't exist, make it
                if(!exists("all_annual")){
                    print("all annual does not exist")
                    all_annual <<- annual_analysis
                    all_monthly <<- monthly_analysis
                    all_quarterly <<- quarterly_analysis
                    all_hourly <<- hourly_analysis
                } else {
                
                print("all annual exists")
                
                # can either do unique(data_frame)
                # or anyDuplicated(left_joint(df,new_df,by=)) > 0
                # or  dat[!duplicated(dat[,c('id','id2')]),]
                # if i only want select columns not duplicated (example:
                # function is too slow, so we only need a few vals unique)
                
                # try this one first
                # if no dups, bind rows
                if(anyDuplicated(bind_rows(all_annual,annual_analysis))==0){
                    print("no duplicates")
                    all_annual <<- bind_rows(all_annual,annual_analysis)
                    all_monthly <<- bind_rows(all_monthly, monthly_analysis)
                    all_quarterly <<- bind_rows(all_quarterly,quarterly_analysis)
                    all_hourly <<- bind_rows(all_hourly, hourly_analysis)
                }
                
                }
                
            }
        # end save simulation    
        )
        
        downloadData <- eventReactive(
            
            # run the model here
            input$downloadData,{
                # end of download data 
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
            }
         
        )
        
        # make a plot
        output$plot1 <- renderPlot({
            
            # reactively run the simulation here
            runSimulation()
            
            if(is.null(model_part4)){
                return(NULL)
            }
            else{
                # input$whatever_variable
               
                ggplot(annual_analysis, aes(x=factor(year),y=wind_pct_avg_curtail))+geom_bar(stat='identity')+
                    theme(text = element_text(size=20)) +ggtitle("Annual Curtailment of Wind (Average)") +
                    geom_text(aes(label=wind_pct_avg_curtail), vjust=4,colour="white")
                
                #plotOutput(g)
            }
        })
        
        output$table1 <- renderTable({
                runSimulation()
            

        });
        


        output$summary2 <- renderPrint({
            generateData <- runSimulation()
            if(is.null(generateData)){
                return(NULL)
            }
            else{
                MyList = list(
                    mu = c(mean(generateData$x1),mean(generateData$x2)),
                    Sigma = cov(generateData)
                )
                return(MyList)
            }
        })
    }
)