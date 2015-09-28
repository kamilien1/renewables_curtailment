# chart
library(dplyr,verbose=F)
library(ggplot2,verbose=F)



# create output strings for printing pdfs
# plot graph
# save pdf

# six graphs
# 1: % wind curtailed vs year
# include a table of data
# 2: plot 1 by months, averaged, no table
# 3: plot 1 by hour, averaged, no table
# 4: year == 2017
# two y-axes:
# axis 1: demand after transmission
# axis 2: % wind curtailment
# x-axis: hourly, 24 hour period
# average axis 1,2 data for 1 month
# plot for winter, summer, and spring_fall seasons
# 3 separate plots
# 5: stacked area plot
# multiple layers to plot
# base_layer = hourly_demand (demand+net transmission)
# l1 = base + solar after T
# l2 = l1+wind after T == demand
# NOTE: make l2 a dark line
# l3 = l2+sol curtailed
# l4 = l3+wind curtailed
# NOTE: make l3, l4 area be pattern, light fill
# and make all others be heavy fill
# so factor and order the factor by
# base, wind after t, solar after t, sol curtail, wind curtail
# choose a few days
# x-axis: hourly, 1-4 days
# 6: twh output annually
# levels: wind after t, solar after t, base
# NOTE: check to make sure wind after t also excludes curtailment

library(ggplot2,verbose=F)
library(Hmisc,verbose=F)
library(gridExtra,verbose=F)

# plot 1: wind curtailment, annually



plot1 <- function(annual_analysis) {
#     library(gridExtra)
#     plot1 <- 
        ggplot(annual_analysis, aes(x=year,y=wind_pct_avg_curtail))+geom_bar(stat='identity')+
        xlab("")+ylab("Wind Curtailment\nPercentage")+ggtitle("Annual Wind Curtailment")+
        theme(legend.text=element_text(size=15),
              legend.title=element_text(size=16,face='bold'),
              axis.title=element_text(size=14,face='bold'),
              plot.title=element_text(size=18,face='bold'),
              legend.position='bottom',
              axis.text = element_text(size=14))+
            geom_text(vjust=1.4,colour="white",size=3.5,aes(label=wind_pct_avg_curtail))
    
    
#     library(reshape2)
#     test <- select(annual_analysis, year, wind_pct_avg_curtail)
#     test <- dcast(test,.~year)
#     test[,1] = "Average Wind\n Curtailment Percentage"
#     row.names(test)=''
#     names(test)[1] = "Year"
#     g <- tableGrob(test)
#     grid.arrange(plot1,g,nrow=2,heights=c(8/10,2/10))

}


# plot 2: wind curtailment, monthly average of all years
plot2 <- function(monthly_analysis){
    ggplot(monthly_analysis%>%group_by(month)%>% 
        summarise(wind_pct_curtail=mean(wind_pct_avg_curtail)),
        aes(x=factor(month),y=wind_pct_curtail))+geom_bar(stat='identity')+
        xlab("")+ylab("Wind Curtailment\nPercentage")+ggtitle("Monthly Wind Curtailment\nAveraged Over all Years")+
        theme(legend.text=element_text(size=15),
              legend.title=element_text(size=16,face='bold'),
              axis.title=element_text(size=14,face='bold'),
              plot.title=element_text(size=18,face='bold'),
              legend.position='bottom',
              axis.text = element_text(size=14))
}
# plot2(monthly_analysis)
# plot 3 wind curtailment, hourly average of all years

plot3 <- function(hourly_analysis){
    ggplot(hourly_analysis%>%group_by(hour)%>% 
               summarise(wind_pct_curtail=mean(wind_pct_avg_curtail)),
           aes(x=factor(hour),y=wind_pct_curtail))+geom_bar(stat='identity')+
        xlab("")+ylab("Wind Curtailment\nPercentage")+ggtitle("Hourly Wind Curtailment\nAveraged Over all Years")+
        theme(legend.text=element_text(size=15),
              legend.title=element_text(size=16,face='bold'),
              axis.title=element_text(size=14,face='bold'),
              plot.title=element_text(size=18,face='bold'),
              legend.position='bottom',
              axis.text = element_text(size=14))
}
# plot3(hourly_analysis)


# plot 4: typical curtailment situation

#spring_fall month = 10
# summer month 8
# winter month 12

# http://steffi.ca/thinkR/?p=91
# http://rpubs.com/kohske/dual_axis_in_ggplot2
# http://stackoverflow.com/questions/13875637/changing-the-y-axis-text-size-with-doubleyscale-plot
# http://www.perceptualedge.com/articles/visual_business_intelligence/dual-scaled_axes.pdf
# library(gridExtra)
# library(grid)

plot4_pre_clean <- function(df_pc, ye = 2017){
    library(dplyr,verbose=F)

    small_df <- select(subset(df_pc,year==ye),date,month,year, hourly_demand_min_scale, 
                       hourly_demand_max_scale,season,hour,curtail_pmin,curtail_pmax,gwh_wind_onshore)
    df_summary <- small_df %>% group_by(year, season, month, hour) %>% 
        summarise(demand=(sum(hourly_demand_min_scale)+sum(hourly_demand_max_scale))/2,
           wind_curtail = ((sum(curtail_pmin))+(sum(curtail_pmax)))/2,
           wind_curtail=100*abs(wind_curtail/(sum(gwh_wind_onshore))))
    
    df_summary
}

# get seasons



## make it prettier
# http://www.ling.upenn.edu/~joseff/rstudy/week4.html

# without ggplot
# http://robjhyndman.com/hyndsight/r-graph-with-two-y-axes/

plot4_aug <- function(df_summary){
    # month 8
    library(Hmisc,verbose=F)
    summer <- subset(df_summary,month==8)
    par(mar=c(5,6,4,5),bg='white')
    plot(summer$hour,summer$demand,type="l",lwd=4,col="red",ylab="",xlab="",xlim=c(0,23),ylim=c(0,max(summer$demand)),xaxs='i')
    abline(v=summer$hour,col='#E4E4E4',lwd=80)
    grid(lwd="1",lty=1,col='white')
    minor.tick(nx=2,ny=2,tick.ratio=0.5)
    axis(1, tck=1,lwd=0.5,at=seq(0,25,2.5), col.ticks="white",labels=F)
    lines(summer$hour,summer$demand,type="l",lwd=4,col="black",ylab="",xlab="",xlim=c(0,23))
    mtext("GWh\nDemand",side=2,line=3)
    par(new=TRUE)
    # try - or + wind curtail
    plot(summer$hour,summer$wind_curtail,,type="l",lwd=4,col="#6B90FF",
         xaxt="n",yaxt="n",xlab="",ylab="",xlim=c(0,23))
    axis(4)
    mtext("Wind Curtailment\nPercentage",side=4,line=3)
    legend("bottomright",inset=c(0.1,0),col=c("black","#6B90FF"),
           lty=1,lwd=4,legend=c("Demand","Wind Curtailment"))
    
    tit <- paste("August ",as.character(unique(df_summary$year))," Hourly Average Demand \nand Curtailment Levels",sep='')
    title(tit)
}
# plot4_aug(df_summary)

plot4_oct <- function(df_summary){
    # month10
    library(Hmisc,verbose=F)
    spring_fall <- subset(df_summary,month==10)
    par(mar=c(5,6,4,5),bg='white')
    plot(spring_fall$hour,spring_fall$demand,type="l",lwd=4,col="red",ylab="",xlab="",xlim=c(0,23),ylim=c(0,max(spring_fall$demand)),xaxs='i')
    abline(v=spring_fall$hour,col='#E4E4E4',lwd=80)
    grid(lwd="1",lty=1,col='white')
    minor.tick(nx=2,ny=2,tick.ratio=0.5)
    axis(1, tck=1,lwd=0.5,at=seq(0,25,2.5), col.ticks="white",labels=F)
    lines(spring_fall$hour,spring_fall$demand,type="l",lwd=4,col="black",ylab="",xlab="",xlim=c(0,23))
    mtext("GWh\nDemand",side=2,line=3)
    par(new=TRUE)
    # try - or + wind curtail
    plot(spring_fall$hour,spring_fall$wind_curtail,,type="l",lwd=4,col="#6B90FF",xaxt="n",yaxt="n",xlab="",ylab="",xlim=c(0,23))
    axis(4)
    mtext("Wind Curtailment\nPercentage",side=4,line=3)
    legend("bottomright",inset=c(0.1,0),col=c("black","#6B90FF"),lty=1,lwd=4,legend=c("Demand","Wind Curtailment"))
    tit <- paste("October ",as.character(unique(df_summary$year))," Hourly Average Demand \nand Curtailment Levels",sep='')
    title(tit)
}
# plot4_oct(df_summary)

plot4_dec <- function(df_summary){
    # month 12
    library(Hmisc,verbose=F)
    winter <- subset(df_summary,month==12)
    par(mar=c(5,6,4,5),bg='white')
    plot(winter$hour,winter$demand,type="l",lwd=4,col="red",ylab="",xlab="",xlim=c(0,23),ylim=c(0,max(winter$demand)),xaxs='i')
    abline(v=winter$hour,col='#E4E4E4',lwd=80)
    grid(lwd="1",lty=1,col='white')
    minor.tick(nx=2,ny=2,tick.ratio=0.5)
    axis(1, tck=1,lwd=0.5,at=seq(0,25,2.5), col.ticks="white",labels=F)
    lines(winter$hour,winter$demand,type="l",lwd=4,col="black",ylab="",xlab="",xlim=c(0,23))
    mtext("GWh\nDemand",side=2,line=3)
    par(new=TRUE)
    # try - or + wind curtail
    plot(winter$hour,winter$wind_curtail,,type="l",lwd=4,col="#6B90FF",xaxt="n",yaxt="n",xlab="",ylab="",xlim=c(0,23))
    axis(4)
    mtext("Wind Curtailment\nPercentage",side=4,line=3)
    legend("bottomright",inset=c(0.1,0),col=c("black","#6B90FF"),lty=1,lwd=4,legend=c("Demand","Wind Curtailment"))
    tit <- paste("December ",as.character(unique(df_summary$year))," Hourly Average Demand \nand Curtailment Levels",sep='')
    title(tit)
}
# plot4_dec(df_summary)

plot4_month <- function(df_summary,mo=1){
    # month 12
    library(Hmisc,verbose=F)
    winter <- subset(df_summary,month==mo)
    par(mar=c(5,6,4,5),bg='white')
    plot(winter$hour,winter$demand,type="l",lwd=4,col="red",ylab="",xlab="",xlim=c(0,23),ylim=c(0,max(winter$demand)),xaxs='i')
    abline(v=winter$hour,col='#E4E4E4',lwd=80)
    grid(lwd="1",lty=1,col='white')
    minor.tick(nx=2,ny=2,tick.ratio=0.5)
    axis(1, tck=1,lwd=0.5,at=seq(0,25,2.5), col.ticks="white",labels=F)
    lines(winter$hour,winter$demand,type="l",lwd=4,col="black",ylab="",xlab="",xlim=c(0,23))
    mtext("GWh\nDemand",side=2,line=3)
    par(new=TRUE)
    # try - or + wind curtail
    plot(winter$hour,winter$wind_curtail,,type="l",lwd=4,col="#6B90FF",xaxt="n",yaxt="n",xlab="",ylab="",xlim=c(0,23))
    axis(4)
    mtext("Wind Curtailment\nPercentage",side=4,line=3)
    legend("bottomright",inset=c(0.1,0),col=c("black","#6B90FF"),lty=1,lwd=4,legend=c("Demand","Wind Curtailment"))
    tit <- paste(mo,'-',as.character(unique(df_summary$year)),
    " Hourly Average Demand \nand Curtailment Levels",sep='')
    title(tit)
}




## plot 5

plot5 <- function(df,ye=2017,mo=1,days=4:10){
    # model_part4
    library(tidyr)
    ye = as.numeric(ye)
    mo = as.numeric(mo)
    days = as.numeric(days)
    plot_set <- df %>% subset(year==ye & month == mo & day %in% days) %>%
        mutate(base_avg = (hourly_demand_max_scale+hourly_demand_min_scale)/2, 
               wind_curtail_avg = abs((curtail_pmin+curtail_pmax)/2),
               solar_curtail_avg=abs((curtail_solar_pmin+curtail_solar_pmax)/2),
               base_gen_avg = abs((total_base_gen_min+total_base_gen_max)/2),
               wind_produced = gwh_wind_onshore-wind_curtail_avg,
               solar_produced = gwh_solar - solar_curtail_avg,
               demand = base_gen_avg+wind_produced+solar_produced) %>%
        select(day,hour,date, base_gen_avg,wind_produced, solar_produced,
               solar_curtail_avg, wind_curtail_avg,demand)%>%
        unite("day_hour",day,hour) %>%
        gather("type_gen","gwh",base_gen_avg:wind_curtail_avg )
    
    
    ggplot(plot_set,aes(x=date,y=gwh,fill=type_gen,group=type_gen )) + geom_area(position='stack',stat='identity')+
        geom_line(aes(y=demand),stat='identity',colour="black")+
        scale_fill_manual(labels = c("Base Generation","Wind Consumed","Solar Consumed",
                                     "Solar Curtailed","Wind Curtailed"),
                            values = c("#BAC2D9", "#6B90FF", "#FFB846", 
                                       "#F3FF0A", "#C2D1FF"), name="Load \nType")+
        theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))+
        ylab("GWh")+xlab("")+ggtitle("2017 Hourly Load Profile and Curtailment")+
        theme(legend.text=element_text(size=14),
              legend.title=element_text(size=16,face='bold'),
              axis.title=element_text(size=16,face='bold'),
              plot.title=element_text(size=18,face='bold'),
              legend.position='bottom',
              axis.text = element_text(size=16))+
        guides(fill=guide_legend(nrow=2,byrow=T))
}
#plot5(model_part4,ye=2025,mo=1,days=9)




plot5_double_lines <- function(df,ye=2017,mo=1,days=4:10){
    # model_part4
    library(tidyr)
    ye = as.numeric(ye)
    mo = as.numeric(mo)
    days = as.numeric(days)
    plot_set <- df %>% subset(year==ye & month == mo & day %in% days) %>%
        mutate(base_avg = (hourly_demand_max_scale+hourly_demand_min_scale)/2, 
               wind_curtail_avg = abs((curtail_pmin+curtail_pmax)/2),
               solar_curtail_avg=abs((curtail_solar_pmin+curtail_solar_pmax)/2),
               base_gen_avg = abs((total_base_gen_min+total_base_gen_max)/2),
               wind_produced = gwh_wind_onshore-wind_curtail_avg,
               solar_produced = gwh_solar - solar_curtail_avg,
               # demand profile INCLUDING transmission
               demand_t = base_gen_avg+wind_produced+solar_produced,
               # demand profile EXCLUDING transmission
               demand_no_t = (hourly_demand_max_scale+hourly_demand_min_scale)/2,
               gen_pmin = (gen_pmin_min+gen_pmax_min)/2) %>%
        select(day,hour,date, base_gen_avg,wind_produced, solar_produced,
               solar_curtail_avg, wind_curtail_avg,demand_t,demand_no_t,gen_pmin,base_avg)%>%
        unite("day_hour",day,hour) %>%
        gather("type_gen","gwh",base_gen_avg:wind_curtail_avg )
    
    
    
    gtitle <- paste(ye," Simulation Hourly Demand,\nCurtailment",sep="")
    
    ggplot(plot_set,aes(x=date,y=gwh,fill=type_gen,group=type_gen )) + 
        geom_area(position='stack',stat='identity')+
        geom_line(aes(y=demand_t),stat='identity',colour="black")+
        geom_line(aes(y=demand_no_t),stat='identity',colour="white")+
        #geom_line(aes(y=gen_pmin),stat='identity',colour='black',size=2,linetype=2)+
        scale_fill_manual(labels = c("Base Generation","Wind Consumed","Solar Consumed",
                                     "Solar Curtailed","Wind Curtailed"),
                          values = c("#BAC2D9", "#6B90FF", "#FFB846", 
                                     "#F3FF0A", "light green"), name="Load \nType")+
        theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))+
        ylab("GWh")+xlab("")+ggtitle(gtitle)+
        theme(legend.text=element_text(size=14),
              legend.title=element_text(size=16,face='bold'),
              axis.title=element_text(size=16,face='bold'),
              plot.title=element_text(size=18,face='bold'),
              legend.position='bottom',
              axis.text = element_text(size=16))+
        guides(fill=guide_legend(nrow=2,byrow=T))
    
}

#plot5_double_lines(model_part4,ye=2014,mo=1,days=10:12)



## plot 6
plot6 <- function(annual_analysis){
    plot_annual <- annual_analysis %>% mutate(wind_consumed = wind_gen_twh +wind_avg_curtailed_twh,
                                      solar_consumed = solar_gen_twh + solar_curtailed_avg_twh) %>%
                select(year,wind_consumed, solar_consumed, total_base_gen_avg_twh) %>%
                gather("gen_type","twh",2:4)
    
    ggplot(plot_annual,aes(x=year,y=twh,fill=gen_type,group=gen_type))+ geom_area(position='stack',stat='identity')+
        scale_fill_manual(labels= c("Wind","Solar","Base Load"),
                values = c( "#6B90FF","#FFB846","#BAC2D9"), name="Generation \nSource" )+
        ylab("TWh")+xlab("")+ggtitle("Annual Generation Forecast")+
        theme(legend.text=element_text(size=15),
              legend.title=element_text(size=16,face='bold'),
              axis.title=element_text(size=14,face='bold'),
              plot.title=element_text(size=18,face='bold'),
              legend.position='bottom',
              axis.text = element_text(size=16))
} 
# plot6(annual_analysis)

plot7 <- function(aa) {
    
    ggplot(aa,
           aes(x=factor(year),y=total_base_gen_avg_util_hours))+geom_bar(stat='identity')+
        xlab("")+ylab("Hours")+ggtitle("Base Generation Utilizaiton Hours\nIncluding Outflow Transmission")+
        theme(legend.text=element_text(size=15),
              legend.title=element_text(size=16,face='bold'),
              axis.title=element_text(size=14,face='bold'),
              plot.title=element_text(size=18,face='bold'),
              legend.position='bottom',
              axis.text = element_text(size=14),
              axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))
    
}
#plot7(annual_analysis)

# ##############################################################################
# ######################### Exploration   ######################################
# ##############################################################################
# # wind pct curtailed by hour of the year
# ggplot(hourly_analysis, aes(x=factor(hour),y=wind_pct_avg_curtail,colour=factor(year)))+geom_freqpoly(aes(group=factor(year)),stat='identity')
# # monthly
# ggplot(monthly_analysis, aes(x=factor(month),y=wind_pct_avg_curtail,colour=factor(year)))+geom_freqpoly(aes(group=factor(year)),stat='identity')
# 
# 
# # average wind profile 
# library(lubridate)
# wind_plot <- model_part4 %>% group_by(month, hour) %>% summarize(power_out = sum(wind_total))
# 
# # wind profile hourly by month
# ggplot(wind_plot, aes(x=factor(hour),y=power_out,colour=factor(month)))+geom_freqpoly(aes(group=factor(month)),stat='identity')
# 
# 
# # compare different models curtailment
# ggplot(all_annual, aes(x=factor(year),y=wind_pct_avg_curtail,colour=factor(gridmodel)))+geom_freqpoly(aes(group=factor(gridmodel)),stat='identity',size=1)+
#     theme(text = element_text(size=20)) + ggtitle("Wind % Curtailed Annually Years")
# 
# base_util_hrs_avg_noT
# base_util_hrs_avg
# 
# 
# ggplot(all_annual, aes(x=factor(year),y=base_util_hrs_avg,colour=factor(gridmodel)))+geom_freqpoly(aes(group=factor(gridmodel)),stat='identity',size=1)+
#     theme(text = element_text(size=20)) + ggtitle("Base Util Hours Annually Years")
# 
# 
# #comparing grid models
# # in progress
# ggplot(all_annual,aes(x=factor(year),y=wind_pct_avg_curtail))+geom_point(aes(colour=gridmodel),size=4)
# # zjk
# ggplot(subset(all_annual, gridmodel > 'k' & transmission_model=='dem_scale'),aes(x=factor(year),y=wind_pct_avg_curtail,group=gridmodel))+
#     geom_line(aes(colour=gridmodel),size=2)
# 
# 
# # wind pct curtail zjk t adjust
# ggplot(subset(all_annual, gridmodel == 'zjk_t_adjust'),aes(x=factor(year),y=wind_pct_avg_curtail,group=transmission_model))+
#     geom_line(aes(colour=transmission_model),size=2) + geom_text(aes(label=wind_pct_avg_curtail))
# 
# # base util hrs avg
# ggplot(subset(all_annual, gridmodel == 'zjk_t_adjust'),aes(x=factor(year),y=base_util_hrs_avg,group=transmission_model))+
#     geom_line(aes(colour=transmission_model),size=2)+ geom_text(aes(label=base_util_hrs_avg))
# 
# 
# # jjt comparisions
# ggplot(subset(all_annual, gridmodel < 'k'),aes(x=factor(year),y=wind_pct_avg_curtail,group=gridmodel))+
#     geom_line(aes(colour=gridmodel))
# 
# 
# # load duration curve
# test_stuff <- subset(model_part4, year==2014)
# dim(test_stuff)
# ymax <- max(test_stuff$hourly_demand_max_scale)
# ymin = min(test_stuff$hourly_demand_max_scale)
# y_seq <- seq(from=ymax,to=ymin,length.out=8760)
# hourly_dem <- test_stuff$hourly_demand_max_scale
# x_seq <- sapply(y_seq, function(x)(sum(x <hourly_dem)))
# df <- data.frame(x_seq=x_seq,y_seq=y_seq)
# df$avg = mean(df$y_seq)
# ggplot(df,aes(x=x_seq,y=y_seq))+geom_line(stat='identity',size=3,colour='transparent')+
#     stat_smooth(colour='black',size=3)+geom_line(aes(y=avg),colour='light blue',size=2)+
#     ggtitle("Load Duration Curve Estimation for \nJJT Region in Year 2013")+
#     xlab("Hours")+ylab("GW Demand")+
#     theme(legend.text=element_text(size=15),
#           legend.title=element_text(size=16,face='bold'),
#           axis.title=element_text(size=18,face='bold'),
#           plot.title=element_text(size=18,face='bold'),
#           legend.position='bottom',
#           axis.text = element_text(size=20))
#     
#   
# ggplot(all_annual,aes(x=I(100*extra_trans_capacity_renewables),y=wind_pct_avg_curtail))+
#     geom_bar(stat='identity')+
#     ggtitle("Sensitivity Analysis for JJT 2018:\nMultiple Transmission Capacity Levels")+
#     xlab("Extra Transmission Capacity % \nAlotted Toward Renewables")+ylab("Curtailment %")+
#     theme(legend.text=element_text(size=15),
#           legend.title=element_text(size=16,face='bold'),
#           axis.title=element_text(size=18,face='bold'),
#           plot.title=element_text(size=18,face='bold'),
#           legend.position='bottom',
#           axis.text = element_text(size=20))
# 
# # now do load duration curve for base load
# # 
# ymax_no_ws <- max((with(test_stuff,hourly_demand_max_scale-gwh_wind_onshore-gwh_solar)))
# ymin_no_ws <- min((with(test_stuff,hourly_demand_max_scale-gwh_wind_onshore-gwh_solar)))
# y_seq_no_ws <- seq(from=ymax_no_ws,to=ymin_no_ws,length.out=8760)
# x_seq_no_ws <- sapply(y_seq_no_ws,function(x) (sum(x < (with(test_stuff,hourly_demand_max_scale-gwh_wind_onshore-gwh_solar)))))
# df_no_ws <- data.frame(x_seq_no_ws = x_seq_no_ws,y_seq_no_ws=y_seq_no_ws)
# ggplot(df_no_ws,aes(x=x_seq_no_ws,y=y_seq_no_ws)) + geom_line(stat='identity') +
#     ggtitle("Load Duration Curve for base load\n(no wind or solar)")
# # 
# # # plot hour on x
# # # on y show the sum of: wind, solar, base gen
# # # on a line show demand after T
# # 
# # 
# # # show output and demand
# # # NOTE these are summary values, we may have some silly issue 
# # # check: the demand sometimes is larger than base load
# library(tidyr)
# demand_plot <- select(hourly_analysis, c(hour,demand_afterT_avg_gwh))
# demand_plot <- unite(demand_plot, yearhr, year, hour)
# supply_plot <- select(hourly_analysis,c(hour,total_base_gen_avg_gwh,wind_gen_gwh,solar_gen_gwh))
# supply_plot <- unite(supply_plot,yearhr, year, hour)
# supply_plot <- gather(supply_plot,"gentype","value", 2:4)
# head(demand_plot)
# all_plot <- left_join(supply_plot,demand_plot,by='yearhr')
# 
# ggplot(subset(all_plot,yearhr < "2018"), aes(x=yearhr,y=value,fill=gentype,group=gentype))+geom_area(position='stack')+
#     geom_line(aes(y=demand_afterT_avg_gwh),stat='identity')#+ coord_cartesian(ylim=c(12000,18000))
# 

# 
# 
# ##### dual-axis plot tests
# 
# ## method 1
# g.top <- ggplot(winter,aes(x=factor(hour),y=demand,group=1))+geom_line(size=4) +
#     #theme(plot.margin = unit(c(1,5,-30,6),units="points"),
#     #      axis.title.y = element_text(vjust =0.25)) +
#     labs(y = "Demand, TWh")+ggtitle("Local + Net Transmission Demand")
# 
# g.bottom <- ggplot(winter, aes(x = factor(hour), y = wind_curtail,group=1)) +
#     geom_line(size=4) +
#     #theme(plot.margin = unit(c(0,5,1,1),units="points")) +
#     labs(x = "Hour", y = "% Wind Curtailed")+ggtitle("Wind Curtail")
# 
# ## plot graphs and set relative heights
# grid.arrange(g.top,g.bottom, heights = c(2/5, 3/5))
# 
# 
# # method 2: ggplot
# http://rpubs.com/kohske/dual_axis_in_ggplot2
# summary(winter)
# max_scale=max(winter$demand)
# min_scale=min(winter$demand)
# max_wind = max(winter$wind_curtail)
# min_wind = min(winter$wind_curtail)
# winter <- mutate(winter,wind_scaled = (max_scale-min_scale)*wind_curtail/max_wind+min_scale)
# 
# 
# library(gtable)
# p1 <- ggplot(winter,aes(x=factor(hour),y=demand,group=1))+geom_line(size=4) +
#     labs(y = "Demand, TWh")+ggtitle("Local + Net Transmission Demand and Wind Curtailment")
# 
# p2 <- ggplot(winter, aes(x = factor(hour), y = wind_curtail,group=1)) +
#     geom_line(size=4,colour="blue") +
#     labs(x = "Hour", y = "% Wind Curtailed")+ggtitle("Wind Curtail")+
#     theme(panel.background = element_rect(fill = NA),panel.grid.major=element_blank(),panel.grid.minor=element_blank())
# 
# # build
# g1 <- ggplot_gtable(ggplot_build(p1))
# g2 <- ggplot_gtable(ggplot_build(p2))
# 
# pp <- c(subset(g1$layout, name == "panel", se = t:r))
# g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
#                      pp$l, pp$b, pp$l)
# 
# # get the left axis data
# ia <- which(g2$layout$name == "axis-l")
# # get all grobs from ia
# ga <- g2$grobs[[ia]]
# # get all children of ga
# ax <- ga$children[[2]]
# # set the width
# ax$widths <- rev(ax$widths)
# #
# ax$grobs <- rev(ax$grobs)
# ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
# g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
# g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
# 
# # draw it
# grid.draw(g)