# chart
library(dplyr)
library(ggplot2)



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



# plot 1: wind curtailment, annually
ggplot(annual_analysis, aes(x=year,y=wind_pct_avg_curtail))+geom_bar(stat='identity')

# plot 2: wind curtailment, monthly average of all years
ggplot(monthly_analysis%>%group_by(month)%>% 
    summarize(wind_pct_curtail=mean(wind_pct_avg_curtail)),
    aes(x=factor(month),y=wind_pct_curtail))+geom_bar(stat='identity')

# plot 3 wind curtailment, hourly average of all years
ggplot(hourly_analysis%>%group_by(hour)%>% 
           summarize(wind_pct_curtail=mean(wind_pct_avg_curtail)),
       aes(x=factor(hour),y=wind_pct_curtail))+geom_bar(stat='identity')



# plot 4: typical curtailment situation
# http://steffi.ca/thinkR/?p=91
# http://rpubs.com/kohske/dual_axis_in_ggplot2
# http://stackoverflow.com/questions/13875637/changing-the-y-axis-text-size-with-doubleyscale-plot
# http://www.perceptualedge.com/articles/visual_business_intelligence/dual-scaled_axes.pdf
library(gridExtra)

# subset to year 2017
# 
small_df <- select(subset(model_part4,year==2017),date,month, hourly_demand_min_scale, 
                   hourly_demand_max_scale,season,hour,curtail_pmin,curtail_pmax,gwh_wind_onshore)

df_summary <- small_df %>% group_by(season, month, hour) %>% summarize(demand=(sum(hourly_demand_min_scale)+sum(hourly_demand_max_scale))/2,
                    wind_curtail = ((sum(curtail_pmin))+(sum(curtail_pmax)))/2,wind_curtail=100*abs(wind_curtail/(sum(gwh_wind_onshore))))



#spring_fall month = 10
# summer month 8
# winter month 12


# without ggplot
# http://robjhyndman.com/hyndsight/r-graph-with-two-y-axes/

winter <- subset(df_summary,month==12)
head(winter)
par(mar=c(5,4,4,5)+.1)
plot(winter$hour,winter$demand,type="l",col="red",ylab="",xlab="")
mtext("demand",side=2,line=3)
par(new=TRUE)
plot(winter$hour,winter$wind_curtail,,type="l",col="blue",xaxt="n",yaxt="n",xlab="Hour",ylab="")
axis(4)
mtext("wind curtailment",side=4,line=3)
legend("topleft",col=c("red","blue"),lty=1,legend=c("demand","curtailment %"))

##############################################################################
######################### Exploration   ######################################
##############################################################################
# wind pct curtailed by hour of the year
ggplot(hourly_analysis, aes(x=factor(hour),y=wind_pct_avg_curtail,colour=factor(year)))+geom_freqpoly(aes(group=factor(year)),stat='identity')
# monthly
ggplot(monthly_analysis, aes(x=factor(month),y=wind_pct_avg_curtail,colour=factor(year)))+geom_freqpoly(aes(group=factor(year)),stat='identity')


# average wind profile 
library(lubridate)
wind_plot <- model_part4 %>% group_by(month, hour) %>% summarize(power_out = sum(wind_total))

# wind profile hourly by month
ggplot(wind_plot, aes(x=factor(hour),y=power_out,colour=factor(month)))+geom_freqpoly(aes(group=factor(month)),stat='identity')


# compare different models curtailment
ggplot(all_annual, aes(x=factor(year),y=wind_pct_avg_curtail,colour=factor(gridmodel)))+geom_freqpoly(aes(group=factor(gridmodel)),stat='identity',size=1)+
    theme(text = element_text(size=20)) + ggtitle("Wind % Curtailed Annually Years")

base_util_hrs_avg_noT
base_util_hrs_avg


ggplot(all_annual, aes(x=factor(year),y=base_util_hrs_avg,colour=factor(gridmodel)))+geom_freqpoly(aes(group=factor(gridmodel)),stat='identity',size=1)+
    theme(text = element_text(size=20)) + ggtitle("Base Util Hours Annually Years")


#comparing grid models
# in progress
ggplot(all_annual,aes(x=factor(year),y=wind_pct_avg_curtail))+geom_point(aes(colour=gridmodel),size=4)
# zjk
ggplot(subset(all_annual, gridmodel > 'k' & transmission_model=='dem_scale'),aes(x=factor(year),y=wind_pct_avg_curtail,group=gridmodel))+
    geom_line(aes(colour=gridmodel),size=2)


# wind pct curtail zjk t adjust
ggplot(subset(all_annual, gridmodel == 'zjk_t_adjust'),aes(x=factor(year),y=wind_pct_avg_curtail,group=transmission_model))+
    geom_line(aes(colour=transmission_model),size=2) + geom_text(aes(label=wind_pct_avg_curtail))

# base util hrs avg
ggplot(subset(all_annual, gridmodel == 'zjk_t_adjust'),aes(x=factor(year),y=base_util_hrs_avg,group=transmission_model))+
    geom_line(aes(colour=transmission_model),size=2)+ geom_text(aes(label=base_util_hrs_avg))


# jjt comparisions
ggplot(subset(all_annual, gridmodel < 'k'),aes(x=factor(year),y=wind_pct_avg_curtail,group=gridmodel))+
    geom_line(aes(colour=gridmodel))


# load duration curve
test_stuff <- subset(model_part4, year==2021)
dim(test_stuff)
ymax <- max(test_stuff$hourly_demand_max_scale)
ymin = min(test_stuff$hourly_demand_max_scale)
y_seq <- seq(from=ymax,to=ymin,length.out=8760)
hourly_dem <- test_stuff$hourly_demand_max_scale
x_seq <- sapply(y_seq, function(x)(sum(x <hourly_dem)))
df <- data.frame(x_seq=x_seq,y_seq=y_seq)
ggplot(df,aes(x=x_seq,y=y_seq))+geom_line(stat='identity')+ggtitle("Load Duration Curve")
# now do load duration curve for base load
# 
ymax_no_ws <- max((with(test_stuff,hourly_demand_max_scale-gwh_wind_onshore-gwh_solar)))
ymin_no_ws <- min((with(test_stuff,hourly_demand_max_scale-gwh_wind_onshore-gwh_solar)))
y_seq_no_ws <- seq(from=ymax_no_ws,to=ymin_no_ws,length.out=8760)
x_seq_no_ws <- sapply(y_seq_no_ws,function(x) (sum(x < (with(test_stuff,hourly_demand_max_scale-gwh_wind_onshore-gwh_solar)))))
df_no_ws <- data.frame(x_seq_no_ws = x_seq_no_ws,y_seq_no_ws=y_seq_no_ws)
ggplot(df_no_ws,aes(x=x_seq_no_ws,y=y_seq_no_ws)) + geom_line(stat='identity') +
    ggtitle("Load Duration Curve for base load\n(no wind or solar)")

# plot hour on x
# on y show the sum of: wind, solar, base gen
# on a line show demand after T


# show output and demand
# NOTE these are summary values, we may have some silly issue 
# check: the demand sometimes is larger than base load
demand_plot <- select(hourly_analysis, c(hour,demand_afterT_avg_gwh))
demand_plot <- unite(demand_plot, yearhr, year, hour)
supply_plot <- select(hourly_analysis,c(hour,total_base_gen_avg_gwh,wind_gen_gwh,solar_gen_gwh))
supply_plot <- unite(supply_plot,yearhr, year, hour)
supply_plot <- gather(supply_plot,"gentype","value", 2:4)
head(demand_plot)
all_plot <- left_join(supply_plot,demand_plot,by='yearhr')

ggplot(subset(all_plot,yearhr < "2018"), aes(x=yearhr,y=value,fill=gentype,group=gentype))+geom_area(position='stack')+
    geom_line(aes(y=demand_afterT_avg_gwh),stat='identity')+ coord_cartesian(ylim=c(12000,18000))




