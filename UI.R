# taken from
# https://aeliv002.shinyapps.io/developingDataProducts
# https://github.com/aeliv002/developing-data-products-project/blob/master/ui.R


library(shiny)
library(xtable)


appTitle <- "Simulations";

#
tPeriodInput <- numericInput("time_period", 
                            "Pmin Calc Time Period", 
                            1, min = 1, max = 24, step = 1)

# transmission data
tScaleInput <- numericInput("transmission_scale", 
                           "Trans Scale", 
                           1, min = 0, max = 2, step = .01)

# may need to have choices be a list with names
# ex: list <- c("display_name" = "display_value",etc/etc)
t_model_names <- unique(t_sched$grid_model)[order(unique(t_sched$grid_model))]
tModelInput <- selectInput('transmission_model', 
                               'Trans Model', choices = t_model_names,
                               selected="dem_scale")

# ignore peak season input for now (not used and hard to format)
#peakSeasonInput = checkboxGroupInput("peak_season", "peak season", choices = 1:12,selected =c(12,1,2,6,7,8))

yearRangeInput <- sliderInput("date_range", "years",
            min = 2012, max = 2025, value = c(2012,2025),sep='')


# # grid models
all_gmodels <- unique(generation_forecast$grid_model)
base_models <- c('jjt','zjk','hlbe','EIMAR')

allModelInput <- selectInput('gridmodel', 
                           'specialty model', choices = all_gmodels,
                           selected="jjt")
baseModelInput <- selectInput('special_subset_model', 
                             'base model', choices = base_models,
                             selected="jjt")

# # base load reserve
spinningReserveInput <- sliderInput("spinning_reserve", "spinning reserve",
                                    min = 0.8, max = 1.3, value = 1.08,step=0.01)

# 
# # reneawbles t data
windTShareInput <- numericInput("wind_transmission_share","wind t_out share",value=0,min=0,max=1,step=0.01)
solarTShareInput <- numericInput("solar_transmission_share","solar t_out share",value=0,min=0,max=1,step=0.01)
extraTCapInput <- sliderInput("extra_trans_capacity_renewables","extra t_out cap % for renewables",value=0.25,min=0,max=1,step=0.01)


# 
# # growth cases
gcases <- unique(growth_cases$growth_model)
growthCaseInput <- selectInput("growth_case","growth cases",selected="noscale",choices=gcases)
demandScaleInput <- numericInput("demand_scale","scale demand",value=1,min=0,max=10,step=0.01)



# # wind and solar datapla
windScaleInput <- numericInput("wind_scale_factor","Wind Scale",value=0.9,min=0,max=10,step=0.01)
solarScaleInput <-numericInput("solar_scale_factor","Solar Scale",value=1,min=0,max=10,step=0.01)
offsetWindInput <- numericInput("offset_wind_hours","Wind Hours Offset",value=0,min=0,max=24,step=1)


# # gen data
rCCPcoalInput <- numericInput("r_ccp_coal","CHP Coal iCap Ratio",value=0.4,min=0,max=1,step=0.01)
rCCPngInput <- numericInput("r_ccp_ng","CHP NatGas iCap Ratio",value=0.9,min=0,max=1,step=0.01)


## plot data
plot_vars <- names(annual_analysis)[c(-1)]
plotYInput <- selectInput("y_plot_var","Y Variable to Plot",selected="wind_pct_avg_curtail",choices=plot_vars)

helpTextElement <- helpText(
    "This simulation runs multiple graphs and shows the raw 
    data associated with select graphs."
)


# run ggplot in the server.R file and plot "plot1"
tabPanel1 <- tabPanel("Plots Go Here", 
                      plotOutput("plot1"))

# print out small summaries of stuff
tabPanel2 <- tabPanel("Summary of distribution", 
                      "Data Saved",
                      verbatimTextOutput("data_saved"),
                      "Data Downloaded as output.xlsx",
                      verbatimTextOutput("data_downloaded"))

# print out the entire table
tabPanel3 <- tabPanel("Generated values", 
                      tableOutput("table1"))

# this is the action button to generate our data
actionButton <- actionButton("actionButton", 
                             "Run Model")

# save button to append it to all_models
saveButton <- actionButton("saveButton","Save Model")
downloadButton <- actionButton("downloadData","Download Data")


## plot button to update plot
updatePlotButton <- actionButton("plotModel","Update Plot")


shinyUI(
    fluidPage(
        
        # math jax adds math to the page, probably don't need it
        withMathJax(),
        titlePanel(appTitle),
        
        sidebarLayout(
            fluid=TRUE,
            
            sidebarPanel(
                helpTextElement,
                hr(), 
                # options to run model
                # and to save model into a data frame
                # note: probably need two save buttons
                # 1: add to data frame to save
                # 2: save it
                fluidRow(
                    column(6, actionButton),
                    column(6, saveButton)
                ),
                # plotting inputs and button
                fluidRow(
                    column(10, plotYInput),
                    column(6, updatePlotButton)
                ),
                hr(),
                fluidRow(
                    
                    # transmission input 
                    column(5, 
                           tScaleInput),
                    column(7,
                           tModelInput)
                ),
                fluidRow(
                    # year and spinning reserver sliders 
                    #column(5,peakSeasonInput),
                    column(10,yearRangeInput),
                    column(10,spinningReserveInput)
                    
                    ),
                fluidRow(
                    # choose the grid model
                    column(8,allModelInput),
                    column(8,baseModelInput)
                    ),
                fluidRow(
                    # renewables and transmission
                    column(6, windTShareInput),
                    column(6, solarTShareInput),
                    column(10, extraTCapInput)
                    ),
                fluidRow(
                    # demand growth
                    column(8,growthCaseInput),
                    column(6,demandScaleInput),
                    column(6, tPeriodInput)
                    ),
                fluidRow(
                    # wind data
                    column(6,windScaleInput),
                    column(6,solarScaleInput),
                    column(6,offsetWindInput)
                    
                    ),
                fluidRow(
                    column(6, rCCPcoalInput),
                    column(6, rCCPngInput)
                    )

           
                
            ),
            
            mainPanel(
                
                # show the panels 
                # panel 1 is plots
                # 2 might be a data summary
                # and 3 could be raw data or remove it
                # or have 3 be a data book
                tabsetPanel(
                    tabPanel1,
                    tabPanel2,
                    tabPanel3
                )
            )
        )
    )
)