# Valo Mara - CS 424 Project 1 Spring '21

library(shiny)
library(shinydashboard)
library(ggplot2)
library(usmap)

# ----- DATA CLEANUP -----
us_energy <- read.table(file="annual_generation_state.csv", sep=",", header=TRUE)
# removing empty state identifier 
us_energy <- us_energy[!(us_energy$STATE == "  "),]
# removing commas from number and converting to numeric values
us_energy$GENERATION..Megawatthours. <- as.numeric(gsub(",","", us_energy$GENERATION..Megawatthours.))
us_energy$STATE <- toupper(us_energy$STATE)
# remove any row with negative generation values
us_energy <- us_energy[(us_energy$GENERATION..Megawatthours. >= 0), ]
# since state is the only non-factor, convert to factor
us_energy$STATE <- factor(us_energy$STATE)
# removing the factors that were designated for removal
us_energy <- subset(us_energy, us_energy$ENERGY.SOURCE != "Other" & us_energy$ENERGY.SOURCE != "Other Gases" &
                     us_energy$ENERGY.SOURCE != "Other Biomass" & us_energy$ENERGY.SOURCE != "Pumped Storage")

# renaming some factor levels
levels(us_energy$ENERGY.SOURCE)[2] <- "Geo"
levels(us_energy$ENERGY.SOURCE)[3] <- "Hydro"
levels(us_energy$ENERGY.SOURCE)[4] <- "Gas"
levels(us_energy$ENERGY.SOURCE)[11] <- "Solar"
levels(us_energy$ENERGY.SOURCE)[14] <- "Wood"

# creating subset with Electric producers and Total energy source rows.
electric <- subset(us_energy, TYPE.OF.PRODUCER=="Total Electric Power Industry" & ENERGY.SOURCE=="Total")

# renaming columns for cleaner look
names(us_energy)[names(us_energy)=="TYPE.OF.PRODUCER"] <- "TYPE OF PRODUCER"
names(us_energy)[names(us_energy)=="ENERGY.SOURCE"] <- "ENERGY SOURCE"
names(us_energy)[names(us_energy)=="GENERATION..Megawatthours."] <- "GENERATION (MWh)"

years <- c(1990:2019)

stateNames <- c(state.name, "Washington DC", "Total US")
states <- c(state.abb, "DC", "US-TOTAL")
names(states) <- stateNames

energy_sources_map <- c("Coal", "Geo", "Hydro", "Gas", "Nuclear", "Petroleum", "Solar", "Wind", "Wood")
energy_sources_check <- c(energy_sources_map, "All")

new_us_energy <- subset(us_energy, us_energy$`ENERGY SOURCE` != "Total" & us_energy$`TYPE OF PRODUCER` == "Total Electric Power Industry")

# ----- UI -----
ui <- dashboardPage(
  dashboardHeader(title="CS 424 Project 1"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   sidebarMenu(
                     menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                     menuItem("Compare", tabName = "compare", icon = icon("th")),
                     menuItem("Map", tabName = "map", icon = icon("globe")),
                     menuItem("About", tabName = "about", icon = icon("info")),
                     checkboxGroupInput("sources", "Check sources to show",
                                        energy_sources_check,
                                        selected = "All")
                   )
                ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard", 
              fluidRow(
                column(4, 
                       fluidRow(
                         box(title = "Total Amount of Energy Sources by Year (Stacked Bar Chart)", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 12,
                             plotOutput("totalenergy")) 
                       ), # row 1
                       fluidRow(
                         box(title = "Percent of Total Energy Sources by Year (Stacked Bar Chart)", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 12,
                             plotOutput("percentenergy"))
                       )), # row 2 / column 1
                
                column(4,
                       fluidRow(
                         box(title = "Total Amount of Energy Sources by Year (Line Chart)", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 12,
                             plotOutput("total_line"))
                       ), # row 1 
                       fluidRow(
                         box(title = "Percent of Total Energy Sources by Year (Line Chart)", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 12,
                             plotOutput("percent_line"))
                       )), # row 2 / column 2
                
                column(4,
                       fluidRow(
                         box(title = "Total Amount of Energy Sources by Year (Table)", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 12,
                             dataTableOutput("total_table"))
                       ), # row 1
                       fluidRow(
                         box(title = "Percent of Total Energy Sources by Year (Table)", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 12,
                             dataTableOutput("percent_table"))
                       )) # row 2 / column 2
              )
      ), # end tab item 
      tabItem(tabName = "compare",
              fluidRow(
                column(1,
                       selectInput("state", "Select State", states)
                       ),
                column(2, 
                       fluidRow(
                         box(title = "Total Amount of Energy Sources by Year (Stacked Bar Chart)", status = "primary", solidHeader = TRUE, collapsed = TRUE,collapsible=TRUE, width = 12,
                             plotOutput("c_total_bar")) 
                       ), # row 1
                       fluidRow(
                         box(title = "Percent of Total Energy Sources by Year (Stacked Bar Chart)", status = "primary", solidHeader = TRUE, collapsed = TRUE,collapsible=TRUE, width = 12,
                             plotOutput("c_percent_bar"))
                       )), # row 2 / column 1
                
                column(2,
                       fluidRow(
                         box(title = "Total Amount of Energy Sources by Year (Line Chart)", status = "primary", solidHeader = TRUE, collapsed = TRUE, collapsible=TRUE, width = 12,
                             plotOutput("c_total_line"))
                       ), # row 1 
                       fluidRow(
                         box(title = "Percent of Total Energy Sources by Year (Line Chart)", status = "primary", solidHeader = TRUE, collapsed=TRUE, collapsible=TRUE, width = 12,
                             plotOutput("c_percent_line"))
                       )), # row 2 / column 2
                
                column(2,
                       fluidRow(
                         box(title = "Total Amount of Energy Sources by Year (Table)", status = "primary", solidHeader = TRUE, collapsed = TRUE,collapsible=TRUE, width = 12,
                             dataTableOutput("c_total_table"))
                       ), # row 1
                       fluidRow(
                         box(title = "Percent of Total Energy Sources by Year (Table)", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 12,
                             dataTableOutput("c_percent_table"))
                       ))
              )
      ),
      tabItem(tabName="map",
              fluidRow(
                column(2,
                       selectInput("energy_source1", "Select Energy Source", energy_sources_map),
                       selectInput("year1", "Select Year", years)
                       ),
                column(4,
                       fluidRow(
                         box(title = "Heat Map Total 1", status = "primary", solidHeader = TRUE, width = 12,
                             plotOutput("us_map_total_1"))
                       ),
                       fluidRow(
                         box(title = "Heat Map Percentage 1", status="primary", solidHeader = TRUE, width = 12,
                                plotOutput("us_map_percentage_1"))
                       )),
                column(4,
                       fluidRow(
                         box(title = "Heat Map Total 2", status = "primary", solidHeader = TRUE, width = 12,
                             plotOutput("us_map_total_2"))
                       ),
                       fluidRow(
                         box(title = "Heat Map Percentage 2", status = "primary", solidHeader = TRUE, width = 12,
                             plotOutput("us_map_percentage_2"))
                       )),
                column(2,
                       selectInput("energy_source2", "Select Energy Source", energy_sources_map),
                       selectInput("year2", "Select Year", years))
              )),
      tabItem(tabName = "about",
              h2("About Me"),
              p("This project was made by Valo Mara for CS 424 Spring '21"),
              p("Original data available from https://www.eia.gov/electricity/data/state/")
      ) # end tab item
    ) # end tab items
  )# end body
)# end page

# ----- SERVER -----
server <- function(input, output) {
  
  # stores the data given the year in the drop down menu
  sourceReactive <- reactive({if("All" %in% input$sources){new_us_energy}
    else {new_us_energy[new_us_energy$`ENERGY SOURCE` %in% input$sources, ]}
  })
  
  usmap1Reactive <- reactive({subset(new_us_energy, new_us_energy$`ENERGY SOURCE` == input$energy_source1 & new_us_energy$YEAR == input$year1)})
  usmap2Reactive <- reactive({subset(new_us_energy, new_us_energy$`ENERGY SOURCE` == input$energy_source2 & new_us_energy$YEAR == input$year2)})
  
  # shows the total amount of each energy source per year
  output$totalenergy <- renderPlot({
    sources <- sourceReactive()
    
    ggplot(sources, aes(fill=`ENERGY SOURCE`, x=YEAR, y=`GENERATION (MWh)`)) + geom_bar(position='stack', stat = 'identity') +
      scale_y_continuous(labels=scales::comma) +
      labs(x="Year", y="Total Generation (MWh)")
  })
  
  # shows the percent of each energy source by year
  output$percentenergy <- renderPlot({
    sources <- sourceReactive()
    
    ggplot(sources, aes(fill=`ENERGY SOURCE`, x=YEAR, y=`GENERATION (MWh)`)) + geom_bar(position='fill', stat='identity') +
      scale_y_continuous(labels=scales::percent) +
      labs(x="Year", y="Percent of Total Generation")
  })
  
  # line chart showing total energy by year
  output$total_line <- renderPlot({
    sources <- sourceReactive()
    
    ggplot(sources, aes(x=YEAR, y=`GENERATION (MWh)`, color=`ENERGY SOURCE`)) + geom_line(stat="summary", fun="sum") + geom_point(stat="summary", fun="sum") +
      scale_y_continuous(labels=scales::comma) +
      labs(x="Year", y="Total Generation (MWh)")
  })
  
  # line chart showing percentage of energy by year
  output$percent_line <- renderPlot({
    sources <- sourceReactive()
    # calculating percentage of each energy source by year
    agg <- aggregate(`GENERATION (MWh)`~YEAR+`ENERGY SOURCE`, sources, sum)
    agg <- transform(agg, Percent = ave(`GENERATION (MWh)`, YEAR, FUN=prop.table))
    
    ggplot(agg, aes(x=YEAR, y=Percent, color=ENERGY.SOURCE)) + geom_line() + geom_point() +
      scale_y_continuous(labels=scales::percent) +
      labs(x="Year", y="Percent of Total Generation")
  })
  
  # table showing total energy by year
  output$total_table <- renderDataTable({
    sources <- sourceReactive()
    
    agg <- aggregate(`GENERATION (MWh)`~YEAR+`ENERGY SOURCE`, sources, sum)
    }, options = list(pageLength = 5, autoWidth=TRUE, order=list(1, 'asc')))
  
  # table showing percentage of energy source by year
  output$percent_table <- renderDataTable({
    sources <- sourceReactive()
    
    agg <- aggregate(`GENERATION (MWh)`~YEAR+`ENERGY SOURCE`, sources, sum)
    agg <- transform(agg, Percent = ave(`GENERATION (MWh)`, YEAR, FUN=prop.table))
    agg$GENERATION..MWh. <- NULL
    agg$Percent <- round(agg$Percent, 2)
    agg
  }, options=list(pageLength=5, autoWidth=TRUE, order=list(1, 'asc')))
  
  output$us_map_total_1 <- renderPlot({
    usmap1 <- usmap1Reactive()
    
    agg <- aggregate(`GENERATION (MWh)`~STATE, usmap1, sum)
    agg$abb <- state.abb[match(agg$STATE, state.abb)]
    agg$fips <- fips(agg$STATE)
    plot_usmap(data = agg, values = "GENERATION (MWh)", labels = TRUE) + labs(fill="Total MWh")
  })
  
  output$us_map_percentage_1 <- renderPlot({
    usmap1 <- usmap1Reactive()
    usmap1 <- subset(usmap1, usmap1$STATE != "US-TOTAL" & usmap1$STATE != "DC")
    
    agg <- aggregate(`GENERATION (MWh)`~STATE, usmap1, sum)
    agg$percentage <- agg$`GENERATION (MWh)` / sum(agg$`GENERATION (MWh)`)
    agg$abb <- state.abb[match(agg$STATE, state.abb)]
    agg$fips <- fips(agg$STATE)
    
    View(agg)
    plot_usmap(data = agg, values = "percentage", labels = TRUE) + labs(fill="Percentage of Total MWh")
  })
  
  output$us_map_total_2 <- renderPlot({
    usmap2 <- usmap2Reactive()
    
    agg <- aggregate(`GENERATION (MWh)`~STATE, usmap2, sum)
    agg$abb <- state.abb[match(agg$STATE, state.abb)]
    agg$fips <- fips(agg$STATE)
    plot_usmap(data = agg, values = "GENERATION (MWh)", labels = TRUE) + labs(fill="Total MWh")
  })
  
  output$us_map_percentage_2 <- renderPlot({
    usmap2 <- usmap2Reactive()
    usmap2 <- subset(usmap2, usmap2$STATE != "US-TOTAL" & usmap2$STATE != "DC")
    
    agg <- aggregate(`GENERATION (MWh)`~STATE, usmap2, sum)
    agg$percentage <- agg$`GENERATION (MWh)` / sum(agg$`GENERATION (MWh)`)
    agg$abb <- state.abb[match(agg$STATE, state.abb)]
    agg$fips <- fips(agg$STATE)
    
    plot_usmap(data = agg, values = "percentage", labels = TRUE) + labs(fill="Percentage of Total MWh")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
