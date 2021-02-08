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
new_us_energy <- subset(us_energy, us_energy$`ENERGY SOURCE` != "Total")

# ----- UI -----
ui <- dashboardPage(
  dashboardHeader(title="CS 424 Project 1"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   selectInput("Year", "Select the year to visualize", years, selected = 2019)
                   ),
  dashboardBody(
    fluidRow(
      column(4, 
        fluidRow(
          box(title = "Total Amount of Energy Sources by Year", status = "primary", solidHeader = TRUE, width = 12,
              plotOutput("totalenergy")) 
        ), # row 1
        fluidRow(
          box(title = "Percent of Total Energy Sources by Year", status = "primary", solidHeader = TRUE, width = 12,
              plotOutput("percentenergy"))
        )), # row 2 / column 1
      
      column(4,
        fluidRow(
          box(title = "Line Chart 1", status = "primary", solidHeader = TRUE, width = 12,
              plotOutput("total_line"))
        ), # row 1
        fluidRow(
          box(title = "Line Chart 2", status = "primary", solidHeader = TRUE, width = 12,
              plotOutput("percent_line"))
        )) # row 2 / column 2
    )
  )# end body
)# end page

# ----- SERVER -----
server <- function(input, output) {
  
  # stores the data given the year in the drop down menu
  ddenergyReactive <- reactive({subset(us_energy, us_energy$YEAR == input$Year & us_energy$`ENERGY SOURCE` != "Total")})
  
  # shows the total amount of each energy source per year
  output$totalenergy <- renderPlot({
    # ddenergy <- ddenergyReactive()
    
    ggplot(new_us_energy, aes(fill=`ENERGY SOURCE`, x=YEAR, y=`GENERATION (MWh)`)) + geom_bar(position='stack', stat = 'identity') +
      scale_y_continuous(labels=scales::comma) +
      labs(x="Year")
  })
  
  # shows the percent of each energy source by year
  output$percentenergy <- renderPlot({
    # ddenergy <- ddenergyReactive()
    
    ggplot(new_us_energy, aes(fill=`ENERGY SOURCE`, x=YEAR, y=`GENERATION (MWh)`)) + geom_bar(position='fill', stat='identity') +
      scale_y_continuous(labels=scales::percent) +
      labs(x="Year", y="Percent of Total Generation")
  })
  
  # line chart showing total energy by year
  output$total_line <- renderPlot({
    # ddenergy <- ddenergyReactive()
    
    ggplot(new_us_energy, aes(x=YEAR, y=`GENERATION (MWh)`, color=`ENERGY SOURCE`)) + geom_line(stat="summary", fun="sum") + geom_point(stat="summary", fun="sum") +
      scale_y_continuous(labels=scales::comma)
  })
  
  # line chart showing percentage of energy by year
  output$percent_line <- renderPlot({
    
    # calculating percentage of each energy source by year
    agg <- aggregate(`GENERATION (MWh)`~YEAR+`ENERGY SOURCE`, new_us_energy, sum)
    agg <- transform(agg, Percent = ave(`GENERATION (MWh)`, YEAR, FUN=prop.table))
    
    ggplot(agg, aes(x=YEAR, y=Percent, color=ENERGY.SOURCE)) + geom_line() + geom_point() +
      scale_y_continuous(labels=scales::percent) +
      labs(x="Year", y="Percent of Total Generation")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
