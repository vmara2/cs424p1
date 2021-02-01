# Valo Mara - CS 424 Project 1 Spring '21

library(shiny)
library(shinydashboard)
library(ggplot2)
library(usmap)

# ----- DATA CLEANUP -----
usenergy <- read.table(file="annual_generation_state.csv", sep=",", header=TRUE)
# removing empty state identifier 
usenergy <- usenergy[!(usenergy$STATE == "  "),]
# removing commas from number and converting to numeric values
usenergy$GENERATION..Megawatthours. <- as.numeric(gsub(",","", usenergy$GENERATION..Megawatthours.))
usenergy$STATE <- toupper(usenergy$STATE)
# remove any row with negative generation values
usenergy <- usenergy[(usenergy$GENERATION..Megawatthours. >= 0), ]
# since state is the only non-factor, convert to factor
usenergy$STATE <- factor(usenergy$STATE)
# removing the factors that were designated for removal
usenergy <- subset(usenergy, usenergy$ENERGY.SOURCE != "Other" & usenergy$ENERGY.SOURCE != "Other Gases" &
                     usenergy$ENERGY.SOURCE != "Other Biomass" & usenergy$ENERGY.SOURCE != "Pumped Storage")

# renaming some factor levels
levels(usenergy$ENERGY.SOURCE)[2] <- "Geo"
levels(usenergy$ENERGY.SOURCE)[3] <- "Hydro"
levels(usenergy$ENERGY.SOURCE)[4] <- "Gas"
levels(usenergy$ENERGY.SOURCE)[11] <- "Solar"
levels(usenergy$ENERGY.SOURCE)[14] <- "Wood"

# creating subset with Electric producers and Total energy source rows.
electric <- subset(usenergy, TYPE.OF.PRODUCER=="Total Electric Power Industry" & ENERGY.SOURCE=="Total")

# renaming columns for cleaner look
names(usenergy)[names(usenergy)=="TYPE.OF.PRODUCER"] <- "TYPE OF PRODUCER"
names(usenergy)[names(usenergy)=="ENERGY.SOURCE"] <- "ENERGY SOURCE"
names(usenergy)[names(usenergy)=="GENERATION..Megawatthours."] <- "GENERATION (MWh)"

years <- c(1990:2019)

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
        ))
    )
  )# end body
)# end page

# ----- SERVER -----
server <- function(input, output) {
  
  # stores the data given the year in the drop down menu
  ddenergyReactive <- reactive({subset(usenergy, usenergy$YEAR == input$Year & usenergy$`ENERGY SOURCE` != "Total")})
  
  output$totalenergy <- renderPlot({
    ddenergy <- ddenergyReactive()
    
    ggplot(ddenergy, aes(fill=`ENERGY SOURCE`, x=as.character(YEAR), y=`GENERATION (MWh)`)) + geom_bar(position='stack', stat = 'identity') +
      scale_y_continuous(labels=scales::comma)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
