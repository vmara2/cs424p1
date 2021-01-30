# Valo Mara - CS 424 Project 1 Spring '21

library(shiny)
library(shinydashboard)
library(ggplot2)

# ----- DATA CLEANUP -----
usenergy <- read.table(file="annual_generation_state.csv", sep=",", header=TRUE)
usenergy <- usenergy[!(usenergy$STATE == "  "),]

# creating subset with Electric producers and Total energy source rows.
electric <- subset(usenergy, TYPE.OF.PRODUCER=="Total Electric Power Industry" & ENERGY.SOURCE=="Total")

#turning the Generation column into numeric values
electric$GENERATION..Megawatthours. <- as.numeric(gsub(",","", electric$GENERATION..Megawatthours.))

# some labels are lowercase, so we make them all uppercase
electric$STATE <- toupper(electric$STATE)

names(electric)[names(electric)=="TYPE.OF.PRODUCER"] <- "TYPE OF PRODUCER"
names(electric)[names(electric)=="ENERGY.SOURCE"] <- "ENERGY SOURCE"
names(electric)[names(electric)=="GENERATION..Megawatthours."] <- "GENERATION (MWh)"

ui <- fluidPage("Hello world")

server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)
