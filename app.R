# Valo Mara - CS 424 Project 1 Spring '21

library(shiny)
library(shinydashboard)
library(ggplot2)

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
# removing 
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


names(electric)[names(electric)=="TYPE.OF.PRODUCER"] <- "TYPE OF PRODUCER"
names(electric)[names(electric)=="ENERGY.SOURCE"] <- "ENERGY SOURCE"
names(electric)[names(electric)=="GENERATION..Megawatthours."] <- "GENERATION (MWh)"

ui <- fluidPage("Hello world")

server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)
