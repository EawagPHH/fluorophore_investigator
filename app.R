# Author: Rachel McLeod
# rachel.mcleod@eawag.ch
library(shiny)
library(tidyverse)
library(readxl)
library(plotly)
library(htmltools)
library(shinyjs)

# Create custom plot theme
theme_cowwid <- function(base_size = 2) {
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- RColorBrewer::brewer.pal("Greys", n = 9)
  color.background = palette[1]
  color.grid.major = palette[3]
  color.axis.text = palette[7]
  color.axis.title = palette[8]
  color.title = palette[9]
  color.subtitle = palette[9]
  
  #Generate fonts
  
  # Begin construction of chart
  theme_bw() +
    
    # Set the colour of the entire chart region
    theme(panel.background = element_rect(fill = color.background, color = color.background)) +
    theme(plot.background = element_rect(fill = color.background, color = color.background)) +
    theme(strip.background = element_blank()) +
    theme(panel.border = element_rect(color = color.axis.text)) +
    #theme(legend.background = element_rect(fill = color.background)) +
    
    # Format the grid
    theme(panel.grid.major = element_line(color = color.grid.major, linewidth = .25)) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.ticks = element_blank()) +
    
    # Format all text
    theme(plot.title = element_text(color = color.title, size = base_size * 10, hjust = 0, vjust = 1.5, face = "bold")) +
    theme(plot.subtitle = element_text(color = color.subtitle, size = base_size * 7, hjust = 0, vjust = 2, face = "italic")) +
    theme(plot.caption = element_text(color = color.title, size = base_size * 8, hjust = 0, vjust = -0.75, face = "italic")) +
    theme(legend.title = element_text(size = base_size * 7, color = color.axis.text, face = "bold")) +
    theme(legend.text = element_text(size = base_size * 6, color = color.axis.title)) +
    theme(axis.text.x = element_text(size = base_size * 6, color = color.axis.text, angle = 90)) +
    theme(axis.text.y = element_text(size = base_size * 6, color  = color.axis.text)) +
    theme(axis.title.x = element_text(size = base_size * 7, color = color.axis.title, hjust = 0.5, vjust = -0.75, face = "bold")) +
    theme(axis.title.y = element_text(size = base_size * 7, color = color.axis.title, hjust = 0.5, vjust = 3, face = "bold")) +
    theme(strip.text = element_text(size = base_size * 6, color = color.axis.text, face = "bold")) +
    
    # Plot margins
    theme(plot.margin = grid::unit(c(10, 10, 10, 10), "pt"))
}

rainbow <- c("#A81705", "#E53935", "#FFB74D", "#FFEE58", "#388E5A", "#42A5F5", "#0F4C81", "#8756D5", "#BBBBBB")

# Define filter parameters
filtheaders <- c("filtmin", "filtmax", "filtname")

# Define Stilla machine filters
EM3filtmin <- c(495, 560, 655)
EM3filtmax <- c(515, 610, 720)
EX3filtmin <- c(415, 530, 630)
EX3filtmax <- c(480, 550, 640)

EM6filtmin <- c(503, 527, 566, 598, 650, 704)
EM6filtmax <- c(537, 551, 597, 642, 684, 755)
EX6filtmin <- c(445, 504, 540, 562, 623, 675)
EX6filtmax <- c(490, 526, 560, 588, 643, 698)

filt3name <- c("Blue", "Green", "Red")
filt6name <- c("Blue", "Teal", "Green", "Yellow", "Red", "Infra-Red")

EM3filters <- data.frame(EM3filtmin, EM3filtmax, filt3name)
colnames(EM3filters) <- filtheaders
EX3filters <- data.frame(EX3filtmin, EX3filtmax, filt3name)
colnames(EX3filters) <- filtheaders

EM6filters <- data.frame(EM6filtmin, EM6filtmax, filt6name)
colnames(EM6filters) <- filtheaders
EX6filters <- data.frame(EX6filtmin, EX6filtmax, filt6name)
colnames(EX6filters) <- filtheaders

# Define Quiagen machine filters
EMQ2filtmin <- c(518, 550)
EMQ2filtmax <- c(548, 564)
EXQ2filtmin <- c(463, 514)
EXQ2filtmax <- c(503, 535)

EMQ5filtmin <- c(518, 550, 580, 611, 654)
EMQ5filtmax <- c(548, 564, 606, 653, 692)
EXQ5filtmin <- c(463, 514, 543, 570, 590)
EXQ5filtmax <- c(503, 535, 565, 596, 640)

filtQ2name <- c("Green", "Yellow")
filtQ5name <- c("Green", "Yellow", "Orange", "Red", "Crimson")

EMQ2filters <- data.frame(EMQ2filtmin, EMQ2filtmax, filtQ2name)
colnames(EMQ2filters) <- filtheaders
EXQ2filters <- data.frame(EXQ2filtmin, EXQ2filtmax, filtQ2name)
colnames(EXQ2filters) <- filtheaders

EMQ5filters <- data.frame(EMQ5filtmin, EMQ5filtmax, filtQ5name)
colnames(EMQ5filters) <- filtheaders
EXQ5filters <- data.frame(EXQ5filtmin, EXQ5filtmax, filtQ5name)
colnames(EXQ5filters) <- filtheaders

# Combine all filter information to create data plot
Filters <- bind_rows(EX3filters, EM3filters, 
                     EX6filters, EM6filters,
                     EXQ2filters, EMQ2filters,
                     EXQ5filters, EMQ5filters,
                     .id = "id") %>%
  mutate(id = as.numeric(id)) %>%
  mutate(Machine = factor(ifelse((id == 1 | id == 2), "Prism3", 
                        ifelse((id == 3 | id == 4), "Prism6", 
                               ifelse((id == 5 | id == 6), "QIAcuity One 2-plex", "QIAcuity One 5-plex")))),
         Type = factor(ifelse((id %% 2) == 1, "Excitation", "Emission")),
         filtname = factor(filtname,
                           levels = c("Blue", "Teal", "Green", "Yellow", "Orange", "Red", "Infra-Red", "Crimson"),
                           ordered = T)) %>%
  select(-id)

Spectra <- read_csv("data/AAT_Fluo_Data.csv", show_col_types = FALSE) %>%
  mutate(Fluorophore = factor(Fluorophore,
                              levels = sort(unique(Fluorophore)),
                              ordered = T),
         Type = factor(Type))

Fluos <- Spectra %>%
  group_by(Fluorophore, Type) %>%
  slice(which.max(RelativeIntensity)) %>%
  filter(Type == "Emission") %>%
  arrange(Wavelength)

fluo_palette <- grDevices::rainbow(length(unique(Fluos$Fluorophore)), start = 0, end = .65, rev = TRUE)
fluo_palette <- setNames(fluo_palette, unique(Fluos$Fluorophore))

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Fluorophore Spectra Investigator"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("Machine",
                  "Select dPCR Machine:",
                  choices = c("Prism3", "Prism6", 
                              "QIAcuity One 2-plex", "QIAcuity One 5-plex", 
                              "Custom")),
      conditionalPanel(condition = "input.Machine == 'Custom'",
                       numericInput("filtnum", 
                                    "Enter the number of filters the machine contains", 
                                    value = 3, 
                                    min = 1, 
                                    step = 1),
                       uiOutput("customFilt")),
      selectInput("Type",
                  "Select Spectrum Type:",
                  choices = c("Emission", "Excitation")),
      checkboxGroupInput("Fluorophores",
                         "Select fluorophores to display:",
                         choices = sort(unique(Spectra$Fluorophore)))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        plotOutput("spectraPlot")
      ),
      fluidRow(
        downloadButton('downloadPlot', 'Download Plot')
      ),
      fluidRow(
        h4("About this tool"),
        HTML(readLines("instructions.txt")) # information how to use
      )
    )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # generate conditional custom filters input
  output$customFilt <- renderUI({
    
    num <- as.integer(input$filtnum)
    
    lapply(1:num, function(i) {
      splitLayout(
        numericInput(paste0("input_", i, "_min"), 
                     label = paste0("Filter ", i, " min wavelength"), 
                     value = 450),
        numericInput(paste0("input_", i, "_max"), 
                     label = paste0("Filter ", i, " max wavelength"), 
                     value = 500)
      )
      
    })
    
  })
  
  # generate data for custom filters
  customData <- reactive({
    
    num <- as.integer(input$filtnum)
    
    customData <- data.frame("filtmin" = integer(), 
                             "filtmax" = integer(), 
                             "filtname" = character())
    
    for (i in 1:num) {
      customData[nrow(customData)+1,] <- c(input[[paste0("input_", i, "_min")]],
                                           input[[paste0("input_", i, "_max")]],
                                           paste("filter", i))
    }
    
    customData
    
  })
  
  # generate plot
  fluoPlot <- function(){
    
    if (input$Machine == "Prism3") {
      colours = rainbow[c(7, 5, 1)]
    } else if (input$Machine == "Prism6") {
      colours = rainbow[c(7, 6, 5, 3, 2, 1)]
    } else if (input$Machine == "QIAcuity One 2-plex") {
      colours = rainbow[c(5, 4)]
    } else if (input$Machine == "QIAcuity One 5-plex") {
      colours = rainbow[c(5, 4, 3, 2, 1)]
    } else {
      colours = grDevices::rainbow(input$filtnum, start = 0, end = .65, rev = TRUE)
    }
    
    if (input$Machine != "Custom") {
      fluoPlot <- ggplot() +
        geom_rect(data = filter(Filters, Machine == input$Machine, Type == input$Type), 
                  aes(xmin = filtmin, xmax = filtmax, 
                      ymin = -Inf, ymax = Inf, 
                      fill = factor(filtname)), alpha = 0.2) +
        geom_line(data = filter(Spectra, Fluorophore %in% input$Fluorophores, Type == input$Type), 
                  aes(x = Wavelength, 
                      y = RelativeIntensity, 
                      color = Fluorophore),
                  linewidth = 1) +
        scale_fill_manual(values = colours) +
        scale_color_manual(values = fluo_palette) +
        guides(fill = guide_legend(order = 1),
               color = guide_legend(order = 2)) +
        labs(x = "Wavelength [nm]", 
             y = "Relative Intensity",
             fill = "Filter",
             colour = "Fluorophore") +
        xlim(400, 800) +
        ylim(0, 100) +
        theme_cowwid()
    } else {
      fluoPlot <- ggplot() +
        geom_rect(data = customData(),
                  aes(xmin = as.numeric(filtmin), xmax = as.numeric(filtmax), 
                      ymin = -Inf, ymax = Inf, 
                      fill = factor(filtname)), alpha = 0.2) +
        geom_line(data = filter(Spectra, Fluorophore %in% input$Fluorophores, Type == input$Type), 
                  aes(x = Wavelength, 
                      y = RelativeIntensity, 
                      color = Fluorophore),
                  linewidth = 1) +
        scale_fill_manual(values = colours) +
        scale_color_manual(values = fluo_palette) +
        guides(fill = guide_legend(order = 1),
               color = guide_legend(order = 2)) +
        labs(x = "Wavelength [nm]", 
             y = "Relative Intensity",
             fill = "Filter",
             colour = "Fluorophore") +
        xlim(400, 800) +
        ylim(0, 100) +
        theme_cowwid()
    }
    
    fluoPlot
    
  }
  
  # plot output
  output$spectraPlot <- renderPlot(fluoPlot())
  
  # download plot
  output$downloadPlot <- downloadHandler(
    filename = "FluoSpectraPlot.png",
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 12, height = 8, res = 900, units = "in")
      ggsave(file, plot = fluoPlot(), device = device)
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
