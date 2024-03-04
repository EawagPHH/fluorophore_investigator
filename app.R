# Author: Rachel McLeod
# rachel.mcleod@eawag.ch

library(shiny)
library(tidyverse)
library(readxl)
library(plotly)

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
colnames(EM3filters) <- c("filtmin", "filtmax", "filtname")
EX3filters <- data.frame(EX3filtmin, EX3filtmax, filt3name)
colnames(EX3filters) <- c("filtmin", "filtmax", "filtname")

EM6filters <- data.frame(EM6filtmin, EM6filtmax, filt6name)
colnames(EM6filters) <- c("filtmin", "filtmax", "filtname")
EX6filters <- data.frame(EX6filtmin, EX6filtmax, filt6name)
colnames(EX6filters) <- c("filtmin", "filtmax", "filtname")

Filters <- bind_rows(EX3filters, EM3filters, EX6filters, EM6filters, .id = "id") %>%
  mutate(Prism = ifelse((id == 1 | id == 2), "Prism3", "Prism6"),
         Type = ifelse(id == 1 | id == 3, "Excitation", "Emission")) %>%
  select(-id)

Spectra <- read_csv("data/AAT_Fluo_Data.csv", show_col_types = FALSE) 

Fluos <- Spectra %>%
  group_by(Fluorophore, Type) %>%
  slice(which.max(RelativeIntensity)) %>%
  filter(Type == "Emission") %>%
  arrange(Wavelength)

fluo_palette <- grDevices::rainbow(length(unique(Fluos$Fluorophore)), start = 0, end = .7, rev = TRUE)
fluo_palette <- setNames(fluo_palette, unique(Fluos$Fluorophore))

rectangle <- function(min = 0, max = 1, color = "#660000") {
  list(
    type = "rect",
    fillcolor = color,
    x0 = min,
    x1 = max,
    xref = "paper",
    y0 = -Inf,
    y1 = Inf
  )
}

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Fluorophore Spectra Investigator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("Prism",
                        "Select Prism machine:",
                        choices = c("Prism3", "Prism6")),
            selectInput("Type",
                        "Select Spectrum Type:",
                        choices = c("Emission", "Excitation")),
            checkboxGroupInput("Fluorophores",
                               "Select fluorophores to display:",
                               choices = unique(Spectra$Fluorophore))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("FluoPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$FluoPlot <- renderPlot({
    
    if (input$Prism == "Prism3") {
      colours = rainbow[c(7, 5, 1)]
    } else {
      colours = rainbow[c(7, 5, 1, 2, 6, 3)]
    }
    
    ggplot(filter(Spectra, Fluorophore %in% input$Fluorophores, Type == input$Type), aes(x = Wavelength, y = RelativeIntensity, color = Fluorophore)) +
    geom_line(size = 1) +
    geom_rect(data = filter(Filters, Prism == input$Prism, Type == input$Type), inherit.aes = FALSE,
              aes(xmin = filtmin, xmax = filtmax, 
                  ymin = -Inf, ymax = Inf, 
                  fill = factor(filtname)), alpha = 0.2) +
    scale_fill_manual(values = colours) +
    scale_color_manual(values = fluo_palette) +
    labs(x = "Wavelength [nm]", 
         y = "Relative Intensity",
         fill = "Filter",
         colour = "Fluorophore") +
    xlim(400, 800) +
    theme_cowwid()

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
