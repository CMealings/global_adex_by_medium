
## Global Adex by medium
version <- 1.1

## User variables
meta <- list(title = "Global Advertising Spend by Medium",
             type = "time series, line",
             format = "US$ millions, current prices",
             note = "Net of discounts. Includes agency commission and excludes production costs. WARC uses variable exchange rates for each period.",
             source = "WARC",
             x.axis = "Year",
             y.axis = "Ad Spend, US$ millons",
             legend = "Medium",
             date.format = "%Y")


## Dependencies
scripts <- grep(".R", list.files(paste0(getwd(), "/scripts/")), value = TRUE)
if (length(scripts) > 0) {
  for (func in scripts)
    source(paste0(getwd(), "/scripts/", func))
}

packages <- c("shiny", "plotly", "XLConnect", "shinythemes", "ggplot2", "reshape2")
reqPackages(packages)


## Paths
path <- paste0(getwd(), "/data.xlsx")


## Read Data
file <- loadWorkbook(path)
data <- readWorksheet(file, 1, header = TRUE, startRow = 6, endRow = 16, dateTimeFormat = meta$date.format, forceConversion = TRUE)
names(data)[1] <- "Year"

## Formatting
data$Year <- as.POSIXct(as.character(data$Year), format = meta$date.format)
data[, 2:10] <- apply(data[, 2:10], 1:2, function(x) round(x / 1000, 1))


# Define UI
ui <- fluidPage(
   
   # Application title
   titlePanel(meta$title),
   
   # Sidebar with Date Range
   sidebarLayout(
      sidebarPanel(
        dateRangeInput("years",
                       "Select Date Range",
                       min = data$Year[1],
                       max = data$Year[length(data$Year)],
                       start = data$Year[1],
                       end = data$Year[length(data$Year)],
                       format = "yyyy"),
        checkboxGroupInput("medium",
                           "Select Mediums",
                           choices = names(data)[names(data) != "Year"],
                           selected = c("Total", "TV"))
      ),
      tabsetPanel(
        tabPanel("Global",
          # Main Panel with chart
          mainPanel(
            plotlyOutput("plot", width = "75%")
          )
        )
      )
   )
)

# Define server logic
server <- function(input, output) {
   
   output$plot <- renderPlotly({
     
     x.min <- match(as.POSIXct(input$years[1], tz = "GMT"), data$Year)
     x.max <- match(as.POSIXct(input$years[2], tz = "GMT"), data$Year)
     x.range <- data[x.min:x.max, 1]
     data.m <- melt(data[data$Year == x.range, c("Year", input$medium)], id.vars = "Year")
     
     ggplotly(
       ggplot(data.m, aes(Year, value, fill = variable)) +
         geom_bar(stat = "identity") +
         geom_text(aes(label = value, nudge_y = 0.5)) +
         labs(x = meta$x.axis, y = meta$y.axis, title = meta$title)
     )
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

