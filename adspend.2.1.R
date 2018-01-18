
## Global Adex by medium
version <- 2.1

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

packages <- c("shiny", "plotly", "XLConnect", "shinythemes", "ggplot2", "reshape2", "abind")
reqPackages(packages)


## Paths
path <- paste0(getwd(), "/data.xlsx")


## Read Data

df <- NULL
file <- loadWorkbook(path)
setMissingValue(file, value = "")

for (sht in 1:length(getSheets(file))) {
  
  data <- readWorksheet(file, sht, header = TRUE, startRow = 6, endRow = 16, dateTimeFormat = meta$date.format, forceConversion = TRUE)
  
  ## Formatting
  
  names(data)[1] <- "Year"
  ## Coerce to date
  data$Year <- as.POSIXct(as.character(data$Year), format = meta$date.format)
  ## Billions, to one d.p.
  data[, 2:10] <- apply(data[, 2:10], 1:2, function(x) {
    round(x / 1000, 1)}
    )
  
  ## Sheet into 3D dataframe
  df <- abind(df, data, along = 3)
}
dimnames(df)[[3]] <- getSheets(file)


# Define UI
ui <- fluidPage(
   
   # Application title
   titlePanel(paste(meta$title, version)),
   
   # Sidebar with Date Range
   sidebarLayout(
      sidebarPanel(
        dateRangeInput("years",
                       "Select Date Range",
                       min = df[1,"Year", "Global"],
                       max = df[dim(df)[1],"Year", "Global"],
                       start = df[1,"Year", "Global"],
                       end = df[dim(df)[1],"Year", "Global"],
                       format = "yyyy"),
        checkboxGroupInput("medium",
                           "Select Mediums",
                           choices = dimnames(df)[[2]][dimnames(df)[[2]] != "Year"],
                           selected = dimnames(df)[[2]][dimnames(df)[[2]] != "Year"])
      ),
      uiOutput("tabs")
   )
)

# Define server logic
server <- function(input, output) {
  
  ## Generate Tabset
  output$tabs <- renderUI({
    
    tabset <- list()
    
    for (i in 1:dim(df)[3]){
      
      ## Generate tab
      tab <- tabPanel(dimnames(df)[[3]][i],
                      
                      ## Generate plot
                      renderPlotly({
                        
                        ## Select data
                        n.tab <- input$tabs
                        x.min <- match(as.character(input$years[1]), df[,"Year", n.tab])
                        x.max <- match(as.character(input$years[2]), df[,"Year", n.tab])
                        x.range <- df[x.min:x.max, "Year" , n.tab]
                        data <- data.frame(df[, , n.tab])
                        data[, 2:10] <- apply(data[, 2:10], 2, as.numeric)
                        data.m <- melt(data[data$Year == x.range, c("Year", input$medium)], id.vars = "Year")
                        
                        ## Plot
                        ggplotly(
                          ggplot(data.m, aes(Year, value, fill = variable)) +
                            geom_bar(stat = "identity") +
                            labs(x = meta$x.axis, y = meta$y.axis, title = meta$title)
                        )
                        
                      })
      )
      ## Append tab to tabset
      tabset <- append(tabset, list(tab))
    }
    do.call(tabsetPanel, args = c(tabset, id = "tabs"))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

