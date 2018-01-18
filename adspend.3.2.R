
## Global Adex by medium
version <- 3.2

## User variables
meta <- list(title = "Advertising Expenditure by Medium, Region",
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

## Load WARC styles
warcStyle()

packages <- c("shiny", "plotly", "XLConnect", "shinythemes", "ggplot2", "reshape2", "abind", "forcats", "showtext")
reqPackages(packages)


## Paths
path <- paste0(getwd(), "/data.xlsx")


## Read Data

df <- NULL
file <- loadWorkbook(path)
setMissingValue(file, value = "")

for (sht in 1:length(getSheets(file))) {
  
  data <- readWorksheet(file, sht, header = TRUE, startRow = 6, endRow = 16, dateTimeFormat = meta$date.format, forceConversion = TRUE, colTypes = rep("numeric", 10))
  
  ## Formatting
  
  names(data)[1] <- "Year"
  ## Billions, to one d.p.
  data[, 2:10] <- apply(data[, 2:10], 1:2, function(x) {
    round(x / 1000, 1)}
    )
  
  ## Sheet into 3D dataframe
  df <- abind(df, data, along = 3)
}
dimnames(df)[[3]] <- getSheets(file)


# Define UI
ui <- fixedPage(includeCSS("styles.css"),
                
   # Application title
   headerPanel(paste(meta$title, version)),
   
   # Sidebar with Date Range
   sidebarPanel(width = 2,
                sliderInput("years",
                            "Select Date Range",
                            min = df[1,"Year", "Global"],
                            max = df[dim(df)[1],"Year", "Global"],
                            value = c(df[1,"Year", "Global"], df[dim(df)[1],"Year", "Global"]),
                            sep = ""),
                checkboxGroupInput("medium",
                                   "Select Mediums",
                                   choices = dimnames(df)[[2]][3:10],
                                   selected = dimnames(df)[[2]][3:10])
   ),
   # Plot Panel
   mainPanel(uiOutput("tabs"), width = 10)
)

# Define server logic
server <- function(input, output, session) {
  
  # close the R session when Chrome closes
  session$onSessionEnded(function() { 
    stopApp()
    q("no") 
  })
  
  ## Generate Tabset
  output$tabs <- renderUI({
    
    tabset <- list()
    
    for (i in 1:dim(df)[3]){
      
      ## Generate tab
      tab <- tabPanel(dimnames(df)[[3]][i],
                      
                      ## Generate plot
                      renderPlotly({
                        
                        ## Select data
                        n.tab <- req(input$tabs)
                        x.min <- match(as.character(input$years[1]), df[,"Year", n.tab])
                        x.max <- match(as.character(input$years[2]), df[,"Year", n.tab])
                        x.range <- df[x.min:x.max, "Year" , n.tab]
                        data <- data.frame(df[, , n.tab])
                        data[, 2:10] <- apply(data[, 2:10], 2, as.numeric)
                        data.m <- melt(data[match(x.range, data$Year), c("Year", input$medium)], id.vars = "Year")
                        
                        
                        ## Plot
                        ggplotly(
                          ggplot(data.m, aes(Year, value, fill = forcats::fct_rev(variable))) +
                            geom_bar(stat = "identity", width = .7) +
                            labs(x = meta$x.axis, y = meta$y.axis) +
                            ## WARC Styling
                            scale_fill_manual(values = rev(unlist(styles$col, use.names = FALSE)),
                                              name = meta$legend) +
                            theme(text=element_text(family = "Aktiv Grotesk Medium",
                                                    colour = styles$col$main$col1))
                        )
                        
                      }),
                      renderUI({
                        req(input$tabs)
                        includeHTML(paste0(getwd(), "/copy/", input$tabs,".html"))
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

