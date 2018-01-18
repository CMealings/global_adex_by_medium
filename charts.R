## Charts

## Line, date range

x.min <- match(as.POSIXct(input$years[1], tz = "GMT"), data$Year)
x.max <- match(as.POSIXct(input$years[2], tz = "GMT"), data$Year)
x.range <- data[x.min:x.max, 1]
data.m <- melt(data[data$Year == x.range, c("Year", input$medium)], id.vars = "Year")

ggplotly(
  ggplot(data.m, aes(Year, value, colour = variable)) +
    geom_line() +
    geom_text(aes(label = value), nudge_y = 0.5) +
    labs(x = meta$x.axis, y = meta$y.axis, title = meta$title)
)