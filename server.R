library(dplyr)
library(tidyr)

## data is gathered from github
baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

## manually set a font and font size for the plots
f1 = list(family="Courier New, monospace", size=12, 
          color="rgb(30,30,30)") ## font

minutesSinceLastUpdate = function(fileName) {
  ## this function returns the minutes since the last update of a file
  (as.numeric(as.POSIXlt(Sys.time())) -  
     as.numeric(file.info(fileName)$ctime)) / 60
}

loadData = function(fileName, columnName) {
  ## this function returns the data (number of confirmed cases by day and country)
  if(!file.exists(fileName) || 
     minutesSinceLastUpdate(fileName) > 10) {
    data = read.csv(file.path(baseURL, fileName), 
                    check.names=FALSE, stringsAsFactors=FALSE) %>%
      select(-Lat, -Long) %>% 
      pivot_longer(-(1:2), names_to = "date", values_to = columnName) %>%
      mutate(date=as.Date(date, format = "%m/%d/%y"),
        `Province/State` = if_else(`Province/State` == "", "<all>", `Province/State`)
      )
    save(data, file = fileName)  
  } else {
    load(file = fileName)
  }
  return(data)
}

allData = 
  loadData(
    "time_series_covid19_confirmed_global.csv", "CumConfirmed") %>%
  inner_join(loadData(
    "time_series_covid19_deaths_global.csv", "CumDeaths")) %>%
  inner_join(loadData(
    "time_series_covid19_recovered_global.csv","CumRecovered"))

server = function(input, output, session) {
  
  data = reactive({
    d = allData %>%
      filter(`Country/Region` == input$country)
    if(input$state != "<all>") {
      d = d %>% 
        filter(`Province/State` == input$state)
    } else {
      d = d %>% 
        group_by(date) %>% 
        summarise_if(is.numeric, sum, na.rm = TRUE)
    }
    d %>%
      mutate(
        dateStr = format(date, format = "%b %d, %Y"),
        NewConfirmed = CumConfirmed - lag(CumConfirmed, default = 0),
        NewRecovered = CumRecovered - lag(CumRecovered, default = 0),
        NewDeaths = CumDeaths - lag(CumDeaths, default = 0)
      )
  })
  
  observeEvent(input$country, {
    states = allData %>%
      filter(`Country/Region` == input$country) %>% 
      pull(`Province/State`)
    states = c("<all>", sort(unique(states)))
    updateSelectInput(session, "state", choices = states, 
                      selected = states[1])
  })
  
  countries = sort(unique(allData$`Country/Region`))
  
  ## update the input for country selected, default is Italy
  updateSelectInput(session, "country", choices = countries, 
                    selected = "Italy")
  
  renderBarPlot = function(varPrefix, legendPrefix, yaxisTitle) {
    ## plotly function, the first half defines the basics of the bar chart (style, axes, legend)
    ## The second part loops through the selected metrics (e.g. Recovered, …) and adds the bars
    renderPlotly({
      data = data()
      plt = data %>% 
        plot_ly() %>%
        config(displayModeBar = FALSE) %>%
        layout(
          barmode = 'group', 
          xaxis = list(
            title = "", tickangle = -90, type='category', 
            ticktext = as.list(data$dateStr), 
            tickvals = as.list(data$date)), 
          yaxis = list(title=yaxisTitle),
          legend = list(x=0.1, y=0.9,bgcolor='rgba(240,240,240,0.5)'),
          font = f1
        )
      for(metric in input$metrics) 
        plt = plt %>%
        add_trace(
          x = ~date, y = data[[paste0(varPrefix, metric)]],type = 'bar', 
          name = paste(legendPrefix, metric, "Cases"),
          marker = list(
            color = switch(metric, 
                         Deaths = 'rgb(200,30,30)', 
                         Recovered = 'rgb(30,200,30)', 
                         Confirmed = 'rgb(100,140,240)'),
            line = list(color='rgb(8,48,107)', width=1.0)
          )
        )
      plt
    })
  }
  
  ## plot the data on renderBarPlot defined above
  output$dailyMetrics = renderBarPlot(
    "New", legendPrefix = "New", yaxisTitle = "New Cases per Day")
  output$cumulatedMetrics = renderBarPlot(
    "Cum", legendPrefix = "Cumulated", yaxisTitle = "Cumulated Cases")
}
