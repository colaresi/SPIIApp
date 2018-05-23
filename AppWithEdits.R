library(tidyverse)

library(shiny)

myData <- readRDS("dataforAnalysis.rds", refhook = NULL)

names <- read.csv("COWstatelist.csv")

dataWithNames <- myData %>% left_join(names)

p <- ggplot(dataWithNames) + geom_step(aes(x = year, y = value, group = StateNme, color = StateNme))
colorData <- ggplot_build(p)$data
colorDf <- data.frame(matrix(unlist(colorData), nrow = 536600, byrow=T))
colors <- colorDf %>% slice(1:67075)
names(colors)[1] <- "colors"

dataWithNamesAndColors <- bind_cols(dataWithNames, colors)

lopData <- filter(dataWithNamesAndColors, variable == "LOP")
polyarchyData <- filter(dataWithNamesAndColors, variable == "v2x_polyarchy")

countryNamesWithColor <- dataWithNamesAndColors %>% select(StateNme,colors) %>% distinct() %>% arrange()
countryNames <- countryNamesWithColor %>% pull(StateNme)

ui <- navbarPage("SPII Data",

  tabPanel("LOP",

    titlePanel("Legislative Oversight Plots"),

    sidebarLayout(
      sidebarPanel(

        selectInput("country",
                    "Choose a Country",
                    choices=countryNames,
                    selected="Argentina"),
        sliderInput("years",
                    "Choose a Date Range",
                    min = lopData %>% summarize(minYear = min(year)),
                    max= lopData %>% summarize(maxYear = max(year)),
                    value=c(2000,2014))
      ),

      mainPanel(
        plotOutput("allCountries"),
        plotOutput("chosenCountry")
      )
    )
  ),
  navbarMenu("Other Variables",
      tabPanel("Polyarchy",
        titlePanel("Polyarchy Plots"),
        sidebarLayout(
          sidebarPanel(

            selectInput("countryP",
                        "Choose a Country",
                        choices = countryNames,
                        selected = "Argentina"),
            sliderInput("yearsP",
                        "Choose a Date Range",
                        min = polyarchyData %>% summarize(minYear = min(year)),
                        max= polyarchyData %>% summarize(maxYear = max(year)),
                        value=c(2000,2014))
          ),

          mainPanel(
            plotOutput("allCountriesP"),
            plotOutput("chosenCountryP")
          )
        )
      ),

      tabPanel("Corruption"),
      tabPanel("Censorship")
    ),

  navbarMenu("Tables",
      tabPanel("All Data",
       fluidPage(
         fluidRow(
           column(4, selectInput("var", "Variable:", c("All", unique(as.character(dataWithNamesAndColors$variable))))),
           column(4, selectInput("state", "Country:", c("All", unique(as.character(dataWithNamesAndColors$StateNme)))))
           ),
         fluidRow(downloadButton("downloadData", "Download")),
         fluidRow(dataTableOutput("table"))
       )
      )
    )
  )

server <- function(input, output) {

  output$allCountries <- renderPlot({
    lopData$Highlight <- factor(ifelse(lopData$StateNme==input$country,input$country,"Other"),levels=c("Other", input$country))
    ggplot(lopData, aes(x=year,y=value)) + geom_step(aes(group = colors, color = colors, alpha = Highlight)) + labs(title="All Countries LOP by Year", x = "Year", y = "LOP") +
      geom_step(data=lopData[lopData$StateNme==input$country, ], aes(color="red")) +
      scale_x_continuous(limits=(input$years)) + theme_minimal() + theme(legend.position = "none")
  })

  output$chosenCountry <- renderPlot({

    title <- paste(input$country, "LOP by Year", sep = " ")
    chosenCountryData <- filter(lopData, StateNme == input$country)
    overallMeanLOP <- lopData %>% group_by(year) %>% summarize(meanLOP = mean(value, na.rm = TRUE))
    finalFilter <- chosenCountryData %>% left_join(overallMeanLOP)
    maxYear <- finalFilter %>% summarize(maxYear = max(year)) %>% pull(maxYear)
    maxValue <- finalFilter %>% summarize(maxValue = max(value)) %>% pull(maxValue)
    maxMeanLOP <- finalFilter %>% summarize(maxMeanLOP = max(meanLOP)) %>% pull(maxMeanLOP)
    ggplot(finalFilter) + geom_step(aes(x = year, y = value), color="red") + geom_text(aes(x = maxYear, y = maxValue, label = input$country)) +
      geom_step(aes(x=year, y=meanLOP)) + geom_text(aes(x=maxYear, y = maxMeanLOP, label = "Overall Mean")) +
      scale_x_continuous(limits=(input$years)) + labs(title = title, x = "Year", y = "LOP") + theme_minimal()

  })

  output$allCountriesP <- renderPlot({
    polyarchyData$Highlight <- factor(ifelse(polyarchyData$StateNme==input$countryP,input$countryP,"Other"),levels=c("Other", input$countryP))
    ggplot(polyarchyData, aes(x=year,y=value)) + geom_step(aes(group = colors, color = colors, alpha = Highlight)) + labs(title="All Countries Polyarchy by Year", x = "Year", y = "Polyarchy") +
      geom_step(data=polyarchyData[polyarchyData$StateNme==input$countryP, ], aes(color="red")) +
      scale_x_continuous(limits=(input$yearsP)) + theme_minimal() + theme(legend.position = "none")
  })

  output$chosenCountryP <- renderPlot({

    title <- paste(input$countryP, "Polyarchy by Year", sep = " ")
    chosenCountryDataP <- filter(polyarchyData, StateNme == input$countryP)
    overallMeanPolyarchy <- polyarchyData %>% group_by(year) %>% summarize(meanPolyarchy = mean(value, na.rm = TRUE))
    finalFilterP <- chosenCountryDataP %>% left_join(overallMeanPolyarchy)
    maxYearP <- finalFilterP %>% summarize(maxYearP = max(year)) %>% pull(maxYearP)
    maxValueP <- finalFilterP %>% summarize(maxValueP = max(value)) %>% pull(maxValueP)
    maxMeanPolyarchy <- finalFilterP %>% summarize(maxMeanPolyarchy = max(meanPolyarchy)) %>% pull(maxMeanPolyarchy)
    ggplot(finalFilterP) + geom_step(aes(x = year, y = value), color="red") + geom_text(aes(x = maxYearP, y = maxValueP, label = input$countryP)) +
      geom_step(aes(x=year, y=meanPolyarchy)) + geom_text(aes(x=maxYearP, y = maxMeanPolyarchy, label = "Overall Mean")) +
      scale_x_continuous(limits=(input$yearsP)) + labs(title = title, x = "Year", y = "Polyarchy") + theme_minimal()

  })

  output$table <- renderDataTable({

    data <- dataWithNames

    if (input$var != "All") {
      data <- (data %>% filter(variable == input$var))
    }
    else{
      data <- dataWithNames
    }
    if (input$state != "ALL") {
      data <- (data %>% filter(StateNme == input$state))
    }
    else{
      data <- data
    }

  })

  output$downloadData <- downloadHandler(
    filename = "SPIIdata.csv",
    content = function(file) {

      data <- dataWithNames

      if (input$var != "ALL") {
        data <- data %>% filter(variable == input$var)
        write.csv(data, file)
      }
      if (input$state != "All") {
        data <- data %>% filter(StateNme == input$state)
        write.csv(data, file)
      }
      else {
          write.csv(data, file)
      }
    }
  )
}

shinyApp(ui = ui, server = server)
