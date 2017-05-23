library(shiny)
library(shinydashboard)
library(williamsmetrics)
library(dplyr)
library(ggplot2)


header <- dashboardHeader(title = "Williams College")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Summa Cum Laude", tabName = "summa", selected = TRUE),
    menuItem("Magna Cum Laude", tabName = "magna"),
    menuItem("Cum Laude", tabName = "cum"),
    menuItem("None", tabName = "none")

  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "summa",
            fluidRow(
              box(width = 300, title = "Explore by Gender",
                  plotOutput("summaGender"),
                  sliderInput("timelineSummaG", "Timeline:", min(graduates$year), max(graduates$year),
                              value = c(min(graduates$year), max(graduates$year))),
                  checkboxGroupInput("raceSummaG", "Races to show:",
                                     c("White",
                                       "Black",
                                       "Asian",
                                       "Hispanic",
                                       "Other"), selected = c("White", "Asian", "Black", "Hispanic", "Other"),
                                     inline = TRUE)
              )),
            fluidRow(

              box(width = 300, title = "Explore by Race",
                  plotOutput("summaRace"),
                  sliderInput("timelineSummaR", "Timeline:", min(graduates$year), max(graduates$year),
                              value = c(min(graduates$year), max(graduates$year))),
                  checkboxGroupInput("genderSummaR", "Genders to show:",
                                     c("Male" = "male",
                                       "Female" = "female"), selected = c("male", "female"),
                                     inline = TRUE)
              )
            )


    ),

    tabItem(tabName = "magna",
            fluidRow(
              box(width = 300, title = "Explore by Gender",
                  plotOutput("magnaGender"),
                  sliderInput("timelineMagnaG", "Timeline:", min(graduates$year), max(graduates$year),
                              value = c(min(graduates$year), max(graduates$year))),
                  checkboxGroupInput("raceMagnaG", "Races to show:",
                                     c("White",
                                       "Black",
                                       "Asian",
                                       "Hispanic",
                                       "Other"), selected = c("White", "Asian", "Black", "Hispanic", "Other"),
                                     inline = TRUE)
              )),
            fluidRow(

              box(width = 300, title = "Explore by Race",
                  plotOutput("magnaRace"),
                  sliderInput("timelineMagnaR", "Timeline:", min(graduates$year), max(graduates$year),
                              value = c(min(graduates$year), max(graduates$year))),
                  checkboxGroupInput("genderMagnaR", "Genders to show:",
                                     c("Male" = "male",
                                       "Female" = "female"), selected = c("male", "female"),
                                     inline = TRUE)
              )
            )


    ),

    tabItem(tabName = "cum",
            fluidRow(
              box(width = 300, title = "Explore by Gender",
                  plotOutput("cumGender"),
                  sliderInput("timelineCumG", "Timeline:", min(graduates$year), max(graduates$year),
                              value = c(min(graduates$year), max(graduates$year))),
                  checkboxGroupInput("raceCumG", "Races to show:",
                                     c("White",
                                       "Black",
                                       "Asian",
                                       "Hispanic",
                                       "Other"), selected = c("White", "Asian", "Black", "Hispanic", "Other"),
                                     inline = TRUE)
              )),
            fluidRow(

              box(width = 300, title = "Explore by Race",
                  plotOutput("cumRace"),
                  sliderInput("timelineCumR", "Timeline:", min(graduates$year), max(graduates$year),
                              value = c(min(graduates$year), max(graduates$year))),
                  checkboxGroupInput("genderCumR", "Genders to show:",
                                     c("Male" = "male",
                                       "Female" = "female"), selected = c("male", "female"),
                                     inline = TRUE)
              )
            )


    ),

    tabItem(tabName = "none",
            fluidRow(
              box(width = 300, title = "Explore by Gender",
                  plotOutput("noneGender"),
                  sliderInput("timelineNoneG", "Timeline:", min(graduates$year), max(graduates$year),
                              value = c(min(graduates$year), max(graduates$year))),
                  checkboxGroupInput("raceNoneG", "Races to show:",
                                     c("White",
                                       "Black",
                                       "Asian",
                                       "Hispanic",
                                       "Other"), selected = c("White", "Asian", "Black", "Hispanic", "Other"),
                                     inline = TRUE)
              )),
            fluidRow(

              box(width = 300, title = "Explore by Race",
                  plotOutput("noneRace"),
                  sliderInput("timelineNoneR", "Timeline:", min(graduates$year), max(graduates$year),
                              value = c(min(graduates$year), max(graduates$year))),
                  checkboxGroupInput("genderNoneR", "Genders to show:",
                                     c("Male" = "male",
                                       "Female" = "female"), selected = c("male", "female"),
                                     inline = TRUE)
              )
            )


    )
  )
)

renderGenderPlot <- function(timeline, races, hon){
  graduates$latin.honors[which(is.na(graduates$latin.honors))] <- "None"
  x <- graduates %>% dplyr::filter(year >= timeline[1], year <= timeline[2], race %in% races, !is.na(gender), latin.honors == hon) %>%
    dplyr::group_by(year) %>% mutate(total = n()) %>% group_by(gender, total, add = TRUE) %>% dplyr::mutate(percent = n()/ total * 100)
  ggplot(x, aes(x = year, y = percent, fill = gender)) + geom_bar(stat = "identity", position = "dodge") + labs(x = "Year", y = "Percent")
}

renderRacePlot <- function(timeline, genders, hon){
  print(hon)
  graduates$latin.honors[which(is.na(graduates$latin.honors))] <- "None"
  x <- graduates %>% dplyr::filter(year >= timeline[1], year <= timeline[2], gender %in% genders, !is.na(race), latin.honors == hon) %>%
    dplyr::group_by(year) %>% mutate(total = n()) %>% group_by(race, total, add = TRUE) %>% dplyr::mutate(percent = n()/ total * 100)
  ggplot(x, aes(x = year, y = percent, fill = race)) + geom_bar(stat = "identity", position = "dodge") + labs(x = "Year", y = "Percent")
}



ui <- dashboardPage(skin = "purple", header, sidebar, body)
server <- function(input, output) {

  output$summaGender <- renderPlot({
    renderGenderPlot(input$timelineSummaG, input$raceSummaG, "Summa Cum Laude")
  })
  output$summaRace <- renderPlot({
    renderRacePlot(input$timelineSummaR, input$genderSummaR, "Summa Cum Laude")
  })

  output$magnaGender <- renderPlot({
    renderGenderPlot(input$timelineMagnaG, input$raceMagnaG, "Magna Cum Laude")
  })
  output$magnaRace <- renderPlot({
    renderRacePlot(input$timelineMagnaR, input$genderMagnaR, "Magna Cum Laude")
  })

  output$cumGender <- renderPlot({
    renderGenderPlot(input$timelineCumG, input$raceCumG, "Cum Laude")
  })
  output$cumRace <- renderPlot({
    renderRacePlot(input$timelineCumR, input$genderCumR, "Cum Laude")
  })

  output$noneGender <- renderPlot({
    renderGenderPlot(input$timelineNoneG, input$raceNoneG, "None")
  })
  output$noneRace <- renderPlot({
    renderRacePlot(input$timelineNoneR, input$genderNoneR, "None")
  })
}


shinyApp(ui, server, options = list(height = 1250, width = 850))

