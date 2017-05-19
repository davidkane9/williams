library(shiny)
library(shinydashboard)
library(williamsmetrics)
library(dplyr)
library(ggplot2)

header <- dashboardHeader(title = "Williams College")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Phi Betta Kappa", tabName = "PBK", selected = TRUE),
    menuItem("Sigma Xi", tabName = "SX")
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "PBK",
            fluidRow(
              box(title = "Explore by Gender",
                  plotOutput("PBKGender"),
                  sliderInput("timelinePbkG", "Timeline:", min(graduates$year), max(graduates$year),
                              value = c(min(graduates$year), max(graduates$year))),
                  checkboxGroupInput("racePbkG", "Races to show:",
                                     c("White",
                                       "Black",
                                       "Asian",
                                       "Hispanic",
                                       "Other"), selected = c("White", "Asian", "Black", "Hispanic", "Other"))
              ),

              box(title = "Explore by Race",
                  plotOutput("PBKRace"),
                  sliderInput("timelinePbkR", "Timeline:", min(graduates$year), max(graduates$year),
                              value = c(min(graduates$year), max(graduates$year))),
                  checkboxGroupInput("genderPbkR", "Genders to show:",
                                     c("Male" = "male",
                                       "Female" = "female"), selected = c("male", "female"))
              )
            )


    ),

    tabItem(tabName = "SX",
            fluidRow(
              box(title = "Explore by Gender",
                  plotOutput("SXGender"),
                  sliderInput("timelineSXG", "Timeline:", min(graduates$year), max(graduates$year),
                              value = c(min(graduates$year), max(graduates$year))),
                  checkboxGroupInput("raceSXG", "Races to show:",
                                     c("White",
                                       "Black",
                                       "Asian",
                                       "Hispanic",
                                       "Other"), selected = c("White", "Asian", "Black", "Hispanic", "Other"))
              ),

              box(title = "Explore by Race",
                  plotOutput("SXRace"),
                  sliderInput("timelineSXR", "Timeline:", min(graduates$year), max(graduates$year),
                              value = c(min(graduates$year), max(graduates$year))),
                  checkboxGroupInput("genderSXR", "Genders to show:",
                                     c("Male" = "male",
                                       "Female" = "female"), selected = c("male", "female"))
              )
            )
    )
  )
)

renderPBKGenderPlot <- function(timeline, races){
  x <- graduates %>% dplyr::filter(year >= timeline[1], year <= timeline[2], race %in% races, !is.na(gender), Phi.Beta.Kappa) %>%
    dplyr::group_by(year) %>% mutate(total = n()) %>% group_by(gender, total, add = TRUE) %>% dplyr::mutate(percent = n()/ total * 100)
  ggplot(x, aes(x = year, y = percent, fill = gender)) + geom_bar(stat = "identity", position = "dodge") + labs(x = "Year", y = "Percent")
}

renderPBKRacePlot <- function(timeline, genders){
  x <- graduates %>% dplyr::filter(year >= timeline[1], year <= timeline[2], gender %in% genders, !is.na(race), Phi.Beta.Kappa) %>%
    dplyr::group_by(year) %>% mutate(total = n()) %>% group_by(race, total, add = TRUE) %>% dplyr::mutate(percent = n()/ total * 100)
  ggplot(x, aes(x = year, y = percent, fill = race)) + geom_bar(stat = "identity", position = "dodge") + labs(x = "Year", y = "Percent")
}



renderSXRacePlot <- function(timeline, genders){
  x <- graduates %>% dplyr::filter(year >= timeline[1], year <= timeline[2], gender %in% genders, !is.na(race), Sigma.Xi) %>%
    dplyr::group_by(year) %>% mutate(total = n()) %>% group_by(race, total, add = TRUE) %>% dplyr::mutate(percent = n()/ total * 100)
  ggplot(x, aes(x = year, y = percent, fill = race)) + geom_bar(stat = "identity", position = "dodge") + labs(x = "Year", y = "Percent")
}

renderSXGenderPlot <- function(timeline, races){
  x <- graduates %>% dplyr::filter(year >= timeline[1], year <= timeline[2], race %in% races, !is.na(gender), Sigma.Xi) %>%
    dplyr::group_by(year) %>% mutate(total = n()) %>% group_by(gender, total, add = TRUE) %>% dplyr::mutate(percent = n()/ total * 100)
  ggplot(x, aes(x = year, y = percent, fill = gender)) + geom_bar(stat = "identity", position = "dodge") + labs(x = "Year", y = "Percent")
}

ui <- dashboardPage(skin = "purple", header, sidebar, body)
server <- function(input, output) {

  output$PBKGender <- renderPlot({
    renderPBKGenderPlot(input$timelinePbkG, input$racePbkG)
  })
  output$PBKRace <- renderPlot({
    renderPBKRacePlot(input$timelinePbkR, input$genderPbkR)
  })

  output$SXGender <- renderPlot({
    renderSXGenderPlot(input$timelineSXG, input$raceSXG)
  })
  output$SXRace <- renderPlot({
    renderSXRacePlot(input$timelineSXR, input$genderSXR)
  })
}


shinyApp(ui, server)

