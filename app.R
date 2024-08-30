library(shiny)
library(dplyr)
library(highcharter)
library(DT)
library(bslib)

test.data <- readRDS("C:\\Users\\tjmay\\OneDrive\\Documents\\R Projects\\Survey Demographics\\data\\test_data.rds")
joined.pop <- readRDS("C:\\Users\\tjmay\\OneDrive\\Documents\\R Projects\\Survey Demographics\\data\\Joined_Pop.rds")
mapdata <- get_data_from_map(download_map_data("countries/us/us-all"))

test.data <- test.data %>% rename(Pop_Difference.WT = Pop_Difference.wt)

ui <- page_fillable(
  card(
    height = 700,
    layout_sidebar(
      sidebar = sidebar(
        bg = "lightgrey",
        tags$h3("Survey Demographics"),
        # Control the marker color
        checkboxInput(inputId = "inWeight", label = "Apply Weights", value = F),
        selectInput(inputId = "inVar", 
                    label = "Survey Demographics",
                    choices = c(
                      "State Population" = "Pop",
                      "Race and Ethnicity: Black" = "Black"
                      # "Income",
                      # "Education Level",
                      # "Age",
                      # "Urban/Rural Populations",
                      # "Religion"
                    ))
      ),
      mainPanel(
        highchartOutput(outputId = "chartUSAMap")
        )
    )
  ),
  card(verbatimTextOutput(outputId = "sum"))
)



server <- function(input, output) {
  # Hold the dataset as a reactive value in which states are filtered
  data <- reactive({
    test.data %>% select(starts_with(input$inVar)) %>% 
      select(ends_with(ifelse(input$inWeight, "WT", "Difference")))
  })


  

  # Render chart
  output$chartUSAMap <- renderHighchart({
    hcmap(
      "countries/us/us-all",
      showInLegend =F,
      data = data(),
      value = 2,
      name = "State"
    ) %>% 
      hc_title(text = "Differences Between Demographics in the Survey Sample and Population")
  })
  
  output$sum <- renderPrint({
    summary(data())
  })

}

shinyApp(ui = ui, server = server)

