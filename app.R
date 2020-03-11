library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(hotr)
library(dsmodules)
library(tidyverse)
library(genero)
library(homodatum)

ui <- panelsPage(
  panel(
    title = "Upload Data", 
    width = 200,
    body = div(
      tableInputUI("initial_data",
                   choices = list(
                     "Sample Data"="sampleData",
                     "Copy & Paste"="pasted",
                     "CSV/XLS Upload"="fileUpload",
                     "Google Sheets" = "googleSheets"
                   ),
                   selected = "sampleData")
    )
  ),
  panel(
    title = "Dataset",
    width = 300,
    collapsed = FALSE,
    body = div(
      uiOutput("dataset") 
    )
  ),
  panel(
    title = "Options",
    width = 250,
    collapse = FALSE,
    body = div(
      uiOutput("controls")
    )
  ),
  panel(
    title = "Viz",
    body = div(
      # infomessage(p("Hello")),
      uiOutput("result"),
      # verbatimTextOutput("debug"),
      shinypanels::modal(id = 'test', title = 'Download plot',
                         dsmodules::downloadFileUI("download_data_button")
      ),
      shinypanels::modalButton(label = "Download Data", modal_id = "test")
    ),
    footer = uiOutput("viz_icons")
  )
)

config_path <- "parmesan"
# Reactive part
input_ids <- parmesan_input_ids(section = NULL, config_path = "parmesan")
input_ids_values <- lapply(input_ids, function(i){
  NA
})
names(input_ids_values) <- input_ids


server <-  function(input, output, session) {
  
  output$debug <- renderPrint({
    str(result())
    data_input()
  })
  
  react_env <- new.env()
  
  
  vals <- reactiveValues()
  vals$inputs <- input_ids_values
  react_env <- new.env()
  
  observe({
    lapply(input_ids, function(i){
      vals$inputs[[i]] <- input[[i]]
      vals
    })
  })
  
  data_input <- reactive({
    req(input$hotr_input)
    hotr_table(input$hotr_input)
  }, env = react_env)
  
  data_input_names <- reactive({
    req(data_input())
    names(data_input())
  }, env = react_env)
  
  name_col <- reactive({
    which_name_column(data_input_names())
  }, env = react_env)
  
  
  inputData <- callModule(tableInput, "initial_data",
                          sampleFile = list(
                            "Employees" = "data/sampleData/employees.csv",
                            "Nombres" = "data/sampleData/nombres.csv",
                            "Class"="data/sampleData/class.csv"
                          ),
                          infoList = list(
                            "pasted" = ("Esto es información sobre el paste"),
                            "fileUpload" = HTML("Esto es información sobre el fileUpload"),
                            "sampleData" = HTML("Info sample Data"),
                            "googleSheets" = HTML("IFO GGO")
                          ))
  
  output$dataset <- renderUI({
    if(is.null(inputData()))return()
    order_var <- input$var_order
    suppressWarnings(
      hotr("hotr_input", data = inputData(), order = order_var, options = list(height = 470), enableCTypes = FALSE)
    )
  })
  
  output$controls <- renderUI({
    parmesan_render_ui(input = input, env = react_env)
  })
  
  result <- reactive({
    req(data_input(), input$male, input$female)
    result_as <- c(male = input$male, female = input$female)
    safe_genero <- safely(genero)
    res <- safe_genero(data_input(), col = input$name_column, result_as = result_as, lang = input$gender_lang)
    res
  })
  
  output$result <- renderUI({
    res <- result()
    warn <- NULL
    if(is.null(res$result)){
      warn <- infomessage(p(res$error$message))
    }
    
    list(
      warn,
      # dataTableOutput("result_table"),
      hotr("hotr_result", data = res$result, options = list(height = 470), enableCTypes = FALSE)
      
    )
  })
  
  output$result_table <- renderDataTable({
    # Guess Gender
    result()$result
    #cars
  })
  
  
}


shinyApp(ui, server)
