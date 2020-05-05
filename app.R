library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(hotr)
library(homodatum)
library(dsmodules)
library(tidyverse)
library(genero)

# Internacionalización
# Arreglar código
# Issue en hotr de la distinción de columnas de la tabla 



ui <- panelsPage(panel(title = "Upload Data", 
                       width = 200,
                       body = tableInputUI("initial_data",
                                           choices = list("Sample data" = "sampleData",
                                                          "Copy & paste" = "pasted",
                                                          "CSV/XLS Upload" = "fileUpload",
                                                          "Google sheets" = "googleSheets"),
                                           selected = "sampleData")),
                 panel(title = "Dataset",
                       width = 300,
                       body = uiOutput("dataset")),
                 panel(title = "Options",
                       width = 250,
                       body = uiOutput("controls")),
                 panel(title = "Viz",
                       can_collapse = FALSE,
                       body = div(#infomessage(p("Hello")),
                         uiOutput("result", height = "80vh"),
                         # verbatimTextOutput("debug"),
                         shinypanels::modal(id = "test", 
                                            title = "Download table",
                                            downloadTableUI("download_data_button", "Download", formats = c("csv", "xlsx", "json")))),
                       footer = shinypanels::modalButton(label = "Download table", modal_id = "test")))


server <-  function(input, output, session) {
  
  path <- "parmesan"
  parmesan <- parmesan_load(path)
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  output_parmesan("controls", 
                  parmesan = parmesan,
                  input = input,
                  output = output)
  
  # output$debug <- renderPrint({
  #   str(result())
  #   data_input()
  # })
  
  inputData <- callModule(tableInput, 
                          "initial_data",
                          sampleFile = list("Names" = "data/sampleData/nombres_altura.csv",
                                            "Employees" = "data/sampleData/employees.csv")#,
                          # infoList = list("pasted" = ("Esto es información sobre el paste"),
                          #                 "fileUpload" = HTML("Esto es información sobre el fileUpload"),
                          #                 "sampleData" = HTML("Info sample Data"),
                          #                 "googleSheets" = HTML("IFO GGO"))
  )
  
  output$dataset <- renderUI({
    if (is.null(inputData())) 
      return()
    # order_var <- input$var_order
    # suppressWarnings(hotr("hotr_input", data = inputData(), order = order_var, options = list(height = 470), enableCTypes = FALSE))
    suppressWarnings(hotr("hotr_input", data = inputData(), order = NULL, options = list(height = 470), enableCTypes = FALSE))
  })
  
  data_input <- reactive({
    req(input$hotr_input)
    hotr_table(input$hotr_input)
  })
  
  data_input_names <- reactive({
    req(data_input())
    names(data_input())
  })
  
  name_col <- reactive({
    which_name_column(data_input_names())
  })
  
  result <- reactive({
    req(data_input(), input$male, input$female)
    result_as <- c(male = input$male, female = input$female)
    safe_genero <- safely(genero)
    res <- safe_genero(data_input(), col = input$name_column, result_as = result_as, lang = input$gender_lang)
    res
  })
  
  output$result <- renderUI({
    lapply(c("csv", "xlsx", "json"), function(z) {
      buttonId <- paste0("download_data_button-DownloadTbl", z)
      session$sendCustomMessage("setButtonState", c("none", buttonId)) 
    })
    res <- result()
    warn <- NULL
    if (is.null(res$result)) {
      warn <- infomessage(p(res$error$message))
    }
    list(warn,
         # dataTableOutput("resu3  lt_table"),
         hotr("hotr_result", data = res$result, options = list(height = 470), enableCTypes = FALSE))
  })
  
  # descargas
  callModule(downloadTable, "download_data_button", table = reactive(result()$result), formats = c("csv", "xlsx", "json"))
  
}


shinyApp(ui, server)
