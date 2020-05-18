library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(dsmodules)
library(hotr)
library(homodatum)
library(tidyverse)
library(genero)



t0UI <- function(id,
                 choices = c("pasted", "fileUpload", "sampleData", "googleSheets"),
                 choicesInline = FALSE,
                 selected = "pasted", ...) {
  
  # lapply(formalArgs(tableInputUI), function(d) {
  lapply(formalArgs(t0UI), function(d) {
    if (shiny::is.reactive(d))
      d <<- do.call(d, args = list())
    d
  })
  
  # UI
  ns <- shiny::NS(id)
  #choiceNames <-  choiceNames %||% choices
  #names(choices) <- choiceNames
  
  #info_style <- ifelse(is.null(uiOutput(ns("tableInputInfo"))), "display:flex;", "display:none;")
  
  shiny::tagList(shiny::div(id = ns("tableInput"),class="tableInput",
                            shiny::radioButtons(ns("tableInput"), "",
                                                choices = choices, selected = selected, inline = choicesInline),
                            shiny::uiOutput(ns("tableInputControls"))),
                 shiny::div(class = "box-tableInputInfo", #style = info_style,
                            shiny::uiOutput(ns("tableInputInfo"))))
  
}

#' @export
t0 <- function(input, output, session,
               infoList = NULL,
               pasteLabel = "Paste", pasteValue = "", pastePlaceholder = "Select your data and paste it here", pasteRows = 5,
               uploadLabel = "Choose CSV File", uploadButtonLabel = "Browse...", uploadPlaceholder = "No file selected",
               sampleLabel = "Select a sample data", sampleFiles = NULL, sampleSelected = NULL,
               googleSheetLabel = "Data from Google Sheet", googleSheetValue = "", googleSheetPlaceholder = "https://docs.google.com/spreadsheets/...",
               googleSheetPageLabel = "Sheet",
               ...) {
  
  # lapply(formalArgs(t0), function(d) {
  #   if (shiny::is.reactive(d))
  #     d <<- do.call(d, args = list())
  #   d
  # })
  # if (shiny::is.reactive(pasteLabel))
  #   pasteLabel <- pasteLabel
  
  output$tableInputControls <- shiny::renderUI({
    
    # str(session)
    # if(!exists(session))
    #   stop("No session defined in server.")
    
    ns <- session$ns
    
    if (shiny::is.reactive(sampleFiles))
      sampleFiles <- sampleFiles()
    
    if(input$tableInput == "sampleData"){
      if(!all(map_lgl(sampleFiles,file.exists)))
        stop("All Sample Files must exist")
    }
    
    tableInputControls <- list(pasted = textAreaInput(ns("inputDataPasted"), label = pasteLabel, value = pasteValue, placeholder = pastePlaceholder, rows = pasteRows),
                               fileUpload =  fileInput(ns("inputDataUpload"), uploadLabel, buttonLabel = uploadButtonLabel, placeholder = uploadPlaceholder,
                                                       accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv", ".xls", ".xlsx")),
                               sampleData = selectInput(ns("inputDataSample"), sampleLabel, choices = sampleFiles, selected = sampleSelected),
                               googleSheets = list(shiny::textInput(ns("inputDataSheet"), label = googleSheetLabel, value = googleSheetValue, placeholder = googleSheetPlaceholder),
                                                   shiny::numericInput(ns("inputDataGoogleSheetSheet"), googleSheetPageLabel, 1))
    )
    tableInputControls[[input$tableInput]]
  })
  
  output$tableInputInfo <- shiny::renderUI({
    ns <- session$ns
    tableInputInfo <- infoList[[input$tableInput]]
    if (is.null(tableInputInfo)) return()
    tableInputInfo
  })
  
  inputData <- shiny::reactive({
    inputType <- input$tableInput
    #readDataFromInputType(inputType)
    if(inputType == "pasted"){
      if (is.null(input$inputDataPasted)) return()
      if(input$inputDataPasted == "")
        return()
      df <- read_tsv(input$inputDataPasted)
    }
    if(inputType ==  "fileUpload"){
      if(is.null(input$inputDataUpload)) return()
      old_path <- input$inputDataUpload$datapath
      path <- file.path(tempdir(),input$inputDataUpload$name)
      file.copy(old_path,path)
      df <- rio::import(path)
    }
    if(inputType ==  "sampleData"){
      if (is.null(input$inputDataSample)) return()
      file <- as.character(input$inputDataSample)
      df <- readr::read_csv(file)
    }
    if (inputType == "googleSheets") {
      if (is.null(input$inputDataSheet)) return()
      if (input$inputDataSheet == "") return()
      library(googlesheets4)
      googlesheets4::sheets_deauth()
      id_file <- gsub(".*\\/d\\/|\\/edit.*", '', input$inputDataSheet)
      googlesheets4::sheets_get(id_file)
      df <- googlesheets4::read_sheet(id_file)
    }
    return(df)
  })
  
  inputData
}



# falta que se traduzcan los choices del selector de idiomas


ui <- panelsPage(useShi18ny(),
                 panel(title = ui_("upload_data"), 
                       width = 200,
                       body = uiOutput("table_input")),
                 # body = tableInputUI("initial_data",
                 #                     choices = list("Sample data" = "sampleData",
                 #                                    "Copy & paste" = "pasted",
                 #                                    "CSV/XLS Upload" = "fileUpload",
                 #                                    "Google sheets" = "googleSheets"),
                 #                     selected = "sampleData")),
                 panel(title = ui_("dataset"), 
                       width = 300,
                       body = uiOutput("dataset")),
                 panel(title = ui_("options"), 
                       width = 250,
                       color = "chardonnay",
                       body = uiOutput("controls")),
                 panel(title = ui_("viz"),
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body = div(#infomessage(p("Hello")),
                         langSelectorInput("lang", position = "fixed"),
                         uiOutput("result", height = "80vh"),
                         # verbatimTextOutput("debug"),
                         shinypanels::modal(id = "test",
                                            title = ui_("download_table"),
                                            # downloadTableUI("download_data_button", "Download", formats = c("csv", "xlsx", "json")))),
                                            uiOutput("modal"))),
                       # footer = uiOutput("modal_button")))
                       footer = shinypanels::modalButton(label = "Download table", modal_id = "test")))

# shinypanels::modal(id = "test",
#                                             title = ui_("download_table"),
#                                             downloadTableUI("download_data_button", "Download", formats = c("csv", "xlsx", "json")))),
#                        footer = shinypanels::modalButton(label = "Download table", modal_id = "test")))


server <-  function(input, output, session) {
  
  i18n <- list(defaultLang = "en", availableLangs = c("es", "en", "pt"))
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = TRUE)
  observeEvent(lang(), {
    uiLangUpdate(input$shi18ny_ui_classes, lang())
  })
  
  output$table_input <- renderUI({
    choices <- c("sampleData", "pasted", "fileUpload", "googleSheets")
    names(choices) <- i_(c("sample", "paste", "upload", "google"), lang = lang())
    tableInputUI("initial_data",
                 # t0UI("initial_data",
                 choices = choices,
                 # selected is important for inputs not be re-initialized
                 selected = ifelse(is.null(input$`initial_data-tableInput`), "sampleData", input$`initial_data-tableInput`))
    
  })
  
  path <- "parmesan"
  parmesan <- parmesan_load(path)
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  parmesan_lang <- reactive({i_(parmesan, lang(), keys = c("label", "choices"))})
  output_parmesan("controls",
                  parmesan = parmesan_lang,
                  input = input,
                  output = output,
                  env = environment())
  
  # output$debug <- renderPrint({
  #   str(result())
  #   data_input()
  # })
  
  
  output$modal <- renderUI({
    dw <- i_("download", lang())
    downloadTableUI("download_data_button", dw, formats = c("csv", "xlsx", "json"))
  })
  
  # output$modal_button <- renderUI({
  #   shinypanels::modalButton(label = "Download table", modal_id = "test")
  # })
  
  # lista labels módulos en idioma seleccionado
  labels <- reactive({
    list(sampleLabel = i_("sample_lb", lang()), 
         sampleFiles = list("Names" = "data/sampleData/nombres_altura.csv",
                            "Employees" = "data/sampleData/employees.csv"),
         pasteLabel = i_("paste", lang()), pasteValue = "", pastePlaceholder = i_("paste_pl", lang()), pasteRows = 5, 
         uploadLabel = i_("upload_lb", lang()), uploadButtonLabel = i_("upload_bt_lb", lang()), uploadPlaceholder = i_("upload_pl", lang()),
         googleSheetLabel = i_("google_sh_lb", lang()), googleSheetValue = "", googleSheetPlaceholder = i_("google_sh_pl", lang()),
         googleSheetPageLabel = i_("google_sh_pg_lb", lang())
         
         # infoList = list("pasted" = ("Esto es información sobre el paste"),
         #                 "fileUpload" = HTML("Esto es información sobre el fileUpload"),
         #                 "sampleData" = HTML("Info sample Data"),
         #                 "googleSheets" = HTML("IFO GGO"))
    )
  })
  
  # observe({
  # observeEvent(i(), {
  inputData <- eventReactive(labels(), {
    # observe({
    do.call(callModule, c(tableInput,
                          # do.call(callModule, c(t0,
                          "initial_data",
                          labels()))
                          # sampleLabel = reactive(i_("sample_lb", lang))))
    #                       sampleLabel = i_("sample_lb", reactive(lang()))#, 
    # sampleFiles = list("Names" = "data/sampleData/nombres_altura.csv",
    #                    "Employees" = "data/sampleData/employees.csv"),
    # pasteLabel = i_("paste", lang()), pasteValue = "", pastePlaceholder = i_("paste_pl", lang()), pasteRows = 5, 
    # ))
    
    
    # inputData <- callModule(t0,
    #                         "initial_data",
    #                         sampleLabel = i_("sample_lb", reactive(lang()))) 
    #                         # Select a sample data(
    #                         i(),
    #                         # sampleLabel = i(), 
    #                         # sampleFiles = list("Names" = "data/sampleData/nombres_altura.csv",
    #                         #                    "Employees" = "data/sampleData/employees.csv"),
    #                         # pasteLabel = i_("paste", lang()), 
    #                         pasteLabel = "DSFHGS<", 
    #                         pasteValue = "", pastePlaceholder = "Selectand paste it here", 
    #                         pasteRows = 5, uploadLabel = "Choose CSV File", uploadButtonLabel = "Browse...", 
    #                         uploadPlaceholder = "No file selected", 
    #                         googleSheetLabel = "Data from Google Sheet", 
    #                         googleSheetValue = "", googleSheetPlaceholder = "https://docs.google.com/spreadsheets/...", 
    #                         googleSheetPageLabel = "Sheet"
    #                         
    #                         # infoList = list("pasted" = ("Esto es información sobre el paste"),
    #                         #                 "fileUpload" = HTML("Esto es información sobre el fileUpload"),
    #                         #                 "sampleData" = HTML("Info sample Data"),
    #                         #                 "googleSheets" = HTML("IFO GGO"))
    # )
  })
  
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
