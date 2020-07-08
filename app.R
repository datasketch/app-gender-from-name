library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(V8)
library(dsmodules)
library(hotr)
library(homodatum)
library(purrr)
# library(rio)
library(genero)
library(shinycustomloader)



ui <- panelsPage(useShi18ny(),
                 panel(title = ui_("upload_data"), 
                       width = 200,
                       body = uiOutput("table_input")),
                 panel(title = ui_("dataset"), 
                       width = 300,
                       body = uiOutput("dataset")),
                 panel(title = ui_("options"), 
                       width = 250,
                       color = "chardonnay",
                       body = uiOutput("controls")),
                 panel(title = ui_("viz"),
                         # div(style = "display: flex; justify-content: space-between;",
                         #     p(class = "panel-header-title text-chardonnay", "viz"),
                         #     downloadTableUI("download_data_button", label = "dw", text = "dw", formats = c("csv", "xlsx", "json"), display = "dropdown")),
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body = div(#infomessage(p("Hello")),
                         langSelectorInput("lang", position = "fixed"),
                         uiOutput("download"),
                         br(),
                         withLoader(uiOutput("result"), type = "image", loader = "loading_gris.gif"))))



server <-  function(input, output, session) {
  
  i18n <- list(defaultLang = "en", availableLangs = c("es", "en", "pt_BR"))
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = FALSE)
  observeEvent(lang(), {uiLangUpdate(input$shi18ny_ui_classes, lang())})
  
  output$table_input <- renderUI({
    choices <- c("sampleData", "pasted", "fileUpload", "googleSheets")
    names(choices) <- i_(c("sample", "paste", "upload", "google"), lang = lang())
    tableInputUI("initial_data",
                 choices = choices,
                 # selected is important for inputs not be re-initialized when language changes
                 selected = ifelse(is.null(input$`initial_data-tableInput`), "sampleData", input$`initial_data-tableInput`))
  })
  
  # lista labels módulos en idioma seleccionado
  labels <- reactive({
    
    sm_f <- paste0("data/sampleData/", i_(c("sample_ch_0", "sample_ch_1"), lang()))
    names(sm_f) <- i_(c("sample_ch_nm_0", "sample_ch_nm_1"), lang())
    
    list(sampleLabel = i_("sample_lb", lang()), 

         sampleFiles = sm_f,
         
         pasteLabel = i_("paste", lang()), 
         pasteValue = "", 
         pastePlaceholder = i_("paste_pl", lang()), 
         pasteRows = 5,
         
         uploadLabel = i_("upload_lb", lang()),
         uploadButtonLabel = i_("upload_bt_lb", lang()), 
         uploadPlaceholder = i_("upload_pl", lang()),
         
         googleSheetLabel = i_("google_sh_lb", lang()),
         googleSheetValue = "",
         googleSheetPlaceholder = i_("google_sh_pl", lang()),
         googleSheetPageLabel = i_("google_sh_pg_lb", lang())
         
         # infoList = list("pasted" = ("Esto es información sobre el paste"),
         #                 "fileUpload" = HTML("Esto es información sobre el fileUpload"),
         #                 "sampleData" = HTML("Info sample Data"),
         #                 "googleSheets" = HTML("IFO GGO"))
    )
  })
  
  inputData <- eventReactive(labels(), {
    do.call(callModule, c(tableInput, "initial_data", labels()))
  })
  
  output$dataset <- renderUI({
    if (is.null(inputData())) 
      return()
    suppressWarnings(hotr("hotr_input", data = inputData(), order = NULL, options = list(height = "80vh"), enableCTypes = FALSE))
  })
  
  path <- "parmesan"
  parmesan <- parmesan_load(path)
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  parmesan_lang <- reactive({i_(parmesan, lang(), keys = c("label", "choices", "text"))})
  output_parmesan("controls",
                  parmesan = parmesan_lang,
                  input = input,
                  output = output,
                  env = environment())
  
  # updating names choices of inputs depending on language
  observeEvent(lang(), {
    ch <- as.character(parmesan$columns$inputs[[2]]$input_params$choices)
    names(ch) <- i_(ch, lang())
    
    updateTextInput(session, "female", value = i_("F", lang()))
    updateTextInput(session, "male", value = i_("M", lang()))
    updateSelectInput(session, "gender_lang", choices = ch, selected = input$gender_lang)
  })
  
  # output$debug <- renderPrint({
  #   str(result())
  #   data_input()
  # })
  
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
  
  output$download <- renderUI({
    lb <- i_("download_table", lang())
    dw <- i_("download", lang())
    downloadTableUI("download_data_button", label = lb, text = dw, formats = c("csv", "xlsx", "json"), display = "dropdown")
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
         hotr("hotr_result", data = res$result, options = list(height = "80vh"), enableCTypes = FALSE))
  })
  
  # dowload
  callModule(downloadTable, "download_data_button", table = reactive(result()$result), formats = c("csv", "xlsx", "json"))
  
}



shinyApp(ui, server)
