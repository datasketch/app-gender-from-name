library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(V8)
library(dsmodules)
library(hotr)
library(tidyr)
library(homodatum)
library(genero)
library(dspins)
library(shinycustomloader)



ui <- panelsPage(useShi18ny(),
                 showDebug(),
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
                       title_plugin = uiOutput("download"),
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body = div(#infomessage(p("Hello")),
                         # HTML('<a id="download_data_button-DownloadTbllink" data-modal="md-download_data_button-DownloadTbllink" class="modal-trigger dropdown-action-item dropdown-action-item-modal-shinypanels" title="Get link" data-action="download_data_button-DownloadTbllink"><img class="dropdown-action-item-image" src="dropdownAction/images/share_link.svg"><span class="dropdown-action-item-label">Get link</span></a>'),
                         langSelectorInput("lang", position = "fixed"),
                         withLoader(uiOutput("result"), type = "image", loader = "loading_gris.gif"))))



server <-  function(input, output, session) {
  
  i18n <- list(defaultLang = "en", availableLangs = c("es", "en", "pt_BR"))
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = FALSE)
  observeEvent(lang(), {uiLangUpdate(input$shi18ny_ui_classes, lang())})
  
  output$table_input <- renderUI({
    choices <- c("sampleData", "pasted", "fileUpload", "googleSheets")
    names(choices) <- i_(c("sample", "paste", "upload", "google"), lang = lang())
    tableInputUI("initial_data",
                 label = "",
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
  
  inputData <- eventReactive(list(labels(), input$`initial_data-tableInput`), {
    do.call(tableInputServer, c("initial_data", labels()))
  })
  
  output$dataset <- renderUI({
    req(inputData())
    suppressWarnings(hotr("hotr_input", data = inputData(), order = NULL, options = list(height = "86vh"), enableCTypes = FALSE))
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
    # hotr_fringe(input$hotr_input)
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
    safe_genero <- purrr::safely(genero)
    safe_genero(data_input(), col = input$name_column, result_as = result_as, lang = input$gender_lang)
  })
  
  output$download <- renderUI({
    lb <- i_("download_table", lang())
    dw <- i_("download", lang())
    gl <- i_("get_link", lang())
    mb <- list(textInput("name", i_("gl_name", lang())),
               textInput("description", i_("gl_description", lang())),
               selectInput("license", i_("gl_license", lang()), choices = c("CC0", "CC-BY")),
               selectizeInput("tags", i_("gl_tags", lang()), choices = list("No tag" = "no-tag"), multiple = TRUE, options = list(plugins= list('remove_button', 'drag_drop'))),
               selectizeInput("category", i_("gl_category", lang()), choices = list("No category" = "no-category")))
    downloadDsUI("download_data_button", dropdownLabel = lb, text = dw, formats = c("csv", "xlsx", "json"),
                 display = "dropdown", dropdownWidth = 170, getLinkLabel = gl, modalTitle = gl, modalBody = mb,
                 modalButtonLabel = i_("gl_save", lang()), modalLinkLabel = i_("gl_url", lang()), modalIframeLabel = i_("gl_iframe", lang()),
                 modalFormatChoices = c("HTML" = "html", "CSV" = "csv", "JSON" = "json"))
  })
  
  output$result <- renderUI({
    res <- result()
    warn <- NULL
    if (is.null(res$result)) {
      warn <- infomessage(p(res$error$message))
    }
    list(warn,
         # dataTableOutput("resu3  lt_table"),
         hotr("hotr_result", data = res$result, options = list(height = "85vh", maxRows = 33), enableCTypes = FALSE))
  })
  
  # url params
  par <- list(user_name = "brandon", org_name = NULL)
  url_par <- reactive({
    url_params(par, session)
  })
  
  # función con user board connect y set locale
  pin_ <- function(x, bkt, ...) {
    x <- dsmodules:::eval_reactives(x)
    bkt <- dsmodules:::eval_reactives(bkt)
    nm <- input$`download_data_button-modal_form-name`
    if (!nzchar(input$`download_data_button-modal_form-name`)) {
      nm <- paste0("saved", "_", gsub("[ _:]", "-", substr(as.POSIXct(Sys.time()), 1, 19)))
      updateTextInput(session, "download_data_button-modal_form-name", value = nm)
    } 
    dv <- fringe(x)
    dv$name <- nm
    dv$slug <- nm
    dv$description <- input$`download_data_button-modal_form-description`
    dv$license <- input$`download_data_button-modal_form-license`
    dv$tags <- input$`download_data_button-modal_form-tags`
    dv$category <- input$`download_data_button-modal_form-category`
    dspins_user_board_connect(bkt)
    Sys.setlocale(locale = "en_US.UTF-8")
    pin(dv, bucket_id = bkt)
  }
  
  
  # dowload
  observe({
    downloadDsServer("download_data_button", element = reactive(result()$result), formats = c("csv", "xlsx", "json"),
                     errorMessage = i_("gl_error", lang()),
                     modalFunction = pin_, reactive(result()$result),
                     bkt = url_par()$inputs$user_name)
    # downloadDsServer("download_data_button", element = data.frame(a = 1:4), formats = c("csv", "xlsx", "json"),
    #                  errorMessage = i_("gl_error", lang()),
    #                  modalFunction = pin_, data.frame(a = 1:4),
    #                  bkt = url_par()$inputs$user_name)
  })
  
}





shinyApp(ui, server)