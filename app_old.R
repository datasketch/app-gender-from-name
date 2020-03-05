library(shiny)
library(dsAppLayout)
library(dsAppWidgets)
library(shinycustomloader)

styles <- "

.shiny-output-error {
  visibility: hidden;
}
  
.shiny-output-error:before {
  visibility: hidden; 
}

.style_section {
  font-weight: 700;
  margin-bottom: 7px;
  letter-spacing: 1px;
}

.form-group label {
  font-size: 15px;
  font-weight: 500;
  letter-spacing: 1px;
}

.buttonDown {
  border: 1px solid;
  margin-left: 5px;
  display: inline-block;
}

.dn {
  font-size: 23pt;
  display: none !important;
  color: blue;
}

"

ui <- dsAppPanels(panel(title = "Datos",
                        color = "chardonnay", 
                        collapsed = FALSE, 
                        width = "600px",
                        body = div(tableInputUI("dataIn",
                                                selected = "sampleData",
                                                choices = list("Muestra" = "sampleData",
                                                               "Copiar & Pegar" = "pasted",
                                                               "Cargar" = "fileUpload",
                                                               "GoogleSheet" = "googleSheet",
                                                               "Mi librería" = "dsLibrary")),
                                   selectizeInput("genderCol", "Columna nombres", choices = ""),
                                   uiOutput("data_preview"))),
                  panel(title = "Clasificación",
                        color = "magenta", 
                        collapsed = FALSE,
                        body = withLoader(uiOutput("tableEnd"), type = "image", loader = "loading.svg")))


random_name <- function(n = 10) {
  paste0(sample(c(LETTERS, letters, 0:9), n, TRUE), collapse = "")
}

removeAccents <- function(string) {
  string <- gsub("\\s+", " ", string)
  accents <- "àèìòùÀÈÌÒÙáéíóúýÁÉÍÓÚÝäëïöüÄËÏÖÜâêîôûÂÊÎÔÛñÑç"
  translation <- "aeiouAEIOUaeiouyAEIOUYaeiouAEIOUaeiouAEIOUnNc"
  chartr(accents, translation, string)
}

gender_classified <- read_csv("data/names-gender-es.csv")

server <- function(input, output, session) {
  
  # reactivo que almacena el plot
  tablas <- reactiveValues(dt = NULL,
                           cl = NULL)
  
  # reactivo que almacena lo que se importa
  inputData <- callModule(tableInput, "dataIn",
                          sampleFile = list("Nombres" = "data/sampleData/nombres.csv"))
  
  # opciones del selector dependiendo las columnas de la tabla importada
  output$data_preview <- renderUI({
    if (is.null(inputData())) return()
    tablas$dt <- inputData()
    updateSelectInput(session, "genderCol", choices = names(inputData()))
    list(dsHot("dataTable", data = inputData()))
  })
  
  # tabla clasificada
  # tabla_class <- eventReactive(input$classifyTable, {
  # observeEvent(tablas$dt, {
  observe({
    tb <- tablas$dt
    if (sum(is.null(input$genderCol) | nzchar(input$genderCol)) == 0) return()
    tb0 <- tb[, input$genderCol]
    names(tb0) <- "z"
    tb0$a <- toupper(removeAccents(tb0$z))
    tb1 <- left_join(tb0, gender_classified, by = "a")
    tb2 <- which(is.na(tb1$gender))
    map(tb2, function(s) {
      dt <- tb1[s, ]
      s0 <- strsplit(dt$a, "\\s")[[1]]
      s1 <- data.frame(a = s0, stringsAsFactors = FALSE) %>%
        left_join(gender_classified, by = "a") %>%
        group_by(gender) %>%
        summarise(b = n())
      if (nrow(s1) <= 1) {
        dt$gender <- NA
      } else {
        if (s1$b[1] == s1$b[2]) {
          dt$gender <- NA
        } else {
          mx <- which(max(s1$b) == s1$b)
          dt$gender <- s1$gender[mx]
        }
      }
      tb1[s, ] <<- dt
    })
    nm <- "z"
    names(nm) <- input$genderCol
    tb3 <- tb1[, c("z", "gender")]
    tb4 <- left_join(tb, tb3, by = nm)
    tablas$cl <- tb4
  })
  # renderizando tabla clasificada
  output$tableEnd <- renderUI({
    list(dsHot("dataTable", data = tablas$cl))
  })
  
  
  # # descargas
  # output$downOptions <- renderUI({
  #   htmltools::tagList(div(downloadButton("img_png", "png", class = "buttonDown"),
  #                          downloadButton("img_jpeg", "jpeg", class = "buttonDown"),
  #                          downloadButton("img_svg", "svg", class = "buttonDown"),
  #                          downloadButton("img_pdf", "pdf", class = "buttonDown")))
  # })
  # 
  # tempDir <- reactive({
  #   last_ext <- input$last_btn
  #   if (is.null(last_ext)) return()
  #   dicTemp <- tempdir()
  #   n <- sample(1:10, 1)
  #   fileName <- random_name(n)
  #   x <-  list("Dir" = dicTemp,
  #              "viz_id" = fileName,
  #              "ext" = paste0(".", gsub(".+_", "", last_ext)))
  #   x
  # })
  # 
  # observe({
  #   map(c("img_png", "img_jpeg", "img_svg", "img_pdf"), function(z) {
  #     output[[z]] <- downloadHandler(filename = function() {paste0(tempDir()$Dir, tempDir()$viz_id, tempDir()$ext)},
  #                                    content = function(file) {ggmagic::save_ggmagic(plot_lego$plt, file, tempDir()$ext)})
  #   })
  # })
  
}
shinyApp(ui, server)
