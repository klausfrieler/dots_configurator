# Define server logic to read selected file ----
library(dplyr)
library(readxl)
library(shiny)
source("../dots_home/show_tests.R")
source("utils.R")

read_test_info("../dots_home/data/longgold_codebook_new.xlsx")
assign("dots_test_params", 
       readxl::read_xlsx("./data/dots_test_params.xlsx") %>% 
         mutate(across(rel_cronbach_alpha:sem, as.numeric)), 
       globalenv())


# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(
    h1(
      img(src = "https://s3-eu-west-1.amazonaws.com/media.dots.org/img/dots_logo_v4.png", height = "76", style = "margin-right:20px"),
      "DOTS Configurator", style = "color:#5c9dd1",             
      img(src = "https://s3-eu-west-1.amazonaws.com/media.dots.org/img/dgm_logo_v2.png", height = "49", style = "margin-left:20px")
    ), windowTitle = "DOTS Home"
  ),
  # Sidebar layout with flowLayout input and output definitions ----
  sidebarLayout(
    sidebarPanel(
      # Input: Select information ----
      shiny::radioButtons("language", "Language selection",
                          choices = c("English" = "en",
                                      "German" = "de"),
                          selected = "en"),
      width = 2
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Introduction", htmlOutput("introduction")),
                  tabPanel("Configure", 
                           inputPanel(
                             make_questionnaire_checkbox_group(dots_test_params),
                             make_all_test_select_boxes(dots_test_params,  width = "300px"),
                             cellArgs = list(
                               style = "width: auto;height: auto;margin: 20px;")
                           )),
                  tabPanel("Output", 
                           uiOutput("download_button"),
                           tableOutput("config"),
                           #tableOutput("outtest")
                  ),
                  tabPanel("Glossary", 
                           htmlOutput("glossary")
                  )
                  
      )
    )
    
  )
)

server <- function(input, output) {
  output$introduction <- renderUI({
    includeHTML("introduction.html")
  })
  
  output$config <- renderTable({
    #browser()
    get_config(input)
  }, digits = 2,
  spacing = "xs",
  hover = T,
  caption = "DOTS Configuration",
  caption.placement = "top")
  
  
  output$download_config_file <- downloadHandler(
    filename = "dots_config.csv",
    content = function(file){
      conf <- get_config(input)
      write.table(conf, file,
                  sep = ";", 
                  row.names = F,
                  fileEncoding = "UTF-8")
    })
  
  output$download_button <- renderUI({
    div(
      downloadButton("download_config_file", "Download DOTS Configuration (CSV)", 
                     style = "margin-top:25px;color:#5c9dd1;font-weight:bold"
      )
    )
  })
  
  output$glossary <- renderUI({
    static_selection_page()
  })
  
}

shinyApp(ui = ui, server = server)