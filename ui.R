ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  
  # App title ----
  titlePanel(tags$b("PHI slide generator app"), windowTitle = "PHI slide app"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # # Input: Slider for the number of bins ----
      fileInput(inputId = "upload_excel_dl", label = h3("Upload signal list here")),
      br(),
      fileInput(inputId = "upload_excel_dl2", label = h3("Upload signal characterization")),
      br(),
      # fileInput(inputId = "upload_excel_dl3", label = h3("Upload Olympics signal characterization")),
      # br(),
      fileInput(inputId = "upload_excel_pt", label = h3("Upload product tracker here")),
      br(),
      fileInput(inputId = "upload_excel_pt2", label = h3("Upload tracker reports here (optional)")),
      br(),
      shinyjs::hidden(
        tags$div(
          id = "show_when_successful_ul",
          dateInput("as_of_date", label = h3("As of date:"), value = Sys.Date(), max = Sys.Date()), #, format = "dd MM yyyy"),
          numericInput("eios_info_num", value = eios_val, label = h3("Average EIOS articles scanned/day"), min = 0, step = 1),
          numericInput("inbox_info_num", value = inbox_val, label = h3("Average Outbreak emails scanned/day"), min = 0, step = 1),
          numericInput("signals_info_num", value = signals_val, label = h3("Signals/events triaged/assessed"), min = 0, step = 1),
          hr(),
          br(),
          downloadButton("dl_pres", label = "Download Powerpoint")
        )
      )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      h2("INSTRUCTIONS"),
      tags$ol(
        tags$li("Upload the ", tags$b("ALL SIGNALS.XLSX"), " extract file (from the signal app) with the upload signal list button."),
        tags$li("Upload", tags$b("PRODUCT TRACKER.XLSX"), " file with the upload product tracker button"),
        tags$li("Set date corresponding to the date of presentation"),
        tags$li("If different from defaults, change average EIOS articles/outbreak emails scanned per day."),
        tags$li("Download slides after processing")
      ),
      p(tags$b("NB: ensure file is not open while uploading")),
      hr(),
      # h3("CONSOLE OUTPUT: for debugging failed uploads"),
      verbatimTextOutput("console_output"),
      # insert console output
      h3("Table status:"),
      DTOutput("upload_table", width = "40%"),
      hr(),
      h3("Signals:"),
      DTOutput("signals_table")
      # # 
      # textOutput("dl_uploaded"),
      # # DT::dataTableOutput("preview_table", width = "100%"),
      # textOutput("pt_uploaded")
    )
  )
)
