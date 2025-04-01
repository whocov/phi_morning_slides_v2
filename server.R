options(shiny.maxRequestSize=30*1024^2)

server <- function(input, output, session) {
  
  input_file_dl <- reactive({
    
    if (is.null(input$upload_excel_dl)) return(NULL)
    
    
    x <- tryCatch(
      read_excel(input$upload_excel_dl$datapath),
      error = function(error_message) {
        print("FAILED TO READ FILE")
        NULL
      })
    
    print(input$upload_excel_dl)
    
    x
    
  })
  
  
  input_file_sig_detail <- reactive({
    
    if (is.null(input$upload_excel_dl2)) return(NULL)
    
    sig.detail0 <- readxl::read_xlsx(input$upload_excel_dl2$datapath,
                                     sheet = 'Signal characterization rationa')
    
    x <- tryCatch(
      readxl::read_xlsx(input$upload_excel_dl2$datapath,
                        col_types = c('date', rep('text', times=(ncol(sig.detail0)-1))),
                        sheet = 'Signal characterization rationa') %>%
        clean_names() %>%
        filter(!is.na(hazard)) %>%
        mutate(date = as.Date(date),
               hazard.country = paste(hazard, country, sep=', ')),
      
      error = function(error_message) {
        print("FAILED TO READ FILE2")
        NULL
      })
    
    # print(input$upload_excel_dl2)
    
    x
    
  })
  
  
  # INPUT Product Tracker
  input_file_pt <- reactive({
    
    x <- tryCatch(
      read_file_pt(input$upload_excel_pt, sheets = "product tracker"),
      error = function(error_message) {
        print("FAILED TO READ PT FILE")
        NULL
      })
    
    if (is.null(x)) return(NULL)
    x <- tryCatch(
      proc_file_pt(x[[1]]),
      error = function(error_message) {
        print("FAILED TO PROCESS PT FILE")
        NULL
      })
    
    if (is.null(x)) return(NULL)
    
    x
    
  })
  
  
  # Input Tracker Reporter
  input_file_pt2 <- reactive({
    
    x <- tryCatch(
      read_file_pt2(input$upload_excel_pt2),
      error = function(error_message) {
        print("FAILED TO READ PT2 FILE")
        NULL
      })
    
    if (is.null(x)) return(NULL)
    print(x)
    x
  })
  
  
  # Filter signals
  signals_fil <- reactive({
    
    if (is.null(input_file_dl())) {
      return(NULL)
    }
    
    x <- tryCatch(
      proc_file_dl(input_file_dl()),
      error = function(error_message) {
        print("FAILED TO PROCESS FILE")
        NULL
      })
    
    x <- signal_filter_date(x, input$as_of_date) 
    
    if (is.null(x)) return(NULL)
    
    if (nrow(x) == 0) return(NULL)
    
    x
  })
  
  
  
  # Filter triaged signals
  signals_fil2 <- reactive({
    
    if (is.null(input_file_dl())) {
      return(NULL)
    }
    
    x <- tryCatch(
      triaged_signal_clean(input_file_dl()),
      error = function(error_message) {
        print("FAILED TO PROCESS FILE")
        NULL
      })
    
    x <- signal_filter_date(x, input$as_of_date) 
    
    if (is.null(x)) return(NULL)
    
    if (nrow(x) == 0) return(NULL)
    
    x
  })
  
  
  
  # Product Tracker
  product_fil <- reactive({
    if (is.null(input_file_pt())) {
      return(NULL)
    }
    
    x <- input_file_pt()
    
    if (is.null(x)) return(NULL)
    
    if (nrow(x) == 0) return(NULL)
    
    x
  })
  
  # Tracker reporter
  product_fil2 <- reactive({
    if (is.null(input_file_pt2())) {
      return(NULL)
    }
    
    x <- input_file_pt2()
    
    if (is.null(x)) return(NULL)
    
    if (nrow(x) == 0) return(NULL)
    
    x
  })
  
  # --------------------------------------------------------------------------------------------------------------------
  # Show date after the signals and the product tracker are entered
  observe({
    if (!is.null(input_file_dl()) & !is.null(input_file_pt())) {
      max_date <- as_date(Sys.Date()) #as_date(max(input_file_dl()$modified_date, na.rm = T))
      updateDateInput(session, inputId = "as_of_date", value = max_date)
      
      shinyjs::showElement("show_when_successful_ul")
    }
  })
  
  
  # Upload tables indicator
  output$upload_table <- renderDT({
    
    tab <- upload_table(input_file_dl(), input_file_pt())
    
    DT::datatable(tab,
                  options = list(paging = FALSE, searching = FALSE, lengthChange = FALSE)) %>% 
      {if(!is.null(.)) {
        formatStyle(
          .,
          "Status",
          backgroundColor = styleEqual(c("Not uploaded/parsed", "Uploaded"), c('#fddbc7', '#d1e5f0'))
        )} else .}
    
    
  })
  
  
  
  output$signals_table <- renderDT({
    
    if (!is.null(signals_fil())) {
      out <- signals_fil() %>% 
        select(created_date, modified_date, region, countries_areas, title)
    } else {
      out <- NULL
    }
    
    DT::datatable(out,
                  options = list(paging = TRUE, searching = TRUE, lengthChange = FALSE)) 
  })
  
  
  output$preview_table <- DT::renderDataTable(
    DT::datatable(signals_fil(),
                  options = list(scrollX = TRUE,
                                 columnDefs = list(list(
                                   targets = 11,
                                   render = JS(
                                     "function(data, type, row, meta) {",
                                     "return type === 'display' && data.length > 6 ?",
                                     "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
                                     "}")
                                 ))), 
                  callback = JS('table.page(3).draw(false);'))
  )
  
  
  # --------------------------------------------------------------------------------------------------------------------
  # OUTPUT SLIDES
  output$dl_pres <- downloadHandler(filename = function() {
    paste0("phi_am_slides_for_", Sys.Date(), ".pptx")
  }, content = function(file) {
    
    shinyalert::shinyalert(
      title = "Preparing Slides...",
      text = "\nThis may take several minutes",
      closeOnClickOutside = FALSE,
      showConfirmButton = FALSE,
      showCancelButton = FALSE,
      imageUrl = "https://upload.wikimedia.org/wikipedia/commons/7/7a/Ajax_loader_metal_512.gif",
      closeOnEsc = FALSE)
    
    tester <- signals_fil()
    slides <- slide_run(sig = signals_fil(), 
                        sig.detail = input_file_sig_detail(), 
                        # sig.olympic = input_file_sig_olympic(), 
                        prod = product_fil(),
                        prod2 = product_fil2(),
                        pres = pres, 
                        eios_num = input$eios_info_num, 
                        inbox_num = input$inbox_info_num,
                        signals_num = input$signals_info_num)
    
    shinyjs::runjs("swal.close();")
    
    
    print(slides, file)
    
    rm(list=ls())
  })
  
}



