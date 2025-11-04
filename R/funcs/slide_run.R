


slide_run <- function(sig,
                      sig.detail = NULL,
                      # sig.olympic = NULL,
                      prod = NULL,
                      prod2 = NULL,
                      pres = read_pptx(here::here("data", "template", "slide_template_new.pptx")),
                      eios_num = 0,
                      inbox_num = 0,
                      signals_num = 0) {

  # -----------------------------------------------------------------------------
  # pres <-  read_pptx(here::here("data", "template", "slide_template_new.pptx"))
  
  pres <- slide_title(pres, sig)
  print("1")
  # slide 1 -----------------------------------------------------------------
  # overview of number of signals
  pres <- pres %>% 
    # add intro slide
    slide_intro(sig, eios_num, inbox_num, signals_num)
  print("2")
  
  # slide 2 - signals  -----------------------------------------
  pres <- pres %>%
    slide_signals_new(., sig.detail)
  print("3")
  
  # slide 3 - signals in more depth -----------------------------------------
  pres <- pres %>%
    slide_signals_rtm(., sig.detail)
  
  print("4")
  
  # slide 4 -----------------------------------------
  pres <- pres %>%
    slide_signals(sig)
  print("5")
  
  # slide 5 - product tracker -----------------------------------------
  if (!is.null(prod)) {
    pres <- pres %>%
      # add intro slide
      slide_products(prod, prod2)
  }
  print("6")
  
  return(pres)

}

