


slide_run <- function(sig,
                      sig.detail = NULL,
                      # sig.olympic = NULL,
                      prod = NULL,
                      prod2 = NULL,
                      pres = read_pptx(here::here("data", "template", "slide_template.pptx")),
                      eios_num = 0,
                      inbox_num = 0,
                      signals_num = 0) {

  # -----------------------------------------------------------------------------
  pres <-  read_pptx(here::here("data", "template", "slide_template.pptx"))
  
  pres <- slide_title(pres, sig)

  # slide 1 -----------------------------------------------------------------
  # overview of number of signals
  pres <- pres %>% 
    # add intro slide
    slide_intro(sig, eios_num, inbox_num, signals_num)

  # slide 2 - signals  -----------------------------------------
  pres <- pres %>%
    slide_signals_new(., sig.detail)

  # slide 3 - signals in more depth -----------------------------------------
  pres <- pres %>%
    slide_signals_rtm(., sig.detail)
  
  
  # slide 4 -----------------------------------------
  pres <- pres %>%
    slide_signals(sig)
  
  # slide 5 - product tracker -----------------------------------------
  if (!is.null(prod)) {
    pres <- pres %>%
      # add intro slide
      slide_products(prod, prod2)
  }
  
  
  return(pres)

}

