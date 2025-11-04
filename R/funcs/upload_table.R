

upload_table <- function(dl, pt) {
  
  tibble(
    "file" = c("Signals", "Product tracker"),
    "Status" = c(
      ifelse(is.null(dl), "Not uploaded/parsed", "Uploaded"),
      ifelse(is.null(pt), "Not uploaded/parsed", "Uploaded")
    )
  )
  
  
}