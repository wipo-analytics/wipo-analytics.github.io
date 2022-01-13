pv_meta <- function(type = NULL, dest = NULL) {
  
  if(!is.null(dest)) {
    if (!dir.exists(dest)){
      dir.create(dest)
      ui_info("{ui_value(dest)} dir created")
    } else {
      ui_info("{ui_value(dest)} dir exists")
    }
  }

if(is.null(type)) {
  ui_stop('Im stuck, please use type = "application" or type = "grant"')
} 
if(isFALSE(type == "grant" || type == "application")) {
  
  ui_stop('Im stuck, please use type = "application" or type = "grant"')
}  
if(type == "grant") {
  
  raw <- read_html("https://patentsview.org/download/data-download-tables")
} 
if(type == "application") {
  
  raw <-  read_html("https://patentsview.org/download/pg-download-tables")
  
} 
  
  tbl <- raw %>% 
    html_element("#myTable") %>% 
    html_table() %>% 
    mutate(zip = str_detect(`Table Name`, "zip")) %>% 
    filter(zip == TRUE) %>% 
    select(-zip) %>% 
    clean_names() %>%
    mutate(number_of_rows = str_remove_all(number_of_rows, ",")) %>% 
    mutate(number_of_rows = str_trim(number_of_rows, side = "both")) %>% 
    mutate(number_of_rows = str_remove_all(number_of_rows, " ")) %>% 
    mutate(number_of_rows = as.numeric(number_of_rows))
  
  urls <- raw %>% 
    html_nodes("#myTable tr :nth-child(1)") %>% 
    html_attr("href") %>% 
    tibble("url" = .) %>% 
    drop_na() %>% 
    mutate(zip = str_detect(url, "zip")) %>% 
    filter(zip == TRUE) %>% 
    select(-zip) %>% 
    mutate(zip_name = basename(url)) %>% 
    separate(zip_name, into = "file_name", sep = "[.]", extra = "drop", remove = FALSE) %>% 
    mutate(download_date = Sys.Date())
  
  out <- bind_cols(tbl, urls) %>% 
    mutate(data_type = glue('{type}')) %>% 
    select(data_type, file_name, zip_name, download_date, everything())
  
  write_csv(out, glue('{dest}/readme_{type}.csv'))
    
}
