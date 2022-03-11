#' Title
#'
#' @param path 
#' @param meta_path 
#' @param save_as 
#' @param dest 
#'
#' @return
#' @export
#' @importFrom tools file_path_sans_ext
#' @examples
pv_import <- function(path = NULL, meta_path = NULL, save_as = NULL, dest = NULL) {
  
  # get the basename of the file from the path
  # use twice to handle tsv.zip
  
  fname <- file_path_sans_ext(path) %>% 
    file_path_sans_ext()
  
  # identify the metadata table and import
  # import the file with vroom specifying the delim and noting that the 
  # default quoting is the same as that for the patensview data as "\""
  meta_path <- readRDS(meta_path) %>% 
    dplyr::filter(file_name == fname)
  
  out <- vroom::vroom(x, delim = "\t")
  
  # validate that the imported file has the same length as the metadata
  # stop with informative message if not
  # Need to handle cases where the metapath is NULL
  
  if (is.null(dest)) {
    
    dest <- getwd()
    pprint(dest)
  
    } else {
    
    dest
  }
  
  # 
  # if(nrow(out) == meta_path$number_of_rows) {
  #   
  #   usethis::ui_done("Number of Rows Matches the expected {ui_value({meta_path$number_of_rows})} for {ui_value({bname})} in the metadata file} file")
  #   
  # } else {
  #   
  #   usethis::ui_stop("{ui_value({nrow(out)} number of rows does not match expected with vroom. Try datatable::fread()?")
  #   
  # }
  # 
  # # address case where save_as is TRUE
  # # use switch to allow user to specify the file type
  # # need a message where dest is NULL that saving to working directory
  # # and needs case for working directory
  # # DEST cannot be null at the moment  
  # 
  # if(!is.null(save_as)) {
  #   
  #   switch(
  #     save_as,
  #     rds = saveRDS(out, file = glue('{dest}/{fname}.rds')),
  #     qs = qs::qsave(out, file = glue('{dest}/{fname}.qs')),
  #     csv = readr::write_csv(out, file = glue('{dest}/{fname}.csv')),
  #     rda = save(out, file = glue('{dest}/{fname}.rda')),
  #     #parquet = arrow::write_parquet(out, sink = glue('{dest}/{fname}.parquet')), # this needs work
  #     stop("Don't know how to make bizzaro <", cls, ">", call. = FALSE)
  #   )  
  #   
  #   usethis::ui_done("saved data to {ui_value(glue('{dest}/{fname}.{save}'))}")
  #   
  # } else {
  #   
  #   out
  # }
}
