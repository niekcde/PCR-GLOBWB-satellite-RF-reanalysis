#function that reads subsample station q_discharge files and collects them in table
subsample_table <- function(subdataset, variables){

  pat <- paste(subdataset$grdc_no, collapse = '|')
  sub_filenames <- filenames[grep(pat, filenames)]
  
  registerDoParallel(cores=2)
  sub_datas  <- foreach(i=1:length(sub_filenames)) %dopar% 
    (vroom(sub_filenames[i], show_col_type=F) %>% 
       mutate(id = str_sub(sub_filenames[i], end = -5, start = 63)))
  
   sub_table <- do.call(rbind, sub_datas) %>% select(datetime, all_of(variables), id) %>% na.omit()
  
  return(sub_table)
}

