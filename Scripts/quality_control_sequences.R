#the function takes in a data frame containing all taxa outliers. 
#For each taxa (Query) it checks if its a bacteria
# df is the dataframe with the outlier taxa and taxonomic info
quality_control <- function(df) {
  Query = unique(df$Query)
  # Initiate a result column
  quality_control_df = data.frame(Query = Query, Decition = 0)
  
  cat("\n")
  cat("\033[31mPress ESC to cancel\033[0m\n")
  Sys.sleep(1)
  cat("\033[31mFor each taxa decide if its an outlier\033[0m\n")
  Sys.sleep(1)
  cat("\033[32mGood luck! ~ Madeleine :-) \033[0m\n")
  cat("\n")
  Sys.sleep(1)
    
  # Iterate for all Queries
  for (i in seq_along(Query)) {
  Query_sub = Query[i]
  # filter for rows with the given query
  query_df <- subset(df, Query == Query_sub)
  query_df = query_df %>% dplyr::mutate(taxonomy = paste0(substr(taxonomy, start = 1, stop = 50), "..."))
  # subset dataframe
  unique_query_df = query_df[, c("Query", "Organism", "taxonomy")]
  
  # check if all unique rows have "Bacteria" as the first 8 characters of the taxonomy
  tax_unique <- unique(substr(unique_query_df$taxonomy, start = 1, stop = 8))
  if (length(tax_unique) == 1 && grepl("Bacteria", tax_unique)) {
    cat(sprintf("Query: %s is a bacteria. Keeping it.\n", Query_sub))
    cat("\n")
    quality_control_df[i,2] = "KEEP"
    
  } else {
    # display the unique rows to the user
    cat(sprintf("Query: %s\n", Query_sub))
    print(unique_query_df)
    
    # ask the user if they want to keep the query
    keep_query <- readline(sprintf("Do you want to keep query %s? (yes or no) ", Query_sub))
    
    # return the query and whether or not to keep it
    if (tolower(keep_query) == "yes" || tolower(keep_query) == "y") {
      cat(sprintf("\033[32mOk - then it's decided %s is a bacteria.\n\033[0m", Query_sub))
      Sys.sleep(1)
      cat("\n")
      cat("\n")
      quality_control_df[i,2] = "KEEP"
    
      } else {
      cat(sprintf("\033[31mOk - let's get rid of this intruder ;) %s has been eliminated!\033[0m\n", Query_sub))
      Sys.sleep(1)
      cat("\n")
      cat("\n")
      quality_control_df[i,2] = "OUTLIER" 
      } }  
  Sys.sleep(0.5) 
  } 
  return(quality_control_df) }  