import_sintax_file <- function (in_file = "sintax_file.txt", confidence = 0.8) {
  # Read in sintax file.
  temp <- read.table(file = in_file, sep = "\t", fill = TRUE, stringsAsFactors = FALSE)
  # Extract otu names.
  otus <- temp[ , 1]
  # Extract taxonomy with confidences.
  taxa <- temp[ , 2]
  # Modify taxonomy field.
  # Delete closing parenthesis
  taxa <- gsub(')', '', taxa)
  # Substitute commma for opening parenthesis.
  taxa <- gsub('\\(', ',', taxa)
  # Subsitute underscore for colon
  taxa <- gsub(':', '_', taxa)
  # Create data frame with ranks and confidences in separate columns.
  class.table <- matrix(data = NA, nrow = length(otus), ncol = 14)
  for (i in 1:nrow(class.table)) {
    taxa.line <- strsplit(taxa[i], ',')
    for (j in 1:14) {
      class.table[i, j] <- taxa.line[[1]][j]
    }
  }
  
  if (all(is.na(class.table[ , 13]))) {
    class.table <- class.table[ , -c(13,14)]
  }
  
  #Create a vector designating confidence columns.
  conf.col.no <- seq(from=2, to=ncol(class.table), by=2)
  
  # Convert these columns to numeric
  class.table <- as.data.frame(class.table)
  class.table[conf.col.no] <- lapply(class.table[conf.col.no], as.character)
  class.table[conf.col.no] <- lapply(class.table[conf.col.no], as.numeric)
  
  # Create a vector designating taxa columns.
  taxa.col.no <- seq(from=1, to=ncol(class.table)-1, by=2)
  
  # Convert these columns to character.
  class.table[taxa.col.no] <- lapply(class.table[taxa.col.no], as.character)
  
  #There may be NA's in some columns, so replace them first.
  # with confidence < specified confidence:
  for (i in 1:nrow(class.table)) {
    for (j in conf.col.no) {
      if (is.na(class.table[i, j])) {
        class.table[i, j] <- confidence/2
      }
    }
  }
  
  # Replace IDs where domain is unidentfied.
  for (i in  1:nrow(class.table)) {
    if (class.table[i, 2] < confidence) {
      class.table[i, 2] <- 1
      class.table[i, 1] <- "uncl_Domain"
    }
  }
  
  # Replace IDs where confidence is less than specified confidence:
  col.no <- seq(from=4, to=ncol(class.table), by=2)
  for (i in 1:nrow(class.table)) {
    for (j in col.no) {
      if (class.table[i, j] < confidence) {
        class.table[i, j] <- 1
        if(substr(class.table[i, (j-3)], 1, 5)=="uncl_") {class.table[i, (j-1)] <- class.table[i, (j-3)]}
        else {class.table[i, (j-1)] <- paste("uncl_", class.table[i, (j-3)], sep="")}
      }
    }
  }
  
  # Remove confidence columns
  class.table <- class.table[ , -c(seq(from = 2, to = ncol(class.table), by = 2))]
  # Add row names and column names.
  row.names(class.table) <- otus
  taxa <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")[1:ncol(class.table)]
  colnames(class.table) <- taxa
  # Convert to phyloseq tax_table
  class.table <- as.matrix(class.table)
  class.table <- tax_table(class.table, errorIfNULL = TRUE)
  return(class.table)
}