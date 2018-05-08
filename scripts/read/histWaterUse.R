readData.histWaterUse <- function(viz){

  library(tidyr)
  library(dplyr)
  files <- unzip(zipfile = viz[['location']], exdir = dirname(viz[['location']]), overwrite = TRUE)
  files <- files[-grep("metadata",files)]
  awudsOLD <- get_awuds_data(awuds.data.files = files)

  return(awudsOLD)

}

get_awuds_data <- function(awuds.data.path = NA, awuds.data.files = NA) {
  
  if ( !is.na(awuds.data.path) ) {
    if ( !dir.exists(awuds.data.path) ) stop('Did not recieve a valid path.')
    files_to_scan <- list.files(path=awuds.data.path,full.names = TRUE)
  } else if ( is.vector(awuds.data.files) ) {
    awuds.data.files.new <- gsub(", ","_", awuds.data.files)
    file.rename(awuds.data.files,awuds.data.files.new)
    
    if ( !file.exists(awuds.data.files.new[1]) || !is.vector(awuds.data.files.new) ) {
      stop('Did not get a valid file.')
    }
    
    files_to_scan <- awuds.data.files.new
  } else {
    stop('Must provide the folder where AWUDS Excel export files or dump file(s) are stored.')
  }
  
  area.names <- c("STATECOUNTYCODE","COUNTYNAME",
                  "HUCCODE","Area","USSTATEHUCCODE","HUCNAME")
  other.names <- c("STUDY","STATECODE","COUNTYCODE",
                   "YEAR","USSTATEALPHACODE","DATASETNAME","BESTAVAILABLE")
  
  for ( check_file in files_to_scan ) {
    if ( grepl('Export.*[1,2][0,9][0-9][0-5].*.xlsx', check_file) ) {
      if ( !exists('files_to_open')) files_to_open <- c()
      files_to_open <- c(files_to_open, file.path(check_file))
    } else if ( grepl('.*.txt',check_file) ) {
      if ( exists('dump_file_to_open') ) {
        stop('Found more than one dump file at the path given, only one is supported.')
      }
      dump_file_to_open <- check_file
    } else if ( grepl('.*zip',check_file) ) {
      tempFolder <- tempdir()
      
      fileNames <- unzip(check_file, exdir = tempFolder, list = TRUE)
      unzip(check_file, exdir = tempFolder)
      dump_file_to_open <- fileNames$Name
    }
  }
  
  if( !exists('dump_file_to_open') && !exists('files_to_open') ) {
    stop('No excel or dump files found.')
  }
  
  if ( exists('files_to_open') ) {
    for ( file_open in files_to_open ) {
      new_awuds_data <- parseExport(file_open, citations = TRUE)
      year<-regmatches(file_open,regexpr('[1,2][0,9][0-9][0-5]',file_open))
      if ( !exists('awuds_data') ) {
        awuds_data<-normalize_awuds_excel( new_awuds_data )
        awuds_data$YEAR <- year
        if (any(grepl("Area.Name",names(awuds_data)))){
          awuds_data <- gather_(awuds_data, "data.element","value", names(awuds_data)[!(names(awuds_data) %in% c("YEAR","Area","Area.Name"))])
        }else{
          awuds_data <- gather_(awuds_data, "data.element","value", names(awuds_data)[!(names(awuds_data) %in% c("YEAR","Area"))])
        }
      } else {
        next_awuds_data <- normalize_awuds_excel( new_awuds_data )
        next_awuds_data$YEAR<-year
        if (any(grepl("Area.Name",names(next_awuds_data)))){
          next_awuds_data <- gather_(next_awuds_data, "data.element","value", names(next_awuds_data)[!(names(next_awuds_data) %in% c("YEAR","Area","Area.Name"))])
        }else{
          next_awuds_data <- gather_(next_awuds_data, "data.element","value", names(next_awuds_data)[!(names(next_awuds_data) %in% c("YEAR","Area"))])
        }
        awuds_data <- dplyr::bind_rows(awuds_data,next_awuds_data)
      }
    }
    
    awuds_data <- unique(awuds_data)
    
    awuds_data <- tidyr::spread(awuds_data,key = data.element, value = value)
    
    
  } else {
    if(length(dump_file_to_open) > 1){
      idCols <- c(area.names,other.names)
      
      totalRows <- 0
      rowVector <- rep(NA, length(dump_file_to_open))
      
      awuds_data <- list()
      colNames <- c()
      
      for(i in dump_file_to_open){
        subData <- data.table::fread(file.path(tempFolder,i), na.strings="--", colClasses="character")
        subData <- as.data.frame(lapply(subData, function(x) {gsub("na", "NaN", x)}), stringsAsFactors=FALSE)
        subData[!(names(subData) %in% idCols)] <- lapply(subData[!(names(subData) %in% idCols)], function(x) as.numeric(x))
        subData <- gather_(subData, "data.element","value", names(subData)[!(names(subData) %in% idCols)])
        
        colNames <- c(colNames, names(subData)[!(names(subData) %in% colNames)])
        
        awuds_data <- data.table::rbindlist(list(awuds_data, subData), fill=TRUE)
        
      }
      idColNames <- names(awuds_data)[(names(awuds_data) %in% idCols)]
      
      awuds_data <- data.table::dcast(data.table::setDT(awuds_data), as.formula(paste(paste(idColNames,collapse = "+"),"~ data.element")), value.var = "value")
      awuds_data <- data.table::setDF(awuds_data)
      # awuds_data <- spread_(awuds_data, "data.element","value")
      
    } else {
      awuds_data<-data.table::fread(dump_file_to_open, na.strings="--", colClasses="character")
      awuds_data <- as.data.frame(lapply(awuds_data, function(x) {gsub("na", "NaN", x)}), stringsAsFactors=FALSE)
      for ( dataCol in names(awuds_data)[!(names(awuds_data) %in% c(area.names,other.names))]) { # Convert all data elements to numeric
        awuds_data[[dataCol]]<-as.numeric(awuds_data[[dataCol]])
      }
    }
  }
  
  
  return(awuds_data)
}

normalize_awuds_excel<-function(parseExport_out) {
  for ( awuds_category in names(parseExport_out) ) {
    if ( !exists('normalized_awuds') ) {
      normalized_awuds <- parseExport_out[[awuds_category]]
    } else {
      normalized_awuds_append <-parseExport_out[[awuds_category]]
      if (any(grepl("Area Name",names(parseExport_out[[awuds_category]])))){normalized_awuds_append <-normalized_awuds_append[,-(which(names(normalized_awuds_append)=="Area Name"))]}
      normalized_awuds <- merge(normalized_awuds, normalized_awuds_append,by='Area')
      # This works, but requires Area to be in the right order.
      # normalized_awuds <- cbind(normalized_awuds, parseExport_out[[awuds_category]][-1])
    }
  }
  names(normalized_awuds) <- make.names(names(normalized_awuds))
  return(normalized_awuds)
}

parseExport <- function(file_path, citations = FALSE){
  sheet_names <- readxl::excel_sheets(file_path)
  
  #user-specified = don't parse the metadata sheet
  user <- "Dataset list" %in% sheet_names 
  if(user){
    sheets_to_parse <- sheet_names[-which(sheet_names == "Dataset list")]
  } else {
    sheets_to_parse <- sheet_names
  }
  
  parsed_data <- lapply(sheets_to_parse, function(sheet, path, citations){
    
    if(citations){
      major_readxl <- packageVersion("readxl")
      if(major_readxl >= "1.0.0"){
        all_df <- readxl::read_excel(path, sheet, skip = 2)
      } else {
        all_df <- readxl::read_excel(path, sheet, skip = 1)
      }
      
    } else {
      all_df <- readxl::read_excel(path, sheet)
    }
    
    # remove notes that appear at bottom of reports 
    notes_pattern <- "[:digit:\\)]"
    which_rows_notes <- grep(notes_pattern, all_df[[1]])
    if(length(which_rows_notes) != 0) {
      df <- all_df[-which_rows_notes,]
      metadata <- list(Notes = as.list(unname(all_df[which_rows_notes,1])))
      attr(df, 'Notes') <- metadata
    } else {
      df <- all_df
    }
    
    df <- removeDuplicateColumns(df)
    df <- removeAllNARows(df)
    
    return(df)
  }, path = file_path, citations = citations)
  names(parsed_data) <- sheets_to_parse
  
  if(user){
    metadata <- readxl::read_excel(file_path, sheet = which(sheet_names == "Dataset list"))
    attr(parsed_data, 'Datasets') <- na.omit(metadata)
  }
  
  return(parsed_data)
}

#' @export
#' @rdname parser
#' 
#' @examples 
#' path <- system.file("extdata", package="wateRuse")
#' enteredData <- parseEnteredElements(file.path(path,"Entered-Data_2005.xlsx"))
parseEnteredElements <- function(file_path){
  all_data <- readxl::read_excel(path = file_path, sheet = 1)
  
  # format metadata from top of excel file
  population_info <- as.character(as.vector(all_data[2,1:2]))
  metadata_description <- all_data[1:5, 1]
  metadata_description[2] <- paste(population_info, collapse = " ")
  metadata_aging_counts <- all_data[1:5, c(15,16)]
  names(metadata_aging_counts) <- c('Data Aging', 'Counts')
  metadata <- list(Descriptive = metadata_description,
                   Aging_counts = metadata_aging_counts)
  
  # format actual data
  df <- readxl::read_excel(path = file_path, sheet = 1, skip = 7)
  df <- df[, which(!is.na(names(df)))] #removing columns that are all NA
  df <- removeAllNARows(df)
  
  #rename columns that have an upstream name
  names(df) <- unlist(lapply(names(df), function(orig_col_name) {
    renamed_col <- switch(orig_col_name, 
                          `Once-Through Cooling` = 'Thermoelectric: Once-Through Cooling', 
                          `Closed-Loop Cooling` = 'Thermoelectric: Closed-Loop Cooling', 
                          Instream = 'Hydroelectric: Instream', 
                          Offstream = 'Hydroelectric: Offstream')
    col_name <- ifelse(!is.null(renamed_col), renamed_col, orig_col_name)
    return(col_name)
  }))
  
  attributes(df) <- append(attributes(df), metadata)
  return(df)
}

#' @export
#' @importFrom stats complete.cases
#' @rdname parser
#' 
#' @examples 
#' path <- system.file("extdata", package="wateRuse")
#' compareData <- parseCompareData(file.path(path, "CompareData.xlsx"))
parseCompareData <- function(file_path){
  sheet_names <- readxl::excel_sheets(file_path)
  parsed_data <- lapply(sheet_names, function(sheet, path, skip){
    
    all_df <- readxl::read_excel(path, sheet)
    metadata <- na.omit(names(all_df))
    
    #grab first occurrence of completely filled row, these are real column headers
    col_names_location <- which(complete.cases(all_df))[1] 
    names(all_df) <- all_df[col_names_location, ]
    
    df <- removeAllNARows(all_df)
    df <- df[which(df[,1] != names(df)[1]),] # remove duplicated column names that appear throughout data
    
    return(df)
  }, path = file_path)
  names(parsed_data) <- sheet_names
  return(parsed_data)
}

parseDumpFiles <- function(file_path){
  read.table(file_path, header = TRUE, sep = '\t',
             quote = NULL, comment.char = "")
}

removeDuplicateColumns <- function(df){
  duplicate_columns <- which(duplicated(names(df)))
  if(length(duplicate_columns) > 0){
    df <- df[, -duplicate_columns]
  }
  return(df)
}

removeAllNARows <- function(df){
  col1 <- df[[1]]
  noNA_df <- df[!is.na(col1),]
  return(noNA_df)
}
