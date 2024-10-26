library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Define the processing function
process_and_save_data <- function(file_path, prefix, save_name) {
  # Load the data
  print("Loading the data...")
  subset_data <- read_csv(file_path)
  
  # Remove unwanted columns if they exist
  cols_to_remove <- c("Level", "gqid", "id", "shapeGroup", "shapeType", "asdf_id")
  subset_data <- subset_data %>%
    select(-any_of(cols_to_remove))
  
  # Reorder columns to have shapeName first, if it exists
  if ("shapeName" %in% names(subset_data)) {
    subset_data <- subset_data %>%
      select(shapeName, everything())
  }
  
  # Transpose and format data
  print("Transposing and formatting the data...")
  temp_df <- as.data.frame(t(subset_data))
  colnames(temp_df) <- temp_df["shapeName",]
  temp_df <- temp_df[-1, ]
  temp_df <- rownames_to_column(temp_df, var = "Variabe_name")
  
  # Function to process the data based on prefix
  process_data <- function(prefix, df) {
    cols <- grep(paste0("^", prefix, "_"), df$Variabe_name, value = TRUE)
    if (length(cols) == 0) return(NULL)
    
    period <- unlist(str_extract_all(cols, "(?<=\\.)\\d+(?=\\.)"))
    variable <- unlist(str_extract(cols, "(?<=\\.)[^.]+$"))
    type <- unlist(str_extract(cols, prefix))
    
    if (prefix == "udel") {
      method_calcul <- unlist(str_extract(cols, "(?<=v501_)[^.]+(?=\\.\\d{4})"))
      parameter <- unlist(str_extract(cols, "(?<=udel_)[^*]+(?=_v501)"))
      df_subset <- df[grep(paste0("^", prefix, "_"), df$Variabe_name), ]
      result <- cbind(period, type, method_calcul, parameter, variable, df_subset)
      
    } else if (prefix == "oco2") {
      parameter <- "co2"
      df_subset <- df[grep(paste0("^", prefix, "_"), df$Variabe_name), ]
      result <- cbind(period, type, parameter, variable, df_subset)
      
    } else if (prefix == "cru") {
      method_calcul <- unlist(str_extract(cols, "(?<=yearly_|monthly_)[^.]+(?=\\.[0-9]+)"))
      parameter <- unlist(str_extract(cols, "(?<=cru_ts_407_)[^_]+(?=_(yearly|monthly))"))
      df_subset <- df[grep(paste0("^", prefix, "_"), df$Variabe_name), ]
      result <- cbind(period, type, method_calcul, parameter, variable, df_subset)
      
    } else if (prefix == "globalwindatlas") {
      df_subset <- df[grep("^globalwindatlas", df$Variabe_name), ]
      parameter <- "windspeed"
      result <- cbind(type, parameter, variable, df_subset)
      
    } else {
      result <- NULL
    }
    
    return(result)
  }
  
  # Process the data for the given prefix
  processed_data <- process_data(prefix, temp_df)
  
  if (is.null(processed_data)) {
    stop("No data found for the given prefix")
  }
  
  # Further processing
  print("Performing further processing...")
  processed_data$parameter <- gsub("\\bpre\\b", "P", processed_data$parameter)
  processed_data$parameter <- gsub("\\btmp\\b", "T", processed_data$parameter)
  
  processed_data$time_step <- ifelse(nchar(processed_data$period) == 4, "yearly", "monthly")
  processed_data$year <- as.numeric(substr(processed_data$period, 1, 4))
  processed_data$month <- as.numeric(substr(processed_data$period, 5, 6))
  
  # Pivot and summarize data
  print("Pivoting and summarizing the data...")
  
  processed_data <- tidyr::pivot_longer(processed_data, 
                                        cols = -c(year, month, type, parameter, variable, Variabe_name, period, method_calcul, time_step), 
                                        names_to = "Province", values_to = "Value")
  
  
  processed_data$Value <-as.numeric(processed_data$Value)
  
  print("Pivoting step 1 and summarizing the data...")
  # Pivot the data to wide format
  pivoted_data <- processed_data %>%
    group_by(year, month, Province) %>%
    pivot_wider(
      names_from = c(parameter, variable),
      values_from = Value,
      names_sep = ""
    ) %>%
    ungroup()
  # Inspect the column names
  
  print("Pivoting step 2 and summarizing the data...")
  # Now select the correct columns
  pivoted_data <- pivoted_data %>%
    select(year, month, Province, Tmin = Tmin, Tmax = Tmax, Tmean = Tmean, Pmin = Pmin, Pmax = Pmax, Pmean = Pmean) %>%
    group_by(year, month, Province) %>%
    summarize(across(everything(), ~ mean(.x, na.rm = TRUE), .names = "{.col}"))
  
  # Save the result
  save_path <- paste0(save_name, ".csv")
  write.csv(pivoted_data, save_path, row.names = FALSE, fileEncoding = "latin1")
  
  # Print success message
  print(paste("File successfully created:", save_path))
}

# usage, olny change the location of dataframe
process_and_save_data("D:/R_project/Econometrie1/66d3d5c23be7857db60c96a2_results.csv", "cru", "usa")
