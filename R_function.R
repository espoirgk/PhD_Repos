library(readr)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(sf)
library(stringi)
Wedataset <- read_csv("D:/R_project/Econometrie1/6615b1142fd4c85a3f67d942_results.csv")
colon_variables <-colnames(Wedataset)


# Création de la liste des régions du Québec
regions_quebec <- c(
  "Abitibi-Témiscamingue",
  "Bas-Saint-Laurent",
  "Côte-Nord",
  "Capitale-Nationale",
  "Centre-du-Québec",
  "Chaudière-Appalaches",
  "Estrie",
  "Gaspésie-Îles-de-la-Madeleine",
  "Lanaudière",
  "Laurentides",
  "Laval",
  "Mauricie",
  "Montérégie",
  "Montréal",
  "Nord-du-Québec",
  "Outaouais",
  "Saguenay-Lac-Saint-Jean"
)

name_a_corriger <- c("Abitibi-TÃ©miscamingue" , "Centre-du-QuÃ©bec", "LanaudiÃ¨re","MontÃ©rÃ©gie",
                     "MontrÃ©al", "Nord-du-QuÃ©bec","Saguenay--Lac-Saint-Jean","Outaouais","Mauricie" ,
                     "Estrie" ,"Bas-Saint-Laurent","Laval","Capitale-Nationale","Laurentides",
                     "ChaudiÃ¨re-Appalaches", "CÃ´te-Nord", "GaspÃ©sie--Ã\u008eles-de-la-Madelei"
                     )



# Ranger name_a_corriger en ordre alphabétique
name_a_corriger <- sort(name_a_corriger)

# Créer un data.frame pour stocker les paires à corriger
paires_a_corriger <- data.frame(correction = regions_quebec, erreur = name_a_corriger)

# Parcourir chaque paire pour corriger les noms dans le dataframe
for (i in 1:nrow(paires_a_corriger)) {
  old_name <- paires_a_corriger[i, "erreur"]
  new_name <- paires_a_corriger[i, "correction"]
  Wedataset$shapeName <- gsub(old_name, new_name, Wedataset$shapeName)
}


# Fonction pour extraire les lignes correspondant aux régions du Québec
regions_quebec_pattern <- paste(regions_quebec, collapse = "|")
subset_quebec <- Wedataset[grep(regions_quebec_pattern, Wedataset$shapeName), ]

#verifier
length(unique(subset_quebec$shapeName))

# Vérifier si les colonnes existent dans le dataframe
cols_to_remove <- c("Level", "gqid", "id", "shapeGroup", "shapeType", "asdf_id")
cols_to_remove <- intersect(cols_to_remove, names(subset_quebec))

# Supprimer les colonnes si elles existent
if (length(cols_to_remove) > 0) {
  subset_quebec <- subset_quebec %>% select(-all_of(cols_to_remove))
}

# Vérifier si la colonne shapeName existe dans le dataframe
if ("shapeName" %in% names(subset_quebec)) {
  # Réorganiser les colonnes pour mettre shapeName en première position
  subset_quebec <- subset_quebec %>% 
    select(shapeName, everything())
}


#formatage base de données
temp_df = t(subset_quebec)
colnames(temp_df) = temp_df["shapeName",]
temp_df <- as.data.frame(temp_df, col.names = names(temp_df["shapeName",]))
temp_df <- temp_df[!rownames(temp_df) == "shapeName", ][-1, ]
temp_df <- rownames_to_column(temp_df)%>% rename(Variabe_name = rowname)


############
# #test
# #^udel_
# udel_cols <- grep("^udel_", temp_df$Variabe_name, value = TRUE)
# # 2. Extraire les années et méthodes des noms de colonnes
# period <- unlist(str_extract_all(udel_cols, "(?<=\\.)\\d+(?=\\.)"))
# variable <- unlist(str_extract(udel_cols, "(?<=\\.)[^.]+$"))
# method_calcul <- unlist(str_extract(udel_cols, "(?<=v501_)[^.]+(?=\\.\\d{4})"))
# parameter <- unlist(str_extract(udel_cols, "(?<=udel_)[^*]+(?=_v501)"))
# type <-unlist(str_extract(udel_cols, "udel"))
# 
# udel_subset_quebec <- temp_df[grep("^udel_", temp_df$Variabe_name), ]
# 
# combined_df <- cbind(period, type, method_calcul, parameter, variable, udel_subset_quebec)
# 
# #^oco2_
# oco2_cols <- grep("^oco2_", temp_df$Variabe_name, value = TRUE)
# period <- unlist(str_extract_all(oco2_cols, "(?<=\\.)\\d+(?=\\.)"))
# variable <- unlist(str_extract(oco2_cols, "(?<=\\.)[^.]+$"))
# type <-unlist(str_extract(oco2_cols, "oco2"))
# parameter <-unlist(str_extract(oco2_cols, "co2"))
# oco2_subset_quebec <- temp_df[grep("^oco2_", temp_df$Variabe_name), ]
# 
# combined_df2 <- cbind(period, type, parameter, variable, oco2_subset_quebec)
# 
# 
# #^cru_
# cru_cols <- grep("^cru_", temp_df$Variabe_name, value = TRUE)
# period <- unlist(str_extract_all(cru_cols, "(?<=\\.)\\d+(?=\\.)"))
# variable <- unlist(str_extract(cru_cols, "(?<=\\.)[^.]+$"))
# type <-unlist(str_extract(cru_cols, "cru"))
# method_calcul <- unlist(str_extract(cru_cols, "(?<=yearly_|monthly_)[^.]+(?=\\.[0-9]+)"))
# parameter <-unlist(str_extract(cru_cols, "(?<=cru_ts_407_)[^_]+(?=_(yearly|monthly))"))
# cru_subset_quebec <- temp_df[grep("^cru_", temp_df$Variabe_name), ]
# 
# combined_df3 <- cbind(period, type, method_calcul, parameter, variable, cru_subset_quebec)
# 
# 
# #^globalwindatlas
# 
# globalwind_cols <- grep("^globalwindatlas", temp_df$Variabe_name, value = TRUE)
# variable <- unlist(str_extract(globalwind_cols, "(?<=\\.)[^.]+$"))
# type <-unlist(str_extract(globalwind_cols, "globalwindatlas"))
# parameter <-unlist(str_extract(globalwind_cols, "windspeed"))
# 
# wind_subset_quebec <- temp_df[grep("^globalwindatlas", temp_df$Variabe_name), ]
# 
# combined_df4 <- cbind(type,parameter, variable, wind_subset_quebec)
# 
# merged_df <- merge(combined_df, combined_df2, all = TRUE)
# 
# merged_df <- merge(merged_df, combined_df3, all = TRUE)
# merged_df <- merge(merged_df, combined_df4, all = TRUE)
# 
# # Replace "pre" with "precip" and "tmp" with "air_temp" in the "parameter" column

##################PREPROCESSING####################################################

# Function to process data for different prefixes
process_data <- function(prefix, df) {
  cols <- grep(paste0("^", prefix, "_"), df$Variabe_name, value = TRUE)
  if (length(cols) == 0) {
    return(NULL)
  }
  
  period <- unlist(str_extract_all(cols, "(?<=\\.)\\d+(?=\\.)"))
  variable <- unlist(str_extract(cols, "(?<=\\.)[^.]+$"))
  type <- unlist(str_extract(cols, prefix))
  
  if (prefix == "udel") {
    method_calcul <- unlist(str_extract(cols, "(?<=v501_)[^.]+(?=\\.\\d{4})"))
    parameter <- unlist(str_extract(cols, "(?<=udel_)[^*]+(?=_v501)"))
    df_subset <- df[grep(paste0("^", prefix, "_"), df$Variabe_name), ]
    
    result <- cbind(period, type, method_calcul, parameter, variable, df_subset)
  } else if (prefix == "oco2") {
    parameter <-unlist(str_extract(cols, "co2"))
    df_subset <- df[grep(paste0("^", prefix, "_"), df$Variabe_name), ]
    
    result <- cbind(period, type, parameter, variable, df_subset)
  } else if (prefix == "cru") {
    method_calcul <- unlist(str_extract(cols, "(?<=yearly_|monthly_)[^.]+(?=\\.[0-9]+)"))
    parameter <-unlist(str_extract(cols, "(?<=cru_ts_407_)[^_]+(?=_(yearly|monthly))"))
    df_subset <- df[grep(paste0("^", prefix, "_"), df$Variabe_name), ]
    
    result <- cbind(period, type, method_calcul, parameter, variable, df_subset)
  } else if (prefix == "globalwindatlas") {
    df_subset <- df[grep("^globalwindatlas", df$Variabe_name), ]
    parameter <-unlist(str_extract(cols, "windspeed"))
    result <- cbind(type, parameter, variable, df_subset)
  } else {
    result <- NULL
  }
  
  return(result)
}

# Apply function for different prefixes
combined_df <- process_data("udel", temp_df)
combined_df2 <- process_data("oco2", temp_df)
combined_df3 <- process_data("cru", temp_df)
combined_df4 <- process_data("globalwindatlas", temp_df)

# Merge dataframes
merged_df <- merge(combined_df, combined_df2, all = TRUE)
merged_df <- merge(merged_df, combined_df3, all = TRUE)
merged_df <- merge(merged_df, combined_df4, all = TRUE)

# Replace "pre" with "precip" only when it's a standalone word
merged_df$parameter <- gsub("\\bpre\\b", "precip", merged_df$parameter)

# Replace "tmp" with "air_temp" only when it's a standalone word
merged_df$parameter <- gsub("\\btmp\\b", "air_temp", merged_df$parameter)

# Create new columns time_step and year based on the period column
merged_df$time_step <- ifelse(nchar(merged_df$period) == 4, "yearly", "monthly")
merged_df$year <- substr(merged_df$period, start = 1, stop = 4)


merged_df <- tidyr::pivot_longer(merged_df, cols = -c(year, type, parameter, variable, Variabe_name, period, method_calcul, time_step), names_to = "Province", values_to = "Value")

merged_df$Value <-as.numeric(merged_df$Value)
merged_df$year <- as.numeric(as.character(merged_df$year))

# Write merged dataframe to a CSV file
write.csv(merged_df, "baseline_Weather_data.csv", row.names = FALSE, fileEncoding = "latin1")

##############################PLOTS##################################################
df = merged_df
#cas1: udel air_temp => min max mean  precip => min max mean sum
# Subset data for "udel" type and "precip" parameter


# PLOTS 1
plot_measurement <- function(dfs, type, parameter, y_label) {
  dfs <- dfs[dfs$type == type & dfs$parameter == parameter, ]
  
  # Set aesthetics based on parameter
  if (parameter == "windspeed") {
    aes <- aes(x = Province, y = Value, color = variable, group = variable)
    facet <- NULL
  } else if (parameter == "co2") {
    aes <- aes(x = year, y = Value, color = variable, group = variable)
    facet <- facet_wrap(facets = vars(Province))
  } else {
    aes <- NULL
    facet <- NULL
  }
  
  ggplot(dfs, aes) +
    geom_line() +
    labs(title = ifelse(parameter == "windspeed",
                        "Valeurs Min, Max et Moyennes par Province de la Vitesse du vent",
                        "Valeurs Min, Max et Moyennes par Province du CO2 au fil des années"),
         x = ifelse(parameter == "windspeed", "Province", "Année"),
         y = ifelse(parameter == "windspeed",
                    "Vitesse du vent (m/s)",
                    "Concentration (ppm)"),
         color = NULL) +  # Remove legend title
    facet +  # Add facet if not NULL
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Plot windspeed graph
data_long <- df[df$type == "globalwindatlas" & df$parameter == "windspeed", ]
plot_measurement(data_long, type = "globalwindatlas", parameter = "windspeed")

# Plot CO2 concentration graph
data_long <- df[df$type == "oco2" & df$parameter == "co2", ]
plot_measurement(data_long, type = "oco2", parameter = "co2")




# PLOTS 2
plot_weather <- function(dfs, type, parameter, vartype = "no_sum") {
  
  if (type == "cru"){
    dfs <- dfs[dfs$type == type & dfs$parameter == parameter, ]
    facet <- facet_wrap(facets = vars(Province))
  }
  else 
    if (type == "udel" & vartype == "no_sum") {
      dfs <- dfs[dfs$type == type & dfs$parameter == parameter & dfs$variable != "sum", ]
      facet <- facet_wrap(facets = vars(Province))
    }
    else {
      dfs <- dfs[dfs$type == type & dfs$parameter == parameter & dfs$variable == "sum", ]
      facet <- NULL
    }
  
  # Set title based on parameter
  if (parameter == "air_temp") {
    title <- "Valeurs Min, Max et Moyennes par Province de la Température"
    y_label <- "Température (°C)"
  } else if (parameter == "precip") {
    title <- "Valeurs Min, Max et Moyennes par Province de la Précipitation"
    y_label <- "Précipitation (mm)"
  } else {
    title <- "Unknown Parameter"
  }
  
p <- ggplot(dfs, aes(x = year, y = Value, color = variable, group = variable)) +
    geom_line() +
    facet +
    labs(title = paste(title, "-", type),
         x = "Année",
         y = y_label,
         color = NULL) +  # Remove legend title
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  if (type == "cru" | vartype == "no_sum") {
    p <- p + scale_x_continuous(breaks = seq(min(dfs$year), max(dfs$year), by = 16))
  }
  return(p)
}

# Plot temperature graph
plot_weather(df, type = "cru", parameter = "precip", vartype = "no_sum")
plot_weather(df, type = "cru", parameter = "air_temp", vartype = "no_sum")

# Plot precipitation graph
plot_weather(df, type = "udel", parameter = "precip", vartype = "no_sum")
plot_weather(df, type = "udel", parameter = "air_temp", vartype = "no_sum")

plot_weather(df, type = "udel", parameter = "precip", vartype = "sum")








