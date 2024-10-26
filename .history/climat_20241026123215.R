# This script performs data analysis on climate data for various regions.


setwd("C:/Users/djama/OneDrive - Université Laval/Documents") ##indiquer le chemin d'accès
getwd()## indiquer le reportoire actuel 

## Importer la base de donnee 
library(readxl)
climat_regions <- read_excel("litterarture/climat_regions.xlsx")
View(climat_regions)

data <- climat_regions
View(data)

install.packages("dplyr")
library(dplyr)
install.packages("summarytools")
library(summarytools)

########################## Statistiques descriptives ################


# Statistiques descriptives par région
summary_data <- data %>%
  group_by(Regions) %>%
  summarise(
    moyenne_temperature1 = mean(Tmax, na.rm = TRUE), 
    moyenne_temperature2 = mean(Tmean, na.rm = TRUE),
    moyenne_temperature3 = mean(Tmin, na.rm = TRUE),
    moyenne_precipitation1 = mean(Pmax, na.rm = TRUE),
    moyenne_precipitation2 = mean(Pmean, na.rm = TRUE),
    moyenne_precipitation3 = mean(Pmin, na.rm = TRUE),
   
    sd_temperature1 = sd(Tmax, na.rm = TRUE),
    sd_temperature2 = sd(Tmean, na.rm = TRUE),  
    sd_temperature3 = sd(Tmin, na.rm = TRUE),   
    sd_precipitation1 = sd(Pmax, na.rm = TRUE),
    sd_precipitation2 = sd(Pmean, na.rm = TRUE),
    sd_precipitation3 = sd(Pmin, na.rm = TRUE),
    
    
  )

# Afficher les résultats
print(summary_data)
View(summary_data)
summary(summary_data)



################################ Tendance #################################


###pacman::p_load(texreg)

####htmlreg(list('1' = PSUM),
###digits = 3, float.pos = "hbt", label = "tab:risktr", single.row = TRUE,
###        caption = "Iniquity")

##htmlreg(list('Mixed logit' = mx.rt),
##        digits = 3, float.pos = "hbt", label = "tab:risktr", single.row = TRUE,
##        caption = "Iniquity")


###### Regression


# Transformer les données en format approprié
data$Annee <- as.numeric(data$Annee)

# Initialiser une liste pour stocker les résultats
results <- list()

# Appliquer la régression linéaire pour chaque région
regions <- unique(data$Regions)

for (region in regions) {
  # Filtrer les données pour la région actuelle
  region_data <- filter(data, Regions == region)
  
  # Appliquer la régression linéaire pour chaque variable en fonction des années
  lm_tmax <- lm(Tmax ~ Annee, data = region_data)
  lm_tmean <- lm(Tmean ~ Annee, data = region_data)
  lm_tmin <- lm(Tmin ~ Annee, data = region_data)
  lm_pmax <- lm(Pmax ~ Annee, data = region_data)
  lm_pmean <- lm(Pmean ~ Annee, data = region_data)
  lm_pmin <- lm(Pmin ~ Annee, data = region_data)
  
  # Stocker les résultats
  results[[region]] <- list(
    Tmax = summary(lm_tmax),
    Tmean = summary(lm_tmean),
    Tmin = summary(lm_tmin),
    Pmax = summary(lm_pmax),
    Pmean = summary(lm_pmean),
    Pmin = summary(lm_pmin)
  )
}

# Afficher les résultats pour chaque région
for (region in names(results)) {
  cat("Résultats pour la région:", region, "\n")
  cat("Tmax:\n")
  print(results[[region]]$Tmax)
  cat("Tmean:\n")
  print(results[[region]]$Tmean)
  cat("Tmin:\n")
  print(results[[region]]$Tmin)
  cat("Pmax:\n")
  print(results[[region]]$Pmax)
  cat("Pmean:\n")
  print(results[[region]]$Pmean)
  cat("Pmin:\n")
  print(results[[region]]$Pmin)
  cat("\n")
}



# Charger les bibliothèques nécessaires
library(haven)  # pour lire le fichier STATA
library(writexl)  # pour écrire dans un fichier Excel

# Lire les données depuis le fichier STATA
Ouranos_Quebec_partielle <- read_dta("C:/Users/djama/Dropbox/Dorlote M Sc/Données/Ouranos_Quebec_partielle.dta")
View(Ouranos_Quebec_partielle)


# Sauvegarder les données dans un fichier Excel
write_xlsx(Ouranos_Quebec_partielle, "C:/Users/djama/Dropbox/Dorlote M Sc/Données/Ouranos_Quebec_partielle.xlsx")



# Charger les bibliothèques nécessaires
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)


####fonction pour extraire et calculer les moyennes
extraction <- function(path,start_year,end_year,nlimit) {
  
  
  # Charger les données du fichier CSV
  data <- read_csv(path)
 
  # Convertir la colonne 'Date' en type date
  data$Date <- ymd(data$Date)
  
   data %>% 
    rename("Region" = "State/Region") 
  
  
  # Filtrer les données à partir de l'année 1990 et Extraire l'année à partir de la colonne 'Date'
  data_filtered <- data %>% 
    filter(year(Date) >= start_year & year(Date) <= end_year)%>%
    mutate(Year = year(Date))
  # Grouper les données par ville et par année, puis calculer la somme de TMAX
  grouped_data <- data_filtered %>%
    group_by(Ville, Year) %>%
    summarise(TMAX_sum = sum(TMAX, na.rm = TRUE)) %>%
    ungroup()
  
  # Réorganiser le tableau pour que les villes soient en lignes et les années en colonnes
  pivot_table <- grouped_data %>%
    pivot_wider(names_from = Year, values_from = TMAX_sum)%>%
    mutate(count= rowSums(!is.na(.)) -1) %>%
    filter(count >= nlimit)
    
  
  # Afficher les résultats
  # print(pivot_table)
  # View(pivot_table)
  
  ## recuperer
  Nomvilles = pivot_table$Ville
  
  # Filtrer les données à partir de l'année 1990
  filtreville <- data_filtered %>% 
    filter(Ville %in% Nomvilles)
  
  View(filtreville)
  #unique(filtreville$Ville)
  
  
  regions <- filtreville%>%
    group_by(Date) %>%
    summarise(
      Precipitation = mean(PRCP, na.rm = TRUE)/10,
      neige = mean(SNOW, na.rm = TRUE), 
      temperaturemax = mean(TMAX, na.rm = TRUE)/10,
      temperaturemin = mean(TMIN, na.rm = TRUE)/10,
    )
  
  #View(regions)
 return(regions)
  
}
Abitibi <- extraction(path ="litterarture/CA007_files/subset_data_Abitibi-Témiscamingue.csv",
                      start_year = 1990,end_year = 2022,nlimit = 16)


Bas_saint_laurent <- extraction(path ="litterarture/CA007_files/subset_data_Bas-Saint-Laurent.csv",
                      start_year = 1990,end_year = 2022,nlimit = 16)


Capitale_Nationale <- extraction(path ="litterarture/CA007_files/subset_data_Capitale-Nationale.csv",
                                start_year = 1990,end_year = 2022,nlimit = 16)

Centre_du_Quebec <- extraction(path ="litterarture/CA007_files/subset_data_Centre-du-Québec.csv",
                                 start_year = 1990,end_year = 2022,nlimit = 16)

Chaudiere_Appalaches <- extraction(path ="litterarture/CA007_files/subset_data_Chaudière-Appalaches.csv",
                               start_year = 1990,end_year = 2022,nlimit = 16)

Cote_Nord <- extraction(path ="litterarture/CA007_files/subset_data_Côte-Nord.csv",
                                   start_year = 1990,end_year = 2022,nlimit = 16)

Estrie <- extraction(path ="litterarture/CA007_files/subset_data_Estrie.csv",
                        start_year = 1990,end_year = 2022,nlimit = 16)
                        
Gaspesieiles_de_la_Madeleine <- extraction(path ="litterarture/CA007_files/subset_data_Gaspésie–Îles-de-la-Madeleine.csv",
                        start_year = 1990,end_year = 2022,nlimit = 16)                      
                        
Lanaudiere <- extraction(path ="litterarture/CA007_files/subset_data_Lanaudière.csv",
                     start_year = 1990,end_year = 2022,nlimit = 16)

Laurentides <- extraction(path ="litterarture/CA007_files/subset_data_Laurentides.csv",
                         start_year = 1990,end_year = 2022,nlimit = 16)

Mauricie <- extraction(path ="litterarture/CA007_files/subset_data_Mauricie.csv",
                          start_year = 1990,end_year = 2022,nlimit = 16)

Monteregie <- extraction(path ="litterarture/CA007_files/subset_data_Montérégie.csv",
                       start_year = 1990,end_year = 2022,nlimit = 16)

Montreal <- extraction(path ="litterarture/CA007_files/subset_data_Montreal (administrative region).csv",
                         start_year = 1990,end_year = 2022,nlimit = 16)

Nord_du_Québec <- extraction(path ="litterarture/CA007_files/subset_data_Nord-du-Québec.csv",
                       start_year = 1990,end_year = 2022,nlimit = 16)

Outaouais <- extraction(path ="litterarture/CA007_files/subset_data_Outaouais.csv",
                             start_year = 1990,end_year = 2022,nlimit = 16)

SaguenayLac_Saint_Jean <- extraction(path ="litterarture/CA007_files/subset_data_Saguenay–Lac-Saint-Jean.csv",
                             start_year = 1990,end_year = 2022,nlimit = 16)

base_climat <- merge(Abitibi,Bas_saint_laurent,Capitale_Nationale,Centre_du_Quebec,
            Chaudiere_Appalaches,Cote_Nord,Estrie,Gaspesieiles_de_la_Madeleine,
            Lanaudiere,Laurentides,Mauricie,Monteregie,Montreal,Nord_du_Québec,
            Outaouais,SaguenayLac_Saint_Jean)


















