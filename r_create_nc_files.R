###Read datasets


#Cette ligne verifie si les packages suivant sont installés
#sinon ces packages seront installés.
Sys.setenv(TZ='GMT')
## First specify the packages of interest
packages = c("magrittr","ncdf4", "raster", "reshape2", "readr", "tidyverse")

## Now load or install and load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

#Read the csv file
path = "D:/Thèses/Dossier_ma_These/Data/Humidity/InPuits2HumWoodTsoilHumsoil.csv"


humidity_dataset <- read_csv(path, na = "NA")


head(humidity_dataset)




#1. Create function (my_extraction_function) that extract the columns of interest in the hole dataframe

start.date = "2018-01-1 00:00:00"
end.date = "2021-12-31 23:30:00"
step.time = 1800
step.date<-seq(as.POSIXct(start.date), as.POSIXct(end.date), by = 1800)

date.index<-format(step.date,"%Y-%m-%d %H:%M")

my_extraction_function <- function(dataset, sr_expresssion){
  
  ColOI = c()
  
  for(i in 1:length(colnames(dataset))){
    if(startsWith(colnames(dataset)[i], sr_expresssion) ){
      #print(colnames(dataset)[i])
      
      # a = length(ColOI) #before
      ColOI = append(ColOI, colnames(dataset)[i])


    }
    if(i==length(colnames(dataset))){
      #return(cbind(date.index, dataset[ColOI])) #If you want to get index in date format
      return(cbind(ID = 1:nrow(dataset), dataset[ColOI]))
    }
    
  }
  
}

##Testing function my_extraction_function
##argument1: dataset: csv file from the path
##argument2: sr_expression: characters from the dataframe columns that you want to extract

#Example
SoilWVC_Test <- my_extraction_function(humidity_dataset, sr_expresssion = "SoilVWC")
# head(SoilWVC_Test)
# SoilWVC_dataset <- my_extraction_function(humidity_dataset, sr_expresssion = "SoilVWC")
# SoilT_dataset <- my_extraction_function(humidity_dataset, sr_expresssion = "SoilT")
# WoodVWC_dataset <- my_extraction_function(humidity_dataset, sr_expresssion = "WoodVWC")
# WoodT_dataset <- my_extraction_function(humidity_dataset, sr_expresssion = "WoodT")


#2. Create function that renames columns and reshape dataframe in n dimensions 
#Reshape and create matrix depending dimension of each variable


my_rename_and_melt_function <-function(dataset, new.names, meltID, lat.value, lon.value){
  if(length(colnames(dataset)) != length(new.names)){
    print(paste("Shape of dataset's columns:",length(colnames(dataset)), "is not equal to new.names:", length(new.names)))
  }
  else{
    dataset = data.frame(dataset)
    names(dataset) <- new.names
    dataset <- melt(dataset, id = meltID)
    
    dataset$variable <- as.character(dataset$variable)
    dataset <- dataset %>%
      add_column(lat = lat.value,
                 .after = meltID)
    dataset <- dataset %>%
      add_column(lon = lon.value,
                 .after = meltID)

    return(dataset[with(dataset, order(date.index, nchar(variable))),])
  }
  
}

# # ##Testing the functions
# new_names = c("ID","Wood.T1", "Wood.T2")
# # #testname = c("ID","Wood.T1", "Wood.T2", "Wood.T3", "Wood.T4") 
# melt_ID = c("ID")




#Apply my_rename_and_melt_function to previous dataset get

reshape_ID = c("date.index")
wood_temp = c("date.index","Wood.T1", "Wood.T2")
wood_vwc = c("date.index","Wood.VWC1", "Wood.VWC2")
soil_layers = c("date.index","20","40","60","80","120",
               "140","160","180","280","380","480")

lon=-17.5
lat=13.94




SoilWVC_dataset_reshape <-my_rename_and_melt_function(dataset = my_extraction_function(humidity_dataset, sr_expresssion = "SoilVWC"),
                                              new.names = soil_layers, meltID = reshape_ID, lat, lon)

SoilT_dataset_reshape <-my_rename_and_melt_function(dataset = my_extraction_function(humidity_dataset, sr_expresssion = "SoilT"),
                                              new.names = soil_layers, meltID = reshape_ID, lat, lon)

WoodVWC_dataset_reshape <-my_rename_and_melt_function(dataset = my_extraction_function(humidity_dataset, sr_expresssion = "WoodVWC"),
                                              new.names = wood_vwc, meltID = reshape_ID, lat, lon)

WoodT_dataset_reshape <-my_rename_and_melt_function(dataset = my_extraction_function(humidity_dataset, sr_expresssion = "WoodT"),
                                              new.names = wood_temp, meltID = reshape_ID, lat, lon)

###Create matrix according to n dimensions



data.distance <- seq(1,1, by=1)
##
# Data is stored for dims
data.time <- 1:length(date.index)
data.layer <-1:11
data.wood <-1:2

## Add dimensions and variables which accompany the dimensions (avoided by create_dimvar = FALSE)
dimLayer <- ncdim_def(name='layer', units='cm', longname='Soil layer in cm', vals=data.layer )

dimWoodType <- ncdim_def(name='type', units='--', longname='type of variable mesure', vals=data.wood )

# Time has the dimension time. The time dimension is the number of time
# steps, while the time variable contains the time value (in this case the
# year) of the time step. Using a year should generally be avoided because
# it's a bit ambiguous. You can find some comments on this in the
#
dimTime <- ncdim_def('time', units='semi hour', longname='Timestamp: every semi hour since 2018-01-1 00:00:00', 
                     calendar="standard", vals=data.time)

# Usually you want to add some coordinate variables like this
# We have a latitude and longitude coordinate for dimensions
# coordinate. Therefor the dimension is cross_shore. You can also say that
# latitude and longitudes are a function of the cross-shore distance.


dimLon <- ncdim_def('lon', units='degrees_east', longname='longitude', calendar="standard", vals=-17.5)

dimLat <- ncdim_def('lat', units='degrees_north', longname='latitude', calendar="standard", vals=13.94)




varLat <- ncvar_def(name='lat', units='degrees_north', dim=list(dimLayer), missval=NA, longname='latitude', prec='double')
varLon <- ncvar_def(name='lon', units='degrees_east', dim=list(dimLayer), missval=NA, longname='latitude', prec='double')

# Finally we create a variable to store the altitude. Here we make a reference
# to the lat and lon coordinates.
varLayer.WVC <- ncvar_def(name='SoilWVC', units='m3H2O m-3soil', 
                          dim=list(dimLayer, dimTime, dimLat, dimLon), missval=-9999, longname='Soil volumetric humidity')
varLayer.T <- ncvar_def(name='Soil_temperature', units='degreeC', 
                        dim=list(dimLayer, dimTime, dimLat, dimLon), missval=-9999, longname='Soil temperature')


varWood.T <- ncvar_def(name='Wood_temperature', units='degreeC', 
                       dim=list(dimWoodType, dimTime, dimLat, dimLon), missval=-9999, longname='Wood temperature')

varWood.WVC <- ncvar_def(name='WoodWVC', units='%', 
                         dim=list(dimWoodType, dimTime, dimLat, dimLon), missval=-9999, longname='Wood water content')

#varDate <- ncvar_def(name='Date', units='semi-hour', dim=list(dimTime), longname='Date link to dim(time)')
# we'll create a list of all variables so we can add them all at once
#vars <- list(varLat, varLon, varAlt)

vars <- list(varLayer.WVC, varLayer.T, varWood.T, varWood.WVC)

# We got the headers done, now let's make the file
outputfile <- 'faidherbiaflux_dataset.nc';
# Create a new empty netcdf file.
con <- nc_create(outputfile, vars)


# Add some extra attributes
#ncatt_put(con, varLat, 'standard_name', 'latitude')
#ncatt_put(con, varLon, 'standard_name', 'longitude')
ncatt_put(con, varLayer.WVC, 'standard_name', 'SoilVWC_Well2_Shaded*')
ncatt_put(con, varLayer.T, 'standard_name', 'SoilT_Well2_Shaded_*')
ncatt_put(con, varWood.WVC, 'standard_name', ' WoodVWC_*')
ncatt_put(con, varWood.T, 'standard_name', ' WoodT_*')

# This variable was implicitly created by the dimension, so we'll just specify it by name
ncatt_put(con, 'time', 'standard_name', 'time')
#ncatt_put(con, varLon, 'axis', 'X')
#ncatt_put(con, varLat, 'axis', 'Y')
ncatt_put(con, 'time', 'axis', 'T')
ncatt_put(con, varLayer.WVC, 'coordinates', 'lat lon layer time')
ncatt_put(con, varLayer.T, 'coordinates', 'lat lon layer time')

ncatt_put(con, varWood.WVC, 'coordinates', 'lat lon type time')
ncatt_put(con, varWood.T, 'coordinates', 'lat lon type time')

ncvar_put(con, varLayer.WVC, SoilWVC_dataset_reshape$value)
ncvar_put(con, varLayer.T, SoilT_dataset_reshape$value)
ncvar_put(con, varWood.WVC, WoodVWC_dataset_reshape$value)
ncvar_put(con, varWood.T, WoodT_dataset_reshape$value)



(con)
nc_close(con)


