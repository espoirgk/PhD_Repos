###By Espoir
###This code enable extraction some row in many files (rstics crop model outputs)
##extract_function(set_path = 'D:/R_project/sidy_files', var_of_interest = "lai.n.", slice_values = c(264, 275, 320))


extract_function <- function(set_path, var_of_interest, slice_values, output_path){
  
  #e.g set_path format: 'D:/R_project/sidy_files'
  #    var_of_interest : 'lai.n.'
  #slice_values : c(264, 275, 320
  #Output_path must be different from set_path
  
  j= 0
  
  for (name_file in list.files(set_path)){
    
    
    if (startsWith(name_file, "mod_")){
      
      dataset = read.csv(paste(set_path, '/', name_file, collapse = "", sep=""), sep = ";")
    
    
      dataset = dataset[,c((which(colnames(dataset) == "jul")):which(colnames(dataset) == colnames(dataset)[length(colnames(dataset))]))]
      
      
      dataset = dataset[dataset$jul %in% slice_values, ][,c("jul", var_of_interest)]
      
      #Create new empty dataframe with 1D 
      newdf <- data.frame(matrix(ncol = 1, nrow = 1))
      #names the first columns
      colnames(newdf) <- "usms_name"
      
      
      #Merge the two dataframe
      newdataset = merge(newdf, dataset)
      
      newdataset$usms_name[is.na((newdataset$usms_name))] <- name_file
      
      #For the first file
      if (j==0){
        
        newdf_empty <- data.frame(matrix(ncol = 3, nrow = 0))
        
        colnames(newdf_empty) <- colnames(newdataset)
        
        final = rbind(newdf_empty, newdataset)
      } else{
        #For the rest of files
        final = rbind(final, newdataset) 
        
      }
      
      
      j <-j+1 # Increment j index 
      
      
        #Store dataframe
        if (j==length(list.files(set_path))){
          
          filename <- paste(output_path,'/', var_of_interest, ".csv", collapse = "", sep="")
          
          if (file.exists(filename)){
            
            print("File already exists")
          } else {
            
            write.csv(final, filename, row.names = FALSE)
        
         }
        
      }
      
    }
    
    
  }
  
}

