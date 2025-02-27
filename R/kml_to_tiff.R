###################################################################
#' Convert a kml to a tif file
#'
#' This function turns a folder of .kml .pngs, specifically from SonarTRX, into a tif file.
#' @param folder_path Path to the input folder with all of the .pngs of your .kml. Delete the "logo" .png ahead of time.
#' @param boundary A SpatVect around the actual data in your .kml. We recommend drawing this using ArcGIS around where you have data. If you're not interested in cropping your .kml, insert this as a blank shapefile the same size as your .kml.
#' @param export_name Path, and name, of the .tif you want to export from this function.
#' @export
#' 
################################################################
kml_to_tiff <- function(folder_path, boundary, export_name){
  
  png_files <- list.files(folder_path, pattern = "\\.png$", full.names = TRUE)
  
  # Read images
  images <- lapply(png_files, rast)
 # Create project extent 
  extent <- ext(boundary)
  
  # Function to extract the layer ending with "1"
  extract_layer <- function(raster) {
    layer_name <- grep("1$", names(raster), value = TRUE)
    if (length(layer_name) > 0) {
      return(raster[[layer_name]])
    } else {
      warning("No layer ending with '1' found in one of the rasters.")
      return(NULL)
    }
  }
  
  # Apply the function to each SpatRaster in 'images'
  extracted_layers <- lapply(images, extract_layer)
  
  #extend extents of all layers to the whole recording
  extended_layers <- lapply(extracted_layers, function(raster) {
    extend(raster, extent)
  })
  gc()
  
  # okay, now change the resolution
  # Choose a reference raster (e.g., first raster in the list)
  reference_raster <- extended_layers[[1]]
  
  # Get the resolution of the reference raster
  reference_resolution <- res(reference_raster)
  
  # Create an empty list to store the resampled rasters
  resampled_rasters <- list()
  
  # Loop over each raster in the list and resample it
  for (i in 1:length(extended_layers)) {
    raster <- extended_layers[[i]]  # Get the current raster
    # Resample the raster to match the reference resolution
    resampled_raster <- resample(raster, reference_raster, method = "bilinear")
    # Store the resampled raster in the list
    resampled_rasters[[i]] <- resampled_raster
  }
  gc()
  # Function to replace 0 values with NA in a SpatRaster
  replace_zero_with_na <- function(raster) {
    raster[raster == 0] <- NA
    return(raster)
  }
  
  # Apply the function to each raster in the list
  for (i in seq_along(resampled_rasters)) {
    resampled_rasters[[i]] <- replace_zero_with_na(resampled_rasters[[i]])
  }
  
  gc()
  
  # Merge the rasters with masked zeros
  merged_raster <- do.call(merge, resampled_rasters)
  
  gc()
  #mask raster
  masked_raster<-mask(merged_raster, boundary)
  gc()
  #write raster
  writeRaster(masked_raster, export_location, filetype = "GTiff", overwrite = TRUE)
  
}