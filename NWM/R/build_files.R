#' Setting Up A File Structure
#' 
#' This function helps define your folder strucutre to execute the NWM functions. 
#' Set up reuqire two things \cr \cr
#' (1) Define working directory using \code{\link[base]{setwd}} \cr 
#' (2) Execute code chunk. 
#' 
#'
#' @examples 
#' setwd("parent_directory")
#' build_files()
#' @author 
#' Mike Johnson and Jim Coll
#' @return 
#' This function will create five subfolders in your working directory set with 'setwd()'. 
#' These forlders are: \cr \cr
#' (1) Flowlines: You will need to place a shapefile of any flowlines here. \cr \cr
#' (2) Images:This will be where all images generated will go. \cr \cr
#' (3) NetCDFs: This is where all .nc files will be placed. Current forcasts obtained with the 'get_current()'
#' function will go in the "Current"subfolder. Output from the "get_choosen()" function will be placed in a folder created
#' related to its name. \cr \cr
#' (4) The output folder will be home to any .csv files created from the "get_CSV()" function or .gif files
#' generated from the "get_GIF()" function. \cr
#' @export
#' 

build_files = function(){
  
  working.directory = getwd()
  
  dir.create(paste0(working.directory, "/Flowlines"))
  dir.create(paste0(working.directory, "/Images"))
  dir.create(paste0(working.directory, "/NetCDFs"))
  dir.create(paste0(working.directory, "/Output"))
  dir.create(paste0(working.directory, "/Raw_RatingCurves"))
  dir.create(paste0(working.directory, "/NetCDFs/Current"))
  dir.create(paste0(working.directory, "/Output/Current"))
  dir.create(paste0(working.directory, "/Images/Current"))
  
  
}

