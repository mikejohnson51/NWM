
# ======================================== Accessing the Most Current Short-term Forecasts From the National Water Model ========================================== # 
                                                          
#                                                           For the ______________________________ HUC6 Unit                                                        #

# ==================================================================  Mike Johnson and Jim Coll  ================================================================== #
    
# Install package and build folders (Only do once) ---------------------------------------------------------------------------------------------------------------- #

  library(devtools); install_git("https://github.com/mikejohnson51/nwm1", "NWM"); library(NWM) # Only need once, comment out after first run

  setwd("/Users/mikejohnson/Desktop/HUC6_020200")                                          # Set to a folder dedicated to this project
  
  HUC6 = "020200"
  build_files()                                                                                # Build subfolders and paths needed for this workflow, 
                                                                                               # again only needed once, then comment out     
  get_HUC6_data(HUC6)
  
# Change when desired  -------------------------------------------------------------------------------------------------------------------------------------------- #
  
  flowlines  = paste0(getwd(),"/Flowlines/", HUC6, "-flow.shp")     # place all NHDFlowline shapefile files in the Flowlines folder, replace .shp name if different 
  catchments = paste0(getwd(),"/Flowlines/", HUC6, "-wbd.shp")    # place all NHDCatchment shapefile files in the Flowlines folder, replace .shp name if different

# Repeat Each Hour  ----------------------------------------------------------------------------------------------------------------------------------------------- #

  get_current()                                                                         # Get most recent short range forcast files from NOMADs server
  NC_comids = get_COMIDs(path="/Users/mikejohnson/Desktop/HUC6_020200/Flowlines/020200-flows.shp")                                                # Extract COMIDs from region of interest
  build_csv(folder = 'current',comids = red,regions.name = "GoldsboroNC")         # Build CSV of flows over each COMID 
  
  viz_current(catchment= catchtest,                                              # Build vizualizations in the Images folder
              flowlines = test, COMID = 6219762, region = "HUC6 020200")
  
# Create .GIF file ------------------------------------------------------------------------------------------------------------------------------------------------ #
  
  directory = getwd()                                 #Save your current working directory
  setwd(paste0(directory,"/Output/"))                 # Change your working directory to your output folder
  
      system('"C:/Program Files/ImageMagick-7.0.6-Q16/convert.exe" -delay 40 F:/DTF-study/ProvingGround/Images/*.png flows.gif')
  
  setwd(directory)                                    # Revert to origional working directory as specified at the start. Each hour you can start again at line 22.
                                                      # In each run previos folders and outputs will be overwitten. Be sure to move/save any desired output

test = readOGR("/Users/mikejohnson/Desktop/HUC6_020200/Flowlines/020200-flows.shp")
catchtest = readOGR("/Users/mikejohnson/Desktop/HUC6_020200/Flowlines/020200-wbd.shp")
# ================================================================================================================================================================= # 
# ================================================================================================================================================================= #
red = test$COMID
