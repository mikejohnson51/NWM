                      
#====================================================== (Arizona Test Case) ===================================================#

                      library(devtools); install_git("https://github.com/mikejohnson51/nwm1", "NWM"); library(NWM)         

                                            setwd('/Users/mikejohnson/Desktop/Arizona/')

                                            
                                                          build_files()

                                                          
#=============================================== (Arizona flood ran in 7.5 min) ===============================================#

                                                          
          living_flood_setup(flowlines_path = 'Geospatial/Flowlines/ForFred/AZ_Deaths.shp', hand.raster.needed = TRUE )

             
#============================================== (Arizona flood ran in 41 seconds) =============================================#

             
          living_flood_data(area_of_interest = "Arizona", flowlines_path = 'Geospatial/Flowlines/ForFred/AZ_Deaths.shp')
                                           
                                           

                                           
#==============================================================================================================================#









