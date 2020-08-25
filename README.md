# SeabirdIJMS
Respository for a Masters Thesis 

• 'Thesis_Soane.R' contains all the code I used to perform GIS, create plots and find values for my tables

• 'SoaneDepthFinal.csv' is the main dataset which includes all references and variables in my statistical modelling 

• 'Publishdate.csv' is a list of the years the sources for diving depth/foraging observations were published. It is only used for Figure 1 

• 'SpeciesNoLiterature.csv' is a dataset of the seabirds I could not find literature for and was used to compile Table 1


Environmental variables are all in the main file 'SoaneDepthFinal.csv'. I downloaded NASA remote sensing datasets and extracted values for each variable at each foraging coordinate.
These are the destinations I downloaded the datasets from:

NASA EARTH OBSERVATIONS Chlorophyll Concentration (1 MONTH - AQUA/MODIS). 
https://neo.sci.gsfc.nasa.gov/view.php?datasetId=MY1DMM_CHLORA
I downloaded 12 floating point rasters in tiff format from January to December 2019
 

NASA EARTH OBSERVATIONS Sea Surface Temperature (1 MONTH - AQUA/MODIS).
https://neo.sci.gsfc.nasa.gov/view.php?datasetId=MYD28M 
Again, I downloaded 12 floating point rasters in tiff format from January to December 2019
 

NASA OCEAN COLOUR WEB Diffuse attenuation coefficient for downwelling irradiance at 490 nm (Kd_490).
https://oceancolor.gsfc.nasa.gov/l3/
This is my turbidity Index. I downloaded 12 level-3 datasets from January to December 2019 at 4km resolution
