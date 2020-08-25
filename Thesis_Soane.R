#ALL DATA AVAILABLE FROM https://github.com/isabeljms/SeabirdIJMS###

      #FIGURES AND TABLES IN LITERATURE REVIEW SECTION#

#TABLE 1
#birds with literature
divingdf<-read.csv("~/Uni/PROJECT/SoaneDepthFinal.csv")
#birds without literature
divingdfb<-read.csv("~/Uni/PROJECT/SpeciesNoLiterature.csv")
library(dplyr)
#selecting three variables
divingdf<- dplyr::select(divingdf, Family, CommonName) 
#excluding species duplicates
divingdfb<-distinct(divingdfb)
divingdf<-distinct(divingdf)
#tally of the number of birds with literature for each family
countfam <- divingdf %>% group_by(Family) %>% tally
#tally of the number of birds without literature for each family
countfamb <- divingdfb %>% group_by(Family) %>% tally
#working out percentages of birds in each family with diving depth data for the table
#Procellariidae 
79+52 #131
(52/131)*100
#Alcidae
15+9
(9/24)*100
#Anatidae
6+16
(16/22)*100
#Hydrobatidae
8+10
(8/18)*100
#Laridae
35+45
(35/80)*100
#Oceanitidae
(3/9)*100
#Phalacrocoracidae
19+18
(19/37)*100
#	Procellariidae
45+52
(45/97)*100
#Spheniscidae
(17/18)*100
#Stercorariidae
(4/6)*100



 #FIGURE 1

publishdate<-read.csv("~/Uni/PROJECT/publishdate.csv")
library(dplyr)
library(ggplot2)
#creating a tally of yearly publishings
cumpub<-publishdate%>%group_by(Yearofpublish)%>%tally 

#creating a square plot size 
par(pty='s') 
# cumulative plot of publish date of sources ustilised
tiff("publish.tiff", units="in", width=10, height=7, res=300)

ggplot(cumpub, aes(x=Yearofpublish, y=cumsum(n))) + geom_line() + geom_point(col="#99004C")+labs(x="Year of Publish",y="Cumulative Sum of Papers",title="Cumulative Plot of Published Papers on Diving Depth")+theme(aspect.ratio=1)
dev.off()



#FIGURE 2a
#import data
divingdf<-read.csv("~/Uni/PROJECT/SoaneDepthFinal.csv")
#loading relevant packages
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(cowplot)

 #opening a map of the world
world <- ne_countries(scale = "medium", returnclass = "sf")
 #Create a map with bird diving depth coordinates across family and weight category                
familyworld<-ggplot(data = world)+scale_size_area("Weight.kg")+
  geom_point(aes(Lon, Lat,colour=Family, size=Weight.kg), data= divingdf) + theme_bw()+
  geom_sf()+ labs(fill="legend", title="", x="", y="")
plot(familyworld)    
#creating a tiff file for better resolution
tiff("familyworld.tiff", units="in", width=10, height=7, res=300)



#FIGURE 2b
divingdf<-read.csv("~/Uni/PROJECT/SoaneDepthFinal.csv")
library(ggplot2)
divingdf$DepthDiving.m1<-divingdf$DepthDiving.m+1
#logging depth
divingdf$logdep<-log(divingdf$DepthDiving.m1)
#bird families displayed across latitude with weight(kg) as point size
ggplot(divingdf, aes(logdep,Lat))+theme(text = element_text(size=13))+geom_point(aes(size=Weight.kg,colour=Family))+labs(y="Latitude", x="Diving Depth (m)")
#plotting to view the graph
plot(latdepth)
#creating a tiff file for good resolution
tiff("familylat.tiff", units="in", width=10, height=7, res=300)



#FIGURE 3
#import data
divingdf<-read.csv("~/Uni/PROJECT/SoaneDepthFinal.csv")
#removing species duplicates
str(divingdf)
divingdf<-divingdf[!duplicated(divingdf$CommonName), ]
#adding 1 to all depths to mitgate for 0 values which cannot be logged
divingdf$DepthDiving.m1<-divingdf$DepthDiving.m+1
#logging depth
divingdf$logdep<-log(divingdf$DepthDiving.m1)
#creating a boxplot to show maximum diving depth across taxonomic family
ggplot(divingdf, aes(x = reorder(Family, logdep, FUN = mean), y = logdep))+theme(text = element_text(size=15))+
  geom_boxplot(fill="white",colour = "black",outlier.colour = "#99004C")+ labs(y="Log Maximum Diving Depth (m)",x="Taxonomic Family",title="Maximum Diving Depth across Taxonomic Families")+coord_flip()
#create a tiff with good resolution
tiff("familydepth.tiff", units="in", width=10, height=7, res=300)
dev.off()



#FIGURE 4
divingdf<-read.csv("~/Uni/PROJECT/SoaneDepthFinal.csv")
#putting conservation statuses into order of threat
library(dplyr)
divingdf<-divingdf %>%   mutate(Conservation.Status = factor(Conservation.Status, levels=c("NA", "DD", "LC", "NT", "VU", "EN", "CR")))
#removing species dupicates
divingdf<-divingdf[!duplicated(divingdf$CommonName), ]
#adding 1 to all depths to mitgate for 0 values which cannot be logged
divingdf$DepthDiving.m1<-divingdf$DepthDiving.m+1
#logging depth
divingdf$logdep<-log(divingdf$DepthDiving.m1)
#creating a boxplot to show maximum diving depth across IUCN conservation status
ggplot(divingdf, aes(logdep,Conservation.Status))+theme(text = element_text(size=15))+
  geom_boxplot(fill="white",colour = "black",outlier.colour = "#99004C")+ labs(y="IUCN Conservation Status",x="Log Maximum Diving Depth (m)",title="Maximum Diving Depth by IUCN Conservation Status")
#creating a tiff with good resolution
tiff("IUCNdepth.tiff", units="in", width=10, height=7, res=300)
#resetting
dev.off()



#FIGURE 5
divingdf<-read.csv("~/Uni/PROJECT/SoaneDepthFinal.csv")

library(dplyr)
#get rid of data deficient and NA values
divingdf<-subset(divingdf,divingdf$Conservation.Status!="NA")
divingdf<-subset(divingdf,divingdf$Conservation.Status!="DD")
#reordering categories
divingdf<-divingdf %>%   mutate(Conservation.Status = factor(Conservation.Status, levels=c("NA", "DD", "LC", "NT", "VU", "EN", "CR")))
#removing species duplicates 
divingdf<-divingdf[!duplicated(divingdf$CommonName), ]
#create a table with a tally of the number of species in each foraging mode and iucn category
Statusbyforage2<-divingdf%>% group_by(Type.Diving,Conservation.Status) %>% summarise(counts = n())
#boxplot to show IUCN status across foraging modes 
ggplot(Statusbyforage2, aes(factor(Type.Diving), counts, fill = Conservation.Status)) + coord_flip()+
  geom_bar(stat="identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format())+
  scale_fill_manual(values=c("forestgreen","chartreuse3","yellow","orangered","red2"))+theme_bw() + labs(title = "Foraging Mode in Seabirds with Relation to IUCN Conservation Status", y= "Percentage of Species", x= "Foraging Mode")
#creating a tiff to get a better resolution
tiff("IUCNforage.tiff", units="in", width=10, height=7, res=300)


#calculating the percentages of birds in each IUCN category 
#used to see if any families had more or less representatives than expected
info<-divingdf%>% group_by(Conservation.Status) %>% summarise(counts = n())
info$percent<-info$counts/181




     #DATA HANDLING AND ANALYSIS#

#DATA AVAILABLE FROM NASA EARTH OBSERVATIONS WEBSITE AND OCEANCOLOR WEBSITE

#CREATING RASTERS TO EXTRACT VALUES FOR ENVIRONMENTAL INDICES

#loading CHLOROPHYLL CONCENTRATION tiffs downloaded from NASA (floating point, 0.1 degrees, every month of 2019)
  #https://neo.sci.gsfc.nasa.gov/view.php?datasetId=MY1DMM_CHLORA
library(raster)
library(sp)
jan<-'~/Uni/PROJECT/jan2019.tiff' 
#turn each into a raster
jan2019=raster(jan)
feb<-'~/Uni/PROJECT/feb2019.tiff' 
feb2019=raster(feb)
mar<-'~/Uni/PROJECT/mar2019.tif'
mar2019=raster(mar)
apr<-'~/Uni/PROJECT/apr2019.tiff'
apr2019=raster(apr)
may<-'~/Uni/PROJECT/may2019.tiff'
may2019=raster(may)
jun<-'~/Uni/PROJECT/jun2019.tif'
jun2019=raster(jun)
jul<-'~/Uni/PROJECT/jul20199.tiff'
jul2019=raster(jul)
aug<-'~/Uni/PROJECT/aug2019.tiff'
aug2019=raster(aug)
sep<-'~/Uni/PROJECT/sep2019.tiff'
sep2019=raster(sep)
oct<-'~/Uni/PROJECT/oct2019.tiff'
oct2019=raster(oct)
nov<-'~/Uni/PROJECT/nov2019.tiff'
nov2019=raster(nov)
dec<-'~/Uni/PROJECT/dec2019.tiff'
dec2019=raster(dec)
#checking what the fill value is (used for land coordinates as this is an oceanic dataset)
head(dec2019)
# turning the fill value 255 into NA #
ja <- reclassify(jan2019, cbind(99999, NA ))
fe <- reclassify(feb2019, cbind(99999, NA ))
ma<-reclassify(mar2019, cbind(99999, NA ))
ap<-reclassify(apr2019, cbind(99999, NA ))
my<-reclassify(may2019, cbind(99999, NA ))
ju<-reclassify(jun2019, cbind(99999, NA ))
jl<-reclassify(jul2019, cbind(99999, NA ))               
au<-reclassify(aug2019, cbind(99999, NA ))
se<-reclassify(sep2019, cbind(99999, NA ))
oc<-reclassify(oct2019, cbind(99999, NA ))
no<-reclassify(nov2019, cbind(99999, NA ))
de<-reclassify(dec2019, cbind(99999, NA ))

#adding all the tiffs together by creating a stack
stack1<-stack(ja,fe,ma,ap,my,ju,jl,au,se,oc,no,de)
#creating a single tiff with mean values of 2019
oceanchlo <- calc(stack1, fun = mean, na.rm = T)
#plot the raster
plot(oceanchlo)
#save this raster into my documents
writeRaster(x = oceanchlo, filename = "oceanchlo.tif", driver = "GeoTiff", overwrite=TRUE)
#load my diving depth dataset
divingdf<-read.csv('~/Uni/PROJECT/SoaneDepthFinal.csv')
#turn latitude and longitude columns into coordinates
coordinates(divingdf)<-~Lon+Lat 
#plot the coordinates on the raster
points(coordinates(divingdf),pch=20)
#extract chlorophyll values for diving coordinates # 
chl <- extract(oceanchlo, divingdf, na.rm=TRUE)
#turn the diving depth dataset back into a dataframe
divingdf<-as.data.frame(divingdf)
#add the chlorophyll concentration values to the dataframe
divingdf$Chlorophyll.Conc<-chl           
#get an idea of how many NAs I have
is.na(sh)
#adjust any coordinates not in the ocean by saving the diving dataset with the new chlorophyll variable
#repeat above steps until no/minimum NAs possible
write.csv(divingdf, "~/Uni/PROJECT/depthsNA.csv")


#use the same stack with 2019 chlorophyll data but with the SD function
oceanchlsd <- calc(stack1, fun = sd, na.rm = T)
#plot ocean chlorophyll sd
plot(oceanchlsd)
#turn latitude and longitude columns back into coordinates
coordinates(divingdf)<-~Lon+Lat 
#plot the coordinates on the raster
points(coordinates(divingdf),pch=20)
#extract standard deviation values from the raster for each foraging coordinate
chlsd <- extract(oceanchlsd, divingdf, na.rm=TRUE)
#turn the diving depth dataset back into a dataframe
divingdf<-as.data.frame(divingdf)
#save the chlorophyll SD variable as a new column
divingdf$Chorophyll.Conc.SD<-chlsd 
#save a csv version of the dataset with new variables
write.csv(divingdf, "~/Uni/PROJECT/chlorosd.csv")



#loading TEMPERATURE tiffs downloaded from NASA (floating point, 0.1 degrees, every month of 2019)
#https://neo.sci.gsfc.nasa.gov/view.php?datasetId=MYD28M
library(raster)
#loading tiffs
JanTemp2019<-'~/Uni/PROJECT/JANTEMP.tiff' 
#turning each tiff into a raster
JanTemp2019=raster(JanTemp2019)
FebTemp2019<-'~/Uni/PROJECT/FEBTEMP.tif' 
FebTemp2019=raster(FebTemp2019)
MarTemp2019<-'~/Uni/PROJECT/MARTEMP.tif'
MarTemp2019=raster(MarTemp2019)
AprTemp2019<-'~/Uni/PROJECT/APRTEMP.tif'
AprTemp2019=raster(AprTemp2019)
MayTemp2019<-'~/Uni/PROJECT/MAYTEMP.tif'
MayTemp2019=raster(MayTemp2019)
JunTemp2019<-'~/Uni/PROJECT/JUNTEMP.tiff'
JunTemp2019=raster(JunTemp2019)
JulTemp2019<-'~/Uni/PROJECT/JULTEMP.tiff'
JulTemp2019=raster(JulTemp2019)
AugTemp2019<-'~/Uni/PROJECT/AUGTEMP.tiff'
AugTemp2019=raster(AugTemp2019)
SepTemp2019<-'~/Uni/PROJECT/SEPTEMP.tif'
SepTemp2019=raster(SepTemp2019)
OctTemp2019<-'~/Uni/PROJECT/OCTTEMP.tiff'
OctTemp2019=raster(OctTemp2019)
NovTemp2019<-'~/Uni/PROJECT/NOVTEMP.tiff'
NovTemp2019=raster(NovTemp2019)
DecTemp2019<-'~/Uni/PROJECT/DECTEMP.tif'
DecTemp2019=raster(DecTemp2019)

# turning the fill value 255 into NA #
JanTemp <- reclassify(JanTemp2019, cbind(99999, NA ))
FebTemp <- reclassify(FebTemp2019, cbind(99999, NA ))
MarTemp<-reclassify(MarTemp2019, cbind(99999, NA ))
AprTemp<-reclassify(AprTemp2019, cbind(99999, NA ))
MayTemp<-reclassify(MayTemp2019, cbind(99999, NA ))
JunTemp<-reclassify(JunTemp2019, cbind(99999, NA ))
JulTemp<-reclassify(JulTemp2019, cbind(99999, NA ))               
AugTemp<-reclassify(AugTemp2019, cbind(99999, NA ))
SepTemp<-reclassify(SepTemp2019, cbind(99999, NA ))
OctTemp<-reclassify(OctTemp2019, cbind(99999, NA ))
NovTemp<-reclassify(NovTemp2019, cbind(99999, NA ))
DecTemp<-reclassify(DecTemp2019, cbind(99999, NA ))
#adding all the tiffs together by creating a stack
stack2<-stack(JanTemp, FebTemp,MarTemp,AprTemp,MayTemp, JunTemp,JulTemp, AugTemp, SepTemp,OctTemp,NovTemp,DecTemp)
#creating a tiff with mean temperature values over 2019
oceantemp <- calc(stack2, fun = mean, na.rm = T)
#writing a raster and saving to documents
writeRaster(x = oceantemp, filename = "oceantemp.tif", driver = "GeoTiff", overwrite=TRUE)

#loading the new mean temperature raster
oceantemp<-'~/Uni/PROJECT/oceantemp.tif'
oceantemp=raster(oceantemp)
#plotting the raster
plot(oceantemp)
#loading diving depth data
divingdf<-read.csv('~/Uni/PROJECT/SoaneDepthFinal.csv')
#creating coordinates from latitude and longitude
coordinates(divingdf)<-~Lon+Lat 
#plotting the foraging coordinates on the mean temperature raster
points(coordinates(divingdf),pch=20)
#extract the temperature value for every coordinate
temp <- extract(oceantemp, divingdf, na.rm=TRUE)
#turn the diving depth dataset back into a dataframe
divingdf<-as.data.frame(divingdf)
#add the temperature values as a new variable
divingdf$temp<-temp
#save the new dataset
write.csv(divingdf, "temp2.csv")


#get a raster for standard deviation of temperature over 2019 by making a SD calculation on the temperature raster stack
oceantempsd <- calc(stack2, fun = sd, na.rm = T)
#save the temperature SD raster 
writeRaster(x = oceantempsd, filename = "oceantempsd.tif", driver = "GeoTiff", overwrite=TRUE)
#plot the temprature SD raster
plot(oceantempsd)
#turn the diving depth data back into coordinates
coordinates(divingdf)<-~Lon+Lat 
#plotting the foraging coordinates on the temperature SD raster
points(coordinates(divingdf),pch=20)
#extract the SD values for each foraging coordinate
tempsd <- extract(oceantempsd, divingdf, na.rm=TRUE)
#turn the diving depth dataset back into a dataframe
divingdf<-as.data.frame(divingdf)
#add the new variable onto the dataset
divingdf$tempsd<-tempsd 

#write a csv to add it to the online dataset
write.csv(divingdf,"~/Uni/PROJECT/divingdepthsd.csv")



#loading TURBIDITY INDEX (downwelling irradiance 490nm) tiffs downloaded from NASA OceanColor (4km, every month of 2019)
#https://oceancolor.gsfc.nasa.gov/l3/
install.packages("ncdf4")
library(ncdf4)
library(raster)
# fill value is -32767
## Reading netcdf file
month1 <- raster("~/Uni/PROJECT/490nm/requested_files/month1.nc")
#saving each ncdf4 file as a tiff file
writeRaster(x = month1, filename = 'month1.tif', format = 'GTiff', overwrite = TRUE)
month2 <- raster("~/Uni/PROJECT/490nm/requested_files/month2.nc")
writeRaster(x = month2, filename = 'month2.tif', format = 'GTiff', overwrite = TRUE)
month3 <- raster("~/Uni/PROJECT/490nm/requested_files/month3.nc")
writeRaster(x = month3, filename = 'month3.tif', format = 'GTiff', overwrite = TRUE)
month4 <- raster("~/Uni/PROJECT/490nm/requested_files/month4.nc")
writeRaster(x = month4, filename = 'month4.tif', format = 'GTiff', overwrite = TRUE)
month5 <- raster("~/Uni/PROJECT/490nm/requested_files/month5.nc")
writeRaster(x = month5, filename = 'month5.tif', format = 'GTiff', overwrite = TRUE)
month6 <- raster("~/Uni/PROJECT/490nm/requested_files/month6.nc")
writeRaster(x = month6, filename = 'month6.tif', format = 'GTiff', overwrite = TRUE)
month7 <- raster("~/Uni/PROJECT/490nm/requested_files/month7.nc")
writeRaster(x = month7, filename = 'month7.tif', format = 'GTiff', overwrite = TRUE)
month8 <- raster("~/Uni/PROJECT/490nm/requested_files/month8.nc")
writeRaster(x = month8, filename = 'month8.tif', format = 'GTiff', overwrite = TRUE)
month9 <- raster("~/Uni/PROJECT/490nm/requested_files/month9.nc")
writeRaster(x = month9, filename = 'month9.tif', format = 'GTiff', overwrite = TRUE)
month10 <- raster("~/Uni/PROJECT/490nm/requested_files/month10.nc")
writeRaster(x = month10, filename = 'month10.tif', format = 'GTiff', overwrite = TRUE)
month11 <- raster("~/Uni/PROJECT/490nm/requested_files/month11.nc")
writeRaster(x = month11, filename = 'month11.tif', format = 'GTiff', overwrite = TRUE)
month12 <- raster("~/Uni/PROJECT/490nm/requested_files/month12.nc")
writeRaster(x = month12, filename = 'month12.tif', format = 'GTiff', overwrite = TRUE)

#loading each tiff file
month1<-'~/Uni/PROJECT/month1.tif' 
#turning each tif into a raster
month1=raster(month1)
month2<-'~/Uni/PROJECT/month2.tif' 
month2=raster(month2)
month3<-'~/Uni/PROJECT/month3.tif'
month3=raster(month3)
month4<-'~/Uni/PROJECT/month4.tif'
month4=raster(month4)
month5<-'~/Uni/PROJECT/month5.tif'
month5=raster(month5)
month6<-'~/Uni/PROJECT/month6.tif'
month6=raster(month6)
month7<-'~/Uni/PROJECT/month7.tif'
month7=raster(month7)
month8<-'~/Uni/PROJECT/month8.tif'
month8=raster(month8)
month9<-'~/Uni/PROJECT/month9.tif'
month9=raster(month9)
month10<-'~/Uni/PROJECT/month10.tif'
month10=raster(month10)
month11<-'~/Uni/PROJECT/month11.tif'
month11=raster(month11)
month12<-'~/Uni/PROJECT/month12.tif'
month12=raster(month12)

# turning the fill value -32767 into NA 
month1 <- reclassify(month1, cbind(-32767, NA ))
month2 <- reclassify(month2, cbind(-32767, NA ))
month3<-reclassify(month3, cbind(-32767, NA ))
month4<-reclassify(month4, cbind(-32767, NA ))
month5<-reclassify(month5, cbind(-32767, NA ))
month6<-reclassify(month6, cbind(-32767, NA ))
month7<-reclassify(month7, cbind(-32767, NA ))               
month8<-reclassify(month8, cbind(-32767, NA ))
month9<-reclassify(month9, cbind(-32767, NA ))
month10<-reclassify(month10, cbind(-32767, NA ))
month11<-reclassify(month11, cbind(-32767, NA ))
month12<-reclassify(month12, cbind(-32767, NA ))
#creating a stack of every raster in 2019
stack3<-stack(month1, month2, month3,month4,month5,month6,month7,month8,month9,month10,month11,month12)
#creating a raster with mean downwelling irradiance values over 2019
oceanturbidity <- calc(stack3, fun = mean, na.rm = T)
#plotting the raster
plot(oceanturbidity)
#loading diving depth data
divingdf<-read.csv('~/Uni/PROJECT/SoaneDepthFinal.csv')
#turning dataset into coordinates
coordinates(divingdf)<-~Lon+Lat 
#plotting coordinates on the turbidity raster
points(coordinates(divingdf),pch=20)
#extract the downwelling irradiance values for each foraging coordinate
turb <- extract(oceanturbidity, divingdf, na.rm=TRUE)
#turn the diving depth dataset back into a dataframe
divingdf<-as.data.frame(divingdf)
#add the new variable onto the dataset
divingdf$turb<-turb
#save the new dataset an update the online spreadsheet
write.csv(divingdf, "turb.csv")


#use the downwelling irradiance stack to find standard deviation 
oceanturbsd <- calc(stack3, fun = sd, na.rm = T)
#write and save a raster
writeRaster(x = oceanturbsd, filename = "oceanturbsd.tif", driver = "GeoTiff", overwrite=TRUE)
#plot the raster
plot(oceanturbsd)
#turn diving dataset back into coodinates
coordinates(divingdf)<-~Lon+Lat 
#plot coordinates on raster
points(coordinates(divingdf),pch=20)
#extract the turbidity SD values from each coordinate
turbsd <- extract(oceanturbsd, divingdf, na.rm=TRUE)
#turn diving dataset back into a dataframe
divingdf<-as.data.frame(divingdf)
#add the new variable
divingdf$Turbidity.Index.SD<-turbsd
#save the new dataset and update online spreadsheet
write.csv(divingdf, "turbsd2.csv")



#EXPLANATORY VARIABLE DESCRIPTIONS (SD, MEAN, MIN/MAX)#
#looking for a description summary of temperature
divingdf<-subset(divingdf,divingdf$Surface.Temperature!="NA")
sd(divingdf$Surface.Temperature)
summary(divingdf$Surface.Temperature)
#looking for a description summary of chlorophyll
divingdf<-subset(divingdf,divingdf$Chlorophyll.Concentration!="NA")
sd(divingdf$Chlorophyll.Concentration)
summary(divingdf$Chlorophyll.Concentration)
(2.5/61)*100
#looking for a description summary of turbidity
divingdf<-subset(divingdf,divingdf$Turbidity.Index!="NA")
sd(divingdf$Turbidity.Index)
summary(divingdf$Turbidity.Index)
(0.17/5.17)*100



#CORRELATIONS
#between Family and weight
library(car)
#variance inflation factor of depth explained by family and weight.kg as family is not continuous
vif(lm(DepthDiving.m~Family+Weight.kg,data=divingdf,na.action="na.exclude"))
#low correlation



#TABLE S1

#correlations between other explanatory variables
z <- subset(na.omit(divingdf), 
            select = c("Surface.Temperature.SD","Surface.Temperature","Turbidity.Index.SD","Turbidity.Index","Chlorophyll.Concentration", "Chlorophyll.Concentration.SD","Weight.kg"))
cor(z)
#Chlorophyll, turbidity, chlorophyll SD and turbidity SD are too high to model together



#NORMALITY TESTS
#histograms of variables
hist(divingdf$DepthDiving.m, breaks=50)
hist(divingdf$Turbidity.Index, breaks=50)
hist(divingdf$Turbidity.Index.SD, breaks=50)
hist(divingdf$Surface.Temperature.SD, breaks=50)
hist(divingdf$Chlorophyll.Concentration, breaks=50)
hist(divingdf$Weight.kg, breaks=50)
hist(divingdf$Surface.Temperature, breaks=50)
str(divingdf)
class(divingdf)
#none of these variables look normally distributed (a apart from SST which has an even range)



#FIGURE S1
divingdf<-read.csv("~/Uni/PROJECT/SoaneDepthFinal.csv")
#removing NAs from the dataset
divingdf<-subset(divingdf,divingdf$DepthDiving.m!="NA")
divingdf<-subset(divingdf,divingdf$Turbidity.Index!="NA")
#looking for skew in a histogram
hist(divingdf$DepthDiving.m, breaks=50)  #high positive skew
#linear model of diving depth explained by the explanatory variable turbidity
a<-lm(divingdf$DepthDiving.m~divingdf$Turbidity.Index)
#clearing all plots
dev.off()
#plotting to assess normality of linear model
plot(a,which=2) #looks bad



#FIGURE S2
divingdf<-read.csv("~/Uni/PROJECT/SoaneDepthFinal.csv")
#removing NAs
divingdf<-subset(divingdf,divingdf$DepthDiving.m!="NA")
divingdf<-subset(divingdf,divingdf$Turbidity.Index!="NA")
#adding one to the diving depths because we want to keep the 0 values
divingdf$DepthDiving.m1<-divingdf$DepthDiving.m+1
#logging the diving depth data
divingdf$logdepth<-log(divingdf$DepthDiving.m1)
#logging the turbidity data
divingdf$logturb<-log(divingdf$Turbidity.Index)
#plotting a histogram
hist(divingdf$logturb, breaks=50) #still skew
#removing the non-diving birds as they wont be used in the LLM
divingdf<-subset(divingdf,divingdf$Depthzero==1)
#creating a model with logs of depth and turbidity
plotting<-lm(divingdf$logdepth~divingdf$logturb)
#plot the linear model
plot(plotting, which=2) #much less skew



#BINOMIAL MODEL
library(lme4)
b<- glm(Depthzero ~ Surface.Temperature+Surface.Temperature.SD+Chlorophyll.Concentration+Weight.kg, data = divingdf, family = binomial(link = logit))
b2<- glm(Depthzero ~ Surface.Temperature+Turbidity.Index+Surface.Temperature.SD+Weight.kg, data = divingdf, family = binomial(link = logit))
b3<- glm(Depthzero ~ Surface.Temperature+Surface.Temperature.SD+Weight.kg, data = divingdf, family = binomial(link = logit))
summary(b)
summary(b2)
summary(b3)
#looking at the deviance against degrees of freedom, they all look as though they have bad model fit



#TABLE 2 
#statistics on binomial model fit

#pseudo r-squared
with(summary(b), 1 - deviance/null.deviance) #0.09 = 9% 

#p-values are opposite to normal, i.e ABOVE 0.05 is significant
#Chi-squared
1-pchisq(392.37, 321) #0.0039
#Hosmer-Lemeshow test 
library(ResourceSelection)
hoslem.test(b$y, b$fitted) #p-value = 1.183e-05
#all show insignificance



#TABLE 3 coefficient summary
summary(b)



#LINEAR MIXED MODEL COMPARISON WITH STATISTICS, AIC, BIC and LogLik
divingdf<-read.csv("~/Uni/PROJECT/SoaneDepthFinal.csv")
#creating logs of all skewed data
divingdf$logdepth<-log(divingdf$DepthDiving.m)
divingdf$logturbsd<-log(divingdf$Turbidity.Index.SD)
divingdf$logturb<-log(divingdf$Turbidity.Index)
divingdf$logch<-log(divingdf$Chlorophyll.Concentration)
divingdf$logchsd<-log(divingdf$Chlorophyll.Concentration.SD)
divingdf$logmass<-log(divingdf$Weight.kg)
divingdf$logsstsd<-log(divingdf$Surface.Temperature.SD)
is.numeric(divingdf$Chorophyll.Concentration.SD)

#TABLE S2
#creating and comparing LLMs to collate values for the table
#starting with log chlorophyll concentration in the model
library(lme4)
trial1<- lmer(logdepth~ logch+Surface.Temperature+logsstsd+logmass+(1|Family), subset(divingdf,Depthzero==1))
#getting a version of trial1 without the random factor
fixed1 <- lm(formula(trial1,fixed.only=TRUE),
             data=eval(getCall(trial1)$data))
#get a table showing AIC values when variables are dropped from the model
step(fixed1) #143.55
#the LLM with the lowest AIC value is the one below:
compare1<- lmer(logdepth~ logch+logsstsd+logmass+(1|Family), subset(divingdf,Depthzero==1))
# get AIC, BIC and LogLik values for the comparison table
AIC(compare1)
BIC(compare1)
logLik(compare1)

#starting another LLM with log chlorophyll concentration SD 
trial2<- lmer(logdepth~ logchsd+Surface.Temperature+logsstsd+logmass+(1|Family), subset(divingdf,Depthzero==1))
#getting a version of trial2 without the random factor
fixed2 <- lm(formula(gtrial2,fixed.only=TRUE),
             data=eval(getCall(trial2)$data))
#get a table showing AIC values when variables are dropped from the model
step(fixed2) #141.86
#the LLM with the lowest AIC value is the one below:
compare2<- lmer(logdepth~ logchsd+logsstsd+logmass+(1|Family), subset(divingdf,Depthzero==1))
# get AIC, BIC and LogLik values for the comparison table
AIC(compare2)
BIC(compare2)
logLik(compare2)

#starting another LLM with log turbidity 
trial3<- lmer(logdepth~ logturb+Surface.Temperature+logsstsd+logmass+(1|Family), subset(divingdf,Depthzero==1))
#getting a version of trial3 without the random factor
fixed3 <- lm(formula(trial3,fixed.only=TRUE),
             data=eval(getCall(trial3)$data))
#get a table showing AIC values when variables are dropped from the model
step(fixed3) #144.03
#the LLM with the lowest AIC value is the one below:
compare3<- lmer(logdepth~ logturb+logsstsd+logmass+(1|Family), subset(divingdf,Depthzero==1))
AIC(compare3)
BIC(compare3)
logLik(compare3)


#starting another LLM with log turbidity SD
trial4<- lmer(logdepth~ logturbsd+Surface.Temperature+logsstsd+logmass+(1|Family), subset(divingdf,Depthzero==1))
#getting a version of trial3 without the random factor
fixed4 <- lm(formula(trial4,fixed.only=TRUE),
             data=eval(getCall(trial4)$data))
step(fixed4) #138.16
#the best AIC so far and of the other stepwise models in the table:
g<- lmer(logdepth~ logturbsd+logsstsd+logmass+(1|Family), subset(divingdf,Depthzero==1))

#just checking a more simple model too
g0<- lmer(logdepth~ logmass+logsstsd+(1|Family), subset(divingdf,Depthzero==1))
#find the statistics of most parsimonious model
AIC(g)
BIC(g)
logLik(g)
#statistics of model g0
AIC(g0)
BIC(g0)
logLik(g0)
#I put all AIC, BIC, LogLik values in a table 



#TABLE 4 and 6
#summary of coefficients (fixed and random)
summary(g)



#TABLE 5
#finding the confidence intervals of fixed coefficients
confint(g)


#finding the conditional and marginal R-squared to help judge the fit of the model
library(MuMIn)
r.squaredGLMM(g)



#FIGURE S3
#creating a coefficient plot
library(coefplot)
coefplot(g, col="#99004C")



#FIGURE S4
#calculates a residual plot for LLMs
devtools::install_github("goodekat/redres")
library(redres)
plot_resqq(g)




