# date: 2014-03-01
# author: Emma
# This is a script for loading the data into R and getting it into a proper format. What's left is to convert date-time into useful formats so they can be used to filter and plot things. 

rm(list = ls())

require(plyr)
require(sp)
require(corpcor)
require(ggplot2)
require(maps)
require(mapdata)
require(maptools)

data <- read.csv("VMS_cleaning/data/VMS.txt",header=FALSE,strip.white=TRUE,stringsAsFactors=FALSE)

###################################
# Reading and formatting Raw Data #
##################################

# since adding the file name to each row (for tracking purposes) There are more blank lines. To find these, search for column 3 NAs and remove
	data <- data[-which(data[,3]==""),]

# convert latitude and longitude to decimal degrees. To do this I need to make a seperate column for seconds. 
	lat_minutes <- as.numeric(data[,4]) # this generates some NAs in remnent header rows. remove them. 

# this generates NAs in a few more empty rows. These rows originally said something like 'xxxx rows selected." where xxx is a huge number. Now I can search for and remove any NAs in data$minutes
	data <- data[-which(is.na(lat_minutes)),]

	lat_minutes <- lat_minutes[-which(is.na(lat_minutes))]
	lon_minutes <- as.numeric(as.character(data[,6]))

# Now I can make a column for seconds
	lat_seconds <- lat_minutes - floor(lat_minutes)
	lon_seconds <- lon_minutes - floor(lon_minutes)

# Now for decimal degrees. According to this website (http://andrew.hedges.name/experiments/convert_lat_long/) I can get decimal degrees by dividing the minutes by 60 and the seconds by 3600.
	latitude <- as.numeric(as.character(data[,3])) + lat_minutes/60 + lat_seconds/3600
	longitude <-as.numeric(as.character(data[,5])) - lon_minutes/60 - lon_seconds/3600
	# because longitudes are all negative, we're dealing with the west coast

# clean up dataframe so only characters and numeric data
	VMS <- data.frame(Vessel_Name = data[,1])
	VMS$Ship_No <- as.character( data[,2])
	VMS$Latitude <- latitude
	VMS$Longitude <- longitude
	VMS$Date_Time <- data[,7]
	VMS$Avg_Speed <- as.numeric(as.character(data[,8]))
	VMS$Avg_Direction <- as.numeric(as.character(data[,9]))
	VMS$Declarations <- as.numeric(as.character(data[,10])) 
	# this should generate some NAs because some declarations are NA
	
# next is to convert the dates and times into date and time objects, first need to split dates and times apart  Because they are currently in a format POSIX recognizes I can call POSIX directly
	dt <- strptime(VMS$Date_Time, format='%Y-%m-%d %H:%M', tz = 'America/Los_Angeles' )
	dt.pos <- as.POSIXct(dt, tz = 'America/Los_Angeles')
	dtl <- as.POSIXlt(dt.pos, tz = 'America/Los_Angeles')
	
  VMS$year = dtl$year + 1900 # month, add 1 since POSIXlt starts at 0	
	VMS$month = dtl$mon+1 # month, add 1 since POSIXlt starts at 0	
	VMS$day = dtl$mday # day of the month
	VMS$time = paste(formatC(dtl$hour, width=2, flag=0), formatC(dtl$min, width=2, flag=0), sep=':')

##################################################
# Merging Declarations and definitions with data #
##################################################

# next step is adding a lookup table to match Doc_number with ship_number. First need to import the lookup table
	Vessel_Codes <- read.csv("Data/VMS/West_Coast_Vessels.csv",header=TRUE,stringsAsFactors=FALSE)

# changing the column name in my dataset to match lookup table
	colnames(VMS)[2] = "Ship_Number"

	VMS <- join(VMS, Vessel_Codes[,1:2], by = "Ship_Number")

# now add a lookup table to add declaration descriptions to declaration codes
	Declaration_Codes <- read.csv("Data/VMS/Declarations.csv",header=TRUE,stringsAsFactors=FALSE)
	colnames(Declaration_Codes)[1] = "Declarations"

	VMS <- join(VMS, Declaration_Codes, by = "Declarations")


#####################
# Adding Ports data #
#####################
	ports <- read.csv("Data/VMS/wc_fishing_communities.csv",header=FALSE)
	colnames(ports) <- c("Ports","Longitude","Latitude")

# add in extra ports that are not part of dataset, but identified by time fishermen spend at them 
# missing some ports (found by finding points that are stationary in movie)
	Nahcotta <- c(-124.0302, 46.5)
	SanFrancisco <- c(-122.4201, 37.80847)
	Astoria2 <- c(-123.7585,46.2) # think already have Astoria, but another port just outside of town
	SanPedro<- c(-118.2652, 33.73503)
	Ventura <- c(-119.2652, 34.2435)
	Oxnard <- c(-119.2285, 34.17006)
	MarinadelRey <- c(-118.4469, 33.97175)
	Petaluma  <- c(-122.5034,38.11667)

	# not in a port, in a neighborhood. VMS not turned off?
	IslaVista <- c(-119.7986, 34.43672)	
	SimiValley <- c(-118.7986, 34.24689)
	Camarillo <- c(-118.9952, 34.22683)
	FountainValley <- c(-117.9568, 33.72006)
	Oxnard_land <- c(-119.1585, 34.23333)
	SantaMaria <- c(-120.4418, 34.94858)
	Nipomo <- c(-120.4701, 35.04181)

	extras <- rbind(Nahcotta, SanFrancisco, Astoria2, SanPedro, Ventura, Oxnard, MarinadelRey, Petaluma, IslaVista, SimiValley, Camarillo, FountainValley, Oxnard_land, SantaMaria, Nipomo)

	extra_ports <- data.frame(Ports = rownames(extras), Longitude = extras[,1], Latitude = extras[,2])
  row.names(extra_ports) <- NULL

	ports <- rbind(ports, extra_ports)

# make list for each port. Find VMS data points within a certain threshold distance from these ports. Bertrand et al. (2005) [find Levy walks in VMS peruvian anchovetta data] use a threshold of 2 nautical miles. According to The Google 2 nautical miles is equivalent to 3.704 km. When longlat=TRUE in spDists the units are km. 

# turn both port and VMS into sp objects
	coordinates(ports) <- c("Longitude","Latitude")
	coordinates(VMS) <- c("Longitude","Latitude")
	
	dn <- vector(mode = "list", length=nrow(ports))	# preallocate list
	threshold <- 3.704

	for (i in 1:nrow(ports)){
	dn[[i]] <- which(spDists(VMS, ports[i,], longlat=TRUE) <= threshold)
	print(i)
		}
	
	names(dn) <- ports$Ports # each list is named for the port it's about
	
	VMS$status <- rep(NA,nrow(VMS))

	for(i in 1:length(dn)){
		if(length(dn[[i]]) > 0) VMS$status[dn[[i]]] <- names(dn)[i]
		print(i)
	}

# result: sorted now has NA if point not within 2 nautical miles of port, and port name if is within 2k nautical miles. 
# but still many points on land but not within 2 nautical miles. To filter those use NOAA GIS shapefiles of coastlines

# read in shapefile

# want intermediate resolution ('i') and just the boundary between ocean and land ('L1'). This is the biggest one I can do without R crashing
  x <- readShapeSpatial("Data/WCspatial/GSHHS_shp/i/GSHHS_i_L1.shp",force_ring=TRUE)

# based on trial and error, found that N.America is the polygon with id==2
  NAmerica <- x[x$id==2,]  	# N. america

# make another column in VMS for whether or not VMS point is onland
  VMS$onland = rep(0,nrow(VMS))

  filtersea <- over(VMS, NAmerica)
  onland <- filtersea[!is.na(filtersea$area),]

# onland provides rowname for atsea data which falls on land. will label in atsea$status 'onland'
  VMS$onland[which(row.names(VMS) %in% row.names(onland))]=1

# turn VMS back into a dataframe
    VMS <- as.data.frame(VMS)

# for now, mark the first and remove the rest VMS duplicates (revisit later with Haynie et al cleaning methods)
    VMS$dups <- rep(0, nrow(VMS))    # row to flag duplicates
    ind <- duplicated(VMS[c("Vessel_Name","Date_Time")]) | duplicated(VMS[c("Vessel_Name", "Date_Time")], fromLast=TRUE)    # finding all duplicates
    VMS$dups[ind] <- 1 # flagging duplicates
    noDups_VMS <- VMS[!duplicated(VMS[,c("Vessel_Name","Date_Time")]),] # removing duplicates

write.csv(VMS, file="VMS_cleaning/results/2014-03-02/VMS_wDups.csv",row.names=F) # write version that includes duplicates
write.csv(noDups_VMS, file="VMS_cleaning/results/2014-03-02/VMS_woDups.csv",row.names=F) # write version that doesn't have duplicates

# double check that no duplicates exist: try loading type II trajectory into adehabitat

    # remove one vessel's tracks (Pacific Future)
    noDups_VMS <- try_VMS
	pfuture <- noDups_VMS[which(noDups_VMS$Vessel_Name=="Pacific Future"),]
	require(adehabitatLT)
	da = as.POSIXct(pfuture$Date_Time, "%Y%m%d %H%M")
	ft_trj <- as.ltraj(xy = pfuture[,c("Longitude","Latitude")], date = da, id=as.character(pfuture$Vessel_Name))

# it worked! let's try a subset with a bunch of different types
subs <- noDups_VMS[1:1000,]
many_trj <- as.ltraj(xy = subs[,c("Longitude","Latitude")], date = as.POSIXct(subs$Date_Time, "%Y%m%d %H%M"), id=as.character(subs$Vessel_Name))

# i think this is good. Now excited to play with the adeHabitatLT package, but should wait until I finish the clustering stuff. This now needs to be written up a little more and formalized (move VMS_woDups.csv to a data folder with documentation)
