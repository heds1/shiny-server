
library(dplyr)
library(proj4)
library(tidyr)
library(data.table) # for reading in data and skipping a column

# two datasets are joined to get all the data for this app. the commuter data
# were downloaded from here: 
#
# 2018 Census Main means of travel to education by Statistical Area 2
# https://datafinder.stats.govt.nz/data/category/census/2018/commuter-view/

# these data were exported as csv and show commuter counts by Statistical Area
# (SA) 2 from the 2018 Census. there are 10 commute categories and the data is
# in a wide format. the coordinates are given in NZTM as the Easting and
# Northing of respondents' residences and workplaces (i.e., each observation
# contains four coordinates). each row contains a unique SA2 residence and
# workplace combination. for example, for a given residence SA2, there can be
# several rows corresponding to the different SA2s that people commuted to.

# however, this dataset does not contain territorial authority (TA) or regional
# council (REGC) data. in order to assign each SA2 to a particular TA or REGC, we
# download the following dataset (note that we use the 2018 dataset rather than
# 2019 or later to be consistent with the census data):

# Statistical Area 2 Higher Geographies 2018 (generalised)
# https://datafinder.stats.govt.nz/layer/95065-statistical-area-2-higher-geographies-2018-generalised/

# these data were exported as csv. the shapefile data is in the first column,
# while the remaining 9 columns contain SA2 codes and names, TA codes and
# names, and REGC codes and names, as well as shapefile metadata. in order to
# read this into R, we use data.table::fread so we can specify which columns to
# bring in (because it'll take forever to load the shapefile variable). we also
# don't really need the TA/REGC codes, just their names.

# read in the TA/REGC mapping data
df_geo <- fread(
		paste0(getwd(), "/statistical-area-2-higher-geographies-2018-generalised.csv"),
		select = c(2,5,7)) %>%
	rename("SA2Code" = 1, "REGCName" = 2, "TAName" = 3)

# set up parameters for coordinate conversion (NZTM --> NZGD)
proj4_params <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"

# read and process commuter data
df <- read.csv(paste0(getwd(), "/2018-census-main-means-of-travel-to-work-by-statistical-area.csv")) %>%
	rename(
		"ResidenceSA2Code" = 1, "ResidenceSA2Name" = 2,
		"ResidenceSA2Easting" = 3, "ResidenceSA2Northing" = 4,
		"WorkplaceSA2Code" = 5, "WorkplaceSA2Name" = 6,
		"WorkplaceSA2Easting" = 7, "WorkplaceSA2Northing" = 8,
		"WorkAtHome" = 9, "DrivePrivateVehicle" = 10,
		"DriveCompanyVehicle" = 11, "PrivatePassenger" = 12,
		"PublicBus" = 13, "Train" = 14,
		"Bicycle" = 15, "WalkOrJog" = 16,
		"Ferry" = 17, "Other" = 18, "Total" = 19) %>%

	# convert to lat/lng:
	# coordinates are in NZTM. we need to convert these to lat/lon for leaflet
	# plotting. we use the proj4 package for this, with parameters described by user
	# mkennedy from this post:
	# https://gis.stackexchange.com/questions/20389/converting-nzmg-or-nztm-to-latitude-longitude-for-use-with-r-map-library
	mutate(
		WorkplaceLng = (project(list(WorkplaceSA2Easting, WorkplaceSA2Northing), proj=proj4_params, inverse=T))$x,
		WorkplaceLat = (project(list(WorkplaceSA2Easting, WorkplaceSA2Northing), proj=proj4_params, inverse=T))$y,
		ResidenceLng = (project(list(ResidenceSA2Easting, ResidenceSA2Northing), proj=proj4_params, inverse=T))$x,
		ResidenceLat = (project(list(ResidenceSA2Easting, ResidenceSA2Northing), proj=proj4_params, inverse=T))$y) %>%
		
	# gather into long format, combining into a 'CommuteType' variable
	pivot_longer(
		cols = c(
			WorkAtHome, DrivePrivateVehicle,
			DriveCompanyVehicle, PrivatePassenger,
			PublicBus, Train,
			Bicycle, WalkOrJog,
			Ferry, Other),
		names_to = 'CommuteType',
		values_to = 'Count') %>%

	# censored values are represented by '-999' (present if there are six or
	# fewer respondents in a given observation for a given commute type to
	# protect privacy). remove these observations.
	filter(Count != -999) %>%

	# threshold the counts into bins so we can pass a weight to addPolylines
	mutate(
		Weight = case_when(
			Count <10 ~ 1,
			Count >=10 & Count <20 ~ 2,
			Count >=20 & Count <30 ~ 3,
			Count >=30 & Count <40 ~ 4,
			Count >=40 & Count <50 ~ 5,
			Count >50 ~ 6)) %>%
	
	# join with TA/REGC data. just do this by renaming the df_geo dataframe to
	# firstly add the residence TA/REGC names, then to add the workplace TA/REGC
	# names.
	left_join(
		y = rename(df_geo, ResidenceREGCName = REGCName, ResidenceTAName = TAName),
		by = c("ResidenceSA2Code" = "SA2Code")) %>%

	left_join(
		y = rename(df_geo, WorkplaceREGCName = REGCName, WorkplaceTAName = TAName),
		by = c("WorkplaceSA2Code" = "SA2Code")) %>%

	select(
		ResidenceSA2Name, WorkplaceSA2Name,
		ResidenceLng, ResidenceLat, WorkplaceLng, WorkplaceLat,
		CommuteType, Count, Weight,
		ResidenceREGCName, ResidenceTAName,
		WorkplaceREGCName, WorkplaceTAName)

write.csv(df, 'cleaned-commuter-data.csv', row.names = FALSE)
