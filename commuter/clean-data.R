
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

# check whether this is local dev or production, set directories appropriately
base_dir <- ifelse(Sys.getenv("SHINY_DEV") == "True",
	"C:/Users/hls/code/shiny-server/commuter/",
	"")

# read in the TA/REGC mapping data
df_geo <- fread(
		paste0(getwd(), "/statistical-area-2-higher-geographies-2018-generalised.csv"),
		select = c(2,5,7)) %>%
	rename("SA2Code" = 1, "REGCName" = 2, "TAName" = 3)

# set up parameters for coordinate conversion (NZTM --> NZGD)
proj4_params <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"

# todo verify if this is the approach i want to take
verbose_commute_names <- list(
	"Bicycle" = "Bicycle",
	"DriveCompanyVehicle" = "Drive company vehicle",
	"DrivePrivateVehicle" = "Drive private vehicle",
	"Ferry" = "Ferry",
	"Other" = "Other",
	"PrivatePassenger" = "Private passenger",
	"PublicBus" = "Public bus",
	"Train" = "Train",
	"WalkOrJog" = "Walk or jog",
	"WorkAtHome" = "Work at home"
)

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

	# remove Area Outside data
	filter(
		ResidenceREGCName != "Area Outside Region",
		WorkplaceREGCName != "Area Outside Region") %>%

	# change to free text commute types for better labels
	mutate(CommuteType = paste0(verbose_commute_names[match(CommuteType, names(verbose_commute_names))])) %>%
	
	# remove superfluous "Region" text from REGCNames
	mutate(
		ResidenceREGCName = gsub(" Region", "", ResidenceREGCName),
		WorkplaceREGCName = gsub(" Region", "", WorkplaceREGCName)) %>%

	select(
		ResidenceSA2Name, WorkplaceSA2Name,
		ResidenceLng, ResidenceLat, WorkplaceLng, WorkplaceLat,
		CommuteType, Count, Weight,
		ResidenceREGCName, ResidenceTAName,
		WorkplaceREGCName, WorkplaceTAName)

# write.csv(df, paste0(base_dir, 'data/cleaned-commuter-data.csv'), row.names = FALSE)
# TODO: remove districts from these data if i'm not going to use them.
# also not actually writing these data, don't need them

# function to create a matrix suitable for passing to leaflet::addPolylines
# this forms a matrix with two columns, corresponding to lat and lng.
# the initial point coordinates are on the first row, the second point
# coordinates are on the second row (these are combined to form the line), and
# the third row contains c(NA, NA). the fourth row starts again with a new line
# (i.e., the next three rows is the next line).

create_polyline_matrix <- function(data) {
	data_vector <- c()
	for (row in 1:nrow(data)) {
		data_vector <- c(
			data_vector,
			as.numeric(c(
				data[row,'ResidenceLng'], data[row,'ResidenceLat'],
			 	data[row,'WorkplaceLng'], data[row,'WorkplaceLat'],
				 NA, NA)))
	}
	return (matrix(data = data_vector, ncol = 2, byrow = TRUE))
}

# create all the commute type-specific line matrices and line weights
for (commute_type in unique(df$CommuteType)) {

	these_data <- filter(df, CommuteType == commute_type)
	# write data containing commuter line weights
	write.csv(select(these_data, CommuteType, Weight),
		paste0(base_dir, 'data/line-weights/', commute_type, '.csv'),
			row.names = FALSE)

	this_matrix <- create_polyline_matrix(these_data)

	# write coordinate data into matrix to be passed to addPolylines
	write.table(this_matrix,
		paste0(base_dir, 'data/line-matrices/', commute_type, '.txt'),
		row.names = FALSE, col.names = FALSE)
}

# generate commute-type proportions by region
commute_type_proportions <- full_join(
	x = df %>%
		group_by(CommuteType) %>%
		summarise(Sum = sum(Count)) %>%
		mutate(
			Proportion = Sum / sum(Sum) * 100,
			ResidenceREGCName = "All of New Zealand"),
	y = df %>%
		group_by(ResidenceREGCName, CommuteType) %>%
		summarise(Sum = sum(Count)) %>%
		mutate(Proportion = Sum / sum(Sum) * 100))

# just to check that all the proportions have been summed correctly:
sum_list <- list()
for (i in unique(commute_type_proportions$ResidenceREGCName)) {
	sum_list[[i]] <- commute_type_proportions %>%
		filter(ResidenceREGCName == i) %>%
		summarise(sum = sum(Proportion))
}
unlist(sum_list)

write.csv(commute_type_proportions, paste0(base_dir, 'data/commute-type-proportions.csv'), row.names = FALSE)


# to get the actual shapefiles (not just TA/REGC metadata as above) in a format
# easy for leaflet to read, we download the TA and REGC data separately:

# Territorial Authority 2018 (generalised)
# https://datafinder.stats.govt.nz/layer/92214-territorial-authority-2018-generalised/

# Regional Council 2018 (generalised)
# https://datafinder.stats.govt.nz/layer/92204-regional-council-2018-generalised/

# but export as shapefiles with projection of WGS 84 (EPSG:4326)

# library(rgdal)

# TAs <- readOGR(
# 	dsn = "C:/Users/hls/code/statsnz/ta",
#     layer = "territorial-authority-2018-generalised")

# REGCs <- readOGR(
# 	dsn = "C:/Users/hls/code/statsnz/regc",
# 	layer = "regional-council-2018-generalised")

# leaflet() %>% addTiles() %>% addPolygons(data = REGCs)





# I wanted to implement autozoom when a particular region on the map was
# clicked. in order to do this, I set up an observeEvent that recorded the
# lat/lng and regional council shape id whenever it was clicked on. something
# like this, that works because you can access the click data from Shiny with
# input$MAPID_shape_click:

# coords_gather <- list()
# output$coords <- eventReactive(input$map_shape_click, {
# 	coords_gather[[input$map_shape_click$id]] <<- input$map_shape_click
# })

# from there, I just turned that list into a dataframe with some extremely bad code:

# for (i in 1:length(coords_gather)) {

# 	this_region <- coords_gather[[i]]
# 	this_id <- this_region['id']
# 	this_lat <- this_region['lat']
# 	this_lng <- this_region['lng']

# 	these_data <- data.frame(
# 		id = this_id, lat = this_lat, lng = this_lng)

# 	if (!exists('regional_coordinates')) {
# 		regional_coordinates <- these_data
# 	} else {
# 		regional_coordinates <- bind_rows(regional_coordinates, these_data)
# 	}
# }

# write.csv(regional_coordinates, 'regional-coordinates.csv', row.names = FALSE)

# test <- df %>%
# 	group_by(ResidenceREGCName, WorkplaceREGCName) %>%
# 	summarise(
# 		n = n()
# 	)

# test2 <- df %>%
# 	mutate(
# 		SameRegion = case_when(
# 			ResidenceREGCName == WorkplaceREGCName ~ 1,
# 			TRUE ~ 0
# 		)
# 	) %>%
# 	group_by(ResidenceREGCName) %>%
# 	summarise(percent_same_region = sum(SameRegion) / n())

# test3 <- df %>%
# 	mutate(
# 		SameTA = case_when(
# 			ResidenceTAName == WorkplaceTAName ~ 1,
# 			TRUE ~ 0
# 		)
# 	) %>%
# 	group_by(ResidenceTAName) %>%
# 	summarise(percent_same_ta = sum(SameTA) / n())





# 	arrange(WorkplaceREGCName, desc(n))


# 	commute_proportions_by_region %>%
# 			filter(CommuteType %in% 'Bicycle') %>%
# 			ggplot(aes(x = ResidenceREGCName, y = Percent)) +
# 				geom_boxplot()


# library(rmapshaper) # for simplify (compress polygons)
# polygon_layers[['regional']] <- rmapshaper::ms_simplify(polygon_layers[['regional']])