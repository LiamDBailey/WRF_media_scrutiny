# Load required libraries ####
library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(here)

#Work out keywords that could be used to extract themes
focal_themes <- readr::read_csv(here::here("./data/WRF_focal_themes.csv"))

# Example of data scraping ####

## DOWNLOAD INDEX OF FILES ####
all_files <- readr::read_delim("http://data.gdeltproject.org/gdeltv2/masterfilelist.txt", col_names = FALSE) %>%
  setNames(nm = c("X1", "UUID", "URL"))

## FILTER ONLY GKG FILES ####
gkg_files <- all_files %>%
  dplyr::filter(stringr::str_detect(URL, ".gkg.")) %>%
  #Include date-time of file
  dplyr::mutate(date_time_string = stringr::str_extract(URL, pattern = "[0-9]{14}"),
                date_time = lubridate::ymd_hms(date_time_string))

## SELECT FOCAL DAY ####
focal_files <- gkg_files %>%
  dplyr::filter(date_time >= lubridate::ymd("2021-07-01") & date_time < lubridate::ymd("2021-07-02"))

## LOOP THROUGH FILES
#Takes approx 3.5min
system.time({example_scrape <- purrr::map_df(.x = 1:nrow(focal_files),
                                .f = function(i){
                                  
                                  url <- focal_files$URL[i]
                                  date_time_string <- focal_files$date_time_string[i]
                                  
                                  #Download and unzip file
                                  download.file(url, destfile = "./tmp/focal_gkg_file.zip")
                                  unzip(zipfile = "./tmp/focal_gkg_file.zip", exdir = "./tmp")
                                  
                                  #Find tmp file of .csv
                                  #Must match datetime string just incase there are other .csv files available
                                  focal_csv <- list.files(path = "./tmp", pattern = date_time_string, full.names = TRUE)
                                  
                                  #Load file
                                  gkg_data <- readr::read_tsv(focal_csv,
                                                              col_names = c("GKGRECORDID", "DATE", "SOURCECOLLECTIONIDENTIFIER",
                                                                            "SOURCECOMMONNAME", "DOCUMENTIDENTIFIER",
                                                                            "COUNTS", "V2.1COUNTS", "THEMES", "ENHANCEDTHEMES",
                                                                            "LOCATIONS", "ENHANCEDLOCATIONS", "PERSONS",
                                                                            "ENHANCEDPERSONS", "ORGANIZATIONS", "ENHANCEDORGANIZATIONS",
                                                                            "TONE", "ENHANCEDDATES", "GCAM", "SHARINGIMAGE",
                                                                            "RELATEDIMAGES", "SOCIALIMAGEEMBEDS", "SOCIALVIDEOEMBEDS",
                                                                            "QUOTATIONS", "ALLNAMES", "AMOUNTS", "TRANSLATIONINFO",
                                                                            "EXTRASXML"),
                                                              col_types = list(.default = col_character()), lazy = FALSE) %>%
                                    #Filter only those that meet our set of focal themes
                                    dplyr::filter(stringr::str_detect(ENHANCEDTHEMES, paste(focal_themes$focal_theme, collapse = "|")))
                                  
                                  #Clear temp files
                                  do.call(file.remove, list(list.files("./tmp", full.names = TRUE)))
                                  
                                  return(gkg_data)
                                  
                                })})

## FILTER RECORDS WITH NO LOCATION AND EXTRACT LAT/LONG ####
#We assume that first lat/long is the location of the event
example_scrape_filter <- example_scrape %>%
  #Filter out those without a location
  dplyr::filter(!is.na(LOCATIONS)) %>%
  rowwise() %>%
  #Extract first set of coordinates
  dplyr::mutate(lat = as.numeric(stringr::str_extract_all(LOCATIONS, pattern = "[0-9]*\\.[0-9]*")[[1]][1]),
                long = -as.numeric(stringr::str_extract_all(LOCATIONS, pattern = "[0-9]*\\.[0-9]*")[[1]][2]))

## CREATE AN SF OBJECT ####
example_scrape_sf <- example_scrape_filter %>%
  #Remove missing lat/long
  dplyr::filter(!is.na(lat) & !is.na(long)) %>%
  sf::st_as_sf(., coords = c("long", "lat"))

## PLOT DATA ####
world_data <- ggplot2::map_data("world")

ggplot() +
  geom_polygon(data = world_data, aes(x = long, y = lat, group = group)) +
  geom_sf(data = example_scrape_sf)
