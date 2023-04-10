# Script to compile:
## 1. ITRDB study metadata
## 2. ITRDB list of raw measurement files
## 3. Dataframe of rwl files

library(dplR)
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(furrr)
plan(multisession)

# Ping WDS-Paleo API
url <- "https://www.ncei.noaa.gov/paleo-search/study/search.json?metadataOnly=true&dataPublisher=NOAA&dataTypeId=18"

resp <- GET(url)

parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"),
                   simplifyDataFrame = TRUE)

p_dat <- structure(list(
  url = url,
  content = parsed,
  response = resp
))

in_list <- unnest(p_dat$content$study, site) 


# Filter for ITRDB sites

itrdb_list <- in_list %>% 
  filter(str_detect(in_list$studyName, "ITRDB")
  )


# 1. Create ITRDB site metadata table -----------------------------------------

# Helper function
get_ITRDB_Meta <- function(itrdb_list) {
  out_df <- itrdb_list %>%
    transmute(NOAAStudyId,
              siteName,
              studyCode,
              investigators,
              first_year = earliestYearCE,
              last_year = mostRecentYearCE
    )
  out_noaa_params <- itrdb_list %>%
    transmute(NOAAStudyId,
              doi,
              contr_year = year(contributionDate),
              NOAASiteId,
              url = onlineResourceLink
    )
  # Extract Lat-Long coordinates
  out_coords <- out_df %>%
    select(NOAAStudyId) %>%
    mutate(coords = pluck(itrdb_list, "geo", "geometry", "coordinates"),
           latitude = map_dbl(coords, ~ as.numeric(.x[1])),
           longitude = map_dbl(coords, ~ as.numeric(.x[2])),
           elevation = as.numeric(pluck(itrdb_list, "geo", "properties", "minElevationMeters"))
    ) %>%
    select(- coords)
  # Extract species codes
  out_spp <- out_df %>%
    select(NOAAStudyId) %>%
    mutate(species_list = map(itrdb_list[["paleoData"]], "species"),
           species_df = map(species_list, ~.x[[1]]$speciesCode),
           species = map_chr(species_df, str_c, collapse = ", ")
    ) %>%
    select(NOAAStudyId, species)
  out_age <- out_df %>% 
    select(NOAAStudyId) %>% 
    mutate(date_type = map_chr(itrdb_list[["paleoData"]], "timeUnit")
    )
  # Extract published references
  pub_list <- unnest(itrdb_list, publication)
  if (nrow(pub_list) > 0 ) {
    out_pub <- pub_list  %>%
      select(NOAAStudyId, citation) %>%
      group_by(NOAAStudyId) %>%
      summarize(reference = str_c(citation, collapse = "; "))
  } else out_pub <- data.frame(NOAAStudyId = NA, citation = NA)
  # Combine
  itrdb_site_meta <- out_df %>%
    left_join(out_age, by = "NOAAStudyId") %>%
    left_join(out_coords, by = "NOAAStudyId") %>%
    left_join(out_spp, by = "NOAAStudyId") %>%
    left_join(out_pub, by = "NOAAStudyId") %>%
    left_join(out_noaa_params, by = "NOAAStudyId") %>% 
    filter(! is.na(studyCode))
  
  return(itrdb_site_meta)
}

itrdb_site_meta <- get_ITRDB_Meta(itrdb_list)

# 2. ITRDB list of raw measurement files -----------------------------------------------

itrdb_rawmeas_files <- itrdb_list %>% 
  select(NOAAStudyId, first_year = earliestYearCE) %>% 
  mutate(data = map(itrdb_list[["paleoData"]], "dataFile"),
         files = map(data, ~.x[[1]][c("linkText", "urlDescription", "fileUrl")]),
         vars = map(data, ~.x[[1]]$variables)
  ) %>% 
  select(-data) %>% 
  unnest(cols = c(files, vars)) %>% 
  filter(urlDescription == "Raw Measurements",
         ! str_detect(linkText, "-noaa")) %>% 
  unnest(vars) %>% 
  filter(cvWhat != "age variable>age") %>% 
  select(NOAAStudyId, first_year, linkText, fileUrl, cvWhat, cvUnit)


# 3. ITRDB RWL files ------------------------------------------------------

# Helper function
read_itrdb_rwl <- function(first_year, fileUrl) {
  require(dplR)
  if (any(c(length(first_year) > 1,
            length(fileUrl) > 1))
  ) abort("the function can only handle one case at a time")
  # read file
  if (first_year < -999) {
    read.rwl(fileUrl, long = TRUE, format = "tucson")
  } else read.rwl(fileUrl, format = "tucson")
}

# Read rwls from the HTTPS server
all_rwl <- itrdb_rawmeas_files %>% 
  transmute(NOAAStudyId,
            RWL = future_map2(first_year, fileUrl,
                       ~ try(read_itrdb_rwl(.x, .y)),
                       .options = furrr_options(seed = TRUE),
                       .progress = TRUE)
            )

# clean up
rm(in_list, itrdb_list, resp, p_dat, parsed)
