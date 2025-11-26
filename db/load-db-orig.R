# Setup -------------------------------------------------------------------


library(DBI) 
require(sf)
# create an R script with read/write privileged credentials:
# username <- ""
# password <- ""
source("creds.R") # username, password for pg
pgCon <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = 'BECMaster', 
  host = host,port = port, user = username, password = password)
dataTables <- c("env", "veg", "mineral", "humus", "admin", "metadata")
lookupTables <- c("USysAllSpecs", "USysTableOfLists")
copy_table <- function(accCon, pgCon, name, prefix = '') {
  dataTable <- DBI::dbReadTable(conn = accCon, name = sprintf('%s%s', prefix, 
    name))
  names(dataTable) <- convert_snake_case(names(dataTable))
  if (prefix == 'BECMaster64_') {
    prefix <- 'BECMaster_'
  }
  DBI::dbAppendTable(conn = pgCon, name = DBI::SQL(sprintf('%s%s', prefix, name)), 
    value = dataTable)
}

convert_snake_case <- function(x) {
  gsub('([a-z]{2,})([A-Z])', '\\1\\2', x) |> 
    tolower()
}

dbExecute(pgCon, "DELETE FROM becmaster_env")
dbExecute(pgCon, "DELETE FROM becmaster_veg")
dbExecute(pgCon, "DELETE FROM becmaster_mineral")
dbExecute(pgCon, "DELETE FROM becmaster_humus")
dbExecute(pgCon, "DELETE FROM becmaster_admin")
dbExecute(pgCon, "DELETE FROM becmaster_metadata")
# Append Data Tables ------------------------------------------------------

accessPath <- "D:/BECMaster/BECMaster.accdb"

accCon <- dbConnect(odbc::odbc(), .connection_string = 
    sprintf("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=%s;",
      accessPath))
lapply(dataTables, copy_table, accCon = accCon, pgCon = pgCon,
  prefix = 'BECMaster_')

# Append Lookup Table -----------------------------------------------------

accessPath <- "D:/VPro64_Latest/VPro64.accdb"
accCon <- dbConnect(odbc::odbc(), .connection_string = 
    sprintf("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=%s;",
      accessPath))
lapply(lookupTables, copy_table, accCon = accCon, pgCon = pgCon,
  prefix = '')

# Write Zones Table -------------------------------------------------------

zones <- data.table::fread('data/BC_BGCs_Info_v12_15.csv')
data.table::setnames(zones, tolower)
DBI::dbWriteTable(pgCon, name = DBI::SQL('becmaster_zones'), value = zones,
  overwrite = TRUE)

# Write Pub Refs ----------------------------------------------------------

accessPath <- "data/tbl_BEC_References.accdb"
accCon <- dbConnect(odbc::odbc(), .connection_string = 
    sprintf("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=%s;",
      accessPath))
references <- DBI::dbReadTable(accCon, name = 'tblBEC_References')
names(references)[1] <- 'id'
names(references) <- tolower(names(references))
DBI::dbAppendTable(pgCon, name = DBI::SQL('becmaster_references'),
  value = references)
pubs <- data.table::fread('data/AllPublished_LMH_SU.csv')
names(pubs) <- c('plot_number', 'site_unit', 'ref_short_name')
Encoding(pubs$site_unit)
pubs$site_unit <- iconv(pubs$site_unit, "latin1", "UTF-8")
DBI::dbWriteTable(pgCon, name = DBI::SQL('becmaster_link_refs_plots'),
  value = pubs, overwrite = TRUE)


# Add Forest Regions ------------------------------------------------------

forests <- sf::st_read('data/ForestRegions_simplified2.gpkg') |> 
  sf::st_transform(4326)
names(forests) <- tolower(names(forests))
leaflet::leaflet(forests) |> 
  leaflet::addTiles() |> 
  leaflet::addPolygons()
DBI::dbWriteTable(pgCon, value = forests, name = DBI::SQL('becmaster_forest_regions'),
  overwrite = TRUE)

# forests <- sf::st_read('data/arctic_bgcs.gpkg') |> 
#   sf::st_transform(4326)|> st_zm()
# names(forests) <- tolower(names(forests))
# leaflet::leaflet(forests) |> 
#   leaflet::addTiles() |> 
#   leaflet::addPolygons()
# forests <- sf::st_read('data/arctic_bgcs.gpkg') |> 
#   sf::st_transform(4326)
# 
# DBI::dbWriteTable(pgCon, value = forests, name = DBI::SQL('becmaster_forest_regions_test'),
#                   overwrite = TRUE)

# Grant Privileges --------------------------------------------------------

DBI::dbExecute(pgCon, 
  'GRANT SELECT ON ALL TABLES IN SCHEMA public to becmaster_reader')

# Fin ---------------------------------------------------------------------


