# Setup -------------------------------------------------------------------
library(DBI)
library(sf)
library(odbc)
library(RPostgres)

# Load credentials
source("creds.R") # defines: host, port, username, password

# Connect to Postgres
pgCon <- dbConnect(
  drv = RPostgres::Postgres(),
  dbname = "BECMaster",
  host = host,
  port = port,
  user = username,
  password = password
)

# Tables to copy (Access names are capitalized)
dataTables   <- c("BECMaster_Env", "BECMaster_Veg", "BECMaster_Mineral",
                  "BECMaster_Humus", "BECMaster_Admin", "BECMaster_Metadata")
lookupTables <- c("USysAllSpecs", "USysTableOfLists")

# Snake_case converter
convert_snake_case <- function(x) {
  gsub("([a-z0-9])([A-Z])", "\\1_\\2", x) |> tolower()
}

# Copy function with schema alignment
copy_table <- function(accCon, pgCon, accName) {
  # Read from Access
  dataTable <- dbReadTable(accCon, accName)
  names(dataTable) <- convert_snake_case(names(dataTable))
  
  # Target Postgres table name: lowercase
  target <- tolower(accName)
  
  # Get Postgres column names
  pgCols <- dbListFields(pgCon, target)
  
  # Keep only matching columns
  commonCols <- intersect(names(dataTable), pgCols)
  dataTable  <- dataTable[, commonCols, drop = FALSE]
  
  # Append
  dbAppendTable(pgCon, name = DBI::SQL(target), value = dataTable)
  message("Copied table: ", target, " (", nrow(dataTable), " rows)")
}

# Truncate existing tables
allTables <- c(dataTables, lookupTables)
for (accName in allTables) {
  target <- tolower(accName)
  dbExecute(pgCon, sprintf("TRUNCATE %s RESTART IDENTITY CASCADE;", target))
  message("Truncated table: ", target)
}

# Connect to Access
accessPath <- "D:/BECMaster/BECMaster.accdb"
accCon <- dbConnect(
  odbc::odbc(),
  .connection_string = sprintf("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=%s;", accessPath)
)

# Copy both sets of tables
lapply(dataTables,   copy_table, accCon = accCon, pgCon = pgCon)
#lapply(lookupTables, copy_table, accCon = accCon, pgCon = pgCon)


# Append Lookup Table -----------------------------------------------------

accessPath <- "D:/VPro64_Latest/VPro64.accdb"
accCon <- dbConnect(odbc::odbc(), .connection_string = 
    sprintf("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=%s;",
      accessPath))
lapply(lookupTables, copy_table, accCon = accCon, pgCon = pgCon)

# lapply(lookupTables, copy_table, accCon = accCon, pgCon = pgCon,
#   prefix = '')

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
DBI::dbExecute(pgCon, "CREATE EXTENSION IF NOT EXISTS postgis;")

sf::st_write(
  forests,
  dsn   = pgCon,
  layer = "becmaster_forest_regions",
  delete_layer = TRUE
)

# 
# DBI::dbWriteTable(pgCon, value = forests, name = DBI::SQL('becmaster_forest_regions'),
#   overwrite = TRUE)

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


