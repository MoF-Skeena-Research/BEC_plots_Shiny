library(shiny)
library(mapboxer)
#username = "becmaster_reader"; password = "BECdata"
# options(shiny.maxRequestSize = 60 * 1024 ^ 2)
con <-  DBI::dbConnect(drv = RPostgres::Postgres(), dbname = 'BECMaster', 
  host = Sys.getenv('PG_SM_HOST'), user = username, password = password)
bcgov_primary <- function() {
  c('#003366', '#fcba19', '#4c81af')
}
bcgov_text <- function() {
  c('#313132')
}
bcgov_links <- function() {
  c('#1A5A96')
}
bcgov_backgrounds <- function() {
  c('dark' = '#38598A', 'light' = '#F2F2F2')
}
bcgov_components <- function() {
  c('#606060')
}
bcgov_semantic_colors <- function() {
  c('error' = '#D8292F', 'success' = '#2E8540')
}
zissou <- function() {
  c('#3b99b1', '#56b29e', '#9fc095', '#eacb2b', '#e8a419', '#e87700', '#f5191c')
}
bcgov_theme <- bslib::bs_add_rules(
  bslib::bs_theme(version = 5, bootswatch = 'simplex',
    font_scale = .8,
    primary = bcgov_primary()[1],
    'link-color' = sprintf('%s', bcgov_links()[1]),
    'progress-height' = '1rem',
    'navbar-padding-y' = '.1rem',
    'body-color' = bcgov_text()[1]
  ),
  '.navbar.navbar-default {
    background-color: $primary !important;
  }
  body {
    background-color: #00000020;
  };
')
card <- function(...,
  title,
  class = '',
  style = '',
  headerClass = 'bg-primary') {
  if (!is.null(title)) {
    title <- shiny::tags$div(class = paste('card-header', headerClass, collapse = ' '),
      title)
  }
  shiny::tags$div(class = paste('card', class, collapse = ' ', sep = ' '),
    title,
    shiny::tags$div(class = 'card-body', ...),
    style = style
  )
}

convert_title_case <- function(x) {
  x |> 
    gsub(pattern = '_', replacement = ' ') |> 
    tools::toTitleCase() |> 
    gsub(pattern = ' ', replacement = '') |> 
    sub(pattern = 'Sv', replacement = 'SV_') |> 
    sub(pattern = '(Utm[a-z])', replacement = '\\U\\1', perl = TRUE) |> 
    sub(pattern = 'Fsr', replacement = 'FSR') |> 
    sub(pattern = 'Id', replacement = 'ID') |> 
    sub(pattern = 'Phm', replacement = 'pHM') |> 
    sub(pattern = 'Cm', replacement = 'CM') |> 
    sub(pattern = 'Xcoord', replacement = 'XCoord') |> 
    sub(pattern = 'Ycoord', replacement = 'YCoord') |> 
    sub(pattern = 'GisBgc', replacement = 'GIS_BGC') |> 
    sub(pattern = 'Ver', replacement = '_VER') |> 
    sub(pattern = 'Bec', replacement = 'BEC') |> 
    sub(pattern = 'BECUse', replacement = 'BEC_Use')
    
}
convert_insert <- function(x, con) {
  if (is.numeric(x)) {
    ifelse(is.na(x), "NULL", as.character(x))
  } else if (is.logical(x)) {
    x <- as.integer(x)
    ifelse(is.na(x), "NULL", as.character(x))
  } else {
    x <- DBI::dbQuoteString(con, as.character(x))
    x
  }
}

# Data --------------------------------------------------------------------

projectIds <- DBI::dbGetQuery(con,
  statement = 'SELECT distinct project_id	FROM public.becmaster_metadata')[['project_id']]
regions <- c(
  '',
  'Cariboo-Chilcotin' = 'RCB',
  'Kootenay-Boundary' = 'RKB',
  'Northeast' = 'RNO',
  'Omineca' = 'ROM',
  'South Coast' = 'RSC',
  'Skeena' = 'RSK',
  'Thompson-Okanagan' = 'RTO',
  'West Coast' = 'RWC'
)
zones <- c('', DBI::dbGetQuery(con, 
  statement = 'SELECT DISTINCT zone
  FROM public.becmaster_zones order by zone')[['zone']])
subzones <- DBI::dbGetQuery(con, 
  statement = 'SELECT DISTINCT zone, bgc
  FROM public.becmaster_zones order by zone, bgc')
subzones <- c('', split(subzones$bgc, subzones$zone))
dateRange <- DBI::dbGetQuery(con,
  'select date(min(date)) as min_date, date(max(date)) as max_date 
  from becmaster_env')
successionalStatus <- c(
  '',
  'Old Climax' = 'OC',
  'Maturing Climax' = 'MC',
  'Disclimax' = 'DC',
  'Young Climax' = 'YC',
  'Overmature Seral' = 'OS',
  'Maturing Seral' = 'MS',
  'Young Seral' = 'YS',
  'Pioneer Seral' = 'PS' 
) |> sort()
structuralStage <- DBI::dbGetQuery(con,
  "select item, item_description from usystableoflists
  WHERE item is not null and list_name = 'StructuralStage' AND LENGTH(item) = 2
  order by item_order")
structuralStage <- c('', stats::setNames(structuralStage$item,
  sprintf('%s (%s)', structuralStage$item_description, structuralStage$item)))
sitePlotQuality <- c('', DBI::dbGetQuery(con,
  "select item, item_description
  from usystableoflists
  WHERE list_name = 'PlotQualitySite' AND item <> 'NA'
  order by item_order")[['item']])
vegPlotQuality <- sitePlotQuality 
soilPlotQuality <- sitePlotQuality
publications <- DBI::dbGetQuery(con,
  'select distinct
  lrp.ref_short_name,
  pub.publication_short_name
  from becmaster_link_refs_plots as lrp
  inner join becmaster_references as pub on lrp.ref_short_name = pub.ref_short_name')
publications <- c('', stats::setNames(publications$ref_short_name,
  publications$publication_short_name))
locationAccuracy <- c(
  '',
  '<1m' = ' <= 1',
  '<5m' = ' <= 5',
  '<10m' = ' <= 10',
  '<30m' = ' <= 30',
  '<100m' = ' <= 100',
  '<1000m' = ' <= 1000',
  '>1000m' = ' > 1000'
)
forests <- sf::st_read(con, layer = 'becmaster_forest_regions')
