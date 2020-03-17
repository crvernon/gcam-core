# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "WATERPROC_DIR" ) ){
    if( Sys.getenv( "WATERPROC" ) != "" ){
        WATERPROC_DIR <- Sys.getenv( "WATERPROC" )
    } else {
        stop("Could not determine location of water data system. Please set the R var WATERPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(WATERPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(WATERPROC_DIR,"/../_common/headers/WATER_header.R",sep=""))
logstart( "L100.water.supply.runoff.R" )
printlog( "Water supply from accessible runoff by basin" )

# libraries to allow dsr style
library(dplyr)
library(tidyr)
library(tibble)

# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "WATER_ASSUMPTIONS", "A_water_data", extension = ".R" )
basin_ID <- readdata( "WATER_MAPPINGS", "basin_ID" )
#AccessibleWater <- readdata( "WATER_LEVEL0_DATA", "AccessibleWater" )
#AccessibleWaterHIST <- readdata( "WATER_LEVEL0_DATA", "AccessibleWaterHIST" )

# read Xanthos outputs for total runoff and accessible water
xanthos_runoff <- readdata( "WATER_LEVEL0_DATA", "xanthos_basin_runoff" )
xanthos_access <- readdata( "WATER_LEVEL0_DATA", "xanthos_accessible_water" )

# -----------------------------------------------------------------------------

# 2. Process data

printlog( "Combine historical and future runoff projections and interpolate as necessary" )

# ensure that all basins are represented in the xanthos outputs
stopifnot(all.equal(sort(basin_ID[["basin.id"]]),
                    sort(xanthos_runoff[["id"]])))
stopifnot(all.equal(sort(basin_ID[["basin.id"]]),
                    sort(xanthos_access[["id"]])))

# convert data to long form
xanthos_runoff %>% as_tibble() %>% 
  gather(year, runoff, -name, -id) %>% 
  mutate(year = as.integer(substr(year, 2, 5))) %>% 
  filter(year < max(model_base_years)) ->
  runoff_1970_2009

xanthos_access %>% as_tibble() %>%  
  gather(year, accessible_water, -name, -id) %>% 
  mutate(year = as.integer(substr(year, 2, 5))) %>% 
  filter(year < max(model_base_years)) ->
  access_1970_2009

# compute the accessible fraction as the average ...
# ... of accessible / runoff for each basin
left_join(runoff_1970_2009,
          access_1970_2009,
          by = c("id", "name", "year")) %>% 
  mutate(access_fraction = accessible_water / runoff) %>%
  group_by(id) %>% summarise(access_fraction = mean(access_fraction)) %>% 
  mutate(access_fraction = round(access_fraction, 3)) %>% 
  rename(basin.id = id) ->
  L100.runoff_accessible

printlog("Use mean water supply runoff and repeat for all model years")
runoff_1970_2009 %>% 
  group_by(id) %>% 
  summarise(runoff.max = round(mean(runoff), 3)) %>% 
  rename(basin.id = id) %>% 
  mutate(year = 2000) %>% 
  complete(year = model_years, nesting(basin.id, runoff.max)) %>% 
  arrange(basin.id) ->
  L100.runoff_max_bm3


# 3. Output

#Add comments for each table
comments.L100.runoff_max_bm3 <- c( "Maximum available freshwater runoff by basin ID / Historical and future years","Unit = billion m^3" )
comments.L100.runoff_accessible <- c( "Proportion of maximum available freshwater runoff that is accessible by basin ID", "Unit = dimensionless")

#write tables as CSV files
writedata( L100.runoff_max_bm3, domain="WATER_LEVEL1_DATA", fn="L100.runoff_max_bm3", comments=comments.L100.runoff_max_bm3 )
writedata( L100.runoff_accessible, domain="WATER_LEVEL1_DATA", fn="L100.runoff_accessible", comments=comments.L100.runoff_accessible)


# Every script should finish with this line
logstop()
