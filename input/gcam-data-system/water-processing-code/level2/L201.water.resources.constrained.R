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
logstart( "L201.water.resources.constrained.R" )
printlog( "Genereate water withdrawal resource input files for region + basin which includes runoff and groundwater." )

library(dplyr)
library(tidyr)
library(tibble)
library(purrr)

# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "WATER_ASSUMPTIONS", "A_water_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
basin_ID <- readdata( "WATER_MAPPINGS", "basin_ID" )
basin_water_demand_1990_2010 <- readdata( "WATER_LEVEL0_DATA", "basin_water_demand_1990_2010" )
L100.runoff_max_bm3 <- readdata( "WATER_LEVEL1_DATA", "L100.runoff_max_bm3" )
L100.runoff_accessible <- readdata( "WATER_LEVEL1_DATA", "L100.runoff_accessible" )
L101.groundwater_grades_uniform_bm3 <- readdata( "WATER_LEVEL1_DATA", "L101.groundwater_grades_uniform_bm3" )
L101.groundwater_grades_constrained_lo_bm3 <- readdata( "WATER_LEVEL1_DATA", "L101.groundwater_grades_constrained_lo_bm3" )
L101.groundwater_grades_constrained_md_bm3 <- readdata( "WATER_LEVEL1_DATA", "L101.groundwater_grades_constrained_md_bm3" )
L101.groundwater_grades_constrained_hi_bm3 <- readdata( "WATER_LEVEL1_DATA", "L101.groundwater_grades_constrained_hi_bm3" )
L101.groundwater_grades_unconstrained_bm3 <- readdata( "WATER_LEVEL1_DATA", "L101.groundwater_grades_unconstrained_bm3" )
L101.groundwater_depletion_watergap <- readdata( "WATER_LEVEL1_DATA", "L101.groundwater_depletion_watergap" )
L101.groundwater_depletion_gleeson <- readdata( "WATER_LEVEL1_DATA", "L101.groundwater_depletion_gleeson" )
L103.water_mapping_R_GLU_B_W_Ws_share <- readdata( "WATER_LEVEL1_DATA", "L103.water_mapping_R_GLU_B_W_Ws_share" )
L103.water_mapping_R_B_W_Ws_share <- readdata( "WATER_LEVEL1_DATA", "L103.water_mapping_R_B_W_Ws_share" )


# -------------------------------------------------------------------------------

#2. Build tables
# Make a couple of tables with all of the possible technologies from the new and old structures

# Create node equivalence lists so that we can use the same subresource headers regardless
# of what type the actual resource is.
# TODO: better place for these?  they are related to headers since they list tag names
printlog( "L201.NodeEquiv: Node tag equivalence list to allow renewable header re-use under a resource tag" )
L201.NodeEquiv <- data.frame( group.name=c("Resources"), tag1=c("resource"), tag2=c("depresource"),
    tag3=c("renewresource"), tag4=c("unlimited.resource"), stringsAsFactors=FALSE )

printlog( "Create a list of region + basin that actually exist with names" )
L201.region_basin <- unique( rbind( L103.water_mapping_R_GLU_B_W_Ws_share[, c( R, B_ID, water_type )],
    L103.water_mapping_R_B_W_Ws_share[, c( R, B_ID, water_type )] ) )
L201.region_basin <- merge( L201.region_basin, GCAM_region_names )
L201.region_basin <- merge( L201.region_basin, basin_ID )
L201.region_basin <- L201.region_basin[ L201.region_basin[[water_type]] == water_W, ]

printlog( "L201.DeleteUnlimitRsrc: Create the delete for the unlimited resource markets for withdrawals" )
L201.DeleteUnlimitRsrc <- L201.region_basin
L201.DeleteUnlimitRsrc$unlimited.resource <- paste( L201.DeleteUnlimitRsrc[[B]], L201.DeleteUnlimitRsrc[[water_type]], sep="-" )
L201.DeleteUnlimitRsrc <- L201.DeleteUnlimitRsrc[, names_UnlimitRsrc[1:2] ]
L201.DeleteUnlimitRsrc <- L201.DeleteUnlimitRsrc[order(L201.DeleteUnlimitRsrc$region, L201.DeleteUnlimitRsrc$unlimited.resource ), ]

printlog( "L201.Rsrc: Create resource markets for water withdrawals" )
L201.Rsrc <- L201.region_basin
L201.Rsrc$resource <- paste( L201.Rsrc[[B]], L201.Rsrc[[water_type]], sep="-" )
L201.Rsrc$output.unit <- water_units_quantity
L201.Rsrc$price.unit <- water_units_price
names( L201.Rsrc )[ names( L201.Rsrc ) == B ] <- "market"
L201.Rsrc <- L201.Rsrc[, names_Rsrc ]
L201.Rsrc <- L201.Rsrc[order(L201.Rsrc$region, L201.Rsrc$resource ), ]

# NOTE: We are all basin characteristics in the first region in the market/basin
L201.first_basin_region <- aggregate( as.formula( paste( R, B_ID, sep="~" ) ), L201.region_basin, FUN=min )
L201.region_basin.first_only <- merge( L201.first_basin_region, L201.region_basin )

printlog( "L201.RsrcPrice: Read in base year price." )
L201.RsrcPrice <- L201.region_basin.first_only
# note we treat it as renewable eventhough it is under a regulare resource since the
# NODE_EQUIV will take care of this fact for us.
L201.RsrcPrice$renewresource <- paste( L201.RsrcPrice[[B]], L201.RsrcPrice[[water_type]], sep="-" )
L201.RsrcPrice$year <- model_years[1]
L201.RsrcPrice$price <- 0.0001
L201.RsrcPrice <- L201.RsrcPrice[, names_RenewRsrcPrice]
L201.RsrcPrice <- L201.RsrcPrice[order(L201.RsrcPrice$region, L201.RsrcPrice$renewresource, L201.RsrcPrice$year ), ]

printlog( "L201.GrdRenewRsrcMax_runoff: Read in annual water runoff supply." )
L201.region_basin.first_only %>% as_tibble() %>% 
  left_join(L100.runoff_max_bm3, by = "basin.id") %>% 
  mutate(renewresource = paste0(basin.name, "-", water_type),
         sub.renewable.resource = "runoff") %>% 
  rename(year.fillout = year,
         maxSubResource = runoff.max) %>% 
  select(region, renewresource, sub.renewable.resource, year.fillout, maxSubResource) %>% 
  arrange(region, renewresource, year.fillout) ->
  L201.GrdRenewRsrcMax_runoff
  

# ==========================================================#
# CREATE INPUTS FOR THE UNCALIBRATED WATER SUPPLY XML

printlog( "L201.RenewRsrcCurves_uncalibrated: Read graded renewable water supply curve for runoff." )

L201.region_basin.first_only %>% as_tibble() %>% 
  left_join(L100.runoff_accessible, by = "basin.id") ->
  access_fraction_uncalibrated

access_fraction_uncalibrated %>% 
  mutate(grade = "grade2",
         renewresource = paste0(basin.name, "-", water_type),
         sub.renewable.resource = "runoff") %>%
  rename(available = access_fraction) %>% 
  complete(grade = c("grade1", "grade2", "grade3"),
           nesting(region, renewresource, sub.renewable.resource)) %>% 
  mutate(available = case_when(
    grade == "grade1" ~ 0,
    grade == "grade2" ~ available,
    grade == "grade3" ~ 1
  )) %>% arrange(renewresource) %>% 
  select(region, renewresource, sub.renewable.resource, grade, available) %>% 
  mutate(extractioncost = case_when(
    grade == "grade1" ~ 0.0000100,
    grade == "grade2" ~ 0.00100,
    grade == "grade3" ~ 10.0
  )) ->
  L201.RenewRsrcCurves_uncalibrated


printlog( "L201.DepRsrcCurves_ground_uniform: Read graded depleteable ground water supply curve for uniform resources" )
L201.DepRsrcCurves_ground_uniform <- L201.region_basin.first_only
# note we treat it as depresource eventhough it is under a regular resource since the
# NODE_EQUIV will take care of this fact for us.
L201.DepRsrcCurves_ground_uniform$depresource <- paste( L201.DepRsrcCurves_ground_uniform[[B]], L201.DepRsrcCurves_ground_uniform[[water_type]], sep="-" )
L201.DepRsrcCurves_ground_uniform$subresource <- "groundwater"
L201.DepRsrcCurves_ground_uniform <- merge( L201.DepRsrcCurves_ground_uniform, L101.groundwater_grades_uniform_bm3 )
L201.DepRsrcCurves_ground_uniform <- L201.DepRsrcCurves_ground_uniform[ order( L201.DepRsrcCurves_ground_uniform$price ), ]
names(L201.DepRsrcCurves_ground_uniform)[ names(L201.DepRsrcCurves_ground_uniform) == "price" ] <- "extractioncost"
names(L201.DepRsrcCurves_ground_uniform)[ names(L201.DepRsrcCurves_ground_uniform) == "avail" ] <- "available"
L201.DepRsrcCurves_ground_uniform <- L201.DepRsrcCurves_ground_uniform[, names_DepRsrcCurves ]
L201.DepRsrcCurves_ground_uniform <- L201.DepRsrcCurves_ground_uniform[order(L201.DepRsrcCurves_ground_uniform$region, L201.DepRsrcCurves_ground_uniform$depresource ), ]

# ==========================================================#


# ==========================================================#
# CREATE INPUTS FOR THE CALIBRATED WATER SUPPLY XML (Turner et al., 2018)
# written in DSR format... ("as_tibble" commands can be removed)

# Calibration procedure (applied to both watergap and gleeson groundwater depletion estimates)
# Step 1: For basins with groundwater depletion... get historical (2000 - 2010) runoff, demand, and groundwater depletion
# Step 2: Assume no unconventional water withdrawals; back-calculate withdrawn runoff fraction using demand and groundwater depletion
# Step 3: Combine with uncalibrated accessible water (used for basins where there is no groundwater depletion historically)
# Step 4: Expand out for smooth resource curve (helps with GCAM solve)
# Step 5: Determine historical grade groundwater based to be allowed and combine with depletion curves


# Step 1

basin_water_demand_1990_2010 %>% as_tibble() %>% 
  filter(year %in% c(2005, 2010)) %>% 
  arrange(basin.id, year) %>% 
  group_by(basin.id) %>% summarise(demand = mean(demand)) ->
  basin_water_demand_2000_2010

L100.runoff_max_bm3 %>% as_tibble() %>% 
  filter(year %in% c(2005, 2010)) %>% 
  group_by(basin.id) %>% summarise(runoff = mean(runoff.max)) ->
  basin_max_runoff_2000_2010

left_join(basin_water_demand_2000_2010,
          basin_max_runoff_2000_2010,
          by = c("basin.id")) ->
  demand_runoff_cal


# Step 2

get_accessible <- function(depletion_cal){
  depletion_cal %>% 
    right_join(demand_runoff_cal, by = "basin.id") %>% 
    mutate(accessible = (demand - depletion) / runoff,
           accessible = if_else(accessible < 0, NA_real_, accessible)) %>% 
    select(basin.id, accessible) %>% as_tibble()
}

get_accessible(L101.groundwater_depletion_watergap) -> accessible_watergap
get_accessible(L101.groundwater_depletion_gleeson) -> accessible_gleeson


# Step 3
combine_accessible <- function(aw){
  L201.region_basin.first_only %>% as_tibble() %>% 
    left_join(aw, by = "basin.id") %>% 
    mutate(renewresource = paste0(basin.name, "-", water_type)) %>% 
    select(renewresource, accessible) %>% 
    right_join(L201.RenewRsrcCurves_uncalibrated, by = "renewresource") %>% 
    mutate(available = case_when(
      grade == "grade2" & is.na(accessible) == TRUE ~ available,
      grade == "grade2" & is.na(accessible) == FALSE ~ accessible,
      grade == "grade1" | grade == "grade3" ~ available
    )) %>% select(-accessible) %>% 
    group_by(renewresource) %>% mutate(x = cumsum(available)) %>% 
    mutate(available = if_else(x >= 2, x, available)) %>% select(-x) %>% ungroup()
    # ^^ fixes for cases with grade 3 availability less than grade 2 availability
}

combine_accessible(accessible_watergap)-> accessible_water_watergap_unsmoothed
combine_accessible(accessible_gleeson)-> accessible_water_gleeson_unsmoothed

# Step 4
# make function to expand out the 3-point resource curve to an interpolated 20-point curve
get_smooth_renewresource <- function(x){
  av <- x$available
  ex <- x$extractioncost
  x_region <- x$region[1]
  
  rnw_spline <- spline(av, ex, method = "hyman", 
                       xout = c(
                         seq(av[1], av[2], length.out = 10),
                         seq(av[2], av[3], length.out = 11))[-10])
  
  tibble(available = rnw_spline$x,
         extractioncost = rnw_spline$y,
         sub.renewable.resource = "runoff",
         grade = paste0("grade", 1:20),
         region = x_region)
}
  
# apply smooth across all basins by first splitting the table, then using purrr::map / map_dfr
accessible_water_watergap_unsmoothed %>% 
  split(.$renewresource) %>%
  map(get_smooth_renewresource) %>%
  map_dfr(magrittr::extract, .id = "renewresource") %>% 
  select(region, renewresource, sub.renewable.resource,
         grade, available, extractioncost) %>% 
  filter(!(grepl("Arctic Ocean", renewresource))) %>% 
  # ^^ remove convex case (Artic Ocean)
  bind_rows(filter(accessible_water_watergap_unsmoothed,
                   grepl("Arctic Ocean", renewresource))) ->
  L201.RenewRsrcCurves_calib_watergap

accessible_water_gleeson_unsmoothed %>% 
  split(.$renewresource) %>%
  map(get_smooth_renewresource) %>%
  map_dfr(magrittr::extract, .id = "renewresource") %>% 
  select(region, renewresource, sub.renewable.resource,
         grade, available, extractioncost) ->
  L201.RenewRsrcCurves_calib_gleeson


# Step 5

L100.runoff_max_bm3 %>%
  filter(year %in% c(1990, 2005, 2010)) %>%
  group_by(basin.id) %>% summarise(runoff = mean(runoff.max)) ->
  runoff_mean_hist

get_accessible_groundwater_hist <- function(x){
  
  access_fraction_uncalibrated %>% 
    select(basin.id, access_fraction) %>% 
    left_join(x, by = "basin.id") %>% 
    left_join(runoff_mean_hist, by = "basin.id") %>% 
    mutate(accessible = if_else(is.na(accessible),
                                access_fraction,
                                accessible),
           accessible_runoff = runoff * accessible) %>% 
           # ^^ get runoff volumes available
    select(basin.id, accessible_runoff) %>% 
    right_join(basin_water_demand_1990_2010, by = "basin.id") %>% 
    # ^^ join the historical demand
    mutate(deficit = demand - accessible_runoff,
           deficit = if_else(deficit <=0, 0, deficit)) %>% 
    # ^^ determine how much water needs to be met by groundwater depletion
    left_join(tibble(year = model_base_years[model_base_years >= 1990],
                     years = diff(model_base_years)), by = "year") %>% 
    mutate(deficit_total = deficit * years) %>% 
    group_by(basin.id) %>% summarise(available = sum(deficit_total)) %>% 
    filter(available > 0) %>% 
    mutate(grade = "grade hist", price = 0.001)
}

join_grade_hist <- function(groundwater_curves, accessible){
  bind_rows(
    L201.region_basin.first_only %>% as_tibble() %>% 
      left_join(groundwater_curves, by = "basin.id"),
    L201.region_basin.first_only %>% as_tibble() %>% 
      left_join(get_accessible_groundwater_hist(accessible), by = "basin.id") %>% 
      filter(is.na(grade) == F)
  ) %>% 
    rename(extractioncost = price) %>% 
    mutate(depresource = paste0(basin.name, "-", water_type),
           subresource = "groundwater",
           available = round(available, 5),
           extractioncost = round(extractioncost, 5)) %>% 
    select(region, depresource, subresource, grade, available, extractioncost) %>% 
    arrange(region, depresource, extractioncost)
}

# create depletable groundwater curves for all six add-on scenarios
# (3 groundwater availability * 2 groundwater calibration sets)

L201.DepRsrcCurves_ground_lo_wg <- join_grade_hist(L101.groundwater_grades_constrained_lo_bm3,
                                                     accessible_watergap)
L201.DepRsrcCurves_ground_md_wg <- join_grade_hist(L101.groundwater_grades_constrained_md_bm3,
                                                   accessible_watergap)
L201.DepRsrcCurves_ground_hi_wg <- join_grade_hist(L101.groundwater_grades_constrained_hi_bm3,
                                                   accessible_watergap)
L201.DepRsrcCurves_ground_lo_gl <- join_grade_hist(L101.groundwater_grades_constrained_lo_bm3,
                                                   accessible_gleeson)
L201.DepRsrcCurves_ground_md_gl <- join_grade_hist(L101.groundwater_grades_constrained_md_bm3,
                                                   accessible_gleeson)
L201.DepRsrcCurves_ground_hi_gl <- join_grade_hist(L101.groundwater_grades_constrained_hi_bm3,
                                                   accessible_gleeson)

# ==========================================================#


# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

# Uniform groundwater curves with (water constraints used in Kim et al. 2016)
write_mi_data( L201.NodeEquiv, "EQUIV_TABLE", "WATER_LEVEL2_DATA", "L201.NodeEquiv", "WATER_XML_BATCH", "batch_water_supply_uncalibrated.xml")
write_mi_data( L201.DeleteUnlimitRsrc, "DeleteUnlimitRsrc", "WATER_LEVEL2_DATA", "L201.DeleteUnlimitRsrc", "WATER_XML_BATCH", "batch_water_supply_uncalibrated.xml")
write_mi_data( L201.Rsrc, "Rsrc", "WATER_LEVEL2_DATA", "L201.Rsrc", "WATER_XML_BATCH", "batch_water_supply_uncalibrated.xml")
write_mi_data( L201.RsrcPrice, "RenewRsrcPrice", "WATER_LEVEL2_DATA", "L201.RsrcPrice", "WATER_XML_BATCH", "batch_water_supply_uncalibrated.xml")
write_mi_data( L201.RenewRsrcCurves_uncalibrated, "RenewRsrcCurves", "WATER_LEVEL2_DATA", "L201.RenewRsrcCurves_uncalibrated", "WATER_XML_BATCH", "batch_water_supply_uncalibrated.xml")
write_mi_data( L201.GrdRenewRsrcMax_runoff, "GrdRenewRsrcMaxNoFO", "WATER_LEVEL2_DATA", "L201.GrdRenewRsrcMax_runoff", "WATER_XML_BATCH", "batch_water_supply_uncalibrated.xml")
write_mi_data( L201.DepRsrcCurves_ground_uniform, "DepRsrcCurves", "WATER_LEVEL2_DATA", "L201.DepRsrcCurves_ground_uniform", "WATER_XML_BATCH", "batch_water_supply_uncalibrated.xml")
insert_file_into_batchxml( "WATER_XML_BATCH", "batch_water_supply_uncalibrated.xml", "WATER_XML_FINAL", "water_supply_uncalibrated.xml", "", "outFile" )

# add on files for calibrated accessible water and groundwater

write_mi_data( L201.RenewRsrcCurves_calib_watergap, "RenewRsrcCurves", "WATER_LEVEL2_DATA", "L201.RenewRsrcCurves_calib_watergap", "WATER_XML_BATCH", "batch_water_supply_lo_wg.xml")
write_mi_data( L201.GrdRenewRsrcMax_runoff, "GrdRenewRsrcMaxNoFO", "WATER_LEVEL2_DATA", "L201.GrdRenewRsrcMax_runoff", "WATER_XML_BATCH", "batch_water_supply_lo_wg.xml")
write_mi_data( L201.DepRsrcCurves_ground_lo_wg, "DepRsrcCurves", "WATER_LEVEL2_DATA", "L201.DepRsrcCurves_ground_lo_wg", "WATER_XML_BATCH", "batch_water_supply_lo_wg.xml")
insert_file_into_batchxml( "WATER_XML_BATCH", "batch_water_supply_lo_wg.xml", "WATER_XML_FINAL", "water_supply_lo_wg.xml", "", "outFile" )

write_mi_data( L201.RenewRsrcCurves_calib_watergap, "RenewRsrcCurves", "WATER_LEVEL2_DATA", "L201.RenewRsrcCurves_calib_watergap", "WATER_XML_BATCH", "batch_water_supply_md_wg.xml")
write_mi_data( L201.GrdRenewRsrcMax_runoff, "GrdRenewRsrcMaxNoFO", "WATER_LEVEL2_DATA", "L201.GrdRenewRsrcMax_runoff", "WATER_XML_BATCH", "batch_water_supply_md_wg.xml")
write_mi_data( L201.DepRsrcCurves_ground_md_wg, "DepRsrcCurves", "WATER_LEVEL2_DATA", "L201.DepRsrcCurves_ground_md_wg", "WATER_XML_BATCH", "batch_water_supply_md_wg.xml")
insert_file_into_batchxml( "WATER_XML_BATCH", "batch_water_supply_md_wg.xml", "WATER_XML_FINAL", "water_supply_md_wg.xml", "", "outFile" )

write_mi_data( L201.RenewRsrcCurves_calib_watergap, "RenewRsrcCurves", "WATER_LEVEL2_DATA", "L201.RenewRsrcCurves_calib_watergap", "WATER_XML_BATCH", "batch_water_supply_hi_wg.xml")
write_mi_data( L201.GrdRenewRsrcMax_runoff, "GrdRenewRsrcMaxNoFO", "WATER_LEVEL2_DATA", "L201.GrdRenewRsrcMax_runoff", "WATER_XML_BATCH", "batch_water_supply_hi_wg.xml")
write_mi_data( L201.DepRsrcCurves_ground_hi_wg, "DepRsrcCurves", "WATER_LEVEL2_DATA", "L201.DepRsrcCurves_ground_hi_wg", "WATER_XML_BATCH", "batch_water_supply_hi_wg.xml")
insert_file_into_batchxml( "WATER_XML_BATCH", "batch_water_supply_hi_wg.xml", "WATER_XML_FINAL", "water_supply_hi_wg.xml", "", "outFile" )

write_mi_data( L201.RenewRsrcCurves_calib_gleeson, "RenewRsrcCurves", "WATER_LEVEL2_DATA", "L201.RenewRsrcCurves_calib_watergap", "WATER_XML_BATCH", "batch_water_supply_lo_gl.xml")
write_mi_data( L201.GrdRenewRsrcMax_runoff, "GrdRenewRsrcMaxNoFO", "WATER_LEVEL2_DATA", "L201.GrdRenewRsrcMax_runoff", "WATER_XML_BATCH", "batch_water_supply_lo_gl.xml")
write_mi_data( L201.DepRsrcCurves_ground_lo_gl, "DepRsrcCurves", "WATER_LEVEL2_DATA", "L201.DepRsrcCurves_ground_lo_gl", "WATER_XML_BATCH", "batch_water_supply_lo_gl.xml")
insert_file_into_batchxml( "WATER_XML_BATCH", "batch_water_supply_lo_gl.xml", "WATER_XML_FINAL", "water_supply_lo_gl.xml", "", "outFile" )

write_mi_data( L201.RenewRsrcCurves_calib_gleeson, "RenewRsrcCurves", "WATER_LEVEL2_DATA", "L201.RenewRsrcCurves_calib_watergap", "WATER_XML_BATCH", "batch_water_supply_md_gl.xml")
write_mi_data( L201.GrdRenewRsrcMax_runoff, "GrdRenewRsrcMaxNoFO", "WATER_LEVEL2_DATA", "L201.GrdRenewRsrcMax_runoff", "WATER_XML_BATCH", "batch_water_supply_md_gl.xml")
write_mi_data( L201.DepRsrcCurves_ground_md_gl, "DepRsrcCurves", "WATER_LEVEL2_DATA", "L201.DepRsrcCurves_ground_md_gl", "WATER_XML_BATCH", "batch_water_supply_md_gl.xml")
insert_file_into_batchxml( "WATER_XML_BATCH", "batch_water_supply_md_gl.xml", "WATER_XML_FINAL", "water_supply_md_gl.xml", "", "outFile" )

write_mi_data( L201.RenewRsrcCurves_calib_gleeson, "RenewRsrcCurves", "WATER_LEVEL2_DATA", "L201.RenewRsrcCurves_calib_watergap", "WATER_XML_BATCH", "batch_water_supply_hi_gl.xml")
write_mi_data( L201.GrdRenewRsrcMax_runoff, "GrdRenewRsrcMaxNoFO", "WATER_LEVEL2_DATA", "L201.GrdRenewRsrcMax_runoff", "WATER_XML_BATCH", "batch_water_supply_hi_gl.xml")
write_mi_data( L201.DepRsrcCurves_ground_hi_gl, "DepRsrcCurves", "WATER_LEVEL2_DATA", "L201.DepRsrcCurves_ground_hi_gl", "WATER_XML_BATCH", "batch_water_supply_hi_gl.xml")
insert_file_into_batchxml( "WATER_XML_BATCH", "batch_water_supply_hi_gl.xml", "WATER_XML_FINAL", "water_supply_hi_gl.xml", "", "outFile" )



logstop()
