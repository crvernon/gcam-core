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
logstart( "L202.water.resources.unlimited.R" )
printlog( "Genereate water unlimited resource input files for region + basin + water_type." )

# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "WATER_ASSUMPTIONS", "A_water_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
basin_ID <- readdata( "WATER_MAPPINGS", "basin_ID" )
L102.unlimited_mapped_water_price_B_W_Y_75USDm3 <- readdata( "WATER_LEVEL1_DATA", "L102.unlimited_mapped_water_price_B_W_Y_75USDm3" )
L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3 <- readdata( "WATER_LEVEL1_DATA", "L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3" )
L103.water_mapping_R_GLU_B_W_Ws_share <- readdata( "WATER_LEVEL1_DATA", "L103.water_mapping_R_GLU_B_W_Ws_share" )
L103.water_mapping_R_B_W_Ws_share<- readdata( "WATER_LEVEL1_DATA", "L103.water_mapping_R_B_W_Ws_share" )

# -------------------------------------------------------------------------------

#2. Build tables
# Make a couple of tables with all of the possible technologies from the new and old structures

printlog( "Create a list of region + basin that actually exist with names" )
L202.region_basin <- unique( rbind( L103.water_mapping_R_GLU_B_W_Ws_share[, c( R, B_ID, water_type )],
    L103.water_mapping_R_B_W_Ws_share[, c( R, B_ID, water_type )] ) )
L202.region_basin <- merge( L202.region_basin, GCAM_region_names )
L202.region_basin <- merge( L202.region_basin, basin_ID )

printlog( "L202.UnlimitRsrc_mapped: Create unlimited resource markets for mapped water types" )
L202.UnlimitRsrc_mapped <- L202.region_basin
L202.UnlimitRsrc_mapped$unlimited.resource <- paste( L202.UnlimitRsrc_mapped[[B]], L202.UnlimitRsrc_mapped[[water_type]], sep="-" )
L202.UnlimitRsrc_mapped$output.unit <- water_units_quantity
L202.UnlimitRsrc_mapped$price.unit <- water_units_price
# Capacity factor is not used for water resources
L202.UnlimitRsrc_mapped$capacity.factor <- 1
names( L202.UnlimitRsrc_mapped )[ names( L202.UnlimitRsrc_mapped ) == B ] <- "market"
L202.UnlimitRsrc_mapped <- L202.UnlimitRsrc_mapped[, names_UnlimitRsrc ]

printlog( "L202.UnlimitRsrc_nonmapped: Create unlimited resource markets for non-mapped water types" )
L202.UnlimitRsrc_nonmapped <- L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3[, c( R, water_type ) ]
L202.UnlimitRsrc_nonmapped <- merge( L202.UnlimitRsrc_nonmapped, GCAM_region_names )
L202.UnlimitRsrc_nonmapped$market <- L202.UnlimitRsrc_nonmapped$region
names( L202.UnlimitRsrc_nonmapped )[ names( L202.UnlimitRsrc_nonmapped ) == water_type ] <- "unlimited.resource"
L202.UnlimitRsrc_nonmapped$output.unit <- water_units_quantity
L202.UnlimitRsrc_nonmapped$price.unit <- water_units_price
# Capacity factor is not used for water resources
L202.UnlimitRsrc_nonmapped$capacity.factor <- 1
L202.UnlimitRsrc_nonmapped <- L202.UnlimitRsrc_nonmapped[, names_UnlimitRsrc ]
# Remove water goods that are only used by ag technologies, in regions with no aglu module
L202.UnlimitRsrc_nonmapped <- subset( L202.UnlimitRsrc_nonmapped,
                            !region %in% no_aglu_regions | !unlimited.resource %in% ag_only_water_types )

printlog( "L202.UnlimitRsrcPrice_mapped: Read in fixed prices for mapped water types." )
L202.unlimit_mapped_rsrc_price.melt <- melt( L102.unlimited_mapped_water_price_B_W_Y_75USDm3,
      id.vars=c( B_ID, water_type ), measure.vars=X_model_years, variable.name="year", value.name="price" )
L202.unlimit_mapped_rsrc_price.melt$year <- as.integer( gsub('^X', '', L202.unlimit_mapped_rsrc_price.melt$year ) )
# We are putting the prices in the first region in the market (basin)
L202.UnlimitRsrcPrice_mapped <- aggregate( as.formula( paste( R, B_ID, sep="~" ) ), L202.region_basin, FUN=min )
L202.UnlimitRsrcPrice_mapped <- merge( L202.UnlimitRsrcPrice_mapped, GCAM_region_names )
L202.UnlimitRsrcPrice_mapped <- merge( L202.UnlimitRsrcPrice_mapped, basin_ID )
L202.UnlimitRsrcPrice_mapped <- merge( L202.UnlimitRsrcPrice_mapped, L202.unlimit_mapped_rsrc_price.melt )
L202.UnlimitRsrcPrice_mapped$unlimited.resource <- paste( L202.UnlimitRsrcPrice_mapped[[B]], L202.UnlimitRsrcPrice_mapped[[water_type]], sep="-" )
L202.UnlimitRsrcPrice_mapped <- L202.UnlimitRsrcPrice_mapped[, names_UnlimitRsrcPrice ]

printlog( "L202.UnlimitRsrcPrice_nonmapped: Read in fixed prices for non-mapped water types." )
L202.unlimit_nonmapped_rsrc_price.melt <- melt( L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3,
      id.vars=c( R, water_type ), measure.vars=X_model_years, variable.name="year", value.name="price" )
L202.unlimit_nonmapped_rsrc_price.melt$year <- as.integer( gsub('^X', '', L202.unlimit_nonmapped_rsrc_price.melt$year ) )
L202.UnlimitRsrcPrice_nonmapped <- merge( L202.unlimit_nonmapped_rsrc_price.melt, GCAM_region_names )
names(L202.UnlimitRsrcPrice_nonmapped)[names(L202.UnlimitRsrcPrice_nonmapped) == water_type] <- "unlimited.resource"
L202.UnlimitRsrcPrice_nonmapped <- L202.UnlimitRsrcPrice_nonmapped[, names_UnlimitRsrcPrice ]

# Remove water goods that are only used by ag technologies, in regions with no aglu module
L202.UnlimitRsrcPrice_nonmapped <- subset( L202.UnlimitRsrcPrice_nonmapped,
                                 !region %in% no_aglu_regions | !unlimited.resource %in% ag_only_water_types )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

# NOTE: we are just writing the mapped and unmapped into a single XML file because we are just going to
# have the contstraint XML files include delete="1" for the appropriate unlimited files anyway.
write_mi_data( L202.UnlimitRsrc_mapped, "UnlimitRsrc", "WATER_LEVEL2_DATA", "L202.UnlimitRsrc_mapped", "WATER_XML_BATCH", "batch_unlimited_water_supply.xml")
write_mi_data( L202.UnlimitRsrc_nonmapped, "UnlimitRsrc", "WATER_LEVEL2_DATA", "L202.UnlimitRsrc_nonmapped", "WATER_XML_BATCH", "batch_unlimited_water_supply.xml")
write_mi_data( L202.UnlimitRsrcPrice_mapped, "UnlimitRsrcPrice", "WATER_LEVEL2_DATA", "L202.UnlimitRsrcPrice_mapped", "WATER_XML_BATCH", "batch_unlimited_water_supply.xml")
write_mi_data( L202.UnlimitRsrcPrice_nonmapped, "UnlimitRsrcPrice", "WATER_LEVEL2_DATA", "L202.UnlimitRsrcPrice_nonmapped", "WATER_XML_BATCH", "batch_unlimited_water_supply.xml")
insert_file_into_batchxml( "WATER_XML_BATCH", "batch_unlimited_water_supply.xml", "WATER_XML_FINAL", "unlimited_water_supply.xml", "", "outFile" )

logstop()
