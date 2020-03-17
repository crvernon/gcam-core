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
logstart( "L103.water.basin_mapping.R" )
printlog( "Calculate percentage shares to map water demands by region / sector to basin" )

# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "WATER_ASSUMPTIONS", "A_water_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
basin_ID <- readdata( "WATER_MAPPINGS", "basin_ID" )
L125.LC_bm2_R_GLU <- readdata( "AGLU_LEVEL1_DATA", "L125.LC_bm2_R_GLU" )
nonirrigation_consumption <- readdata( "WATER_LEVEL0_DATA", "nonirrigation_consumption" )
nonirrigation_withdrawal <- readdata( "WATER_LEVEL0_DATA", "nonirrigation_withdrawal" )

# -----------------------------------------------------------------------------

# 2. Process data
printlog( "Calculate shares for irrigation mappings by region + GLU + water type to basin" )
# Irrigation is pretty easy since it is just a one to one mapping from GLU to basin
L103.water_mapping_R_GLU_B_W_Ws_share.C <- unique( L125.LC_bm2_R_GLU[, c( R, "GLU" ) ] )
L103.water_mapping_R_GLU_B_W_Ws_share.C[["GLU"]] <- as.integer( sub( "GLU", "", L103.water_mapping_R_GLU_B_W_Ws_share.C[["GLU"]] ) )
L103.water_mapping_R_GLU_B_W_Ws_share.C[[B_ID]] <- L103.water_mapping_R_GLU_B_W_Ws_share.C[["GLU"]]
L103.water_mapping_R_GLU_B_W_Ws_share.C[[water_sector]] <- irr_water_sector
L103.water_mapping_R_GLU_B_W_Ws_share.C[["share"]] <- 1
L103.water_mapping_R_GLU_B_W_Ws_share.C[[water_type]] <- water_C
L103.water_mapping_R_GLU_B_W_Ws_share.W <- L103.water_mapping_R_GLU_B_W_Ws_share.C
L103.water_mapping_R_GLU_B_W_Ws_share.W[[water_type]] <- water_W
L103.water_mapping_R_GLU_B_W_Ws_share <- rbind( L103.water_mapping_R_GLU_B_W_Ws_share.C, L103.water_mapping_R_GLU_B_W_Ws_share.W )
L103.water_mapping_R_GLU_B_W_Ws_share <- L103.water_mapping_R_GLU_B_W_Ws_share[, c( R, "GLU", B_ID, water_type, water_sector, "share" ) ]

printlog( "Calculate shares for non-irrigation mappings by region + water type to basin" )
# First clean up and reshape data for processing
L103.nonirrigation_water_demand.C <- nonirrigation_consumption[, c( "GCAM_ID_1", "ISO_3DIGIT", nonirr_water_sectors ) ]
L103.nonirrigation_water_demand.C[[water_type]] <- water_C
L103.nonirrigation_water_demand.W <- nonirrigation_withdrawal[, c( "GCAM_ID_1", "ISO_3DIGIT", nonirr_water_sectors ) ]
L103.nonirrigation_water_demand.W[[water_type]] <- water_W
L103.nonirrigation_water_demand <- rbind( L103.nonirrigation_water_demand.C, L103.nonirrigation_water_demand.W )
names(L103.nonirrigation_water_demand)[1:2] <- c( B_ID, "iso" )
L103.nonirrigation_water_demand$iso <- tolower( L103.nonirrigation_water_demand$iso )
L103.nonirrigation_water_demand <- melt( L103.nonirrigation_water_demand, measure.vars=nonirr_water_sectors,
    variable.name=water_sector, value.name="demand" )
# map coutries to GCAM regions and aggregate
L103.nonirrigation_water_demand <- merge( L103.nonirrigation_water_demand, iso_GCAM_regID[, c( "iso", R ) ] )
L103.nonirrigation_water_demand <- aggregate( L103.nonirrigation_water_demand[,"demand", drop=F], by=as.list(
    L103.nonirrigation_water_demand[, c( R, B_ID, water_type, water_sector ) ] ), sum )
# convert total water demands to shares by basin to use as the region + water type + water sector to basin mapping
L103.nonirrigation_water_demand.total <- aggregate( L103.nonirrigation_water_demand[,"demand", drop=F], by=as.list(
    L103.nonirrigation_water_demand[, c( R, water_type, water_sector ) ] ), sum )
names(L103.nonirrigation_water_demand.total)[names(L103.nonirrigation_water_demand.total) == "demand"] <- "total.demand"
L103.nonirrigation_water_demand <- merge( L103.nonirrigation_water_demand, L103.nonirrigation_water_demand.total )
L103.nonirrigation_water_demand$share <- L103.nonirrigation_water_demand$demand / L103.nonirrigation_water_demand$total.demand
# clean up columns and rows with no share
L103.water_mapping_R_B_W_Ws_share <- L103.nonirrigation_water_demand[, c( R, B_ID, water_type, water_sector, "share" ) ]
L103.water_mapping_R_B_W_Ws_share <- subset( L103.water_mapping_R_B_W_Ws_share, share > 0 )

#TODO: error checking?  Missing basins, NaNs

# 3. Output

#Add comments for each table
comments.L103.water_mapping_R_GLU_B_W_Ws_share <- c( "water mapping by region / GLU / water type / water sector to basin","Unit = share" )
comments.L103.water_mapping_R_B_W_Ws_share <- c( "water mapping by region / water type / water sector to basin","Unit = share" )

#write tables as CSV files
writedata( L103.water_mapping_R_GLU_B_W_Ws_share, domain="WATER_LEVEL1_DATA", fn="L103.water_mapping_R_GLU_B_W_Ws_share", comments=comments.L103.water_mapping_R_GLU_B_W_Ws_share )
writedata( L103.water_mapping_R_B_W_Ws_share, domain="WATER_LEVEL1_DATA", fn="L103.water_mapping_R_B_W_Ws_share", comments=comments.L103.water_mapping_R_B_W_Ws_share )

# Every script should finish with this line
logstop()
