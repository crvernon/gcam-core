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
logstart( "L102.water.supply.unlimited.R" )
printlog( "Fill out assumed prices by basin / water type if users wanted to run with an unlimited supply" )

# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "WATER_ASSUMPTIONS", "A_water_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
basin_ID <- readdata( "WATER_MAPPINGS", "basin_ID" )
A_unlimited_water_price <- readdata( "WATER_ASSUMPTIONS", "A_unlimited_water_price" )

# -----------------------------------------------------------------------------

# 2. Process data

printlog( "Split water types that are mapped to basin vs just the regional level" )
L102.mapped_water_prices <- subset( A_unlimited_water_price, water_type %in% mapped_water_types )
L102.nonmapped_water_prices <- subset( A_unlimited_water_price, water_type %!in% mapped_water_types )

# some error checking that a users did not try to specify a region ID for mapped water types or
# basin ID for non-mapped.
stopifnot( !any( !is.na( L102.mapped_water_prices[[R]] ) ) )
stopifnot( !any( !is.na( L102.nonmapped_water_prices[[B_ID]] ) ) )

printlog( "Augment explictly assumed unlimited water supply prices with default where missing." )
# Fill all basins
L102.all_basin_water_type <- data.frame( basin.id=rep( basin_ID[[B_ID]], length( mapped_water_types ) ),
                                         water_type=mapped_water_types[ sort( rep( 1:length( mapped_water_types ), nrow( basin_ID ) ) ) ] )
L102.all_basin_water_type[, grep( 'X[0-9]{4}', names( L102.mapped_water_prices ), value=T ) ] <- DEFAULT_UNLIMITED_WATER_PRICE
# water withdrawals have a different default price
L102.all_basin_water_type[ L102.all_basin_water_type[[water_type]] == water_W, grep( 'X[0-9]{4}', names( A_unlimited_water_price ), value=T ) ] <- DEFAULT_UNLIMITED_IRR_WATER_PRICE
# Combine the assumed water prices with all possible basin + water types that are not already
# represented in the assumed prices.
L102.unlimited_mapped_water_price_B_W_Y_75USDm3 <- rbind( L102.mapped_water_prices, L102.all_basin_water_type[
    vecpaste( L102.all_basin_water_type[, c( B_ID, water_type ) ] ) %!in%
    vecpaste( L102.mapped_water_prices[, c( B_ID, water_type ) ] ), ] )

# Fill all regions
nonmapped_water_types <- all_water_types[all_water_types %!in% mapped_water_types]
L102.all_region_water_type <- data.frame( GCAM_region_ID=rep( GCAM_region_names[[R]], length( nonmapped_water_types ) ),
                                         water_type=nonmapped_water_types[ sort( rep( 1:length( nonmapped_water_types ), nrow( GCAM_region_names ) ) ) ] )
L102.all_region_water_type[, grep( 'X[0-9]{4}', names( L102.nonmapped_water_prices ), value=T ) ] <- DEFAULT_UNLIMITED_WATER_PRICE
# Combine the assumed water prices with all possible region + water types that are not already
# represented in the assumed prices.
L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3 <- rbind( L102.nonmapped_water_prices, L102.all_region_water_type[
    vecpaste( L102.all_region_water_type[, c( R, water_type ) ] ) %!in%
    vecpaste( L102.nonmapped_water_prices[, c( R, water_type ) ] ), ] )

printlog( "Interpolate/fillout missing years" )
L102.unlimited_mapped_water_price_B_W_Y_75USDm3 <- gcam_interp( L102.unlimited_mapped_water_price_B_W_Y_75USDm3, c( historical_years, future_years ), rule=2 )
L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3 <- gcam_interp( L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3, c( historical_years, future_years ), rule=2 )


# 3. Output

#Add comments for each table
comments.L102.unlimited_mapped_water_price_B_W_Y_75USDm3 <- c( "unlimited water supply price by basin ID / mapped water type for all years","Unit = 1975$ / m^3" )
comments.L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3 <- c( "unlimited water supply price by region ID / non-mapped water type for all years","Unit = 1975$ / m^3" )

#write tables as CSV files
writedata( L102.unlimited_mapped_water_price_B_W_Y_75USDm3, domain="WATER_LEVEL1_DATA", fn="L102.unlimited_mapped_water_price_B_W_Y_75USDm3", comments=comments.L102.unlimited_mapped_water_price_B_W_Y_75USDm3 )
writedata( L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3, domain="WATER_LEVEL1_DATA", fn="L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3", comments=comments.L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3 )

# Every script should finish with this line
logstop()
