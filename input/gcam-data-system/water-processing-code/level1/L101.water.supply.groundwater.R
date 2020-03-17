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
logstart( "L101.water.supply.groundwater.R" )
printlog( "Water supply from groundwater by basin" )

library(dplyr)
library(tidyr)
library(tibble)


# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "WATER_ASSUMPTIONS", "A_water_data", extension = ".R" )
basin_ID <- readdata( "WATER_MAPPINGS", "basin_ID" )
groundwater_uniform <- readdata( "WATER_LEVEL0_DATA", "groundwater_uniform" )
groundwater_constr_05 <- readdata( "WATER_LEVEL0_DATA", "groundwater_constrained_05pct" )
groundwater_constr_25 <- readdata( "WATER_LEVEL0_DATA", "groundwater_constrained_25pct" )
groundwater_constr_40 <- readdata( "WATER_LEVEL0_DATA", "groundwater_constrained_40pct" )
groundwater_trend_gleeson <- readdata( "WATER_LEVEL0_DATA", "groundwater_trend_gleeson" )
groundwater_trend_watergap <- readdata( "WATER_LEVEL0_DATA", "groundwater_trend_watergap" )
# -----------------------------------------------------------------------------

# 2. Compute uniform groundwater grades (see Sonny Kim)

printlog( "Calculate crustal model at various price points to convert it into a graded resource instead." )
calc_crustal_prod <- function(basin.id, price) {
    beta <- 1.0 # default, beta factor not utilized
    concentration <- 1000 # dummy value not utilized

    rsrc.params <- groundwater_uniform[ groundwater_uniform$basin.id == basin.id, ]
    return( rsrc.params$base.rsc * ( price / rsrc.params$base.prc ) ^ ( rsrc.params$alpha * beta ) - rsrc.params$base.cum )
}

# TODO: move to assumptions?
L101.num.grades <- 10
L101.max.price.increase <- 10000
# Size the output data frame for all basins and grades, the actual data will be
# filled in next.
L101.groundwater_grades_uniform_bm3 <- data.frame( basin.id=1:(L101.num.grades * max(basin_ID[[B_ID]])),
                                           price=0.0,
                                           avail=0.0 )
for( i in 1:max(basin_ID[[B_ID]]) ){
    price.points <- exp( seq( log( groundwater_uniform[ groundwater_uniform$basin.id == i, "base.prc" ] ),
                              log( groundwater_uniform[ groundwater_uniform$basin.id == i, "base.prc" ] * L101.max.price.increase ),
                               length.out=L101.num.grades ) )
    curr.offset <- (i-1) * L101.num.grades + 1
    L101.groundwater_grades_uniform_bm3[curr.offset:(curr.offset+L101.num.grades-1), B_ID] <- i
    L101.groundwater_grades_uniform_bm3[curr.offset:(curr.offset+L101.num.grades-1), "price"] <- price.points
    cumul.prod <- calc_crustal_prod( i, price.points )
    L101.groundwater_grades_uniform_bm3[curr.offset:(curr.offset+L101.num.grades-1), "avail"] <- c( cumul.prod[2:L101.num.grades], 0 )
    L101.groundwater_grades_uniform_bm3[curr.offset:(curr.offset+L101.num.grades-1), "grade"] <- paste0( "grade", 1:length(price.points) )
}

# We need to make adjustments for some basins for which we want to give enough supply to cover their
# historical use.
L101.groundwater_grades_bm3.hist <- subset( groundwater_uniform, hist.use > 0, select=c( B_ID, "hist.price", "hist.use" ) )
L101.groundwater_grades_bm3.hist$grade <- "grade hist"
names(L101.groundwater_grades_bm3.hist)[names(L101.groundwater_grades_bm3.hist) == "hist.price"] <- "price"
names(L101.groundwater_grades_bm3.hist)[names(L101.groundwater_grades_bm3.hist) == "hist.use"] <- "avail"
L101.groundwater_grades_uniform_bm3 <- rbind( L101.groundwater_grades_uniform_bm3, L101.groundwater_grades_bm3.hist[,
    names( L101.groundwater_grades_uniform_bm3 ) ] )

# 3. Process constrained groundwater grades (see Sean Turner)

# all of this data is already in correct format in Level 0
L101.groundwater_grades_constrained_lo_bm3 <- groundwater_constr_05
L101.groundwater_grades_constrained_md_bm3 <- groundwater_constr_25
L101.groundwater_grades_constrained_hi_bm3 <- groundwater_constr_40

# amend for "unconstrained" groundwater scenario
L101.groundwater_grades_unconstrained_bm3 <- subset(L101.groundwater_grades_uniform_bm3, grade == "grade1" | grade == "grade2")
L101.groundwater_grades_unconstrained_bm3[which(L101.groundwater_grades_unconstrained_bm3$grade == "grade1"),]$avail <- 10000000 
L101.groundwater_grades_unconstrained_bm3[which(L101.groundwater_grades_unconstrained_bm3$grade == "grade2"),]$avail <- 0
L101.groundwater_grades_unconstrained_bm3[which(L101.groundwater_grades_unconstrained_bm3$grade == "grade1"),]$price <- 0.01 
L101.groundwater_grades_unconstrained_bm3[which(L101.groundwater_grades_unconstrained_bm3$grade == "grade2"),]$price <- 10

# 4. Prepare groundwater depletion calibration data

# prepare watergap calibration data
groundwater_trend_watergap %>% as_tibble() %>% 
  spread(scenario, trend_km3PerYr) %>%
  mutate(human_only = hi - nhi) %>%
  filter(human_only < 0, hi < 0) %>%
  rename(depletion = human_only) %>%
  mutate(depletion = round(-depletion, 6)) %>% 
  select(basin.id, depletion) ->
  L101.groundwater_depletion_watergap

# GLEESON
groundwater_trend_gleeson %>% as_tibble() %>% 
  rename(depletion = netDepletion) %>% 
  filter(depletion > 0) %>%
  mutate(depletion = round(depletion, 6)) %>% 
  arrange(basin.id) %>% select(basin.id, depletion) ->
  L101.groundwater_depletion_gleeson


# 5. Output
#Add comments for each table
comments.L101.groundwater_grades_uniform_bm3 <- c( "graded ground water supply curve (uniform) by basin ID","Unit = billion m^3" )
comments.L101.groundwater_grades_constrained_lo_bm3 <- c( "graded ground water supply curve (constrained with 5% physically exploitable water) by basin ID","Unit = billion m^3" )
comments.L101.groundwater_grades_constrained_md_bm3 <- c( "graded ground water supply curve (constrained with 25% physically exploitable water) by basin ID","Unit = billion m^3" )
comments.L101.groundwater_grades_constrained_hi_bm3 <- c( "graded ground water supply curve (constrained with 40% physically exploitable water) by basin ID","Unit = billion m^3" )
comments.L101.groundwater_grades_unconstrained_bm3 <- c( "graded ground water supply curve (unconstrained) by basin ID","Unit = billion m^3" )
comments.L101.groundwater_depletion_watergap <- c( "year ~2000-2009  groundwater depletion by basin ID based on Watergap simulations","Unit = billion m^3 / year" )
comments.L101.groundwater_depletion_gleeson <- c( "year ~2000-2009  groundwater depletion by basin ID based on Gleeson data","Unit = billion m^3 / year" )


#write tables as CSV files
writedata( L101.groundwater_grades_uniform_bm3, domain="WATER_LEVEL1_DATA", fn="L101.groundwater_grades_uniform_bm3", comments=comments.L101.groundwater_grades_uniform_bm3 )
writedata( L101.groundwater_grades_constrained_lo_bm3, domain="WATER_LEVEL1_DATA", fn="L101.groundwater_grades_constrained_lo_bm3", comments=comments.L101.groundwater_grades_constrained_lo_bm3 )
writedata( L101.groundwater_grades_constrained_md_bm3, domain="WATER_LEVEL1_DATA", fn="L101.groundwater_grades_constrained_md_bm3", comments=comments.L101.groundwater_grades_constrained_md_bm3 )
writedata( L101.groundwater_grades_constrained_hi_bm3, domain="WATER_LEVEL1_DATA", fn="L101.groundwater_grades_constrained_hi_bm3", comments=comments.L101.groundwater_grades_constrained_hi_bm3 )
writedata( L101.groundwater_grades_unconstrained_bm3, domain="WATER_LEVEL1_DATA", fn="L101.groundwater_grades_unconstrained_bm3", comments=comments.L101.groundwater_grades_unconstrained_bm3 )
writedata( L101.groundwater_depletion_watergap, domain="WATER_LEVEL1_DATA", fn="L101.groundwater_depletion_watergap", comments=comments.L101.groundwater_depletion_watergap )
writedata( L101.groundwater_depletion_gleeson, domain="WATER_LEVEL1_DATA", fn="L101.groundwater_depletion_gleeson", comments=comments.L101.groundwater_depletion_gleeson )



# Every script should finish with this line
logstop()
