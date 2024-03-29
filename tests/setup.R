# skewed by black swan tails
# means, medians sing adieu
# claims distribution
tf <- tempfile()

this_url <-	"https://www.soa.org/Files/Research/1999.zip"

download.file( this_url , tf , mode = 'wb' )

unzipped_file <- unzip( tf , exdir = tempdir() )

mlces_df <- read.csv( unzipped_file )

names( mlces_df ) <- tolower( names( mlces_df ) )
# mlces_fn <- file.path( path.expand( "~" ) , "MLCES" , "this_file.rds" )
# saveRDS( mlces_df , file = mlces_fn , compress = FALSE )
# mlces_df <- readRDS( mlces_fn )
mlces_df <- 
	transform( 
		mlces_df , 
		
		one = 1 ,
		
		claimant_relationship_to_policyholder =
			ifelse( relation == "E" , "covered employee" ,
			ifelse( relation == "S" , "spouse of covered employee" ,
			ifelse( relation == "D" , "dependent of covered employee" , NA ) ) ) ,
			
		ppo_plan = as.numeric( ppo == 'Y' )
	)
	
nrow( mlces_df )

table( mlces_df[ , "claimant_relationship_to_policyholder" ] , useNA = "always" )
mean( mlces_df[ , "totpdchg" ] )

tapply(
	mlces_df[ , "totpdchg" ] ,
	mlces_df[ , "claimant_relationship_to_policyholder" ] ,
	mean 
)
prop.table( table( mlces_df[ , "patsex" ] ) )

prop.table(
	table( mlces_df[ , c( "patsex" , "claimant_relationship_to_policyholder" ) ] ) ,
	margin = 2
)
sum( mlces_df[ , "totpdchg" ] )

tapply(
	mlces_df[ , "totpdchg" ] ,
	mlces_df[ , "claimant_relationship_to_policyholder" ] ,
	sum 
)
quantile( mlces_df[ , "totpdchg" ] , 0.5 )

tapply(
	mlces_df[ , "totpdchg" ] ,
	mlces_df[ , "claimant_relationship_to_policyholder" ] ,
	quantile ,
	0.5 
)
sub_mlces_df <- subset( mlces_df , ( ( claimyr - patbrtyr ) < 18 ) )
mean( sub_mlces_df[ , "totpdchg" ] )
var( mlces_df[ , "totpdchg" ] )

tapply(
	mlces_df[ , "totpdchg" ] ,
	mlces_df[ , "claimant_relationship_to_policyholder" ] ,
	var 
)
t.test( totpdchg ~ ppo_plan , mlces_df )
this_table <- table( mlces_df[ , c( "ppo_plan" , "patsex" ) ] )

chisq.test( this_table )
glm_result <- 
	glm( 
		totpdchg ~ ppo_plan + patsex , 
		data = mlces_df
	)

summary( glm_result )
# $0 deductible
stopifnot( nrow( mlces_df ) == 1591738 )

# $1,000 deductible
mlces_above_1000_df <- subset( mlces_df , totpdchg > 1000 )
stopifnot( nrow( mlces_above_1000_df ) == 402550 )
# $0 deductible
stopifnot( round( sum( mlces_df[ , 'totpdchg' ] ) , 0 ) == 2599356658 )

# $1,000 deductible
stopifnot( round( sum( mlces_above_1000_df[ , 'totpdchg' ] - 1000 ) , 0 ) == 1883768786 )
library(dplyr)
mlces_tbl <- as_tibble( mlces_df )
mlces_tbl %>%
	summarize( mean = mean( totpdchg ) )

mlces_tbl %>%
	group_by( claimant_relationship_to_policyholder ) %>%
	summarize( mean = mean( totpdchg ) )
library(data.table)
mlces_dt <- data.table( mlces_df )
mlces_dt[ , mean( totpdchg ) ]

mlces_dt[ , mean( totpdchg ) , by = claimant_relationship_to_policyholder ]
library(duckdb)
con <- dbConnect( duckdb::duckdb() , dbdir = 'my-db.duckdb' )
dbWriteTable( con , 'mlces' , mlces_df )
dbGetQuery( con , 'SELECT AVG( totpdchg ) FROM mlces' )

dbGetQuery(
	con ,
	'SELECT
		claimant_relationship_to_policyholder ,
		AVG( totpdchg )
	FROM
		mlces
	GROUP BY
		claimant_relationship_to_policyholder'
)
