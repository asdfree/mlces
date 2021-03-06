if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

library(lodown)
lodown( "mlces" , output_dir = file.path( getwd() ) )
mlces_df <- readRDS( file.path( getwd() , "mlces1997.rds" ) )

mlces_df <- 
	transform( 
		mlces_df , 
		
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
library(dplyr)
mlces_tbl <- tbl_df( mlces_df )
mlces_tbl %>%
	summarize( mean = mean( totpdchg ) )

mlces_tbl %>%
	group_by( claimant_relationship_to_policyholder ) %>%
	summarize( mean = mean( totpdchg ) )
