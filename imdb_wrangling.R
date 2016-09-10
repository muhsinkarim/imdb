##### IMDB TV Writers
# Analyse the writers of a given tv series.
# Quantify the number of episodes written per writer.
# Determine which writer's average IMDB rating.
# Track what other tv shows writers have written for.

# https://www.r-bloggers.com/r-package-to-access-the-open-movie-database-omdb-api/

#---------------------------------------------------------------------------------------------------------------------------
# Load libraries

    rm(list = ls())

    #devtools::install_github("hrbrmstr/omdbapi")
    library(omdbapi)


#---------------------------------------------------------------------------------------------------------------------------
# Module: Select tv show

    tvSeries <- "Breaking Bad"
#    tvSeries <- "Community"

    
#---------------------------------------------------------------------------------------------------------------------------
# Module: Get episodes data

        
### Create empty data frame of seasons and episodes

    colNumber <- length(find_by_title(tvSeries, type = "series", season = 1, episode = 1))
    df <- data.frame(matrix(NA, nrow = 0, ncol = colNumber))    
    colnames(df) <- colnames(find_by_title(tvSeries, type = "series", season = 1, episode = 1))
    

### Iterate through season and episodes to build data frame

    ## Parameters
    seasonArray <- 1:50 # Capped at 50 seasons        
    episodeSelection <- 1

    ## Iterate
    for (seasonSelection in seasonArray) {

        ## Break if no entries for season
        dfCheck <- find_by_title(tvSeries, type = "series", season = seasonSelection, episode = 1)
        if (length(dfCheck) == 0) break 

        ## Set dfBindAdd to prevent non zero length for next episode data
        dfBind <- find_by_title(tvSeries, type = "series", season = seasonSelection, episode = 1)
        
        ## Get data for next series of episodes
        episodeSelection <- 1 # Set to first episode for new season
        while (length(dfBind) != 0) {
            
            ## Get data for each episode
            dfBind <- find_by_title(tvSeries, type = "series", season = seasonSelection, episode = episodeSelection)

            ## Bind data to data frame
            df <- rbind.data.frame(df, dfBind)

            ## Increment episode
            episodeSelection <- episodeSelection + 1
        } 
    }


#---------------------------------------------------------------------------------------------------------------------------
# Module: Get writers data
    
                            
