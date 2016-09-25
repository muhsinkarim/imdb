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
    library(dplyr)
    library(shiny)


#---------------------------------------------------------------------------------------------------------------------------
# Module: Select tv show

    tvSeries <- "Breaking Bad"
#    tvSeries <- "Community"
#    tvSeries <- "Game of Thrones"

    
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

    
### Select columns
    
    df <- select(df, Title, Year, Rated, Released, Season, Episode, Runtime, Genre, Director, Writer, Actors, Plot, 
                 Language, Country, imdbRating, imdbVotes, imdbID, seriesID)

    
#---------------------------------------------------------------------------------------------------------------------------
# Module: Get writers data
    

### Create data frame with episode writer on each line
    
    ## Create writers data frame
    dfSubset <- select(df, imdbID, Writer)
    
    ## Create empty data frame
    dfWriters <- data.frame(matrix(NA, nrow = 0, ncol = 3))
    colnames(dfWriters) <- c(colnames(dfSubset), "Is writer")
    
    
    ## Iterate through each row
    for (i in 1:nrow(dfSubset)) {
        
        ## Get writers
        theWriters <- dfSubset$Writer[i]
        
        ## Split writers
        theWriters <- unlist(strsplit(theWriters, ","))
        
        ## Place each writer on a seperate line
        for (j in theWriters) {
            
            ## Check if writer of episode
            # If brackets present, likely not a writer
            if (grepl("\\)" , j)) {
                isWriters <- "No" # Not writer of episode
            } else {
                isWriters <- "Yes" # Writer of episode
            }
                        
            ## Get the Id, Writer and Is writer
            dfWritersAdd <- t(as.data.frame(c(df$imdbID[i], j, isWriters)))
            rownames(dfWritersAdd) <- NULL        
            colnames(dfWritersAdd) <- c(colnames(dfSubset), "isWriter")

            ## Bind
            dfWriters <- rbind.data.frame(dfWriters, dfWritersAdd)
        }
    }
   
     
### Create writers data frame with writer on each line
    
    ## Remove Writer column from df
    dfTemp <- select(df, -Writer)
    
    ## Merge data frames
    dfWriters <- left_join(dfWriters, dfTemp, by = "imdbID")
    
    ## Remove leading and trailing whitespace
    dfWriters$Writer <- gsub("^\\s+|\\s+$", "", dfWriters$Writer)
        
    ## Rearange columns
    dfWriters <- dfWriters %>%
        select(
            Title:Director,
            Writer,
            isWriter,
            Actors:imdbVotes,
            imdbID,
            seriesID
        )
    
    
#---------------------------------------------------------------------------------------------------------------------------
# Module: Plots

### Try latest Github version of ggraptR    
    
    #devtools::install_github('cargomoose/raptR', force = TRUE) # install
    library("ggraptR") 
    
    
### Number of episodes per writer
    
    ## Group by writer
    dfGroup <-
        dfWriters %>%
        filter(isWriter == "Yes") %>%
        group_by(Writer) %>%
        summarise(Count = n())

    ## Order by most frequent writer
    dfGroup <- dfGroup[order(dfGroup$Count, decreasing = T), ]
    
    
    ggraptR()
    
    
    dfGroup$Writer <- reorder(dfGroup$Writer, dfGroup$Count)
    
    ggplot(dfGroup, 
           aes(y = Count, x = Writer)) + 
        geom_bar(stat = "identity", position = "identity", alpha = 0.5, fill = "#06602B") + 
        coord_flip() + 
        theme(text = element_text(color = "black", size = 15, hjust = 0.5, vjust = 0.5)) + 
        ggtitle("Breaking Bad episodes written per writer") +
        xlab("Awesome BB writer") + ylab("Number of episodes")
    
    
    
    
    ggplot(dfGroup, 
           aes(y = Count, x =  Writer)) + 
        geom_bar(stat = "identity", 
                 position = "identity", 
                 alpha = 0.5) + 
        coord_flip() + 
        theme_grey() + 
        theme(text = element_text(color = "black", size = 15, hjust = 0.5, vjust = 0.5)) + 
        xlab("Writer") + ylab("Count")
    