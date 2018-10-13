
## THis function will produce 20 lineups for use in draft kings daily fantasy football games
## Using a genetic algorithm and fatntasy pros projections.
##
## @param total_salary The total allowed payroll for the draft kings game, defaults to 50k
## @param dk_address the address of the draft kings salaries file. Needs to be downloaded from 
##        the DK website
## @param num_lineups the number of lineups desired
## @param dest_dir where files should be written. Just Folder Names ~ 'Week 6' or 'NFL/Week 6'
##
## @return how ever many lineups you asked for in a dataframe
##
produce_lineups <- function(total_salary = 50000, dk_address, num_lineups, dest_dir = NULL)
{
    # load the packages required
    suppressPackageStartupMessages(require(XML))
    suppressPackageStartupMessages(require(RCurl))
    suppressPackageStartupMessages(require(dplyr))
    suppressPackageStartupMessages(require(data.table))
    suppressPackageStartupMessages(require(tidyr))
    suppressPackageStartupMessages(require(genalg))
    suppressPackageStartupMessages(require(parallel))
    suppressPackageStartupMessages(require(stringi))
    
    # load required functions
    source(paste0(getwd(), '/Draft-Kings-Genetic-Solution/scrape_projections.R'))
    source(paste0(getwd(), '/Draft-Kings-Genetic-Solution/load_salaries.R'))
    source(paste0(getwd(), '/Draft-Kings-Genetic-Solution/fit_ga_model.R'))
    
    proj <- scrape_projections(0)
    sals <- load_salaries(dk_address)
    
    data <- sals %>%
        dplyr::left_join(proj) %>%
        dplyr::arrange(Name) %>%
        as.data.frame()
    
    # Writes the players with salaries getting null projections for debugging purposes
    if(is.na(dest_dir))
        null_add <- paste0(getwd(), '/null_players.csv')
    else
        null_add <- paste0(getwd(), '/', dest_dir,'/null_players.csv')
    
    write.csv(data[is.na(data$FPTS), ], null_add, 
              row.names = FALSE, quote = FALSE)
    
    ## Removes the na points players
    data <- data %>% dplyr::filter(!is.na(FPTS)) %>% as.data.frame()
    
    qb_ind <- which(data$Position == 'QB')
    rb_ind <- which(data$Position == 'RB')
    wr_ind <- which(data$Position == 'WR')
    te_ind <- which(data$Position == 'TE')
    df_ind <- which(data$Position == 'DST')
    flex_ind <- c(rb_ind, wr_ind, te_ind)
    
    ## Set the paramters for the GA
    genome_size <- nrow(data)
    population_size <- 2 * genome_size
    generations <- 2 * genome_size
    mut_chance <- 0.01
    
    ## start the timer!
    start_time <- Sys.time()
    
    ## Initialize paraellelication to make this faster
    if(is.na(dest_dir)) {
        out_file <- paste0(getwd(), '/parallel_out.txt')
    } else {
        out_file <- paste0(getwd(), '/', dest_dir,'/parallel_out.txt')
    }
    
    if (file.exists(out_file)) {
        file.remove(out_file)
    }
    
    print(paste('Starting Lineup Building - Track progress in', out_file))
    
    cores <- detectCores() - 1
    cl <- makeCluster(cores, outfile = out_file)
    
    ## You can track the output in this file by opening and updating it in notepad++
    
    ## Import the necessary vaiables to cluster
    clusterExport(cl, envir = environment(),
                  varlist =  c('genome_size', 'population_size', 'generations', 
                               'mut_chance', 'qb_ind', 'rb_ind', 'wr_ind', 'te_ind', 
                               'df_ind', 'flex_ind', 'data', 'fit_ga_model', 
                               'total_salary','start_time', 'num_lineups'))

    ## Create the desired number of lineups
    lineups <- parLapply(cl, 1:num_lineups, fun = function(i) fit_ga_model(i, getwd()))
    on.exit(stopCluster(cl))
    
    # COnvert list to dataframe
    lineups <- do.call(rbind.data.frame, lineups)
    
    ## Check the time
    end_time <- Sys.time()
    print(paste('Lineups Done!'))
    print(paste('Took', round((end_time - start_time), 2), 'Minutes'))
     
    ## Write the lineups out to a csv
    if(is.na(dest_dir))
        lineup_add <- paste0(getwd(), '/lineups.csv')
    else
        lineup_add <- paste0(getwd(), '/', dest_dir,'/lineups.csv')
    
    write.csv(lineups, lineup_add, row.names = FALSE, quote = FALSE)
    
    print(paste('Lineups available at', lineup_add))
    lineups
}
