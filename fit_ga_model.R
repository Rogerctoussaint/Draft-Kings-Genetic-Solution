fit_ga_model <- function(i, wd)
{
    require(genalg)
    source(paste0(wd, '/Draft-Kings-Genetic-Solution/evalFunc.R'))
    source(paste0(wd, '/Draft-Kings-Genetic-Solution/evalFuncUpside.R'))
    source(paste0(wd, '/Draft-Kings-Genetic-Solution/evalFuncDownside.R'))
    
    ## Checks for the type of fitness function we want.
    if (risky[i] == 'normal') {
        eval <- evalFunc
    } else if (risky[i] == 'upside') {
        eval <- evalFuncUpside
    } else if (risky[i] == 'downside') {
        eval <- evalFuncDownside
    }
    
    ga_model <- rbga.bin(size = genome_size,
                         popSize = population_size,
                         iters = generations,
                         mutationChance = mut_chance,
                         evalFunc = eval)
    
    solution <- ga_model$population[which.min(ga_model$evaluations),]
    team <- data[which(solution == 1), ]
    
    qb_out <- which(team$Position == 'QB')
    rb_out <- which(team$Position == 'RB')
    wr_out <- which(team$Position == 'WR')
    te_out <- which(team$Position == 'TE')
    df_out <- which(team$Position == 'DST')
    
    if(length(rb_out) == 3)
        flex_out <- rb_out[3]
    if(length(wr_out) == 4)
        flex_out <- wr_out[4]
    if(length(te_out) == 2)
        flex_out <- te_out[2]
    
    temp_lineup <- data.frame(QB_NAME = team[qb_out, 'Name'],
                              QB_PTS = team[qb_out, 'FPTS'],
                              RB1_NAME = team[rb_out[1], 'Name'],
                              RB1_PTS = team[rb_out[1], 'FPTS'],
                              RB2_NAME = team[rb_out[2], 'Name'],
                              RB2_PTS = team[rb_out[2], 'FPTS'],
                              WR1_NAME = team[wr_out[1], 'Name'],
                              WR1_PTS = team[wr_out[1], 'FPTS'],
                              WR2_NAME = team[wr_out[2], 'Name'],
                              WR2_PTS = team[wr_out[2], 'FPTS'],
                              WR3_NAME = team[wr_out[3], 'Name'],
                              WR3_PTS = team[wr_out[3], 'FPTS'],
                              TE1_NAME = team[te_out[1], 'Name'],
                              TE1_PTS = team[te_out[1], 'FPTS'],
                              FLX_NAME = team[flex_out, 'Name'],
                              FLX_PTS = team[flex_out, 'FPTS'],
                              DST_NAME = team[df_out, 'Name'],
                              DST_PTS = team[df_out, 'FPTS'],
                              TOTAL_PTS = sum(team$FPTS),
                              TOTAL_SALARY = sum(team$Salary),
                              RISKINESS = risky[i])
    
    curr_time <- Sys.time()
    print(paste('Lineup', i, 'Done After', curr_time - start_time, 'minutes'))
    ## Output the lineup
    temp_lineup
}