evalFunc <- function(x) 
{
    ## Assigns the team indexes
    team_ind <- which(x == 1)
    
    ## Calculates salary and total score
    curr_sal <- sum(data[team_ind, 'Salary'])
    curr_score <- sum(data[team_ind, 'FPTS'])
    upside <- sum(data[team_ind, 'FPTS_HIGH'] - data[team_ind, 'FPTS'])
    downside <- sum(data[team_ind, 'FPTS'] - data[team_ind, 'FPTS_LOW'])
    
    ## Gives 0 fitness if salary is too high
    if(curr_sal > total_salary) 
        return(abs(total_salary - curr_sal) * total_salary)
    ## Gives 0 fitness if team size is not 9
    if(length(team_ind) != 9) 
        return(total_salary * abs(length(team_ind) - 9))
    ## Gives 0 fitness if total of rb, wr, and te's is too high or low
    if(sum(team_ind %in% flex_ind) != 7) 
        return(abs(sum(team_ind %in% flex_ind)-7) * total_salary) 
    ## Gives 0 fitness if anything but 1 qb
    if(sum(team_ind %in% qb_ind) != 1) 
        return(total_salary)
    ## Gives 0 fitness if anything but 1 defense
    if(sum(team_ind %in% df_ind) != 1) 
        return(total_salary)
    ## Gives 0 fitness for wrong number of rbs
    if(!(sum(team_ind %in% rb_ind)) %in% c(2,3)) 
        return(total_salary)
    ## Gives 0 fitness for wrong number of wrs
    if(!(sum(team_ind %in% wr_ind)) %in% c(3,4)) 
        return(total_salary)
    ## Gives 0 fitness for wrong number of te's
    if(!(sum(team_ind %in% rb_ind)) %in% c(1,2)) 
        return(total_salary)
    
    ## return the negated score as the fitness!
    return(-curr_score)
    #return(-curr_score - (0.5 * upside))
    #return(-curr_score + (0.5 * downside))
}