# Draft-Kings-Genetic-Solution
Applies a genetic algorithm to solve the knapsasck 0-1 problem that is daily fantasy football. 

Enforces weight by draft kings salaries and limit, while fitness is measured by total fantasy points as projected by the fantasy pros consensus projections found at https://www.fantasypros.com/nfl/projections/qb.php

## Dependencies
This requires the following packages

- `XML`
- `RCurl`
- `dplyr`
- `data.table`
- `tidyr`
- `genalg`
- `parallel`
- `stringi`

## How to Run

First, download the Draft Kings salaries csv from the Draft Kings website to where you want to store it. Your working directory should be one level below the repo (ie `Draft-Kings-Genetic-Solution` should be folder in your wd.

The `produce_lineups()` function will write a csv with the returned lineups, as well as return the R object. `produce_lineups()` requires the following arguments.

- `total_salary` = the salary limit from draft kings. Defaults to 50,000
- `dk_file` = the path to the Draft Kings salaries file downloaded from the website.
- `num_lineups` = the number of lineups wanted
- `dest_dir` = Where you want to write output. Should be a folder in your wd, and should be passed soley as folder names with no beginning or ending '/'s (ie 'Week 6' or '2018 Season/Week 6'

## Outputs

The `produce_lineups()` function will write the following files to your `dest_dir`

- `full_lineups.csv` = These are the full lineups, including the exptected scores and salaries for each player
- `lineups.csv` = These are the lineups with just names, total salary, and total expected score
- `player_data.csv` = This is all the player salaries and projections. Good to look at players getting null point projections due to match errors on the join.
- `parallel_out.txt` = Track this file to make sure the parallelization is going swimmingly

## Notes

Parallelization is done by the `parallel` package, which the genetic algorithm is implemented by the `genalg` package. 

Since Week 6, the population size was lowered to 500 and the number of iterations was lowered 400. These were lowered to prevent over-fitting to the projections.

Additionally, one quarter of the lineups produced now assign more fitness to lineups with higher upside as measured by the highest prediction in the fantasy pros composite projection. Another quarter of lineups are considering the downside and minimizing it. 

Big Thanks to this article: https://datashoptalk.com/double-yo-money/ for the motivation/direction

## Results

Week 6: $0.56 per dollar bet - BAD, first week, moving forward
 

#### Excuses

- Need to run close to kickoff with updated projections. Got burned week 6 by Vannett being a last minute injury after inputting lineups the night before.
