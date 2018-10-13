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
- `dk_file` = the name of the Draft Kings salaries file downloaded from the website. This should be placed at `~/Draft Kings/Week x/`
- `num_lineups` = the number of lineups wanted
- `dest_dir` = Where you want to write output. Should be a folder in your wd, and should be passed soley as folder names with no beginning or ending '/'s (ie 'Week 6' or '2018 Season/Week 6'

## Outputs

The `produce_lineups()` function will write the following files to your `dest_dir`

- `lineups.csv` = These are the lineups - Have fun!
- `null_players.csv` = These are the players with DK salaries that do not get projected points. Could be for a variety of reasons and this is good to check. A lot of the logic for cleaning names is bad right now and will be improved eventually.
- `parallel_out.txt` = Track this file to make sure the parallelization is going swimmingly

## Notes

Parallelization is done by the `parallel` package, which the genetic algorithm is implemented by the `genalg` package. 

The genetic algorithm is set with a population size and iterations of 2 * genome size. This may be a bit overkill and will be adjusted if runtime becomes a concern, but parallelization keeps it well under an hour. If lack of diversity at positions like quarterback becomes an issue, these parameters may be lowered. This relationship can easily be changed as we see how the algorithm performs.

Big Thanks to this article: https://datashoptalk.com/double-yo-money/ for the motivation/direction