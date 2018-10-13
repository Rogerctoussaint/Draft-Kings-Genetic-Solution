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

## How to Run

First, the folder structure should be created. As a note, this was done incredibly lazily and will be updated eventually. Create a Draft Kings folder and a Week `x` folder where you'll put the downloaded csv from draft kings. With the code in a `code` folder, you'll be good to again. Again, this will be improved when I feel like it.

The `produce_lineups()` function will write a csv with the returned lineups, as well as return the R object. `produce_lineups()` requires the following arguments.

- `total_salary` = the salary limit from draft kings. Defaults to 50,000
- `week` = NFL week, only matters for folder organization.
- `dk_file` = the name of the Draft Kings salaries file downloaded from the website. This should be placed at `~/Draft Kings/Week x/`
- `num_lineups` = the number of lineups wanted

## Notes

Parallelization is done by the `parallel` package, which the genetic algorithm is implemented by the `genalg` package. 

The genetic algorithm is set with a population size and iterations of 2 * genome size. This may be a bit overkill and will be adjusted if runtime becomes a concern, but parallelization keeps it well under an hour. If lack of diversity at positions like quarterback becomes an issue, these parameters may be lowered. This relationship can easily be changed as we see how the algorithm performs.
