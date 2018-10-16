
load_salaries <- function(dk_address) 
{
    data <- fread(dk_address) %>% 
        dplyr::select(Position, Name, Salary, 'Game Info', ID) %>%
        dplyr::mutate(Name = tolower(Name)) %>%
        dplyr::rename(Game_Time = 'Game Info') %>%
        tidyr::separate(Game_Time, into = c('Matchup', 'Date', 'Time', 'Time_Zone'), sep = ' ') %>%
        dplyr::select(Name, Position, Salary, Matchup, Date, Time, ID) %>%
        as.data.frame()
    
    # the annoying name list
    data[data$Name == 'mitchell trubisky', 'Name'] <- 'mitch trubisky'
    data[data$Name %like% 'todd gurley', 'Name'] <- 'todd gurley'
    data[data$Name %like% 'will fuller', 'Name'] <- 'will fuller'
    data[data$Name %like% 'melvin gordon', 'Name'] <- 'melvin gordon'
    data[data$Name %like% 'allen robinson', 'Name'] <- 'allen robinson'
    data[data$Name %like% 'robert griffin', 'Name'] <- 'robert griffin'
    data[data$Name %like% 'paul richardson', 'Name'] <- 'paul richardson'
    data[data$Name %like% 'willie snead', 'Name'] <- 'willie snead'
    data[data$Name %like% 'joe webb', 'Name'] <- 'joe webb'
    data[data$Name %like% 'duke johnson', 'Name'] <- 'duke johnson'
    data[data$Name %like% 'd.j. chark', 'Name'] <- 'd.j. chark'
    data[data$Name %like% 'ray-ray mccloud', 'Name'] <- 'ray-ray mccloud'
    data[data$Name %like% 'sammie coates', 'Name'] <- 'sammie coates'
    data[data$Name %like% 'jojo natson', 'Name'] <- 'jojo natson'
    data[data$Name %like% 'buddy howell', 'Name'] <- 'gregory howell'
        
    data
}