  
# This function scrapes all the projections 
scrape_projections <- function(pt_limit) 
{
    # scrape each position individually
    qb_proj <- scrape('qb', pt_limit)
    rb_proj <- scrape('rb', pt_limit)
    wr_proj <- scrape('wr', pt_limit)
    te_proj <- scrape('te', pt_limit)
    kr_proj <- scrape('k',  pt_limit)
    df_proj <- scrape('dst',pt_limit)
    
    # Join them into one data frame
    data <- bind_rows(qb_proj, rb_proj, wr_proj, te_proj, kr_proj, df_proj)
    
    data # output that ish
}

## This is the helper function for scraping all the positions
scrape <- function(pos, pt_limit) 
{
    # Get the URL for the position
    url <- paste0('https://www.fantasypros.com/nfl/projections/', pos, 
                  '.php?max-yes=true&min-yes=true')
    url <- getURL(url)
    
    # Scrape the data and filter to players with more than 0 projected points
    data <- readHTMLTable(url, stringsAsFactors = FALSE)$data
    # dplyr doesn't like it when columns have the same name, so this is down with base R
    data <- data[ ,c('Player', 'FPTS')]

    ## Currently, the average, high, and low scores are all in one string in FPTS.
    ## THis next chunk will split this string and output the respectivce scores to 
    ## the right column in data
    
    temp <- data$FPTS
    temp <- strsplit(temp, split='')
    index <- lapply(temp, function(x) which(x == '.') + 1)
    
    pts <- NULL 
    for(i in 1:nrow(data)) {
        pts[i] <- as.numeric(paste0(temp[[i]][1:index[[i]][1]], collapse = ''))
    }
    
    high <- NULL
    for(i in 1:nrow(data)) {
        high[i] <- as.numeric(paste0(temp[[i]][(index[[i]][1]+1):index[[i]][2]], 
                                     collapse = ''))
    }
    
    low <- NULL
    for(i in 1:nrow(data)) {
        low[i] <- as.numeric(paste0(temp[[i]][(index[[i]][2]+1):index[[i]][3]], 
                                    collapse = ''))
    }
    
    data$FPTS <- pts
    data$FPTS_HIGH <- high
    data$FPTS_LOW <- low
    
    ## Filter out any players with 0 projected points and create position column
    data <- data %>%
        dplyr::mutate(FPTS = as.numeric(FPTS)) %>%
        dplyr::filter(FPTS > pt_limit) %>%
        dplyr::mutate(Position = toupper(pos),
                      Name = tolower(trimws(substring(Player, 1, nchar(Player)-11)))) %>%
        dplyr::select(Name, Position, FPTS, FPTS_HIGH, FPTS_LOW) %>%
        as.data.frame()
    
    ## YAY DEFENSE HANDLING
    if(data[1, 'Position'] == 'DST') {
        data[data$Name %like% 'arizona', 'Name'] <- 'cardinals'
        data[data$Name %like% 'atlanta', 'Name'] <- 'falcons'
        data[data$Name %like% 'baltimore', 'Name'] <- 'ravens'
        data[data$Name %like% 'buffalo', 'Name'] <- 'bills'
        data[data$Name %like% 'carolina', 'Name'] <- 'panthers'
        data[data$Name %like% 'chicago', 'Name'] <- 'bears'
        data[data$Name %like% 'cincinnati', 'Name'] <- 'bengals'
        data[data$Name %like% 'cleveland', 'Name'] <- 'browns'
        data[data$Name %like% 'dallas', 'Name'] <- 'cowboys'
        data[data$Name %like% 'denver', 'Name'] <- 'broncos'
        data[data$Name %like% 'green bay', 'Name'] <- 'packers'
        data[data$Name %like% 'houston', 'Name'] <- 'texans'
        data[data$Name %like% 'indianapolis', 'Name'] <- 'colts'
        data[data$Name %like% 'jacksonville', 'Name'] <- 'jaguars'
        data[data$Name %like% 'kansas city', 'Name'] <- 'cardinals'
        data[data$Name %like% 'los angeles c', 'Name'] <- 'chargers'
        data[data$Name %like% 'los angeles r', 'Name'] <- 'rams'
        data[data$Name %like% 'miami', 'Name'] <- 'dolphins'
        data[data$Name %like% 'minnesota', 'Name'] <- 'vikings'
        data[data$Name %like% 'new england', 'Name'] <- 'patriots'
        data[data$Name %like% 'new york g', 'Name'] <- 'giants'
        data[data$Name %like% 'new york j', 'Name'] <- 'jets'
        data[data$Name %like% 'oakland', 'Name'] <- 'raiders'
        data[data$Name %like% 'philadelphia', 'Name'] <- 'eagles'
        data[data$Name %like% 'pittsburgh', 'Name'] <- 'steelers'
        data[data$Name %like% 'san fran', 'Name'] <- '49ers'  # check this one next week
        data[data$Name %like% 'seattle', 'Name'] <- 'seahawks'
        data[data$Name %like% 'tampa', 'Name'] <- 'buccaneers'
        data[data$Name %like% 'tennessee', 'Name'] <- 'titans'
        data[data$Name %like% 'washington', 'Name'] <- 'redskins'
    }
    
    ## THe annoying name list
    data[data$Name %like% 'chris herndon', 'Name'] <- 'chris herndon'
    
    data # output!
}
    
