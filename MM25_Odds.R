setwd('~/Downloads')
library(dplyr)

elo_data <- read.csv("March Madness 2025 - Silver Elo.csv")

matchups <- list(
  South = list(
    list(c('Auburn', "Alabama State")),
    list(c("Louisville", "Creighton")),
    list(c("Michigan", "UC San Diego")),
    list(c("Texas A&M", "Yale")),
    list(c("Ole Miss", "North Carolina")),
    list(c("Iowa State", "Lipscomb")),
    list(c("Marquette", "New Mexico")),
    list(c("Michigan State", "Bryant"))
  ),
  West = list(
    list(c("Florida", "Norfolk State")),
    list(c("UConn", "Oklahoma")),
    list(c("Memphis", "Colorado State")),
    list(c("Maryland", "Grand Canyon")),
    list(c("Missouri", "Drake")),
    list(c("Texas Tech", "UNC Wilmington")),
    list(c("Kansas", "Arkansas")),
    list(c("St. John's", "Omaha"))
  ),
  East = list(
    list(c("Duke", "Mount St. Mary's")),
    list(c("Mississippi State", "Baylor")),
    list(c("Oregon", "Liberty")),
    list(c("Arizona", "Akron")),
    list(c("BYU", "VCU")),
    list(c("Wisconsin", "Montana")),
    list(c("Saint Mary's", "Vanderbilt")),
    list(c("Alabama", "Robert Morris"))
  ),
  Midwest = list(
    list(c("Houston", "SIU Edwardsville")),
    list(c("Gonzaga", "Georgia")),
    list(c("Clemson", "McNeese")),
    list(c("Purdue", "High Point")),
    list(c("Illinois", "Xavier")),
    list(c("Kentucky", "Troy")),
    list(c("UCLA", "Utah State")),
    list(c("Tennessee", "Wofford"))
  )
)

simulate_matchup <- function(team1, team2, elo_data) {
  elo1 <- elo_data$Elo[elo_data$Team == team1]
  elo2 <- elo_data$Elo[elo_data$Team == team2]
  prob_team1 <- 1 / (1 + 10^((elo2 - elo1)/400))
  return(runif(1) < prob_team1)
}

simulate_game <- function(match, elo_data) {
  if (length(match) == 3) {
    playin_winner <- ifelse(simulate_matchup(match[2], match[3], elo_data), match[2], match[3])
    ifelse(simulate_matchup(match[1], playin_winner, elo_data), match[1], playin_winner)
  } else {
    ifelse(simulate_matchup(match[1], match[2], elo_data), match[1], match[2])
  }
}

simulate_region <- function(region_bracket, elo_data) {
  current_round <- lapply(region_bracket, `[[`, 1)
  round_progress <- list()
  
  winners <- sapply(current_round, function(x) simulate_game(x, elo_data))
  round_progress$Round32 <- winners
  
  sweet16 <- split(winners, ceiling(seq_along(winners)/2))
  winners <- sapply(sweet16, function(pair) simulate_game(pair, elo_data))
  round_progress$Sweet16 <- winners

  elite8 <- split(winners, ceiling(seq_along(winners)/2))
  winners <- sapply(elite8, function(pair) simulate_game(pair, elo_data))
  round_progress$Elite8 <- winners

  final4 <- split(winners, ceiling(seq_along(winners)/2))
  winners <- sapply(final4, function(pair) simulate_game(pair, elo_data))
  round_progress$Final4 <- winners[1]
  
  return(round_progress)
}

simulate_tournament <- function(elo_data, matchups, simulations = 1000) {
  results <- data.frame(Team = elo_data$Team) %>% 
    mutate(Round_of_32 = 0,
           Sweet_16 = 0,
           Elite_8 = 0,
           Final_Four = 0,
           National_Championship = 0,
           National_Champion = 0)
  
  for (i in 1:simulations) {
    region_results <- lapply(matchups, simulate_region, elo_data)
    
    all_rounds <- lapply(region_results, function(x) {
      data.frame(
        Round32 = x$Round32,
        Sweet16 = x$Sweet16,
        Elite8 = x$Elite8,
        Final4 = x$Final4
      )
    })
    
    final_four <- unlist(lapply(region_results, function(x) x$Final4))
    national_semis <- split(final_four, ceiling(seq_along(final_four)/2))
    championship_teams <- sapply(national_semis, function(pair) simulate_game(pair, elo_data))
    
    champion <- simulate_game(championship_teams, elo_data)
    
    for (region in names(all_rounds)) {
      df <- all_rounds[[region]]
      
      results$Round_of_32[results$Team %in% df$Round32] <- results$Round_of_32[results$Team %in% df$Round32] + 1
      results$Sweet_16[results$Team %in% df$Sweet16] <- results$Sweet_16[results$Team %in% df$Sweet16] + 1
      results$Elite_8[results$Team %in% df$Elite8] <- results$Elite_8[results$Team %in% df$Elite8] + 1
      results$Final_Four[results$Team %in% df$Final4] <- results$Final_Four[results$Team %in% df$Final4] + 1
    }
    
    results$National_Championship[results$Team %in% championship_teams] <- results$National_Championship[results$Team %in% championship_teams] + 1
    results$National_Champion[results$Team == champion] <- results$National_Champion[results$Team == champion] + 1
  }
  
  results[-1] <- results[-1] / simulations
  return(results)
}

set.seed(123)
simulation_results <- simulate_tournament(elo_data, matchups, simulations = 100000)

simulation_results <- simulation_results %>%
  mutate(Seed = elo_data$Seed) %>%
  arrange(Seed,Team) %>%
  select(Team, Seed, everything())

write.csv(simulation_results, "tournament_simulation_results.csv", row.names = FALSE)

print(simulation_results)
