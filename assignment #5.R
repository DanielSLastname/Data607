# Extract player data based on structured format
players <- list()
i <- 1
while (i <= length(txt_lines)) {
  line <- str_trim(txt_lines[i])
  
  if (grepl("^\\d+ \\|", line)) {
    
    # Extract player name
    player_name <- str_trim(str_extract(line, "(?<=\\| ).*?(?= \\|)"))
    
    # Extract total points
    total_points <- as.numeric(str_extract(line, "\\d+\\.\\d"))
    
    # Extract state, USCF ID, and pre-rating
    next_line <- ifelse(i + 1 <= length(txt_lines), str_trim(txt_lines[i + 1]), NA)
    player_state <- ifelse(!is.na(next_line), str_extract(next_line, "^[A-Z]{2}"), NA)
    pre_rating <- as.numeric(str_extract(next_line, "(?<=R: )\\d+"))
    
    # Extract opponent numbers safely
    opponent_nums <- as.numeric(unlist(str_extract_all(line, "(?<=W |L |D )\\d+")))
    
    # Ensure opponent_nums is not empty
    if (length(opponent_nums) == 0) {
      opponent_nums <- NA
    }
    
    # Only store if the required fields exist
    if (!is.na(player_name) && !is.na(pre_rating) && !is.na(total_points)) {
      players[[length(players) + 1]] <- data.frame(
        Name = player_name,
        State = player_state,
        TotalPoints = total_points,
        PreRating = pre_rating,
        Opponents = list(opponent_nums)
      )
    }
    
    i <- i + 2
  } else {
    i <- i + 1
  }
}

# Combine into a data frame
players_df <- bind_rows(players)

# Display the first few rows
head(players_df)








