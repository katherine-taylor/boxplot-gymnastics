library(tidyverse)

clean_gym_data <- function(results_vector) {
  gymnast_first_name <- c()
  gymnast_last_name <- c()
  rank <- c()
  comp_number <- c()
  gym <- c()
  vault_d <- c()
  vault_e <- c()
  vault_nd <- c()
  vault_score <- c()
  bars_d <- c()
  bars_e <- c()
  bars_nd <- c()
  bars_score <- c()
  beam_d <- c()
  beam_e <- c()
  beam_nd <- c()
  beam_score <- c()
  floor_d <- c()
  floor_e <- c()
  floor_nd <- c()
  floor_score <- c()
  aa_score <- c()

  for (i in 1:(length(results_vector) / 5)) {
    row_1 <- str_split(results_vector[5 * (i - 1) + 1], pattern = " ") |>
      unlist()
    gymnast_first_name <- append(gymnast_first_name, row_1[3])
    gymnast_last_name <- append(gymnast_last_name, row_1[4])
    rank <- append(rank, row_1[1])
    comp_number <- append(comp_number, parse_number(row_1[2]))
    vault_d <- append(vault_d, row_1[6])
    bars_d <- append(bars_d, row_1[7])
    beam_d <- append(beam_d, row_1[8])
    floor_d <- append(floor_d, row_1[9])

    row_2 <- str_split(results_vector[5 * (i - 1) + 2], pattern = " ") |>
      unlist()
   
    if (!str_equal(row_2[4],"Exec:") & !str_equal(row_2[3],"Exec:")) {
       
      gym <- append(gym, glue(row_2[2], row_2[3], row_2[4], .sep = " "))
      vault_e <- append(vault_e, row_2[6])
      bars_e <- append(bars_e, row_2[7])
      beam_e <- append(beam_e, row_2[8])
      floor_e <- append(floor_e, row_2[9])
    } else if (!str_equal(row_2[3],"Exec:")) {
     
      gym <- append(gym, glue(row_2[2], row_2[3], .sep = " "))
      vault_e <- append(vault_e, row_2[5])
      bars_e <- append(bars_e, row_2[6])
      beam_e <- append(beam_e, row_2[7])
      floor_e <- append(floor_e, row_2[8])
    } else {
        
      gym <- append(gym, row_2[2])
      vault_e <- append(vault_e, row_2[4])
      bars_e <- append(bars_e, row_2[5])
      beam_e <- append(beam_e, row_2[6])
      floor_e <- append(floor_e, row_2[7])
    }

    row_3 <- str_split(results_vector[5 * (i - 1) + 3], pattern = " ") |>
      unlist()

    vault_nd <- append(vault_nd, row_3[3])
    bars_nd <- append(bars_nd, row_3[4])
    beam_nd <- append(beam_nd, row_3[5])
    floor_nd <- append(floor_nd, row_3[6])

    row_4 <- str_split(results_vector[5 * (i - 1) + 4], pattern = " ") |>
      unlist()

    vault_score <- append(vault_score, row_4[3])
    bars_score <- append(bars_score, row_4[4])
    beam_score <- append(beam_score, row_4[5])
    floor_score <- append(floor_score, row_4[6])
    aa_score <- append(aa_score, row_4[7])
  }

  us_senior_night_1_22 <- tibble(gymnast_first_name, gymnast_last_name, rank, comp_number, gym, vault_d, vault_e, vault_nd, vault_score, bars_d, bars_e, bars_nd, bars_score, beam_d, beam_e, beam_nd, beam_score, floor_d, floor_e, floor_nd, floor_score, aa_score)

  return(us_senior_night_1_22)
}

clean_worlds_data <- function(results_vector){
    gymnast_first_name <- c()
    gymnast_last_name <- c()
    comp_number <- c()
    vault_d <- c()
    vault_e <- c()
    vault_score <- c()
    bars_d <- c()
    bars_e <- c()
    bars_score <- c()
    beam_d <- c()
    beam_e <- c()
    beam_score <- c()
    floor_d <- c()
    floor_e <- c()
    floor_score <- c()
    for(i in 1:5){
        
        row_1 <- str_split(results_vector[2*(i-1)+2], " ") |> unlist()
        
        row_2 <- str_split(results_vector[2*(i-1)+3], " ") |>  unlist()
        
        gymnast_first_name <- append(gymnast_first_name, row_1[4])
        gymnast_last_name <- append(gymnast_last_name, row_1[3])
        comp_number <- append(comp_number,row_1[2])
        
        if(length(row_1) < 13 & length(row_1) > 10){
        vault_d <- append(vault_d, NA)
        vault_e <- append(vault_e, NA)
        vault_score <- append(vault_score, NA)
        bars_d <- append(bars_d, row_1[6])
        bars_e <- append(bars_e, row_2[3])
        bars_score <- append(bars_score, row_1[7])
        beam_d <- append(beam_d, row_1[8])
        beam_e <- append(beam_e, row_2[4])
        beam_score <- append(beam_score, row_1[9])
        floor_d <- append(floor_d,row_1[10] )
        floor_e <- append(floor_e, row_2[5])
        floor_score <- append(floor_score, row_1[11])
        }
        else if(length(row_1) < 10){
            vault_d <- append(vault_d, row_1[6])
            vault_e <- append(vault_e, row_2[3])
            vault_score <- append(vault_score, row_1[7])
            bars_d <- append(bars_d, NA)
            bars_e <- append(bars_e, NA)
            bars_score <- append(bars_score, NA)
            beam_d <- append(beam_d, NA)
            beam_e <- append(beam_e, NA)
            beam_score <- append(beam_score, NA)
            floor_d <- append(floor_d, NA)
            floor_e <- append(floor_e, NA)
            floor_score <- append(floor_score, NA)
        }
        else{
            vault_d <- append(vault_d, row_1[6])
            vault_e <- append(vault_e, row_2[3])
            vault_score <- append(vault_score, row_1[7])
            bars_d <- append(bars_d, row_1[8])
            bars_e <- append(bars_e, row_2[4])
            bars_score <- append(bars_score, row_1[9])
            beam_d <- append(beam_d, row_1[10])
            beam_e <- append(beam_e, row_2[5])
            beam_score <- append(beam_score, row_1[11])
            floor_d <- append(floor_d,row_1[12])
            floor_e <- append(floor_e, row_2[6])
            floor_score <- append(floor_score, row_1[13])
        }
        
        
    }
    world_scores <- tibble(gymnast_first_name, gymnast_last_name, comp_number, vault_d, vault_e, vault_score, bars_d, bars_e, bars_score, beam_d, beam_e, beam_score, floor_d, floor_e, floor_score)
    return(world_scores)
}

clean_worlds_data_23 <- function(results_vector){
    gymnast_first_name <- c()
    gymnast_last_name <- c()
    comp_number <- c()
    vault_d <- c()
    vault_e <- c()
    vault_score <- c()
    bars_d <- c()
    bars_e <- c()
    bars_score <- c()
    beam_d <- c()
    beam_e <- c()
    beam_score <- c()
    floor_d <- c()
    floor_e <- c()
    floor_score <- c()
    
    # Simone
    row_1 <- str_split(results_vector[2], " ") |> unlist()
    
    row_2 <- str_split(results_vector[3], " ") |>  unlist()
    
    gymnast_first_name <- append(gymnast_first_name, row_1[4])
    gymnast_last_name <- append(gymnast_last_name, row_1[3])
    comp_number <- append(comp_number,row_1[2])
    vault_d <- append(vault_d, row_1[6])
    vault_e <- append(vault_e, row_2[3])
    vault_score <- append(vault_score, row_1[7])
    bars_d <- append(bars_d, row_1[8])
    bars_e <- append(bars_e, row_2[5])
    bars_score <- append(bars_score, row_1[9])
    beam_d <- append(beam_d, row_1[10])
    beam_e <- append(beam_e, row_2[6])
    beam_score <- append(beam_score, row_1[11])
    floor_d <- append(floor_d,row_1[12])
    floor_e <- append(floor_e, row_2[7])
    floor_score <- append(floor_score, row_1[13])

    # Skye
    row_1 <- str_split(results_vector[4], " ") |> unlist()
    
    row_2 <- str_split(results_vector[5], " ") |>  unlist()
    
    gymnast_first_name <- append(gymnast_first_name, row_1[4])
    gymnast_last_name <- append(gymnast_last_name, row_1[3])
    comp_number <- append(comp_number,row_1[2])
    vault_d <- append(vault_d, NA)
    vault_e <- append(vault_e, NA)
    vault_score <- append(vault_score, NA)
    bars_d <- append(bars_d, row_1[6])
    bars_e <- append(bars_e, row_2[3])
    bars_score <- append(bars_score, row_1[7])
    beam_d <- append(beam_d, row_1[8])
    beam_e <- append(beam_e, row_2[4])
    beam_score <- append(beam_score, row_1[9])
    floor_d <- append(floor_d,NA)
    floor_e <- append(floor_e, NA)
    floor_score <- append(floor_score, NA)
    
    # Shilese
    row_1 <- str_split(results_vector[6], " ") |> unlist()
    
    row_2 <- str_split(results_vector[7], " ") |>  unlist()
    
    gymnast_first_name <- append(gymnast_first_name, row_1[4])
    gymnast_last_name <- append(gymnast_last_name, row_1[3])
    comp_number <- append(comp_number,row_1[2])
    vault_d <- append(vault_d, row_1[6])
    vault_e <- append(vault_e, row_2[3])
    vault_score <- append(vault_score, row_1[7])
    bars_d <- append(bars_d, row_1[8])
    bars_e <- append(bars_e, row_2[4])
    bars_score <- append(bars_score, row_1[9])
    beam_d <- append(beam_d, row_1[10])
    beam_e <- append(beam_e, row_2[5])
    beam_score <- append(beam_score, row_1[11])
    floor_d <- append(floor_d,row_1[12])
    floor_e <- append(floor_e, row_2[6])
    floor_score <- append(floor_score, row_1[13])

    # Leanne
    row_1 <- str_split(results_vector[10], " ") |> unlist()
    
    row_2 <- str_split(results_vector[11], " ") |>  unlist()
    
    gymnast_first_name <- append(gymnast_first_name, row_1[4])
    gymnast_last_name <- append(gymnast_last_name, row_1[3])
    comp_number <- append(comp_number,row_1[2])
    vault_d <- append(vault_d, row_1[6])
    vault_e <- append(vault_e, row_2[3])
    vault_score <- append(vault_score, row_1[7])
    bars_d <- append(bars_d, row_1[8])
    bars_e <- append(bars_e, row_2[4])
    bars_score <- append(bars_score, row_1[9])
    beam_d <- append(beam_d, row_1[10])
    beam_e <- append(beam_e, row_2[5])
    beam_score <- append(beam_score, row_1[11])
    floor_d <- append(floor_d,row_1[12])
    floor_e <- append(floor_e, row_2[6])
    floor_score <- append(floor_score, row_1[13])

    # Joscelyn
    row_1 <- str_split(results_vector[8], " ") |> unlist()
    
    row_2 <- str_split(results_vector[9], " ") |>  unlist()
    
    gymnast_first_name <- append(gymnast_first_name, row_1[4])
    gymnast_last_name <- append(gymnast_last_name, row_1[3])
    comp_number <- append(comp_number,row_1[2])
    vault_d <- append(vault_d, row_1[6])
    vault_e <- append(vault_e, row_2[3])
    vault_score <- append(vault_score, row_1[7])
    bars_d <- append(bars_d, NA)
    bars_e <- append(bars_e, NA)
    bars_score <- append(bars_score, NA)
    beam_d <- append(beam_d, NA)
    beam_e <- append(beam_e, NA)
    beam_score <- append(beam_score, NA)
    floor_d <- append(floor_d,row_1[8])
    floor_e <- append(floor_e, row_2[4])
    floor_score <- append(floor_score, row_1[9])
        
    
    world_scores <- tibble(gymnast_first_name, gymnast_last_name, comp_number, vault_d, vault_e, vault_score, bars_d, bars_e, bars_score, beam_d, beam_e, beam_score, floor_d, floor_e, floor_score)
    return(world_scores)
}