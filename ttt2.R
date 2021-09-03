library(tidyverse)
library(httr)
library(jsonlite)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# let 2 AIs play against each other, give algorithms names
# save history of games
# allow scoring for o (currently only x)
# GUI to play against AI
# GUI to play against jobs.cz AIs

boundary_coord_names <- function(df_M){
  
  row_helper <- df_M %>%
    filter( rowMeans(. == "_") < 1) %>%
    row.names() %>%
    as.integer()
  
  if(length(row_helper)==0){
    row_min <- NA
    row_max <- NA
    col_min <- NA
    col_max <- NA
  } else{
    row_min <- min(row_helper, na.rm = F)
    row_max <- max(row_helper, na.rm = T)
    
    col_helper <- df_M %>%
      select_if(map_lgl(., ~mean(.x == "_") < 1)) %>%
      names() %>%
      as.integer() 
    col_min <- min(col_helper, na.rm = T)
    col_max <- max(col_helper, na.rm = T)
  }
  
  b_coords <- data.frame(
    row_min, row_max, col_min, col_max
  )
  
  attr(df_M, "b_coords") <- b_coords
  df_M

}
generate_df_M <- function(nrows = 25, ncols = 28){
  
  M <- rep("_", times = (2*nrows + 1)*(2*ncols + 1) ) %>% matrix(nrow = (2*nrows + 1) )
  col_names <- -ncols:ncols
  row_names <- nrows:(-nrows)
  colnames(M) <- col_names
  row.names(M) <- row_names

  
  df_M <- M %>%
    as.data.frame(rownames = "rn") %>%
    boundary_coord_names()
  
}
play_move <- function(df_M, .row, .column, mark){
  
  df_M[.row, .column] <- mark
  boundary_coord_names(df_M)
  

}
subset_df <- function(df_M, .margin_fct = 4, .realative = T){
  
  if(.realative){
    if(.margin_fct >= 1){
      
      row_subset <- intersect(
        as.character( (coalesce(attributes(df_M)$b_coords$row_max, 0) + .margin_fct):(coalesce(attributes(df_M)$b_coords$row_min, 0) - .margin_fct) ),
        row.names(df_M)
      )
      
      col_subset <- intersect(
        as.character( (coalesce(attributes(df_M)$b_coords$col_min, 0) - .margin_fct):(coalesce(attributes(df_M)$b_coords$col_max, 0) + .margin_fct) ),
        names(df_M)
      )
      
      df_M[
        row_subset,
        col_subset
      ] %>%
        boundary_coord_names()
      
    }
  } else{
    
    df_M[
      as.character(-(-.margin_fct:.margin_fct)),
      as.character(-.margin_fct:.margin_fct)
    ] %>%
      boundary_coord_names()
  }

}
gg_df <- function(df_M){
  
  if(is.na(attributes(df_M)$b_coords$row_min) | as.numeric(attributes(df_M)$b_coords) %>% max() %>% `<`(12) ){
    # pokud není zahráno nic nebo je zahráno ménì než +12
    df <- df_M %>%
      subset_df(14, .realative = F) 
    }
  else{
    df <- df_M %>%
      subset_df(28)
  }
  
  df <- df %>%
    rownames_to_column("y") %>%
    pivot_longer(-y, names_to = "x") %>%
    # filter(value != "_") %>%
    mutate(
      x = as.integer(x),
      y = as.integer(y),
      value = str_replace(value, "_", " ")
    )
  
  if(nrow(df)>0){
    df %>% 
      ggplot(aes(x, y)) +
      geom_tile(aes(fill = value, width = 0.9, height = 0.9)) +
      scale_x_continuous(breaks = seq(min(df$x), max(df$x), by = 1), minor_breaks = NULL) +
      scale_y_continuous(breaks = seq(min(df$y), max(df$y), by = 1), minor_breaks = NULL)
  } else{
    print("no rows to be displayed")
  }
  
}
to_long_with_ddad <- function(df_M){
  df_M_long <- df_M %>%
    rownames_to_column("y") %>%
    pivot_longer(-y, names_to = "x") %>%
    mutate(x = as.integer(x),
           y = as.integer(y))
  
  df_ddad <- lst(
    dd = (col(df_M) - row(df_M)), # descending diagonal  + (ncol(df_M))
    ad = row(df_M) + col(df_M) - 1L # ascending diagonal  
  ) %>%
    map(
      ~.x %>%
        as.data.frame(row.names = row.names(df_M)) %>%
        set_names(names(df_M)) %>%
        rownames_to_column("y") %>%
        pivot_longer(-y, names_to = "x") %>%
        mutate(x = as.integer(x),
               y = as.integer(y))
    ) %>%
    bind_rows(.id = "name") %>%
    pivot_wider() %>%
    mutate(dd = dd - min(dd) + 1)
  
  df_combined <- left_join(df_M_long, df_ddad, by = c("y", "x"))
  
  # %>%
  #   mutate(rn = row_number()) %>%
  #   rename("mark" = "value") %>%
  #   pivot_longer(-c(rn, mark)) %>%
  #   nest_by(name, value) %>%
  #   mutate(
  #     data = data %>% pull(mark) %>% c() %>% list()
  #     )

}
prepare_eval <- function(df_M, mark = "x"){
  
  # pruning to consider only fields from the relative distance of 5 from where there has already been played
  # only consider as playable moves from the relative distance of 1
  # evaluate the moves 
  # choose the best move
  # play the move inside M
  # send the move (API)
  
  # pokud prázdná plocha, zahraju na 0, 0
  if(is.na(attributes(df_M)$b_coords$row_min)){
    return(
      tibble(y = "0", x = "0")
    ) # y and x
  } 
  # lze 5 oponent, zahrát to
  else if(T){
    # df_M <- play_move(df_M, "0", "1", mark = "o"); gg_df(df_M)
    
    # only focusing the search for fives to the relative distance of 5 from the marks
    df_pruned <- df_M %>%
      subset_df(.margin_fct = 5, .realative = T) %>%
      to_long_with_ddad()
    
    # only cosidering as playable spaces next to the played ones
    # creating df_long for every possible move
    df_possible_moves <- df_M %>%
      subset_df(.margin_fct = 1, .realative = T) %>%
      to_long_with_ddad() %>%
      filter(value == "_") %>%
      mutate(value = mark) %>%
      split(sort(as.numeric(rownames(.)))) %>%
      map_df(
        ~.x %>%
          anti_join(df_pruned, ., by = c("y", "x")) %>%
          bind_rows(.x) %>%
          arrange(desc(y), x) %>%
          mutate(
            move_y = .x$y,
            move_x = .x$x
            )
      )
    
    df_eval <- df_possible_moves %>%
      mutate(
        move_ident = paste(move_y, move_x), move_y = NULL, move_x = NULL
      ) %>%
      pivot_longer(-c(move_ident, value), values_to = "nr") %>%
      group_by(move_ident, name, nr) %>%
      summarise(values = paste0(value, collapse = "")) %>% 
      filter(nchar(values) >= 5 & str_detect(values, "x|o")) %>%
      rowwise() %>%
      mutate(
        values_fivers = str_sub(
          values,
          start = 1:(nchar(values)-4),
          end = 5:nchar(values)
        ) %>% list()
      ) %>% ungroup() %>%
      select(move_ident, values_fivers) %>%
      unnest(cols = c(values_fivers)) %>%
      filter(
        str_detect(values_fivers, pattern = "x|o") &
          !( str_detect(values_fivers, pattern = "x") & str_detect(values_fivers, pattern = "o") )
      ) %>%
      mutate(
        n_mark = str_count(values_fivers, mark),
        n_opposite_mark = str_count(values_fivers, c("x", "o") %>% .[.!=mark]),
        score = 0.9*case_when(
          n_mark == 1 ~ 1,
          n_mark == 2 ~ 73,
          n_mark == 3 ~ 511,
          n_mark == 4 ~ 1751,
          n_mark == 5 ~ 10000,
          T ~ 0
        ) -
          case_when(
            n_opposite_mark == 1 ~ 1,
            n_opposite_mark == 2 ~ 73,
            n_opposite_mark == 3 ~ 511,
            n_opposite_mark == 4 ~ 1751,
            n_opposite_mark == 5 ~ 10000,
            T ~ 0
          )
      ) %>%
      group_by(move_ident) %>%
      summarise(score = sum(score))
    
    best_move <- df_eval %>%
      slice_max(order_by = score, n = 1, with_ties = T) %>%
      slice_sample(n = 1) %>%
      transmute(
        y = str_split(move_ident, pattern = " ", simplify = T)[1],
        x = str_split(move_ident, pattern = " ", simplify = T)[2]
          )
      
    return(best_move)
    

    

  }
}

# dpøedìlat subset_df tak, aby pokud je attr NA, nevracelo nic?

# hraní si ----------------------------------------------------------------

df_M <- generate_df_M()

for(i in 1:10){
  
  paste("Hraje x, kolo:", i) %>% print()
  x_m <- prepare_eval(df_M, mark = "x")
  df_M <- play_move(df_M, x_m$y, x_m$x, "x")

  paste("Hraje o, kolo:", i) %>% print()
  o_m <- prepare_eval(df_M, mark = "o")
  df_M <- play_move(df_M, o_m$y, o_m$x, "o")
  gg_df(df_M)
  
  Sys.sleep(2)
  
}






df_M <- generate_df_M() %>%
  prepare_eval(mark = "x")
  # play_move("0", "0", "x") %>%
  play_move("1", "0", "o") %>%
  play_move("-1", "0", "x") %>%
  play_move("0", "-1", "o") %>%
  play_move("0", "1", "x") %>%
  # play_move("25", "28", "o") %>%
  # play_move("-25", "-28", "x") %>%
  gg_df










