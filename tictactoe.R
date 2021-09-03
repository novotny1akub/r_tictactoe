library(tidyverse)
library(httr)
library(jsonlite)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# https://piskvorky.jobs.cz/api/doc
# zacina 1 a o je druhá což odpovídá cross a cricles z api
# https://stackoverflow.com/questions/6952607/ai-strategy-for-gomoku-a-variation-of-tic-tac-toe
# https://www.playgomoku.online/gomoku-offline
# http://blog.trixi.cz/2013/02/popis-piskvorkoveho-algoritmu/


# gomoku functions --------------------------------------------------------

move_assign_score <- function(.string){
  
  # kladné body
  case_when(
    str_detect(.string, "___x___") ~ 5e0,
    str_detect(.string, "__x__") ~ (5e0 - 1e0),
    T ~ 0
  ) +
    case_when(
      str_detect(.string, "__xx__") ~ 5e1,
      str_detect(.string, "_xx__") | str_detect(.string, "__xx_") | str_detect(.string, "__x_x__")  ~ 5e1 - 2e1,
      str_detect(.string, "o_xx_o") ~ 0,
      T ~ 0
    ) +
    case_when(
      str_detect(.string, "__xxx__") ~ 5e3,
      str_detect(.string, "_xxx__") | str_detect(.string, "__xxx_") ~ 5e3 - 1e3,
      str_detect(.string, "_x_xx_") | str_detect(.string, "_xx_x_") ~ 5e3 - 3e3,
      str_detect(.string, "oxxx__") | str_detect(.string, "__xxxo") | str_detect(.string, "oxxx_o") | str_detect(.string, "o_xxxo") | str_detect(.string, "oxxxxo") ~ 0,
      T ~ 0
    ) +
    case_when(
      str_detect(.string, "_xxxx_") ~ 5e5,
      str_detect(.string, "oxxxx_") | str_detect(.string, "_xxxxo") ~ 2*5e3 - 3e3,
      str_detect(.string, "ox_xxxo") | str_detect(.string, "oxx_xxo") | str_detect(.string, "oxxx_xo") ~ 2*5e3 - 3e3,
      T ~ 0
    ) +
    case_when(
      str_detect(.string, "xxxxx") ~ 5e9, T ~ 0
    ) +
    
    # záporné body
    case_when(
      str_detect(.string, "xooo__") | str_detect(.string, "__ooox") ~ -(10e0 + 4e0),
      str_detect(.string, "___o___") ~ -10e0,
      str_detect(.string, "__o__o__") ~ -10e0,
      str_detect(.string, "__o__") ~ -(10e0 - 1e0),
      T ~ 0
    ) +
    case_when(
      str_detect(.string, "__oo__") ~ -10e1,
      str_detect(.string, "_oo__") | str_detect(.string, "__oo_") | str_detect(.string, "__o_o__")  ~ -(10e1 - 2e1),
      str_detect(.string, "x_oo_x") ~ 0,
      T ~ 0
    )  +
    case_when(
      str_detect(.string, "__ooo_") | str_detect(.string, "_ooo__") | str_detect(.string, "_o_o_o_") | str_detect(.string, "_oo_o_") | str_detect(.string, "_o_oo_")  ~ -10e3,
      T ~ 0
    )  +
    case_when(
      str_detect(.string, "oooo_") | str_detect(.string, "_oooo") | str_detect(.string, "o_ooo") | str_detect(.string, "oo_oo")  | str_detect(.string, "ooo_o") ~ -10e5,
      T ~ 0
    )  +
    case_when(
      str_detect(.string, "ooooo") | str_detect(.string, "_oooo_") ~ -10e9,
      T ~ 0
    )
  
}

play_move <- function(.row, .column, mark, M = M){
  cen_r <- ceiling(ncol(M)/2)
  cen_c <- ceiling(nrow(M)/2)
  M[cen_r-.row, cen_c+.column] <<- mark
} # play_move(0, 0, "x", M)
view_matrix <- function(M){
  
  cen_r <- ceiling(ncol(M)/2)
  cen_c <- ceiling(nrow(M)/2)
  
  M %>%
    as_tibble() %>%
    set_names(~str_remove(., "V") %>% as.integer() %>% `-`(cen_c) %>% as.character()) %>%
    slice( (cen_r - 10):(cen_r + 10) ) %>%
    mutate(rn = 10:-10) %>%
    column_to_rownames("rn") %>%
    select(`-10`:`10`)
  
}

generate_matrix_tst <- function(nrows = 25*2 + 1, ncols = 2*28 + 1){
  
  rep("_", times=nrows*ncols) %>% matrix(nrow=nrows)
  
}
generate_df_M_long <- function(M){
  df_M_long <- M %>%
    as_tibble(rownames = "r_number") %>%
    pivot_longer(-1, names_to = "c_number") %>%
    mutate(
      c_number = str_remove(c_number, "V") %>% as.integer(),
      r_number = as.integer(r_number)
    )
  
  
  df_rcd_IDs <- lst(
    dd = (col(M) - row(M)) + (ncol(M)), # descending diagonal
    ad = row(M) + col(M) - 1L # ascending diagonal
  ) %>%
    map(
      ~.x %>%
        as_tibble(rownames = "r_number") %>%
        pivot_longer(-1, names_to = "c_number") %>%
        mutate(
          c_number = str_remove(c_number, "V") %>% as.integer(),
          r_number = as.integer(r_number)
        )
    ) %>%
    bind_rows(.id = "name") %>%
    pivot_wider()
  
  left_join(df_M_long, df_rcd_IDs, by = c("r_number", "c_number")) %>%
    select( r_number, c_number, dd, ad, value)
  
}
generate_df_pruned_moves <- function(df_M_long, search_depth_fct = 5){
  df_M_pruned <- df_M_long %>%
    filter(value != "_") %>%
    summarise(
      min_r = min(r_number),
      min_c = min(c_number),
      max_r = max(r_number),
      max_c = max(c_number)
    ) %>%
    crossing(df_M_long) %>%
    filter(
      (min_r - search_depth_fct) <= r_number & r_number <= (max_r + search_depth_fct) &
        (min_c - search_depth_fct) <= c_number & c_number <= (max_c + search_depth_fct) & 
        value == "_"
    ) %>%
    select(-c("min_r", "max_r", "min_c", "max_c", "value")) %>%
    mutate(
      move_id = row_number()
    )
}
generate_df_M_after_move <- function(df_pruned_moves, df_M_long){
  df_pruned_moves %>%
    mutate(
      value = "x"
    ) %>%
    group_split(move_id) %>%
    map(
      ~.x %>%
        select(-move_id) %>%
        bind_rows(df_M_long) %>%
        distinct(r_number, c_number, dd, ad, .keep_all = T)
    )
  
}
generate_df_scored_moves <- function(df_M_after_move){
  df_M_after_move %>%
    map_dfr(
      ~tibble(
        r_score = .x %>%
          arrange(r_number, c_number) %>%
          group_by(r_number) %>%
          summarise(.groups = "drop", value = paste(value, collapse = "")) %>%
          mutate(r_score = move_assign_score(value)) %>%
          pull(r_score) %>%
          sum(),
        c_score = .x %>%
          arrange(c_number, r_number) %>%
          group_by(c_number) %>%
          summarise(.groups = "drop", value = paste(value, collapse = "")) %>% 
          mutate(c_score = move_assign_score(value)) %>%
          pull(c_score) %>%
          sum(),
        dd_score = .x %>%
          arrange(dd, desc(r_number)) %>%
          group_by(dd) %>%
          summarise(.groups = "drop", value = paste(value, collapse = "")) %>%
          mutate(dd_score = move_assign_score(value)) %>%
          pull(dd_score) %>%
          sum(),
        ad_score = .x %>%
          arrange(ad, r_number) %>%
          group_by(ad) %>%
          summarise(.groups = "drop", value = paste(value, collapse = "")) %>%
          mutate(ad_score = move_assign_score(value)) %>%
          pull(ad_score) %>%
          sum()
      )
    ) %>%
    mutate(
      move_id = row_number(),
      tot_score = r_score + c_score + dd_score + ad_score
    ) %>%
    slice_max(order_by = tot_score) %>%
    slice_sample(n = 1)
    # arrange(desc(tot_score), desc(ad_score), desc(dd_score))
  
}

fce_next_move_cords_df <- function(M){
  # M <- generate_matrix_tst(); play_move(0, 0, "o", M);
  df_M_long <- generate_df_M_long(M)
  df_pruned_moves <- generate_df_pruned_moves(df_M_long)
  df_M_after_move <- generate_df_M_after_move(df_pruned_moves, df_M_long)
  df_scored_moves <- generate_df_scored_moves(df_M_after_move)
  
  cen_r <- ceiling(ncol(M)/2)
  cen_c <- ceiling(nrow(M)/2)
  
  df_next_move <- df_scored_moves %>%
    head(1) %>%
    select(move_id) %>%
    left_join(df_pruned_moves %>% select(-dd, -ad), by = c("move_id")) %>%
    select(-move_id) %>%
    mutate(
      r_number = -(r_number - cen_r),
      c_number = c_number - cen_c
    )
  
  df_next_move
  
  # M[df_next_move$r_number, df_next_move$c_number] <<- "x"
  
}

# REST functions ----------------------------------------------------------

REST_login <- function(.nick = "allGoodNamesAreGone", .email = "novotnyjakub@email.cz"){
  
  login <- list(
    "nickname" = .nick,
    "email" = .email
  )
  
  
  req <- POST(
    url = "https://piskvorky.jobs.cz/api/v1/user",
    body = toJSON(login, auto_unbox = T)
  )
  
  c(
    "user_id" = content(req)$userId,
    "user_token" = content(req)$userToken
  )
  
}
REST_game <- function(.user_token = "dd8eb54f-b344-4eed-9be1-240ab3c19934"){
  req <- POST(
    url = "https://piskvorky.jobs.cz/api/v1/connect",
    body = list("userToken" = .user_token) %>% toJSON(auto_unbox = T)
  )
  
  c(
    "game_token" = content(req)$gameToken
  )

}
REST_hraje_se <- function(game_id, .user_token = "dd8eb54f-b344-4eed-9be1-240ab3c19934"){
  r <- POST(
    url = "https://piskvorky.jobs.cz/api/v1/checkStatus",
    body =
      list(
        "userToken" = .user_token,
        "gameToken" = game_id
      ) %>%
      toJSON(auto_unbox = T)
  )
  
  c <- content(r)
  
  if(r$status_code == 226){
    print("hra uz skoncila")
    F
  } else if ( !(is.null(c$playerCrossId) | is.null(c$playerCircleId)) ){
    T
  } else if( (is.null(c$playerCrossId) | is.null(c$playerCircleId))){
    print("cekame na hrace a za 15 s to zkusime znovu")
    Sys.sleep(15)
    return(REST_hraje_se(game_id, .user_token))
  }
  
}
REST_played_moves_to_df <- function(game_id, .user_token = "dd8eb54f-b344-4eed-9be1-240ab3c19934"){
  
  c <- POST(
    url = "https://piskvorky.jobs.cz/api/v1/checkStatus",
    body =
      list(
        "userToken" = .user_token,
        "gameToken" = game_id
      ) %>%
      toJSON(auto_unbox = T)
  ) %>%
    content()
  
  if(length(c$coordinates) != 0){
    df <- c$coordinates %>%
      bind_rows() %>%
      mutate(
        order = case_when(
          c$playerCrossId == playerId ~ 1,
          c$playerCircleId == playerId ~ 2
        ),
        playerId = str_detect(playerId, "d361402b-1f20-47a3-b025-36f5b0bf066f") %>% if_else("ja", "souper")
        ) %>%
      select(playerId, "row" = y, "col" = x, order) %>%
      group_by(playerId) %>%
      mutate(round = row_number()) %>% ungroup() %>%
      arrange(round, order)
  } else{
    df <- tibble(playerId = character(0), row = integer(0), col = integer(0))
  }
  
  df
}
REST_play_move <- function(.y, .x, game_id, .user_token = "dd8eb54f-b344-4eed-9be1-240ab3c19934"){
  req <- POST(
    url = "https://piskvorky.jobs.cz/api/v1/play",
    body =
      list(
        "userToken" = .user_token,
        "gameToken" = game_id,
        "positionX" = as.integer(.x),
        "positionY" = as.integer(.y)
      ) %>%
      toJSON(auto_unbox = T)
  )
  
  content(req)
  
}

fce_hrajeme <- function(.user_token = "dd8eb54f-b344-4eed-9be1-240ab3c19934"){
  
  if( REST_hraje_se(game_id) ){
    played_moves_df <- REST_played_moves_to_df(game_id)
    
    # nikdo ještì nehrál
    if(nrow(played_moves_df) == 0){
      r <- REST_play_move(0,0, game_id)
      if(r$statusCode %in% c(406, 410)){
        print("chtel jsem hrat, ale prvni hraje souper, cekam 5s")
        Sys.sleep(5)
        }
      else{
        print("prvni jsem zahral ja")
      }
    } else {
      if( tail(played_moves_df$playerId, 1) == "ja"){
        Sys.sleep(5)
        print("hraje souper, cekame 5 s")
      } else if( tail(played_moves_df$playerId, 1) == "souper" & nrow(played_moves_df) >= 1){
        print("hraju ja")
        # prepsat na pravdu dle API
        played_moves_df %>%
          mutate(mark = if_else(playerId == "ja", "x", "o")) %>%
          group_split(row_number()) %>%
          walk(
            ~ play_move(.x$row, .x$col, .x$mark, M)
          )
        # spoèíst další tah a zahrát
        df_move <- fce_next_move_cords_df(M)
        r <- REST_play_move(df_move$r_number,df_move$c_number, game_id)
        play_move(df_move$r_number, df_move$c_number, "x", M)
        
      } else{
        print("neocekavana chyba")
      }
    }
  }
}

# kod pro hrani hry -------------------------------------------------------


# více her
for(ii in 1:4){
  
  paste("Hra", ii) %>%
    print()
  
  # jedna hra
  M <- generate_matrix_tst()
  game_id <- REST_game()
  for(i in 1:100){
    fce_hrajeme()
    print(i)
  }
  
}
