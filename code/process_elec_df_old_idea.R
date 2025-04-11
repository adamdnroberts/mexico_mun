#create list of columns to exclude when calculating max/second
no_part <- c("p1", "p2","TOTAL","Municipio","year","p1_name","p2_name","estado", "NO REG.", "ANULADOS")

# functions used in the main function
first_largest_colname <- function(row, df) {
  sorted_indices <- order(row, decreasing = TRUE, na.last = TRUE)
  colnames(df)[3:(ncol(df) - 1)][sorted_indices[1]]
}

second_largest_colname <- function(row, df) {
  sorted_indices <- order(row, decreasing = TRUE, na.last = TRUE)
  colnames(df)[3:(ncol(df) - 1)][sorted_indices[2]]
}

#main function
process_df <- function(df) {
  colnames(df)[2] <- "year"
  
  df <- df %>% mutate(across(setdiff(names(df),c("estado","Municipio")), as.numeric))
  
  # Find max vote winner
  df$p1 <- apply(df[!(colnames(df) %in% no_part)], 1, max, na.rm = TRUE)
  
  # Find second max winner
  df$p2 <- apply(df[!(colnames(df) %in% no_part)], 1, function(x) sort(x, decreasing = TRUE, na.last = TRUE)[2])
  
  df$p1_name <- apply(df[!(colnames(df) %in% no_part)], 1, first_largest_colname, df = df)
  df$p2_name <- apply(df[!(colnames(df) %in% no_part)], 1, second_largest_colname, df = df)
  
  df$p1[df$p1 < 0] <- NA
  
  df$PAN_pct <- (df$PAN / (df$p1 + df$p2)) - 0.5 # PAN percentage compared to top two
  df$PAN_treat <- ifelse(df$PAN_pct > 0, 1, 0)
  
  df$PRD_pct <- df$PRD / (df$p1 + df$p2) - 0.5 # PRD percentage compared to top two
  df$PRD_treat <- ifelse(df$PRD_pct > 0, 1, 0)
  
  # setDT(df)[, c("next_PAN_pct") :=
  #              .(shift(PAN_pct, 1L, fill = NA, type = "lead")), by = Municipio]
  # 
  # setDT(df)[, c("prev_PAN_pct") :=
  #             .(shift(PAN_pct, 1L, fill = NA, type = "lag")), by = Municipio]
  
  return(df)
}
