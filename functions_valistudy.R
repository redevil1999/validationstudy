# function 

# it takes df, col1, col2, col3, col4, questionname_df, questionname_col

fixing_MA <- function(df, col1, col2, col3, col4, name_of_col){
  validation_df <- as.data.frame(df)

  STAI_nestenaldri <- validation_df %>%
    select(c(1, col1))
  names(STAI_nestenaldri)[2] <- "nestenaldri"
  
  STAI_avogtil <- validation_df %>%
    select(c(1, col2))
  names(STAI_avogtil)[2] <- "avogtil"
  
  STAI_ofte <- validation_df %>%
    select(c(1,col3))
  names(STAI_ofte)[2] <- "ofte"
  
  STAI_nestenalltid <- validation_df %>%
    select(c(1,col4))
  names(STAI_nestenalltid)[2] <- "nestenalltid"
  
  STAI_a <- full_join(STAI_nestenaldri, STAI_avogtil, by = "session_id") %>%
    mutate(
      colA = coalesce(STAI_nestenaldri$nestenaldri, STAI_avogtil$avogtil))
  
  STAI_b <- full_join(STAI_a, STAI_ofte, by = "session_id") %>%
    mutate(
      colB = coalesce(STAI_ofte$ofte, STAI_a$colA))
  
  STAI_c <- full_join(STAI_b, STAI_nestenalltid, by = "session_id") %>%
    mutate(
      colC = coalesce(STAI_nestenalltid$nestenalltid, STAI_b$colB))
  
  STAI_c %>%
    select("session_id", colC) %>%
    rename({{ name_of_col }} := colC)
}






# <!-- trying function but didn't work -->
# <!--  -->
# <!-- # Applying the custom function using apply() -->
# <!-- these_columns <- c(76,77,78,79) -->
# <!-- multiple_answers <- apply(firstdatacheck[, c(76,77,78,79)], 1, check_multians_columns, df = firstdatacheck, columns = these_columns) -->
