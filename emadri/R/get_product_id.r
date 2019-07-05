
#' get_product_id Function
#'
#' This function allows you to get a list of product_id according to user's input text.
#' @param user_input_text User's input text
#' @param full_data Product database
#' @param N length limitation of the results
#' @keywords userinput
#' @export
#' @examples
#' get_product_id("I need a comfortable dress", read.csv("sentiment_df_v1"), 3 )

get_product_id <- function(user_input_text, full_data, N){
  user_id <- 1
  df <- data.frame(user_id, user_input_text)
  input_matrix <- df_to_matrix(df)
  input_df <- data.frame(input_matrix)
  input_df <- setDT(input_df, keep.rownames = TRUE)[]
  names(input_df)[1] <- "user_id"
  input_df %>% 
    gather(words, count, -user_id) -> tidy_input
  
  adj_df <- merge(x = tidy_input, y = full_data, 
                  by.x = "words", by.y = "value", all.x = TRUE)
  adj_df <- na.omit(adj_df)
  category_Df <- merge(x = tidy_input, y = full_data, 
                       by.x = "words", by.y = "item_name", all.x = TRUE)
  results <- merge(x = adj_df, y = category_Df, by = "product_id", all.x = TRUE, all.y = TRUE)
  
  results <- results$product_id
  results <- na.omit(results)
  results <- results[!duplicated(results)]
  results <- as.character(results)
  results = results[which(nchar(results) == 10)]
  results <- head(results, N)
  return(results)
}

