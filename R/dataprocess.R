#' data process function
#'
#' This function help you to clean the brackets in old pro and turn the column in data frame
#' into character vector.
#' @param data data frame.
#' @import magrittr
#' @examples
#' data %<>% dataprocess(.)


dataprocess = function(data){
  # 轉換資料、清除括號
  for(i in 1:length(data)){
    data[,i] %<>% as.character(.)
  }

  for(i in 1:length(data)){
    data[,i] %<>% gsub('\\(',"",.) %<>% gsub('\\)',"",.)
  }
  ans = data
  return(data)
}
