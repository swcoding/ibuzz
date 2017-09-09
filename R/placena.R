#' placena function
#'
#' @description Data frame is able to be written as csv only if there doesn't exist any blank cell in it.
#' This function is to turn the blank cell into NA value.
#' @param g Column contains blank cell.
#' @param nrow nrow of the column


# 將空格補上NA，才能 write.csv
placena = function(g,nrow){
  ans = c(g,rep("",nrow-length(g)))
  return(ans)
}
