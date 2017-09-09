#' check_and_fill function
#'
#' @description This function is to fill the possible conj between the adj and the noun automatically.
#' @param data A newword format data frame.
#' @details Since the function is for fill the word automactically, you need to check oldpro first and give
#' the class to the cells in evaluation word column which contains '/'.
#' @import magrittr
#' @examples data %<>% check_and_fill(.)

check_and_fill = function(data){

  colnames(data) = c("industry", "category", "name", "same")
  ans = data[ -which(grepl('/',data$same)),]
  for(i in 1:nrow(data)){
    value = data$same[i]

    if(grepl('/',value)){
      str = strsplit(value, '/') %>% unlist
      df = fillword(industry = data[1,1],
                    category = data[i,]$category,
                    noun = str[1],
                    nchar = as.numeric(str[2]),
                    adj = str[3],
                    class = str[4])
      colnames(df) = colnames(data)
      ans = rbind(ans, df)

    }
  }

  # 修正名稱
  for(i in 1:nrow(ans)){
    value = ans$name[i]
    if(grepl('/',value)){
      c = strsplit(value,'/') %>% unlist
      ans$name[i] = paste0(c[1],c[3])
    }
  }


  colnames(ans) = c("industry","category","name","same")
  return(ans)
}
