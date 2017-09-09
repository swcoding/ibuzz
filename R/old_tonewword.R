#' old_tonewword function
#'
#' @description To turn the oldpro into newword format.
#' @param data An oldpro format data frame.
#' @param where A string. It will be added as the first column in newword.
#' @details Notice that if there are no ';' in any cell in the data frame, this function will return a 0 row data frame without
#' any error.
#' @import magrittr
#' @import tidyr
#' @examples
#' gym = read.csv("...") # gym is an oldpro file
#' gym_newword = old_tonewword(data = gym, where = "gym")

old_tonewword = function(data,where){

  # 轉換資料、清除括號
  data %<>% dataprocess(.)

  # gather 並去除空格
  data = gather(data, "category","name",1:length(data)) %>% .[which(!is.na(.$name)),]

  # 先命名另一個df，將沒有同義詞的列先放進去
  ans = data[-which(grepl(';',data$name)),]
  ans$same = ans$name

  # 檢查並生成同義詞
  for(i in 1:length(data$name)){

    if(grepl(';',data$name[i])==T){
      c = strsplit(data$name[i],';') %>% unlist()
      temp = data.frame("category" = rep(data$category[i], length(c)),
                        "name" = rep(c[1],length(c)),
                        "same" = c)
      ans = rbind(ans,temp)
    }

  }

  industry = rep(where,nrow(ans)) %>% as.character()
  ans = cbind(industry,ans)
  colnames(ans) = c("industry","category","name","same")
  return(ans)
}
