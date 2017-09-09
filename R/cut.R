#' cut function
#'
#' @description Cut the synonym in the cells in oldpro and return a string vector contains synonyms.
#' @param g Strings contain ';'
#' @import magrittr
#' @details It will jump error message if there are no ';' in the string.


# cut the ';'
cut = function(g){

  ans = g[-which(grepl(';',g))] %>% .[-which(.=="")]
  temp = c()
  for(i in 1:length(g)){
    if(grepl(";",g[i])){
      c = strsplit(g[i],";") %>% unlist()
      temp %<>%  c(., c)
    }
  }
  ans %<>% c(ans,temp) %<>% unique
  return(ans)
}
