#' fillword function
#' @description This function is for the use of function `check_and_fill`.
#' @param nchar A number. Notice that if nchar = n, you want to put the conj. whose length is less than 2
#' into the string, you have to key n/2 as input.
#' @param class A string. See details.
#' @import magrittr


common = read.csv("D:/things/R/ibuzz/common_170906.csv",header=T, encoding = "big5") %>% dataprocess()
for(i in 1:length(common)){
  c = common[i] %>% unlist
  c = c[!is.na(c)]
  names(c) = NULL
  assign(colnames(common)[i], c)
}

fillword = function(industry,category,noun, nchar, adj, class){
  #連接詞

  connect = switch(class,
                   notbad = notbad,
                   mei = mei,
                   boo_v = boo_v,
                   boo_a = boo_a,
                   mei_v = mei_v,
                   mei_a = mei_a,
                   mei_n = mei_n,
                   v_a = v_a,
                   v_v = v_v
                   )
  len = sapply(connect, nchar)
  connect = which(len<= nchar/2) %>% names()

  str = paste0(noun, connect, adj)
  df = data.frame("industry" = industry,
                  "category" = category,
                  "name" = paste0(noun,adj),
                  "same" = str)
  return(df)
}
