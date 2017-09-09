#' newword_newpro function
#'
#' @description Turn the newword format file into newpro format.
#' @import magrittr
#' @examples data_stat = newword_newpro(data, 'stat')
#' data_key = newword_newpro(data, 'key')

newword_newpro = function(phone,class){
  # phone = data
  # class = 'stat' or 'key'


  # 處理資料
  data = phone
  data = dataprocess(data)
  colnames(data) = c("industry","category","name",'same')

  #### stat ####
  if(class == 'stat'){
    # 轉換
    t = table(data$name)
    replicatename = which(t>1) %>% names()
    l = list()
    for(i in 1:length(replicatename)){
      word = data[ which(data$name %in% replicatename[i]), ]$same  #取出同義詞數量大於一的
      l[[i]] = word
    }
    length = sapply(l, function(x) length(x))
    attributes(l) = list(names = replicatename,
                         class = 'data.frame',
                         row.names = 1:max(length))

    # 補上空格、否則write.csv 會error
    for(i in 1:length(l)){
      l[,i] = placena(l[,i],nrow = max(length))
    }

    # 回傳
    return(l)
  }

  #### key ####
  else{
    # 轉換
    category = unique(data$category)
    l = list()
    for(i in 1:length(category)){
      l[[i]] = data[ data$category == category[i],]$same
    }
    length = sapply(l , function(x) length(x))
    attributes(l) = list(names = category,
                         class = 'data.frame',
                         row.names = 1:max(length))
    # 補上空格、否則write.csv 會error
    for(i in 1:length(l)){
      l[,i] = placena(l[,i],nrow = max(length))
    }

    # 回傳
    return(l)
  }
}
