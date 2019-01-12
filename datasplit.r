datasplit<-function(AU,rat,seed)
{
  set.seed(seed)
  t<-ncol(AU)
  ss = sample.split(AU[,c(t)], SplitRatio = rat)
  #write.csv(AU,'AU.csv')
  train = subset(AU, ss == TRUE)
  test = subset(AU, ss == FALSE)
  list1<-list("a"=train,"b"=test)
  return(list1)
}