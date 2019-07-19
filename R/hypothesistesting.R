hypothesis <- function(data,vec=0,t)
{
  response=data[,t] #storing the target variable
  options(warn=-1)
  if(!is.data.frame(data)) #check if dataframe or not
    stop("The given object is not a data frame")
  {if (vec==0) #if nothing mentioned,take all the columns
  {
    d=data
  }
    else
    {
      d=data[,vec]
    }}

  #initialise the count of numerical and categorical variables
  count_n=0
  count_c=0
  
  #initialising vectors for statistical tests
  name_t=c() #storing name of numerical variable
  test_n=c() #storing p-vales of T-test
  name_k=c() #storing names of target variable or numeric
  name_r=c() #storing names of target variable for factor
  name_c=c() #storing name of categorical variable
  test_c=c() #storing p-vales of chi square test
  result_n=c() #storing result for t-test
  
  for(i in 1:ncol(d))
  {
    
    if(is.numeric(d[,i])) #check if the variable is numeric
    {
      
      count_n=count_n+1
      name_t[count_n]<-names(d[i])
      name_k[count_n]<-names(d[t])
      test_n[count_n]<-t.test(d[,i], y = d[t],
                              alternative = c("two.sided", "less", "greater"),
                              mu = 0, paired = FALSE, var.equal = FALSE,
                              conf.level = 0.95)$p.value
      result_n[count_n]<-ifelse(test_n[count_n]<0.05,'Reject Null','Fail to Reject Null')
    }
      
      
      if(is.factor(d[,i])) #check if the variable is a factor
      {
        count_c=count_c+1
        tbl<-table(d[,i], response  )
        name_r[count_c]<-names(d[t])
        name_c[count_c]<-names(d[i])
        test_c[count_c]<-chisq.test(tbl)$p.value
      }
  }
  k = data.frame(Variable=c(name_t),Target=c(name_k),P_value=c(test_n),Result=c(result_n))
  write.xlsx(k, file="hypothesis_testing.xlsx", sheetName="t-test",append=TRUE, row.names=FALSE)
  l = data.frame(Predictor=c(name_c),Target=c(name_r),P_value=c(test_c))
  write.xlsx(l, file="hypothesis_testing.xlsx", sheetName="chisquare-test",append=TRUE, row.names=FALSE)
}
