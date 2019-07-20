
graphplot_every<- function(data,filepath='')

{
  library("dplyr")
  nums<-names(select_if(data, is.numeric))
  labels<-names(select_if(data, is.factor))

  print(nums)
  print(labels)
  dir.create(filepath)

  for(var in nums)
  {
    png(paste(unlist(names(data[var])),'.png', sep=""))
    par(mfrow=c(1,2))
    boxplot(data[,var], main = paste('Boxplot of', unlist(names(data[var]))),
            ylab = names(data)[var], col = 'orange', border = 'black',
            horizontal = T)
    hist(data[,var], main = paste('Histogram of', unlist(names(data[var]))),
         xlab = names(data)[var], ylab = 'No. of Houses', col = 'blue', border=F)
    dev.off()
    a = list.files(pattern = '.png')
    file.copy(a, filepath)

  }

  for(var in labels)
  {
    png(paste(unlist(names(data[var])),'.png', sep=""))

    counts <- table(data[var])

    slices <- c(counts)
    lbls <- c(counts)

    par(mfrow=c(1,2))

    barplot(counts, main = paste('Barplot of', unlist(names(data[var]))), col = rainbow(length(slices)))
    pie(slices, labels = lbls, main =paste('PieChart of', unlist(names(data[var]))), col = rainbow(length(slices)))

    dev.off()

    a = list.files(pattern = '.png')
    file.copy(a, filepath)


  }
}

## Creative Functions
## Frequency of categories for categorical variable

categorical_count<- function(data)
{
  d = data
  cat_col = c()
  a = sapply(colnames(d), function(x) class(d[[x]]))
  a=as.matrix(a)

  for (i in 1:ncol(d))
  {
    if (is.factor(d[,i]))
    {
      values = c()
      values = append(values, c(names(d[i])))
      cat_col = append(cat_col, values, after = length(cat_col))
    }
  }


  for (i in 1:length(cat_col))
  {
    cat('\nLevels in ',cat_col[i], "    Count Values")


    j = as.matrix(table(d[cat_col[i]]))
    colnames(j) = ""
    print(print.gap=30,j, justify = "right")
  }
}

plotscatter_all<- function(data)
{
  par(mfrow=c(1,1))

  for (i in 1:ncol(data))
  {
    if (class(data[,i])!="factor" && length(unique(data[,i]))>12)
    {
      for (j in (i+1):ncol(data))
      {
        if (class(data[,j])!="factor" && length(unique(data[,j]))>12)
        {
          png(paste(unlist(names(data[i])),"vs",unlist(names(data[j])),'.png', sep=""))
          title<-paste("Scatter plot of ",unlist(names(data[i])),"vs",unlist(names(data[j]))," with loess fit")
          scatter.smooth(data[,i],data[,j], xlab = unlist(names(data[i])), ylab = unlist(names(data[j])),main=title,col="yellowgreen")

          dev.off()


        }

      }}}}

outlier_count<- function(data)
{
  # if (class(fram) != "data.frame")
  # {
  #   print('Not a data frame')
  #   return (NULL)
  # }

  Upper_Count <- c()
  Lower_Count <- c()
  Upper_Count.afterlog<- c()
  Lower_Count.afterlog<- c()

  names <- c()

  library("dplyr")
  nums<-names(select_if(data, is.numeric))

  for (i in nums)
  {
    print("entered")

    print(i)
    x <- data[i][!is.na(data[i])]

    q25 = quantile(x)[2]
    q75 = quantile(x)[4]

    print("afterx")

    upperbound = q75 + 1.5*(q75-q25)
    lowerbound = q25 - 1.5*(q75-q25)

    names[i] = names(data[i])
    Upper_Count[i] = sum(x>upperbound)
    Lower_Count[i] = sum(x<lowerbound)

    o2 <- log10(data[i][!is.na(data[i])])

    q25_ = quantile(o2)[2]
    q75_ = quantile(o2)[4]

    logupperbound = q75_ + 1.5*(q75_-q25_)
    loglowerbound = q25_ - 1.5*(q75_-q25_)

    Upper_Count.afterlog[i] = sum(o2>logupperbound, na.rm = TRUE)
    Lower_Count.afterlog[i] = sum(o2<loglowerbound, na.rm = TRUE)


  }
  outlierTable <- cbind(Upper_Count,Lower_Count)
  outlierTable <- cbind(outlierTable,Upper_Count.afterlog)
  outlierTable <- cbind(outlierTable,Lower_Count.afterlog)

  rownames(outlierTable) = names
  return (outlierTable)

}

missing_count<- function(data)
{
  m <- c()
  n <- c()
  o <- c(1:length(names(data)))
  for (i in o)
  {
    n[i] <- sum(is.na(data[,i]))
    m[i] <- round((sum(is.na(data[,i])))/length(data[, i])*100, 3)
  }

  n<-data.frame("Count_of_Missing"=n)
  m<-data.frame("Percentage__Missing"=m)
  new<-cbind(n,m)
  rownames(new)=names(data)
  return(new)
}
