MyDocumentation<- function(){

  ##	DOCUMENTATION  ##

- core of R is VECTOR (array of same class objects)
- everything is implicitly object
- every object have class and length
- atomic classes: numeric, logic, character, integer, complex

1-numeric object but 1L is number (not object)

inf, -inf, NaN (for mathematical operations), NA (missing values)

## ls()  ls.str()  rm("objectName")  argt(function)  strl+L == clean 
## v = vector(1:10) || c(1:10)
## names(v) = vectorNames
## c = as.character(v)  <- explicit conversion |
## implicit conversion: character > numeric > logical
 
## m = matrix(v, nrow, ncol)
## dim(v) = c(2,5)
## cbind(v1, v2) | rbind(v1, v2)
## dimnames(m) = list( rowVectorNames, colVectorNames)
## v = m[1,] <- vector   ||   m2 = m[1, , drop=FALSE] <- matrix where dim=(1,2)

## l = list(1, "a", TRUE, F, 1+4i) || list(name1=1, name2="a", name3=TRUE)
## l[1] <- returns 1st element of list as LIST
## l[[ 1 ]] <- returns 1st element of list as class of that element's class
## l[ vectorOfColToReturnAsList ]
## l[[ name1 ]] == l$name1

e.g.{
l=list(food = list("fruits", "meat", "bread"), animals = list("cat", "shark"))	

l[[c(1,3)]] == l[[1]][[3]] == l$food[3] == l$f[3] == l[["f", exact=FALSE]]
							<      partial   matching      >
}

## f = factor( c("yes", "yes", "no", "yes", "no") )
## factor( c(...), levels = c("yes", "no") )
## unclass(f)
## attr(f, "levels")
## table(f) <- levels and frequency

- Data frames 
## df = data.frame( colName1 = v1, colName2 = v2)
## data.matrix() <- convert dataFrame to matrix
## nrow(df) || ncol(df)
## row.names

-Reading/Writing data
optimization: load into RAM, comment.char="",
 use colClasses !, or use colClasses = sapply(df,class) were df has nrow=50
## read.table()
## read.csv()
##
##
## -works with one R object
## dput(df, file="deparsedData.r")  ||  dget("deparsedData.r")
## -works with muliple R object
## dump(c("v","m","df"), file = "multipleDiparsed.r") || source("multipleDiparsed.r")


- Removing NA values
## v = v[!is.na(v)]
e.g.{
logVector = complete.cases(df) <- rows that have NA values
df = df[logVector, ]
}



}

for(i in 1:10){...}  
for(i in seq_along(v)){...}
for(i in seq_len(nrow(m))){...}
for(element in v){...}

Remove from workplace rm(list=ls())
