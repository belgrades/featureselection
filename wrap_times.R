library(parsedate)
library(ggplot2)

lappend <- function ( lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}

eappend <- function(elem, ...){
  elem <- c(elem, ...)
  return(elem)
}

create_gaps = function(data){
  id = unique(data$get_params.prdctvId)
  gap = double()
  prdctvID = character()
  for(i in id){
    whom = data$timestamp[data$get_params.prdctvId==i]
    if(length(whom)>=2){
      for(x in 2:length(whom)){
        gap = c(gap, as.double(difftime(whom[x], whom[x-1], units="secs")))  
        prdctvID = c(prdctvID, i)
      }
    }else{
      gap = c(gap, 0.0)  
      prdctvID = c(prdctvID, i)
    }
  }
  x = data.frame(prdctvID, gap)
  return(x)
}

get_url = function(data, prdctvID, timestamp){
  url = data$get_params.url[data$get_params.prdctvId == prdctvID & data$timestamp == timestamp]
  return(url)
}

create_trans = function(data, tolerance = 120){
  id = unique(data$get_params.prdctvId)
  tolerance = 120
  # To fill in Data Frame
  source.id = character()
  trans.id = numeric()
  transaction = character()
  actual = 1
  
  for(i in id){
    whom = data$timestamp[data$get_params.prdctvId==i]
    if(length(whom)>=2){
      # Agregamos el primer URL
      trans.nueva = get_url(data, i, whom[1])
      
      for(x in 2:length(whom)){
        if(as.double(difftime(whom[x], whom[x-1], units="secs"))<tolerance){
          trans.nueva = paste(trans.nueva, get_url(data, i, whom[x]), sep = ",")
        }else{
          # Fallo la tolerancia, nueva transaccion
          
          # Agregamos indice, actualizamos indice
          source.id = c(source.id, i)
          trans.id = c(trans.id, actual)
          actual = actual + 1
          
          # Agregamos URL
          transaction = c(transaction, paste(unique(strsplit(trans.nueva, split = ",")[[1]]), collapse = ","))
          trans.nueva = get_url(data, i, whom[x])
        }
      }
      source.id = c(source.id, i)
      trans.id = c(trans.id, actual)
      actual = actual + 1
      transaction = c(transaction, paste(unique(strsplit(trans.nueva, split = ",")[[1]]), collapse = ","))
      # Terminamos de analizar para el is i actual
    }else{
      source.id = c(source.id, i)
      trans.id = c(trans.id, actual)
      actual = actual + 1 
      transaction = c(transaction, get_url(data, i, whom[1]))
    }
  }
  items = transaction
  x = data.frame(items)
  return(x)
}

data = read.csv(file.choose(),
                encoding = "UTF-8", 
                stringsAsFactors = F)

data$timestamp = substr(data$timestamp,
                        start = 1, 
                        stop = nchar(data$timestamp) -1)

# Change Date Format from ISO 8601 to POSIX
data$timestamp = parse_iso_8601(data$timestamp)

#data = data[1:100,1:7]
# Creating gaps structure


new = create_trans(data = data, tolerance = 120)

write.csv(x = new, file = "vivalafete.csv")

gaps = create_gaps(data)

# Subset according to gap
gaps = subset(gaps, subset = gap<=120 & gap>0)

hist(gaps$gap, 
     nclass=20, 
     border = "white", 
     col = "steelblue",
     xlab = "Gaps",
     ylab = "Gaps density",
     main = "Histogram of Gaps t(n) - t(n-1)")
box()
x = seq(min(gaps$gap), max(gaps$gap), length = 100)
lines(density(gaps$gap, bw=1), lwd = 2)

curve(dbeta(x,4,50),xlim=c(0,1))

exp = data.frame(rexp(n = length(gaps$gap), rate = 1/18))
names(exp) = c("gap") 
gaps2 = data.frame(gaps$gap)
names(gaps2) = c("gap")
exp$type = "Exponencial"
gaps2$type = "Reales"

hist.data = rbind(exp, gaps2)
hist.data$dates = as.double(hist.data$gap)

ggplot(hist.data, aes(x = gap, fill = type)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
