library(parsedate)
library(urltools)
library(arules)


transform_url <-function(url){
  # Esta funcion depende del contexto del problema

  parameter_values <- param_get(url, c("college","q","cat","size_range","apparel_collection"))
  htp = strsplit(url, "\\?")[[1]][[1]]
  nombres = names(parameter_values)
  i = 1
  nombres[[1]]
  for (row in parameter_values) {
    if (parameter_values[nombres[[i]]][[1]] != ""){
      htp = param_set(htp, nombres[[i]],parameter_values[nombres[[i]]][[1]])
    }
    #  print(htp)
    i = i + 1
  }

  htp = substr(x = htp, start = 28, stop = nchar(htp))

  return(htp)
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
  # Remove bad formatting
  data$timestamp = substr(data$timestamp,
                          start = 1, 
                          stop = nchar(data$timestamp) -1)
  
  # Change Date Format from ISO 8601 to POSIX
  data$timestamp = parse_iso_8601(data$timestamp)
  
  id = unique(data$get_params.prdctvId)
  
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

create_transactions = function(trans){
  
  new$items = as.character(new$items)
  
  transactions = list()
  for(i in 1:nrow(trans)){
    single = character()
    for(page in strsplit(x = trans$items[i], split = ",")[[1]]){
      single = eappend(single, page)
    }
    transactions = lappend(transactions, single)
  }
  return(transactions)
}

exit = process(data){
    data = read.csv(file.choose(),
                encoding = "UTF-8",
                stringsAsFactors = F)
    new = create_trans(data = data, tolerance = 120)
    trans = create_transactions(new)
}


