library("urltools")

pixel_hit_vivelafete <- read.csv(file.choose(), 
                                 stringsAsFactors = F)
url = "https://www.vivelafete.com/collegiate.html?q=cs136&college=puente&college=179&p=2,2015-10-24T22:09:19.908Z,https://www.vivelafete.com/collegiate.html?college=179&p=2,67.141.5.176&cat=89&size_range=41"


transform_url <-function(url){
  parameter_values <- param_get(url, c("p","college","q","cat","size_range","apparel_collection"))
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
transform_url(url)

i = 1
for(url in pixel_hit_vivelafete$get_params.url){
  pixel_hit_vivelafete[i, 2] = transform_url(url)
  i=i+1
}

write.csv(x = pixel_hit_vivelafete, file = "vivalafete.csv", row.names = F)
