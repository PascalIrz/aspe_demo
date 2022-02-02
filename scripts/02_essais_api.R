library(httr)
library(jsonlite)
library(tidyverse)

depts_reg <- depts %>% 
  sf::st_drop_geometry() %>% 
  filter(str_length(code_insee) == 2) %>% # métropole
  mutate(code_region = str_sub(nuts3, 3, 4),
         region = case_when(code_region == ))

url <- "https://geo.api.gouv.fr/departements?fields=nom,code,codeRegion,region"

# resultats <- 
#   httr::content(
#     httr::GET(url),             # url correspond à l'url à interroger
#     as="text",                  # type de la sortie renvoyée
#     httr::content_type_json(),  # type de la réponse de l'url
#     encoding= "UTF-8"            # encodage de la réponse de l'url
#   )
# 
# df <- data.frame(
#   lapply(c("nom_dept","code_dept","code_region"), function(x){
#     jsonlite::fromJSON(resultats, flatten = TRUE)$product[[x]]
#   })
# )
# colnames(df) <- c("product_name","nova_groups","nutriscore_grade")
# df  

# -------------------------------

dept_reg <- RJSONIO::fromJSON("url")

data <- RJSONIO::fromJSON("https://geo.api.gouv.fr/communes?codePostal=35200&fields=nom,code,codesPostaux,codeDepartement,codeRegion,population&format=json&geometry=centre")
  
get_by_code <- function(query){
  url <- paste("https://geo.api.gouv.fr/communes?codePostal=",query, "&fields=nom,code,codesPostaux,codeDepartement,codeRegion,population&format=geojson&geometry=contour", sep = "")
  result <- RJSONIO::fromJSON(url)
  return(result)
}


prov <- get_by_code("35200")

geojsonsf::geojson_sf(prov %>% unlist())


# en json ; très lent / dernière étape
url <- "https://geo.api.gouv.fr/communes?fields=nom,code,codeDepartement,codeRegion,population&format=json&geometry=centre"
reponse <- httr::GET(url)
reponse2 <- reponse$content
reponse3 <- rawToChar(reponse2)
reponse4 <- RJSONIO::fromJSON(reponse3, encoding = "UTF-8")
# reponse5 <- reponse4 %>% 
#   reduce(rbind) %>% 
#   as.data.frame()
# reponse5 <- enframe(reponse4 %>% unlist(recursive = F))
# reponse5 <- bind_rows(reponse4%>% unlist(recursive = T))
# reponse5 <- purrr::map_df(reponse4 %>% unlist(recursive = F), dplyr::bind_rows)
# reponse5 <- purrr::map_df(reponse4 %>% unlist(recursive = T), ~.x)
# reponse5 <- as.data.frame(reponse4)
# reponse5 <- as_data_frame(reponse4)
# do.call(rbind.data.frame, reponse4)
# sample <- reponse4[1:34975]
reponse5 <- reponse4[map(reponse4, length) == 5] # pbs sur communes où champs manquants (st paul amsterdam)
reponse6 <- reponse5 %>% map(as.data.frame)
reponse7 <- reponse6 %>% bind_rows


# en geojson
url <- "https://geo.api.gouv.fr/communes?codePostal=35200&fields=nom,code,codeDepartement,codeRegion,population&format=geojson&geometry=contour"
reponse <- httr::GET(url)
reponse2 <- reponse$content
reponse3 <- rawToChar(reponse2)
reponse4 <- RJSONIO::fromJSON(reponse3)
reponse5 <- sf::st_read(reponse4 %>% unlist(recursive = F))


# en geojson, france entière
url <- "https://geo.api.gouv.fr/communes?codePostal=35000&fields=nom,code,codesPostaux,codeDepartement,codeRegion,population&format=geojson&geometry=contour"
reponse <- httr::GET(url)
reponse2 <- reponse$content
reponse3 <- rawToChar(reponse2)
# reponse4 <- geojsonio::geojson_read(reponse3, parse = T)
# reponse4 <- sf::st_read(reponse3, parse = T)
reponse4 <- RJSONIO::fromJSON(reponse3)
reponse5 <- reponse4$features
# reponse6 <- sf::st_read(reponse5)
# reponse6 <- geojsonio::geojson_read(reponse5, what = "sp")
reponse6 <- geojsonR::FROM_GeoJson(reponse3)
#reponse7 <- geojsonio::geojson_read(reponse6, what = "sp")
reponse7 <- geojsonio::geojson_read(reponse6, what = "list", parse = T)
reponse7 <- sf::st_read(reponse4)
reponse7 <- rgdal::readOGR(reponse6)


class(reponse6$features)

# -------------------------
url_ae <- "ftp://Admin_Express_ext:Dahnoh0eigheeFok@ftp3.ign.fr/ADMIN-EXPRESS-COG_3-0__SHP__FRA_L93_2021-05-19.7z"
myfile <- tempfile()
download.file(url_ae, destfile = myfile)




