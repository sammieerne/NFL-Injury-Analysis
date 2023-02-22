if(require(pacman)== FALSE) install.packages("pacman")
pacman::p_load(tidyverse, 
               tidycensus, # for getting the census data
               httr, jsonlite, # pkgs that we might use for API,
               janitor,
               lubridate,# for making a column name from row 1
               magrittr, rvest, httr, xml2 )

grass_turf_url= "https://www.nbcsports.com/washington/football-team/which-nfl-stadiums-have-artificial-turf-vs-real-grass"
grass_turf= read_html(grass_turf_url)
grass_turf2= html_elements(grass_turf, "p > span > span > span > span > span > span")
grass_turf3= grass_turf2[1][[1]]
# WORK IN PROGRESS

nfl_turf= "https://www.foxweather.com/lifestyle/artificial-turf-vs-real-grass-nfl-stadiums-football"
nfl_turf2= read_html(nfl_turf)
nfl_turf3= html_elements(nfl_turf2, " > ul")
nfl_turf4= nfl_turf3[[1]][1]

nfl_1= "https://www.nbcchicago.com/news/sports/nbcsports/which-nfl-stadiums-have-artificial-turf-vs-real-grass/2711863/"
nfl_2= read_html(nfl_1)
nfl_3= html_elements(nfl_2, "div > div.article-content.rich-text > p > span > span > span > span > span > span")
nfl_4= nfl_3[[1]][[1]]
library(xml2)
nfl_5= xml_attrs(nfl_4)


## WIKIPEDIA TABLE ##
nfl_wiki= "https://en.wikipedia.org/wiki/List_of_current_National_Football_League_stadiums"
nfl_wiki2= read_html(nfl_wiki)
nfl_wiki3<- nfl_wiki2 %>%
  html_node("table") %>%
  html_table()
nfl_wiki4= html_elements(nfl_wiki2, "#mw-content-text > div.mw-parser-output > table:nth-child(22) > tbody")
nfl_wiki5= html_table(nfl_wiki4, header= TRUE)
nfl_wiki6= nfl_wiki5[[1]]
nfl_wiki7= data_frame(nfl_wiki6)
nfl_wiki8=nfl_wiki7[-1]
nfl_wiki9= nfl_wiki8[-8]
write_csv(nfl_wiki9, 'nfl_wikipedia_data')

  

  
  