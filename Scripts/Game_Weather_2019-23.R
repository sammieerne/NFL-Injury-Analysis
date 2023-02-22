pacman::p_load(tidyverse, rvest, stringr)

schedule = read.csv('data/NFL_Schedule_2019_23.csv')
since = read.csv('data/NFL_Days_Since_Last_Game.csv')

schedule$Date = since$Date |> 
  as.Date(format = "%m/%d/%y")

schedule = schedule |> 
  dplyr::mutate(
    Season = ifelse(schedule$Week == 'WildCard', as.character(as.numeric(format(schedule$Date, format = "%Y")) - 1),
                    ifelse(schedule$Week == 'Division', as.character(as.numeric(format(schedule$Date, format = "%Y")) - 1),
                           ifelse(schedule$Week == 'ConfChamp', as.character(as.numeric(format(schedule$Date, format = "%Y")) - 1),
                                  ifelse(schedule$Week == 'SuperBowl', as.character(as.numeric(format(schedule$Date, format = "%Y")) - 1),
                                         ifelse((schedule$Week == '17' & format(schedule$Date, format = "%m") == '01') | (schedule$Week == '18' & format(schedule$Date, format = "%m") == '01'), as.character(as.numeric(format(schedule$Date, format = "%Y")) - 1),
                                                format(schedule$Date, format = "%Y"))))))
  )



# create an empty list to store the results
results <- list()

# loop through each row of the data frame
for (i in 1:nrow(schedule)) {
  # extract the variables for the current row
  season = schedule$season[[i]]
  week = schedule$Week[[i]]
  date = schedule$Date[[i]]
  away = tail(strsplit(schedule$Away[i], " ")[[1]], 1)
  away = ifelse(away == 'Team' & date >= '2020-09-13' & date <= '2020-09-20', 
                'redskins',
                ifelse(away == 'Team' & date > '2020-09-20' & date <= '2020-11-22', 
                       'football%20team',
                       ifelse(away == 'Team' & date > '2020-11-22' & date <= '2020-12-13',
                              'Washington',
                              ifelse(away == 'Team' & date == '2020-12-20',
                                     'football%20team',
                                     ifelse((away == 'Team' | away == 'Commanders') & date > '2020-12-20',
                                            'Washington',
                                            tail(strsplit(schedule$Away[i], " ")[[1]], 1))))))
  
  home = tail(strsplit(schedule$Home[i], " ")[[1]], 1)
  home = ifelse(home == 'Team' & date >= '2020-09-13' & date <= '2020-09-20', 
                'redskins',
                ifelse(home == 'Team' & date > '2020-09-20' & date <= '2020-11-22', 
                       'football%20team',
                       ifelse(home == 'Team' & date > '2020-11-22' & date <= '2020-12-13',
                              'washington',
                              ifelse(home == 'Team' & date == '2020-12-20',
                                     'football%20team',
                                     ifelse((home == 'Team' | home == 'Commanders') & date > '2020-12-20',
                                            'washington',
                                            tail(strsplit(schedule$Home[i], " ")[[1]], 1))))))
  
  
  # construct the URL and read the HTML
  url = ifelse(week == 'WildCard', 
               paste0('https://www.nflweather.com/game/', season, '/',  week,'-weekend/', away, '-at-', home),
               ifelse(week == 'Division', paste0('https://www.nflweather.com/game/', season, '/',  week,'al-playoffs/', away, '-at-', home),
                      ifelse(week == 'ConfChamp', paste0('https://www.nflweather.com/game/', season, '/%20conf-championships/', away, '-at-', home),
                             ifelse(week == 'SuperBowl' & season == '2021', paste0('https://www.nflweather.com/game/', season, '/',  week, '/', home, '-at-', away),
                                    ifelse(week == 'SuperBowl', paste0('https://www.nflweather.com/game/', season, '/', week, '/', away, '-at-', home),
                                           paste0('https://www.nflweather.com/game/', season, '/week-',  week, '/', away, '-at-', home))))))
  
  read_url <- read_html(url)
  
  # extract the weather data from each quarter
  q1_temp <- html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(1) > p:nth-child(5) > b')) |> 
    str_extract("\\d+") |> 
    as.numeric()
  q2_temp <- html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(2) > p:nth-child(5) > b')) |> 
    str_extract("\\d+") |> 
    as.numeric()
  q3_temp <- html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(3) > p:nth-child(5) > b')) |> 
    str_extract("\\d+") |> 
    as.numeric()
  q4_temp <- html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(4) > p:nth-child(5) > b')) |> 
    str_extract("\\d+") |> 
    as.numeric()
  
  q1_feels_like = html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(1) > p:nth-child(6) > b')) |> 
    str_extract("\\d+") |> 
    as.numeric()
  q2_feels_like = html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(2) > p:nth-child(6) > b')) |> 
    str_extract("\\d+") |> 
    as.numeric()
  q3_feels_like = html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(3) > p:nth-child(6) > b')) |> 
    str_extract("\\d+") |> 
    as.numeric()
  q4_feels_like = html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(4) > p:nth-child(6) > b')) |> 
    str_extract("\\d+") |> 
    as.numeric()
  
  
  q1_wind = html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(1) > p:nth-child(7) > b')) |> 
    str_extract("\\d+") |> 
    as.numeric()
  q2_wind = html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(2) > p:nth-child(7) > b')) |> 
    str_extract("\\d+") |> 
    as.numeric()
  q3_wind = html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(3) > p:nth-child(7) > b')) |> 
    str_extract("\\d+") |> 
    as.numeric()
  q4_wind = html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(4) > p:nth-child(7) > b')) |> 
    str_extract("\\d+") |> 
    as.numeric()
  
  q1_humidity = html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(1) > p:nth-child(8) > b')) |> 
    str_extract("\\d+") |> 
    as.numeric()
  q2_humidity = html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(2) > p:nth-child(8) > b')) |> 
    str_extract("\\d+") |> 
    as.numeric()
  q3_humidity = html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(3) > p:nth-child(8) > b')) |> 
    str_extract("\\d+") |> 
    as.numeric()
  q4_humidity = html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(4) > p:nth-child(8) > b')) |> 
    str_extract("\\d+") |> 
    as.numeric()
  
  q1_percipitation_prob = html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(1) > p:nth-child(13) > b')) |> 
    str_extract("\\d+") |> 
    as.numeric()
  q2_percipitation_prob = html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(2) > p:nth-child(13) > b')) |> 
    str_extract("\\d+") |> 
    as.numeric()
  q3_percipitation_prob = html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(3) > p:nth-child(13) > b')) |> 
    str_extract("\\d+") |> 
    as.numeric()
  q4_percipitation_prob = html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(4) > p:nth-child(13) > b')) |> 
    str_extract("\\d+") |> 
    as.numeric()
  
  # calculate the average weather data for the game
  avg_temp = mean(c(q1_temp, q2_temp, q3_temp, q4_temp))
  avg_feels_like = mean(c(q1_feels_like, q2_feels_like, q3_feels_like, q4_feels_like))
  avg_wind_mph = mean(c(q1_wind, q2_wind, q3_wind, q4_wind))
  avg_humidity_percent = mean(c(q1_humidity, q2_humidity, q3_humidity, q4_humidity))
  avg_percipitation_prob_percent = mean(c(q1_percipitation_prob, q2_percipitation_prob, q3_percipitation_prob, q4_percipitation_prob))
  
  
  
  # create a data frame with the results for the current row
  result <- data.frame(week, season, date, away, home, avg_temp, avg_feels_like, avg_wind_mph, avg_humidity_percent, avg_percipitation_prob_percent)
  
  # add the data frame to the results list
  results[[i]] = result
}

# combine the results into a single data frame
weather = do.call(rbind, results)

write.csv(weather, "Game_Weather_2019-23.csv")

schedule = schedule |> 
  dplyr::mutate(
    Avg_Temp = weather$avg_temp,
    Avg_Feels_Like = weather$avg_feels_like,
    Avg_Wind_MPH = weather$avg_wind_mph,
    Avg_Humidity_Percent = weather$avg_humidity_percent,
    Avg_Percipitation_Prob_Percent = weather$avg_percipitation_prob_percent
  )

write.csv(schedule, "NFL_Schedule_2019-23.csv")








#forecast = html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(1) > p:nth-child(4)'))
#visibility = html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(1) > p:nth-child(9) > b'))
#barometer = html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(1) > p:nth-child(10) > b'))
#dew_point = html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(1) > p:nth-child(11) > b'))
#cloud_cover = html_text2(html_elements(read_url, 'body > div > div:nth-child(4) > div > div.span10 > div:nth-child(7) > div:nth-child(1) > p:nth-child(12) > b'))

