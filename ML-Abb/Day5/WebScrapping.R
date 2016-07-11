#Web Scrapping

# We would like to get data for "preseason 2014" for NFL players

library(rvest)

url1 <-
  "http://www.nfl.com/stats/categorystats?tabSeq=0&season=2014&seasonType=PRE&d-447263-n=1&d-447263-o=2&statisticCategory=PASSING&conference=null&d-447263-p=1&d-447263-s=PASSING_YARDS"

url2 <-
  "http://www.nfl.com/stats/categorystats?tabSeq=0&season=2014&seasonType=PRE&d-447263-n=1&d-447263-o=2&conference=null&statisticCategory=PASSING&d-447263-p=2&d-447263-s=PASSING_YARDS"

url3 <-
  "http://www.nfl.com/stats/categorystats?tabSeq=0&season=2014&seasonType=PRE&d-447263-n=1&d-447263-o=2&statisticCategory=PASSING&conference=null&d-447263-p=3&d-447263-s=PASSING_YARDS"


tbl1 <- url1  %>%
  html()  %>%
  html_nodes(xpath = '//*[@id="result"]')  %>%
  html_table()
tbl1 <- tbl1[[1]]

tbl2 <- url2 %>%
  html() %>%
  html_nodes(xpath = '//*[@id="result"]') %>%
  html_table()

tbl2 <- tbl2[[1]]

tbl3 <- url3 %>%
  html() %>%
  html_nodes(xpath = '//*[@id="result"]') %>%
  html_table()

tbl3 <- tbl3[[1]]

data <- rbind(tbl1,tbl2,tbl3)
View(data)

library(dplyr)

topPlayers <- data %>%
  arrange(desc(Rate)) %>%
  slice(1:15)

library(ggplot2)

ggplot(topPlayers, aes(x = Rate, y = reorder(Player,Rate))) +
  geom_point()
