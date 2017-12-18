library(readr)
library(dplyr)

url <- "https://query.data.world/s/cen2Np2vlZrbaVwyXwFuTBtKVdtayO"

# Read data from the web
movies <- read_csv(url)

movies <- mutate(movies, movie_title = substr(movie_title,1,nchar(movie_title)-1))

oscars <-  read_csv("S:/Nagel_Derek.DN/academy_awards.csv")

oscars <-  mutate(oscars, was_nominated = "YES")

best_pictures  <-  filter(oscars, Category == "Best Picture")

movies <- left_join(movies, best_pictures, by = c("movie_title" = "Nominee"))