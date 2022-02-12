
# import libraries
pacman::p_load(jsonlite, tidyverse, glue, httr)

# load api_key from config.json file
key <- read_json("config.json")

# declare target movie ids
movie_id <- 560:565

# create vector of urls with movie ids and api_key
url_list <- glue('https://api.themoviedb.org/3/movie/{movie_id}?api_key={key$api_key}&language=en-US')

# customize GET function to extract content, parse from raw to CHar and extract JSON
customGET <- function (url) {
  response <- GET(url)
  res <- response$content %>%
    rawToChar() %>%
    fromJSON()

  return(res)
}

# vectorize customGET function to easily call urls from url_list
vCustomGET <- Vectorize(customGET, vectorize.args = "url")

# call urls from url_list
response_list <- vCustomGET(url = url_list)

# convert response to dataframe
df <- as.data.frame(t(rbind(response_list)))

# declare relevant headers
df_columns <- c('budget', 'id', 'imdb_id', 'original_title', 'release_date', 'revenue', 'runtime', 'genres')

# select desired headers and unnest all except genres
df <- df %>%
  select(all_of(df_columns)) %>%
  rename('movie_id' = 'id') %>%
  unnest(cols = -c(genres))

# create table of unique genre name and id
genres_df <- df %>% select(genres) %>% pull() %>% Reduce(rbind, .) %>% data.frame() %>% unique()

# create funtion to serialize genre columns
serializer_fn <- function(x) {
  if_else(!is.na(x), 1, 0)
}

# explode genre column and serialize
df <- df %>%
  unnest(genres) %>%
  pivot_wider(names_from = name, values_from = id, values_fn = serializer_fn, values_fill = 0)

# create datetime  table
datetime_df <- df %>%
  select(movie_id, release_date) %>%
  mutate(release_date= lubridate::ymd(release_date),
         day = as.numeric(format(release_date, "%d")),
         month = as.numeric(format(release_date, "%m")),
         year = as.numeric(format(release_date, "%Y")),
         day_of_week = format(release_date, "%A"),
         )

# load data to desired location (excel spreadsheet this time)
library(writexl)

# assign each table to a separate worksheet
export_list <- list(
  'tmdb_movies' = df,
  'genres' = genres_df,
  'tmdb_datetimes' = datetime_df
)

# write to xlsx file
writexl::write_xlsx(export_list, 'tmdb_export.xlsx')

