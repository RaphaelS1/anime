library(data.table)

# Load scraped data
anime_list <- jsonlite::read_json("data/data.txt")
anime_list <- lapply(anime_list, function(x) {
  x$title <- NULL
  x$startDate <- as.Date(
    sprintf("%s-%s-01", x$startDate$year, x$startDate$month)
  )
  x$endDate <- as.Date(
    sprintf("%s-%s-01", x$endDate$year, x$endDate$month)
  )
  if (length(x$endDate) == 0) {
    x$endDate <- NA
  }
  x$genres <- paste0(
    x$genres,
    collapse = ","
  )
  x$tags <- paste0(
    vapply(x$tags, "[[", character(1), "name")[seq(min(3, length(x$tags)))],
    collapse = ","
  )
  x
})

anime_df <- data.table(do.call(rbind, anime_list))
rm(list = setdiff(ls(), "anime_df"))