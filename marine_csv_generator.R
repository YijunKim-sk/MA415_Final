# Marine Ecology

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

service_key <- "qChQ4KPdlNcuGXDwbaDmoh0vspXIsKOYrjfNZxX5SL0MECi4z8GWAnX9rhQoVDhs9bSZ7Ig6+4r50sqCy96gvA=="
dates <- seq(ymd("2025-02-02"), ymd("2025-04-01"), by = "1 day") %>%
  format("%Y%m%d")

results <- list()

for (date in dates) {
  Sys.sleep(0.3)  
  tryCatch({
    res <- GET(
      url = "https://apis.data.go.kr/1192136/vortex/GetVortexApiService",
      query = list(
        serviceKey = service_key,
        type = "json",
        reqDate = date,
        numOfRows = 10,
        pageNo = 1
      )
    )
    
    if (status_code(res) == 200) {
      result <- fromJSON(content(res, "text", encoding = "UTF-8"))
      item <- result$response$body$items$item
      if (!is.null(item)) {
        for (i in seq_along(item$uri)) {
          results[[length(results) + 1]] <- list(
            date = date,
            uri = item$uri[[i]]
          )
        }
      }
    }
  }, error = function(e) {
    message(paste("Failed on", date, ":", e$message))
  })
}

eddy_df <- bind_rows(results)
write.csv(eddy_df, "data/eddy_image_2025.csv", row.names = FALSE)
