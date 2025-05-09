library(readr)
library(httr)
library(magick)  

eddy_data <- read_csv("data/eddy_image_2025.csv")

dir.create("www/marine_images", recursive = TRUE, showWarnings = FALSE)

for (i in seq_len(nrow(eddy_data))) {
  date <- eddy_data$date[i]
  url <- eddy_data$uri[i]
  dest <- paste0("www/marine_images/", date, ".png")
  
  if (!file.exists(dest)) {
    try({
      tmp <- tempfile(fileext = ".png")
      GET(url, write_disk(tmp, overwrite = TRUE))
      
      img <- image_read(tmp)
      img_resized <- image_scale(img, "1000") 
      
      image_write(img_resized, path = dest, format = "png")
    }, silent = TRUE)
  }
}
