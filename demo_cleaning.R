library(tidyverse)

df <- read.csv("data/demo_before_cleaning.csv", fileEncoding = "utf-8")

colnames(df) <- c("region", "town", "legal_district", "administrative_district",
                  "year", "households", "population_total", "population_male", "population_female")

town_map <- c("울릉읍" = "Ulleung-eup", "서면" = "Seo-myeon", "북면" = "Buk-myeon")

legal_map <- c(
  "도동리" = "Dodong-ri",
  "독도리" = "Dokdo-ri",
  "저동리" = "Jeodong-ri",
  "사동리" = "Sadong-ri",
  "남양리" = "Namyang-ri",
  "남서리" = "Namseo-ri",
  "태하리" = "Taeha-ri",
  "천부리" = "Cheonbu-ri",
  "나리"   = "Nari",
  "현포리" = "Hyeonpo-ri"
)

admin_map <- c(
  "도동1리" = "Dodong 1-ri", "도동2리" = "Dodong 2-ri", "도동3리" = "Dodong 3-ri",
  "독도리" = "Dokdo-ri",
  "저동1리" = "Jeodong 1-ri", "저동2리" = "Jeodong 2-ri", "저동3리" = "Jeodong 3-ri",
  "사동1리" = "Sadong 1-ri", "사동2리" = "Sadong 2-ri", "사동3리" = "Sadong 3-ri",
  "남양1리" = "Namyang 1-ri", "남양2리" = "Namyang 2-ri", "남양3리" = "Namyang 3-ri",
  "남서1리" = "Namseo 1-ri", "남서2리" = "Namseo 2-ri",
  "태하1리" = "Taeha 1-ri", "태하2리" = "Taeha 2-ri",
  "천부1리" = "Cheonbu 1-ri", "천부2리" = "Cheonbu 2-ri",
  "천부3리" = "Cheonbu 3-ri", "천부4리" = "Cheonbu 4-ri",
  "나리" = "Nari", "추산리" = "Chusan-ri",
  "현포1리" = "Hyeonpo 1-ri", "현포2리" = "Hyeonpo 2-ri"
)

df <- df %>%
  mutate(
    town = town_map[town],
    administrative_district = admin_map[administrative_district],
    legal_district = legal_map[legal_district]
  )

write.csv(df, "data/ulleung_population_cleaned.csv", row.names = FALSE)
