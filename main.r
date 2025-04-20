# Установка и загрузка необходимых библиотек 
if (!require("leaflet")) install.packages("leaflet") 
if (!require("dplyr")) install.packages("dplyr") 
if (!require("htmltools")) install.packages("htmltools") 
if (!require("jsonlite")) install.packages("jsonlite") 
if (!require("htmlwidgets")) install.packages("htmlwidgets") 

library(leaflet) 
library(dplyr) 
library(htmltools) 
library(jsonlite) 
library(htmlwidgets) 

# Установка рабочей директории 
setwd("/home/puslore/Workspace/CMS/Project") 
options(browser = "/usr/bin/waterfox") 

# Загрузка данных 
data <- read.csv("./data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE) 

print("Количество квартир в регионе") 
sum(data['Region'] == 'Moscow region') 

print("Количество квартир в Москве") 
sum(data['Region'] == 'Moscow') 

# Добавил столбик "стоимость за кв. м" 
data["price_per_meter"] = data["Price"]/data["Area"] 

# Средняя цена за квадратный метр по станциям метро 
average_price_per_meter <- aggregate(price_per_meter ~ Metro.station, data = data, FUN = mean) 
head(average_price_per_meter)

count_per_station <- aggregate(Price ~ Metro.station, data = data, FUN = length) 
colnames(count_per_station) <- c("Metro.station", "Count") 

# Количество квартир на станцию метро (с фильтрацией Count > 40) 
count_filtered <- count_per_station[count_per_station$Count > 40, ] 

# Объединение таблиц 
average_price_per_meter_location <- merge( 
  average_price_per_meter, 
  count_filtered, 
  by = "Metro.station" 
) 

# [станция_метро, стоимость_квартиры] 
flats_for_map <- data.frame(Metro.station = data$Metro.station, Price = data$Price) 
head(flats_for_map)

# Функция для извлечения данных о станциях из JSON файла 
extract_stations <- function(file_path) { 
  json_data <- fromJSON(file_path, simplifyDataFrame = FALSE) 
  stations_list <- list() 
  
  for (i in 1:length(json_data$lines)) { 
    line <- json_data$lines[[i]] 
    for (j in 1:length(line$stations)) { 
      station <- line$stations[[j]] 
      stations_list[[length(stations_list) + 1]] <- data.frame( 
        Metro.station = station$name, 
        lat = station$lat, 
        lng = station$lng, 
        line_name = line$name, 
        line_color = paste0("#", line$hex_color), 
        stringsAsFactors = FALSE 
      ) 
    } 
  } 
  
  return(do.call(rbind, stations_list)) 
} 

# Извлечение станций только из одного JSON файла 
metro_coords_df <- extract_stations("metro_stations_data.json") 

# Печать информации о количестве станций 
cat("Количество станций в файле:", nrow(metro_coords_df), "\n") 

# Обеспечиваем уникальность станций (если есть дубликаты) 
metro_coords_df <- metro_coords_df %>% 
  group_by(Metro.station) %>% 
  slice(1) %>%  # Берем первую запись для каждой станции
  ungroup() 

cat("Количество уникальных станций:", nrow(metro_coords_df), "\n") 

# Объединение данных о ценах с координатами 
map_data <- merge(average_price_per_meter_location, metro_coords_df, by = "Metro.station", all.x = TRUE) 

# Проверка наличия пропущенных координат 
missing_coords <- map_data[is.na(map_data$lat) | is.na(map_data$lng), ] 
if (nrow(missing_coords) > 0) { 
  warning(paste("Отсутствуют координаты для следующих станций:", 
                paste(missing_coords$Metro.station, collapse = ", "))) 
} 

# Удаление записей с отсутствующими координатами 
map_data <- map_data %>% 
  filter(!is.na(lat) & !is.na(lng)) 

# Создаем подписи для всплывающих окон 
labels <- sprintf( 
  " **%s** <br>Средняя цена за м²: %s ₽<br>Количество квартир: %d<br>Линия: %s", 
  map_data$Metro.station, 
  format(round(map_data$price_per_meter), big.mark = " "), 
  map_data$Count, 
  map_data$line_name 
) %>% lapply(htmltools::HTML) 

# Определяем цветовую палитру в зависимости от цены 
pal <- leaflet::colorNumeric( 
  palette = "RdYlBu", 
  domain = map_data$price_per_meter, 
  reverse = TRUE 
) 

# Создаем карту 
map <- leaflet(map_data) %>% 
  # Добавляем базовую карту 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  # Устанавливаем начальный вид на Москву 
  setView(lng = 37.62, lat = 55.75, zoom = 10) %>% 
  # Добавляем круги, размер которых зависит от количества квартир 
  addCircleMarkers( 
    lng = ~lng, 
    lat = ~lat, 
    radius = ~sqrt(Count) * 1.5,  # Размер круга зависит от количества квартир 
    color = ~pal(price_per_meter),  # Цвет зависит от цены 
    stroke = FALSE, 
    fillOpacity = 0.7, 
    popup = labels, 
    group = "Станции" 
  ) %>% 
  # Добавляем легенду 
  addLegend( 
    position = "bottomright", 
    pal = pal, 
    values = ~price_per_meter, 
    title = "Цена за м²", 
    labFormat = labelFormat(suffix = " ₽", big.mark = " ") 
  ) 

# Улучшаем карту добавлением дополнительных функций 
map <- map %>% 
  # Добавляем собственный заголовок 
  addControl(html = "<h3>Средняя стоимость квартир по станциям метро</h3>", position = "topright") %>%
  # Добавляем элементы управления слоями
  addLayersControl(
    overlayGroups = c("Станции"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Сохраняем карту в HTML файл
saveWidget(map, "moscow_property_prices.html", selfcontained = TRUE)

# Открываем карту в браузере
browseURL("moscow_property_prices.html")
