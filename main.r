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

# Настройка Waterfox как браузера по умолчанию
# Укажите правильный путь к исполняемому файлу Waterfox
options(browser = "/usr/bin/waterfox") # Путь может отличаться, настройте под вашу систему

# Загрузка данных
data <- read.csv("./data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

print("Количество квартир в регионе")
sum(data['Region'] == 'Moscow region')

print("Количество квартир в Москве")
sum(data['Region'] == 'Moscow')

# Добавил столбик "стоимость за кв. м"
data["price_per_meter"] = data["Price"]/data["Area"]

# data
head(data)

# Средняя цена за квадратный метр по станциям метро
average_price_per_meter <- aggregate(price_per_meter ~ Metro.station, data = data, FUN = mean)

# average_price_per_meter
head(average_price_per_meter)

count_per_station <- aggregate(Price ~ Metro.station, data = data, FUN = length)
colnames(count_per_station) <- c("Metro.station", "Count")

# count_per_station
head(count_per_station)

# Количество квартир на станцию метро (с фильтрацией Count > 40)
count_filtered <- count_per_station[count_per_station$Count > 40, ]

# Объединение таблиц
average_price_per_meter_location <- merge(
  average_price_per_meter,
  count_filtered,
  by = "Metro.station"
)

# Станция метро, средняя стоимость за кв.м, количество
# average_price_per_meter_location
head(average_price_per_meter_location)

#[станция_метро, стоимость_квартиры]
flats_for_map <- data.frame(Metro.station = data$Metro.station, Price = data$Price)
# flats_for_map
head(flats_for_map)

# Загрузка и обработка JSON с координатами станций метро
metro_json <- fromJSON("metro_stations_data.json", simplifyDataFrame = FALSE)

# Извлечение данных о станциях из всех линий метро
metro_stations <- list()
for (i in 1:length(metro_json$lines)) {
  line <- metro_json$lines[[i]]  # Используем [[]] вместо $ для правильного доступа
  for (j in 1:length(line$stations)) {
    station <- line$stations[[j]]  # Используем [[]] для доступа к элементам списка
    metro_stations[[length(metro_stations) + 1]] <- data.frame(
      Metro.station = station$name,
      lat = station$lat,
      lng = station$lng,
      line_name = line$name,
      line_color = paste0("#", line$hex_color),
      stringsAsFactors = FALSE
    )
  }
}

# Объединение всех станций в единый датафрейм
metro_coords_df <- do.call(rbind, metro_stations)

# Удаление дубликатов (одна станция может встречаться на нескольких линиях)
metro_coords_df <- metro_coords_df %>%
  group_by(Metro.station) %>%
  slice(1) %>%
  ungroup()

# Объединение данных о ценах с координатами
map_data <- merge(average_price_per_meter_location, metro_coords_df, by = "Metro.station", all.x = TRUE)

# Проверка наличия пропущенных координат
missing_coords <- map_data[is.na(map_data$lat) | is.na(map_data$lng), ]
if (nrow(missing_coords) > 0) {
  warning(paste("Отсутствуют координаты для следующих станций:", 
                paste(missing_coords$Metro.station, collapse = ", ")))
}

# Удаление записей с отсутствующими координатами
map_data <- map_data %>% filter(!is.na(lat) & !is.na(lng))

# Создаем подписи для всплывающих окон
labels <- sprintf(
  "<strong>%s</strong><br/>Средняя цена за м²: %s ₽<br/>Количество квартир: %d<br/>Линия: %s",
  map_data$Metro.station, 
  format(round(map_data$price_per_meter), big.mark = " "), 
  map_data$Count,
  map_data$line_name
) %>% lapply(htmltools::HTML)

# Определяем цветовую палитру в зависимости от цены
# Используем функцию colorNumeric напрямую из пакета leaflet
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
  addControl(html = "<h3>Средняя стоимость жилья возле станций метро</h3>", position = "topright") %>%
  # Добавляем слои для переключения между разными базовыми картами
  addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
  # Добавляем контроль слоев
  addLayersControl(
    baseGroups = c("Light", "OSM", "Dark", "Satellite"),
    overlayGroups = c("Станции"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Сохраняем карту как HTML-файл без автоматического открытия
saveWidget(map, file = "moscow_property_prices.html", selfcontained = TRUE)

# Выводим путь к файлу
file_path <- file.path(getwd(), "moscow_property_prices.html")
cat("Карта сохранена в:", file_path, "\n")

# Открываем карту в Waterfox
# Используем tryCatch для обработки возможных ошибок
tryCatch({
  # Явно указываем путь к браузеру Waterfox
  browseURL(file_path, browser = getOption("browser"))
}, error = function(e) {
  cat("Ошибка при открытии браузера:", e$message, "\n")
  cat("Для просмотра карты откройте файл", file_path, "в браузере Waterfox вручную\n")
})
