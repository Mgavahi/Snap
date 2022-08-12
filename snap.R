### Import the libraries ----
library(tidyverse) ## for data wrangling.
library(jsonlite) ## for parsing json.
library(httr) ## for making request.
library(glue) ## for interpreted string.
library(ggmap) ## for geometric points.

### Getting the Api URL
url <- "https://app.snapp.taxi/api/api-base/v2/passenger/newprice/s/6/0"

### Initial and final points ----
# Using yours data points.
data_point <- "{\"points\":[{\"lat\":\"38.******\",\"lng\":\"57.******\"},{\"lat\":\"38.******\",\"lng\":\"57.******\"},null],\"waiting\":null,\"round_trip\":false,\"voucher_code\":null,\"service_types\":[1],\"hurry_price\":null,\"hurry_flag\":null,\"priceriderecom\":false}"

### Headers Api URL
headers <- c(
    "authority" = "app.snapp.taxi",
    "accept" = "*/*",
    "accept-language" = "en-US,en;q=0.9",
    "app-version" = "pwa",
    "authorization" = "*****", # Add yours Authorization.
    "content-type" = "application/json",
    "cookie" = "*****", # Add yours Cookie.
    "locale" = "fa-IR",
    "origin" = "https://app.snapp.taxi",
    "referer" = "https://app.snapp.taxi/pre-ride",
    "sec-fetch-dest" = "empty",
    "sec-fetch-mode" = "cors",
    "sec-fetch-site" = "same-origin",
    "sec-gpc" = "1",
    "user-agent" = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/104.0.5112.81 Safari/537.36",
    "x-app-name" = "passenger-pwa",
    "x-app-version" = "5.0.1" )

### Sending file to server
snap_p <- POST(url = url,
               add_headers(.headers = headers), 
               body = data_point) |>
    stop_for_status() # sending error type.

### Json parse function for parsing request
snap_json_func <- function(req){
    snap_text = content(req, as = "text", encoding = "UTF-8") # convert to text.
    if(identical(snap_text, "")) warn("NO output for parse.") # if text return empty get warning.
    fromJSON(snap_text) # convert to json data.
}

snap_json <- snap_json_func(snap_p)
price_init <- snap_json$data$prices$final # getting price.

### Extracting initial and final points.
point_in_fi <- str_extract_all(data_point, "\\d+\\.*\\d*")

start_lat <- point_in_fi[[1]][1] |> as.numeric()
start_long <- point_in_fi[[1]][2] |> as.numeric()
final_lat <- point_in_fi[[1]][3] |> as.numeric()
final_long <- point_in_fi[[1]][4] |> as.numeric()

### Creating a data frame by start latitude and longitude
df_initial <- tibble(ID = 1, latitude = start_lat, longitude = start_long)

### Create circles data frame from the centers data frame
make_circles <- function(centers, radius, nPoints = 100){
    # centers: the data frame of centers with ID.
    # radius: radius measured in kilometer.
    # nPoints: number of points.
    meanLat <- mean(centers$latitude)
   
    # length per longitude changes with latitude, so need correction.
    radiusLon <- radius /111 / cos(meanLat/57.3) # Each degrees of longitude represents (40075 / 360) * cos(latitude) kilomemters.(1 radian = 57.3 degree (180 / pi))
    radiusLat <- radius / 111 # Each degree of the latitude represents 40075 / 360 kilometers.
    circleDF <- data.frame(ID = rep(centers$ID, each = nPoints))
    angle <- seq(0, 2*pi, length.out = nPoints)
    
    circleDF$lon <- unlist(lapply(centers$longitude, function(x) x + radiusLon * cos(angle)))
    circleDF$lat <- unlist(lapply(centers$latitude, function(x) x + radiusLat * sin(angle)))
    return(circleDF)
}

### here is the data frame for all circles
my_circle <- make_circles(df_initial, radius = 0.2)

### Creating data frame for latitude, longitiude and price
nPoint_data <- tibble(id = "", new_data = "")
nPoint_price <- tibble(id = "", new_price = "")

### Loop for getting all data and prices
for(i in c(1:nrow(my_circle))){
    
    # nPoint_data, data frame getting data latitude and longitude for all points for body in POST function.
    nPoint_data = nPoint_data |> 
        add_row(id = paste0(i), new_data = glue(
        "{{\"points\":[{{\"lat\":\"{my_circle$lat[i]}\",\"lng\":\"{my_circle$lon[i]}\"}},{{\"lat\":\"38.******\",\"lng\":\"57.******\"}},null],\"waiting\":null,\"round_trip\":false,\"voucher_code\":null,\"service_types\":[1],\"hurry_price\":null,\"hurry_flag\":null,\"priceriderecom\":false}}"
    )) |> mutate(long = glue("{my_circle$lon[i]}"), lat = glue("{my_circle$lat[i]}")) |>
        drop_na()
   
     # sending request
    snap_new_data <- POST(url = url,
                          add_headers(.headers = headers), 
                          body = nPoint_data$new_data[i + 1]) |> stop_for_status()
    # getting prices for any location.
    snap_new_data_json = snap_json_func(snap_new_data)
    price <- snap_new_data_json$data$prices$final
    
    # nPoint_price, data frame for adding all prices.
    nPoint_price = nPoint_price |> 
        add_row(id = paste0(i), new_price = as.character(price))
    
    # merging nPoint_data and nPoint_price to gether.
    full_data = nPoint_data |> merge(nPoint_price, by = "id")
}

### Getting prices and points that's less than initial point price
less_price <- full_data[-1, ] |> 
    distinct(new_price, .keep_all = TRUE) |> 
    arrange(new_price) |> 
    filter(new_price <= price_init)

less_price |>
    select(-2) |> view()
