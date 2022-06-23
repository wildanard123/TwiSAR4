Server <- function(input, output, session) {
  
  showNotification("Mapping", duration = NULL, type = "message")

 
    
  # whatever name you assigned to your created app.  Mine is crd230
  appname <- "rmarkdown"
  
  ## api key 
  
  key <- "hrkyjBJDlj46U5bRNgdGsREmo"
  secret <- "rmCwYg8sWq0SoMmaUx1TvFFhKiQmPuS8UYXNHIR0HW2qQe0UpA"
  access_token <- "1452031524405014529-UjeJPEun3v67PP3mvYbs1HHc2BJQoo"
  access_secret <- "hbWIuvZ7ixqWKBi4aCzegHbYzofpwR5rF3RNuM8qJxFms"
  
  twitter_token <- create_token(
    app = appname,
    consumer_key = key,
    consumer_secret = secret,
    access_token = access_token,
    access_secret = access_secret)

#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////Crawling
  
  #mengumpulkan tweet terbaru yang berisi kata " ".search_tweets()
  #Batas pencarian yang dikembalikan ke 18.000 setiap 15 menit
  # Create 0-row data frame which will be used to store data
  dat2 <- data.frame(x = numeric(0), y = numeric(0))
  
  withProgress(message = 'Download Tweet Flood', value = 0, {
    # Number of times we'll go through the loop
    n <- 100
  
#    bbox=c(91.406250,-11.695273,140.800781,8.407168),  
  banjir_tweets <- search_tweets(q="#twisar banjir OR #banjir OR (banjir bandang) OR #banjirbandang OR (banjir rob) OR #banjirrob -#erupsi -#gunungberapi -ibadah -erupsi -pray -#pray -bebas -anti -kiamat -mimpi -celana -meletus -akhlak -introspeksi -direnungkan -@aniesbaswedan -@jokowi -anies -aniesbaswedan -prestasi -penghargaan -pujian -mimpi -basah -pray -praying -orderan -donasi -#TwitterBot -#BersamaKurangiRisiko -@petabencana -is:retweet -is:reply -is:quote lang:in", n = 750, include_rts = FALSE, ) 
  
  
  #Animasi Loading Selesai
  for (i in 1:n) {
    # Each time through the loop, add another row of data. This is
    # a stand-in for a long-running computation.
    dat2 <- rbind(dat2, data.frame(x = rnorm(1), y = rnorm(1)))
    # Increment the progress bar, and update the detail text.
    incProgress(1/n, detail = paste("Mapping", i)) 
    # Pause for 0.1 seconds to simulate a long computation.
    Sys.sleep(0.0001) 
  }
  })
#  plot(dat2$x, dat2$y)
  
  #Animasi Loading mulai
  # Create 0-row data frame which will be used to store data
  dat2 <- data.frame(x = numeric(0), y = numeric(0))
  
  withProgress(message = 'Download Tweet Angin Kencang', value = 0, {
    # Number of times we'll go through the loop
    n <- 100
    
  angin_tweets <- search_tweets(q="#twisar (angin kencang) OR (puting beliung) OR (angin topan) OR (angin puyuh) OR #anginpuyuh OR #angintopan OR #anginkencang OR #putingbeliung -#erupsi -#gunungberapi -ibadah -erupsi -berapi -meletus -pray -#pray -tinggi -serem -akhlak -introspeksi -direnungkan -emot -@aniesbaswedan -@jokowi -anies -buang -kipas -kece -halilintar -terbantai -anggap -donasi", n =750, include_rts = FALSE, ) 
  
    #Animasi Loading Selesai
    for (i in 1:n) {
      # Each time through the loop, add another row of data. This is
      # a stand-in for a long-running computation.
      dat2 <- rbind(dat2, data.frame(x = rnorm(1), y = rnorm(1)))
      # Increment the progress bar, and update the detail text.
      incProgress(1/n, detail = paste("Mapping", i)) 
      # Pause for 0.1 seconds to simulate a long computation.
      Sys.sleep(0.0001) 
    }
  })
  
#  plot(dat2$x, dat2$y)
  
  #Animasi Loading mulai
  # Create 0-row data frame which will be used to store data
  dat2 <- data.frame(x = numeric(0), y = numeric(0))
  
  withProgress(message = 'Download Tweets Gelombang Tinggi', value = 0, {
    # Number of times we'll go through the loop
    n <- 100
    
  gelombang_tweets <- search_tweets(q="#twisar #gelombangbesar OR #gelombangtinggi OR #ombaktinggi OR #ombakbesar OR (gelombang besar) OR (gelombang tinggi) OR (gelombang pasang) OR (abrasi pantai) OR (abrasi pesisir) OR (ombak besar) OR (ombak tinggi) OR (swell wave) -#erupsi -#gunungberapi -ibadah -erupsi -pray -#pray -akhlak -introspeksi -iman -direnungkan -elektromagnetik -masalah -protes -Bagai -cek -chek -tes -test -netizen -fans -dukungan -revolusi -hidup -vaksinasi -virus -corona -donasi", n = 750, include_rts = FALSE, ) 
  
  #Animasi Loading Selesai
  for (i in 1:n) {
    # Each time through the loop, add another row of data. This is3
    # a stand-in for a long-running computation.
    dat2 <- rbind(dat2, data.frame(x = rnorm(1), y = rnorm(1)))
    # Increment the progress bar, and update the detail text.
    incProgress(1/n, detail = paste("Mapping", i)) 
    # Pause for 0.1 seconds to simulate a long computation.
    Sys.sleep(0.0001) 
  }
  })
  
#  plot(dat2$x, dat2$y)
  
  #Animasi Loading mulai
  # Create 0-row data frame which will be used to store data
  dat2 <- data.frame(x = numeric(0), y = numeric(0))
  
  withProgress(message = 'Download Tweets Longsor', value = 0, {
    # Number of times we'll go through the loop
    n <- 100
  
  longsor_tweets <- search_tweets(q="#twisar (tanah longsor) OR #tanahlongsor -anti -menyadarkan -#gunung -#erupsi -#gunungberapi -ibadah -erupsi -pray -#pray -skincare -@jokowi -serem -kiamat -luntur -detak -meletus -akhlak -introspeksi -akhlak -direnungkan -sadar -akhir -otaknya -otak -empedu -empedunya -kisah -suci -kajian -salju -infografis -pray -praying -orderan -donasi -#TwitterBot -#BersamaKurangiRisiko -@petabencana lang:in", n = 750, include_rts = FALSE, ) 
  
  #Animasi Loading Selesai
  for (i in 1:n) {
    # Each time through the loop, add another row of data. This is
    # a stand-in for a long-running computation.
    dat2 <- rbind(dat2, data.frame(x = rnorm(1), y = rnorm(1)))
    # Increment the progress bar, and update the detail text.
    incProgress(1/n, detail = paste("Mapping", i)) 
    # Pause for 0.1 seconds to simulate a long computation.
    Sys.sleep(0.0001) 
  }
  })

#  plot(dat2$x, dat2$y)
  
  #Animasi Loading mulai
  # Create 0-row data frame which will be used to store data
  dat2 <- data.frame(x = numeric(0), y = numeric(0))
  
  withProgress(message = 'Download Tweets Gempa', value = 0, {
    # Number of times we'll go through the loop
    n <- 100
  
  gempa_tweets <- search_tweets(q="#twisar (gempa bumi) OR gempabumi OR #gempabumi -#erupsi -#gunungberapi -ibadah -erupsi -gunung -pray -#pray -basah -serem -kiamat -jantung -detak -meletus -iman -akhlak -introspeksi -direnungkan -jokowi -donasi -is:retweet -is:reply -is:quote lang:in", n = 750, include_rts = FALSE, ) 
  
  #Animasi Loading Selesai
  for (i in 1:n) {
    # Each time through the loop, add another row of data. This is
    # a stand-in for a long-running computation.
    dat2 <- rbind(dat2, data.frame(x = rnorm(1), y = rnorm(1)))
    # Increment the progress bar, and update the detail text.
    incProgress(1/n, detail = paste("Mapping", i)) 
    # Pause for 0.1 seconds to simulate a long computation.
    Sys.sleep(0.0001) 
  }
  })
 
# plot(dat2$x, dat2$y)
  
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////proses
  
  #Animasi Loading mulai
  # Create 0-row data frame which will be used to store data
  dat2 <- data.frame(x = numeric(0), y = numeric(0))
  
  withProgress(message = 'Pemetaan..', value = 0, {
    # Number of times we'll go through the loop
    n <- 100
    
  
  #menampilkan hasil pencarian
  
  glimpse(banjir_tweets) 
  glimpse(angin_tweets)
  glimpse(gelombang_tweets) 
  glimpse(longsor_tweets)
  glimpse(gempa_tweets)
  
  #Berapa banyak lokasi ditangkap oleh tweet yang berisi kata " "
  
  length(unique(banjir_tweets$place_full_name))
  length(unique(angin_tweets$place_full_name))
  length(unique(gelombang_tweets$place_full_name))
  length(unique(longsor_tweets$place_full_name))
  length(unique(gempa_tweets$place_full_name))
  
  #tabel dari 10 tempat teratas tweeting tentang " "
  
  banjir_tweets %>% 
    filter(is.na(place_full_name) == FALSE & place_full_name != "") %>% 
    count(place_full_name, sort = TRUE) %>% 
    slice(1:10)
  angin_tweets %>% 
    filter(is.na(place_full_name) == FALSE & place_full_name != "") %>% 
    count(place_full_name, sort = TRUE) %>% 
    slice(1:10)
  gelombang_tweets %>% 
    filter(is.na(place_full_name) == FALSE & place_full_name != "") %>% 
    count(place_full_name, sort = TRUE) %>% 
    slice(1:10)
  longsor_tweets %>% 
    filter(is.na(place_full_name) == FALSE & place_full_name != "") %>% 
    count(place_full_name, sort = TRUE) %>% 
    slice(1:10)
  gempa_tweets %>% 
    filter(is.na(place_full_name) == FALSE & place_full_name != "") %>% 
    count(place_full_name, sort = TRUE) %>% 
    slice(1:10)
   
 
#/////////////////////////////////////////////////////////////////////////////////////////   MENAMPILKAN PROFIL


#Banjir
  m <- banjir_tweets %>% 
  filter(is.na(place_full_name) == FALSE & place_full_name != "") %>% 
    count(profile_image_url, screen_name, status_url, sort = TRUE) %>%
    slice(1:10)
  output$image1<- renderUI({
    imgurl1 <- m[1,1]
    tags$img(src=imgurl1, width = 35, height = 35)
  })
  observeEvent(input$MyImage, { print("Hey there")})
  output$nama1 <- renderText({
    texturl1 <- m[1,2]
    paste(texturl1)
  })
  
  output$link1 <- renderUI({
    linkurl1 <- m[1,3]
    tags$a(href=linkurl1, class="btn btn-info","Go to tweet")
  })
  output$image2<- renderUI({
    imgurl1 <- m[2,1]
    tags$img(src=imgurl1, width = 35, height = 35)
  })
  observeEvent(input$MyImage, { print("Hey there")})
  output$nama2 <- renderText({
    texturl1 <- m[2,2]
    paste(texturl1)
  })
  output$link2 <- renderUI({
    linkurl1 <- m[2,3]
    tags$a(href=linkurl1, class="btn btn-info","Go to tweet")
  })
  
#Angin
  a <- angin_tweets %>% 
    filter(is.na(place_full_name) == FALSE & place_full_name != "") %>% 
    count(profile_image_url, screen_name, status_url, sort = TRUE) %>%
    slice(1:10)
  output$image3<- renderUI({
    imgurl1 <- a[1,1]
    tags$img(src=imgurl1, width = 35, height = 35)
  })
  observeEvent(input$MyImage, { print("Hey there")})
  output$nama3 <- renderText({
    texturl1 <- a[1,2]
    paste(texturl1)
  })
  output$link3 <- renderUI({
    linkurl1 <- a[1,3]
    tags$a(href=linkurl1, class="btn btn-info","Go to tweet")
  })
  output$image4<- renderUI({
    imgurl1 <- a[2,1]
    tags$img(src=imgurl1, width = 35, height = 35)
  })
  observeEvent(input$MyImage, { print("Hey there")})
  output$nama4 <- renderText({
    texturl1 <- a[2,2]
    paste(texturl1)
  })
  output$link4 <- renderUI({
    linkurl1 <- a[2,3]
    tags$a(href=linkurl1, class="btn btn-info","Go to tweet")
  })
  
#Gelombng
  g <- gelombang_tweets %>% 
    filter(is.na(place_full_name) == FALSE & place_full_name != "") %>% 
    count(profile_image_url, screen_name, status_url, sort = TRUE) %>%
    slice(1:10)
  output$image5<- renderUI({
    imgurl1 <- g[1,1]
    tags$img(src=imgurl1, width = 35, height = 35)
  })
  observeEvent(input$MyImage, { print("Hey there")})
  output$nama5 <- renderText({
    texturl1 <- g[1,2]
    paste(texturl1)
  })
  output$link5 <- renderUI({
    linkurl1 <- g[1,3]
    tags$a(href=linkurl1, class="btn btn-info","Go to tweet")
  })
  output$image6<- renderUI({
    imgurl1 <- g[2,1]
    tags$img(src=imgurl1, width = 35, height = 35)
  })
  observeEvent(input$MyImage, { print("Hey there")})
  output$nama6 <- renderText({
    texturl1 <- g[2,2]
    paste(texturl1)
  })
  output$link6 <- renderUI({
    linkurl1 <- g[2,3]
    tags$a(href=linkurl1, class="btn btn-info","Go to tweet")
  })
  
  #Longsor
  L <- longsor_tweets %>% 
    filter(is.na(place_full_name) == FALSE & place_full_name != "") %>% 
    count(profile_image_url, screen_name, status_url, sort = TRUE) %>%
    slice(1:10)
  output$image7<- renderUI({
    imgurl1 <- L[1,1]
    tags$img(src=imgurl1, width = 35, height = 35)
  })
  observeEvent(input$MyImage, { print("Hey there")})
  output$nama7 <- renderText({
    texturl1 <- L[1,2]
    paste(texturl1)
  })
  output$link7 <- renderUI({
    linkurl1 <- L[1,3]
    tags$a(href=linkurl1, class="btn btn-info","Go to tweet")
  })
  output$image8<- renderUI({
    imgurl2 <- L[2,1]
    tags$img(src=imgurl2, width = 35, height = 35)
  })
  observeEvent(input$MyImage, { print("Hey there")})
  output$nama8 <- renderText({
    texturl2 <- L[2,2]
    paste(texturl2)
  })
  output$link8 <- renderUI({
    linkurl2 <- L[2,3]
    tags$a(href=linkurl2, class="btn btn-info","Go to tweet")
  })

#Gempa
  P <- gempa_tweets %>% 
    filter(is.na(place_full_name) == FALSE & place_full_name != "") %>% 
    count(profile_image_url, screen_name, status_url, sort = TRUE) %>%
    slice(1:10)
  output$image9<- renderUI({
    imgurl1 <- P[1,1]
    tags$img(src=imgurl1, width = 35, height = 35)
  })
  observeEvent(input$MyImage, { print("Hey there")})
  output$nama9 <- renderText({
    texturl1 <- P[1,2]
    paste(texturl1)
  })
  output$link9 <- renderUI({
    linkurl1 <- P[1,3]
    tags$a(href=linkurl1, class="btn btn-info","Go to tweet")
  })
  output$image10<- renderUI({
    imgurl2 <- P[2,1]
    tags$img(src=imgurl2, width = 35, height = 35)
  })
  observeEvent(input$MyImage, { print("Hey there")})
  output$nama10 <- renderText({
    texturl2 <- P[2,2]
    paste(texturl2)
  })
  output$link10 <- renderUI({
    linkurl2 <- P[2,3]
    tags$a(href=linkurl2, class="btn btn-info","Go to tweet")
  })
  

  #Animasi Loading Selesai
  for (i in 1:n) {
    # Each time through the loop, add another row of data. This is
    # a stand-in for a long-running computation.
    dat2 <- rbind(dat2, data.frame(x = rnorm(1), y = rnorm(1)))
    # Increment the progress bar, and update the detail text.
    incProgress(1/n, detail = paste("Mapping", i)) 
    # Pause for 0.1 seconds to simulate a long computation.
    Sys.sleep(0.0001) 
  }
  })
#  plot(dat2$x, dat2$y)
  
    
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  #visualisasikan distribusi menggunakan ggplot()
  output$grafikbanjir <- renderPlot({
    banjir_tweets %>%
    count(place_full_name, sort = TRUE) %>%
    mutate(location = reorder(place_full_name,n)) %>%
    na.omit() %>%
    top_n(10) %>%
    ggplot(aes(x = location,y = n)) +
    geom_col(fill = "#1DA1F2") +
    coord_flip() +
    labs(x = "Place",
         y = "Count",
         title = "Flood Tweets - locations ")
  })
  
  output$grafikangin <- renderPlot({
  angin_tweets %>%
    count(place_full_name, sort = TRUE) %>%
    mutate(location = reorder(place_full_name,n)) %>%
    na.omit() %>%
    top_n(10) %>%
    ggplot(aes(x = location,y = n)) +
    geom_col(fill = "#1DA1F2") +
    coord_flip() +
    labs(x = "Place",
         y = "Count",
         title = "angin Tweets - locations ")
  })
  
  output$grafikgelombang <- renderPlot({
  gelombang_tweets %>%
    count(place_full_name, sort = TRUE) %>%
    mutate(location = reorder(place_full_name,n)) %>%
    na.omit() %>%
    top_n(10) %>%
    ggplot(aes(x = location,y = n)) +
    geom_col(fill = "#1DA1F2") +
    coord_flip() +
    labs(x = "Place",
         y = "Count",
         title = "gelombang Tweets - locations ")
  })
  
  output$grafiklongsor <- renderPlot({
  longsor_tweets %>%
    count(place_full_name, sort = TRUE) %>%
    mutate(location = reorder(place_full_name,n)) %>%
    na.omit() %>%
    top_n(10) %>%
    ggplot(aes(x = location,y = n)) +
    geom_col(fill = "#1DA1F2") +
    coord_flip() +
    labs(x = "Place",
         y = "Count",
         title = "longsor Tweets - locations ")
  })
  
  output$grafikgempa <- renderPlot({
  gempa_tweets %>%
    count(place_full_name, sort = TRUE) %>%
    mutate(location = reorder(place_full_name,n)) %>%
    na.omit() %>%
    top_n(10) %>%
    ggplot(aes(x = location,y = n)) +
    geom_col(fill = "#1DA1F2") +
    coord_flip() +
    labs(x = "Place",
         y = "Count",
         title = "gempa Tweets - locations ")
  })
  
  #mengekstrak bujur dan garis lintang lokasi tweet
  banjir_tweets <- lat_lng(banjir_tweets)
  angin_tweets <- lat_lng(angin_tweets)
  gelombang_tweets <- lat_lng(gelombang_tweets)
  longsor_tweets <- lat_lng(longsor_tweets)
  gempa_tweets <- lat_lng(gempa_tweets)
  
  #menciptakan dua kolom baru lat&lng dan filter
  angin_tweets.geo <- angin_tweets %>%
    filter(is.na(lat) == FALSE & is.na(lng) == FALSE)
  banjir_tweets.geo <- banjir_tweets %>%
    filter(is.na(lat) == FALSE & is.na(lng) == FALSE)
  gelombang_tweets.geo <- gelombang_tweets %>%
    filter(is.na(lat) == FALSE & is.na(lng) == FALSE)
  longsor_tweets.geo <- longsor_tweets %>%
    filter(is.na(lat) == FALSE & is.na(lng) == FALSE)
  gempa_tweets.geo <- gempa_tweets %>%
    filter(is.na(lat) == FALSE & is.na(lng) == FALSE)
  
  #mengkonversi non-spatial data frames menjadi objek sf 
  angin_tweets.geo.sf <- st_as_sf(angin_tweets.geo, coords = c("lng", "lat"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84")
  banjir_tweets.geo.sf <- st_as_sf(banjir_tweets.geo, coords = c("lng", "lat"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84")
  gelombang_tweets.geo.sf <- st_as_sf(gelombang_tweets.geo, coords = c("lng", "lat"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84")
  longsor_tweets.geo.sf <- st_as_sf(longsor_tweets.geo, coords = c("lng", "lat"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84")
  gempa_tweets.geo.sf <- st_as_sf(gempa_tweets.geo, coords = c("lng", "lat"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84")
  
  #menambahkan icon
  
  logobanjir <- makeIcon(
    iconUrl = "gambar/banjir2.png",
    iconWidth = 20,
    iconHeight = 20,)
  logoangin <- makeIcon(
    iconUrl = "gambar/anginkencang2.png",
    iconWidth = 20,
    iconHeight = 20,)
  logogelombang <- makeIcon(
    iconUrl = "gambar/gelombangtinggi2.png",
    iconWidth = 20,
    iconHeight = 20,)
  logolongsor <- makeIcon(
    iconUrl = "gambar/longsor2.png",
    iconWidth = 20,
    iconHeight = 20,)
  logogempa <- makeIcon(
    iconUrl = "gambar/gempa2.png",
    iconWidth = 20,
    iconHeight = 20,)
  
  #membuat legenda
#  html_legend <- "<b>Legenda</b> </br> 
# <img src='gambar/banjir.png' Width = '15' Height = '15'/> Banjir <br/>
# <img src='gambar/angin.png' Width = '15' Height = '15'/> Angin kencang <br/>
# <img src='gambar/gelombang.png' Width = '15' Height = '15'> Gelombang tinggi <br/>
# <img src='gambar/longsor.png' Width = '15' Height = '15'> Tanah longsor <br/>
# <img src='gambar/gempa.png' Width = '15' Height = '15'> Gempa bumi "
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// TABEL
  

  
  # output$tablebanjir <- renderReactable({ 
  # table_banjir_tweets <- banjir_tweets %>% 
  #   filter(is.na(place_full_name) == FALSE & place_full_name != "")
  #   table_data_banjir <- select(table_banjir_tweets, db_banjir_tweets, status_id, created_at, text, hashtags, status_url, media_url, location, bbox_coords, favorite_count, retweet_count, quote_count, reply_count, user_id, verified, source, screen_name, profile_image_url, followers_count, account_created_at, profile_url)
  #   reactable(table_data_banjir,
  #             filterable = TRUE, searchable = TRUE, bordered = TRUE, 
  #             striped = TRUE, highlight = TRUE,
  #             defaultPageSize = 10, showPageSizeOptions = TRUE, 
  #             showSortable = TRUE, pageSizeOptions = c(10, 25, 50, 100), defaultSortOrder = "desc",
  #             columns = list(
  #               created_at = colDef(defaultSortOrder = "asc"),
  #               screen_name = colDef(defaultSortOrder = "asc"),
  #               text = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
  #               favorite_count = colDef(filterable = FALSE),
  #               retweet_count = colDef(filterable =  FALSE),
  #               urls_expanded_url = colDef(html = TRUE)
  #             ))
  # })
  
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// DATABASE
 
  db_banjir_tweets <- banjir_tweets %>% 
  filter(is.na(place_full_name) == FALSE & place_full_name != "")
  db_data_banjir <- select(db_banjir_tweets, status_id, created_at, text, hashtags, status_url, media_url, location, bbox_coords, favorite_count, retweet_count, quote_count, reply_count, user_id, verified, source, screen_name, profile_image_url, followers_count, account_created_at, profile_url)
    saveData <- function(db_data_banjir) {
    # Connect to the database
    db <- mongo(collection = "banjir", url = "mongodb+srv://wildanard123:Dhany123!@cluster0.caq2e.mongodb.net/db_twisar?retryWrites=true&w=majority")
    db$insert(db_data_banjir)
  }
  saveData(db_data_banjir) 
  
  db_angin_tweets <- angin_tweets %>% 
    filter(is.na(place_full_name) == FALSE & place_full_name != "")
  db_data_angin <- select(db_angin_tweets, status_id, created_at, text, hashtags, status_url, media_url, location, bbox_coords, favorite_count, retweet_count, quote_count, reply_count, user_id, verified, source, screen_name, profile_image_url, followers_count, account_created_at, profile_url)
  saveData <- function(db_data_angin) {
    # Connect to the database
    db <- mongo(collection = "angin_kencang", url = "mongodb+srv://wildanard123:Dhany123!@cluster0.caq2e.mongodb.net/db_twisar?retryWrites=true&w=majority")
    db$insert(db_data_angin)
  }
  saveData(db_data_angin) 
  
  db_gelombang_tweets <- gelombang_tweets %>% 
    filter(is.na(place_full_name) == FALSE & place_full_name != "")
  db_data_gelombang <- select(db_gelombang_tweets, status_id, created_at, text, hashtags, status_url, media_url, location, bbox_coords, favorite_count, retweet_count, quote_count, reply_count, user_id, verified, source, screen_name, profile_image_url, followers_count, account_created_at, profile_url)
  saveData <- function(db_data_gelombang) {
    # Connect to the database
    db <- mongo(collection = "gelombang_tinggi", url = "mongodb+srv://wildanard123:Dhany123!@cluster0.caq2e.mongodb.net/db_twisar?retryWrites=true&w=majority")
    db$insert(db_data_gelombang)
  }
  saveData(db_data_gelombang) 
  
  db_longsor_tweets <- longsor_tweets %>% 
    filter(is.na(place_full_name) == FALSE & place_full_name != "")
  db_data_longsor <- select(db_longsor_tweets, status_id, created_at, text, hashtags, status_url, media_url, location, bbox_coords, favorite_count, retweet_count, quote_count, reply_count, user_id, verified, source, screen_name, profile_image_url, followers_count, account_created_at, profile_url)
  saveData <- function(db_data_longsor) {
    # Connect to the database
    db <- mongo(collection = "tanah_longsor", url = "mongodb+srv://wildanard123:Dhany123!@cluster0.caq2e.mongodb.net/db_twisar?retryWrites=true&w=majority")
    db$insert(db_data_longsor)
  }
  saveData(db_data_longsor) 
  
  
  db_gempa_tweets <- gempa_tweets %>% 
    filter(is.na(place_full_name) == FALSE & place_full_name != "")
  db_data_gempa <- select(db_gempa_tweets, status_id, created_at, text, hashtags, status_url, media_url, location, bbox_coords, favorite_count, retweet_count, quote_count, reply_count, user_id, verified, source, screen_name, profile_image_url, followers_count, account_created_at, profile_url)
  saveData <- function(db_data_gempa) {
    # Connect to the database
    db <- mongo(collection = "gempa_bumi", url = "mongodb+srv://wildanard123:Dhany123!@cluster0.caq2e.mongodb.net/db_twisar?retryWrites=true&w=majority")
    db$insert(db_data_gempa)
  }
  saveData(db_data_gempa) 

 
#  banjir = mongo("Benders", url = "mongodb://127.0.0.1:27017/geolocation")
#  banjir$insert(db_data_banjir)
#  loadData <- function() {
    # Connect to the database
#    db <- mongo( url = "mongodb+srv://wildanard123:Dhany123!@cluster0.caq2e.mongodb.net/geolocation?retryWrites=true&w=majority")
    # Read all the entries
#    db_data_banjir <- db$find()
#    db_data_banjir
#  }
#  loadData()
  
  
 # banjir = mongo("databanjir", url = "mongodb://127.0.0.1:27017/geolocation")
 # banjir$insert(db_data_banjir)
 # con <- mongo( url = "mongodb+srv://wildanard123:Dhany123!@cluster0.caq2e.mongodb.net/geolocation?retryWrites=true&w=majority")
 # con$insert(db_data_banjir)
   
  
  
    # Connect to the database
 #   db <- mongo(collection = "uploads.files", url = "mongodb+srv://wildanard123:Dhany123!@cluster0.caq2e.mongodb.net/db_twisar?retryWrites=true&w=majority")
#    data_output <- db$find(limit = 1, skip = 0, fields = '{ "_id" : false, "filename" : true }')
#    glimpse(data_output)
  
  
   
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// OVERVIEW
  
  output$jmltweetbanjir <- renderText({
    texturl1 <- length(unique(banjir_tweets$place_full_name))
    paste(" Flood (",texturl1," )")
  })
  output$jmltweetangin <- renderText({
    texturl2 <- length(unique(angin_tweets$place_full_name))
    paste(" Strong wind (",texturl2," )")
  })
  output$jmltweetgelombang <- renderText({
    texturl3 <- length(unique(gelombang_tweets$place_full_name))
    paste(" High wave (",texturl3," )")
  })
  output$jmltweetlongsor <- renderText({
    texturl4 <- length(unique(longsor_tweets$place_full_name))
    paste(" Landslide (",texturl4," )")
  })
  output$jmltweetgempa <- renderText({
    texturl5 <- length(unique(gempa_tweets$place_full_name))
    paste(" Earthquake (",texturl5," )")
  })
  
  output$jmltweettotal <- renderText({
    texturl1 <- length(unique(banjir_tweets$place_full_name))
    texturl2 <- length(unique(angin_tweets$place_full_name))
    texturl3 <- length(unique(gelombang_tweets$place_full_name))
    texturl4 <- length(unique(longsor_tweets$place_full_name))
    texturl5 <- length(unique(gempa_tweets$place_full_name))
    texturl6 <- texturl1+texturl2+texturl3+texturl4+texturl5
    paste(texturl6," Tweet")
  })
  
  output$jmllokasitotal <- renderText({
  placeurl1 <- length(unique(banjir_tweets$place_full_name))
  placeurl2 <- length(unique(angin_tweets$place_full_name))
  placeurl3 <- length(unique(gelombang_tweets$place_full_name))
  placeurl4 <- length(unique(longsor_tweets$place_full_name))
  placeurl5 <- length(unique(gempa_tweets$place_full_name))
  placeurl6 <- placeurl1+placeurl2+placeurl3+placeurl4+placeurl5
  paste(placeurl6)
  })
  
  #Render Leaflet///////////////////////////////////////////////////////////////////////////////////////////////////// RENDER LEAFLET
  output$mymap <- renderLeaflet({
  l <- leaflet(data = "banjir_tweets.geo.sf, angin_tweets.geo.sf, gelombang_tweets.geo.sf, longsor_tweets.geo.sf, gempa_tweets.geo.sf") %>% setView(113.921327,-0.789275,4) 
  #memakai peta Esri
  esri <- grep("^OpenStreetMap", providers, value = TRUE)
  for (provider in esri) {
    #    l <- l %>% addProviderTiles(provider, group = provider)
  } 
  l %>% 
#   addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Simple map") %>%
#    addProviderTiles(providers$Esri.WorldStreetMap, group = "World Street Map") %>%
#    addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
#    addProviderTiles(providers$Esri.WorldTopoMap, group = "World Topography") %>%
   
    # addLayersControl(baseGroups = names(esri),
  #  addProviderTiles("OpenStreetMap.Mapnik") %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "World Street Map") %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
    addProviderTiles(providers$GeoportailFrance.orthos, group = "World Imagery") %>%
    addProviderTiles(providers$OpenTopoMap, group = "World Topography") %>%
    addProviderTiles(providers$OpenRailwayMap, group = "Railway") %>%
    addProviderTiles(providers$Stamen.TonerHybrid, group = "Region") %>%
    
    #menambahkan zoom out globe dan fokus lokasi pengguna
    addEasyButton(easyButton(
      icon="fa-globe", title="Zoom to Level 1",
      onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
    addEasyButton(easyButton(
      icon="fa-crosshairs", title="Locate Me",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
    
    #menambahkan marker
    addMarkers(data = banjir_tweets.geo.sf, 
               popup = ~text,
               label = ~screen_name,
               icon = logobanjir,
               group = "Flood"
    ) %>%
    addMarkers(data = angin_tweets.geo.sf, 
               popup = ~text,
               label = ~screen_name,
               icon = logoangin,
               group = "Strong wind"
    ) %>%
    addMarkers(data = gelombang_tweets.geo.sf, 
               popup = ~text,
               label = ~screen_name,
               icon = logogelombang,
               group = "High wave"
    ) %>%
    addMarkers(data = longsor_tweets.geo.sf, 
               popup = ~text,
               label = ~screen_name,
               icon = logolongsor,
               group = "Landslide"
    ) %>%
    addMarkers(data = gempa_tweets.geo.sf, 
               popup = ~text,
               label = ~screen_name,
               icon = logogempa,
               group = "Earthquake"
    ) %>%
    
    #menambahkan legenda
    
    #    addControl(html = html_legend, position = "topleft") %>%
    
    
    
    #menambahkan layer peta esri dan minimap
    addLayersControl(
      overlayGroups =  c("Flood", "Strong wind", "High wave", "Landslide", "Earthquake", "Railway", "Region"),
      baseGroups = c("World Street Map", "Dark", "World Imagery", "World Topography" ),
      #names(esri),
      position = "topright"
    )%>%
    
    addMiniMap(tiles = esri[[1]], toggleDisplay = TRUE,
               position = "bottomleft") %>%
    htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }") 

  }) 

}    