library(shiny)
library(ggplot2)

source("helper.R")

toA <- function(e) { return(paste0("<a href=https://vk.com/audios", result$user_id, "?performer=1&q=", curlEscape(e), ">",e,"</a>"))}


shinyServer(function(input, output, session) {

renderArtists <- function(artists) {
  stats <- stats_artists(artists)
  output$number_unique <- renderText({paste("There are", stats$number_unique, "artists at your audios")})
  output$number_russian <- renderText({paste("There are", stats$number_russian, "russian artists")})
  output$p_factor <- renderText({paste("Your p_factor:", stats$p_factor)})

  output$names <- renderTable({
            # a <- sapply(unique(artists), toA)
    dat <- as.data.frame(table(artists))
    # a <- paste0("<a target='_blank' href=https://vk.com/audios", result$user_id, "?performer=1&q=", curlEscape(artists), ">", artists,"</a>")
    # dat <- data.frame(artists = a)
    dat[with(dat, order(-Freq, artists)), ][1:20, ]
  },  sanitize.text.function = function(x) x)
}

renderArtistsFriends <- function(id, token) {
  output$friends <- renderTable({

    count <- 1
    getUniqueArtist <- function(id) {
      # Increment the progress bar, and update the detail text.
      incProgress(1/n, detail = paste("Doing part", count))
      # Wait to not violate vk.com API restrictions
      count <- count + 1
      Sys.sleep(0.3)
      list(id=id, artists=unique(fetch_artists(id, token)))
    }
    
    ids <- get_friends(id, token)
    # ids <- ids[1:5]

    withProgress(message = 'Fetching friends audio', value = 0, {
          # Number of times we'll go through the loop
          n <- length(ids)
          
          artists <- lapply(ids, getUniqueArtist)
    })
    dat <- as.data.frame(table(unlist(artists)))
    dat[with(dat, order(-Freq, Var1)), ][1:20, ]
  })
}

#  output$plot <- renderPlot({
#   progress <- Progress$new(session, min=1, max=15)
#   on.exit(progress$close())
#   progress$set(message = 'Calculation in progress',
#     detail = 'This may take a while...')
#   for (i in 1:15) {
#     progress$set(value = i)
#     Sys.sleep(0.5)
#   }
#   plot(cars)
# })


 output$text1 <- renderText({
  count <- get_members(input$user_id);
  paste("User with id", input$user_id, "have", count, "friends.")
})


 observe({
  print('change session$clientData')
  print(session$clientData$url_pathname)
  if(session$clientData$url_search != "") {
    print('go for token')
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query$code)) {
      print(query$code)
      result <- get_token(query$code)
      print(result)
      # getArtists(result, output)

      artists <- fetch_artists(result$user_id, result$token)

      renderArtists(artists)
      renderArtistsFriends(result$user_id, result$token)
    } 
  } else if (session$clientData$url_hash_initial != ""){
    print("token")
    query <- parseQueryString(session$clientData$url_hash_initial)
    print(query)

    if(!is.null(query$'#access_token')) {
      print('access_token')
      print(query$'#access_token')
      artists <- fetch_artists(query$user_id, query$'#access_token')
    # print(artists)

      renderArtists(artists)
    }
  } else {
    print("redirect")
    session$sendCustomMessage(type='redirect', 1)
  }
})

})