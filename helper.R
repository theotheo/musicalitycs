library(RCurl)
library(RJSONIO)

# get group members ids 
get_members <- function(id) {
	url <- paste0("https://api.vk.com/method/groups.getMembers?group_id=", id, "&v=5.24")
	ids <- fromJSON(getURL(url, ssl.verifypeer = FALSE))
	return(ids$response)
}

# get group members ids 
get_friends <- function(id, token) {
	url <- paste0("https://api.vk.com/method/friends.get?user_id=", id, "&v=5.25", "&access_token=", token)
	response <- fromJSON(getURL(url, ssl.verifypeer = FALSE))
	return(response$response$items)
}

fetch_artists_for_friends <- function(id, token) {
	ids <- get_friends(id, token)
	artists <- lapply(ids, function(id) list(id=id, artists=fetch_artists(id, token)))
}

# https://vk.com/pages?oid=-1&p=%D0%90%D0%B2%D1%82%D0%BE%D1%80%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D1%8F_%D0%BA%D0%BB%D0%B8%D0%B5%D0%BD%D1%82%D1%81%D0%BA%D0%B8%D1%85_%D0%BF%D1%80%D0%B8%D0%BB%D0%BE%D0%B6%D0%B5%D0%BD%D0%B8%D0%B9
#  https://oauth.vk.com/authorize?client_id=4596301&scope=audio,friends&redirect_uri=http://oauth.vk.com/blank.html&display=page&response_type=token 
# token <- 'd428ee47cb763a5b3f7f6d5521698b9e456a79a7add1f8be52d8ce3c488f60b56372e7e6615bb6f68289a'

fetch_artists <- function(id, token) {
	print('fetch_artists')
	# url <- "https://api.vk.com/method/audio.get?owner_id=36637947&v=5.24&access_token=562abf625dec921262a49e71570cb6aef6e0fb2cdeca230e4e4206d8477a3b6895485b71c325af5774edf"
	url <- paste0("https://api.vk.com/method/audio.get?owner_id=", id, "&v=5.25", "&access_token=", token)
	audios <- fromJSON(getURL(url, ssl.verifypeer = FALSE))
	# print(audios)
	artists <- unlist(lapply(audios$response$items, function(x) x$artist))

	# artists1 <- c()
	# len = length(audios$response$items)
	# for(i in 1:len) {
	# 	artists1 <- append(artists, audios$response$items[[i]]$artist)	
	# }

	return(artists)
}

stats_artists <- function(artists) {
	# number_all = length(artists)
	unique = unique(artists) 
	number_unique = length(unique)
	number_russian = count_russian(unique)
	p_factor = number_russian / number_unique
	return(list(number_unique = number_unique, number_russian = number_russian, p_factor = p_factor))
}

get_token <- function(code) {
	# app_id <- '4596301'
	app_id <- '4598768'
	# app_secret <- '7BIDCjwBof30qFHDXdeR'
	app_secret <- '4ChKwssDpzY3qecmrZ9W'
	# redirect_uri <- 'https://theotheo.shinyapps.io/musifucker/'
	redirect_uri <- 'http://127.0.0.1:6054/' 
	url = paste0('https://oauth.vk.com/access_token?client_id=', app_id, 
		'&client_secret=', app_secret, 
		'&code=', code, 
		'&redirect_uri=', redirect_uri)
	print(getURL)
	response <- fromJSON(getURL(url, ssl.verifypeer = FALSE))
	print(response)
	return(list(token = response$access_token, user_id = response$user_id))
}

simple_check_russian <- function (name) {
	first_symbol = substring(name, 1, 1)
	code <- strtoi(charToRaw(first_symbol),16L)
	# 'russianA ' -- 192, 'russian YA' -- 255
	if (code >= 192 && code <= 255) {
		return(TRUE)
	}
	return(FALSE)
} 

count_russian <- function(names) {
	count <- 0
	len = length(names)
	if(len != 0) {
		for(i in 1:length(names)) {
	 		if(simple_check_russian(names[i])) 
	 			count = count + 1
		}
	}
	return(count)
}