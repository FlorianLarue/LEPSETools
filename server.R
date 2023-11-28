#######################################################################
##                   LEPSE tools shiny application                   ##
##  This app contains several tools to help with work done at LEPSE  ##
##               around phenotyping and crop modelling               ##
##                       Author: Florian Larue                       ##
##                      Last updated: 2023-03-27                     ##
#######################################################################

##---------------------------------------------------------------
##    Define the Server behavior of the LEPSEtool Shiny App     -
##---------------------------------------------------------------

shinyServer(function(input, output, session) {
  source("./apps/leafprofile_server.R", local=TRUE)
  source("./apps/phenodynmonitor_server.R", local=TRUE)
  source("./apps/tillering_server.R", local=TRUE)
  
  get_last_bookmark <- function() {
    list.files(path = 'shiny_bookmarks/') %>%
      map_df(., ~ paste0('shiny_bookmarks/', .x) %>%
               file.info) %>%
      slice_max(atime) %>%
      rownames()
  }
  get_last_bookmark <- possibly(get_last_bookmark, otherwise = '')

  onBookmarked(function(url) {
    if(!dir_exists("shiny_bookmarks/latest")){
      dir_create("shiny_bookmarks/latest")
    }
    file_move(path = path("shiny_bookmarks", 
                          parseQueryString(sub("^.*\\?", "", url))$`_state_id_`, 
                          "input.rds"),
              new_path = path("shiny_bookmarks/latest/input.rds"))
    file_move(path = path("shiny_bookmarks", 
                          parseQueryString(sub("^.*\\?", "", url))$`_state_id_`, 
                          "values.rds"),
              new_path = path("shiny_bookmarks/latest/values.rds"))
    dir_delete(path("shiny_bookmarks", 
                    parseQueryString(sub("^.*\\?", "", url))$`_state_id_`))
  })
  
  observeEvent(req(get_last_bookmark() != "" && is.null(parseQueryString(
    session$clientData$url_search)$`_state_id_`)), {
    shinyalert("Loading", text="Please wait while session is being restored",
               type = "warning")
    bookmarkName <- basename(get_last_bookmark())
    updateQueryString(queryString = paste0("?_state_id_=", bookmarkName))
    session$reload()
  }, once = TRUE)
  
  onBookmark(function(state) {
    state$values$currentMessages <- currentMessages$mes
  })
  
  onRestore(function(state) {
    for(i in 0:length(state$values$currentMessages)) {
      if(i != 0) {
        if(state$values$currentMessages[[i]][4] == "Info") {
          tmpType <- "received"
        } else {
          tmpType <- "sent"
        }

        shinydashboardPlus::updateUserMessages(
          id = "pmmessages",
          action = "add",
          content = list(
            author = state$values$currentMessages[[i]][1],
            date = state$values$currentMessages[[i]][2],
            image = "message.png",
            type = tmpType,
            text = state$values$currentMessages[[i]][3]
          )
        )
      }
    }
  })
})