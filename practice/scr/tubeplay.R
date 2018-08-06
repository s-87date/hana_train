# install.packages("miniUI")
library(miniUI)
tubeplay <- function(url=NA, 
                     width = "560", height = "315", background = "white",
                     viewer = getOption("viewer", utils::browseURL)) {
  if(is.na(url)==TRUE){
    print("ENTER URL")
  }
  # make video_id
  video_id <- gsub("^.*\\?v=", "", url)
  # make_ui
  ui <- miniUI::miniPage(
    htmltools::tags$iframe(width = width, height = height,
                           src = paste("https://www.youtube.com/embed", video_id, sep = "/"),
                           frameborder="0")
  )
  htmltools::html_print(ui)
}
tubeplay("https://www.youtube.com/watch?v=rY7Cw4YAF3c")
