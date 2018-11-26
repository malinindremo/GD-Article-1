SaveA4 <- function (q, filename, landscape = T, scale=1, scaleh=scale, scalev=scale){
  if (landscape) {
    ggsave(filename, plot = q, width = 297*scaleh, height = 210*scalev, 
           units = "mm")
  }
  else {
    ggsave(filename, plot = q, width = 210*scaleh, height = 297*scalev, 
           units = "mm")
  }
}
