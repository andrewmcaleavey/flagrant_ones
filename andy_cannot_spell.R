#' Recoding typos in player names
#'
#' @param x a data set to be cleaned
#'
#' @return a clean data set
#' @export
#'
#' @examples
andy_cannot_spell <- function(x){
  x <- replace(x, grep("Nikola Vucevic", x), "Nikola Vučević")
  x <- replace(x, grep("Marvin Bagley III", x), "Marvin Bagley")
  x <- replace(x, grep("Caris Levert", x), "Caris LeVert")
  x <- replace(x, grep("Kristaps Porziņģis", x), "Kristaps Porziņģis")  # not in the data
  x <- replace(x, grep("Mfiondu Kabengele", x), "Mfiondu Kabengele")  # rookie
  x <- replace(x, grep("Tim Hardaway Jr.", x), "Tim Hardaway")
  x <- replace(x, grep("Ja Morant", x), "Ja Morant")  # rookie, might be listed as Temetrius Morant eventually
  x <- replace(x, grep("Mo Bamba", x), "Mohamed Bamba")
  x <- replace(x, grep("Zach Lavine", x), "Zach LaVine")
  x <- replace(x, grep("DeAndre Ayton", x), "Deandre Ayton")
  x <- replace(x, grep("De'Andre Hunter", x), "De'Andre Hunter")  # rookie
  x <- replace(x, grep("Otto Porter Jr.", x), "Otto Porter")
  x <- replace(x, grep("Davis Bertans", x), "Dairis Bertāns")
  x <- replace(x, grep("Jaren Jackson Jr.", x), "Jaren Jackson")
  x <- replace(x, grep("Steph Curry", x), "Stephen Curry")
  x <- replace(x, grep("Javale McGee", x), "JaVale McGee")
  x <- replace(x, grep("Tyler Herro", x), "Tyler Herro")  # rookie
  x <- replace(x, grep("Fred Van Vleet", x), "Fred VanVleet")
  x <- replace(x, grep("Zion Williamson", x), "Zion Williamson")  # rookie
  x <- replace(x, grep("TJ Warren", x), "T.J. Warren")
  x <- replace(x, grep("Kentavious Caldwell Pope", x), "Kentavious Caldwell-Pope")
  x <- replace(x, grep("Lebron James", x), "LeBron James")
  x
}
# rookies may require attention once real data  from 2019-2020 starts

# here are the names that caused problems:
# "Nikola Vucevic"           "Marvin Bagley III"        "Caris Levert"             NA                        
# [5] NA                         "Kristaps Porziņģis"       "Mfiondu Kabengele"        NA                        
# [9] NA                         "Tim Hardaway Jr."         NA                         NA                        
# [13] "Ja Morant"                "Mo Bamba"                 NA                         NA                        
# [17] "Zach Lavine"              NA                         NA                         "DeAndre Ayton"           
# [21] "De'Andre Hunter"          "Otto Porter Jr."          NA                         NA                        
# [25] "Davis Bertans"            NA                         NA                         "Jaren Jackson Jr."       
# [29] NA                         NA                         "Steph Curry"              NA                        
# [33] NA                         "Javale McGee"             "Tyler Herro"              NA                        
# [37] NA                         "Fred Van Vleet"           "Zion Williamson"          "TJ Warren"               
# [41] "Kentavious Caldwell Pope" "Lebron James"    