'''test <- "sdlkj"


pchars <- pchar("s")
pchard <- pchar("d")
pcharl <- pchar("l")
pcharx <- pchar("x")

pchars %then% pchard %>% run(test)

pchars %or_else% pchard %>% run(test)

dorl <- pchard %or_else% pcharl
xorl <- pcharx %or_else% pcharl

pchars %then% xorl %>% run(test)

#map_p <- function(.p, .f){
#  purrr::map(.p, .f = map_p_, f = .f)
#}

any_sdk <- any_of(c("d", "k","s"))
any_xyz <- any_of(c("x", "y", "z"))

any_sdk %>% run(test)
any_xyz %>% run(test)

parse_digit

parse_digit("9")

parse_digit %then%
  parse_digit %then%
  parse_digit %then%
  parse_digit %>% purrr::invoke(.x = "1235dflkj")


parse_three_digits <- function(){

  parser <- parse_digit %then% parse_digit %then% parse_digit

  func <- function(vec){paste0(vec, collapse = "")}

  map_p(.f = func, .p = parser)

}


parse_three_digits <- function(){

  parser <- parse_digit %then% parse_digit %then% parse_digit

  func <- function(vec){paste0(vec, collapse = "")}

  map_p(.f = func, .p = parser)

}


parse_three_digits <- function(){

  parse_digit %then% parse_digit %then% parse_digit %map_p%
    function(vec){paste0(vec, collapse = "")}

}

temp <- parse_three_digits()("1234asdf")

temp'''
