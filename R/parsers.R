first <- function(string){
  substr(string, 1,1)
}

rest <- function(string){
  substr(string, 2, nchar(string))
}

is.success <- function(result){
  !is.na(result[["match"]][1])
}

#' parse a single character
#'
#' @param char
#'
#' @return
#' @export
#'
#' @examples
pchar <- function(char){
  function(input){
    if(char == first(input)){
      list(match = char, rest = rest(input), error = NA)
    } else {
      error <- sprintf("expecting %s, got %s", char, first(input))
      list(match = NA, rest = input, error = error)
    }
  }
}

#' then
#' the 'and then' combinator
#' '
#' @param parserL
#' @param parserR
#'
#' @return
#' @export
#'
#' @examples
#'
#' pchar('p') %then% pchar('a')('parse')
then <- function(parserL, parserR){
  function(input){
    if(nchar(input) == 0){
      #should this be null?
      return(list())
    }

    resultL <- parserL(input)

    if(!is.success(resultL)){
      stop(resultL$error)
    }

    resultR <- parserR(resultL$rest)

    if(!is.success(resultR)){
      stop(resultR$error)
      } else {
        #should this be a nested list instead of a vector of successes?
        list(
          match = c(resultL$match, resultR$match),
          rest = resultR$rest,
          error = NA)
      }
    }
  }
#' @export
`%then%`<- then

#' the or combinator
#'
#' @param parserL
#' @param parserR
#'
#' @return
#' @export
#'
#' @examples
or_else <- function(parserL, parserR){
  function(input){

    if(is.success(input)){
      input <- input$rest[1]
    }

    if(nchar(input) == 0){
      return(list())
    }
    resultL <- parserL(input)
    if(is.success(resultL)){
      resultL
    } else {
      parserR(input)
    }
  }
}

#' export
`%or_else%` <- or_else

choice <- function(.p){
  purrr::reduce(.p, .f = or_else)
}

any_of <- function(chars){
  chars %>%
    purrr::map(pchar) %>%
    choice()
  }

map_p <- function(.f, .p){
  function(input){
    result <- run(.p, input)
    if(is.success(result)){
      result$match <- .f(result$match)
      result
    } else {
      list()
    }
  }
}

map_p_pipe <- function(.p, .f){
  function(input){
    result <- run(.p, input)
    if(is.success(result)){
      result$match <- .f(result$match)
      result
    } else {
      list()
    }
  }
  }

`%map_p%` <- map_p_pipe

return_p <- function(x){
  function(input){
    return(success(list(
      match = x,
      rest = input
    )))
  }
}

parse_digit <- any_of(as.character(0:9))

parse_string <- function(string){
  stringr::str_split(string, "") %>%
    purrr::map(pchar) %>%
    purrr::reduce( .f =  then ) %map_p%
    paste0()
}

