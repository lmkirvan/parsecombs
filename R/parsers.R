first <- function(string){
  substr(string, 1,1)
}

rest <- function(string){
  substr(string, 2, nchar(string))
}

is.success <- function(result){
  !is.na(result[["match"]][1])
}

collapse <- function(vec){
  paste0(vec, collapse = "")
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
      l <- list(match = char, rest = rest(input), error = NA)
      structure(l, "success" = TRUE)
    } else {
      error <- sprintf("expecting %s, got %s", char, first(input))
      l <- list(match = NA, rest = input, error = error)
      structure(l, "success" = FALSE)
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
      resultL
    }

    resultR <- parserR(resultL$rest)

    if(!is.success(resultR)){
      resultR
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

#' anyof combinator, takes a vector of characters and returns a parser that
#' mactches any of them
#'
#' @param chars
#'
#' @return
#' @export
#'
#' @examples
any_of <- function(chars){
  chars %>%
    purrr::map(pchar) %>%
    choice()
  }

map_p <- function(.f, .p){
  function(input){
    result <- .p(input)
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
    result <- .p(input)
    if(is.success(result)){
      result$match <- .f(result$match)
      result
    } else {
      input
    }
  }
  }


`%map_p%` <- map_p_pipe


return_p <- function(x){
  function(input){
    list(
      match = x,
      rest = input,
      error = NA
      )
    }
  }

many_inner <- function(.p, input){
  result <- .p(input)
  if(!is.success(result)){
    list(
      match = character(),
      rest = result$rest,
      error = NA
    )
  } else {
    next_result <- many_inner(.p, result$rest)
    list(
      match = c(result$match, next_result$match),
      rest = next_result$rest,
      error = NA
    )
  }
}

many <- function(.p){
  function(input){
    many_inner(.p, input)
  }
}

many1 <- function(.p){
  function(input){
    (.p %then% many(.p))(input)
  }
}

parse_digit <- any_of(as.character(0:9))

parse_string <- function(string){
  stringr::str_split(string, "")[[1]] %>%
    purrr::map(pchar) %>%
    purrr::reduce(.f = then) %map_p%
    collapse
}

parse_int <- function(input){
  many1(parse_digit) %map_p%
    collapse %map_p%
    as.integer %>%
    purrr::invoke(input)
}



