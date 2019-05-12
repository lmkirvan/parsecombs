first <- function(string){
  substr(string, 1,1)
}

rest <- function(string){
  substr(string, 2, nchar(string))
}

success <- function(list){
  class(list) <- "p_success"
  list
}

is.success <- function(list){
  class(list) == "p_success"
}

failure <- function(list){
  class(list) <- "p_failure"
  list
}

is.failure <- function(list){
  class(list) == "p_failure"
}

parser <- function(.p){
  .p <- list(.p)
  class(.p) <- "parser"
  .p
}

pchar <- function(char){
  .p <- function(input){
    if(char == first(input)){
      l <- list(match = char, rest = rest(input), error = NA)
      success(l)
    } else {
      error <- sprintf("expecting %s, got %s", char, first(input))
      l <- list(match = NA, rest = input, error = error)
      failure(l)
    }
  }
  parser(.p)
}

run <- function(.p, input){
  purrr::invoke(.f = .p[[1]], .x = input)
}

then <- function(parserL, parserR){
  .p <- function(input){

    if(is.success(input)){
      input <- input$rest[1]
    }

    if(nchar(input) == 0){
      #should this be null?
      return(list())
    }

    resultL <- run(parserL, input)

    if(!is.success(resultL)){
      stop(resultL$error)
    }

    resultR <- run(parserR, resultL$rest)

    if(!is.success(resultR)){
      stop(resultR$error)
      } else {
        #should this be a nested list instead of a vector of successes?
        success(list(
          match = c(resultL$match, resultR$match),
          rest = resultR$rest))
      }
    }
  parser(.p)
  }

`%then%`<- then

or_else <- function(parserL, parserR){
  .p <- function(input){

    if(is.success(input)){
      input <- input$rest[1]
    }

    if(nchar(input) == 0){
      return(list())
    }
    resultL <- run(parserL, input)
    if(is.success(resultL)){
      resultL
    } else {
      run(parserR, input)
    }

  }
  parser(.p)
}

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
      stop(result$error)
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
      stop(result$error)
    }
  }
}

`%map_p%` <- map_p_pipe

return_p <- function(x){
  .p <- function(input){
    return(success(list(
      match = x,
      rest = input
    )))
  }
  parser(.p)
}

apply_p <- function(paserL, parserR){
  parserL %then% parserR %>%
    map_p(function(.f, .x){
      .f(.x)
    })
}

`%apply%` <- apply_p

parse_digit <- any_of(as.character(0:9))

parse_string <- function(string){
  stringr::str_split(string, "") %>%
    purrr::map(pchar) %>%
    purrr::reduce( .f =  then ) %map_p%
    glue::glue_colapse
}


many <- function(.p){
  function(input){
    if(is.success(.p(input)))
  }
}


