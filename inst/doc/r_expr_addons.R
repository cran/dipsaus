## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library(shiny)
library(dipsaus)
data(ToothGrowth)

## ----eval=FALSE---------------------------------------------------------------
# if( exists('aa') && !is.null(aa) ){
#   aa <- 1
# }

## ----eval=TRUE----------------------------------------------------------------
aa %?<-% 1
print(aa)

## -----------------------------------------------------------------------------
l %?<-% list()

l$aa %?<-% 1

print(l)

## -----------------------------------------------------------------------------
# e already exists
e <- list(aa = 1)

# %?<-% will not evaluate rhs, nor assign values
system.time({
  e %?<-% { Sys.sleep(10); list(aa = 2) }
  print(e)
})


## -----------------------------------------------------------------------------
li <- c('A', 'T', 'G', 'C')
li |> iapply(alist(el, ii) %=>% {
  sprintf('The index for %s is %s', el, ii)
})


## -----------------------------------------------------------------------------
alist(a, b=a^2, ...) %=>% {
  print(c(a , b,...))
}

## -----------------------------------------------------------------------------
match.call(textInput, call = quote(textInput('inputId', 'label', 'aaa')))

## -----------------------------------------------------------------------------
match.call(tagList, call = quote(tagList(
  div(
    tags$ul(
      tags$li(textInput('inputId', 'label', 'aaa'))
    )
  )
)))

## -----------------------------------------------------------------------------
match_calls(call = tagList(
  div(
    tags$ul(
      tags$li(textInput('inputId', 'label', 'aaa'))
    )
  )
), recursive = TRUE)

## -----------------------------------------------------------------------------
match_calls(call = tagList(
  div(
    tags$ul(
      tags$li(textInput('inputId', 'label', 'aaa'))
    )
  )
), recursive = TRUE, replace_args = list(
  'inputId' = function(v, ...){
    as.call(list(quote(ns), v))
  }
))


## ----eval=FALSE---------------------------------------------------------------
# x_tmp <- x |>
#   do_something(...)
# 
# plot(x_tmp)
# 
# final_results <- x_tmp |>
#   do_others(...)

## ----eval=FALSE---------------------------------------------------------------
# final_results <- x |>
#   do_something(...) |>
#   no_op(plot, ylim = c(0,100)) |>
#   do_others(...)

## ----fig.width=7,fig.height=5-------------------------------------------------

par(mfrow = c(1,2))

result <- (1:10) |> 
  iapply(alist(el, ii) %=>% {
    rnorm(20, el, ii)
  }, simplify = FALSE) |> 
  unlist() |> 
  
  # Begin no-ops, result will not change
  no_op({
    # Use expression and "." to refer the data
    print(summary(.))
  }) |> 
  no_op(
    # Use function and pass ... to function
    plot, x = seq(0,1,length.out = 200), 
    type = 'p', ylim = c(-20,20), pch = 16,
    xlab = 'Time', ylab = 'Value', las = 1
  ) |> 
  no_op(hist, xlab = 'Values', main = 'Histogram') 

str(result)

## -----------------------------------------------------------------------------
ToothGrowth |> 
  do_aggregate(len ~ ., mean)

