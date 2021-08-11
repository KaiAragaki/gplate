# Class definition of Plate object

setClassUnion("list_or_null", c("list", "NULL"))
setClassUnion("numeric_or_null", c("numeric", "NULL"))


setClass("Plate",
         slots = c(
           layers = "list_or_null",
           meta = "list_or_null",
           wells = "numeric_or_null"),
         prototype = c(
           layers = list(),
           meta = NULL,
           wells = NULL
         )
)

