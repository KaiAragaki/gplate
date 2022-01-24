get_margin <- function(m) {

  if (length(m) == 1) {
    return(list(top = m[1], right = m[1], bottom = m[1], left = m[1]))
  }

  if (length(m) == 2) {
    return(list(top = m[1], right = m[2], bottom = m[1], left = m[2]))
  }

  if (length(m) == 3) {
    return(list(top = m[1], right = m[2], bottom = m[3], left = m[2]))
  }

  if (length(m) == 4) {
    return(list(top = m[1], right = m[2], bottom = m[3], left = m[4]))
  }

}
