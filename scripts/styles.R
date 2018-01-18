## WARC style
warcStyle <- function() {
styles <- list(
  col = list(
    main = list(
      col1 = "#313678",
      col2 = "#6F80EA",
      col3 = "#3D46C4"
    ),
    support = list(
      col4 = "#B7295E",
      col5 = "#2C929D",
      col6 = "#8A3EFF"
    ),
    focus = list(
      "#F2385A",
      "#37CF85"
    )
))

assign("styles", styles, pos = parent.frame())
}