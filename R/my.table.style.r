

  #' @title Style table for flextable package
  #' @description Create pretty tables for MS Word documents
  #' @param x flextable object
  #' @returns format for table
  #' @examples
  #' my.table.style (x)
  #' @export
my.table.style = function(x){
  std_b = fp_border(color="black")
  cuali.t <- regulartable(x)
  cuali.t <- set_formatter_type(cuali.t, fmt_double = "%.01f")
  cuali.t <- fontsize(cuali.t, size=10)
  cuali.t <- font(cuali.t, fontname = "Calibri")
  cuali.t <- align(cuali.t, align="left", j=1)
  cuali.t <- align(cuali.t, align="left", j=2)
  cuali.t <- border_remove(cuali.t)
  cuali.t <- hline(cuali.t, border = std_b, part="header")
  cuali.t <- hline_top(cuali.t, border = std_b, part="all")
  cuali.t <- hline_bottom(cuali.t, border = std_b, part="all")
  cuali.t <- width(cuali.t, width = 1.4)
}
