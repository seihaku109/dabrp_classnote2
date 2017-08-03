if(!require(rmarkdown)) install.packages("rmarkdown")
library(rmarkdown,lib.loc = "C:/Users/mrchypark/Documents/R/win-library/3.4")

render(input = "/Users/joongbaek/r_fastcampus/dabrp_classnote2/class7sample/pdftesthangul.Rmd",
       encoding="UTF-8")
