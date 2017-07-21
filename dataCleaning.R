

install.packages('xlsx')
file <- system.file("tests", "macdonald.xlsx", package = "xlsx")
res <- read.xlsx(file, 1)  # read first sheet
head(res)

df