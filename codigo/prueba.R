df1 <-
  data.frame(c("male", "female", "male"),
             c("1", "2", "3", "4", "5", "6"),
             seq(141, 170))

names(df1) = c("gender", "age", "height")

df1$age <- factor(
  df1$age,
  levels = c(1, 2, 3, 4, 5, 6),
  labels = c("16-24", "25-34", "35-44", "45-54", "55-64", "65+")
)

q1a = c(1, 0, 1, 0, 0, 1)
q1b = c(0, 0, 2, 2, 2, 0)
q1c = c(0, 0, 3, 3, 0, 3)
# 1,2 and 3 used to be compatible with existing datasets. 
# Could change all to 1 if necessary.

df2 <- data.frame(q1a = q1a, q1b = q1b, q1c = q1c)
df1 <- cbind(df1, df2)

# Función para calcular las frecuencias multirepuestas
multfreqtable = function(data, question.prefix) {
  # Find the columns with the questions
  a = grep(question.prefix, names(data))
  # Find the total number of responses
  b = sum(data[, a] != 0)
  # Find the totals for each question
  d = colSums(data[, a] != 0)
  # Find the number of respondents
  e = sum(rowSums(data[,a]) !=0)
  # d + b as a vector. This is your overfall frequency 
  f = as.numeric(c(d, b))
  data.frame(question = c(names(d), "Total"),
             freq = f,
             percent = (f/b)*100,
             percentofcases = (f/e)*100 )
}

a <- grep("q1", names(df1))
df1[,a]
sum(df1[, a] != 0)
d
library(ufs)
multiResponse (df1, c('q1a', 'q1b', 'q1c'))