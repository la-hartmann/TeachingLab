devtools::load_all()
library(ltm)
library(TeachingLab)
library(tidyverse)

first_educator_survey_19_20 <- get_diagnostic_survey(year = "19_20", update = FALSE, write = FALSE)

educator_survey_19_20 <- first_educator_survey_19_20[!is.na(first_educator_survey_19_20$pre1),]

followup_educator <- first_educator_survey_19_20[!is.na(first_educator_survey_19_20$post1),]

reverse_1 <- c("pre1", "pre3", "pre4", "pre8", "pre9")
reverse_2 <- c("post1", "post3", "post4", "post8", "post9")

pre <- educator_survey_19_20 |>
  select(5:13)
post <- followup_educator |>
  select(131:139)
### More complicated way of binding rows ###
full <- data.frame(mapply(c, pre,post))

clean <- c("Very true" = 6, "TRUE" = 5, "Somewhat true" = 4,
           "Untrue" = 3, "Somewhat untrue" = 2, "Very untrue" = 1)
cpre <- apply(pre, 2, function(col) clean[col])
cpost <- apply(post, 2, function(col) clean[col])
cfull <- apply(full, 2, function(col) clean[col])

cpre[, reverse_1] <- 7 - cpre[, reverse_1] %% 7
cpost[, reverse_2] <- 7 - cpost[, reverse_2] %% 7
cfull[, reverse_1] <- 7 - cfull[, reverse_1] %% 7

summary_stats <- function(df) {
  
  ca <- cronbach.alpha(df)$alpha
  suma <- summary(rowSums(df))/24
  sd <- sd(rowSums(df))/24
  print("Croan")
  print(ca)
  print(suma)
  print("SD")
  print(sd)
}
fill <- function(data){
  for(i in 1:ncol(data)){
    data[is.na(data[,i]), i] <- median(data[,i], na.rm = TRUE)
  }
  return(data)
}
cpre_r <- data.frame(cpre[,1:2])
cpost_r <- data.frame(cpost[,1:2])
cfull_r <- data.frame(mapply(c, cpre_r, cpost_r))

summary_stats(fill(cpre))
summary_stats(fill(cpost))
summary_stats(fill(cfull))


summary_stats(fill(cpre_r))
summary_stats(fill(cpost_r))
summary_stats(fill(cfull_r))

cpre_h <- data.frame(cpre[,5:9])
cpost_h <- data.frame(cpost[,5:9])
cfull_h <- data.frame(mapply(c, cpre_h, cpost_h))

summary_stats(fill(cpre_h))
summary_stats(fill(cpost_h))
summary_stats(fill(cfull_h))

cpre_g <- data.frame(cpre[,3:4])
cpost_g <- data.frame(cpost[,3:4])
cfull_g <- data.frame(mapply(c, cpre_g, cpost_g))

summary_stats(fill(cpre_g))
summary_stats(fill(cpost_g))
summary_stats(fill(cfull_g))


diagnonstic_19_20_3 <- diagnonstic_19_20[!is.na(diagnonstic_19_20$pre35),]
diagnonstic_19_20_4 <- diagnonstic_19_20[!is.na(diagnonstic_19_20$post35),]

pree <- diagnonstic_19_20_3[, 68:71]
poste <- diagnonstic_19_20_4[, 200:203]
fulle <- data.frame(mapply(c, pree,poste))

clean <- c("Strongly agree" = 6, "Agree" = 5, "Somewhat agree" = 4,
           "Somewhat disagree" = 3, "Disagree" = 2, "Strongly disagree" = 1)
cpree <- apply(pree, 2, function(col) clean[col])
cposte <- apply(poste, 2, function(col) clean[col])
cfulle <- apply(fulle, 2, function(col) clean[col])

summary_stats(fill(cpree))
summary_stats(fill(cposte))
summary_stats(fill(cfulle))


diagnonstic_19_20_5 <- diagnonstic_19_20[!is.na(diagnonstic_19_20$pre47),]
diagnonstic_19_20_6 <- diagnonstic_19_20[!is.na(diagnonstic_19_20$post47),]

preo <- diagnonstic_19_20_5[, 110:115]
posto <- diagnonstic_19_20_6[, 223:228]
fullo <- data.frame(mapply(c, preo,posto))

clean <- c("Almost always" = 4, "Often" = 3, "Sometimes" = 2,
           "Almost never" = 1)
cpreo <- apply(preo, 2, function(col) clean[col])
cposto <- apply(posto, 2, function(col) clean[col])
cfullo <- apply(fullo, 2, function(col) clean[col])

summary_stats(fill(cpreo))
summary_stats(fill(cposto))
summary_stats(fill(cfullo))



