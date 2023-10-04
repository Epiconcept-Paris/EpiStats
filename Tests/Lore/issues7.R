


# Test --------------------------------------------------------------------

df <-  data.frame(
  outcome = c(rep(0, 10), rep(1, 8), rep(NA, 3)),
  fooditem1 = c(rep(0, 10), rep(1, 11)),
  fooditem2 = c(rep(0, 10), rep(1, 5), rep(NA, 6)),
  fooditem3 = c(rep(0, 21)),
  fooditem4 = c(rep(1, 21))
)

cc(df, outcome, fooditem1)


# Data preparation --------------------------------------------------------

df <-  data.frame(
  outcome = sample(c(0, 1, NA), 1000, replace = TRUE, prob = c(0.40, 0.59, 0.01)),
  fooditem1 = sample(c(0, 1), 1000, replace = TRUE, prob = c(0.5, 0.5)),
  fooditem2 = sample(c(0, 1, NA), 1000, replace = TRUE, prob = c(0.20, 0.79, 0.01)),
  fooditem3 = c(rep(0, 1000)),
  fooditem4 = c(rep(1, 1000)),
  fooditem5 = sample(c(0, 1, NA), 1000, replace = TRUE, prob = c(0.20, 0.79, 0.01))
)

table(df$fooditem1, df$outcome, useNA = "always")
table(df$fooditem2, df$outcome, useNA = "always")
table(df$fooditem3, df$outcome, useNA = "always")
table(df$fooditem4, df$outcome, useNA = "always")
table(df$fooditem5, df$outcome, useNA = "always")

df$fooditem5 <- ifelse(df$outcome == 0 & df$fooditem5 == 1, 0, df$fooditem5)
table(df$fooditem5, df$outcome, useNA = "always")

write.csv(df, "Tests/Lore/Test.csv", row.names = FALSE, na = "")
df <- read.csv("Tests/Lore/Test.csv")

df$fooditem6 <- sample(c(FALSE, TRUE, NA), 1000, replace = TRUE, prob = c(0.20, 0.79, 0.01))
df$fooditem7 <- sample(c("0","1", NA), 1000, replace = TRUE, prob = c(0.20, 0.79, 0.01))


# CC issue ----------------------------------------------------------------

cc(df, outcome, fooditem1)
cc(df, outcome, fooditem2)
cc(df, outcome, fooditem3)
cc(df, outcome, fooditem4)
cc(df, outcome, fooditem5)
cc(df, outcome, fooditem6)
cc(df, outcome, fooditem7)

cc(df, outcome, "fooditem1")
cc(df, outcome, "fooditem2")
cc(df, outcome, "fooditem3")
cc(df, outcome, "fooditem4")

cc(df, "outcome", "fooditem1")
cc(df, "outcome", "fooditem2")
cc(df, "outcome", "fooditem3")
cc(df, "outcome", "fooditem4")

for(x in c("fooditem1", "fooditem2")) {
  print(cc(df, outcome, x))
}

cctable(df, "outcome", c("fooditem1", "fooditem2", "fooditem3", "fooditem4", "fooditem5"))
cctable(df, "outcome", c("fooditem1", "fooditem2", "fooditem3", "fooditem4", "fooditem5", "fooditem6", "fooditem7"))



# CS ----------------------------------------------------------------------

cs(df, outcome, fooditem1)
cs(df, outcome, fooditem2)
cs(df, outcome, fooditem3)
cs(df, outcome, fooditem4)
cs(df, outcome, fooditem5)

cs(df, outcome, "fooditem1")
cs(df, outcome, "fooditem2")
cs(df, outcome, "fooditem3")
cs(df, outcome, "fooditem4")

cs(df, "outcome", "fooditem1")
cs(df, "outcome", "fooditem2")
cs(df, "outcome", "fooditem3")
cs(df, "outcome", "fooditem4")

for(x in c("fooditem1", "fooditem2")) {
  print(cs(df, outcome, x))
}

cstable(df, "outcome", c("fooditem1", "fooditem2", "fooditem3", "fooditem4", "fooditem5"))
cstable(df, "outcome", c("fooditem1", "fooditem2", "fooditem3", "fooditem4", "fooditem5", "fooditem6", "fooditem7"))

# Same issue with epitable ------------------------------------------------

library(epiuf)
epitable(df, outcome, fooditem1)
epitable(df, outcome, fooditem2)
epitable(df, outcome, "fooditem1")
epitable(df, outcome, "fooditem2")
for(x in c(fooditem1, fooditem2)) {
  epitable(df, outcome, x)
}


