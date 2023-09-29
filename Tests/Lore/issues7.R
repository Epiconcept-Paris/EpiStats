
df <-  data.frame(
  outcome = c(rep(0, 10), rep(1, 8), rep(NA, 3)),
  fooditem1 = c(rep(0, 10), rep(1, 11)),
  fooditem2 = c(rep(0, 10), rep(1, 5), rep(NA, 6)),
  fooditem3 = c(rep(0, 21)),
  fooditem4 = c(rep(1, 21))
)

cc(df, outcome, fooditem1)


df <-  data.frame(
  outcome = sample(c(0, 1, NA), 1000, replace = TRUE, prob = c(0.40, 0.59, 0.01)),
  fooditem1 = sample(c(0, 1, NA), 1000, replace = TRUE, prob = c(0.5, 0.5, 0.)),
  fooditem2 = sample(c(0, 1, NA), 1000, replace = TRUE, prob = c(0.20, 0.79, 0.01)),
  fooditem3 = c(rep(0, 1000)),
  fooditem4 = c(rep(1, 1000))
)

cc(df, outcome, fooditem1)
cc(df, outcome, fooditem2)
cc(df, outcome, fooditem3)
cc(df, outcome, fooditem4)

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

library(epiuf)
epitable(df, outcome, fooditem1)
epitable(df, outcome, fooditem2)
epitable(df, outcome, "fooditem1")
epitable(df, outcome, "fooditem2")
for(x in c(fooditem1, fooditem2)) {
  epitable(df, outcome, x)
}


