local({
  library(ggplot2)

  # Relationship of a car's weight (`wt`) versus miles per gallon (`mpg`)
  gg <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()

  # save
  saveRDS(gg, file = 'data/ex_1.Rds')
})

local({
  library(ggplot2)

  # counts (or sums of weights)
  # Number of cars in each class
  gg <- ggplot(mpg, aes(class)) + geom_bar()

  # save
  saveRDS(gg, file = 'data/ex_2.Rds')
})

local({
  library(ggplot2)

  # Distribution of highway miles per gallon (`hwy`) in each class
  gg <- ggplot(mpg, aes(x = class, y = hwy)) + geom_boxplot()

  # save
  saveRDS(gg, file = 'data/ex_3.Rds')
})
