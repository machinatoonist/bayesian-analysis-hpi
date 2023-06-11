# https://rafalab.dfci.harvard.edu/dsbook/

# https://rafalab.dfci.harvard.edu/dsbook/probability.html#examples

B <- 10000
monty_hall <- function(strategy) {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize == "car"]
  my_pick  <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
  stick <- my_pick
  switch <- doors[!doors %in% c(my_pick, show)]
  choice <- ifelse(strategy == "stick", stick, switch)
  choice == prize_door
}

monty_hall("switch")
stick <- replicate(B, monty_hall("stick"))
mean(stick)

switch <- replicate(B, monty_hall("switch"))
mean(switch)
