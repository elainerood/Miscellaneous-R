## Original version from 10/26/18

library(dplyr)

draw_cards <- function(n = 1,
                       suits = c("Hearts", "Clubs", "Diamonds", "Spades"),
                       per_suit = 13,
                       repl = FALSE,
                       use_special = TRUE,
                       special_cards = c("1" = "Ace", "11" = "Jack",
                                         "12" = "Queen", "13" = "King")) {
  #' n = number of cards to draw
  #' suits = names for the suits (vector of strings)
  #' per_suit = number of cards per suit
  #' repl = draw with/without replacement; default = without
  #' use_special = use Ace, Jack, etc (vs 1, 11, etc)
  #' special_cards = special designation for individual card values (e.g., ace, king, etc).
  #'   Must be a named character vector where the names are the corresponding value in range 1:per_suit.

  sc_vals <- as.numeric(names(special_cards))
  card_vals <- as.character(1:per_suit)

  if(use_special) {
    for(i in seq_along(sc_vals)) {
      x <- min(grep(pattern = paste0(sc_vals[i], "$"), x = card_vals))
      card_vals[x] <- special_cards[i]
    }
  }

  n_deck <- length(suits) * per_suit
  deck <- data_frame(Value = rep(card_vals, times = length(suits)),
                     Suit = rep(suits, each = per_suit))


  draw_df <- slice(deck, sample(dim(deck)[1], n, replace = repl))
  draw <- paste(draw_df$Value, "of", draw_df$Suit)
  return(draw)
}

draw_cards(5)
draw_cards(n = 3, suits = c("Rabbits", "Mice", "Hawks", "Foxes"),
           per_suit = 5, use_special = FALSE)

## V2 (1/20/19)

library(purrr)
library(glue)

draw_cards2 <- function(n = 1,
                        suits = c("Hearts", "Clubs", "Diamonds", "Spades"),
                        per_suit = 13,
                        replace = FALSE,
                        use_special = FALSE,
                        special_cards = c("Ace" = 1, "Jack" = 11,
                                          "Queen" = 12, "King" = 13)) {
  #' n = number of cards to draw
  #' suits = names for the suits (vector of strings)
  #' per_suit = number of cards per suit
  #' replace = draw with/without replacement; default = without
  #' use_special = use special names like Ace, Jack, etc (vs 1, 11, etc).
  #'   Defaults to not using because the default settings for a standard card deck won't always work for custom decks.
  #' special_cards = names/designations for individual card values (e.g., ace, king, etc).
  #'   Must be a named character vector where the values are in range 1:per_suit and the names are the corresponding designations.

  card_vals <- as.character(1:per_suit)

  if(use_special) {
    card_vals <- map_chr(
      seq_along(card_vals),
      ~ ifelse(.x %in% special_cards,
               names(special_cards)[which(special_cards == .x)],
               .x)
    )
  }

  n_deck <- length(suits) * per_suit
  deck <- tibble(Value = rep(card_vals, times = length(suits)),
                 Suit = rep(suits, each = per_suit))


  draw_df <- slice(deck, sample(dim(deck)[1], n, replace = replace))
  draw <- glue("{draw_df$Value} of {draw_df$Suit}")
  return(draw)
}

set.seed(1395)
draw_cards(5)
set.seed(1395)
draw_cards2(5)
# Same answer, better reading format