## Tarot version of draw_cards (3/23/19)

library(dplyr)
library(glue)

# major_names = c("The Fool (0)", "The Magician (1)",
#                 "The High Priestess (2)", "The Empress (3)",
#                 "The Emperor (4)", "The Hierophant (5)",
#                 "The Lovers (6)", "The Chariot (7)",
#                 "Strength (8)", "The Hermit (9)",
#                 "The Wheel of Fortune (10)", "Justice (11)",
#                 "The Hanged Man (12)", "Death (13)",
#                 "Temperance (14)", "The Devil (15)",
#                 "The Tower (16)", "The Star (17)",
#                 "The Moon (18)", "The Sun (19)",
#                 "Judgement (20)", "The World (21)")

# major_names = c("0 - The Fool",              "1 - The Magician",
#                 "2 - The High Priestess",    "3 - The Empress",
#                 "4 - The Emperor",           "5 - The Hierophant",
#                 "6 - The Lovers",            "7 - The Chariot",
#                 "8 - Strength",              "9 - The Hermit",
#                 "10 - The Wheel of Fortune", "11 - Justice",
#                 "12 - The Hanged Man",       "13 - Death",
#                 "14 - Temperance",           "15 - The Devil",
#                 "16 - The Tower",            "17 - The Star",
#                 "18 - The Moon",             "19 - The Sun",
#                 "20 - Judgement",            "21 - The World")
# Alternately, make a df with two columns, one w/0:21 and the other w/the names, then just paste() them into the format I want, which would be so much easier except that I did this part already

draw_tarot <- function(n = 1,
                       suits = c("Wands", "Swords", "Cups", "Coins"),
                       minor_names = c(1:10, "Page", "Knight", "Queen", "King"),
                       major_names = c("The Fool", "The Magician",
                                       "The High Priestess", "The Empress",
                                       "The Emperor", "The Hierophant",
                                       "The Lovers", "The Chariot",
                                       "Strength", "The Hermit",
                                       "The Wheel of Fortune", "Justice",
                                       "The Hanged Man", "Death",
                                       "Temperance", "The Devil",
                                       "The Tower", "The Star",
                                       "The Moon", "The Sun",
                                       "Judgement", "The World"),
                       replace = FALSE,
                       limit = NA_character_,
                       exclude = NA_character_
) {
  #' n = number of cards to draw
  #' suits = names for the suits (vector of strings)
  #' minor_names = minor arcana card designations (vector of strings)
  #' major_names = major arcana card names (vector of strings)
  #' replace = draw with/without replacement; default = without
  #' limit = "major" or "minor" if you only want to draw from that arcana, otherwise default is NA to draw from both
  #' exclude = certain major arcana cards to exclude, if any (vector of string/s that must match 1+ of the major arcana names)
  #' ^May later generalize to any card, but that's not something I care about atm -- could do a setdiff() and filter() for that -> might need a new column with the glue() result there instead of external

  limit <- ifelse(is.na(limit), limit,
                  match.arg(limit, c("major", "minor")))
  major_names <- setdiff(major_names, exclude)

  n_deck <- length(suits) * length(minor_names) + length(major_names)
  deck <- tibble(Value = c(rep(minor_names, times = length(suits)),
                           major_names),
                 Suit = c(rep(suits, each = length(minor_names)),
                          rep(NA_character_, times = length(major_names)))
  )

  if(!is.na(limit)) {
    if(limit == "major") {deck <- filter(deck, is.na(Suit))
    } else if(limit == "minor") {deck <- filter(deck, !is.na(Suit))}
  }

  draw_df <- slice(deck, sample(dim(deck)[1], n, replace = replace))
  result <- ifelse(is.na(draw_df$Suit),
                   glue("{draw_df$Value}"),
                   glue("{draw_df$Value} of {draw_df$Suit}")
  )
  draw <- glue("{result}")
  # ^Better output format w/glue
  return(draw)
}

draw_tarot(5)
draw_tarot(21, exclude = "The Fool", limit = "major")
draw_tarot(15, limit = "minor")