library(tidyverse)
# Test names from fakenamegenerator.com, with some manual changes
# Assuming no middle names are spelled out, only initials
namelist <- c("Ollie J. Sherman", "Carlos Pointer Jr", "Maria C. Stallard",
              "Rosa King", "Ione Nevarez Madera", "Clorinda de la Rosa",
              "Olson Jr Terry", "Archie L. Torreggiani III", "R A Jones")

make_lastfirst <- function(x) {
  if(str_detect(x, ",")) {
    ## Format is already "Last [Suffix], First [Middle]"
    result <- x
  } else {
    ## Format is "First [Middle] Last [Suffix]"
    tmp <- str_split(x, " ") %>% unlist()

    if(length(tmp) == 2) {
      ## Format is "First Last"
      result <- paste0(tmp[2], ", ", tmp[1])
    } else {
      ## Format includes middle name, complicated last names, and/or suffix
      if(str_detect(x, "Jr|Sr|III")) {
        ## Suffix
        suf_ind <- str_which(tmp, "Jr|Sr|III")
        # This should just be the max, ideally, but let's be safe and assume it's not
        non_ind <- setdiff(seq_along(tmp), suf_ind)

        result <- paste0(paste(tmp[max(non_ind)], tmp[suf_ind], collapse = " "),
                         ", ", paste(tmp[-c(max(non_ind), suf_ind)], collapse = " "))
      } else {
        ## No suffix
        if(nchar(str_replace(tmp, "\\.", "")[2]) == 1) {
          # ^Remove any period in the name before calling nchar() so that the number of actual letters is counted
          ## Middle initial
          result <- paste0(tmp[length(tmp)], ", ",
                           paste(tmp[-length(tmp)], collapse = " "))
          # ^Could hardcode 3 as length but should I? Come back to it later
        } else {
          ## No middle initial
          result <- paste0(paste(tmp[-1], collapse = " "), ", ", tmp[1])
        }
      }
    }
  }
}

map_chr(namelist, make_lastfirst)
# Probably missing some cases but for my purposes, looks like it works