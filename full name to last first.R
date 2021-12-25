## Original version from 8 Aug 2019

library(tidyverse)
# Test names from fakenamegenerator.com, with some manual changes for variation
# Assuming no middle names are spelled out, only initials
namelist <- c("Ollie J. Sherman", "Carlos Pointer Jr", "Maria C. Stallard",
              "Rosa King", "Ione Nevarez Madera", "Clorinda de la Rosa",
              "Olson Snr., Terry", "Archie L. Torreggiani III", "R A Jones")
suffixlist <- c("Jr", "Jnr", "Sr", "Snr", "III", "IV") %>% 
  paste(collapse = "|")

make_lastfirst <- function(x) {
  ### Input is a character vector of names
  ### Output is a character vector of names formatted to "Last [Suffix], First [Middle]"
  
  if(str_detect(x, ",")) {
    ## Format is already "Last [Suffix], First [Middle]"
    result <- x
  } else {
    ## Format is "First [Middle] Last [Suffix]"
    tmp <- str_split(x, " ") %>% unlist()

    if(length(tmp) == 2) {
      ## Format is "First Last"
      result <- paste(tmp[2], tmp[1], sep = ", ")
    } else {
      ## Format includes middle name, multiple-word last names, and/or suffix
      ## Suffix
      if(str_detect(x, suffixlist)) {
        suffix_idx <- str_which(tmp, suffixlist)
        nonsuff_idx <- setdiff(seq_along(tmp), suffix_idx)

        result <- paste(paste(tmp[max(nonsuff_idx)], tmp[suffix_idx]),
                        paste(tmp[-c(max(nonsuff_idx), suffix_idx)], collapse = " "),
                        sep = ", ")
      ## No suffix
      } else {
        ## Middle initial
        if(nchar(str_replace(tmp, "\\.", "")[2]) == 1) {
          # ^Remove any period in the name before calling nchar() so that the
          # number of actual letters is counted
          # (assumes middle name is listed second instead of third/etc)
          result <- paste(tmp[length(tmp)],
                           paste(tmp[-length(tmp)], collapse = " "),
                          sep =  ", ")
        ## No middle initial
        } else {
          result <- paste(paste(tmp[-1], collapse = " "), tmp[1], sep =  ", ")
        }
      }
    }
  }
}

map_chr(namelist, make_lastfirst) %>% bind_cols(namelist)
# Definitely missing many cases/formats (e.g., assumes no titles are included, no mononyms,
# single middle initials, single-word first names, no multi-word names with suffixes, etc),
# but works for my purposes