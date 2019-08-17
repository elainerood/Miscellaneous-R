library(tidyverse)

contacts <- readLines("Contacts.vcf")
start_ind <- which(contacts == "BEGIN:VCARD")
end_ind <- which(contacts == "END:VCARD")

# x + 2 removes "begin" and "version" lines; y - 1 removes "end" line
contacts_mapped <- map2(start_ind, end_ind, ~ contacts[(.x + 2):(.y - 1)])
rm(list = c("contacts", "start_ind", "end_ind"))

#### The process ----
#' For privacy reasons I'm taking out the specific steps of my process,
#' because it involves looking at the actual data, but the general idea was:
#' 1) Look at how the text is stored
#' 2) Come up with a way to keep both the field title and field value,
#'    eventually settle on making it a two-column df that can later be spread()
#' 3) Realize that contact photo info has to be specifically dealt with somehow,
#'    determine this can be done with a combination of drop_na() and filter()
#' 4) Find the reason I have duplicate names for spread() to object to,
#'    which ends up being (multiple) custom labels for information. Pull out a
#'    bit of old code from another project to make the custom labels unique
#' 5) Neaten up the field names a bit

## The result:
contacts_df <- map_dfr(contacts_mapped, function(contactX) {
  x <- map_dfr(contactX,
               function(x) {
                 spl <- str_split(x, ":") %>% unlist()
                 tibble(name = spl[1], value = spl[2])
                 }) %>%
    mutate(value = gsub("vnd.android.cursor.item/", "", value),
           value = ifelse(name == "X-ANDROID-CUSTOM",
                          gsub(";1;{2,}", "", value), value),
           value = ifelse(name == "X-ANDROID-CUSTOM",
                          gsub(";{2,}", "", value), value)) %>%
    distinct() %>%
    drop_na() %>%
    filter(!grepl("PHOTO", name)) %>%
    rowwise() %>%
    # ^ https://stackoverflow.com/questions/46886407/mutate-with-a-list-column-function-in-dplyr
    mutate(name = ifelse(str_detect(name, "TEL\\;X-CUSTOM"), "TEL;X-CUSTOM", name),
           name = str_replace_all(name, "\\;", "_"),
           tmp = strsplit(value, ";"),
           value = ifelse(name == "X-ANDROID-CUSTOM", tmp[[2]], value),
           name = ifelse(name == "X-ANDROID-CUSTOM", tmp[[1]], name) %>%
             str_to_upper()
           ) %>%
    select(-tmp)

  if(!identical(duplicated(x$name), integer(0))) {
    dup_cols <- duplicated(x$name)
    x$name[dup_cols] <- sprintf("%s-%02d",
                                x$name[dup_cols],
                                1:sum(dup_cols))
  }

  x %>% spread(name, value)
  }
)
# View(contacts_df)
# Everything is in the right format now, so it's time to make values look nicer

#### Additional cleanup ----
namesort <- map_chr(contacts_df$N, function(x) {
  y <- x %>% str_split("\\;") %>% unlist()

  paste0(y[1], ", ", y[2], " ", y[3], " ",
         y[4], " ", y[5]) %>%
    str_trim(side = "right")
})

address_clean <- function(x) {
  if(is.na(x)) {
    return(NA_character_)
  } else {
    y <- x %>% str_split("\\;") %>% unlist() %>%
      str_trim(side = "both") # %>% `[`(-(1:2))
    y_paste <- paste0(y[3], "; ",
                      y[4], ", ", y[5], " ", y[6],
                      ifelse(y[7] == "", "", paste0("; ", y[7]))
    )
    # -1:2 bc first two are always(?) blank
    # or maybe if there's apt # or something? think about that more
  }
}

homeaddress <- map_chr(contacts_df$ADR_HOME, address_clean)
workaddress <- map_chr(contacts_df$ADR_WORK, address_clean)


contacts_df2 <- bind_cols(
  contacts_df %>%
    select(-N, -ADR_HOME, -ADR_WORK) %>%
    mutate(TEL_CELL = ifelse(is.na(TEL_CELL), TEL_CELL_PREF, TEL_CELL),
           TEL_CELL_PREF = ifelse(TEL_CELL == TEL_CELL_PREF, NA, TEL_CELL_PREF)),
  NAME_SORT = namesort,
  ADR_HOME = homeaddress,
  ADR_WORK = workaddress) %>%
  rename(NAME = FN) %>%
  select(NAME_SORT, NAME, starts_with("TEL_"),
         starts_with("EMAIL_"), starts_with("ADR_"),
         BDAY, everything()) %>%
  arrange(NAME_SORT) %>%
  select_if(~ !all(is.na(.)))
# ^ https://stackoverflow.com/questions/15968494/how-to-delete-columns-that-contain-only-nas

rm(list = c("namesort", "workaddress", "homeaddress"))

# All columns are still character and idk if that's how I want it to be or not, but otherwise everything looks ok. Can write_csv() now or once that's determined