#--- Instalace balíčků (pokud je ještě nemáte) ----
# install.packages(c("pdftools","stringr","dplyr","tidyr"))

#--- Načtení knihoven ----------------------------
library(pdftools)   # pdftools::pdf_text
library(stringr)    # stringr::str_detect, str_extract, str_remove, str_trim, str_replace
library(dplyr)      # dplyr::tibble, mutate, case_when, rename, select
library(tidyr)      # tidyr::separate

#--- 1) Načtení a předzpracování textu ------------
pdf_file <- "Literature_rev/Mertlik_2021_Elateridarium.pdf"
txt       <- pdftools::pdf_text(pdf_file)
lines     <- unlist(strsplit(txt, "\n"))
lines     <- stringr::str_trim(lines)
lines     <- lines[lines != ""]

#--- 2) Rozpoznání nadpisů druhů a skládání záznamů -
species_current <- NA_character_
records         <- list()
buf             <- NULL

for (ln in lines) {
  # pokud je to řádek typu "Genus species (Author, Year)", uložíme jako aktuální druh
  if (stringr::str_detect(ln, "^[A-Z][a-z]+\\s[a-z]+\\s?\\(.*\\)$")) {
    species_current <- ln
    next
  }
  # nový záznam, pokud řádek začíná čtvercem "####:"
  if (stringr::str_detect(ln, "^\\d{4}:")) {
    if (!is.null(buf)) {
      records <- append(records, list(list(text = buf, species = species_current)))
    }
    buf <- ln
  } else if (!is.null(buf)) {
    # pokračování aktuálního záznamu
    buf <- paste(buf, ln, sep = " ")
  }
}
# přidej poslední buf
if (!is.null(buf)) {
  records <- append(records, list(list(text = buf, species = species_current)))
}

#--- 3) Parsování polí do tibble -------------------
df <- tibble::tibble(
  raw   = vapply(records, `[[`, "", "text"),
  Druh  = vapply(records, `[[`, "", "species")
) %>%
  dplyr::mutate(
    # původní Lokalita jako celý text po čtverci
    Ctverec  = stringr::str_extract(raw, "^\\d{4}"),
    Lokalita = stringr::str_trim(stringr::str_remove(raw, "^\\d{4}:")),
    Datum    = stringr::str_extract(raw, "\\d{1,2}\\.\\d{1,2}\\.\\d{4}"),
    Pocet    = stringr::str_extract(raw, "\\d+ ?ex\\.|\\d+ ?♂|\\d+ ?♀"),
    Substrat = stringr::str_extract(raw, "Cervus|Equus|ovčí pastvina|světelná UV past"),
    Lat      = as.numeric(stringr::str_extract(raw, "\\d{2}\\.\\d+(?=N)")),
    Lon      = as.numeric(stringr::str_extract(raw, "\\d{2}\\.\\d+(?=E)")),
    Zpusob   = dplyr::case_when(
      str_detect(raw, "observ")                     ~ "observ.",
      str_detect(raw, "leg\\. et coll\\.")          ~ "leg. et coll.",
      str_detect(raw, "leg\\.") & str_detect(raw, "det\\. et coll\\.") ~ "leg., det. et coll.",
      TRUE                                          ~ NA_character_
    ),
    Autor    = dplyr::case_when(
      str_detect(raw, "Mertlik") ~ "J. Mertlik",
      str_detect(raw, "Hron")    ~ "V. Hron",
      TRUE                       ~ NA_character_
    ),
    Poznamka = stringr::str_extract(raw, "(lezl[^,]+|Březový potok[^,]+|pastvina[^,]+)")
  ) %>%
  #--- přejmenování původní Lokalita na Text ---
  dplyr::rename(Text = Lokalita) %>%
  #--- vytvoření nového sloupce Lokalita (substring Text) ---
  dplyr::mutate(
    Lokalita = stringr::str_extract(Text, "^.*?(?=\\d|\\()")
  ) %>%
  #--- rozdělení sloupce Druh na Druh a Popis_druhu ---
  tidyr::separate(
    col   = Druh,
    into  = c("Druh", "Popis_druhu"),
    sep   = " \\(",
    fill  = "right",
    extra = "merge"
  ) %>%
  dplyr::mutate(
    Popis_druhu = stringr::str_replace(Popis_druhu, "\\)$", "")
  ) %>%
  #--- výběr finálních sloupců -----------------
dplyr::select(
  Druh, Popis_druhu, Ctverec, Lokalita, Text, Datum,
  Pocet, Substrat, Lat, Lon, Zpusob, Autor, Poznamka
)

#--- 4) Náhled výsledku ----------------------------
print(df)

#--- 5) (volitelné) export --------------------------
# readr::write_csv(df, "nalezy_koprofagove_clean.csv")
# openxlsx::write.xlsx(df, "nalezy_koprofagove_clean.xlsx")
