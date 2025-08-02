#----------------------------------------------------------#
# Mertlik -----
#----------------------------------------------------------#
#--------------------------------------------------#
# 1) Načtení PDF do vektoru řádků -----
#--------------------------------------------------#
pdf_file <- "Literature_rev/Mertlik_2021_Elateridarium.pdf"
pages    <- pdftools::pdf_text(pdf_file)
lines    <- unlist(strsplit(pages, "\n"))
lines    <- stringr::str_trim(lines)
lines    <- lines[lines != ""]

#--- 2) Detekce species‑headerů s rozšířeným regexem ---
# nově pokrývá i variantu Genus (Subgenus) species (Author, Year)
sp_pattern <- "^[A-Z][a-z]+(\\s\\([^)]*\\))?\\s[a-z]+\\s?\\(.*\\)$"
is_sp      <- stringr::str_detect(lines, sp_pattern)
sp_idx     <- which(is_sp)
sp_name    <- lines[sp_idx]

species_map <- dplyr::tibble(
  line_idx  = sp_idx,
  Druh_full = sp_name
) %>%
  dplyr::arrange(line_idx) %>%
  dplyr::mutate(
    end_idx = dplyr::lead(line_idx, default = length(lines)+1) - 1
  )

#--- 3) Seskupování řádků do záznamů ---------------
records    <- list()
buf_lines  <- NULL
buf_start  <- NULL

for(i in seq_along(lines)) {
  ln <- lines[i]
  if(stringr::str_detect(ln, "^\\d{4}:")) {
    if(!is.null(buf_lines)) {
      records <- append(records, list(list(text=buf_lines, start=buf_start)))
    }
    buf_lines <- ln
    buf_start <- i
  } else if(!is.null(buf_lines)) {
    buf_lines <- c(buf_lines, ln)
  }
}
# přidej poslední
if(!is.null(buf_lines)) {
  records <- append(records, list(list(text=buf_lines, start=buf_start)))
}

#--- 4) Sestav datový rámec a přiřaď Druh podle intervalů
df <- dplyr::tibble(
  raw   = vapply(records, function(x) paste(x$text, collapse=" "), ""),
  start = vapply(records, `[[`, NA_integer_, "start")
) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    # vyber právě jeden species_map řádek, jehož interval obsahuje start
    Druh_full = {
      hit <- species_map %>% dplyr::filter(start >= line_idx & start <= end_idx)
      if(nrow(hit)==1) hit$Druh_full else NA_character_
    }
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Druh_full)) %>%        # volitelné: zahodit záznamy bez druhu
  dplyr::mutate(
    # rozdělení Druh_full
    Druh        = stringr::str_extract(Druh_full, "^[A-Z][a-z]+(\\s\\([^)]*\\))?\\s[a-z]+"),
    Popis_druhu = stringr::str_extract(Druh_full, "(?<=\\().*(?=\\))"),

    # extrakce ostatních polí
    Ctverec  = stringr::str_extract(raw, "^\\d{4}"),
    Text     = stringr::str_trim(stringr::str_remove(raw, "^\\d{4}:")),
    Lokalita = stringr::str_extract(Text, "^.*?(?=\\d| \\()"),
    Datum    = stringr::str_extract(raw, "\\d{1,2}\\.\\d{1,2}\\.\\d{4}"),
    Pocet    = stringr::str_extract(raw, "\\d+ ?ex\\.|\\d+ ?♂|\\d+ ?♀"),
    Substrat = stringr::str_extract(raw, "Cervus|Equus|ovčí pastvina|světelná UV past"),
    Lat      = as.numeric(stringr::str_extract(raw, "\\d{2}\\.\\d+(?=N)")),
    Lon      = as.numeric(stringr::str_extract(raw, "\\d{2}\\.\\d+(?=E)")),
    Zpusob   = dplyr::case_when(
      stringr::str_detect(raw, "observ")                     ~ "observ.",
      stringr::str_detect(raw, "leg\\. et coll\\.")          ~ "leg. et coll.",
      stringr::str_detect(raw, "leg\\.") & stringr::str_detect(raw, "det\\. et coll\\.") ~ "leg., det. et coll.",
      TRUE                                          ~ NA_character_
    ),
    Autor    = {
      known_authors <- c("Mertlik", "Hron", "Šípek", "Hejda", "Novák", "Zíka", "Křivanec", "Růžička")
      author_hits <- known_authors[stringr::str_detect(raw, known_authors)]
      if(length(author_hits) > 0) paste(author_hits, collapse = "; ") else NA_character_
    },
    Poznamka = stringr::str_extract(raw, "(lezl[^,]+|Březový potok[^,]+|pastvina[^,]+)")
  ) %>%
  dplyr::select(
    Druh, Popis_druhu, Ctverec, Lokalita, Text, Datum,
    Pocet, Substrat, Lat, Lon, Zpusob, Autor, Poznamka
  )

#--- 5) Ověření ----------
cat("Euoniticellus fulvus řádků:", sum(df$Druh=="Euoniticellus fulvus"), "\n")
cat("Onthophagus (O.) taurus řádků:", sum(df$Druh=="Onthophagus (O.) taurus"), "\n")
cat("Onthophagus (P.) coenobita řádků:", sum(df$Druh=="Onthophagus (P.) coenobita"), "\n")
cat("Agrilinus convexus řádků:", sum(df$Druh=="Agrilinus convexus"), "\n")

#--- 6) Náhled ------------------------------------
print(df)

#--- 7) Export (volitelné) ------------------------
# readr::write_csv(df, "nalezy_clean.csv")
openxlsx::write.xlsx(df, "Outputs/Data/nalezy_clean_Mertlik_2021.xlsx")
