#--- instalace (pokud chybí) ---------------------
# install.packages(c("pdftools","stringr","dplyr","tidyr"))

#--- načtení knihoven -----------------------------
library(pdftools)   # pdftools::pdf_text
library(stringr)    # str_detect, str_extract, str_trim, str_replace_all
library(dplyr)      # tibble, mutate, case_when, select, rowwise
library(tidyr)      # separate

#--- 1) Načtení PDF do vektoru řádků ---------------
pdf_file <- "Literature_rev/Mertlik_2021_Elateridarium.pdf"
pages    <- pdftools::pdf_text(pdf_file)
lines    <- unlist(strsplit(pages, "\n"))
lines    <- str_trim(lines)
lines    <- lines[lines != ""]

#--- 2) Detekce species‑headerů s rozšířeným regexem ---
# nově pokrývá i variantu Genus (Subgenus) species (Author, Year)
sp_pattern <- "^[A-Z][a-z]+(\\s\\([^)]*\\))?\\s[a-z]+\\s?\\(.*\\)$"
is_sp      <- str_detect(lines, sp_pattern)
sp_idx     <- which(is_sp)
sp_name    <- lines[sp_idx]

species_map <- tibble(
  line_idx  = sp_idx,
  Druh_full = sp_name
) %>%
  arrange(line_idx) %>%
  mutate(
    end_idx = lead(line_idx, default = length(lines)+1) - 1
  )

#--- 3) Seskupování řádků do záznamů ---------------
records    <- list()
buf_lines  <- NULL
buf_start  <- NULL

for(i in seq_along(lines)) {
  ln <- lines[i]
  if(str_detect(ln, "^\\d{4}:")) {
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
df <- tibble(
  raw   = vapply(records, function(x) paste(x$text, collapse=" "), ""),
  start = vapply(records, `[[`, NA_integer_, "start")
) %>%
  rowwise() %>%
  mutate(
    # vyber právě jeden species_map řádek, jehož interval obsahuje start
    Druh_full = {
      hit <- species_map %>% filter(start >= line_idx & start <= end_idx)
      if(nrow(hit)==1) hit$Druh_full else NA_character_
    }
  ) %>%
  ungroup() %>%
  filter(!is.na(Druh_full)) %>%        # volitelné: zahodit záznamy bez druhu
  mutate(
    # rozdělení Druh_full
    Druh        = str_extract(Druh_full, "^[A-Z][a-z]+(\\s\\([^)]*\\))?\\s[a-z]+"),
    Popis_druhu = str_extract(Druh_full, "(?<=\\().*(?=\\))"),
    
    # extrakce ostatních polí
    Ctverec  = str_extract(raw, "^\\d{4}"),
    Text     = str_trim(str_remove(raw, "^\\d{4}:")),
    Lokalita = str_extract(Text, "^.*?(?=\\d| \\()"),
    Datum    = str_extract(raw, "\\d{1,2}\\.\\d{1,2}\\.\\d{4}"),
    Pocet    = str_extract(raw, "\\d+ ?ex\\.|\\d+ ?♂|\\d+ ?♀"),
    Substrat = str_extract(raw, "Cervus|Equus|ovčí pastvina|světelná UV past"),
    Lat      = as.numeric(str_extract(raw, "\\d{2}\\.\\d+(?=N)")),
    Lon      = as.numeric(str_extract(raw, "\\d{2}\\.\\d+(?=E)")),
    Zpusob   = case_when(
      str_detect(raw, "observ")                     ~ "observ.",
      str_detect(raw, "leg\\. et coll\\.")          ~ "leg. et coll.",
      str_detect(raw, "leg\\.") & str_detect(raw, "det\\. et coll\\.") ~ "leg., det. et coll.",
      TRUE                                          ~ NA_character_
    ),
    Autor    = case_when(
      str_detect(raw, "Mertlik") ~ "J. Mertlik",
      str_detect(raw, "Hron")    ~ "V. Hron",
      TRUE                       ~ NA_character_
    ),
    Poznamka = str_extract(raw, "(lezl[^,]+|Březový potok[^,]+|pastvina[^,]+)")
  ) %>%
  select(
    Druh, Popis_druhu, Ctverec, Lokalita, Text, Datum,
    Pocet, Substrat, Lat, Lon, Zpusob, Autor, Poznamka
  )

#--- 5) Ověření pro Euoniticellus fulvus ----------
cat("Euoniticellus fulvus řádků:", sum(df$Druh=="Euoniticellus fulvus"), "\n")

#--- 6) Náhled ------------------------------------
print(df)

#--- 7) Export (volitelné) ------------------------
# readr::write_csv(df, "nalezy_clean.csv")
# openxlsx::write.xlsx(df, "nalezy_clean.xlsx")
