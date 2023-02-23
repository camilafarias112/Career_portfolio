View(scholar::get_publications("6jg00_wAAAAJ") %>%
  arrange(desc(year)) %>%
  detailed_entries(
    what = author,
    when = year,
    with = title,
    where = journal
  ))

table <- get_publications("6jg00_wAAAAJ") %>%
  arrange(desc(year))
write_tsv(table %>%
            select(title), "order_publications.txt")
