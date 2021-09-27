find_ids_new <- function(year) {
  base_url <- "https://www.footywire.com/afl/footy/ft_match_list?year="

  z <- year %>%
    purrr::map(~ paste0(base_url, .)) %>%
    purrr::map(xml2::read_html) %>%
    purrr::map(~ rvest::html_nodes(., "a")) %>%
    purrr::map(~ xml2::xml_attr(., "href")) %>%
    purrr::reduce(c)


  x <- grepl("\\?mid=", z)
  z[x]
  # return(x)
}


find_ids_old <- function(year) {
  year %>%
    purrr::map(~ paste0("https://www.footywire.com/afl/footy/ft_match_list?year=", .)) %>%
    # nolint
    purrr::map(xml2::read_html) %>%
    purrr::map(~ rvest::html_nodes(., ".data:nth-child(5) a")) %>%
    purrr::map(~ rvest::html_attr(., "href")) %>%
    purrr::map(~ stringr::str_extract(., "\\d+")) %>%
    purrr::map_if(is.character, as.numeric) %>%
    purrr::reduce(c)
}

year <- 2010:2019
find_ids_old(year)
find_ids_new(year)

microbenchmark::microbenchmark(
  old = find_ids_old(year),
  new = find_ids_new(year)
)

print(benchmark, signif = 2)
ggplot2::autoplot(benchmark)
