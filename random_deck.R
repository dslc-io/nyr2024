gen_pkg_deck <- function(word, path_word = "random", tries = 10) {
  if (tries == 0) {
    cli::cli_abort("Out of tries!", class = "nyr_error-repeated_failures")
  }
  remaining <- tries - 1
  tryCatch(
    .gen_pkg_deck_impl(word, path_word),
    error = function(cnd) {
      return(gen_pkg_deck(word, path_word, tries = remaining))
    }
  )
  .gen_pkg_deck_images(word, path_word)
}

.gen_pkg_deck_impl <- function(word, path_word = "random") {
  deck <- .generate_deck(word)
  .save_deck(deck, path_word)
}

.create_pkg_name <- function(word) {
  pkg <- stringr::str_replace_all(
    word, "(?<=[^aeiouAEIOU])[aeiouAEIOU](r|R)", "R"
  )
  pkg <- stringr::str_replace_all(pkg, "r", "R")
  if (!stringr::str_detect(pkg, "r|R")) {
    pkg <- paste0("r", pkg)
  }
  pkg
}

.create_talk_description <- function(pkg, word) {
  word_title_case <- snakecase::to_title_case(word)
  glue::glue(
    "Lightning talk about {pkg}, an R package about {word_title_case}.",
    "The talk should have at most 5 slides.",
    "It should not contain any code.",
    "Do not include a live demonstration.",
    .sep = " "
  )
}

.create_header <- function(talk_title) {
  title_line <- paste("title: |", talk_title, sep = "\n  ")
  paste(
    "---",
    title_line,
    "css: style.css",
    "execute:",
    "  eval: false",
    "  echo: true",
    "format:",
    "  revealjs:",
    "    theme: dark",
    "    footer: Jon Harmon | @jonthegeek@fosstodon.org",
    "    link-external-newwindow: true",
    "    transition: slide",
    "---",
    "\n",
    sep = "\n"
  )
}

.generate_deck <- function(word) {
  print(paste("Generating deck. word =", word))
  pkg <- .create_pkg_name(word)
  talk_title <- glue::glue("Introducing {{{pkg}}}: A New R Package")
  talk_description <- .create_talk_description(pkg, word)
  deck <- robodeck::gen_deck(
    talk_title, minutes = 5, description = talk_description
  )
  .finalize_deck(deck, talk_title)
}

.finalize_deck <- function(deck, talk_title) {
  deck <- stringr::str_remove_all(deck, "```markdown")
  deck <- stringr::str_remove_all(deck, "```")
  header <- .create_header(talk_title)
  paste(header, deck, sep = "\n")
}

.save_deck <- function(deck, path_word) {
  withr::local_dir("random_deck")
  deck_path <- glue::glue("{path_word}.qmd")
  html_path <- glue::glue("{path_word}.html")
  writeLines(deck, deck_path)
  quarto::quarto_render(deck_path, output_file = html_path)
}

.gen_pkg_deck_images <- function(word, path_word) {
  withr::local_dir("random_deck")
  deck_path <- glue::glue("{path_word}.qmd")
  deck <- readLines(deck_path)
  images <- stringr::str_subset(deck, "^\\!\\[.*\\]\\(.*\\)$") |>
    strcapture(
      "^\\!\\[(.*)\\]\\((.*)\\)$",
      x = _,
      tibble::tibble(desc = character(), filename = character())
    )
  pkg <- .create_pkg_name(word)
  talk_title <- glue::glue("Introducing {{{pkg}}}: A New R Package")

  purrr::walk2(
    images$desc,
    images$filename,
    \(desc, filename) {
      robodeck::gen_image(
        desc,
        title = talk_title,
        image_path = filename,
        image_size = "large"
      )
    }
  )
}
