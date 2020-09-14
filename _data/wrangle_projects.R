library(tidyverse)

titles <- tribble(
  ~id, ~title, ~main_theme, ~cross_theme, ~student_url,
  1, "Towards Translating Network Science and Brain Graphs into Neurosurgery", "health", NA, "doctoral-students/anujan-poologaindran",
  2, "Budgeting for SDGs: A Data-driven Approach", "economic", "policy", "enrichment-students/daniele-guariso",
  3, "Within and between classroom transmission patterns of seasonal influenza inform management of COVID-19 in schools", "health", "policy", "enrichment-students/akira-endo",
  4, "Intriguing Properties of Adversarial ML Attacks in the Problem Space", "secure", "tools", "enrichment-students/feargus-pendlebury",
  5, "Estimating counterfactual treatment outcomes over time through adversarially balanced representations", "health", "tools", "doctoral-students/ioana-bica",
  6, "Using comprehensive transcriptome analysis to reveal the landscape of pathobiology in early rheumatoid arthritis", "health", NA, "enrichment-students/katriona-goldmann",
  7, "FAIR-Biomed: A browser extension for accessing open data in the biomedical domain", "health", "tools", "enrichment-students/katriona-goldmann",
  8, "Regulation of AI and corresponding explainability practices", NA, "policy", "doctoral-students/keri-grieman",
  9, "Community detection - Bayesian inference for robust detection of assortative structure", "engineer", NA, "enrichment-students/lizhi-zhang",
  10, "Exploration and Exploitation in US Corporate Research", "economic", NA, "doctoral-students/nikolas-kuhlen",
  11, "An agent-based model of jaywalking: Representing contested street space in models of pedestrian movement", "social", NA, "enrichment-students/obi-thompson-sargoni",
  12, "Identification and impact assessment of recurring traffic bottlenecks using ANPR technology", "engineer", "policy", "enrichment-students/pedro-pinto-da-silva",
  13, "Early Warning Signals for COVID-19 Using Probabilistic Risk Awareness Framework", "health", "tools", "doctoral-students/prateek-gupta",
  14, "Using machine learning to improve resolution and bias in urban temperature projections", "engineer", "policy", "enrichment-students/risa-ueno",
  15, "Hierarchical Monte Carlo Fusion", "engineer", "theory", "doctoral-students/ryan-chan",
  17, "Tailored Bayes: a risk modelling framework under unequal classification costs", "health", "tools", "enrichment-students/solon-karapanagiotis",
  18, "Protein domain-domain interaction prediction via deep neural networks", "health", NA, "enrichment-students/tugce-oruc",
) %>%
  mutate(student_url = str_glue("https://www.turing.ac.uk/people/{student_url}")) %>%
  mutate(title = na_if(title, ""))

posters <- read_csv(
  file = "students_20200831.csv",
  col_names = TRUE,
  col_types = cols(
    name = col_character(),
    email = col_character(),
    cohort = col_character(),
    abstract = col_character(),
    programme = col_character(),
    extra = col_skip()
  )
) %>%
  mutate(programme =
    if_else(row_number() == 9,
            "Health and medical science;Data-centric engineering",
            programme)
  ) %>%
  mutate(cohort = as.factor(cohort)) %>%
  mutate(programme = str_split(programme, ";", n = 2)) %>%
  rowwise() %>%
  mutate(programme = list(set_names(programme,  c("p1", "p2")))) %>%
  tidyr::unnest_wider(programme) %>%
  mutate(p2 = str_remove_all(p2, ';')) %>%
  mutate(p2 = na_if(p2, "")) %>%
  filter(!stringr::str_detect(name, "Pap Aron")) %>%
  mutate(abstract = str_remove_all(abstract, "\n")) %>%
  arrange(name, abstract) %>%
  mutate(id = row_number()) %>%
  # fix individual abstracts
  mutate(abstract = if_else(id == 5,
                            str_split(abstract[5], "Abstract: ", n = 2)[[1]][2],
                            abstract)) %>%
  mutate(abstract = if_else(id == 6,
                            str_remove(abstract, "Background"),
                            abstract)) %>%
  mutate(abstract = if_else(id == 18,
                            str_remove(abstract, filter(titles, id == 18) %>% pull(title)),
                            abstract)) %>%
  select(id, everything()) %>%
  inner_join(titles, by = "id") %>%
  rename(student = name) %>%
  select(id, student, title, abstract, email, cohort, main_theme, cross_theme, student_url) %>%
  mutate(title = replace_na(title, "To be defined")) %>%
  mutate(id = str_pad(as.character(id), 2, "left", "0")) %>%
  mutate(production = "false") %>%
  rename(poster_id = id) %>%
  mutate(file_id = str_glue("{poster_id}-{student}")) %>%
  mutate(file_id = str_to_lower(file_id)) %>%
  mutate(file_id = str_replace_all(file_id, " ", "-"))

# posters <- posters %>%
#   mutate(cancel = poster_id == "01")

write_csv(posters, "posters.csv")

frontmatter <- function(x) {
  glue::glue("
  ---
  poster_id: |
    {x$poster_id}
  student: {x$student}
  title: |
    {x$title}
  abstract: |
    {x$abstract}
  email: {x$email}
  cohort: {x$cohort}
  main_theme: {x$main_theme}
  cross_theme: {x$cross_theme}
  student_url: {x$student_url}
  file_id: |
    {x$file_id}
  production: {x$production}
  ---
  ")
}

# cancel: {tolower(x$cancel)}

posters %>%
  group_by(poster_id) %>%
  group_split() %>%
  map(~{
    write_lines(frontmatter(.x), path = glue::glue("../_posters/{.x$poster_id}.md"))
  })
