library("tidyverse")
library("lsa")
library("ggcorrplot")

# Import table of skills, with identifiers, description, and column with embeddings
skill_table <- read_tsv("Embeddings/embedding_skills.csv", col_types = cols(definition = "c"))

skill_table

# the `embeddings` variable is currently a character, but actually contains a numeric vector, to unpack


# Transform skill table into embeddings vector table ----------------------

# Create table of skill embedding vectors (each skill is a column) 
skill_embeddings <- skill_table %>%
  # Remove the [brackets] from the embedding variable
  mutate(embeddings = str_remove_all(embeddings, "\\[|\\]")) %>% 
  # Splits the constituent elements of each record of embedding in a separate row, treat as numeric vector
  separate_longer_delim(embeddings, delim = ", ") %>% 
  mutate(embeddings = as.numeric(embeddings)) %>% 
  # Reshape the table in "wide" format: one colum per skill, embeddings row-wise
  select(preferredLabel, embeddings) %>% 
  group_by(preferredLabel) %>% 
  mutate(feature = 1:n()) %>% 
  pivot_wider(names_from = preferredLabel, values_from = embeddings) %>% 
  select(-feature)

skill_embeddings


# Compute cosine similarity between skill+description embeddings ----------

set.seed(1234)
skill_embeddings_sample <- skill_embeddings[, sample(1:ncol(skill_embeddings), size = 10)]

skill_embeddings_sample %>% names()

skill_similarity <- cosine(as.matrix(skill_embeddings_sample))

ggcorrplot(skill_similarity, lab = TRUE) +
  labs(
    title = "Similarity between all skills seems to high",
    subtitle = "Cosine similarity of embedding vectors between pairs of randomly-selected ESCO skills")


skill_similarity %>% 
  as_tibble(rownames = "skill") %>% 
  write_csv("esco_sample_similarities.csv")

# Compute embeddings for skills and descriptions, separately ----

set.seed(1234)
esco_sample <- skill_table %>% slice_sample(n = 100) %>% 
  select(skill = preferredLabel, description)

write_rds(esco_sample, file = "esco_sample.rds")

# Get embeddings from OpenAI API, frame them in a table, column-wise
esco_sample_emb_skills <- create_embedding(model = "text-embedding-ada-002", input = esco_sample$skill)$data$embedding %>%
  set_names(esco_sample$skill) %>% 
  as_tibble()

write_rds(esco_sample_emb_skills, file = "esco_sample_emb_skills.rds")

# Compute pairwise cosine similarity
esco_sample_emb_skills %>% 
  select(1:10) %>% 
  as.matrix() %>%
  cosine() %>% 
  ggcorrplot(lab = TRUE) +
  labs(
    title = "Similarity between all skills seems to high",
    subtitle = "Cosine similarity of embedding vectors between pairs of randomly-selected ESCO skills"
    )

esco_sample_emb_descriptions <- create_embedding(model = "text-embedding-ada-002", input = esco_sample$description)$data$embedding %>%
  set_names(esco_sample$description) %>% 
  as_tibble()



find_similar <- function(skill) {
  bind_cols(esco_sample_emb_skills[, skill], esco_sample_emb_descriptions) %>%
    as.matrix() %>% 
    cosine() %>% 
    as_tibble(rownames = "compare") %>% 
    filter(compare == names(esco_sample_emb_skills[, skill])) %>% 
    pivot_longer(cols = -compare, names_to = "match_description", values_to = "similarity") %>% 
    filter(match_description != compare) %>% 
    arrange(desc(similarity)) %>% 
    slice_head(n = 5) 
} 


esco_closest_descriptions <- esco_sample_emb_skills %>% 
  names() %>% 
  map_dfr(~ find_similar(.))

# When is the closest description not the correct one? 
esco_closest_descriptions %>% 
  group_by(compare) %>% 
  slice_max(similarity) %>% 
  left_join(select(skill_table, preferredLabel, description),  by = c("compare" = "preferredLabel")) %>% 
  filter(description != match_description) %>% 
  select(skill = compare, `Official description` = description, `Closest description` = match_description, `Similarity` = similarity ) %>% 
  pander(split.cell = 20)

esco_closest_descriptions %>%
  group_by(compare) %>% 
  mutate(
    rank_similarity = row_number() %>% factor(labels = c("1st closest", "2nd closest", "3rd closest", "4th closest", "5th closest")),
    gap = similarity - lead(similarity)
    ) %>% 
  ggplot(aes(x = rank_similarity, y = similarity, group = compare)) + geom_line(aes(alpha = gap)) + geom_point() +
  scale_alpha_binned(range = c(0.2, 0)) +
  labs(
    title = "The closest skill description (usually the correct one) has much higher similarity to the skill label than the rest",
    subtitle = "Distribution of cosine similarity between the embedding of skills and each possible description",
    x = "Similarity rank: from highest to 5th-highest cosine similarity",
    y = "Cosine similarity"
  )
