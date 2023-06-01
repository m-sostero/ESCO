library("tidyverse")
library("openai")
library("lsa")
library("ggcorrplot")

# Import table of skills, with identifiers, description,
# Includes a column with embeddings of preferredlabel + description computed by Vesna
esco_table <- read_tsv("Embeddings/embedding_skills.csv", col_types = cols(definition = "c"))

esco_table

# the `embeddings` variable is currently a character, but actually contains a numeric vector, to unpack


# Transform skill table into embeddings vector table ----------------------

# Create table of skill embedding vectors (each skill is a column) 
skill_embeddings <- esco_table %>%
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
  write_csv("Sample/esco_sample_similarities.csv")

# Compute embeddings for skills and descriptions, separately ----

set.seed(1234)
esco_sample <- skill_table %>%
  slice_sample(n = 100) %>% 
  select(skill = preferredLabel, description)

write_rds(esco_sample, file = "Sample/esco_sample.rds")

esco_sample <- read_rds("Sample/esco_sample.rds")



# Get text embeddings from OpenAI API ----

# Define function that takes a vector string as input
# and returns a table of embeddings (one column per string, named after the string values)

esco_table$preferredLabel

get_embedding_table <- function(string_vec) {
  
  
  map()
  
  create_embedding(model = "text-embedding-ada-002", input = string_vec)$data$embedding %>%
    set_names(string_vec) %>% 
    as_tibble()
}

# Get embeddings for selected ESCO skills (preferredLabel)
# esco_sample_emb_skills <- get_embedding_table(esco_sample$description)
# write_rds(esco_sample_emb_skills, file = "Sample/esco_sample_emb_skills.rds")
esco_sample_emb_skills <- read_rds("Sample/esco_sample_emb_skills.rds")


# Get embeddings for selected ESCO skill descriptions
# esco_sample_emb_desc <- get_embedding_table(esco_sample$description)
# write_rds(esco_sample_emb_desc, file = "Sample/esco_sample_emb_desc.rds")
esco_sample_emb_desc <- read_rds("Sample/esco_sample_emb_desc.rds")


# Get embeddings for *all* ESCO skills (preferredLabel)
esco_emb_skills <- get_embedding_table(esco_table$preferredLabel)
write_rds(esco_emb_skills, file = "Embeddings/esco_sample_emb_skills.rds")


# Compute pairwise cosine similarity between skill embeddings
esco_sample_emb_skills %>% 
  select(1:10) %>% 
  as.matrix() %>%
  cosine() %>% 
  ggcorrplot(lab = TRUE, hc.order = TRUE) +
  labs(
    title = "Similarity between all skills is quite high",
    subtitle = "Cosine similarity of embedding vectors between pairs of randomly-selected ESCO skills"
  )




# Find the closest embeddings relative to a reference ----


cos_sim_vec <- function(A, B) {
  return(sum(A*B, na.rm = TRUE) / sqrt(sum(A^2, na.rm = TRUE)*sum(B^2, na.rm = TRUE)))
}

# Possibly more efficient for larger matrices
# Matrix <- as.matrix(DF)
# sim <- Matrix / sqrt(rowSums(Matrix * Matrix))
# sim <- sim %*% t(sim)

# Define function that, given a table of reference embedding on the LHS,
# finds the n_closest most cosine-similar embeddings from a table in the reference table on the RHS
find_closest_embeddings <- function(reference, comparison, n_closest = 5) {
  map_dfr(
    # Iterate over elements (named embedding vectors) in `reference`
    reference, \(x)
    # Compute distance with respect to each element (named embedding vectors) in `comparison`
    map_dfc(comparison, \(y) cos_sim_vec(x, y)) %>% 
      gather(key = "comparison", value = "similarity") %>%
      # Keep the `n_closest` elements of comparisons with highest cosine similarity, rank them
      slice_max(similarity, n = n_closest) %>% 
      mutate(similarity_rank = row_number()),
    .id = "reference"
  )
}

esco_sample_closest <- find_closest_embeddings(esco_sample_emb_skills, esco_sample_emb_desc, n = 5)



# When is the closest description not the correct one? ----

esco_sample_closest %>% 
  left_join(select(esco_table, preferredLabel, description),  by = c("reference" = "preferredLabel")) %>% 
  filter(description != comparison) %>% 
  select(skill = compare, `Official description` = description, `Closest description` = match_description, `Similarity` = similarity) %>% 
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
