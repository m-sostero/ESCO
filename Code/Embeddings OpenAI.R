library("openai")
library("tidyverse")
library("ggrepel")
theme_set(theme_bw())

# OPENAI_API_KEY in .Renviron
Sys.getenv("OPENAI_API_KEY")
# write it with Sys.setenv(OPENAI_API_KEY = "sk-xxx...")

ex_strings <- c(
  "feline friends say",
  "meow",
  "canine friends say",
  "woof",
  "god says",
  "let there be light",
  "Matteo asks",
  "Does this work?"
  )

# Get embeddings from OpenAI API
ex_embeddings <- create_embedding(model = "text-embedding-ada-002", input = ex_strings)

#model = ? "text-similarity-babbage-001"

# Frame embedding vectors in a table 
text_embeddings <- ex_embeddings$data$embedding %>%
  set_names(ex_strings) %>% 
  as_tibble()

text_embeddings

# Reduce vectors to two dimensions using Principal component analysis
text_embeddings_pca <- prcomp(text_embeddings, center = TRUE, scale = TRUE)

summary(text_embeddings_pca)

# Plot strings in space spanned by first two PCA dimensions
prcomp(text_embeddings)$rotation[,1:2] %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  ggplot(aes(x = PC1, y = PC2, label = rowname)) + geom_point() + geom_text_repel()

# Compute semantic similarity  (cosine similarity)
text_similarity <- cosine(as.matrix(text_embeddings))

# Plot pairwise similarities between skills
ggcorrplot(skill_similarity, lab = TRUE) +
  labs(title = "Cosine similarity of embedding vectors between pairs of randomly-selected ESCO skills")
