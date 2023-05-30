import os
import pandas as pd
import openai
import tiktoken

openai.api_key = os.getenv("OPENAI_API_KEY")
openai.Model.list()

# embedding model parameters
from openai.embeddings_utils import get_embedding
embedding_model = "text-embedding-ada-002"
embedding_encoding = "cl100k_base"  # this is the encoding for text-embedding-ada-002
max_tokens = 8000  # the maximum for text-embedding-ada-002 is 8191

# Function to monitor progress
def monitor_progress(current_row, total_rows):
    progress_percentage = (current_row / total_rows) * 100
    print(f"Progress: {current_row}/{total_rows} rows processed ({progress_percentage:.2f}%).")

# Load data
input_datapath = "C:/Users/titti/Desktop/OPENAI/skills.csv"
df = pd.read_csv(input_datapath, sep="\t")

# Remove whitespaces
df["description"] = df.description.str.strip()
df["description"] = df["description"].astype("string")

# Get encoding
encoding = tiktoken.get_encoding(embedding_encoding)

# Calculate embeddings
embeddings = []
total_rows = len(df)
for i, row in df.iterrows():
    description = row["description"]
    embedding = get_embedding(description, engine=embedding_model)
    embeddings.append(embedding)
    
    # Call progress monitoring function
    monitor_progress(i + 1, total_rows)

# Assign embeddings to the dataframe
df["embeddings"] = embeddings

print("Processing complete.")

os.makedirs('C:/Users/titti/Desktop/OPENAI', exist_ok=True)
df.to_csv('C:/Users/titti/Desktop/OPENAI/embedding_skills.csv',sep='\t', index=False)