import pandas as pd
import numpy as np


df = pd.read_csv('C:/Users/titti/Desktop/OPENAI/embedding_skills.csv' ,sep='\t')
df["embeddings"] = df.embeddings.apply(eval).apply(np.array)

# reduced dataframe
df_reduced=df.head(100)


# search through a specific prompt
def search(df, description, n=1, pprint=True):
    description = get_embedding(
        description,
        engine="text-embedding-ada-002"
    )
    df["similarity"] = df.embeddings.apply(lambda x: cosine_similarity(x, description))
    
    results = (
         df.sort_values("similarity", ascending=False)
        .head(n)
        .conceptUri.str.replace("", "")
        .str.replace("Occupational Description", ":")
        )
    if pprint:
        for r in results:
            print(r[:400])
            print()
        return str(results)

results = search(df, "coordinate duties of musical staff", n = 1)

 #some simulation which results simOUT
f = open('results.txt','wt')
f.write(str(results) + '\n')
f.close()
