# Create the default project structure.

# Where raw data is stored
dir.create('data/raw', recursive=TRUE)
# Where data collected externally is stored
dir.create('data/external', recursive=TRUE)
# Where data after processing (ready for modeling) is stored
dir.create('data/processed', recursive=TRUE)

# where trained models are stored
dir.create('models', recursive=TRUE)

# where experimental results are stored
dir.create('results', recursive=TRUE)
