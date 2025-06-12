  ## Bash script to download data from the provided URL
#!/bin/bash

# Assign the URL to a variable
URL="https://zenodo.org/records/14808331/files/FLUXDATAKIT_FLUXNET.tar.gz"

# Check if the data is already downloaded
if [ -d "/mnt/data/vajira/fluxDataKit/FLUXDATAKIT_FLUXNET" ]; then
  echo "Data already exists in /mnt/data/vajira/fluxDataKit/FLUXDATAKIT_FLUXNET. Skipping download."
  exit 0
fi

# check if tar file already exists. If so continue to extract. Otherwise download
if [ -f "/mnt/data/vajira/fluxDataKit/FLUXDATAKIT_FLUXNET.tar.gz" ]; then
  echo "Tar file already exists. Skipping download"
else
  # Create the directory if it doesn't exist
  mkdir -p /mnt/data/vajira/fluxDataKit

  # Download the file
  echo "Downloading data from $URL..."
  wget -O /mnt/data/vajira/fluxDataKit/FLUXDATAKIT_FLUXNET.tar.gz "$URL"
fi
# Extract the tar.gz file
echo "Extracting data..."
tar -xzf /mnt/data/vajira/fluxDataKit/FLUXDATAKIT_FLUXNET.tar.gz -C /mnt/data/vajira/fluxDataKit
# Remove the tar.gz file after extraction
rm /mnt/data/vajira/fluxDataKit/FLUXDATAKIT_FLUXNET.tar.gz
echo "Data is ready for use in /mnt/data/vajira/fluxDataKit/FLUXDATAKIT_FLUXNET."
exit 0
# End of script