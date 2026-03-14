# Saliencies, not Face-Template Features, Guide Young Infants to Faces in Everyday Experience

## Overview
This repository contains the R code and datasets for the following paper:

Authors masked. (in review). Saliencies, not face-template features, guide young infants to faces in everyday experience.

## Description
This repository provides all materials necessary to reproduce the analyses and figures presented in our paper examining age-related changes in infants' daily face experiences during infancy. The analyses focus on domain-general saliencies and face-template features of faces viewed by 26 infants using head-mounted cameras at home.

## Repository Structure
The repository is organized into 3 main folders:

- **Data**
  - ImageList_Face_master.csv
  - ImageList_FaceSaliency2_rnd1_01.csv.zip  *(compressed, part 1 of 10)*
  - ImageList_FaceSaliency2_rnd1_02.csv.zip  *(compressed, part 2 of 10)*
  - ImageList_FaceSaliency2_rnd1_03.csv.zip  *(compressed, part 3 of 10)*
  - ImageList_FaceSaliency2_rnd1_04.csv.zip  *(compressed, part 4 of 10)*
  - ImageList_FaceSaliency2_rnd1_05.csv.zip  *(compressed, part 5 of 10)*
  - ImageList_FaceSaliency2_rnd1_06.csv.zip  *(compressed, part 6 of 10)*
  - ImageList_FaceSaliency2_rnd1_07.csv.zip  *(compressed, part 7 of 10)*
  - ImageList_FaceSaliency2_rnd1_08.csv.zip  *(compressed, part 8 of 10)*
  - ImageList_FaceSaliency2_rnd1_09.csv.zip  *(compressed, part 9 of 10)*
  - ImageList_FaceSaliency2_rnd1_10.csv.zip  *(compressed, part 10 of 10)*
  - ImageList_Homeview2_master.csv  
- **Figures**
  - Figure1A.jpg  
  - Figure2.jpg  
  - Figure3C-G.jpg
  - Figure4.jpg  
  - FigureS1.jpg
  - FigureS2bc.jpg
  - FigureS3.jpg 
- **Scripts**
  - Figure1-3.R
  - Figure4.R
  - FigureS2.R

## Dataset Overview
### Sample Characteristics
- Participants: 26 infants
- Infant ages: 2–9 months (grouped into 2–3 months, 5–6 months, and 8–9 months)
- Sessions: 75 unique recording sessions
- Data type: 467,562 images sampled from videos at 1 Hz

### Data Files

**Note:** The randomization analysis dataset is split into 10 compressed files (`ImageList_FaceSaliency2_rnd1_01.csv.zip` through `ImageList_FaceSaliency2_rnd1_10.csv.zip`) to comply with GitHub file size limits. These files must be uncompressed before use. The R scripts automatically combine these files after uncompression. See the [Usage](#usage) section below for instructions.

#### ImageList_Homeview2_master.csv (Image corpus)
Contains 467,562 rows (excluding header) with 7 columns:

| Column           | Description               | Values                                                                                |
| ---------------- | ------------------------- | ------------------------------------------------------------------------------------- |
| AgeCategory      | Infant age in months      | 02-03/05-06/08-09                                                                     |
| Participant      | Participant identifier    | e.g., "25874"                                                                         |
| Session          | Session identifier        | e.g., "P011_25874"                                                                    |
| Cam              | Camera identifier         | e.g., "48"                                                                            |
| image_name       | Image identifier          | e.g., "P011_25874_Cam48_MOVI009_000.jpg"                                            |
| FaceInView       | Face visible              | 0/1                                                                                   |
| DoNotCode        | Quality control flag      | 0/1                                                                                   |

#### ImageList_Face_master.csv (Images containing faces)
Contains 79,397 rows (excluding header) with 24 columns:

| Column              | Description                                                      | Values                                    |
| ------------------- | ---------------------------------------------------------------- | ----------------------------------------- |
| AgeCategory         | Infant age in months                                             | 02-03/05-06/08-09                         |
| Participant         | Participant identifier                                           | e.g., "25874"                             |
| video_name          | Video identifier                                                 | e.g., "P011_25874_Cam48_MOVI009.mp4"      |
| image_name          | Image identifier                                                 | e.g., "P011_25874_Cam48_MOVI009_000.jpg"  |
| N_face              | Number of faces per image                                        | Numeric                                   |
| face_id             | Face identifier                                                  | Numeric                                   |
| facial_area_x1      | X-coordinate of face bounding box (top-left)                     | Numeric                                   |
| facial_area_y1      | Y-coordinate of face bounding box (top-left)                     | Numeric                                   |
| facial_area_x2      | X-coordinate of face bounding box (bottom-right)                 | Numeric                                   |
| facial_area_y2      | Y-coordinate of face bounding box (bottom-right)                 | Numeric                                   |
| size                | Visual size of face                                              | Numeric (range: 0–1)                      |
| Saliency_max_face   | Maximum visual saliency value within face region                 | Numeric (range: 0–1)                      |
| Saliency_DKL        | Maximum saliency value (DKL color channel)                       | Numeric (range: 0–1)                      |
| Saliency_Intensity  | Maximum saliency value (intensity channel)                       | Numeric (range: 0–1)                      |
| Saliency_Orientation| Maximum saliency value (orientation channel)                     | Numeric (range: 0–1)                      |
| Saliency_Flicker    | Maximum saliency value (flicker channel)                         | Numeric (range: 0–1)                      |
| Saliency_Motion     | Maximum saliency value (motion channel)                          | Numeric (range: 0–1)                      |
| centering_x         | Face centering (x-coordinate)                                    | Numeric (range: 0–1)                      |
| centering_y         | Face centering (y-coordinate)                                    | Numeric (range: 0–1)                      |
| N_fp                | Number of visible facial parts                                   | Numeric (range: 0–4)                      |
| eyes                | Number of visible eyes                                           | Numeric (range: 0–2)                      |
| nose                | Nose visible                                                     | 0/1                                       |
| mouth               | Mouth visible                                                    | 0/1                                       |
| roll                | Face orientation along roll axis (degrees)                       | Numeric (range: −180–180)                 |

#### ImageList_FaceSaliency2_rnd1_[01-10].csv (Control analysis - 10 parts)
Each file contains a subset of the complete randomization dataset. When combined, the dataset contains 56,941 rows (excluding header) with 5 columns:

| Column           | Description                                                      | Values                                    |
| ---------------- | ---------------------------------------------------------------- | ----------------------------------------- |
| AgeCategory      | Infant age in months                                             | 02-03/05-06/08-09                         |
| Participant      | Participant identifier                                           | e.g., "25874"                             |
| image_name       | Image identifier                                                 | e.g., "P011_25874_Cam48_MOVI009_000.jpg"  |
| face_id          | Face identifier                                                  | Numeric                                   |
| Rep              | Randomization iteration                                          | Numeric (range: 1–1000)                   |
| saliency         | Maximum saliency map value within face region from randomized saliency map | Numeric (range: 0–1)           |

## Script Descriptions

### Figure1-3.R
- Analyzes age-related changes in measures of domain-general saliencies and face-template features
- Generates the following figures:
  - Figure 1
  - Figure 2
  - Figure 3
  - Figure S1
  - Figure S3

### Figure4.R
- Analyzes age-related changes in temporal properties of face experiences
- Computes domain-general saliency scores and overall face-template scores
- Analyzes relationships between temporal properties and overall domain-general saliencies/face-template scores
- Generates Figure 4

### FigureS2.R
- Uncompresses and combines the 10 split randomization analysis files
- Compares group means of saliency map values between the original dataset and randomized datasets
- Generates Figure S2

## Requirements

### R Version
- R 4.5.0 or higher recommended

### Required R Packages
- tidyverse: 2.0.0
- lme4: 1.1.37
- lmerTest: 3.2.0
- car: 3.1.3
- modelbased: 0.13.0
- emmeans: 1.11.2.8
- effectsize: 1.0.1
- circular: 0.5.2
- ggpubr: 0.6.0
- ggsignif: 0.6.4
- here: 1.0.1
- knitr: 1.50
- magick: 2.9.0

## Usage

### 1. Uncompress the Split Data Files
Before running the analyses, you must uncompress all 10 split data files. The R scripts will automatically combine them.

**On macOS/Linux:**
```bash
cd Data
for i in {01..10}; do
  unzip ImageList_FaceSaliency2_rnd1_${i}.csv.zip
done
```

**On Windows (PowerShell):**
```powershell
cd Data
1..10 | ForEach-Object { 
  $num = $_.ToString("00")
  Expand-Archive "ImageList_FaceSaliency2_rnd1_$num.csv.zip" -DestinationPath .
}
```

**On Windows (Command Prompt):**
- Manually extract each zip file (01 through 10) to the `Data` folder

**In R:**
```r
# Alternative: Uncompress directly in R
library(tidyverse)

# Uncompress all 10 files
for (i in sprintf("%02d", 1:10)) {
  unzip(paste0("Data/ImageList_FaceSaliency2_rnd1_", i, ".csv.zip"), 
        exdir = "Data")
}
```

### 2. Run the Scripts
After uncompressing the data files, run the R scripts in the following order:
1. Figure1-3.R
2. Figure4.R
3. FigureS2.R *(automatically combines the 10 CSV files)*

Ensure all required R packages are installed before running the scripts.

## Contact
For questions or issues, please open an issue on this repository or contact [contact information].

## Authors
- [Author Masked](https://github.com/dororo1225)