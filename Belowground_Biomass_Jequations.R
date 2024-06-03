###mmsf
QUAL.f <- read.csv("QUAL_baseline_MMSF_fineroots_BA26.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE)
QUAL.c <- read.csv("QUAL_baseline_MMSF_coarseroots_BA26.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE)
ACSA.f <- read.csv("ACSA_baseline_coarseroots_MMSF_BA26.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE)
ACSA.c <- read.csv("ACSA_baseline_fineroots_MMSF_BA26.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE)
LITU.f <- read.csv("LITU_baseline_MMSF_BA26_fineroots.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE)
LITU.c <- read.csv("LITU_baseline_MMSF_BA26_coarseroots.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE)
QUAL.stand <- QUAL.f$StandQUAL + QUAL.c$StandQUAL
ACSA.stand <- ACSA.f$StandACSA + ACSA.c$StandACSA
LITU.stand <- LITU.f$StandLITU + LITU.c$StandLITU


library(readxl)
tree_DBH <- read_excel("LITU_baseline_MMSF_BA26T.xlsx")
d <- read_excel("C:/Users/danic/OneDrive - Indiana University/Research/Data collected/Baselines comparaisons/Combined_baselines_ASites.xlsx")
