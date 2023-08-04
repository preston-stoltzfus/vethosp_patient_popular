install.packages("tidyverse")
install.packages("skimr")
install.packages("openxlsx")
library(tidyverse)
library(skimr)
library(openxlsx)

# importing my data
vet_patient_list_2023 <- read_csv("C:/Users/prest/OneDrive/R/vet_patient_list_2023/vet_patient_list_2023.csv")

# getting familiar with my data
View(vet_patient_list_2023)
head(vet_patient_list_2023)
skim(vet_patient_list_2023)

# selecting out rows needed for analysis
vet_patient_list_2023_selects <- vet_patient_list_2023 %>% 
  select(name, breedname, speciesname, description)
# getting familiar with the data again
head(vet_patient_list_2023_selects)
skim(vet_patient_list_2023_selects)


# cleaning begins


# renaming column from "description" to "sex"
vet_patient_list_2023_selects <- vet_patient_list_2023_selects %>%
  rename(sex = description)

# changing data in data columns to lower case and cleaning any whitespace that may/may not be there
vet_patient_list_2023_selects$breedname <- trimws(tolower(vet_patient_list_2023_selects$breedname))
vet_patient_list_2023_selects$sex <- trimws(tolower(vet_patient_list_2023_selects$sex))
vet_patient_list_2023_selects$name <- trimws(tolower(vet_patient_list_2023_selects$name))
vet_patient_list_2023_selects$speciesname <- trimws(tolower(vet_patient_list_2023_selects$speciesname))


# Cleaning up the "sex" column

vet_patient_list_2023_selects$sex <- gsub("undetermined", "unknown", vet_patient_list_2023_selects$sex)
vet_patient_list_2023_selects$sex <- gsub("neutered male", "male, neutered", vet_patient_list_2023_selects$sex)
vet_patient_list_2023_selects$sex <- gsub("spayed female", "female, spayed", vet_patient_list_2023_selects$sex)


# Cleaning up the "name" column

# Row names to be removed, creating specific list since the grepl code below will cast to wide of a net for some names/efficient to just add here
rows_to_remove_names <- c("general use", "kittens-general use", "zzgeneral stuff", "aaaestimates", "aaestimate", 
                          "a center use", "a", "communications", "misc cat", "misc", "miscellaneous", "unknown", "cubex test", "Transfer to Downtown", 
                          "Transfer to Downtown", "zzzzzz")

vet_patient_list_2023_selects <- vet_patient_list_2023_selects[!(vet_patient_list_2023_selects$name %in% rows_to_remove_names), ]

# removing patients that are used for in house things (so fake accounts) or where brought in once for a kitten/puppy exam but never back again
# we know they were never brought back again because their name is still things like "pink collar" and such which breeders
# use to identify different puppies/kittens in the exam rooms
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("client", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("merch", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("pup", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("kitten", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("\\bpink\\b", name))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("miss blue", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("mr blue", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("yellow", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("mr red", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("miss red", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("red collar", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("\\d", name))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("miss white", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("miss purple", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("mr black", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("mr gray", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("mr brown", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("mr green", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("mr orange", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("collar", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("test", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("experiment", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("transfer", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("idexx", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("invoice", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("stray", name, ignore.case = TRUE))
vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !grepl("found", name, ignore.case = TRUE))

vet_patient_list_2023_selects <- subset(vet_patient_list_2023_selects, !is.na(name))


# Cleaning up the "speciesname" column

# Removing fake accounts used for internal purposes
vet_patient_list_2023_selects <- vet_patient_list_2023_selects[!(vet_patient_list_2023_selects$speciesname %in% c("xxxxxxx", "cash small", "cash large")), ]


# Cleaning up the "breedname" column

# Change same breeds with multiple names to the same breed while not combining the "mixed" breeds. For example I 
# don't want to combine "yorkshire terrier" with "yorkshire terrier mix" but yet still need to format things properly
# currently the names are in different orders so I needed a function to find them in any way but exclude certain words.

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("yorkshire", vet_patient_list_2023_selects$breedname) &
    grepl("terrier", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "terrier, yorkshire",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("jack", vet_patient_list_2023_selects$breedname) &
    grepl("russell", vet_patient_list_2023_selects$breedname) &
      grepl("terrier", vet_patient_list_2023_selects$breedname) &
        !grepl("mix", vet_patient_list_2023_selects$breedname),
  "terrier, jack russell",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("jack", vet_patient_list_2023_selects$breedname) &
    grepl("russell", vet_patient_list_2023_selects$breedname) &
      grepl("terrier", vet_patient_list_2023_selects$breedname) &
        grepl("mix", vet_patient_list_2023_selects$breedname),
  "terrier, jack russell mix",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("bull", vet_patient_list_2023_selects$breedname) &
    grepl("terrier", vet_patient_list_2023_selects$breedname) &
      grepl("mix", vet_patient_list_2023_selects$breedname),
  "terrier, bull mix",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("fox", vet_patient_list_2023_selects$breedname) &
    grepl("(smooth)", vet_patient_list_2023_selects$breedname) &
    grepl("terrier", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "terrier, fox smooth",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("boston", vet_patient_list_2023_selects$breedname) &
    grepl("terrier", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "terrier, boston",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("cairn", vet_patient_list_2023_selects$breedname) &
    grepl("terrier", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "terrier, cairn",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("tibetan", vet_patient_list_2023_selects$breedname) &
    grepl("terrier", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "terrier, tibetan",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("american", vet_patient_list_2023_selects$breedname) &
    grepl("staffordshire", vet_patient_list_2023_selects$breedname) &
       !grepl("mix", vet_patient_list_2023_selects$breedname),
  "terrier, american staffordshire",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("americanstaffordshire", vet_patient_list_2023_selects$breedname) &
    grepl("terrier", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "terrier, american staffordshire",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("west", vet_patient_list_2023_selects$breedname) &
    grepl("highland", vet_patient_list_2023_selects$breedname) &
      grepl("terrier", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "terrier, west highland white",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("biewerterrier", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "terrier, biewer",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("aussie-doodle", vet_patient_list_2023_selects$breedname) &
       grepl("mini", vet_patient_list_2023_selects$breedname) &
        !grepl("mix", vet_patient_list_2023_selects$breedname),
  "aussiedoodle, mini",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("labradoodle", vet_patient_list_2023_selects$breedname) &
    grepl("mini", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "labradoodle, mini",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("cock-apoo", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "cockapoo",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("yorkie-poo", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "terrier, yorkie-poo",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("dsh", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "domestic shorthair",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("labrador", vet_patient_list_2023_selects$breedname) &
    grepl("retriever", vet_patient_list_2023_selects$breedname) &
      !grepl("mix", vet_patient_list_2023_selects$breedname),
  "retriever, labrador",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("golden", vet_patient_list_2023_selects$breedname) &
    grepl("retriever", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "retriever, golden",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("dakota", vet_patient_list_2023_selects$breedname) &
      grepl("sport", vet_patient_list_2023_selects$breedname) &
        grepl("retriever", vet_patient_list_2023_selects$breedname) &
          !grepl("mix", vet_patient_list_2023_selects$breedname),
  "retriever, dakota sport",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("retriever", vet_patient_list_2023_selects$breedname) &
    grepl("mixed", vet_patient_list_2023_selects$breedname),
  "retriever mix",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("german", vet_patient_list_2023_selects$breedname) &
    grepl("shepherd", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "shepherd, german",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("australian", vet_patient_list_2023_selects$breedname) &
    grepl("shepherd", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "shepherd, australian",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("anatolian", vet_patient_list_2023_selects$breedname) &
    grepl("shepherd", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "shepherd, anatolian",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("french", vet_patient_list_2023_selects$breedname) &
    grepl("bulldog", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "bulldog, french",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("pit", vet_patient_list_2023_selects$breedname) &
    grepl("bull", vet_patient_list_2023_selects$breedname) &
      grepl("terrier", vet_patient_list_2023_selects$breedname) &
        !grepl("mix", vet_patient_list_2023_selects$breedname),
  "pitbull",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("african", vet_patient_list_2023_selects$breedname) &
    grepl("gray", vet_patient_list_2023_selects$breedname) &
      !grepl("mix", vet_patient_list_2023_selects$breedname),
  "parrot, african grey",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("belgian", vet_patient_list_2023_selects$breedname) &
    grepl("malinois", vet_patient_list_2023_selects$breedname) &
      !grepl("mix", vet_patient_list_2023_selects$breedname),
  "malinois, belgian",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("wolfhound", vet_patient_list_2023_selects$breedname) &
    grepl("irish", vet_patient_list_2023_selects$breedname) &
      !grepl("mix", vet_patient_list_2023_selects$breedname),
  "wolfhound, irish",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("galgo", vet_patient_list_2023_selects$breedname) &
    grepl("spanish", vet_patient_list_2023_selects$breedname) &
      grepl("greyhound", vet_patient_list_2023_selects$breedname) &
        !grepl("mix", vet_patient_list_2023_selects$breedname),
  "greyhound, spanish",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("corgi", vet_patient_list_2023_selects$breedname) &
    grepl("welsh", vet_patient_list_2023_selects$breedname) &
      grepl("pembroke", vet_patient_list_2023_selects$breedname) &
        !grepl("mix", vet_patient_list_2023_selects$breedname),
  "corgi, pembroke welsh",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("dlh", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "domestic longhair",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("dmh", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "domestic mediumhair",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("husky", vet_patient_list_2023_selects$breedname) &
    grepl("siberian", vet_patient_list_2023_selects$breedname) &
      !grepl("mix", vet_patient_list_2023_selects$breedname),
  "husky, siberian",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("maine", vet_patient_list_2023_selects$breedname) &
    grepl("coon", vet_patient_list_2023_selects$breedname) &
      !grepl("mix", vet_patient_list_2023_selects$breedname),
  "coon, maine",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("bullmastiff", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "mastiff, bull",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("bullmastiff", vet_patient_list_2023_selects$breedname) &
    grepl("mix", vet_patient_list_2023_selects$breedname),
  "mastiff, bull mix",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("bernese", vet_patient_list_2023_selects$breedname) &
    grepl("mountain", vet_patient_list_2023_selects$breedname) &
      grepl("dog", vet_patient_list_2023_selects$breedname) &
        !grepl("mix", vet_patient_list_2023_selects$breedname),
  "mountain dog, bernese",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("bernese", vet_patient_list_2023_selects$breedname) &
    grepl("mountain", vet_patient_list_2023_selects$breedname) &
      grepl("dog", vet_patient_list_2023_selects$breedname) &
        grepl("mix", vet_patient_list_2023_selects$breedname),
  "mountain dog, bernese mix",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("greater", vet_patient_list_2023_selects$breedname) &
    grepl("swiss", vet_patient_list_2023_selects$breedname) &
      grepl("mountain", vet_patient_list_2023_selects$breedname) &
        grepl("dog", vet_patient_list_2023_selects$breedname) &
        !grepl("mix", vet_patient_list_2023_selects$breedname),
  "mountain dog, greater swiss",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("oriental", vet_patient_list_2023_selects$breedname) &
    grepl("longhair", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "oriental, longhair",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("oriental", vet_patient_list_2023_selects$breedname) &
    grepl("shorthair", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "oriental, shorthair",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("other", vet_patient_list_2023_selects$breedname) &
    grepl("canin", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "other",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("other", vet_patient_list_2023_selects$breedname) &
    grepl("reptile/amphibian", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "other reptile",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("other", vet_patient_list_2023_selects$breedname) &
    grepl("reptile", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "other reptile",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("other", vet_patient_list_2023_selects$breedname) &
    grepl("breed", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "other",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("miniature", vet_patient_list_2023_selects$breedname) &
    grepl("schnauzer", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "schnauzer, miniature",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("english", vet_patient_list_2023_selects$breedname) &
    grepl("setter", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "setter, english",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("rhodesian", vet_patient_list_2023_selects$breedname) &
    grepl("ridgeback", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "ridgeback, rhodesian",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("shorthair", vet_patient_list_2023_selects$breedname) &
    grepl("american", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "domestic shorthair",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("short", vet_patient_list_2023_selects$breedname) &
    grepl("haired", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "shorthair, misc",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("short", vet_patient_list_2023_selects$breedname) &
    grepl("haired", vet_patient_list_2023_selects$breedname) &
      !grepl("american", vet_patient_list_2023_selects$breedname) &
        !grepl("mix", vet_patient_list_2023_selects$breedname),
  "shorthair, misc",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("shorthair", vet_patient_list_2023_selects$breedname) &
    grepl("english", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "shorthair, english",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("short", vet_patient_list_2023_selects$breedname) &
    grepl("haired", vet_patient_list_2023_selects$breedname) &
      grepl("american", vet_patient_list_2023_selects$breedname) &
        !grepl("mix", vet_patient_list_2023_selects$breedname),
  "shorthair, english",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("welsh", vet_patient_list_2023_selects$breedname) &
    grepl("springer", vet_patient_list_2023_selects$breedname) &
      grepl("spaniel", vet_patient_list_2023_selects$breedname) &
        !grepl("mix", vet_patient_list_2023_selects$breedname),
  "spaniel, welsh springer",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("english", vet_patient_list_2023_selects$breedname) &
    grepl("springer", vet_patient_list_2023_selects$breedname) &
      grepl("spaniel", vet_patient_list_2023_selects$breedname) &
        !grepl("mix", vet_patient_list_2023_selects$breedname),
  "spaniel, english springer",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("cocker", vet_patient_list_2023_selects$breedname) &
    grepl("spaniel", vet_patient_list_2023_selects$breedname) &
      !grepl("mix", vet_patient_list_2023_selects$breedname),
  "spaniel, cocker",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("clumber", vet_patient_list_2023_selects$breedname) &
    grepl("spaniel", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "spaniel, clumber",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("cavalier", vet_patient_list_2023_selects$breedname) &
    grepl("king", vet_patient_list_2023_selects$breedname) &
      grepl("charles", vet_patient_list_2023_selects$breedname) &
        grepl("spaniel", vet_patient_list_2023_selects$breedname) &
          !grepl("mix", vet_patient_list_2023_selects$breedname),
  "spaniel, cavalier king charles",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("teddy", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "shichon (teddy bear)",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- ifelse(
  grepl("teddybear", vet_patient_list_2023_selects$breedname) &
    !grepl("mix", vet_patient_list_2023_selects$breedname),
  "shichon (teddy bear)",
  vet_patient_list_2023_selects$breedname)

vet_patient_list_2023_selects$breedname <- gsub("^golden$", "retriever, golden", vet_patient_list_2023_selects$breedname)


# done cleaning


#Looking at unique values in each column
unique_breeds <- vet_patient_list_2023_selects %>% 
  count(breedname)
View(unique_breeds)

unique_canine_breeds <- vet_patient_list_2023_selects %>%
  filter(speciesname == "canine") %>%
  count(breedname)
View(unique_canine_breeds)

unique_feline_breeds <- vet_patient_list_2023_selects %>%
  filter(speciesname == "feline") %>%
  count(breedname)
View(unique_feline_breeds)

unique_sex <- vet_patient_list_2023_selects %>% 
  count(sex)
View(unique_sex)

unique_names <- vet_patient_list_2023_selects %>% 
  count(name)
View(unique_names)

unique_speciesname <- vet_patient_list_2023_selects %>% 
  count(speciesname)
View(unique_speciesname)


# Creating data frames with just top 10 values

top10_speciesname <- unique_speciesname[order(-unique_speciesname$n), ][1:10, ]
top10_names <- unique_names[order(-unique_names$n), ][1:10, ]
top10_feline_breeds <- unique_feline_breeds[order(-unique_feline_breeds$n), ][1:10, ]
top10_canine_breeds <- unique_canine_breeds[order(-unique_canine_breeds$n), ][1:10, ]


# saving the data in different ways in a folder called "project_results"

# Export as excel in 1 file with separate sheets
# Create a new Excel workbook
wb <- createWorkbook()

# Add each data frame as a separate sheet
addWorksheet(wb, "Top 10 Species Name")
writeData(wb, sheet = "Top 10 Species Name", top10_speciesname)

addWorksheet(wb, "Top 10 Names")
writeData(wb, sheet = "Top 10 Names", top10_names)

addWorksheet(wb, "Top 10 Feline Breeds")
writeData(wb, sheet = "Top 10 Feline Breeds", top10_feline_breeds)

addWorksheet(wb, "Top 10 Canine Breeds")
writeData(wb, sheet = "Top 10 Canine Breeds", top10_canine_breeds)

# Save the workbook to a file
saveWorkbook(wb, file.path("project_results", "top10_breeds_names_species.xlsx"))

# make/save bar plots of top 10
speciesname_plot <- ggplot(top10_speciesname, aes(x = reorder(speciesname, -n), y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 10 Species Names", x = "Species Name", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

names_plot <- ggplot(top10_names, aes(x = reorder(name, -n), y = n)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Top 10 Names", x = "Name", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

feline_breeds_plot <- ggplot(top10_feline_breeds, aes(x = reorder(breedname, -n), y = n)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Top 10 Feline Breeds", x = "Feline Breed", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

canine_breeds_plot <- ggplot(top10_canine_breeds, aes(x = reorder(breedname, -n), y = n)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Top 10 Canine Breeds", x = "Canine Breed", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plots as images in the "project_results" subfolder
ggsave(filename = file.path("project_results", "top10_speciesname_plot.png"), plot = speciesname_plot, width = 10, height = 6, dpi = 300)
ggsave(filename = file.path("project_results", "top10_names_plot.png"), plot = names_plot, width = 10, height = 6, dpi = 300)
ggsave(filename = file.path("project_results", "top10_feline_breeds_plot.png"), plot = feline_breeds_plot, width = 10, height = 6, dpi = 300)
ggsave(filename = file.path("project_results", "top10_canine_breeds_plot.png"), plot = canine_breeds_plot, width = 10, height = 6, dpi = 300)


# exporting entire cleaned data to CSV file
write.csv(vet_patient_list_2023_selects, file = file.path("project_results", "cleaned_vet_patient_list_2023.csv"), row.names = FALSE)
















