library(ggplot2)
library(plyr)
library(dplyr)

### Import and merge csv files ###

t <- list.files(path = ".", pattern="*.csv")

spectra <- lapply(t, function(i) {
  read.csv(i, stringsAsFactors = FALSE)})

names(spectra <- gsub("\\s1\\.csv|\\.csv", "", t))

spectra <- lapply(spectra, "[", -(1:5), ) # Delete intro rows of each file

ed <- bind_rows(spectra, .id = "Sample")
ed$XLabel <- as.numeric(ed$XLabel)
ed$Wavenumber <- as.numeric(ed$Wavenumber)


### Factor and offset spectra ###

ed$Sample <- factor(ed$Sample, levels(ed$Sample)[c("reorder spectra here")]) #order by factor level
ed$offset <- as.integer(ed$Sample) / "offset factor here" #to 'stack' spectra
ta <- ddply(ed, .(Sample), mutate, gr = Wavenumber + offset)

### Plot spectra ###

x <- ggplot(data = ta) +
  geom_line(aes(XLabel, gr, group = (Sample), colour = Sample)) +
  guides(colour = guide_legend(reverse=TRUE)) +
  scale_x_reverse(lim = (c(4000,700)), breaks = seq(4000, 700, -500), name = "Wavenumber") +
  scale_y_continuous(name = "Absorbance, arbitrary units", breaks = NULL, labels = NULL) +
  theme_bw()

### To plot particular spectra: x %+% subset(ta, Sample %in% c("list spectra here"))

ggsave("file_name.pdf", width = 8.4, height = 6) #Save as pdf image

### To extract individual spectra from list: list2env(spectra, envir = .GlobalEnv)
