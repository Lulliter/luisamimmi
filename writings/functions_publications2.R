# ====== Define functions for formatting a citation
# ====== Source: Joel Nitta https://github.com/joelnitta/joelnitta-home

# ====== Font Awesome Extension for Quarto
# --- search FA ICONS https://fontawesome.com/search?o=r&m=free
# --- search AI ICONS https://jpswalsh.github.io/academicons/

# {{< fa brands r-project >}}
# {{< ai doi >}}



# 1) - intermediate - FUN print Simple reference -----------------------------------------------------
# Academicons icon
# `key_select` Bibtex key
# `bib` Bibtex bibliography read in with RefManageR::ReadBib(mybib, mytalk )

print_ref_simple <- function(key_select, bib) {
	# Silently cite the key
	NoCite(bib, key_select)
	# Capture the output of printing the reference
	capture.output(foo <- print(bib[[key_select]] , .opts = list(check.entries = FALSE))) %>%
		paste(collapse = " ") %>%
		# Make my name in bold
		str_replace_all("Mimmi, L. M.", "__Mimmi, L. M.__") %>%
		str_replace_all("L. M. Mimmi", "__L. M. Mimmi__")
}

# 2a) - intermediate - FUN doi >>> doi BUTTON -----------------------------------
# `key_select` Bibtex key
# `bib_df` dataframe from .bib

doi_link <- function(key_select, bib_df = mybib_df_acad) {
	doi <- filter(bib_df, key == key_select) %>% pull(doi)  # which COL
	if (anyNA(doi)) return(NULL) #	if (is.na (doi)) return(NULL) #-> error when knit
	paste0("[{{< ai doi >}}]{style=\"color: #7a7a7a;\"}", glue(" [{doi}](https://doi.org/{doi})"))
}

# 2b) - intermediate - FUN preprint >>> open_material BUTTON --------------------------------------------
# Academicons icon
# `key_select` Bibtex key
# `bib_df` dataframe from .bib
open_link <- function(key_select, bib_df = mybib_df_acad) {
	open_material <- filter(bib_df, key == key_select) %>%  pull(preprint  ) # which COL
	if (anyNA(open_material)) return(NULL) #	if (is.na (doi)) return(NULL) #-> error when knit
	paste0("[{{< fa lock-open >}}]{style=\"color: #239b86;\"}", glue(" [Open version]({open_material})"))
}

# - intermediate - FUN researchgate >>> rg BUTTON -
rg_link <- function(key_select, bib_df = mybib_df_acad) {
	rg <- filter(bib_df, key == key_select) %>%  pull(researchgate  ) # which COL
	if (anyNA(rg)) return(NULL) #	if (is.na (doi)) return(NULL) #-> error when knit
	paste0("[{{< fa lock-open >}}]{style=\"color: #239b86;\"}", glue(" [Working Papers]({rg})"))
}

# 2c) - intermediate - FUN rss >>> blog BUTTON -------------------------------------------------------------
# Fontawsome icon
# `key_select` Bibtex key
# `bib_df` dataframe from .bib

blog_link <- function(key_select, bib_df = mybib_df_acad) {
	blog <- filter(bib_df, key == key_select) %>%  pull(rss) # extract COL
	if (anyNA(blog)) return(NULL)
	paste0("[{{< fa solid square-rss >}}]{style=\"color: #3b7697;\"}", glue(" [Blog Post]({blog})"))
}

#3b7697

# 2d) - intermediate - FUN archive  >>> thesis BUTTON --------------------------
# Academicons icon
# `key_select` Bibtex key
# `bib_df` dataframe from .bib
thesis_link <- function(key_select, bib_df = mybib_df_acad) {
	thesis <- filter(bib_df, key == key_select) %>%  pull(archive  ) # which COL
	if (anyNA(thesis)) return(NULL) #	if (is.na (doi)) return(NULL) #-> error when knit
	paste0("[{{< ai archive >}}]{style=\"color: #7f6b00;\"}", glue(" [Univ. repository]({thesis})"))
}

# 2e) - intermediate - FUN open-access  >>> unpublished BUTTON --------------------------
# Academicons icon
# `key_select` Bibtex key
# `bib_df` dataframe from .bib
unpublished_link <- function(key_select, bib_df = mybib_df_acad) {
	unpublished <- filter(bib_df, key == key_select) %>%  pull('open-access'  ) # which COL
	if (anyNA(unpublished)) return(NULL) #	if (is.na (doi)) return(NULL) #-> error when knit
	paste0("[{{< fa solid file-lines >}}]{style=\"color:#7f173d;\"}", glue(" [Other publications]({unpublished})"))
}

# # 2g) - intermediate - FUN file_pdf_path  >>> fa-file-pdf BUTTON --------------------------
# # Academicons icon
# # `key_select` Bibtex key
# # `bib_df` dataframe from .bib
# PDF_download_link <- function(key_select, bib_df = mybib_df_acad) {
# 	PDF_download <- filter(bib_df, key == key_select) %>%  pull(file_pdf_path) # which COL
# 	if (anyNA(file_pdf_path)) return(NULL) #	if (is.na (doi)) return(NULL) #-> error when knit
# 	paste0("[{{< fa solid file-pdf >}}]{style=\"color:#7f173d;\"}", "[PDF]", here::here(glue("{file_pdf_path}")))
# 	}

# 3) Print - FUN Reference + various [icon+link]  --------------------------

print_ref_buttons2 <- function(key_select, bib = mybib, bib_df = mybib_df_acad) {
	# --  USES ABOVE FUNCTION `print_ref_simple`
	ref <- print_ref_simple(key_select = key_select, bib = bib) # CALL Function 1)
	# --  USES ABOVE FUNCTION `doi_link`
	doi <- doi_link(key_select, bib_df = mybib_df_acad) # CALL Function 3)
	#preprint <- link_button(key_select, "preprint", "Preprint", bib_df) # 1
	# --  USES ABOVE FUNCTION `preprint_link`
	open_material <- open_link(key_select, bib_df = mybib_df_acad) # CALL Function 3)
	# --  USES ABOVE FUNCTION `rg_link`
	rg <- rg_link(key_select, bib_df = mybib_df_acad) # CALL Function 3)
	# --  USES ABOVE FUNCTION `blog_link`
	blog <- blog_link(key_select, bib_df = mybib_df_acad) # CALL Function 3)
	# --  USES ABOVE FUNCTION `thesis_link`
	thesis <- thesis_link(key_select, bib_df = mybib_df_acad) # CALL Function 3)
	# --  USES ABOVE FUNCTION `unpublished_link`
	unpublished <- unpublished_link(key_select, bib_df = mybib_df_acad) # CALL Function 3)

	# # --  USES ABOVE FUNCTION `PDF_download_link`
	# PDF_download <- PDF_download_link(key_select, bib_df = mybib_df_acad) # CALL Function 3)

	# --  RETURN
	paste(ref, "<br>", doi, open_material, rg, blog, thesis,  unpublished,
			#PDF_download,

			"<br>", sep = " ")  # sep = "<br>"

}

#  print_ref_buttons2("mimmi_italy_2024")
