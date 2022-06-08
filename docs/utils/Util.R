# MAKE --------------------------------------------------------------------

library(rmake)
rmakeSkeleton('.')
# creates
# Makefile
# Makefile.R


job <- list(
	rRule('dataset.rds', 'preprocess.R', 'dataset.csv'),
	markdownRule('report.pdf', 'report.Rmd', 'dataset.rds'),
	markdownRule('details.pdf', 'details.Rmd', 'dataset.rds')
)
makefile(job, "Makefile")
This will create three build rules



# DISTILL -----------------------------------------------------------------
# ----- https://rstudio.github.io/distill/blog.html#creating-a-collection


distill::create_post("How I created an automated CV updating workflow with R{pagedown}", collection = "portfolio")

distill::create_post(title = "How I created an automated CV updating workflow with R{pagedown}",
							collection = "posts",
							slug = "2022-06-03-CV-R-rpagedown",
							draft = FALSE								)
# v Created new collection at _projects
# v Created post at _projects/2022-06-03-2022-06-03-cv-r-rpagedown
# o TODO: Register 'projects' collection in _site.yml
# o TODO: Create listing page for 'projects' collection
#
# See docs at https://rstudio.github.io/distill/blog.html#creating-a-collection


distill::create_post(title = "TEST",
							collection = "posts",
							slug = "2022-06-03-TEST",
							draft = FALSE								)



