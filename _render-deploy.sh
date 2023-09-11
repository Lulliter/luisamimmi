########################################################
# ===== Build LOCAL version
# R -e 'rmarkdown::render("_posts/slug/slud.Rmd")' # basically knit each post...


#=====  (Clean Everiting *INSIDE* ./docs/*)
setopt rmstarsilent # prevent from asking me permission
rm -rf  docs/*
unsetopt rmstarsilent


########################################################
# ===== Build PUBLIC version
R -e 'rmarkdown::render_site(encoding = "UTF-8")' # install to default location.
########################################################


#=========================================== (Push to Github repo) ================================================#
# check status
git status

# ..... Add ALL changes to git Index.
			git add -A
# ..... Add TRACKED items changes to git Index.
			git add -u
# ..... Add site `/docs` items changes to git Index.
			# cd docs
			# git add . #cd .. # (back to ./blog)
		git add	images/LMMimmi_RES_ENG_sanitized.pdf
       git add images/LMMimmi_RES_ITA_sanitized.pdf
			git add	    docs/images/LMMimmi_RES_ENG_sanitized.pdf
       	git add docs/images/LMMimmi_RES_ITA_sanitized.pdf

git add  _posts/
git add  about.Rmd
git add  docs/about.html
git add  docs/footer_theme.css
git add  docs/index.xml
git add  docs/mytheme.css
git add  docs/posts/



# ....Create Std commit "message"....
NOW=$(date +"%d-%b-%Y %R")
# echo "$NOW"
msg="rebuilt on $NOW"

# ... Commit Those changes.
git commit -m "$msg"
		# git commit -m "css footer attempt"
		# git commit -m "current hybrid site  on CV "
		# git commit -m "small fix"
		# git commit -m "add post r-lists"


# ... Push source and build repos.
git push origin master


#=========================================== (tracked by mistake ) ================================================#
git rm -r --cached .Rproj.user
#=========================================== (check to ./docs) ================================================#
########################################################
cd ./docs

# Go To (git) ./sub-folder (one below the Rproj wdir)
#	cd ./MexicoWaterSurvey
	ls

## ================== # ERRORS  # ================== #
# ----- 1. git push origin master REJECTED
#( error: failed to push some refs to 'https://github.com/Lulliter/LulaWebsite.git')
# ----- SOLUTION
git fetch origin
git merge origin YOUR_BRANCH_NAME # Merges updates made online with your local work
# OR
git pull origin YOUR_BRANCH_NAME # Grabs online updates and merges them with your local work
#
# Rscript -e 'distill::create_post(ti '
