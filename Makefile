# prepare the package for release
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: clean devtools_check

doc.pdf:
	R CMD Rd2pdf -o doc.pdf .

build:
	cd ..;\
	R CMD build --no-manual $(PKGSRC)

build-cran:
	cd ..;\
	R CMD build $(PKGSRC)

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build-cran
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

roxygenise:
	R -e "roxygen2::roxygenise()"

devtools_test:
	R -e "devtools::test()"

devtools_check:
	R -e "devtools::check()"

vignette:
	cd vignettes;\
	R -e "rmarkdown::render('knowledgegraphs.Rmd')"

clean:
	$(RM) doc.pdf

download_primekg:
	wget -O kg.csv https://dataverse.harvard.edu/api/access/datafile/6180620
	mv kg.csv inst/extdata/
	R -e "source('scripts/primekg_to_phecode.R')"
