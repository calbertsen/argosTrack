R?=R
PACKAGE=argosTrack

all: doc build check test install

doc:
	@echo "\033[0;32mUpdating documentation\033[0;0m"
	rm -f ${PACKAGE}/src/*.so
	$(R) -q -e 'devtools::document("${PACKAGE}")'

build: 
	@echo "\033[0;32mBuilding package\033[0;0m"
	$(R) CMD build ${PACKAGE}

check: build
	@echo "\033[0;32mChecking package as cran\033[0;0m"
	$(eval VERSION = $(shell Rscript -e "l<-readLines(\"${PACKAGE}/DESCRIPTION\");cat(gsub(\"Version: \",\"\",l[grepl(\"Version: \",l)]))"))
	$(R) CMD check --as-cran ${PACKAGE}_${VERSION}.tar.gz

checkquick: build
	@echo "\033[0;32mChecking package as cran (no vignette) \033[0;0m"
	$(eval VERSION = $(shell Rscript -e "l<-readLines(\"${PACKAGE}/DESCRIPTION\");cat(gsub(\"Version: \",\"\",l[grepl(\"Version: \",l)]))"))
	$(R) CMD check --as-cran --no-vignettes --no-build-vignettes ${PACKAGE}_${VERSION}.tar.gz

checkbc: build
	@echo "\033[0;32mChecking package with bioconductor\033[0;0m"
	$(eval VERSION = $(shell Rscript -e "l<-readLines(\"${PACKAGE}/DESCRIPTION\");cat(gsub(\"Version: \",\"\",l[grepl(\"Version: \",l)]))"))
	$(R) CMD BiocCheck --no-check-bioc-views ${PACKAGE}_${VERSION}.tar.gz

install: build
	@echo "\033[0;32mInstalling package\033[0;0m"
	$(eval VERSION = $(shell Rscript -e "l<-readLines(\"${PACKAGE}/DESCRIPTION\");cat(gsub(\"Version: \",\"\",l[grepl(\"Version: \",l)]))"))
	@echo "Version: ${VERSION}"
	$(R) CMD INSTALL ${PACKAGE}_${VERSION}.tar.gz

install_dependencies:
	@echo "\033[0;32mInstalling package dependencies\033[0;0m"
	@echo "source('https://raw.githubusercontent.com/calbertsen/caMisc/master/R/build_from_github.R'); \
	installDependencies('${PACKAGE}/DESCRIPTION',dependencies=c(\"Depends\",\"LinkingTo\",\"Imports\",\"Suggests\",\"Enhances\"))" | $(R) -q --slave

install_bc:
	@echo "\033[0;32mInstalling BiocCheck\033[0;0m"
	$(R) -q -e 'if (!"BiocManager" %in% rownames(installed.packages())) install.packages("BiocManager"); BiocManager::install("BiocCheck")'

test:
	@echo "\033[0;32mRunning all tests\033[0;0m"
	@RUN_ALL_TESTS=true $(R) -q --slave -e 'lf<-list.files("${PACKAGE}/tests","\\.R$$",full.names=TRUE); for(f in lf) source(f, chdir = TRUE, print.eval = TRUE )'

testquick:
	@echo "\033[0;32mRunning almost all tests\033[0;0m"
	@RUN_ALL_TESTS=false $(R) -q --slave -e 'lf<-list.files("${PACKAGE}/tests","\\.R$$",full.names=TRUE); for(f in lf) source(f, chdir = TRUE, print.eval = TRUE )'

coverage:
	@echo "\033[0;32mChecking code coverage for all tests\033[0;0m"
	@RUN_ALL_TESTS=true $(R) --slave -e 'aa<-covr::package_coverage("${PACKAGE}"); covr::report(aa,file="${PACKAGE}-report.html",browse = FALSE); aa'

coveragequick:
	@echo "\033[0;32mChecking code coverage for quick tests\033[0;0m"
	@RUN_ALL_TESTS=false $(R) --slave -e 'aa<-covr::package_coverage("${PACKAGE}"); covr::report(aa,file="${PACKAGE}-report.html",browse = FALSE); aa'

clean:
	@echo "\033[0;32mCleaning directory\033[0;0m"
	git clean -f -d

uninstall:
	@echo "\033[0;32mUninstalling package\033[0;0m"
	$(R) CMD REMOVE ${PACKAGE}
