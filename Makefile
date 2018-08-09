R?=R
PACKAGE=argosTrack
VERSION = $(shell Rscript -e "l<-readLines(\"${PACKAGE}/DESCRIPTION\");cat(gsub(\"Version: \",\"\",l[grepl(\"Version: \",l)]))")

TARBALL := ${PACKAGE}_${VERSION}.tar.gz
RFILES := $(wildcard ${PACKAGE}/R/*.R)
NAMESPACEFILE := ${PACKAGE}/NAMESPACE
RCHECK := ${PACKAGE}.Rcheck


$(NAMESPACEFILE): $(RFILES)
	@echo "\033[0;32mUpdating documentation\033[0;0m"
	rm -f ${PACKAGE}/src/*.so
	$(R) -q -e 'devtools::document("${PACKAGE}")'
	@touch $@

$(TARBALL): $(NAMESPACEFILE)
	@echo "\033[0;32mBuilding package\033[0;0m"
	$(R) CMD build ${PACKAGE}

build: $(TARBALL)

$(RCHECK): $(TARBALL)
	@echo "\033[0;32mChecking package as cran\033[0;0m"
	$(R) CMD check --as-cran $(TARBALL)

check: $(RCHECK)

checkbc: $(TARBALL)
	@echo "\033[0;32mChecking package with bioconductor\033[0;0m"
	$(R) CMD BiocCheck --no-check-bioc-views $(TARBALL)

install: $(TARBALL)
	@echo "\033[0;32mInstalling package\033[0;0m"
	$(R) CMD INSTALL $(TARBALL)

install_dependencies:
	@echo "\033[0;32mInstalling package dependencies\033[0;0m"
	@echo "source('https://raw.githubusercontent.com/calbertsen/caMisc/master/caMisc/R/build_from_github.R'); \
	installDependencies('${PACKAGE}/DESCRIPTION',dependencies=c(\"Depends\",\"LinkingTo\",\"Imports\",\"Suggests\",\"Enhances\"))" | $(R) -q --slave

install_bc:
	@echo "\033[0;32mInstalling BiocCheck\033[0;0m"
	$(R) -q -e 'if (!"BiocManager" %in% rownames(installed.packages())) install.packages("BiocManager"); BiocManager::install("BiocCheck")'

test: $(TARBALL)
	@echo "\033[0;32mRunning all tests\033[0;0m"
	@RUN_ALL_TESTS=true RUNTEST_PKG='../../${TARBALL}' $(R) -q --slave -e 'lf<-list.files("${PACKAGE}/tests","\\.R$$",full.names=TRUE); for(f in lf) source(f, chdir = TRUE, print.eval = TRUE )'

test_installed:
	@echo "\033[0;32mRunning all tests\033[0;0m"
	@RUN_ALL_TESTS=true RUNTEST_PKG=${PACKAGE} $(R) -q --slave -e 'lf<-list.files("${PACKAGE}/tests","\\.R$$",full.names=TRUE); for(f in lf) source(f, chdir = TRUE, print.eval = TRUE )'


testquick: $(TARBALL)
	@echo "\033[0;32mRunning almost all tests\033[0;0m"
	@RUN_ALL_TESTS=false RUNTEST_PKG=../../$(TARBALL) $(R) -q --slave -e 'lf<-list.files("${PACKAGE}/tests","\\.R$$",full.names=TRUE); for(f in lf) source(f, chdir = TRUE, print.eval = TRUE )'

testquick_installed:
	@echo "\033[0;32mRunning almost all tests\033[0;0m"
	@RUN_ALL_TESTS=false RUNTEST_PKG=argosTrack $(R) -q --slave -e 'lf<-list.files("${PACKAGE}/tests","\\.R$$",full.names=TRUE); for(f in lf) source(f, chdir = TRUE, print.eval = TRUE )'

COVRALL := ${PACKAGE}-report.html
$(COVRALL): $(RFILES)
	@echo "\033[0;32mChecking code coverage for all tests\033[0;0m"
	@RUN_ALL_TESTS=true $(R) --slave -e 'aa<-covr::package_coverage("${PACKAGE}"); covr::report(aa,file="${PACKAGE}-report.html",browse = FALSE); aa'

coverage: $(COVRALL)

COVRQUICK := ${PACKAGE}-report-quick.html
$(COVRQUICK): $(RFILES)
	@echo "\033[0;32mChecking code coverage for quick tests\033[0;0m"
	@RUN_ALL_TESTS=false $(R) --slave -e 'aa<-covr::package_coverage("${PACKAGE}"); covr::report(aa,file="${PACKAGE}-report-quick.html",browse = FALSE); aa'

coveragequick: $(COVRQUICK)

clean:
	@echo "\033[0;32mCleaning directory\033[0;0m"
	git clean -f -d

uninstall:
	@echo "\033[0;32mUninstalling package\033[0;0m"
	$(R) CMD REMOVE ${PACKAGE}
