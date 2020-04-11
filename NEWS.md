# como 11.12.1.9000

1. Added a `NEWS.md` file to track changes to the package.

2. Re-structured DESCRIPTION

    * Used Authors@R format for identifying authors and maintainers. This format is more appropriate in being able to identify roles of the person/s involved such as author (aut), contributor (ctb), maintainer (cre), copyright holder (cph), etc. See example in *Writing R Extensions* under the section for [the DESCRIPTION file](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#The-DESCRIPTION-file).

    * Re-aligned the description section for the package to keep to 80 spaces width

    * Re-formatted dependencies declaration. Before, single declaration using *Depends* which included version of R and then the long list of package dependencies. Current, *Depends* used only to declare dependency on a minimum R version. Package dependencies are now declared separately based on type of dependency i.e. *Imports* or *Suggests* (see *Writing R Extensions* under the section for [Package Dependencies](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-Dependencies) for further information on different dependency types). Based on how the CoMo app is structured at the moment, the only function in the package is the function to run/open the Shiny CoMo app which uses a dependency on the `shiny` package. So, this has been classified as an *Imports* type. All other packages have been organised under *Suggests* type. On installation of package using `remotes::install_github()` and specifying `dependencies = TRUE`, all packages identified as *Imports* and as *Suggests* will be installed along with the CoMo App package. This will potentially solve some of the installation issues and will negate the need to do the steps specified in the current README of installing `pacman` and installing all packages separately before installing the app.

3. Added to .Rbuildignore all documents and others in root directory of package that is not used for building the package

    * When package is built, it requires specific files and folders present in the root directory of the package. Any unknown file or document found will cause a package build error. These unknown files have been ignored in the build by including them in the .Rbuildignore file. These files/documents are:

        * `docs` folder

        * `.Rproj` file created when an RStudio project is initiated

        * `.Rproj.user` file is a hidden file idenitying the RStudio user

        * `data-raw` folder (see number 6 below for more details about this folder)

4. Added an R script that will produce package documentation

    * created como.R inside the R directory which contains package description that would be read on package build and will be produced when creating help pages when installed by user.

    * This R script will also be the location to declare and specify functions that have been imported from external packages. Here I added `@importFrom shiny runApp` which signifies that this package uses the `runApp()` function from Shiny.

5. Minor addition and edition to the `comomodel()` function

    * Added examples - required for building a package and for routine R CMD checks

    * Removed @import specification - this is already addressed by the Imports specification in DESCRIPTION

    * Added annotation for line of code that runs the Shiny app

6. Created a data processing folder `data-raw` and moved the raw data and data processing scripts from `data` to `data-raw`

    The standard practice when creating R packages is to create a separate folder or directory for data processing as the `data` folder is reserved for the outputs of the data processing which are in `.rda` format. These are datasets that are installed alongside the package and is documented accordingly.

7. Edited the `prepare_data.R` script in the `data-raw` directory

    * Removed first line that sets the working directory. This is not good practice when developing a script in general because it makes your code not replicable by everyone. When `setwd()` is used, you set the directory to your specific directory structure which is not always the case for any other person who will be engaging with your code/script. Best example is this when I tried to build the package and run an `R CMD check`, it failed automatically because of the `setwd()`.

    * Added general annotations

    * Added correct file paths for raw data during processing.

    * Created `.rda` file formats for each of the datasets processed (see next point)

8. Created `.rda` dataset/s from raw data and saved them into `data` directory

    The approach used by author for data can be tricky when packaging an app as the data is not portable. The new approach used here where the datasets are included into the R package means that the datasets are installed in a way that is consistent with the R architecture and is highly portable. When package is installed, the datasets are installed lazily and are loaded into the RAM when needed. This can have knock-on effects with regard to the Shiny app loading at the start.

9. Created a data documentation script in R directory called `data.R`

    When .rda files are generated and stored in the R package, the data needs to be documented. This documentation is similar to package documentation using `Roxygen`. See `data.R` in R directory for syntax.

    Documenation is outputed in the `man` directory alongside documentation of function/s.

10. Added continuous integration and continuous development (CI/CD) for package unit testing

    * Key issues so far has been problematic installs of the package by other users which can be problem when they are unable to access the application online

    * CI/CD allows for frequent and incremental and unit testing on various operating software (Ubuntu, macOS, Windows) and with various versions of R everytime changes to the package are made. This tests whether installation of the package will succeed or not in these different setups. This also allows author/s to trigger an error that users might report and replicate the issue, diagnose and solve

    * Added Travis CI/CD to test Ubuntu/Linus platforms

    * Added Appveyor CI/CD to test Windows platforms

    * To add macOS CI/CD via GitHub Actions (this developer uses macOS so testing done locally)

11. Edited the README.md

    * Added badges for CI/CD for Travis and Appveyor so that we are alerted when there are issues iwth package builds

    * Added lifecycle badge to identify level of development of current package and application (set to `Maturing`)

    * Moved badges below the README header (this is the typical location for badges)

    * Edited section on install offline app

12. Added package vignettes

    Package vignettes are guide documents that come with package installation. This can serve as a way for users to be provided with help on the basis of the modelling done and on how to use the app. Placeholder vignettes have been added to show how this can be implemented.

13. Re-structured the `docs` directory 

    * Removed CNAME (this can be activitated again within GitHub repository such that the repository pages can be pointed to comomodel.net)

    * Removed index.html (this is replaced by the package documentation website)

    * Added a yaml file called _pkgdown.yml in the inst/ directory to provide a template for the package website.

    * generated a package website using `pkgdown` package.

    The `docs` directory has been re-structured such that a package specific website can be (has been) produced for the `como` R package which can show documentation of the package and the datasets included in the package that are used by the Shiny app.

     In this website, a site link to the online app is included. This presentation may lend itself to more transparent documentation and more detailed guides etc for application users that doesn't have to be added to the application itself.
