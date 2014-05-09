# tulip

tulip is an R package of Poiscon consulting's UTILity Functions. tulip
is an anagram of PUTIL. Its purpose is to provide basic functions for other
packages.

## Installation

To install (and load) the latest version:

    # install.packages("devtools")
    library(devtools)
    install_github("poissonconsulting/tulip")
    library(tulip)
    
## Contact

You are welcome to:

* submit suggestions and bug-reports at: https://github.com/poissonconsulting/tulip/issues
* send a pull request on: https://github.com/poissonconsulting/tulip/

## Versioning

Releases are numbered with the following semantic versioning format:

<major>.<minor>.<patch>

And constructed with the following guidelines:

- breaking backward compatibility bumps the major (and resets the minor and patch)
- new additions without breaking backward compatibility bumps the minor (and resets the patch)
- bug fixes and misc changes bump the patch
