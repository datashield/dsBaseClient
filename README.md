dsBaseClient
============

DataSHIELD client side base R library.




| Branch   | dsBase status | dsBaseClient status | dsBaseClient tests |
| -------- | ------------  | ------------------- | ------------------ |
| Master   | [![Build Status](https://dev.azure.com/datashield-testing/datashield/_apis/build/status/datashield.dsBase?branchName=master)](https://dev.azure.com/datashield-testing/datashield/_build/latest?definitionId=1&branchName=master) | [![Build Status](https://dev.azure.com/datashield-testing/datashield/_apis/build/status/datashield.dsBaseClient?branchName=master)](https://dev.azure.com/datashield-testing/datashield/_build/latest?definitionId=1&branchName=master) | [Tests](https://datashield.github.io/testStatus/dsBaseClient/master/latest/) |
| v6.0-dev | [![Build Status](https://dev.azure.com/datashield-testing/datashield/_apis/build/status/datashield.dsBase?branchName=v6.0-dev)](https://dev.azure.com/datashield-testing/datashield/_build/latest?definitionId=1&branchName=v6.0-dev) | [![Build Status](https://dev.azure.com/datashield-testing/datashield/_apis/build/status/datashield.dsBaseClient?branchName=v6.0-dev)](https://dev.azure.com/datashield-testing/datashield/_build/latest?definitionId=1&branchName=v6.0-dev) | [Tests](https://datashield.github.io/testStatus/dsBaseClient/v6.0-dev/latest/) |
| v6.0.1-dev | [![Build Status](https://dev.azure.com/datashield-testing/datashield/_apis/build/status/datashield.dsBase?branchName=v6.0.1-dev)](https://dev.azure.com/datashield-testing/datashield/_build/latest?definitionId=1&branchName=v6.0.1-dev) | [![Build Status](https://dev.azure.com/datashield-testing/datashield/_apis/build/status/datashield.dsBaseClient?branchName=v6.0.1-dev)](https://dev.azure.com/datashield-testing/datashield/_build/latest?definitionId=1&branchName=v6.0.1-dev) | [Tests](https://datashield.github.io/testStatus/dsBaseClient/v6.0.1-dev/latest/) |
| v6.1-dev | [![Build Status](https://dev.azure.com/datashield-testing/datashield/_apis/build/status/datashield.dsBase?branchName=v6.1-dev)](https://dev.azure.com/datashield-testing/datashield/_build/latest?definitionId=1&branchName=v6.1-dev) | [![Build Status](https://dev.azure.com/datashield-testing/datashield/_apis/build/status/datashield.dsBaseClient?branchName=v6.1-dev)](https://dev.azure.com/datashield-testing/datashield/_build/latest?definitionId=1&branchName=v6.1-dev) | [Tests](https://datashield.github.io/testStatus/dsBaseClient/v6.1-dev/latest/) |


[![License](https://img.shields.io/badge/license-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)




About
=====

DataSHIELD is a software package which allows you to do non-disclosive federated analysis on sensitive data. Our website (https://www.datashield.ac.uk) has in depth descriptions of what it is, how it works and how to install it. A key point to highlight is that DataSHIELD has a client-server infrastructure, so the dsBase package (https://github.com/datashield/dsBase) needs to be used in conjuction with the dsBaseClient package (https://github.com/datashield/dsBaseClient) - trying to use one without the other makes no sense.

Detailed instructions on how to install DataSHIELD are at https://www.datashield.ac.uk/wiki. The code here is organised as:


| Location                     | What is it? |
| ---------------------------- | ------------| 
| obiba CRAN                   | Where you probably should install DataSHIELD from. |
| releases                     | Stable releases. |
| master branch                | Mostly in sync with the latest release, changes rarely. |
| v6.0-dev branch (or similar) | Bleeding edge development. We make no claims that this branch is fully functional at any given time. |
