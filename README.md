<img src="www/IDEA_logo_500.png"  height="120"/>

# Mortality Minder

## About
**Mortality Minder** is a web application built on the [R Shiny](https://shiny.rstudio.com/) application framework, written primarily in R, with some functionality written in javascript. It creates highly visual analysis of mortality data provided by [CDCWonder](https://wonder.cdc.gov/) and [County Health Rankings](http://www.countyhealthrankings.org/). You may find a live version of the app here: https://lp01.idea.rpi.edu/shiny/erickj4/MortalityMinder/

## Local Setup
To run a local instance of the application, follow the steps below:

1. Clone the repository to your local setup.
2. Open RStudio and navigate to your local repository location in the RStudio files tab.
3. Open app.R and select `Run App` on the top-right corner of the code window.

## Reference
If you use **Mortality Minder** in your work, please use the following reference

```
Bennett, Kristin P, and Erickson S John. “Mortality Minder.” Rensselaer Institute for 
Data Exploration and Applications, 1 Nov. 2019, https://lp01.idea.rpi.edu/shiny/erickj4/MortalityMinder/.
```

## Development
The **Mortality Minder** app is in active development and undergoes daily updates. Please be sure to consistently pull from master in your local setup. If you notice any bugs, create an issue. Please ensure issues have a descriptive title, and the comment should describe where the bug was found and how you were using the application. 

For all contributors, **any** changes made to the application should be written in a separate branch, and sent as a pull request. Please refrain from pushing changes directly to master, as we must ensure a working copy of the app at all times. When in doubt, follow this protocol:

1. Clone the repo...
2. cd into the resulting, new directory...
3. git checkout -b new_branch and git push origin (to claim their branch)
4. Code, code, code...
5. git commit -a -m "My awesome changes"
6. git commit -a -m "More awesome changes"
7. While still in your branch, do a git pull (only if others are also working on the branch)
8. Switch to master with git checkout master
9. Sync your master using git pull  <<< This is the important step!
10. Now switch back to your branch with git checkout new_branch
11. Now git commit -a -m "Final awesome changes" and git push origin
12. Now you can do a PR, which should be safe from conflict.


