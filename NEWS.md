# milQuant 2.0.3 _2024-..._


# milQuant 2.0.2 _2024-10-19_

### Minor changes and fixes
* Fix bug in Workflow-tab where layer selection was not applied
* Display number of resources to be loaded in workflow tab already selected by layer

# milQuant 2.0.1 _2024-03-04_
* The custom colour pallet for periods (also used in the [Map of Miletus](https://geoserver.dainst.org/catalogue/#/map/5764)) is now used in the plots that can display periods. 
* The tables displayed when clicking a bar can be downloaded. 

### Minor changes
* No more forced titles for plots.
* The settings used to connect can now be saved as defaults directly. 
* Search result is highlighted in the Fulltext Search table.

# milQuant 2.0.0 _2023-12-17_
* Restructure the app as a package.
* Generic Bar Chart and Aoristic Density graphs are now included instead of split tabs linked in the sidebar. 
* The new Search-tab lets users perform fulltext-searches on any text input field. 
* There are now tabs to display Plaster and Mollusk Quantifications.
* Place and Operations are now selected in the Home-Tab, and a Popover in the sidebar informs users of the selection.
* The Home-Tab now contains the Version history and better information / guides on the App, as well as the DB-Overview, which is now a tabpanel here. 
* Resources in the workflow-tab have to be manually loaded with Layer and Category selection to reduce loading time. 
* Some buttons and options now feature informative tooltips.

### Smaller fixes
* Variables for plots are preselected by inputType.
* If the selected Place only contains one Operation, it is automatically selected. 
* Better binning of bars and display of axis in the Activity-tab.



# milQuant 1.1.0 _2023-11-23_
* Added a button that refreshes the index, so that newly added resources can be seen as well.
* Added the 'Activity' tab which shows latest and overall changes to the database by *Place* and user.
* All buttons are now disabled while shiny is busy to avoid unnecessary queries.
* Moved the database query to each tab. Now only the data that is needed in each tab is queried from Field Desktop, hopefully reducing overall loading time and memory usage.
* Redesigned the *overview*-plot in Plotly. It now displays the complete project database with the exception of *Type*- and *Image*-categories.
* Added many tabs for different *Find*-categories using generalized *barplot*-module (Plotly).
* Modularized all other tabs. 
* From now on, (almost) all plots are produced in Plotly directly to make better use of Plotlys features.
* Corrected typos, minor text and made some layout changes.


# milQuant 1.0.3 _2023-10-30_
* uses [idaifieldR 0.3.2](https://github.com/lsteinmann/idaifieldR/releases/tag/v0.3.2)
* change the plot download buttons and handlers to a module: fixes non-reactive plot download

# milQuant 1.0.2 _skipped/internal_
* use [idaifieldR 0.3.1](https://github.com/lsteinmann/idaifieldR/releases/tag/v0.3.1)


# milQuant 1.0.1 _2023-03-05_
* the connection settings (name/pw) are reusable (with milQuant-dist)
* improved plot legends and added bar plot display options
* fixed period order in pottery & quant B plots
* fixed issue with dates
* use plotly for all graphs
* add a workflow overview
* add object table to pottery plot
* project, operation and trench(es) are saved and restored on exit by button

# milQuant 1.0.0 _2023-02-21_
* first "release" (version number changed to be same as the standalone version)
* no more fluidPage()
* fixed if-condition for trench selection

# milQuant 0.2.3 _2023-02-18_

* use idaifieldR v0.2.2 (fixing a problem where with an if-condition)
* rudimentary form for bricks and brick quantification added
* fix downloading plot in Finds, Loomweights and Bricks
* fix error handling in connection (now actually displays the error)
* proper label in period selector
* custom titles and subtitles for all plots
* add logo/icon and custom spinner
* restructure directories and connect/home/overview pages

# milQuant 0.2.1 _2022-05-15_

* uses idaifieldR v0.2.1
* WIP

# milQuant 0.2.0 _2022-05-15_

* First executable version
* login screen to change connection details (ip, username, password) to suit different field clients and projects
* contains plot generation and download for: find overview, pottery (single), pottery quantification forms A & B from the milt-configuration, loomweight (milet configuration)
* uses idaifieldR v0.2
* not perfect, but currently working for miletus
