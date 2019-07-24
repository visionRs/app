# app
This Repo contains R code for the app

### Directory Structure
```bash
+-- _app
    |__ui.R
    |__server.R
|__archive
    |__00_Header.R
    |__01_Sidebar.R
    |__02_Body.R
    |__app.R (old app version)
    |__app_old.R (old app version)
|__plots
    |__00_Plots-Bar.R
    |__01_Plots-Scatter.R
    |__02_Plots-Line.R
|__www
    |__template.css
    
    
    ```bash
├── app
│   ├── css
│   │   ├── **/*.css
│   ├── favicon.ico
│   ├── images
│   ├── index.html
│   ├── js
│   │   ├── **/*.js
│   └── partials/template
├── dist (or build)
├── node_modules
├── bower_components (if using bower)
├── test
├── Gruntfile.js/gulpfile.js
├── README.md
├── package.json
├── bower.json (if using bower)
└── .gitignore
```
  
  ```

### How to run the app
`runApp("app")`
