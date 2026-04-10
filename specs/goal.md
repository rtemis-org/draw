# rtemis.draw Plan

- Define S7 classes to represent all necessary echarts structures, with type checking and validation of inputs
- Every class should serialize to a plain named list that matches ECharts’ option structure
- Create Theme abstract class, with Light and Dark themes as concrete implementations. More themes will be added later 
- Use `jsonlite` to convert those lists to JSON, and `htmlwidgets` to render the visualizations
- Create plain functions for the user-facing API, which create the necessary S7 objects and call the appropriate methods to generate the visualizations
- User-facing create_theme() function can create new subclasses of Theme on the fly
- Use latest version of all packages involved
