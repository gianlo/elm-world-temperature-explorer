World Temperature Explorer
--------------------------

This is an `elm` demo project.
It implements a viewer of time series from the [climatedataapi](https://datahelpdesk.worldbank.org/knowledgebase/articles/902061-climate-data-api) from `worldbank.org`.

The user is presented with yearly temperature average line charts for a bunch world countries from 1900 to today.

The frontend is developed both in plain elm (see the `Main` module that uses the `HtmlView`) and using `elm-ui` (see the `ElmUiView` module).

Both implementations provide the user with the facility of:
- zooming on the time axis (_from_ and _to_ year range)
- selecting and deselecting what series are displayed
- removing time series data for a nation
- adding a time series for a nation yet not displayed


How to run
----------

- Install elm `0.19`
- For the plain `elm` implementation run: `elm make src/Main.elm`
- For the `elm-ui` implementation run: `elm make src/ElmUiView.elm` 
- View the `index.html` produced by the compiler in a browser


Limitations
-----------

The plain `elm` implementation does not style the html produced.

References
----------

[Elm language](https://elm-lang.org/)

[Elm ui](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/)

[Elm line charts](https://package.elm-lang.org/packages/terezka/line-charts/latest/)

[World Bank: Climate Data API](https://datahelpdesk.worldbank.org/knowledgebase/articles/902061-climate-data-api)
