World Temperature Explorer
--------------------------

This is an `elm` demo project.
It implements a viewer of time series from the [climate data API](https://datahelpdesk.worldbank.org/knowledgebase/articles/902061-climate-data-api) from `worldbank.org`.

The user is presented with yearly temperature average line charts for a bunch of countries from 1900 to today.

The frontend is developed both in plain elm (see the `Main` module that uses the `HtmlView` module) and using `elm-ui` (see the `ElmUiView` module).

Both implementations provide the user with the ability to:
- zoom on the time axis (_from_ and _to_ year range)
- select what series are displayed on the graph
- remove the time series for a country
- add the time series for a country not yet displayed



How to run
----------

- Install elm `0.19.1`
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
