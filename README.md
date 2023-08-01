# Declarative GUI for desktop apps in Scala

The GUI is implemented using the [Elm architecture](https://elmbridge.github.io/curriculum/The%20Elm%20Architecture.html).

In addition, the code is decomposed into independently implemented parts:

- The `View[E]` type constructor and its subtypes: `Label`, `Button`, `TileV`, `TileH`.
- The Elm program type (`Elm.Program`). This is a value supplied by the user. This value describes the entire GUI declaratively.
- The graphical rendering backend. This is a function of type `View[E] => Future[E]`. This returns a `Future` value that completes when the next user-generated event becomes available. (There is only one user-generated next event.)
- The external commands type `C[E]` and the external subscription type `S[E]`. These types may be defined arbitrarily by the user, as long as the corresponding runners are provided. The runners are functions of type `C[E] => CancellableStream[E]` and `S[E] => CancellableStream[E]`.
- The `runloop` function that runs an `Elm.Program` using a given rendering backend and external runners.

All functions (except the actual user-defined code) are parameterized by the type parameters `E` and `M`.
In addition, the runloop is parameterized by the view type constructor and the external effects constructors.
This meakes the architecture completely flexible. The same Elm program can be run in any environment.

# Future plans

- Some proof-of-concept code that runs. (Done, with a very simple layout and a Java AWT-based backend.)
- View model needs to have some commonly used widgets and layouts. Use Elm-UI, Elm-Components, etc., as inspiration. (Drop-down list box with search field.)
- Cassowary layout constraints or other layout patterns. See, for example, [kiwi-java](https://github.com/alexbirkett/kiwi-java) or Apple's `AutoLayout`.
- Rendering backends: Java AWT, Java Swing, JavaFX, [Jexer](https://gitlab.com/AutumnMeowMeow/jexer), [lanterna](https://github.com/mabe02/lanterna), [tui-scala](https://github.com/oyvindberg/tui-scala).
- Streamline and simplify the Scala types and interfaces. For clarity, consider using traits with named methods instead of bare curried functions.
- Implement a modular architecture where we can combine Elm programs and/or runloops together. Use partial functions to glue event types.
- Implement a number of "standard" cases for external effects, such as timers and HTTP.
- Implement mutable views with optimized rendering. Use unique object IDs created at compile time to minimize mutation.
- Implement an analog of "Elm ports" for external effects.
- Cross-compile Elm programs to Scala values of type `Elm.Program`. Import Elm library as a Scala dependency.
- Verify that the Scala code is purely functional and export it into lambda-terms for run-time introspection or to Elm or to Dhall.
- A protocol for safe remote GUI execution.
- Advanced features: drag-and-drop, multiple windows, modal dialogs, vector graphics, animations, Web Sockets, database data sources for on-demand lists or tables, infinite on-demand image loading, pixel density, PDF screenshot export, self-editing GUI.
