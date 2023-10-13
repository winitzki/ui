# Declarative GUI for desktop apps in Scala

The GUI is implemented using the [Elm architecture](https://elmbridge.github.io/curriculum/The%20Elm%20Architecture.html).

The code is decomposed into independently implemented parts:

- The `View[E]` type constructor and its subtypes: `Label`, `Button`, `TileV`, `TileH`. This is pure data implemented via case classes.
- The Elm program type (`Elm.Program`). This is a value supplied by the user. This value describes the entire GUI declaratively. This value contains some functions but those functions are pure and have no side effects.
- A graphical rendering backend (`Elm.UiBackend`). This trait needs to be implemented separately for a chosen graphical toolkit (AWT, Swing, etc.). This trait contains functions for rendering a view and for processing events that come from external effects.
- The external commands type `C[E]` and the external subscriptions type `S[E]`. These types may be defined arbitrarily by the user, as long as the corresponding runners are provided (`Elm.EffectRunner`). Both commands and subscriptions are external effects that may generate one or more events. The difference between commands and subscriptions is that subscriptions can be canceled but commands cannot be. A runner for commands is `runCommand: C[E] => (E => Unit) => Unit`. A runner for subscriptions is `listen: S[E] => (E => Unit) => Unit => Unit`.
- The runloop is implemented as a class `Elm.RunLoop`. It can run an `Elm.Program` using a given rendering backend and an effect runner.

All functions (except the actual user-defined code) are parameterized by the type parameters `E` and `M`.
In addition, the runloop is parameterized by the view type constructor `V[_\` and the external effects constructors.

This makes the architecture flexible:

- An Elm program is pure business logic, without any details about the graphical backend or the implementation of external effects.
- An Elm program is a pure value that can be serialized (as long as the serialization format supports functions). One promising possibility is to serialize Elm programs as [dhall](https://dhall-lang.org/) values.
- The same Elm program can be run with any graphical backend and with different external effect runners. (For example, if an external effect is a timer, a runner could speed up or slow down the clock for debugging purposes.)
- External effect runners are implemented completely independently of the chosen graphical backend.

# Future plans

- Some proof-of-concept code that runs. (Done, with a very simple layout and a Java AWT-based backend.)
- View model needs to have some commonly used widgets and layouts. Use Elm-UI, Elm-Components, etc., as inspiration. (Drop-down list box with search field.)
- Cassowary layout constraints or other layout patterns. See, for example, [kiwi-java](https://github.com/alexbirkett/kiwi-java) or Apple's `AutoLayout`.
- Rendering backends: Java AWT, Java Swing, JavaFX, [Jexer](https://gitlab.com/AutumnMeowMeow/jexer), [lanterna](https://github.com/mabe02/lanterna), [tui-scala](https://github.com/oyvindberg/tui-scala).
- Streamline and simplify the Scala types and interfaces. For clarity, consider using traits with named methods instead of bare curried functions. (Done.)
- Implement a modular architecture where we can combine Elm programs and/or runloops together. Use partial functions to glue event types.
- Implement a number of "standard" cases for external effects, such as timers, HTTP requests, random numbers, audio inputs.
- Implement mutable views with optimized rendering. Use unique object IDs created at compile time to minimize mutation.
- Implement an analog of "Elm ports" for arbitrary external effects.
- Cross-compile Elm programs to Scala values of type `Elm.Program`. Import Elm libraries as a Scala dependency.
- Verify that the Scala code is purely functional and export it into lambda-terms for run-time introspection, or to Elm, or to dhall.
- A protocol for safe remote GUI execution based on Elm program values.
- Advanced features: drag-and-drop, multitouch, multiple windows, modal dialogs, vector graphics, raster graphics, animations, Web Sockets, database data sources for on-demand lists or tables, infinite on-demand image loading, pixel density support, screenshot export to PDF, self-editing GUI.

# Miscellaneous notes

- `java.awt.Panel` is "heavy" because it contains an opaque native window behind it. Instead, we could use `java.awt.Container` or `java.awt.Component` and extend those interfaces, perhaps with better performance. https://web.archive.org/web/20000829121830/http://java.sun.com/products/jdk/1.1/docs/guide/awt/designspec/lightweights.html

- The `View` type constructor may need a second type parameter to describe nested subviews. However, we need to limit ourselves to just one extra type parameter.

- Need to verify that GUI operations are only performed on the GUI event thread. (Done.)

- Figure out whether we really need both Subscriptions and Commands, and whether we should have a function of type `E => M => M` or `M => E => M`, and `E => M => C[E]` or `M => E => C[E]`. Probably better to have `M => E =>` because then we can have a partial function of `E` that depends on `M`.
