_An even simpler string templating engine._

A string template engine is a practical function that transforms a string with blanks _(called,
historically, "format string")_ and a collection of values into a human readable string with the
blanks filled:

    f :: FormatString -> Some xs -> string

There are a number of string templating engines in Haskell. To name a few:

* `fmt`
* `text-format`
* `text-format-simple`
* `formatting`

Even more so in other widely used languages:

* `int printf(const char *format, ...)` in C.
* `str.format(*args, **kwargs)` in Python.

This package offers a template engine that is concise and straightforward in both reading and
writing, most similar to Python's `format)` method.

One particular feature of this engine is that the format string is parsed at compile time via
Template Haskell. This ensures type safety in that the correspondence of blanks to values is
enforced by the type system. At the same time, the collection of values may simply be presented as
any number of arguments, without cluttering with any collection notation --- that is, our engine,
much like the `printf` of C and `format` of Python, is polyary.

We offer two consumer packages that share the logic but offer `String` and `Text` interface
separately. The target type is a technical detail; in text, we refer to human readable strings of
any specific type simply as "strings".

There is a small, closed selection of blanks you may use in a format string:

* The simplest is `{}`, ...
* `{0}` through `{n}`
* Specialized and efficient variants of `[]` and `[n]` that do not apply `show`.

We do not offer a mini-language of format specifiers and modifiers, as some other template engines
do, because we believe it to be against clarity and simplicity. If you wish to display a certain
type in a specific fashion _(say, as a hexadecimal number)_, you may either transform it to a
string yourself, or create a `newtype` with a custom `Show` instance and enjoy automagical
conversion.
