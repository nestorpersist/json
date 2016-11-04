Persist-Json is a new fast Json parser written in Scala.
It includes:

   1. A standard Scala tree form of Json based on Scala immutable collections.
   2. A very fast Json parser that takes a Json string and produces the Scala form.
   3. A very fast Compact unparser that takes the Scala form and produces a compact single-line Json string.
   4. A Pretty unparser that takes the Scala form and produces a nicely formatted Json string.
   5. A set of methods for working with the Scala form that augment the existing standard collection methods.
   6. A mapper that converts between the Scala form and user-defined classes.

## Documentation
1. [ScalaDoc](http://nestorpersist.github.com/json)

## Reference

Persist-Json can be referenced in sbt from Maven Central as

    "com.persist" % "persist-json_2.11" % "1.1.3"

## History
This parser started as part of the OStore NoSQL database
project.

[OStore](https://github.com/nestorpersist/ostore)

OStore makes extensive use of Json and needed a pure-Scala
Json parser whose output would be immutable Scala data.
For this purpose, the Twitter parser based on the parsing
combinator example in the Odersky book was choosen.

[Twitter Json](https://github.com/stevej/scala-json)

Although it met the functional need, it was much too slow.
The OStore Json parser mostly maintains the API of the 
Twitter Json parser but is very much faster.

Since the OStore Json parser has utility beyond its use in
OStore, it was broken off as this separate Persist-Json GitHub project.

## Thanks

YourKit is kindly supporting the Persist-Json open source project with its full-featured Java Profiler.
YourKit, LLC is the creator of innovative and intelligent tools for profiling
Java and .NET applications. Take a look at YourKit's leading software products:
[YourKit Java Profiler](http://www.yourkit.com/java/profiler/index.jsp) and
[YourKit .NET Profiler](http://www.yourkit.com/.net/profiler/index.jsp).


