pathological
============

Scala's missing IO Library. The aim of this library is to do as little work as possible wrapping existing java io functionality and providing a pleasant, high-level interface for reading and writing data from file (instead of just strings.)

Status
------

At the moment this is definitely more vision than reality. There exists a mess with a fair amount of implementation on the path DSL and the first work towards the reader end of things in the `so.modernized.pathological` namespaces, and the sketch of a cleaner, clearer implementation in the `so.modernized.and.pathological` namespace, where I'd like most further work to stay. At the moment the interface that I have in mind for readers is something like this:

```scala
import so.modernized.and.pathological.Pathological._ // this should only really expose the 'read' and 'write' methods, along with the implicit decorator methods needed to complete things

implicit val format = DefaultFormatter // provides methods to go from String => A for some set of As

read("~/data/sentences/train*.tsv") // reads in all files in the directory matching glob
  .linewise // returns an iterator over the lines of all the files
  .asTSV // splits each line on tab, and returns an iterator of Seqs
  .into[String, Int, Int, Double] // returns an Iterator[(String, Int, Int, Double)]
```

At the moment some of these intermediate stages return wrapper objects with the methods in question, I'm willing to be persuaded about whether or not these should be wrappers or implicit methods on the underlying classes. Currently the into methods need to be written manually, and ideally this code would be generated up to arity 22 (to match scala's tuple implementation). Also, the code for case classes (or really any classes that can be fully characterized by their longest constructor) is in `so.modernized.pathological` but not `so.modernized.and.pathologcal`.


Plans and TODOs
---------------

Everything after this point is still a todo.

- [ ] macro code to generate intos of different arities
- [ ] into method for subtypes of Product
- [ ] integrate path code and filesystem code from old namespace
- [ ] integrate case class code from old namespace
- [ ] think about and work up implementation for writing
