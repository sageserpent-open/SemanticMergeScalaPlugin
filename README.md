# Neptunium - the Semantic Merge Scala Plugin
## What?

A plugin that integrates with:-
 1. the version control system *Plastic SCM*.
 1. the standalone merge tool *Semantic Merge*.
 1. the Git front-end *GMaster*.

It adds support for syntax-aware structured merging of **Scala** code.

It is MIT licensed - although the products mentioned above have their own licenses completely independent of this plugin.

## How?

You build the plugin from source and configure Plastic SCM / Semantic Merge / GMaster to pick it up. It's hardly shrinkwrapped, but it's not hard to do either.

So, you're a Scala developer if you want to use this plugin, aren't you? You know about Git because you are here on Github?

Right, so clone this repository and fire up SBT with the task `assembly`. That will generate a file `neptunium.jar` in the `target\scala-2.12` directory of the SBT build. That file is the built plugin.

Next, integrate this into Plastic SCM / Semantic Merge / GMaster - the official documentation is here: https://users.semanticmerge.com/documentation/external-parsers/external-parsers-guide.shtml and an older blog post is here: http://codicesoftware.blogspot.com/2015/09/custom-languages-in-semantic-version.html.

Specifically for this plugin, locate the file `externalparsers.conf` in your appdata directory - eg: `C:\Users\<you>\AppData\Local\plastic4\externalparsers.conf`.

For GMaster, the file path is slightly different: `C:\Users\<you>\AppData\Local\gmaster\config\externalparsers.conf`.

Edit it to point to `neptunium.jar`, like this:-

~~~~
.sc=C:\<blah>\target\scala-2.12\neptunium.jar
.scala=C:\<blah>\target\scala-2.12\neptunium.jar
~~~~

Restart Plastic SCM / Semantic Merge/ GMaster as appropriate and your Scala files should work with the syntax / structure-aware diff and merge functionality. Awesome.
 
## The Small Print

*`TL;DR - Caveat Emptor`*

It works well, but has a couple of rough edges.

There are tests and internal code contracts, but these mostly show that the various third party components integrate well together - there is no attempt to cover lots of Scala code cases, although there is one test that uses a large and fairly complex piece of Scala code to verify the plugin.

From doing manual dogfood testing of my own Scala projects (not just the public ones on GitHub), it seems stable and provides sensible results - I use it on a regular basis nowadays and have no problems. It even copes with a sample of ScalaZ changesets!

Scalatest tests are handled to some extent; if you move a test between classes or files or within a file, it should get picked up as a move refactoring.

However, there is no guarantee that it won't mangle your Scala files during a merge, and if it does, you are on your own; this plugin doesn't come with guarantees or liability. Having said that, please do report bugs, or better yet, raise a pull request with a fix.

Where there is whitespace between sections of code, then the end of one section can overshoot a line break and spill on to the line on which the following section starts - this is quite harmless, but looks a bit strange sometimes.

Please do fork this repository, have a play, and raise pull requests - collaborators are welcome!

## How it works

### Plugin Architecture ####

OK, that's slightly high-faluting for a project with two source files in it, but let's talk about what is expected of the plugin by Plastic SCM / Semantic Merge / GMaster, and what provides it.

First off, see here: https://users.semanticmerge.com/documentation/external-parsers/external-parsers-guide.shtml.

The plugin's job is to be a console application that reads triplets of lines from standard input - the first line refers to the name of a Scala file that constitutes an input to a diff or merge - could be the left hand, the right hand or the base - we don't care. The plugin does **not** perform the diff, rather it transforms that file into a description of a tree of positions that demarcate interesting constructs in the file. This description is written to the file that is named by the third line of the triplet.

(The second line in each triplet contains the character set encoding of the Scala file.)

Where the input file named by the first line in the triplet comes from and the fate of the output file named by the third line is not the plugin's concern - it just reads from one, and creates / writes to the other. Job done. The input file might be part of a version controlled source tree, or it might be a temporary file. Don't know and don't care. The output file might be deleted or might just pile up somewhere in a temporary directory for scavenging later - don't know and don't care, either.

Likewise, the diff / merge mechanism is opaque to the plugin - the plugin just creates the descriptions that in turn are fed off as input to some mysterious mechanism out of its control.

So, the plugin reads triplets of lines and for each triplet, it performs a transformation from Scala input to position tree output. It carries on doing this, reading triplet after triplet until it receives the special sentinel value `end` on standard input, at which point it quits. The plugin has no idea when it is iterating away over triplets as to whether these are from one or many invocations of diff / merge in Plastic SCM / Semantic Merge / GMaster, or when the next triplet will arrive - it just sits there blocked on standard input until the next triplet or the end sentinel arrives.

There is some flow control though, in that when the plugin initialises, it creates and writes to a *flag-file* whose path is given by the second command line argument passed in by Plastic SCM / Semantic Merge / GMaster to signal back to the host program that the latter may start sending it line triplets - this file contains the string `READY`. Furthermore, for each transformation that was carried out successfully on receipt of a triplet, it will write `OK` to standard output - or if something went wrong, `KO`. That's it really for the interaction protocol. Nice and simple.

(The first command line argument describes the mode the plugin should operate in - for now, it is always the string `shell`.)

This is realised in the `Main` object (which is the top level application object, naturally) by a ScalaZ Stream pipeline that pumps from standard input to standard output, carrying out the transformation as a side effect within a task buried in the pipeline. The pipeline quits when the end sentinel is received, otherwise it extracts pairs of input and output filenames from successive triplets and hands the pairs off to the `FileProcessor` standalone object that does the real work - so the protocol logic is quite distinct from the transformation logic.


OK, that's enough about `Main`, what of the `FileProcessor`?

`FileProcessor` reads a Scala file and writes out the position tree description as YAML. It does this quite distinctly from the pipeline in `Main` - the latter is purely about freighting from standard input to standard output, there are no side branches in the pipeline to read the Scala files and write the description files.

It uses the Scalameta library to parse the Scala source, building up an abstract syntax tree to which an instance of `Traverser` is applied.

The traveser then yields a new tree, distinct from the Scalameta's own abstract syntax tree; this is the position tree - a `Position` is an abstraction from the Scalameta library; it models a chunk of text within the Scala source, could be a caret position or could be a selection of several adjacent characters - there are other variations on this theme too. The plugin is only really interested in the multicharacter selection case, although caret-style positions are tolerated.

This position tree should correspond roughly to the syntactic structure of the Scala code being compiled. By roughly, I mean that it can leave out the fine detail and just report on the big picture items such as classes and methods. However, the plugin has to ensure that the positions on the tree completely cover the entire source file - no gaps are permitted.

So there is a bit of post-processing of the position tree, using the `transform` method to do a functional transformation of the tree structure. I tried to use the ScalaZ rose tree abstraction to do this, but the code got quite messy - so for now, I've stuck with yet another hand-rolled tree data structure. Please feel free to refactor this to reuse an existing third party library that is tested, possibly the ScalaZ one.

The post processing simplifies the tree structure to avoid overwhelming the end user with too much detail, and fills in the gaps in the position tree. When simplifying, the algorithm uses the notion of *interesting* tree nodes to decide on what to preserve and what to fuse together or just discard outright.

Once the position tree has been given the treatment, there is straightforward but long-winded slog via recursive descent that writes out YAML in the manner expected by Plastic SCM / Semantic Merge / GMaster.

Be aware that the consumer of the YAML has its own notion of *sections*, *containers* and *terminals* - these are represented as case-classes in their own right, so the aforementioned recursive descent creates another intermediate tree structure that corresponds to the consumer's YAML model. That tree is then serialized as actual YAML by Circe and CirceYaml.

Within a YAML container, there is postcondition forced on the plugin by the end consumer that the child sections in a container must all abut with each other to form a single contiguous piece of text without gaps. It is permitted to have a header at the start of the container and a footer at the end, these make up the gaps between the start and end of the parent container and the leading and trailing child sections, respectively. That's the model forced on the plugin and it has to roll with it, regardless of what the Scala grammar looks like. This is enforced in the intermediate tree model via code contracts, so if you make mistake when working on the code, this should be detected, causing the plugin to fail fast.
