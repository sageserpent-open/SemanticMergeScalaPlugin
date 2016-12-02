# Neptunium - the Semantic Merge Scala Plugin
## What?

A plugin that integrates with:-
 1. the version control system *Plastic SCM*.
 1. the standalone merge tool *Semantic Merge*.

It adds support for syntax-aware structured merging of **Scala** code.

Currently, only a Windows build is supported - it could easily be generalised to run on Unix/OSX, but that is as per demand - please fork this repository and raise a pull request if you want to do this yourself, your contributions would be very welcome.

It is MIT licensed - although the products mentioned above have their own licenses completely independent of this plugin.

## How?

You build the plugin from source and configure Plastic SCM / Semantic Merge to pick it up. It's hardly shrinkwrapped, but it's not hard to do either.

So, you're a Scala developer if you want to use this plugin, aren't you? You know about Git because you are here on Github?

Right, so clone this repository and fire up SBT with the custom task `allInOneCommandScript`. That will generate a file `neptunium.cmd` in the `target` directory of the SBT build. That file is the built plugin.

Next, integrate this into Plastic SCM / Semantic Merge - some background is here: http://codicesoftware.blogspot.com/2015/09/custom-languages-in-semantic-version.html.

Specifically for this plugin, locate the file `externalparsers.conf` in your appdata directory - eg: `C:\Users\<you>\AppData\Local\plastic4`.

Edit it to point to `neptunium.cmd`, like this:-

`.sc=C:\<blah>\target\neptunium.cmd
.scala=C:\<blah>\target\neptunium.cmd`

Restart Plastic SCM and / or Semantic Merge and your Scala files should work with the syntax / structure-aware diff and merge functionality. Awesome.
 
## The Small Print

*`TL;DR - Caveat Emptor`*

It works, but has rough edges.

It won't trash your Scala files, at least as far as I have verified by manually tested by dog fooding my own Scala projects through it, not just the ones you can see on Github. If it does mess up your files, please feel free to talk to the hand. Or raise a bug report - or fix it yourself!

(BTW - *where are the tests?!*)

However, it needs finessing - you'll see this if you look closely at the ends of the highlighted sections in a diff or merge. They tend to overshoot the construct under scrutiny and grab a token or two from the following construct. It doesn't actually matter in practice, but looks unsightly (and gives an unsettling feeling).

This is down to the combination of using the Scala presentation compiler to build a tree of positions in the Scala source code being analysed - the abstract syntax tree does not correspond exactly to the source, so the positions don't quite line up. This would be fine in itself, only the diff / merge functionality that this code plugs into needs the see **all** of the source code without gaps. The plugin uses a very sloppy heuristic to plug these gaps, which in turn leads to the overshoot. It can be fixed, but it's not a high priority for me.

Please do fork this repository, have a play, and raise pull requests - collaborators are welcome!

## How it works

### Plugin Architecture ####

OK, that's slightly high-faluting for a project with two source files in it, but let's talk about what is expected of the plugin by Plastic SCM and Semantic Merge, and what provides it.

First off, see here: http://blog.plasticscm.com/2015/09/custom-languages-in-semantic-version.html.

The plugin's job is to be a console application that reads pairs of filenames from standard input - the first half of each pair refers to the name of a Scala file that constitutes an input to a diff or merge - could be the left hand, the right hand or the base - we don't care. The plugin does **not** perform the diff, rather it transforms that file into a description of a tree of positions that demarcate interesting constructs in the file. This description is written to the file that is named by the second half of the pair.

Where the first file in the pair comes from and the fate of the second is not the plugin's concern - it just reads from one, and creates / writes to the other. Job done. The first file might be part of a version controlled source tree, or it might be a temporary file. Don't know and don't care. The second file might be deleted or might just pile up somewhere in a temporary directory for scavenging later - don't know and don't care, either.

Likewise, the diff / merge mechanism is opaque to the plugin - the plugin just creates the descriptions that in turn are fed off as input to some mysterious mechanism out of its control.

So, the plugin reads pairs of files and for each pair, it performs a transformation from Scala input to position tree output. It carries on doing this, reading pair after pair until it receives the special sentinel value `end` on standard input, at which point it quits. The plugin has no idea when it is iterating away over pairs as to whether these are from one or many invocations of diff / merge in Plastic SCM / Semantic Merge, or when the next pair will arrive - it just sits there blocked on standard input until the next pair or the end sentinel arrives.

There is some flow control though, in that when the plugin initialises, it writes `READY` to standard output to signal back to the host program that the latter may start sending it pairs of filenames. Furthermore, for each transformation that was carried out successfully on receipt of a pair, it will write `OK` to standard output - or if something went wrong, `KO`. That's it really for the interaction protocol. Nice and simple.

This is realised in the `Main` object (which is the top level application object, naturally) by a ScalaZ Stream pipeline that pumps from standard input to standard output, carrying out the transformation as a side effect within a task buried in the pipeline. The pipeline quits when the end sentinel is received, otherwise it pairs successive filenames and hands the pairs off to the `FileProcessor` standalone object that does the real work - so the protocol logic is quite distinct from the transformation logic.
 
There is a headache caused by `FileProcessor`'s use of the Scala presentation compiler - it needs to have the compiler's library as an explicit classpath dependency distinct from the one that the application sees, and to make matters worse, that dependency must be on a JAR file. Now, the plugin is distributed as a self-executing JAR, i.e. a JAR embedded within a command script, so the plugin can't just add itself to that explicit classpath - won't work, the file extension has to be .JAR. Hmmm.

The rather ungainly solution is to get `Main` to make a copy of its own executable file with the .JAR suffix in a temporary directory, this is what is picked up by the Scala presentation compiler. Yes, a *copy*, not a link - if the plugin uses a link, that prevents it from deleting the temporary file when it shuts down due to the executing program pinning the file under Windows. Even with a copy, it is still not possibly to delete the copy as the plugin shuts down, because the copy is loaded in memory by the presentation compiler. Ugh. The brute-force solution adopted by `Main` is to fork the entire process into an outer process that creates the copy, and then cleans up later - the inner process takes care of the pipeline and is the one that loads the copy into memory. So once the inner process terminates, the outer one can safely delete the copy. Messy, but it does work.

OK, having griped about `Main`, what of the `FileProcessor`?

`FileProcessor` reads a Scala file and writes out the position tree description as YAML. It does this quite distinctly from the pipeline in `Main` - the latter is purely about freighting from standard input to standard output, there are no side branches in the pipeline to read the Scala files and write the description files.

It executes the Scala presentation compiler on the Scala input, building up an abstract syntax tree to which an instance of `PositionTreeBuilder` is applied internally as a visitor object.

The visitor then yields a new tree, distinct from the compiler's internal abstract syntax tree; this is the position tree - a `Position` is an abstraction from the presentation compiler library; it models a chunk of text within the Scala source, could be a caret position or could be a selection of several adjacent characters - there are other variations on this theme too. The plugin is only really interested in the multicharacter selection case, although caret-style positions are tolerated.

This position tree should correspond roughly to the syntactic structure of the Scala code being compiled. By roughly, I mean that it can leave out the fine detail and just report on the big picture items such as classes and methods. However, the plugin has to ensure that the positions on the tree completely cover the entire source file - no gaps are permitted.

So there is a bit of post-processing of the position tree, using the `transform` method to do a functional transformation of the tree structure. I tried to use the ScalaZ rose tree abstraction to do this, but the code got quite messy - so for now, I've stuck with yet another tree data structure. Please feel free to refactor this to reuse an existing third party library that is tested, possibly the ScalaZ one.

The post processing simplifies the tree structure to avoid overwhelming the end user with too much detail, and fills in the gaps in the position tree. When simplifying, the algorithm uses the notion of *interesting* tree nodes to decide on what to preserve and what to fuse together. I have tried approaches where boring tree nodes are simply thrown away, but the current fusion idea seems to give the nicest looking results. That's not to say it couldn't be improved. Take a look through the commit history to see the also-rans.

Once the position tree has been given the treatment, there is straightforward but long-winded slog via recursive descent that writes out YAML in the manner expected by Plastic SCM / Semantic Merge, with a bit of logic to indent the YAML to make it human-readable.

Be aware that the consumer of the YAML has its own notion of *sections*, *containers* and *terminals* - these can be represented as case-classes in their own right, but it was simpler to go with a homogenous position tree representation and encode directly into YAML, rather than introduce another intermediate tree structure.

Within a YAML container, there is postcondition forced on the plugin by the end consumer that the child sections in a container must all abut with each other to form a single contiguous piece of text without gaps. It is permitted to have a header at the start of the container and a footer at the end, these make up the gaps between the start and end of the parent container and the leading and trailing child sections, respectively. That's the model forced on the plugin and it has to roll with it, regardless of what Scala looks like. Failure to adhere to this will result in a mangled diff or merge - you have been warned!
