# Neptunium - the Semantic Merge Scala Plugin
## What?

A plugin that integrates with:-
 1. the version control system *Plastic SCM*.
 1. the standalone merge tool *Semantic Merge*.

It adds support for syntax-aware structured merging of **Scala** code.

Currently, only a Windows build is supported - it could easily be generalised to run on Unix/OSX, but that as per demand - please fork this repository and raise a pull request if you want to do this yourself, your contributions would be very welcome.

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
